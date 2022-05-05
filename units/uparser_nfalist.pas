unit uparser_nfalist;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, uparser_nfastate, uparser_nfaset, uparser_nfa, fgl,
  uparser_types, uparser_terminal, ucharset32, deployment_parser_types_12,
  deployment_parser_module_12;

type

  TNFADictType = (ndtCharacter,ndtCharRange);

  TNFADictionaryItem = record
      DictType:    TNFADictType;
      Character:   TChar;
      CharacterTo: TChar;
      CharSet:     TCharSet;
      class operator = (const a,b: TNFADictionaryItem): boolean;
    end;

  TNFADictionary = class(specialize TFPGList<TNFADictionaryItem>)
    protected
      FParser: TLCGParser;
    public
      constructor Create(_parser: TLCGParser);
      procedure BuildFromNFA(_nfa: TNFA);
      function  HighestCharacter: TChar;
      function  IndexOfCharacter(c32: TChar): integer;
      function  IndexOfRange(c32,t32: TChar): integer;
      procedure Dump(_strm: TStream);
      function  Titles: TStringArray;
      function  TitlesAsXML: TStringArray;
  end;


  TNFAListRow = class(TObject)
    public
      OwnerName:   string;
      IsAccepting: boolean;
      TerminalStyle: TTerminalStyle;
      AcceptToken: TTokenIdentifier;
      NextState: array of TStateIdentifier;
      Epsilons:  TNFASet;
      constructor Create(_rowsize: integer);
      destructor Destroy; override;
      procedure AttemptEpsilonAdd(_nfa: TNFA; _statenum: TStateIdentifier);
      procedure CreateFromState(_nfa: TNFA; _dictionary: TNFADictionary; _statenum: TStateIdentifier);
  end;

  TNFAList = class(specialize TFPGObjectList<TNFAListRow>)
    protected
      FDictionary: TNFADictionary;
      FParser:     TLCGParser;
    public
      constructor Create(_parser: TLCGParser);
      destructor Destroy; override;
      function  AcceptToken(_set: TNFASet): TTokenIdentifier;
      procedure CreateFromNFA(_nfa: TNFA);
      procedure Dump(_strm: TStream);
      procedure DumpXML(_strm: TStream);
      property Dictionary: TNFADictionary read FDictionary;
  end;

implementation

uses
  uparser_utility, uparser_exception;

{ Utility code }

class operator TNFADictionaryItem.=(const a,b: TNFADictionaryItem): boolean;
begin
  Result := True;
  if (a.DictType <> b.DictType) then
    Exit(False);
  case a.DictType of
    ndtCharacter:   Result := (a.Character = b.Character);
    ndtCharRange:   Result := (a.Character = b.Character) and (a.CharacterTo = b.CharacterTo);
  end;
end;

function DictCompareFunc(const a,b: TNFADictionaryItem): integer;
begin
  if (a.DictType = ndtCharacter) and (b.DictType = ndtCharRange) then
    Result := -1
  else if (a.DictType = ndtCharRange) and (b.DictType = ndtCharacter) then
    Result := 1
  else
    begin
      if a.Character < b.Character then
        Result := -1
      else if a.Character > b.Character then
        Result := 1
      else
        Result := 0;
    end;
end;

{ TNFADictionary}

constructor TNFADictionary.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
end;

procedure TNFADictionary.BuildFromNFA(_nfa: TNFA);
var s: TState;
    a: TAction;
    r: TNFADictionaryItem;
begin
  // Add all the items to the dictionary
  FParser.Monitor(ltWarAndPeace,'Building dictionary from NFA');
  for s in _nfa.StateList do
    for a in s.Actions do
      case a.ActionType of
        atCharacter:  begin
                        r.DictType  := ndtCharacter;
                        r.Character := a.ActionChar;
                        r.CharSet   := nil;
                        if IndexOf(r) < 0 then
                          Add(r);
                      end;
        {
        atCharRange:  begin
                        r.DictType    := ndtCharRange;
                        r.Character   := a.ActionChar;
                        r.CharacterTo := a.ActionCharTo;
                        if IndexOf(r) < 0 then
                          Add(r);
                      end;
        }
      end;
  // Finally sort the dictionary into the right order
  Sort(@DictCompareFunc);
end;

procedure TNFADictionary.Dump(_strm: TStream);
var i: TNFADictionaryItem;
begin
  WriteLnStringToStreamUnderlined(_strm,'DICTIONARY');
  for i in Self do
      case i.DictType of
        ndtCharacter: WriteLnStringToStream(_strm,'Character ' + MakePrintable(i.Character));
        ndtCharRange: WriteLnStringToStream(_strm,'Character Range ' + MakePrintable(i.Character) + '-' + MakePrintable(i.CharacterTo));
      end;
  WriteLnStringToStream(_strm,'');
end;

function TNFADictionary.HighestCharacter: TChar;
begin
  Result := #0;
  if Count > 0 then
    case Items[Count-1].DictType of
      ndtCharacter: Result := Items[Count-1].Character;
      ndtCharRange: Result := Items[Count-1].CharacterTo;
    end;
end;

function TNFADictionary.IndexOfCharacter(c32: TChar): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if (Items[i].DictType = ndtCharacter) and
       (Items[i].Character = c32) then
      Exit(i);
end;

function TNFADictionary.IndexOfRange(c32,t32: TChar): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if (Items[i].DictType = ndtCharRange) and
       (Items[i].Character = c32) and
       (Items[i].CharacterTo = t32) then
      Exit(i);
end;

function TNFADictionary.Titles: TStringArray;
var i: integer;
begin
  SetLength(Result,Count);
  for i := 0 to Count-1 do
    case items[i].DictType of
      ndtCharacter: Result[i] := MakePrintable(items[i].Character);
      ndtCharRange: Result[i] := MakePrintable(items[i].Character) + '-' + MakePrintable(items[i].CharacterTo);
    end;
end;

function TNFADictionary.TitlesAsXML: TStringArray;
var i: integer;
begin
  SetLength(Result,Count);
  for i := 0 to Count-1 do
    case items[i].DictType of
      ndtCharacter: Result[i] := StringAsXMLHeading(items[i].Character);
      ndtCharRange: Result[i] := StringAsXMLHeading(items[i].Character) + '-' + StringAsXMLHeading(items[i].CharacterTo);
    end;
end;

{ TNFAListRow code }

constructor TNFAListRow.Create(_rowsize: integer);
begin
  inherited Create;
  Epsilons := TNFASet.Create;
  SetLength(NextState,_rowsize);
end;

destructor TNFAListRow.Destroy;
begin
  Epsilons.Free;
  inherited Destroy;
end;

procedure TNFAListRow.AttemptEpsilonAdd(_nfa: TNFA; _statenum: TStateIdentifier);
// Tries to find an epsilon entry in the list. If it isn't there, we add it
var index: integer;
    i:     integer;
    act:   TAction;
    state: TState;
begin
  index := 0; // Keep the compiler quiet
  if not Epsilons.Find(_statenum,index) then
    begin
      Epsilons.Add(_statenum);
      // Now iterate through the new state and any epsilon actions from that
      // This is a recursive activity
      state := _nfa.Items[_statenum];
      for i := 0 to state.Actions.Count-1 do
        begin
          act := state.Actions[i];
          if act.ActionType = atEpsilon then
            AttemptEpsilonAdd(_nfa,act.NextState);
        end;
    end;
end;

procedure TNFAListRow.CreateFromState(_nfa: TNFA; _dictionary: TNFADictionary; _statenum: TStateIdentifier);
// Create an NFA list entry from a single state
// Takes account of epsilon transitions
var ci: integer;
    i: integer;
    act: TAction;
    state: TState;
    bi: integer;
    c32: TChar;
begin
  state := _nfa.Items[_statenum];
  OwnerName     := state.OwnerName;
  TerminalStyle := state.TerminalStyle;
  IsAccepting := state.IsAccepting;
  AcceptToken := state.AcceptToken;
  // Wipe out next values
  for ci := 0 to Length(NextState)-1 do
    NextState[ci] := NO_STATE_IDENTIFIER;
  Epsilons.Count := 0;
  // Now iterate through and add the actions and epsilons
  for i := 0 to state.Actions.Count-1 do
    begin
      act := state.Actions[i];
      case act.ActionType of
        atCharacter:  begin
                        c32 := act.ActionChar;
                        bi := _dictionary.IndexOfCharacter(c32);
                        NextState[bi] := act.NextState;
                      end;
      end; // case
    end;
  // Finally, deal with epsilons
  AttemptEpsilonAdd(_nfa,_statenum); // Always be sure to add our own state
end;

{ TNFAList code }

constructor TNFAList.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
  FDictionary := TNFADictionary.Create(_parser);
end;

destructor TNFAList.Destroy;
begin
  FDictionary.Free;
  inherited Destroy;
end;

function TNFAList.AcceptToken(_set: TNFASet): TTokenIdentifier;
// Iterate through the set and find the accept token (if any)
// If two different accept tokens are present for the same set then an
// error is flagged as it's possible for the same input string to have two
// different accept states
var i,k,m: integer;
    kstyle: TTerminalStyle; // @@@@@ just for debug
    mstyle: TTerminalStyle;
    kname:  string;
    mname:  string;
begin
  Result := NO_TOKEN_IDENTIFIER;
  m := 0;
  for i := 0 to _set.Count-1 do
    begin
      k := _set.Items[i];
      if Items[k].IsAccepting then
        begin
          if Result <> NO_TOKEN_IDENTIFIER then
            begin
              // We already have a different accepting state registered for this
              // lookahead. One of the following things can happen:
              //
              // 1. The previous item is a Keyword or Symbol and this item is a
              //    Terminal in which case we abandon the later terminal item
              //    and give priority to the Keyword/Symbol
              // 2. The previous item is a Terminal and this item is a Keyword
              //    or Symbol, we ignore the previous item and the later
              //    Keyword / Symbol is used instead
              // 3. The previous item is a Terminal and this item is a Terminal
              //    OR.... the previous item is a Keyword/Symbol and this item
              //    is a Keyword/Symbol, therefore it is not possible to
              //    resolve the conflict and an error occurs
              //
              // kstyle is the current accept token and mstyle is the previous
              // accept token
              kstyle := Items[k].TerminalStyle;
              mstyle := Items[m].TerminalStyle;
              kname  := Items[k].OwnerName;
              mname  := Items[m].OwnerName;
              if (mstyle in [tsKeyword,tsSymbol]) and
                 (kstyle in [tsTerminal]) then
                k := m // 1. Abandon latest one (k) keep earlier (m)
              else if (mstyle in [tsTerminal]) and
                      (kstyle in [tsKeyword,tsSymbol]) then
                m := k  // 2. Abandon earlier (m) use latest (k)
              else      // 3. Raise error as we cannot resolve the conflict
                FParser.Monitor(ltError,'The same text could result in multiple lexical tokens %d and %d being accepted (%s and %s)',
                  [Result,Items[k].AcceptToken,mname,kname]);
            end;
          Result := Items[k].AcceptToken;
          m := k;
        end;
    end;
end;


procedure TNFAList.CreateFromNFA(_nfa: TNFA);
// Create the NFA list from an NFA graph
// Assumes Items[0] is always the entry point to the graph
var i: integer;
    entry: TNFAListRow;
begin
  FDictionary.BuildFromNFA(_nfa);
  for i := 0 to _nfa.StateList.Count-1 do
    begin
      entry := TNFAListRow.Create(FDictionary.Count);
      entry.CreateFromState(_nfa,FDictionary,i);
      Add(entry);
    end;
end;

procedure TNFAList.Dump(_strm: TStream);
const MIN_COL_WIDTH = 5;
var titles: TStringArray;
    i:      integer;
    j:      integer;
    s:      string;
    ent:    TNFAListRow;
    column_sizes: array of integer;
begin
  FDictionary.Dump(_strm);
  titles := FDictionary.Titles;
  WriteLnStringToStreamUnderlined(_strm,'NFA LIST');
  WriteLnStringToStream(_strm,'');
  // Get column sites
  SetLength(column_sizes,FDictionary.Count);
  for i := 0 to Length(titles)-1 do
    if Length(titles[i]) > MIN_COL_WIDTH then
      column_sizes[i] := Length(titles[i])
    else
      column_sizes[i] := MIN_COL_WIDTH;
  // Gather titles
  s := 'State Token ';
  for i := 0 to Length(titles)-1 do
    s := s + Format('%' + IntToStr(column_sizes[i]) + '.' +
                    IntToStr(column_sizes[i]) + 's ',[titles[i]]);
  s := s + 'Epsilons';
  WriteLnStringToStream(_strm,s);
  // Gather underlines
  s := '----- ----- ';
  for i := 0 to Length(titles)-1 do
      s := s + StringOfChar('-',column_sizes[i]) + ' ';
  s := s + '--------';
  WriteLnStringToStream(_strm,s);
  // Now the main list
  for i := 0 to Count-1 do
    begin
      ent := Items[i];
      s := Format('%4d',[i]);
      if ent.IsAccepting then
        s := '(' + s + ') ' + Format('%4d ',[ent.AcceptToken])
      else
        s := ' ' + s + '       ';
      for j := 0 to Length(titles)-1 do
        if ent.NextState[j] = NO_STATE_IDENTIFIER then
          s := s + StringOfChar(' ',column_sizes[j]+1)
        else
          s := s + Format('%' + IntToStr(column_sizes[j]) + 'd ',[ent.NextState[j]]);
      s := s + ent.Epsilons.AsString;
      WriteLnStringToStream(_strm,s);
    end;
  // Finally a blank line
  WriteLnStringToStream(_strm,'');
end;

procedure TNFAList.DumpXML(_strm: TStream);
var titles: TStringArray;
    i:      integer;
    j:      integer;
    s:      string;
    ent:    TNFAListRow;
begin
  titles := FDictionary.TitlesAsXML;
  WriteLnStringToStream(_strm,'  <NFA>');
  // Now the main list
  for i := 0 to Count-1 do
    begin
      ent := Items[i];
      s := '    <Entry>';
      s := s + '<State>' + IntToStr(i) + '</State>';
      s := s + '<Accept>';
      if ent.IsAccepting then
        s := s + IntToStr(ent.AcceptToken);
      s := s + '</Accept>';
      for j := 0 to Length(titles)-1 do
        begin
          s := s + '<' + titles[j] + '>';
          if ent.NextState[j] <> NO_STATE_IDENTIFIER then
            s := s + IntToStr(ent.NextState[j]);
          s := s + '</' + titles[j] + '>';
        end;
      s := s + '<Epsilons>' + ent.Epsilons.AsString + '</Epsilons>';
      s := s + '</Entry>';
      WriteLnStringToStream(_strm,s);
    end;
  // Finally a blank line
  WriteLnStringToStream(_strm,'  </NFA>');
end;

{
function TNFAList.GetDictionary: TCharSet;
var cs: TCharSet;
    i:  integer;
    c:  char;
begin
  cs := [];
  for i := 0 to Count-1 do
    for c in char do
      if Items[i].NextState[c] <> NO_STATE_IDENTIFIER then
        cs := cs + [c];
  GetDictionary := cs;
end;
}

end.

