unit uparser_nfastate;

{$mode objfpc}{$H+}

//
// Defines the parser TState and TStateList types (NFA states)
//

interface

uses
  Classes, SysUtils, fgl, uparser_types, uparser_terminal, ucharset32,
  deployment_parser_types_12;

type

  TAction = class(TObject)
    public
      NextState:       TStateIdentifier;
      ActionType:      TActionType;
      ActionChar:      TChar;
      ActionCharTo:    TChar;
      ActionSet:       TCharSet;
      constructor Create;
      procedure Dump(_strm: TStream);
      function  HighestCharUsed: TChar;
      function  LowestCharUsed: TChar;
  end;

  TActionList = class(specialize TFPGObjectList<TAction>)
    public
//      procedure DemoteCharacterSet(_csfrom: TCharSet; _to: TChar);
      procedure Dump(_strm: TStream);
      function  HighestCharUsed: TChar;
      function  LowestCharUsed: TChar;
//      procedure MarkSetUsed(_split: TSplittableSet);
//      procedure RenameCharacterSet(_csfrom, _csto: TCharSet);
//      procedure SplitActions(_split: TSplittableSet);
//      procedure SplitSet(_split: TSplittableSet);
  end;

  TState = class(TObject)
    public
      TerminalStyle: TTerminalStyle;
      IsAccepting:   boolean;
      OwnerName:     string;
      AcceptToken:   TTokenIdentifier;
      Actions:       TActionList;
      constructor Create;
      destructor Destroy; override;
      procedure  AddAction(_nextstate: TStateIdentifier; _actiontype: TActionType; _actionchar: TChar = #0; _actioncharto: TChar = #0);
      procedure Dump(_strm: TStream; _terminals: TTerminalList);
      function  HighestCharUsed: TChar;
      function  LowestCharUsed: TChar;
//      procedure MarkSetUsed(_split: TSplittableSet);
//      procedure SplitActions(_split: TSplittableSet);
//      procedure SplitSet(_split: TSplittableSet);
  end;

  TStateList = class(specialize TFPGObjectList<TState>)
    protected
//      FSplittableSet: TSplittableSet;
    public
      constructor Create;
      destructor Destroy; override;
      function  AllocateState(_isaccepting: boolean = False; _accepttoken: TTokenIdentifier = 0): TStateIdentifier;
//      procedure CombineCharsets(_cslist: TCharSetList);
//      procedure DemoteCharacterSet(_csfrom: TCharSet; _to: TChar);
      procedure Dump(_strm: TStream; _terminals: TTerminalList);
      function  HighestCharUsed: TChar;
      function  LowestCharUsed: TChar;
//      procedure MinimiseCharsets;
//      procedure RenameCharacterSet(_csfrom, _csto: TCharSet);
      procedure SetStateName(_state: TStateIdentifier; _title: string);
  end;

implementation

uses
  uparser_utility, uparser_exception, deployment_parser_module_12;

{ TAction code }

constructor TAction.Create;
begin
  inherited Create;
end;

procedure TAction.Dump(_strm: TStream);
begin
  case ActionType of
   atCharacter: WriteLnStringToStream(_strm,Format('Char "%s" -> %d',[MakePrintable(ActionChar),NextState]));
//   atCharRange: WriteLnStringToStream(_strm,Format('Range "%s"-"%s" -> %d',[MakePrintable(ActionChar),MakePrintable(ActionCharTo),NextState]));
   atEpsilon:   WriteLnStringToStream(_strm,Format('Epsilon -> %d',[NextState]));
  end; // Case
end;

function TAction.HighestCharUsed: TChar;
begin
  Result := High(TSetOfChar);
  case ActionType of
   atCharacter: Result := ActionChar;
//   atCharRange: Result := ActionCharTo;
   atEpsilon:   raise Exception.Create('Attempting to check highest character for an epsilon state');
  end; // Case
end;

function TAction.LowestCharUsed: TChar;
begin
  Result := Low(TSetOfChar);
  case ActionType of
   atCharacter: Result := ActionChar;
//   atCharRange: Result := ActionChar;
   atEpsilon:   raise Exception.Create('Attempting to check lowest character for an epsilon state');
  end; // Case
end;

{ TActionList code }

{
procedure TActionList.DemoteCharacterRange(_csfrom: TCharSet; _to: TChar);
var a: TAction;
begin
  for a in Self do
    if (a.ActionType = atCharRange) and (a.ActionSet = _csfrom) then
      begin
        a.ActionType := atCharacter;
        a.ActionChar := _to;
        a.ActionSet  := nil;
      end;
end;
}

procedure TActionList.Dump(_strm: TStream);
var i: Integer;
begin
  if Count = 0 then
    exit;
  for i := 0 to Count-1 do
    begin
      if i > 0 then
        WriteStringToStream(_strm,'                 ');
      Items[i].Dump(_strm);
    end;
end;

function TActionList.HighestCharUsed: TChar;
var a: TAction;
    h: TChar;
begin
  Result := #0;
  for a in Self do
    if a.ActionType <> atEpsilon then
      begin
        h := a.HighestCharUsed;
        if h > Result then
          Result := h;
      end;
end;

function TActionList.LowestCharUsed: TChar;
var a: TAction;
    l: TChar;
begin
  Result := High(TChar);
  for a in Self do
    if a.ActionType <> atEpsilon then
      begin
        l := a.LowestCharUsed;
        if l < Result then
          Result := l;
      end;
end;

{
procedure TActionList.MarkSetUsed(_split: TSplittableSet);
var a: TAction;
begin
  for a in Self do
    case a.ActionType of
      atCharacter: _split.MarkCharUsed(a.ActionChar);
      atCharRange: _split.MarkRangeUsed(a.ActionChar,a.ActionCharTo);
    end; // Case
end;
}

{
procedure TActionList.RenameCharacterSet(_csfrom, _csto: TCharSet);
var a: TAction;
begin
  for a in Self do
    if (a.ActionType = atCharRange) and (a.ActionSet = _csfrom) then
      a.ActionSet := _csto;
end;
}

{
procedure TActionList.SplitActions(_split: TSplittableSet);
var a:     TAction;
    anew:  TAction; // New action
    i:     integer;
    k:     integer;
    total: integer;
    added: integer;
    csf:   TCharSetFragment;
begin
  total := Count;
  i := 0;
  while i < total do
    if Items[i].ActionType = atCharRange then
      begin
        a := Items[i];
        csf.First := a.ActionChar;
        csf.Last  := a.ActionCharTo;
        added := 0;
        // Iterate through the range adding the split fragments
        for k := 0 to _split.Count-1 do
          if (_split.Items[k].First >= csf.First) and
             (_split.Items[k].Last  <= csf.Last) then
            begin
              if _split.Items[k].First = _split.Items[k].Last then
                begin  // Add a single character
                  anew := TAction.Create;
                  anew.ActionType   := atCharacter;
                  anew.ActionChar   := _split.Items[k].First;
                  anew.ActionCharTo := a.ActionChar;
                  anew.NextState    := a.NextState;
                end
              else
                begin  // Add a range
                  anew := TAction.Create;
                  anew.ActionType   := atCharRange;
                  anew.ActionChar   := _split.Items[k].First;
                  anew.ActionCharTo := _split.Items[k].Last;
                  anew.NextState    := a.NextState;
                end;
              // Insert new record the current record
              Insert(i+1+added,anew);
              Inc(added);
            end;
        Delete(i);
        total := total - 1 + added;
        i := i + added;
      end
    else
      Inc(i);
end;

procedure TActionList.SplitSet(_split: TSplittableSet);
var a: TAction;
begin
  for a in Self do
    case a.ActionType of
      atCharacter: _split.SplitWithChar(a.ActionChar);
      atCharRange: _split.SplitWithRange(a.ActionChar,a.ActionCharTo);
    end; // Case
end;
}

{ TState code }

constructor TState.Create;
begin
  inherited Create;
  IsAccepting := False;
  AcceptToken := 0;
  Actions := TActionList.Create;
end;

destructor TState.Destroy;
begin
  Actions.Free;
  inherited Destroy;
end;

procedure TState.AddAction(_nextstate: TStateIdentifier; _actiontype: TActionType; _actionchar: TChar = #0; _actioncharto: TChar = #0);
var action: TAction;
    i:      integer;
begin
  // First check the action doesn't exist already
  //
  for i := 0 to Actions.Count-1 do
    if (Actions.Items[i].ActionType   = _actiontype) and
       (Actions.Items[i].ActionChar   = _actionchar) and
       (Actions.Items[i].ActionCharTo = _actioncharto) and
       (Actions.Items[i].NextState    = _nextstate) then
    Exit; // Already in there
  // Doesn't exist, so add it
  action := TAction.Create;
  action.NextState     := _nextstate;
  action.ActionType    := _actiontype;
  action.ActionChar    := _actionchar;
  action.ActionCharTo  := _actioncharto;
  Actions.Add(action);
end;

{
procedure TState.DemoteCharacterSet(_csfrom: TCharSet; _to: TChar);
begin
  Actions.DemoteCharacterSet(_csfrom, _to);
end;
}

procedure TState.Dump(_strm: TStream; _terminals: TTerminalList);
begin
  Actions.Dump(_strm);
  if IsAccepting then
    WriteLnStringToStream(_strm,Format('Accepting state with token %d "%s"',[AcceptToken,MakePrintable(_terminals.TerminalName(AcceptToken))]));
end;

function TState.HighestCharUsed: TChar;
begin
  result := Actions.HighestCharUsed;
end;

function TState.LowestCharUsed: TChar;
begin
  result := Actions.LowestCharUsed;
end;

{
procedure TState.MarkSetUsed(_split: TSplittableSet);
begin
  Actions.MarkSetUsed(_split);
end;
}

{
procedure TState.RenameCharacterSet(_csfrom, _csto: TCharSet);
begin
  Actions.RenameCharacterSet(_csfrom, _csto);
end;
}

{
procedure TState.SplitActions(_split: TSplittableSet);
begin
  Actions.SplitActions(_split);
end;

procedure TState.SplitSet(_split: TSplittableSet);
begin
  Actions.SplitSet(_split);
end;
}

{ TStateList code}

constructor TStateList.Create;
begin
  inherited Create;
//  FSplittableSet := TSplittableSet.Create;
end;

destructor TStateList.Destroy;
begin
//  FSplittableSet.Free;
  inherited Destroy;
end;

function TStateList.AllocateState(_isaccepting: boolean; _accepttoken: TTokenIdentifier): TStateIdentifier;
var state: TState;
begin
  state := TState.Create;
  state.IsAccepting := _isaccepting;
  state.AcceptToken := _accepttoken;
  AllocateState := Add(state);
end;

procedure TStateList.Dump(_strm: TStream; _terminals: TTerminalList);
var i: Integer;
begin
//  FSplittableSet.Dump(_strm);
  WriteLnStringToStreamUnderlined(_strm,'DUMP OF THE STATE LIST');
  for i := 0 to Count-1 do
    begin
      WriteStringToStream(_strm,Format('State no. %6d ',[i]));
      Items[i].Dump(_strm,_terminals);
      WriteLnStringToStream(_strm,'');
    end;
end;

function TStateList.HighestCharUsed: TChar;
var high: TChar;
    s:    TState;
    h:    TChar;
begin
  high := #0;
  for s in Self do
    begin
      h := s.HighestCharUsed;
      if h > high then
        high := h;
    end;
  Result := high;
end;

function TStateList.LowestCharUsed: TChar;
var low: TChar;
    s:    TState;
    l:    TChar;
begin
  low := High(TChar);
  for s in Self do
    begin
      l := s.LowestCharUsed;
      if l < low then
        low := l;
    end;
  Result := low;
end;


procedure TStateList.SetStateName(_state: TStateIdentifier; _title: string);
var st: TState;
    act: TAction;
    next: TStateIdentifier;
begin
  // Iterate through the states setting the title for each state
  if _state = NO_STATE_IDENTIFIER then
    Exit;
  st := Items[_state];
  if st.OwnerName = '' then
    begin
      st.OwnerName := _title;
      for act in st.Actions do
        begin
          next := act.NextState;
          if next <> NO_STATE_IDENTIFIER then
            SetStateName(next,_title);
        end;
    end;
end;


end.

