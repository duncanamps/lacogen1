unit ucharset32;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, deployment_parser_module,
  lacogen10_setliteral_procs, deployment_parser_types;

type
  // Character set list flags, default is [cslBrackets,cslRanges,cslTranslate]
  //
  //   cslName       Include the name, e.g. {CharSetName} = [0-9A-Za-z]
  //   cslBrackets   Include the [ ] round the set literal
  //   cslRanges     Use x-y, for example A-D instead of ABCD
  //   cslTranslate  Turn control characters into their codes
  //
  TCSLFlags = (cslName,cslBrackets,cslRanges,cslTranslate,cslXML);
  TCSLSet = set of TCSLFlags;

  // Character set fragment. A simple range of characters from x-y, or it
  // could just be a single character where First=Last
  TCharSetFragment = record
    First:  TChar;
    Last:   TChar;
    class operator = (a,b: TCharSetFragment) c: boolean;
  end;

  // The character set definition. This is made up of an ordered list of
  // fragments

  TCharSet = class(specialize TFPGList<TCharSetFragment>)
    protected
      FName:      TString;
      FParser:    TLCGParser;
      FSLParser:  TSLParser;
      FTemporary: boolean;
      FUnwanted:  boolean;
      FUsed:      boolean;
    public
      constructor Create(const _name: TString; _parser: TSLParser; _mainparser: TLCGParser);
      procedure AddCharacter(_a: TChar);
      procedure AddFragment(_frag: TCharSetFragment);
      procedure AddRange(_a,_b: TChar);
      procedure AddSet(_set: TCharSet);
      function  AsText(_flags: TCSLSet = [cslBrackets,cslRanges,cslTranslate]): TString;
      function  GetFirstChar(var c: TChar): boolean;
      function  GetSetCount: integer;
      function  Matches(_set: TCharSet): boolean;
      procedure SetLiteral(const s: TString);
      procedure SubtractCharacter(_a: TChar);
      procedure SubtractFragment(_frag: TCharSetFragment);
      procedure SubtractRange(_a,_b: TChar);
      procedure SubtractSet(_set: TCharSet);
      property Name:      TString read FName;
      property Temporary: boolean read FTemporary write FTemporary;
      property Unwanted:  boolean read FUnwanted  write FUnwanted;
      property Used:      boolean read FUsed      write FUsed;
  end;

  TCharSetList = class(specialize TFPGObjectList<TCharSet>)
    private
      FParser:    TLCGParser;
      FSLParser:  TSLParser;
      FTempIndex: integer;
    public
      constructor Create(_parser: TLCGParser);
      destructor Destroy; override;
      function  AllocateCharSet(const _name: TString): TCharSet;
      function  AllocateCharSetTemp: TCharSet;
      procedure DeleteUnwanted;
      procedure Dump(_strm: TStream; _showtemp: boolean = False);
      procedure DumpXML(_strm: TStream; _showtemp: boolean = False);
      function  FindEquivalent(_set: TCharSet): TCharSet;
      function  FindByName(const _name: TString): TCharSet;
      function  FindMustByName(const _name: TString): TCharSet;
      function  IndexOfByName(const _name: TString): integer;
      procedure RemoveTemporary;
  end;

// Forward declarations

function CSListCompare(const a,b: TCharSet): integer;

operator in (_c32: TChar; _f32: TCharSetFragment) b: boolean;
operator in (_c32: TChar; _s32: TCharSet) b: boolean;

implementation

uses
  uparser_utility, uparser_exception, htmlelements;

{ Helper routines }

function CompareFragment32(const a,b: TCharSetFragment): integer;
begin
  if (a.First < b.First) then
    Result := -1
  else if (a.First > b.First) then
    Result := 1
  else
    Result := 0;
end;

function CSListCompare(const a,b: TCharSet): integer;
begin
  if (a.FName < b.FName) then
    Result := -1
  else if (a.FName > b.FName) then
    Result := 1
  else
    Result := 0;
end;

operator in (_c32: TChar; _f32: TCharSetFragment) b: boolean;
begin
  Result := (_c32 >= _f32.First) and (_c32 <= _f32.Last);
end;

operator in (_c32: TChar; _s32: TCharSet) b: boolean;
var frag: TCharSetFragment;
begin
  for frag in _s32 do
    if _c32 in frag then
      Exit(True);
  Exit(False);
end;

{ TCharSetFragment }

class operator TCharSetFragment.= (a,b: TCharSetFragment) c: boolean;
begin
  Result := (a.First = b.First) and
            (a.Last  = b.Last);
end;

{ TCharSet}

constructor TCharSet.Create(const _name: TString; _parser: TSLParser; _mainparser: TLCGParser);
begin
  inherited Create;
  FName := _name;
  FUsed := False;
  FSLParser := _parser;
  FParser   := _mainparser;
end;

procedure TCharSet.AddCharacter(_a: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _a;
  frag.Last  := _a;
  AddFragment(frag);
end;

procedure TCharSet.AddFragment(_frag: TCharSetFragment);
//
// Add a single fragment to the existing list
// If AAA and BBB are existing fragments and XXX is the new one, we could have
// one of the following scenarios:
//
// Scenario 1:                           Adding to an empty set
//                   XXXXX
//
// Scenario 2: AAAAA           BBBBBB    Simple, no overlap
//                     XXXXXX
//
// Scenario 3: AAAAA           BBBBBB    Some overlapping, need to combine A+X
//                XXXXXX
//
// Scenario 4: AAAAA  BBBBB              Multiple overlapping, combine A+X+B
//                XXXXXX
//
// Scenario 5:    AAAAA BBBBB            Total enclosure delete A and B
//              XXXXXXXXXXXXXXX
//
// Scenario 6: AAAAAAAA                  Totally within existing, delete X
//                XXXX
var i:     integer;
    processed34: boolean; // True of Scenario 3/4 (overlaps) have been processed
    processed5:  boolean; // True if Scenario 5 has been processed

  procedure SortIfNeeded;
  begin
    if Count > 1 then
      Sort(@CompareFragment32);
  end;

begin
  // Check for scenario 1
  if Count = 0 then
    begin
      Add(_frag);
      Exit;
    end;
  // Check for scenario 6 deleting the request if appropriate
  for i := 0 to Count-1 do
    if (_frag.First >= Items[i].First) and (_frag.Last <= Items[i].Last) then
      Exit; // Already in the range specified by iter, so bale out
  // Check for scenario 5, a total enclosure of other sets
  processed5 := False;
  for i := Count-1 downto 0 do
    if (Items[i].First >= _frag.First) and (Items[i].Last <= _frag.Last) then
      begin
        processed5 := True;
        Delete(i);
      end;
  if processed5 then
    begin
      AddFragment(_frag); // Recursive! We do this as we may create additional
                          // Scenario 1 to deal with
      Exit;
    end;
  // Check for scenario 3 and 4, doing the combining as necessary
  processed34 := False;
  for i := Count-1 downto 0 do
    begin
      if ((_frag.First >= Items[i].First) and (Ord(_frag.First) <= (Ord(Items[i].Last)+1))) then
        begin
          processed34 := True;
          if Items[i].First < _frag.First then
            _frag.First := Items[i].First;
          if Items[i].Last > _frag.Last then
            _frag.Last := Items[i].Last;
          Delete(i);
        end
      else if ((Ord(_frag.Last) >= Ord(Items[i].First)-1) and (_frag.Last <= Items[i].Last)) then
        begin
          processed34 := True;
          if Items[i].First < _frag.First then
            _frag.First := Items[i].First;
          if Items[i].Last > _frag.Last then
            _frag.Last := Items[i].Last;
          Delete(i);
        end;
    end;
  if processed34 then
    begin
      AddFragment(_frag); // Recursive! We do this as we may create additional
                          // Scenario 1 to deal with
      Exit;
    end;
  // We are now left with scenario 2
  // We know that there are at least 1 or more existing ranges in the list, so
  // we can check for scenario 2 where the new fragment is either before or
  // after the existing list items
  if (_frag.Last < Items[0].first) or (_frag.First > Items[Count-1].Last) then
    begin
      Add(_frag);
      SortIfNeeded;
      Exit;
    end;
  for i := 0 to Count-2 do
    if (_frag.First > Items[i].Last) and (_frag.Last < Items[i+1].First) then
      begin
        Add(_frag);
        SortIfNeeded;
        Exit;
      end;
end;

procedure TCharSet.AddRange(_a,_b: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _a;
  frag.Last  := _b;
  AddFragment(frag);
end;

procedure TCharSet.AddSet(_set: TCharSet);
var frag: TCharSetFragment;
begin
  for frag in _set do
    AddFragment(frag);
end;

function TCharSet.AsText(_flags: TCSLSet): TString;
var frag: TCharSetFragment;
    c:    TChar;
    prefix, mid, suffix: string;
begin
  if cslXML in _flags then
    prefix := '    <' + EscapeHTML(MakeXMLHeading(Name)) + '>'
  else if cslName in _flags then
    prefix := Name + ' = '
  else
    prefix := '';
  if cslBrackets in _flags then
    prefix := prefix + '[';
  mid := '';
  for frag in Self do
    if (frag.First = frag.Last) then
      mid := mid + MakePrintable(frag.First)
    else
      begin
        if cslRanges in _flags then
          mid := mid +
                 MakePrintable(frag.First) +
                 '-' +
                 MakePrintable(frag.Last)
          else
            for c := frag.First to frag.Last do
              mid := mid + MakePrintable(c);
      end;
  suffix := '';
  if cslBrackets in _flags then
    suffix := ']';
  if cslXML in _flags then
    suffix := suffix + '</' + EscapeHTML(MakeXMLHeading(Name)) + '>';
  Result := prefix + mid + suffix;
end;

function TCharSet.GetFirstChar(var c: TChar): boolean;
begin
  Result := True; // Assume OK for now
  if Count > 0 then
    c := Items[0].First
  else
    Result := False;
end;

function TCharSet.GetSetCount: integer;
var f: TCharSetFragment;
begin
  Result := 0;
  for f in Self do
    Result := Result + Ord(f.Last) - Ord(f.First) + 1;
end;

function TCharSet.Matches(_set: TCharSet): boolean;
var i: integer;
begin
  Result := True; // Assume a match for now
  if Count <> _set.Count then
    Exit(False);  // Sizes don't match therefore sets don't match
  for i := 0 to Count-1 do
    if Items[i] <> _set.Items[i] then
      Exit(False);
end;

procedure TCharSet.SetLiteral(const s: TString);
begin
  TSLParser(FSLParser).MakeSetLiteral(Self,s);
end;

procedure TCharSet.SubtractCharacter(_a: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _a;
  frag.Last  := _a;
  SubtractFragment(frag);
end;

procedure TCharSet.SubtractFragment(_frag: TCharSetFragment);
var i: integer;
    r: TCharSetFragment;
// Scenarios against existing set items
//
// Scenario 1: AAAAAA              Outside completely, no action
//                      XXXXXX
//
// Scenario 2: AAAAAA              Part inside and part out, need to trim A
//                 XXXXXX
//
// Scenario 3: AAAAAA              Straight match, delete A
//             XXXXXX
//
// Scenario 4:  AAAA               Encompassing, variation on 3, delete A
//             XXXXXX
//
// Scenario 5: AAAAAA              Split A into A and B
//               XX
begin
  for i := Count-1 downto 0 do
    begin
      if (_frag.First > Items[i].Last) or
         (_frag.Last  < Items[i].First) then
        begin
          ;                          // Scenario 1 - do nothing
        end
      else if (_frag.First <= Items[i].First) and
              (_frag.Last  >= Items[i].Last) then
        Delete(i)                    // Scenario 3/4
      else if (_frag.First >  Items[i].First) and
              (_frag.Last  >= Items[i].Last) then
        begin                        // Scenario 2a (X to the right of A)
          r := Items[i];
          r.Last := Chr(Ord(_frag.First) - 1);
          Items[i] := r;
        end
      else if (_frag.First <= Items[i].First) and
              (_frag.Last  <  Items[i].Last) then
        begin                        // Scenario 2b (X to the left of A)
          r := Items[i];
          r.First := Chr(Ord(_frag.Last) + 1);
          Items[i] := r;
        end
      else if (_frag.First > Items[i].First) and
              (_frag.Last  < Items[i].Last) then
        begin                        // Scenario 5 (X completely within A)
          r := Items[i];
          r.First := Chr(Ord(_frag.Last) + 1);
          Insert(i+1,r);
          r := Items[i];
          r.Last := Chr(Ord(_frag.First) - 1);
          Items[i] := r;
        end
      else
        FParser.Monitor(ltInternal,'Logic has failed in TCharSet::SubtractFragment()');
    end;
end;

procedure TCharSet.SubtractRange(_a,_b: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _a;
  frag.Last  := _b;
  SubtractFragment(frag);
end;

procedure TCharSet.SubtractSet(_set: TCharSet);
var frag: TCharSetFragment;
begin
  for frag in _set do
    SubtractFragment(frag);
end;

{ TCharSetList32 }

constructor TCharSetList.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;             // Main parser
  FSLParser := TSLParser.Create;  // Secondary parser for set literals
end;

destructor TCharSetList.Destroy;
begin
  FSLParser.Free;
  inherited Destroy;
end;

function TCharSetList.AllocateCharSet(const _name: TString): TCharSet;
begin
  if IndexOfByName(_name) >= 0 then
    FParser.Monitor(ltInternal,'Attempting to add character set ' + _name +
                              ' which already exists');
  Result := TCharSet.Create(_name,FSLParser,FParser);
  Add(Result);
end;

function TCharSetList.AllocateCharSetTemp: TCharSet;
var name: string;
begin
  name := Format('$TEMP%4.4X',[FTempIndex]);
  Inc(FTempIndex);
  Result := AllocateCharSet(name);
  Result.FTemporary := True;
end;

procedure TCharSetList.DeleteUnwanted;
var i: integer;
begin
  FParser.Monitor(ltWarAndPeace,'Deleting unwanted character sets');
  for i := Count-1 downto 0 do
    if Items[i].Unwanted then
      begin
        FParser.Monitor(ltWarAndPeace,'  Deleting ' + Items[i].Name);
        Delete(i);
      end;
end;

procedure TCharSetList.Dump(_strm: TStream; _showtemp: boolean);
var cs: TCharSet;
begin
  WriteLnStringToStreamUnderlined(_strm,'CHARACTER SETS');
  for cs in Self do
    if (not cs.FTemporary) or _showtemp then
      WriteLnStringToStream(_strm,cs.AsText([cslName,cslBrackets,cslRanges,cslTranslate]));
  WriteLnStringToStream(_strm,'');
end;

procedure TCharSetList.DumpXML(_strm: TStream; _showtemp: boolean);
var cs: TCharSet;
begin
  WriteLnStringToStream(_strm,'  <CharSets>');
  for cs in Self do
    if (not cs.FTemporary) or _showtemp then
      WriteLnStringToStream(_strm,cs.AsText([cslRanges,cslTranslate,cslXML]));
  WriteLnStringToStream(_strm,'  </CharSets>');
end;

function TCharSetList.FindEquivalent(_set: TCharSet): TCharSet;
// Find a set where the list matches the _set parameter. Not ourselves,
// obviously. Return nil if we didn't have any luck. Don't return sets which
// have been marked as unwanted
var s: TCharSet;
begin
  Result := nil;
  for s in Self do
    if (s <> _set) and
       (not s.Unwanted) and
       s.Matches(_set)  then
    begin
      Result := s;
      Exit;
    end;
end;

function TCharSetList.FindByName(const _name: TString): TCharSet;
var index: integer;
begin
  Result := nil;
  index := IndexOfByName(_name);
  if index >= 0 then
    Result := Items[index];
end;

function TCharSetList.FindMustByName(const _name: TString): TCharSet;
begin
  Result := FindByName(_name);
  if Result = nil then
    FParser.Monitor(ltError,'Could not find character set ' + _name);
end;

function TCharSetList.IndexOfByName(const _name: TString): integer;
// Returns index position of the named character set or -1 if non-existent
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Name = _name then
      Exit(i);
end;

procedure TCharSetList.RemoveTemporary;
var i: integer;
begin
  for i := Count-1 downto 0 do
    if Items[i].FTemporary then
      Delete(i);
end;

end.

