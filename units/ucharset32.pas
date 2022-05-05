unit ucharset32;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, deployment_parser_module_12,
  lacogen12_setliteral_procs, deployment_parser_types_12;

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


  // The character set definition. This is made up of an ordered list of
  // fragments

  TCharSet = class
    protected
      FName:      TString;
      FParser:    TLCGParser;
      FSLParser:  TSLParser;
      FTemporary: boolean;
      FUnwanted:  boolean;
      FUsed:      boolean;
    public
      FSet:       set of char;
      constructor Create(const _name: TString; _parser: TSLParser; _mainparser: TLCGParser);
      procedure AddCharacter(_a: TChar);
      procedure AddRange(_a,_b: TChar);
      procedure AddSet(_set: TCharSet);
      function  AsText(_flags: TCSLSet = [cslBrackets,cslRanges,cslTranslate]): TString;
      function  GetFirstChar(var c: TChar): boolean;
      function  GetSetCount: integer;
      function  Matches(_set: TCharSet): boolean;
      procedure SetLiteral(const s: TString);
      procedure SubtractCharacter(_a: TChar);
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

operator in (_c32: TChar; _s32: TCharSet) b: boolean;

implementation

uses
  uparser_utility, uparser_exception, htmlelements;

{ Helper routines }

function CSListCompare(const a,b: TCharSet): integer;
begin
  if (a.FName < b.FName) then
    Result := -1
  else if (a.FName > b.FName) then
    Result := 1
  else
    Result := 0;
end;

operator in (_c32: TChar; _s32: TCharSet) b: boolean;
begin
  Exit(_c32 in _s32.FSet);
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
begin
  FSet := FSet + [_a];
end;

procedure TCharSet.AddRange(_a,_b: TChar);
var i: TChar;
begin
  for i := _a to _b do
    FSet := FSet + [i];
end;

procedure TCharSet.AddSet(_set: TCharSet);
begin
  FSet := FSet + _set.FSet;
end;

function TCharSet.AsText(_flags: TCSLSet): TString;
var c:    TChar;
    prefix, mid, suffix: string;
    hmc: integer;
    defeat: integer;

  // Count how many continuous characters are in the set starting with _from
  // Returns 0 if the _from character isn't in the set

  function HowManyContinuous(_from: TChar): integer;
  var done: boolean;
  begin
    Result := 0;
    done := False;
    while not done do
      begin
        if _from in FSet then
          begin
            Inc(Result);
            if _from < High(TSetOfChar) then
              Inc(_from)
            else
              done := True;
          end
        else
          done := True;
      end;
  end;

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
  defeat := 0;
  for c in TSetOfChar do
    begin
      if defeat > 0 then
        Dec(defeat)
      else
        begin
          hmc := HowManyContinuous(c);
          if (hmc >= 4) and (c > ' ') then
            begin
              mid := mid + MakePrintable(c);
              mid := mid + '-';
              mid := mid + MakePrintable(Chr(Ord(c)+hmc-1));
              defeat := hmc - 1;
            end
          else
            if (c in FSet) then
              mid := mid + MakePrintable(c);
        end;
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
  for c in TSetOfChar do
    if c in FSet then
      Exit;
  Result := False;
end;

function TCharSet.GetSetCount: integer;
var c: TChar;
begin
  Result := 0;
  for c in TSetOfChar do
    if c in FSet then
      Inc(Result);
end;

function TCharSet.Matches(_set: TCharSet): boolean;
begin
  Result := (_set.FSet = FSet);
end;

procedure TCharSet.SetLiteral(const s: TString);
begin
  TSLParser(FSLParser).MakeSetLiteral(Self,s);
end;

procedure TCharSet.SubtractCharacter(_a: TChar);
begin
  FSet := FSet - [_a];
end;

procedure TCharSet.SubtractRange(_a,_b: TChar);
var c, tmp: TChar;
begin
  if _a > _b then
    begin
      tmp := _a;
      _a  := _b;
      _b  := tmp;
    end;
  for c := _a to _b do
    FSet := FSet - [c];
end;

procedure TCharSet.SubtractSet(_set: TCharSet);
begin
  FSet := FSet - _set.FSet;
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

