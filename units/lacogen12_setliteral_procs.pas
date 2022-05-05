unit lacogen12_setliteral_procs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, deployment_parser_module_12,
  deployment_parser_types_12;

type
  TSLParser = class(TLCGParser)
    private
      ProcList: array of TLCGParserProc;
      function ActLitSetDefs({%H-}Parser: TLCGParser):             TLCGParserStackEntry;
      function ActLitSetEmpty({%H-}Parser: TLCGParser):            TLCGParserStackEntry;
      function ActLitIgnore({%H-}Parser: TLCGParser):              TLCGParserStackEntry;
      function ActLitLiteralRange({%H-}Parser: TLCGParser):        TLCGParserStackEntry;
      function ActLitLiteralSingle({%H-}Parser: TLCGParser):       TLCGParserStackEntry;
      function ActLitCharNonEscaped({%H-}Parser: TLCGParser):      TLCGParserStackEntry;
      function ActLitCharHex({%H-}Parser: TLCGParser):             TLCGParserStackEntry;
      function ActLitCharEscaped({%H-}Parser: TLCGParser):         TLCGParserStackEntry;
      function MyReduce({%H-}Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
    public
      constructor Create;
      destructor  Destroy; override;
      procedure MakeSetLiteral(cs: TObject; const s: string);
      procedure RegisterFuncs;
  end;


implementation

uses
  uparameters, uparser_exception,
  strutils, ucharset32;

var
  charset:        TCharSet;
  strm:           TStringStream;


constructor TSLParser.Create;
begin
  inherited Create;
  LoadFromResource('LACOGEN12_SETLITERAL');
  RegisterFuncs;
  OnReduce := @MyReduce;
end;

destructor TSLParser.Destroy;
begin
  inherited Destroy;
end;

function TSLParser.ActLitCharEscaped(Parser: TLCGParser): TLCGParserStackEntry;
var lastchar: TChar;
    slen:     integer;
begin
  lastchar := #0;
  slen := Length(ParserStack[ParserSP-1].Buf);
  if slen > 0 then
    lastchar := RightStr(ParserStack[ParserSP-1].Buf,1)[1];
  Result.Buf := '';
  if (lastchar = 'N') or (lastchar = 'n') then
    Result.Buf := '10'        // N/n = New line
  else if (lastchar = 'R') or (lastchar = 'r') then
    Result.Buf := '13'        // R/r = Carriage return
  else if (lastchar = 'T') or (lastchar = 't') then
    Result.Buf := '9'         // T/t = Tab
  else
    Result.Buf := IntToStr(Ord(lastchar)); // Anything else as itself
end;

function TSLParser.ActLitCharHex(Parser: TLCGParser): TLCGParserStackEntry;
var ival: integer;
begin
  ival := Hex2Dec(RightStr(ParserStack[ParserSP-1].Buf,Length(ParserStack[ParserSP-1].Buf)-2));
  Result.Buf := IntToStr(ival);
end;

function TSLParser.ActLitCharNonEscaped(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := IntToStr(Ord(ParserStack[ParserSP-1].Buf[1]));
end;

function TSLParser.ActLitIgnore(Parser: TLCGParser): TLCGParserStackEntry;
var empty: TString;
begin
  // Do nothing
  SetLength(empty,0);
  Result.Buf := empty;
end;

function TSLParser.ActLitLiteralRange(Parser: TLCGParser): TLCGParserStackEntry;
var rangefrom, rangeto: TChar;
begin
  rangefrom := Chr(StrToInt(ParserStack[ParserSP-3].Buf));
  rangeto   := Chr(StrToInt(ParserStack[ParserSP-1].Buf));
  charset.AddRange(rangefrom,rangeto);
  Result.Buf := '<LiteralDef>';
end;

function TSLParser.ActLitLiteralSingle(Parser: TLCGParser): TLCGParserStackEntry;
begin
//  CheckCharacterRange(ParserStack[ParserSP-1].Buf);
  charset.AddCharacter(Chr(StrToInt(ParserStack[ParserSP-1].Buf)));
  Result.Buf := '<LiteralDef>';
end;

function TSLParser.ActLitSetDefs(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '<Grammar>';
end;

function TSLParser.ActLitSetEmpty(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf := '<Grammar>';
end;

procedure TSLParser.MakeSetLiteral(cs: TObject; const s: string);
begin
  strm := TStringStream.Create(s);
  try
    if cs is TCharSet then
      charset := TCharSet(cs)
    else
      raise Exception.Create('Object is not character set');
    Parse(strm);
  finally
    strm.Free;
  end;
end;

function TSLParser.MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  if Assigned(ProcList[RuleIndex]) then
    Result := ProcList[RuleIndex](Self)
  else
    raise Exception.CreateFmt('No procedure for rule index %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TSLParser.RegisterFuncs;
  procedure ProcRegister(_funcname: string; _func: TLCGParserProc);
  var i: integer;
  begin
    for i := 0 to Rules-1 do
      if _funcname = RuleProcs[i] then
        ProcList[i] := _func;
  end;
begin
  SetLength(ProcList,Rules);
  ProcRegister('ActLitSetDefs',            @ActLitSetDefs);
  ProcRegister('ActLitSetEmpty',           @ActLitSetEmpty);
  ProcRegister('ActLitIgnore',             @ActLitIgnore);
  ProcRegister('ActLitLiteralRange',       @ActLitLiteralRange);
  ProcRegister('ActLitLiteralSingle',      @ActLitLiteralSingle);
  ProcRegister('ActLitCharNonEscaped',     @ActLitCharNonEscaped);
  ProcRegister('ActLitCharHex',            @ActLitCharHex);
  ProcRegister('ActLitCharEscaped',        @ActLitCharEscaped);
end;

end.

