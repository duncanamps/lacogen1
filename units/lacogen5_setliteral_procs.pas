unit lacogen5_setliteral_procs;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, ucharset, uparser_deployment;

// Forward declarations

procedure LCG_LITERAL_Error_Handler({%H-}_ptr: pointer; _logtype: TLogType; const _msg: string);
procedure LCG_MakeSetLiteral(cs: TCharSetObject; const s: string);

function ActLitSetNotDefs({%H-}_ptr: pointer):          TParserStackEntry;
function ActLitSetDefs({%H-}_ptr: pointer):             TParserStackEntry;
function ActLitSetEmpty({%H-}_ptr: pointer):            TParserStackEntry;
function ActLitIgnore({%H-}_ptr: pointer):              TParserStackEntry;
function ActLitLiteralRange({%H-}_ptr: pointer):        TParserStackEntry;
function ActLitLiteralSingle({%H-}_ptr: pointer):       TParserStackEntry;
function ActLitCharNonEscaped({%H-}_ptr: pointer):      TParserStackEntry;
function ActLitCharHex({%H-}_ptr: pointer):             TParserStackEntry;
function ActLitCharEscaped({%H-}_ptr: pointer):         TParserStackEntry;
function PROC_XXACCE_XGRAMM_XEOFX({%H-}_ptr: pointer):  TParserStackEntry;


implementation

uses
  lacogen5_setliteral_parser, uparameters, uparser_exception, uparser_types,
  strutils, lacogen5_setliteral_lexer;

var
  charset:        TCharSetObject;
  strm:           TStringStream;



procedure LCG_LITERAL_Error_Handler(_ptr: pointer; _logtype: TLogType; const _msg: string);
begin
  case _logtype of
    ltError:    raise LCGErrorException.Create(_msg);
    ltInternal: raise LCGInternalException.Create(_msg);
  end;
end;

procedure CheckCharacterRange(const s: string);
{$IFDEF CS_8BIT}
var v: word;
{$ENDIF}
begin
{$IFDEF CS_8BIT}
  v := StrToInt(s);
  if v > $ff then
    raise LCGErrorException.Create('Attempt to create a 16 bit character in an 8 bit codespace');
{$ENDIF}
end;

function ActLitCharEscaped(_ptr: pointer): TParserStackEntry;
begin
  case Rightstr(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf,1) of
    'n','N': Result.Buf := '10';
    'r','R': Result.Buf := '13';
    't','T': Result.Buf := '9';
    '-','[','\',']','^': Result.Buf := IntToStr(Ord(Rightstr(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf,1)[1]));
  end; // case
end;

function ActLitCharHex(_ptr: pointer): TParserStackEntry;
var ival: integer;
begin
  ival := Hex2Dec(RightStr(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf,Length(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf)-2));
  Result.Buf := IntToStr(ival);
end;

function ActLitCharNonEscaped(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := IntToStr(Ord(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf[1]));
end;

function ActLitIgnore(_ptr: pointer): TParserStackEntry;
begin
  // Do nothing
  Result.Buf := '';
end;

function ActLitLiteralRange(_ptr: pointer): TParserStackEntry;
begin
  CheckCharacterRange(LCG_LITERAL_Stack[LCG_LITERAL_SP-3].Buf);
  CheckCharacterRange(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf);
  charset.AddRange(TCharType(StrToInt(LCG_LITERAL_Stack[LCG_LITERAL_SP-3].Buf)),TCharType(StrToInt(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf)));
  Result.Buf := '<LiteralDef>';
end;

function ActLitLiteralSingle(_ptr: pointer): TParserStackEntry;
begin
  CheckCharacterRange(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf);
  charset.AddChar(TCharType(StrToInt(LCG_LITERAL_Stack[LCG_LITERAL_SP-1].Buf)));
  Result.Buf := '<LiteralDef>';
end;

function ActLitSetDefs(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := '<Grammar>';
end;

function ActLitSetEmpty(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := '<Grammar>';
end;

function ActLitSetNotDefs(_ptr: pointer): TParserStackEntry;
begin
  charset.OpNegate;
  Result.Buf := '<Grammar>';
end;

function PROC_XXACCE_XGRAMM_XEOFX(_ptr: pointer): TParserStackEntry;
begin
  charset.OpNegate;
  Result.Buf := '<Grammar>';
end;

procedure LCG_MakeSetLiteral(cs: TCharSetObject; const s: string);
begin
  strm := TStringStream.Create(s);
  try
    charset := cs;
    LCG_LITERAL_Parser(strm,@LCG_LITERAL_lexer_init,@LCG_LITERAL_lexer,nil);
  finally
    strm.Free;
  end;
end;

end.

