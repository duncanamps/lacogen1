unit test_expr_fp_procs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, deployment_parser_types, deployment_parser_module;

procedure lexer_debug(const _msg: string);
procedure parser_debug(const _msg: string);

function MyAbs(param: double): double;
function MyArcCos(param: double): double;
function MyArcSin(param: double): double;
function MyArcTan(param: double): double;
function MyCos(param: double): double;
function MyDegToRad(param: double): double;
function MyExp(param: double): double;
function MyFactorial(param: double): double;
function MyInt(param: double): double;
function MyLn(param: double): double;
function MyRadToDeg(param: double): double;
function MySin(param: double): double;
function MyTan(param: double): double;

function MyPower(x,y: double): double;

function procadd(Parser: TLCGParser):       TLCGParserStackEntry;
function procbrackets(Parser: TLCGParser):  TLCGParserStackEntry;
function proccopy(Parser: TLCGParser):      TLCGParserStackEntry;
function procdiv(Parser: TLCGParser):       TLCGParserStackEntry;
function procfunc1(Parser: TLCGParser):     TLCGParserStackEntry;
function procfunc2(Parser: TLCGParser):     TLCGParserStackEntry;
function prochelp(Parser: TLCGParser):      TLCGParserStackEntry;
function procid(Parser: TLCGParser):        TLCGParserStackEntry;
function procignore(Parser: TLCGParser):    TLCGParserStackEntry;
function proclet1(Parser: TLCGParser):      TLCGParserStackEntry;
function proclet2(Parser: TLCGParser):      TLCGParserStackEntry;
function procmul(Parser: TLCGParser):       TLCGParserStackEntry;
function procnumber(Parser: TLCGParser):    TLCGParserStackEntry;
function procpower(Parser: TLCGParser):     TLCGParserStackEntry;
function procprint(Parser: TLCGParser):     TLCGParserStackEntry;
function procsub(Parser: TLCGParser):       TLCGParserStackEntry;
function procunaryminus(Parser: TLCGParser): TLCGParserStackEntry;
function procunaryplus(Parser: TLCGParser): TLCGParserStackEntry;
function PROC_XXACCE_XCOMMA_XEOFX(Parser: TLCGParser): TLCGParserStackEntry;

implementation

uses
  Dialogs, ftestexprfp, math,
  strutils;

type
  TUpdateProc = procedure (const _msg: string);

  TFuncProc1 = function (_v: double): double;
  TFuncProc2 = function (_x,_y: double): double;

  TFuncEntry1 = record
    FuncName: string;
    FuncProc: TFuncProc1;
    FuncHelp: string;
  end;

  TFuncEntry2 = record
    FuncName: string;
    FuncProc: TFuncProc2;
    FuncHelp: string;
  end;

const
  FuncArray1: array[0..12] of TFuncEntry1 = (
    (FuncName: 'ABS';     FuncProc: @MyAbs;        FuncHelp: 'Absolute value of x, if x < 0 then it is converted into a positive number'),
    (FuncName: 'ACOS';    FuncProc: @MyArcCos;     FuncHelp: 'Arc (inverse) cosine of x with result in radians'),
    (FuncName: 'ASIN';    FuncProc: @MyArcSin;     FuncHelp: 'Arc (inverse) sine of x with result in radians'),
    (FuncName: 'ATAN';    FuncProc: @MyArcTan;     FuncHelp: 'Arc (inverse) tangent of x with result in radians'),
    (FuncName: 'COS';     FuncProc: @MyCos;        FuncHelp: 'Cosine of x where x is in radians'),
    (FuncName: 'DEG2RAD'; FuncProc: @MyDegToRad;   FuncHelp: 'Convert degrees to radians'),
    (FuncName: 'EXP';     FuncProc: @MyExp;        FuncHelp: 'Natural exponent of x, f=e^x'),
    (FuncName: 'FACT';    FuncProc: @MyFactorial;  FuncHelp: 'Factorial of x which must be an integer value'),
    (FuncName: 'INT';     FuncProc: @MyInt;        FuncHelp: 'Integer or truncated version of x with no fractional part'),
    (FuncName: 'LOG';     FuncProc: @MyLn;         FuncHelp: 'Natural log of x'),
    (FuncName: 'RAD2DEG'; FuncProc: @MyRadToDeg;   FuncHelp: 'Convert radians to degrees'),
    (FuncName: 'SIN';     FuncProc: @MySin;        FuncHelp: 'Sine of x where x is in radians'),
    (FuncName: 'TAN';     FuncProc: @MyTan;        FuncHelp: 'Tangent of x where x is in radians')
    );

  FuncArray2: array[0..0] of TFuncEntry2 = (
    (FuncName: 'POWER';   FuncProc: @MyPower;      FuncHelp: 'Power function x^y')
    );

procedure lexer_debug(const _msg: string);
begin
  DisplayUpdate(_msg);
end;

procedure parser_debug(const _msg: string);
begin
  DisplayUpdate(_msg);
end;

function MyAbs(param: double): double;
begin
  Result := Abs(param);
end;

function MyArcCos(param: double): double;
begin
  Result := ArcCos(param);
end;

function MyArcSin(param: double): double;
begin
  Result := ArcSin(param);
end;

function MyArcTan(param: double): double;
begin
  Result := ArcTan(param);
end;

function MyCos(param: double): double;
begin
  Result := Cos(param);
end;

function MyDegToRad(param: double): double;
begin
  Result := DegToRad(param);
end;

function MyExp(param: double): double;
begin
  Result := Exp(param);
end;

function MyFactorial(param: double): double;
var ival: integer;
    rval: double;
    i:    integer;
begin
  if Frac(param) <> 0.0 then
    raise Exception.Create('Attempt to calculate factorial of non-integer value');
  ival := Trunc(param+0.5);
  rval := 1;
  for i := 2 to ival do
    rval := rval * i;
  Result := rval;
end;

function MyInt(param: double): double;
begin
  Result := Int(param);
end;

function MyLn(param: double): double;
begin
  Result := Ln(param);
end;

function MyRadToDeg(param: double): double;
begin
  Result := RadToDeg(param);
end;

function MySin(param: double): double;
begin
  Result := Sin(param);
end;

function MyTan(param: double): double;
begin
  Result := Tan(param);
end;


{ f(x,y) functions }

function MyPower(x,y: double): double;
begin
  Result := Power(x,y);
end;


{ Main parser routines }

function procadd(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(StrToFloat(Parser.ParserStack[Parser.ParserSP-3].Buf) + StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
end;

function procbrackets(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := Parser.ParserStack[Parser.ParserSP-2].Buf;
end;

function proccopy(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := Parser.ParserStack[Parser.ParserSP-1].Buf;
end;

function procdiv(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(StrToFloat(Parser.ParserStack[Parser.ParserSP-3].Buf) / StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
end;

function procfunc1(Parser: TLCGParser): TLCGParserStackEntry;
var funcname: string;
    param:    double;
    i:        integer;
begin
  funcname := UpperCase(Parser.ParserStack[Parser.ParserSP-4].Buf);
  param    := StrToFloat(Parser.ParserStack[Parser.ParserSP-2].Buf);
  for i := Low(FuncArray1) to High(FuncArray1) do
    if FuncArray1[i].FuncName = funcname then
      begin
        Result.Buf := FloatToStr(FuncArray1[i].FuncProc(param));
        Exit;
      end;
  raise Exception.Create('Function ' + funcname + ' is not defined');
end;

function procfunc2(Parser: TLCGParser): TLCGParserStackEntry;
var funcname: string;
    paramx:   double;
    paramy:   double;
    i:        integer;
begin
  funcname := UpperCase(Parser.ParserStack[Parser.ParserSP-6].Buf);
  paramx   := StrToFloat(Parser.ParserStack[Parser.ParserSP-4].Buf);
  paramy   := StrToFloat(Parser.ParserStack[Parser.ParserSP-2].Buf);
  for i := Low(FuncArray2) to High(FuncArray2) do
    if FuncArray2[i].FuncName = funcname then
      begin
        Result.Buf := FloatToStr(FuncArray2[i].FuncProc(paramx,paramy));
        Exit;
      end;
  raise Exception.Create('Function ' + funcname + ' is not defined');
end;

function prochelp(Parser: TLCGParser): TLCGParserStackEntry;
var _update: TUpdateProc;
    i:       integer;
    prefixlen: integer;
begin
  Parser.Monitor(ltInfo,'HELP');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'Basic commands');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'  PRINT expression, e.g. PRINT 5 or PRINT SIN(DEG2RAD(45))');
  Parser.Monitor(ltInfo,'  ? expression, same as above');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'  LET variablename = expression, e.g. LET x = 5');
  Parser.Monitor(ltInfo,'  variablename = expression, shortcut for the above');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'Operators listed in order of precedence');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'  ^ power');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'  * multiply');
  Parser.Monitor(ltInfo,'  / divide');
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'  + addition');
  Parser.Monitor(ltInfo,'  - subtraction');
  Parser.Monitor(ltInfo,'');
  // Calculate longest function name
  prefixlen := 0;
  for i := Low(FuncArray1) to High(FuncArray1) do
    if Length(FuncArray1[i].FuncName) > prefixlen then
      prefixlen := Length(FuncArray1[i].FuncName);
  // The print first batch of function names
  Parser.Monitor(ltInfo,'FUNCTIONS f(x)');
  for i := Low(FuncArray1) to High(FuncArray1) do
    Parser.Monitor(ltInfo,PadRight(FuncArray1[i].FuncName+'(x)',prefixlen+4) +
            FuncArray1[i].FuncHelp);
  // f(x,y) functions next
  prefixlen := 0;
  for i := Low(FuncArray2) to High(FuncArray2) do
    if Length(FuncArray2[i].FuncName) > prefixlen then
      prefixlen := Length(FuncArray2[i].FuncName);
  Parser.Monitor(ltInfo,'');
  Parser.Monitor(ltInfo,'FUNCTIONS f(x,y)');
  for i := Low(FuncArray2) to High(FuncArray2) do
    Parser.Monitor(ltInfo,PadRight(FuncArray2[i].FuncName+'(x,y)',prefixlen+6) +
            FuncArray2[i].FuncHelp);
  Result.Buf := '';
end;

function procid(Parser: TLCGParser): TLCGParserStackEntry;
var v: double;
begin
  v := 0;
  GetVar(Parser.ParserStack[Parser.ParserSP-1].Buf,v);
  Result.Buf := FloatToStr(v);
end;

function procignore(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := '';
end;

function proclet1(Parser: TLCGParser): TLCGParserStackEntry;
begin
  AssignVar(Parser.ParserStack[Parser.ParserSP-3].Buf,StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
  Result.Buf := Parser.ParserStack[Parser.ParserSP-1].Buf;
end;

function proclet2(Parser: TLCGParser): TLCGParserStackEntry;
begin
  AssignVar(Parser.ParserStack[Parser.ParserSP-3].Buf,StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
  Result.Buf := Parser.ParserStack[Parser.ParserSP-1].Buf;
end;

function procmul(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(StrToFloat(Parser.ParserStack[Parser.ParserSP-3].Buf)*StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
end;

function procnumber(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := proccopy(Parser);
end;

function procpower(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(Power(StrToFloat(Parser.ParserStack[Parser.ParserSP-3].Buf),StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf)));
end;

function procprint(Parser: TLCGParser): TLCGParserStackEntry;
var _update: TUpdateProc;
begin
  _update := TUpdateProc(Parser);
  Parser.Monitor(ltInfo,Parser.ParserStack[Parser.ParserSP-1].Buf);
  Result.Buf := Parser.ParserStack[Parser.ParserSP-1].Buf;
end;

function procsub(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(StrToFloat(Parser.ParserStack[Parser.ParserSP-3].Buf) - StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
end;

function procunaryminus(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := FloatToStr(-StrToFloat(Parser.ParserStack[Parser.ParserSP-1].Buf));
end;

function procunaryplus(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result := proccopy(Parser);
end;

function PROC_XXACCE_XCOMMA_XEOFX(Parser: TLCGParser): TLCGParserStackEntry;
begin
  Result.Buf   := Parser.ParserStack[Parser.ParserSP-2].Buf;
end;


end.

