unit ftestexprfp;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, fgl,
  deployment_parser_module, deployment_parser_types;

type

    TVariable = record
      VarName:  string;
      VarValue: double;
      class Operator = (A,B : TVariable): boolean;
    end;

    TVariableList = class(specialize TFPGList<TVariable>)

    end;

    TMyReduceFunc = function (Parser: TLCGParser): TLCGParserStackEntry;


  { TForm1 }

  TForm1 = class(TForm)
    btnGo: TButton;
    edtInstruction: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    StringGrid1: TStringGrid;
    procedure btnGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    LCGParser: TLCGParser;
    ReduceProcs: array of TMyReduceFunc;
    procedure MonitorProc(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
  public
    Variables: TVariableList;
    procedure Msg(const _msg: string);
    function  Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
    procedure UpdateVarGrid;
  end;

const
  init_vars: array[0..1] of TVariable =
    ((VarName: 'e';  VarValue: 2.7182818284590452353602874713527),
     (VarName: 'pi'; VarValue: 3.1415926535897932384626433832795)
    );

var
  Form1: TForm1;


procedure AssignVar(const _varname: string; _varvalue: double);
procedure GetVar(const _varname: string; var _varvalue: double);
procedure DisplayUpdate(const _msg: string);

implementation

{$R *.lfm}

uses
  test_expr_fp_procs;

class Operator TVariable.= (A,B : TVariable) : boolean;
begin
  Result := (A.VarName = B.VarName);
end;

procedure AssignVar(const _varname: string; _varvalue: double);
var index: integer;
    v:     TVariable;
begin
  v.VarName  := _varname;
  v.VarValue := _varvalue;
  // If the variable doesn't exist, create it otherwise do an update
  index := Form1.Variables.IndexOf(v);
  if index < 0 then
    begin
      Form1.Variables.Add(v);
    end
  else // Update
    begin
      Form1.Variables[index] := v;
    end;
  Form1.UpdateVarGrid;
end;

procedure GetVar(const _varname: string; var _varvalue: double);
var index: integer;
    v:     TVariable;
begin
  v.VarName  := _varname;
  v.VarValue := _varvalue;
  index := Form1.Variables.IndexOf(v);
  if index < 0 then
    raise Exception.Create('Variable ' + _varname + ' not defined');
  _varvalue := Form1.Variables.Items[index].VarValue;
end;

procedure DisplayUpdate(const _msg: string);
begin
  Form1.Msg(_msg);
end;

procedure parser_debug(const _msg: string);
begin
  DisplayUpdate(_msg);
end;

function CompareFunc(const Item1, Item2: TVariable): Integer;
begin
  if Item1.VarName = Item2.VarName then
    Result := 0
  else if Item1.VarName < Item2.VarName then
    Result := -1
  else
    Result := 1;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var v: TVariable;
    procnames: TStringArray;

  procedure MakeSetProc(const _procname: string; _reduce: TMyReduceFunc);
  var i: integer;
      changed: boolean;
  begin
    changed := False;
    for i := 0 to LCGParser.Rules-1 do
      if procnames[i] = _procname then
        begin
          ReduceProcs[i] := _reduce;
          changed := True;
        end;
    if not changed then
      raise Exception.Create('Procedure named ' + _procname + ' not found');
  end;

begin
  // Parser setup
  LCGParser := TLCGParser.Create;
  LCGParser.OnMonitor := @MonitorProc;
  LCGParser.OnReduce  := @Reduce;
  LCGParser.LoadFromResource('TEST_EXPR_FP');

  // Set up the procs
  SetLength(ReduceProcs,LCGParser.Rules);
  procnames := LCGParser.RuleProcs;
  MakeSetProc('procadd',        @procadd);
  MakeSetProc('procbrackets',   @procbrackets);
  MakeSetProc('proccopy',       @proccopy);
  MakeSetProc('procdiv',        @procdiv);
  MakeSetProc('procfunc1',      @procfunc1);
  MakeSetProc('procfunc2',      @procfunc2);
  MakeSetProc('prochelp',       @prochelp);
  MakeSetProc('procid',         @procid);
  MakeSetProc('procignore',     @procignore);
  MakeSetProc('proclet1',       @proclet1);
  MakeSetProc('proclet2',       @proclet2);
  MakeSetProc('procmul',        @procmul);
  MakeSetProc('procnumber',     @procnumber);
  MakeSetProc('procpower',      @procpower);
  MakeSetProc('procprint',      @procprint);
  MakeSetProc('procsub',        @procsub);
  MakeSetProc('procunaryminus', @procunaryminus);
  MakeSetProc('procunaryplus',  @procunaryplus);
//  MakeSetProc('PROC_XXACCE_XCOMMA_XEOFX',  @PROC_XXACCE_XCOMMA_XEOFX);

  // FP specific stuff
  Variables := TVariableList.Create;
  for v in init_vars do
    Variables.Add(v);
  UpdateVarGrid;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Variables.Free;
  LCGParser.Free;
end;

procedure TForm1.btnGoClick(Sender: TObject);
var strm: TStringStream;
begin
  strm := TStringStream.Create(edtInstruction.Text);
  try
    try
      LCGParser.InitRun;
      LCGParser.LexBufBlock := 512;
      LCGParser.Parse(strm);
  //    FP_parser(strm,@FP_lexer_init,@FP_lexer,@DisplayUpdate);
      edtInstruction.SelectAll;
    except
      // Silent exception, we deal with the problem through the message system
    end;
  finally
    strm.Free;
  end;
end;

procedure TForm1.MonitorProc(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
begin
  Memo1.Lines.Add(Message);
end;

procedure TForm1.Msg(const _msg: string);
begin
  Memo1.Lines.Add(_msg);
end;

function TForm1.Reduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  if Assigned(ReduceProcs[RuleIndex]) then
    Result := ReduceProcs[RuleIndex](Parser)
  else
    raise Exception.CreateFmt('Rule code not present for rule %d (%s)',[RuleIndex,Parser.RuleProcs[RuleIndex]]);
end;

procedure TForm1.UpdateVarGrid;
var v: TVariable;
    r: integer;
begin
  Variables.Sort(@CompareFunc);
  StringGrid1.BeginUpdate;
  try
    while StringGrid1.RowCount > 1 do
      StringGrid1.DeleteRow(1);
//    StringGrid1.InsertRowWithValues(0,['Var','Value']);
    r := 1;
    for v in Variables do
      begin
        StringGrid1.InsertRowWithValues(r,[v.VarName,FloatToStr(v.VarValue)]);
        Inc(r);
      end;
  finally
    StringGrid1.EndUpdate;
  end;
end;

end.

