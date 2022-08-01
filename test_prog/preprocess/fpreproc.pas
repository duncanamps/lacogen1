unit fpreproc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, upreproc,
  deployment_parser_module_13, deployment_parser_types_13;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure MyMonitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
    procedure MyToken(Parser: TLCGParser; Token: TToken);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  uparser_exception;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var pp: TPreParser;
    istream: TFileStream;
    ostream: TStringStream;
    s:       string;
begin
  Memo1.Lines.Delimiter := #10;
  Memo1.Lines.StrictDelimiter := True;
  s := '';
  pp := TPreParser.Create;
  pp.OnMonitor := @MyMonitor;
  pp.OnToken   := @MyToken;
  istream := TFileStream.Create('C:\Users\Duncan Munro\Dropbox\dev\lazarus\lacogen1\test\v1.3\defines.lac',fmOpenRead);
  ostream := TStringStream.Create(s);
  try
    pp.Parse(istream,ostream);
    pp.DefineList.Sort;
    pp.DefineList.Dump(ostream);
    Memo1.Lines.DelimitedText := ostream.DataString;
  finally
    FreeAndNil(ostream);
    FreeAndNil(istream);
    FreeAndNil(pp);
  end;
end;

procedure TForm1.MyMonitor(Parser: TLCGParser; LogType: TLCGLogType; const Message: string);
begin
  Memo1.Lines.Add(Format('%s [%d,%d]: %s',[LogTitles[LogType],Parser.InputLine,Parser.InputColumn,Message]));
end;

procedure TForm1.MyToken(Parser: TLCGParser; Token: TToken);
var id: integer;
begin
  id := Token.ID;
  Memo2.Lines.Add(Format('[%d,%d] #%d (%s): %s',[Token.Row,Token.Col,id,Parser.Tokens[id].Name,Token.Buf]));
end;

end.

