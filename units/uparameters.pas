unit uparameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uparser_utility;

type

  PTBoolean = ^boolean;
  PTInteger = ^integer;
  PTString  = ^string;

  // All parameters are stored as strings

  TParameter = class(TObject)
    protected
      FName:  string;  // Parameter name
      FValue: string;  // Parameter value
      FPVar:  pointer; // The pointer to the associated variable
      function ReadAsBoolean: boolean; virtual;
      function ReadAsInteger: integer; virtual;
      function ReadAsNonTerminal: string; virtual;
      function ReadAsString: string; virtual;
      procedure SetExternalVar; virtual; abstract;
      procedure WriteAsBoolean(_p: boolean); virtual;
      procedure WriteAsInteger(_p: integer); virtual;
      procedure WriteAsNonTerminal(const _p: string); virtual;
      procedure WriteAsString(const _p: string); virtual;
    public
      constructor Create(const _name: string; const _value: string; _pvar: pointer = nil);
      property AsBoolean:     boolean     read ReadAsBoolean     write WriteAsBoolean;
      property AsInteger:     integer     read ReadAsInteger     write WriteAsInteger;
      property AsNonTerminal: string read ReadAsNonTerminal write WriteAsNonTerminal;
      property AsString:      string read ReadAsString      write WriteAsString;
      property Name:          string read FName;
      property Value:         boolean     read ReadAsBoolean;
  end;

  TParameterBoolean = class(TParameter)
    protected
      procedure SetExternalVar; override;
      procedure WriteAsString(const _p: string); override;
    public
      property Value: boolean read ReadAsBoolean write WriteAsBoolean;
  end;

  TParameterInteger = class(TParameter)
    protected
      procedure SetExternalVar; override;
    public
      property Value: integer read ReadAsInteger write WriteAsInteger;
  end;

  TParameterNonTerminal = class(TParameter)
    protected
      procedure SetExternalVar; override;
    public
      property Value: string read ReadAsNonTerminal write WriteAsNonTerminal;
  end;

  TParameterString = class(TParameter)
    protected
      procedure SetExternalVar; override;
    public
      property Value: string read ReadAsString write WriteAsString;
  end;

  TParameterList = class(specialize TFPGObjectList<TParameter>)
    public
      procedure Dump(_stream: TStream);
      procedure DumpXML(_stream: TStream);
      procedure DumpSL(_sl: TStringList);
      function  IndexOf(const _name: string): integer;
      function  Parameter(const _name: string): TParameter;
  end;


var
  gParameterList: TParameterList; // Global parameters


implementation

uses
  uparser_exception, strutils, uparser_rule, htmlelements,
  deployment_parser_types_12;

// TParameter code

constructor TParameter.Create(const _name: string; const _value: string; _pvar: pointer);
begin
  // Make sure name doesn't exist already
  if gParameterList.IndexOf(UpperCase(_name)) >= 0 then
    raise LCGInternalException.Create('Parameter ' + AnsiString(_name) + ' already exists');
  // Now add the name
  inherited Create;
  FName  := UpperCase(_name);
  FValue := _value;
  FPVar := _pvar;
end;

function TParameter.ReadAsBoolean: boolean;
begin
  ReadAsBoolean := (FValue = string('TRUE'));
end;

function TParameter.ReadAsInteger: integer;
begin
  ReadAsInteger := StrToInt(FValue);
end;

function TParameter.ReadAsNonTerminal: string;
begin
  ReadAsNonTerminal := FValue;
end;

function TParameter.ReadAsString: string;
begin
  ReadAsString := FValue;
end;

procedure TParameter.WriteAsBoolean(_p: boolean);
begin
  if _p = True then
    FValue := 'TRUE'
  else
    FValue := 'FALSE';
  SetExternalVar;
end;

procedure TParameter.WriteAsInteger(_p: integer);
begin
  FValue := IntToStr(_p);
  SetExternalVar;
end;

procedure TParameter.WriteAsNonTerminal(const _p: string);
begin
  FValue := _p;
  SetExternalVar;
end;

procedure TParameter.WriteAsString(const _p: string);
begin
  FValue := _p;
  SetExternalVar;
end;

// TParameterBoolean code

procedure TParameterBoolean.SetExternalVar;
begin
  if FPVar <> nil then
    PTBoolean(FPVar)^ := AsBoolean;
end;

procedure TParameterBoolean.WriteAsString(const _p: string);
begin
  inherited WriteAsString(UpperCase(_p));
end;

// TParameterInteger code

procedure TParameterInteger.SetExternalVar;
begin
  if FPVar <> nil then
    PTInteger(FPVar)^ := AsInteger;
end;

// TParameterNonTerminal code

procedure TParameterNonTerminal.SetExternalVar;
begin
  if FPVar <> nil then
    PTString(FPVar)^ := AsString;
end;

// TParameterString code

procedure TParameterString.SetExternalVar;
begin
  if FPVar <> nil then
    PTString(FPVar)^ := AsString;
end;


// TParameterList code

procedure TParameterList.Dump(_stream: TStream);
var i: integer;
    longest: integer; // The longest parameter name
begin
  WriteLnStringToStreamUnderlined(_stream,'PARAMETER LIST');
  // First calculate the longest name
  longest := 0;
  for i := 0 to Count-1 do
    if Length(Items[i].Name) > longest then
      longest := Length(Items[i].Name);
  // No dump the values
  for i := 0 to Count-1 do
    WriteLnStringToStream(_stream,PadRight(AnsiString(Items[i].Name),longest) + ' = ' + AnsiString(Items[i].AsString));
  // Finally a blank line at end
  WriteLnStringToStream(_stream,'');
end;

procedure TParameterList.DumpXML(_stream: TStream);
var i: integer;
begin
  WriteLnStringToStream(_stream,'  <Parameters>');
  // Now dump the values
  for i := 0 to Count-1 do
    WriteLnStringToStream(_stream,'    <Parameter><Name>' + EscapeHtml(Items[i].Name) + '</Name><Value>' + EscapeHtml(Items[i].AsString) +'</Value></Parameter>');
  // Finally...
  WriteLnStringToStream(_stream,'  </Parameters>');
end;

procedure TParameterList.DumpSL(_sl: TStringList);
var strm: TStringStream;
    s:    string;
begin
  strm := TStringStream.Create('');
  try
    Dump(strm);
    strm.Position := 0;
    while strm.Position < strm.Size do
      begin
        s := strm.ReadAnsiString(-1);
        s := LeftStr(s,Length(s)-1); // Remove the LF from the end
        _sl.Add(AnsiString(s));
      end;
  finally
    strm.Free;
  end;
end;

function TParameterList.IndexOf(const _name: string): integer;
var i: integer;
begin
  IndexOf := -1; // Assume not found for now
  i := 0;
  while i < Count do
    if Items[i].FName = _name then
      begin
        IndexOf := i;
        break;
      end
    else
      Inc(i);
end;

function TParameterList.Parameter(const _name: string): TParameter;
var i: integer;
begin
  i := IndexOf(UpperCase(_name));
  if i < 0 then
    raise LCGErrorException.Create('Parameter ' + AnsiString(_name) + ' not defined');
  Parameter := Items[i];
end;

initialization
  gParameterList := TParameterList.Create(True);
  // Boolean parameters
//  gParameterList.Add(TParameterBoolean.Create('%FASTLEXER',     'TRUE'));
//  gParameterList.Add(TParameterBoolean.Create('%FASTPARSER',    'TRUE'));
  gParameterList.Add(TParameterBoolean.Create('%COMMENTNESTED', 'TRUE'));
  // String parameters
  gParameterList.Add(TParameterString.Create('%AUTHOR',         ''));
  gParameterList.Add(TParameterString.Create('%CODEPREFIX',     ''));
  gParameterList.Add(TParameterString.Create('%COPYRIGHT',      ''));
  gParameterList.Add(TParameterString.Create('%LICENCE',        ''));
  gParameterList.Add(TParameterString.Create('%TITLE',          ''));
  gParameterList.Add(TParameterString.Create('%UNITLEXER',      ''));
  gParameterList.Add(TParameterString.Create('%UNITPARSER',     ''));
  gParameterList.Add(TParameterString.Create('%VERSION',        ''));
  // Integer parameters
  gParameterList.Add(TParameterInteger.Create('%LEXERBUFFER',   '4096'));
  gParameterList.Add(TParameterInteger.Create('%LEXERTAB',      '4'));
  gParameterList.Add(TParameterInteger.Create('%PARSERBUFFER',  '1024'));
  // Non-terimal parameters
  gParameterList.Add(TParameterNonTerminal.Create('%START',          '', @startnonterminal));

finalization
  gParameterList.Free;

end.

