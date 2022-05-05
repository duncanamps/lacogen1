unit ucmdoptions;

{$mode objfpc}{$H+}

//
// Deal with the command line options
//



interface

uses
  Classes, SysUtils;
type
  TProgOptions = (poCompile,poDump,poHelp,poVerbose,poWarAndPeace,poXML);

  TOptionParam = (opNone,opOptional,opMandatory);

  TOptions = record
    ShortOption: string;
    LongOption:  string;
    ParamReqd:   TOptionParam;
    Parameter:   string;
    Description: string;
  end;

const
  MAX_SCREEN_WIDTH = 80;

  OPTION_PARAM_DESC: array[TOptionParam] of string =
    ('','::',':');

  CMD_OPTIONS: array[TProgOptions] of TOptions =
    ((ShortOption: 'c';
      LongOption:  'compile';
      ParamReqd:   opOptional;
      Parameter:   'file';
      Description: 'Generate the DFA and LALR tables to a .lacobj file'),

     (ShortOption: 'd';
      LongOption:  'dump';
      ParamReqd:   opOptional;
      Parameter:   'file';
      Description: 'Dump of all generated info to .txt file'),

     (ShortOption: 'h';
      LongOption:  'help';
      ParamReqd:   opNone;
      Parameter:   '';
      Description: 'Display this help listing'),

     (ShortOption: 'v';
      LongOption:  'verbose';
      ParamReqd:   opNone;
      Parameter:   '';
      Description: 'Display more information about the activities'),

     (ShortOption: 'w';
      LongOption:  'warandpeace';
      ParamReqd:   opNone;
      Parameter:   '';
      Description: 'Display MUCH more information about the activities'),

     (ShortOption: 'x';
      LongOption:  'xml';
      ParamReqd:   opOptional;
      Parameter:   'file';
      Description: 'Generate .xml file containing parser tables')

      );

var
  gShortOptions:     string;
  gLongOptions:      string;
  gLongOptionsArray: array[TProgOptions] of string;


function  PullString(var s: string; width: integer = MAX_SCREEN_WIDTH): string;
procedure ShowOptions;



implementation

uses
  strutils, uparser_exception, deployment_parser_types_12;

var
  tmp_sl: TStringList;
  iter:   TProgOptions;


function LongOptionDesc(index: TProgOptions): string;
begin
  Result := '--' + CMD_OPTIONS[index].LongOption;
  case CMD_OPTIONS[index].ParamReqd of
    opNone:       ;  // Do nothing
    opOptional:   Result := Result + '[=' + CMD_OPTIONS[index].Parameter + ']';
    opMandatory:  Result := Result + '=' + CMD_OPTIONS[index].Parameter;
  end; // Case
end;

function NextChar(const s: string): char;
begin
  if Length(s) = 0 then
    raise LCGInternalException.Create('Call to NextChar with empty parameter');
  NextChar := s[1];
end;

function PullChar(var s: string): char;
begin
  Result := NextChar(s);
  s := RightStr(s,Length(s)-1);
end;

function NextWord(s: string): string;
begin
  Result := '';
  while (Length(s) > 0) and (NextChar(s) = ' ') do
    Result := Result + PullChar(s{%H-});
  if Length(s) = 0 then
    exit
  else
    while (Length(s) > 0) and (NextChar(s) <> ' ') do
      Result := Result + PullChar(s);
end;

procedure PullWhiteSpace(var s: string);
begin
  while (Length(s) > 0) and (NextChar(s) = ' ') do
    PullChar(s);
end;

function PullWord(var s: string): string;
begin
  Result := NextWord(s);
  s := RightStr(s, Length(s) - Length(Result));
end;

function PullString(var s: string; width: integer = MAX_SCREEN_WIDTH): string;
var myword:   string;
    done:     boolean;
begin
  Result := '';
  if Length(s) = 0 then
    raise LCGInternalException.Create('Call to PullString with empty parameter');
  done := False;
  while (Length(s) > 0) and (not done) do
    begin
      myword := NextWord(s);
      // Three cases to process here
      // 1. The existing result + word is within the width
      // 2. The existing result + word is over the width, spill to new line
      // 3. The result is empty and word is over the width, raise exception
      if (Result = '') and (Length(myword) > width) then // situation 3
        raise LCGInternalException.Create('Word to long for space available');
      if (Length(result) + Length(myword)) > width then
        begin // situation 2, spill to a new line
          done := True;
          PullWhitespace(s);
        end
      else
        Result := Result + PullWord(s);
    end;
end;

function ShortOptionDesc(index: TProgOptions): string;
begin
  Result := '-' + CMD_OPTIONS[index].ShortOption;
  case CMD_OPTIONS[index].ParamReqd of
    opNone:       ; // Do nothing
    opOptional:   Result := Result + ' [' + CMD_OPTIONS[index].Parameter + ']';
    opMandatory:  Result := Result + ' ' + CMD_OPTIONS[index].Parameter;
  end; // Case
end;

procedure ShowOptions; // Display the options
var col1,col2,col3: integer; // The columns for short, long, description
    indent: integer;         // The indent size for 2nd rows onwards
    s:      string;
begin
  col1 := 0;
  col2 := 0;
  col3 := 0;
  // Pass through to get the maximum lengths of each column
  for iter in TProgOptions do
    begin
      if Length(ShortOptionDesc(iter)) > col1 then
        col1 := Length(ShortOptionDesc(iter));
      if Length(LongOptionDesc(iter)) > col2 then
        col2 := Length(LongOptionDesc(iter));
      indent := col1 + col2 + 2;
      col3 := MAX_SCREEN_WIDTH - indent;
      if col3 < 1 then
        raise LCGInternalException.Create('Help file options too wide for screen');
    end;
  // Now we have all the column widths, go round again and print
  for iter in TProgOptions do
    begin
      write(PadRight(ShortOptionDesc(iter),col1) + ' ');
      write(PadRight(LongOptionDesc(iter),col2) + ' ');
      s := CMD_OPTIONS[iter].Description;
      writeln(PullString(s,col3));
      while Length(s) > 0 do
        writeln(StringOfChar(' ',indent) + PullString(s));
    end;
end;

initialization

  // Construct all the options arrays and strings
  gShortOptions := '';
  gLongOptions  := '';
  for iter in TProgOptions do
    begin
      gShortOptions := gShortOptions + CMD_OPTIONS[iter].ShortOption +
                       OPTION_PARAM_DESC[CMD_OPTIONS[iter].ParamReqd];
      gLongOptionsArray[iter] := CMD_OPTIONS[iter].LongOption +
                       OPTION_PARAM_DESC[CMD_OPTIONS[iter].ParamReqd];
    end;
  tmp_sl := TStringList.Create;
  try
    tmp_sl.AddStrings(gLongOptionsArray);
    tmp_sl.Delimiter := ' ';
    gLongOptions := tmp_sl.DelimitedText;
  finally
    tmp_sl.Free;
  end;


end.

