program lacogen12;

{
    LaCoGen - LAzarus COmpiler GENerator
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, uparser, ucmdoptions, uparser_exception,
  uparser_types, uparser_utility, uparameters,
  uparser_canonical, uparser_expanded, uparser_lalrsets,
  uparser_nfa, uparser_nfalist, uparser_nfaset, uparser_nfastate,
  uparser_output, uparser_rule, uparser_terminal,
  uparser_dfalist, ucharset32, deployment_parser_types_12,
  deployment_parser_module_12;


type

  { TLaCoGen }

  TLaCoGen = class(TCustomApplication)
  protected
    FCompiledFilename:   string;
    FLongOptions:        string;
    FParser:             TLacParser;
    FSourceFilename:     string;
    FXMLFilename:        string;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Monitor(_lt: TLCGLogType; _elapsed: TDateTime; const _msg: string; const _context: string; _line, _col: integer);
    procedure Monitor2(Parser: TLCGParser; _lt: TLCGLogType; const _msg: string);
    procedure OutputTitle;
    procedure ProcessParameters;
    procedure ProcessFN(shortoption: char; const longoption: string;
                        const extension: string; var filename: string);
    procedure WriteHelp; virtual;
  end;

{ TLaCoGen }

constructor TLaCoGen.Create(TheOwner: TComponent);
var sl: TStringList;
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  // Convert the options string array into a single string
  sl := TStringList.Create;
  try
    sl.AddStrings(gLongOptionsArray);
    sl.Delimiter := ' ';
    FLongOptions := sl.DelimitedText;
  finally
    sl.Free;
  end;
  // Set up parser table
  FParser := TLacParser.Create;
  FParser.OnMonitor := @Monitor2;
end;

destructor TLaCoGen.Destroy;
begin
  FParser.Free;
  inherited Destroy;
end;

procedure TLaCoGen.Monitor(_lt: TLCGLogType; _elapsed: TDateTime; const _msg: string; const _context: string; _line, _col: integer);
var s: string;
begin
  if _line <= 0 then
    s := Format('%7s %7.3f: %s %s',
                [LogTitles[_lt],_elapsed,_msg,_context])
  else
    s := Format('%7s %7.3f [%' + IntToStr(DIGITS_LINE) + 'd,%' +
                IntToStr(DIGITS_COLUMN) + 'd]: %s %s',
                [LogTitles[_lt],_elapsed,_line,_col,_msg,_context]);
  WriteLn(s);
end;

procedure TLaCoGen.Monitor2(Parser: TLCGParser; _lt: TLCGLogType; const _msg: string);
begin
  Monitor(_lt,(Now()-Parser.StartTime)*86400.0,_msg,'',Parser.InputLine,Parser.InputColumn);
end;

procedure TLaCoGen.DoRun;
begin
  // Title message
  OutputTitle;
  // Process the parameters
  ProcessParameters;

  { add your program here }
  try
  if not Terminated then
    begin
      try
        FParser.ProcessFile;
        FParser.CSList.Sort(@CSListCompare);
        // Compute stuff
        FParser.Monitor(ltVerbose,'Minimising character sets');
        FParser.MinimiseCharSets;
        FParser.ComputeDFA;
        // Create the dollar rule
        FParser.Monitor(ltVerbose,'Creating parent rule ' + ACCEPT_CONDITION);
        FParser.CreateDollarRule;
        // Create follow (first/last/follow) sets
        FParser.Monitor(ltVerbose,'Creating follow sets');
        FParser.CreateFollowSets;
        // Create expanded rule list
        FParser.Monitor(ltVerbose,'Creating rule table');
        FParser.CreateExpandedList;
        // Create canonical table from the expanded rule list
        FParser.Monitor(ltVerbose,'Creating LALR canonical table');
        FParser.CreateParserTable;
        // Check for Shift/Reduce conflict
        FParser.Monitor(ltVerbose,'Checking for Shift/Reduce conflicts');
        FParser.CheckSR;
        // Check for Reduce/Reduce conflict
        FParser.Monitor(ltVerbose,'Checking for Reduce/Reduce conflicts');
        FParser.CheckRR;
        // Create the output table from the parser table
        FParser.Monitor(ltVerbose,'Creating parser output matrix');
        FParser.CreateParserOutput;
        // Check for stuff not used
        FParser.CheckCsNotUsed;
        FParser.CheckSymbolsNotUsed;
        FParser.CheckRulesNotUsed;
        // Create compiled output if needed
        if HasOption('c','compile') then
          begin
            FParser.Monitor(ltVerbose,'Creating compiled object file');
            FParser.Compile(FCompiledFilename);
          end;
        // Dump text and/or XML if required
        FParser.Dump;
        FParser.DumpXML;
        FParser.Monitor(ltInfo,'Processing completed');
        FParser.Monitor(ltWarAndPeace,Format('%6d source lines',[FParser.LineMax]));
        FParser.Monitor(ltWarAndPeace,Format('%6d character sets',[FParser.CSList.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d symbols',[FParser.TerminalList.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d NFA records',[FParser.NFAList.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d DFA records',[FParser.DFAList.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d First set records containing %d items',[FParser.FirstSetList.Count, FParser.FirstSetList.TotalItems]));
        FParser.Monitor(ltWarAndPeace,Format('%6d Last set records containing %d items',[FParser.LastSetList.Count,  FParser.LastSetList.TotalItems]));
        FParser.Monitor(ltWarAndPeace,Format('%6d Follow set records containing %d items',[FParser.FollowSetList.Count,FParser.FollowSetList.TotalItems]));
        FParser.Monitor(ltWarAndPeace,Format('%6d Rule entries with %d expanded',[FParser.RuleList.Count,FParser.ExpandedList.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d Canonical table records',[FParser.CanonicalTable.Count]));
        FParser.Monitor(ltWarAndPeace,Format('%6d Output table records',[FParser.ParserOutput.Count]));
      except
        On E:LCGErrorException do    FParser.Monitor(ltInfo,'Processing failed due to irrecoverable error');   // Nothing, dealt with already
        On E:LCGInternalException do FParser.Monitor(ltInfo,'Processing failed due to internal error');        // Nothing, dealt with already
        On E:Exception do FParser.Monitor(ltError,E.Message);
      end;
    end;
  except
    // Silent exception, we will have dealt with the error already
  end;
  // stop program loop
  Terminate;
end;

procedure TLaCoGen.OutputTitle;
const START_YEAR = 2020;
var stamp: TDateTime;
    year,month,day: word;
    exestr: string;
begin
  exestr := ExtractFileName(ParamStr(0));
{$IFDEF Windows}
  exestr := StringReplace(exestr,'.exe','',[rfReplaceAll,rfIgnoreCase]);
{$ENDIF}
  WriteLn('');
  WriteLn('LaCoGen V1.2');
  stamp := Now;
  DecodeDate(stamp,year,month,day);
  if year = START_YEAR then
    WriteLn(Format('Copyright (C)%d Duncan Munro <duncan@duncanamps.com>',[START_YEAR]))
  else
    WriteLn(Format('Copyright (C)%d-%d Duncan Munro <duncan@duncanamps.com>',[START_YEAR,year]));
  if ParamCount = 0 then
    WriteLn('This program comes with ABSOLUTELY NO WARRANTY; for details' + #13 + #10 +
            'type ''' + exestr + ' -i''' + #13 + #10 +
            'This is free software, and you are welcome to redistribute it' + #13 + #10 +
            'under certain conditions; type ''' + exestr + ' -r'' for details.');
  WriteLn('');
end;

procedure TLaCoGen.ProcessParameters;
var
  ErrorMsg: String;
  NonOptions: array of string;
begin
  // First check that all parameters are valid
  ErrorMsg:=CheckOptions(gShortOptions, FLongOptions);
  if ErrorMsg<>'' then
    raise LCGErrorException.Create(ErrorMsg);
  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  // Info
  if HasOption('i','info') then
    begin
      WriteLn('Disclaimer of Warranty');
      WriteLn;
      WriteLn('THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY' + #13 + #10 +
              'APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT' + #13 + #10 +
              'HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY' + #13 + #10 +
              'OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,' + #13 + #10 +
              'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR' + #13 + #10 +
              'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM' + #13 + #10 +
              'IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF' + #13 + #10 +
              'ALL NECESSARY SERVICING, REPAIR OR CORRECTION.' + #13 + #10);
      Terminate;
      Exit;
    end;
  // Redistribution
  if HasOption('r','redistribution') then
    begin
      WriteLn('This program is free software: you can redistribute it and/or modify' + #13 + #10 +
              'it under the terms of the GNU General Public License as published by' + #13 + #10 +
              'the Free Software Foundation, either version 3 of the License, or' + #13 + #10 +
              '(at your option) any later version.' + #13 + #10 + #13 + #10 +
              'This program is distributed in the hope that it will be useful,'  + #13 + #10 +
              'but WITHOUT ANY WARRANTY; without even the implied warranty of'  + #13 + #10 +
              'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the' + #13 + #10 +
              'GNU General Public License for more details.' + #13 + #10 + #13 + #10 +
              'You should have received a copy of the GNU General Public License' + #13 + #10 +
              'along with this program.  If not, see <https://www.gnu.org/licenses/>.' + #13 + #10);
      Terminate;
      Exit;
    end;
  // Process the filename parameters
  NonOptions := GetNonOptions(gShortOptions,gLongOptionsArray);
  if Length(NonOptions) <> 1 then
    raise LCGErrorException.Create('A single filename must be specified');
  FParser.SrcFilename := NonOptions[0];
  FSourceFilename := NonOptions[0];
  // Set a default first
  FCompiledFilename   := ChangeFileExt(FSourceFilename,'.lacobj');
  // Now process the filename options
  ProcessFN('c', 'compile',    '.lacobj',       FCompiledFilename);
  ProcessFN('d', 'dump',       '.txt',          FParser.DumpFilename);

  ProcessFN('x', 'xml',        '.xml',          FParser.XMLFilename);
  // And the remaining options
  if HasOption('v','verbose') then
    FParser.Verbose := vbVerbose;
  if HasOption('w','warandpeace') then
    FParser.Verbose := vbWarAndPeace;
end;

procedure TLaCoGen.ProcessFN(shortoption: char; const longoption: string;
   const extension: string; var filename: string);
begin
  // But process an option if it's there
  if HasOption(shortoption,longoption) then
    begin
      filename := GetOptionValue(shortoption,longoption);
      if filename = '' then
        filename := ChangeFileExt(FSourceFilename,extension)
      else
        if ExtractFileExt(filename) = '' then
          filename := filename + extension;
    end;
end;

procedure TLaCoGen.WriteHelp;
var s: string;
begin
  { add your help code here }
  writeln('Usage: lacogen11 inputfile[.lac] [options]');
  writeln('');
  writeln('Options:');
  writeln('');
  ShowOptions;
  writeln('');
  s := 'Where [file] is specified, it will be used. If there is no extension ' +
       'on the name, one will be added. If no [file] is specified at all, it ' +
       'will be created from inputfile with the appropriate extension added';
  while s <> '' do
    writeln(PullString(s));
end;

var
  Application: TLaCoGen;

{$R *.res}

begin
  Application:=TLaCoGen.Create(nil);
  Application.Title:='LaCoGen11 Command Line';
  Application.Run;
  Application.Free;
end.

