unit uparser;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, uparser_nfa,
  uparser_exception, uparser_terminal,
  uparser_nfalist, uparser_rule, uparser_lalrsets, uparser_expanded,
  uparser_canonical, uparser_output, uparser_dfalist, ucharset32,
  deployment_parser_module, deployment_parser_types;


type
  TVerbose = (vbNone,vbVerbose,vbWarAndPeace);

  TLacParserProc = function : TLCGParserStackEntry of object;

  TLacParser = class(TLCGParser)
    protected
      FNoOptimise:     boolean;
      FResourceOut:    boolean;
      FSparse:         boolean;
      FVerbose:        TVerbose;
      ProcList: array of TLacParserProc;
      function ActCopy:                        TLCGParserStackEntry;
      function ActIgnore:                      TLCGParserStackEntry;
      function ActOptProcHashTerminal:         TLCGParserStackEntry;
      function ActParameterDefBoolean:         TLCGParserStackEntry;
      function ActParameterDefInteger:         TLCGParserStackEntry;
      function ActParameterDefNonTerminal:     TLCGParserStackEntry;
      function ActParameterDefString:          TLCGParserStackEntry;
      function ActParameterDefString2:         TLCGParserStackEntry;
      function ActRuleAtomNonTerminal:         TLCGParserStackEntry;
      function ActRuleAtomStringLiteral:       TLCGParserStackEntry;
      function ActRuleAtomTerminal:            TLCGParserStackEntry;
      function ActRuleBodyRuleAtom:            TLCGParserStackEntry;
      function ActSetDefine:                   TLCGParserStackEntry;
      function ActSetAdd:                      TLCGParserStackEntry;
      function ActSetSub:                      TLCGParserStackEntry;
      function ActSetUse:                      TLCGParserStackEntry;
      function ActSetLiteral:                  TLCGParserStackEntry;
      function ActTerminalDef:                 TLCGParserStackEntry;
      function ActTerminalDefIgnore:           TLCGParserStackEntry;
      function ActTerminalDefKeyword:          TLCGParserStackEntry;
      function ActTerminalDefSymbol:           TLCGParserStackEntry;
      function ActTerminalDefVirtual:          TLCGParserStackEntry;
      function ActTerminalOr:                  TLCGParserStackEntry;
      function ActTerminalConcatenate:         TLCGParserStackEntry;
      function ActTerminalClosedBracketedOpt:  TLCGParserStackEntry;
      function ActTerminalClosedBracketedPlus: TLCGParserStackEntry;
      function ActTerminalClosedBracketedStar: TLCGParserStackEntry;
      function ActTerminalBracketBracket:      TLCGParserStackEntry;
      function ActTerminalBracketMixed:        TLCGParserStackEntry;
      function ActTerminalBracketTerminal:     TLCGParserStackEntry;
      function ActTerminalStringLiteral:       TLCGParserStackEntry;
      function ActTerminalBracketSetName:      TLCGParserStackEntry;
      function ActTerminalBracketSetLiteral:   TLCGParserStackEntry;
      function ActRuleDef:                     TLCGParserStackEntry;
      function ActRuleListOrRule:              TLCGParserStackEntry;
      function ActRuleListOrOptProc:           TLCGParserStackEntry;
      function ActRuleRuleBodyOptProc:         TLCGParserStackEntry;
      function PROC_XXACCE_XCONTE_XEOFX:       TLCGParserStackEntry;
      function MyReduce({%H-}Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
      procedure RegisterFuncs;
      procedure SetVerbose(_v: TVerbose);
    public
      CSList:          TCharSetList;
      TerminalList:    TTerminalList;
      NFAList:         TNFAList;
      DFAList:         TDFAList;
      RuleList:        TRuleList;
      FirstSetList:    TParserSetList;
      FollowSetList:   TParserSetList;
      LastSetList:     TParserSetList;
      ExpandedList:   TExpandedList;
      CanonicalTable:     TCanonicalTable;
      ParserOutput:    TParserOutput;
      DumpFilename:    string;
      NFA:             TNFA;
      SrcFilename:     string;
      LineMax:         integer;    // Highest source line number
      XMLFilename:     string;
      constructor Create;
      destructor Destroy; override;
      procedure CheckCsNotUsed;
      procedure CheckRR;
      procedure CheckRulesNotUsed;
      procedure CheckSR;
      procedure CheckSymbolsNotUsed;
      procedure Compile(const _filename: string);
      procedure ComputeDFA;
      procedure CreateExpandedList;
      procedure CreateDollarRule;
      procedure CreateFollowSets;
      procedure CreateParserOutput;
      procedure CreateParserTable;
      procedure Dump;
      procedure DumpXML;
      procedure InitRun;
      procedure MinimiseCharSets;
      procedure ProcessFile;
      property Verbose:      TVerbose       read FVerbose      write SetVerbose;
    end;

{
const
  gMonitorStatusText: array[TMonitorStatus] of string =
    ( 'Verbose',
      'Information',
      'WARNING',
      'ERROR',
      'FATAL INTERNAL ERROR');
}

{
var
  CSList: TCharSetList;
}

implementation

uses
  uparameters, uparser_types, {lacogen10_parser, lacogen10_lexer, }
  uparser_utility;


{$IFDEF DARWIN}
function Max(_a,_b: integer): integer;
begin
  if _a > _b then
    Exit(_a)
  else
    Exit(_b);
end;

{$ENDIF}


function Whitespace(const s: string): boolean;
var i: integer;
begin
  Whitespace := True; // Assume it is for now
  for i := 1 to Length(s) do
    if not (s[i] in [#9,#10,#13,#32,#160]) then
      begin
        Whitespace := False;
        Exit;
      end;
end;

constructor TLacParser.Create;
begin
  inherited Create;
  Verbose := vbNone;
  CSlist := TCharSetList.Create(Self);
  NFA := TNFA.Create;
  NFA.AllocateState();  // Allocate state 0
  NFAList := TNFAList.Create(Self);
  DFAList := TDFAList.Create;
  TerminalList := TTerminalList.Create(Self);
  RuleList := TRuleList.Create;
  FirstSetList  := TParserSetList.Create;
  FollowSetList := TParserSetList.Create;
  LastSetList   := TParserSetList.Create;
  ExpandedList := TExpandedList.Create;
  CanonicalTable   := TCanonicalTable.Create;
  ParserOutput  := TParserOutput.Create;
  LoadFromResource('LACOGEN10');
  RegisterFuncs;
  OnReduce := @MyReduce;
end;

destructor TLacParser.Destroy;
begin
  ParserOutput.Free;
  CanonicalTable.Free;
  ExpandedList.Free;
  LastSetList.Free;
  FollowSetList.Free;
  FirstSetList.Free;
  RuleList.Free;
  TerminalList.Free;
  DFAList.Free;
  NFAList.Free;
  NFA.Free;
  CSList.Free;
  inherited Destroy;
end;

procedure TLacParser.CheckCsNotUsed;
var cs: TCharSet;
begin
  for cs in CSList do
    if (not cs.Used) and (not cs.Temporary) then
      Monitor(ltWarning,'Character set %s is not used',[cs.Name]);;
end;

procedure TLacParser.CheckRR;
begin
  try
    CanonicalTable.CheckRR(FollowSetList);
  except
    On E:Exception do
      Monitor(ltWarning,E.Message);
  end;
end;

procedure TLacParser.CheckRulesNotUsed;
var t: TRule;
begin
  for t in RuleList do
    if not t.Used then
      Monitor(ltWarning,'Rule %s is not used',[t.AsText(TerminalList)]);;
end;

procedure TLacParser.CheckSR;
begin
  try
    CanonicalTable.CheckSR(FollowSetList);
  except
    On E:Exception do
      Monitor(ltWarning,E.Message);
  end;

end;

procedure TLacParser.CheckSymbolsNotUsed;
var t: TTerminalDefObject;
    typestr: string;
begin
  for t in TerminalList do
    if not t.FUsed then
      begin
        case t.FTerminalStyle of
          tsSpecial:         typestr := 'Special terminal';
          tsTerminal:        typestr := 'Terminal';
          tsTerminalIgnored: typestr := 'Ignored terminal';
          tsKeyword:         typestr := 'Keyword';
          tsSymbol:          typestr := 'Symbol';
          tsNonTerminal:     typestr := 'Non-terminal';
          tsVirtual:         typestr := 'Virtual';
          tsAccept:          typestr := 'Accept non-terminal';
        end;
        Monitor(ltWarning,'%s %s is not used',[typestr,MakePrintable(t.FTerminalName)]);;
      end;
end;

procedure TLacParser.Compile(const _filename: string);
// Output everything to a .lacobj file
const MAGIC_WORD = $0143414C;
var _strm: TFileStream;

  function CalcBytesToUse(maxval: UINT32): UINT8;
  begin
    Result := 1;
    if maxval > $000000FE then Result := 2;
    if maxval > $0000FFFE then Result := 3;
    if maxval > $00FFFFFE then Result := 4;
  end;

  procedure WriteUINT8(_u: UINT8);
  begin
    _strm.Write(_u,sizeof(_u));
  end;

  procedure WriteUINT16(_u: UINT16);
  begin
    _strm.Write(_u,sizeof(_u));
  end;

  procedure WriteUINT32(_u: UINT32);
  begin
    _strm.Write(_u,sizeof(_u));
  end;

  procedure WriteUINTX(_u: UINT32; _mb: UINT8);
  begin
    _strm.Write(_u,_mb);
  end;

  procedure WriteStr(_s: string);
  begin
    WriteUINT16(Length(_s));
    if _s <> '' then
      _strm.Write(_s[1],Length(_s));
    WriteUINT8(0);
  end;

  procedure WriteHeader;
  var j: integer;
  begin
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($01);
    WriteUINT32(gParameterList.Count);
    for j := 0 to gParameterList.Count-1 do
      begin
        WriteUINT8($11);
        WriteStr(gParameterList.Items[j].Name);
        WriteStr(gParameterList.Items[j].AsString);
      end;
  end;

  procedure WriteDictionary;
  var j: integer;
      mb: UINT8;
  begin
    mb := CalcBytesToUse(Ord(NFAList.Dictionary.HighestCharacter));
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($02);
    WriteUINT8(mb);
    WriteUINTX(NFAList.Dictionary.Count,mb);
    for j := 0 to NFAList.Dictionary.Count-1 do
      case NFAList.Dictionary[j].DictType of
       ndtCharacter:  begin
                        WriteUINT8($21);
                        WriteUINTX(Ord(NFAList.Dictionary[j].Character),mb);
                      end;
       ndtCharRange:  begin
                        WriteUINT8($22);
                        WriteUINTX(Ord(NFAList.Dictionary[j].Character),mb);
                        WriteUINTX(Ord(NFAList.Dictionary[j].CharacterTo),mb);
                      end;
      end; // Case
  end;

  procedure WriteTokens;
  var j: integer;
  begin
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($03);
    WriteUINT32(TerminalList.Count);
    for j := 0 to TerminalList.Count-1 do
      begin
        WriteUINT8($31);
        if TerminalList.Items[j].FIgnore then
          WriteUINT8(1)
        else
          WriteUINT8(0);
        WriteStr(MakePrintable(TerminalList.Items[j].FTerminalName));
      end;
  end;

  procedure WriteDFA;
  var j,k: integer;
      _mb: UINT8;
  begin
    _mb := CalcBytesToUse(DFAList.Count);
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($04);
    WriteUINT8(_mb);
    WriteUINT32(DFAList.Count);
    for j := 0 to DFAList.Count-1 do
      begin
        WriteUINT8($41);
        WriteUINT32(DFAList.Items[j].AcceptToken);
        for k := 0 to NFAList.Dictionary.Count-1 do
          WriteUINTX(DFAList.Items[j].NextState[k].ID,_mb);
      end;
  end;

  procedure WriteRules;
  var j: integer;
  begin
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($05);
    WriteUINT32(RuleList.Count);
    for j := 0 to RuleList.Count-1 do
      begin
        WriteUINT8($51);
        WriteUINT32(RuleList.Items[j].RuleHead);
        WriteUINT32(RuleList.Items[j].Count);
        WriteStr(RuleList.Items[j].RuleID);
        WriteStr(RuleList.Items[j].AsText(TerminalList,False));
        WriteStr(RuleList.Items[j].RuleProc);
      end;
  end;

  procedure WriteLALR;
  var j,k: integer;
      _mb: UINT8;
      _mv: UINT32;
  begin
    _mv := Max(RuleList.Count,ParserOutput.Count);
    _mb := CalcBytesToUse(_mv);
    WriteUINT32(MAGIC_WORD);
    WriteUINT8($06);
    WriteUINT8(_mb);
    WriteUINT32(ParserOutput.Count);
    for j := 0 to ParserOutput.Count-1 do
      begin
        WriteUINT8($61);
        for k := 0 to TerminalList.Count-1 do
          begin
            WriteUINT8(Ord(ParserOutput.Items[j].FCols[k].OutputType));
            WriteUINTX(ParserOutput.Items[j].FCols[k].Destination,_mb);
          end;
      end;
  end;

begin
  _strm := TFileStream.Create(_filename,fmCreate,fmShareDenyWrite);
  try
    Monitor(ltWarAndPeace,'  Writing header');
    WriteHeader;
    Monitor(ltWarAndPeace,'  Writing dictionary');
    WriteDictionary;
    Monitor(ltWarAndPeace,'  Writing tokens');
    WriteTokens;
    Monitor(ltWarAndPeace,'  Writing DFA');
    WriteDFA;
    Monitor(ltWarAndPeace,'  Writing Rules');
    WriteRules;
    Monitor(ltWarAndPeace,'  Writing LALR table');
    WriteLALR;
    Monitor(ltWarAndPeace,'  Done');
  finally
    _strm.Free;
  end;
end;

procedure TLacParser.ComputeDFA;
var t: TTerminalDefObject;
begin
  // Create NFA list
  for t in TerminalList do
    NFA.StateList.SetStateName(t.FFirst,MakePrintable(t.FTerminalName));
  // Other routines
  Monitor(ltVerbose,'Creating NFA list from state fragments');
  NFAList.CreateFromNFA(NFA);
  Monitor(ltVerbose,'Computing DFA');
  DFAList.CreateFromNFAList(NFAList);
  Monitor(ltWarAndPeace,'  Minimising DFA');
  DFAList.Minimise(NFAList.Dictionary);
  Monitor(ltWarAndPeace,'  Renaming DFA states');
  DFAList.EnumerateStateIDs(NFAList.Dictionary);
end;

procedure TLacParser.CreateDollarRule;
var obj: TTerminalDefObject;
    index: integer;
begin
  try
    // Add the terminal for <$accept>
    obj := TTerminalDefObject.Create;
    obj.FTerminalName := ACCEPT_CONDITION;
    obj.FTerminalFriendly := '$';
    obj.FTokenName        := '$';
    obj.FFirst            := NO_STATE_IDENTIFIER;
    obj.FLast             := NO_STATE_IDENTIFIER;
    obj.FTerminalStyle    := tsAccept;
    obj.FNodeNumber := TerminalList.Add(obj);
    obj.FUSed       := True;
    // Add a rule for the $ sign
    index := TerminalList.IndexOf(startnonterminal);
    if index < 0 then
      Monitor(ltError,'Start non terminal ' + startnonterminal + ' not found');
    RuleList.CreateRuleFromString(IntToStr(obj.FNodeNumber),IntToStr(index)+':1',TerminalList);
  except
    On E:Exception do
      Monitor(ltError,E.Message);
  end;
end;

procedure TLacParser.CreateExpandedList;
var i: TRuleIdentifier;
begin
  try
    // Add the top level rules first
    for i := 0 to RuleList.Count-1 do
      if TerminalList.TerminalName(RuleList.Items[i].RuleHead) = ACCEPT_CONDITION then
        ExpandedList.AddRule(i,RuleList[i],TerminalList,True);
    // Now expand to all the other rules
    ExpandedList.ExpandRules(RuleList,TerminalList);
  except
    on E:Exception do
      Monitor(ltError,E.Message);
  end;
end;

procedure TLacParser.CreateFollowSets;
var j: integer;
    r: TRule;
    marker: integer;
    changing: boolean;
begin
  try
    // Create first set from rule list
    for r in RuleList do
      begin
        if r.Count = 0 then // Epsilon rule
          FirstSetList.Add(r.RuleHead,NO_TOKEN_IDENTIFIER)
        else
          FirstSetList.Add(r.RuleHead,r.items[0]);
      end;
    FirstSetList.ExpandFirst;

    // Create last set from rule list
    for r in RuleList do
      begin
        if r.Count = 0 then // Epsilon rule
          LastSetList.Add(r.RuleHead,NO_TOKEN_IDENTIFIER)
        else
          LastSetList.Add(r.RuleHead,r.items[r.Count-1]);
      end;
  //  LastSetList.ExpandLast;

    // Create follow set from rule list
    for r in RuleList do
      for j := 1 to r.Count-1 do
        FollowSetList.Add(r.Items[j-1],r.Items[j]);
    // If there is an empty rule, we can add the follow set for the empty rule
    // into any sets that have that rule head in their own follow set
    // We may have to alternate between appending and expanding and do this a
    // few times
    marker := FollowSetList.TotalItems;
    changing := True;
    while changing do
      begin
        changing := False;
        for r in RuleList do
          if r.Count = 0 then
            FollowSetList.Append(r.RuleHead);
        // Finally expand the follow set
        FollowSetList.ExpandFollow(FirstSetList,LastSetList);
        if FollowSetList.TotalItems <> marker then
          begin
            marker := FollowSetList.TotalItems;
            changing := True;
          end;
      end;
  except
    On E:Exception do
      Monitor(ltError,E.Message);
  end;
end;

procedure TLacParser.CreateParserOutput;
begin
  try
    ParserOutput.CreateOutput(Self,CanonicalTable,TerminalList,ExpandedList,FollowSetList);
  except
    On E:Exception do
      Monitor(ltError,E.Message);
  end;
end;

procedure TLacParser.CreateParserTable;
var _start: integer;
begin
  try
    _start := TerminalList.IndexOf(ACCEPT_CONDITION);
    if _start < 0 then
      Monitor(ltError,'Start non-terminal ' + startnonterminal + ' not found');
    CanonicalTable.CreateFromExpanded(_start,ExpandedList,TerminalList);
    CanonicalTable.Sort(@ParserCompareFunc);
  except
    On E:Exception do
      Monitor(ltError,E.Message);
  end;
end;


procedure TLacParser.Dump;
var strm: TFileStream;
begin
  if DumpFilename <> '' then
    begin
      Monitor(ltVerbose,'Creating dump file');
      strm := TFileStream.Create(DumpFilename,fmCreate);
      try
        // Dump parameters
        gParameterList.Dump(strm);
        // Dump character set
        CSList.Dump(strm,False);  // Don't show temporary sets
        // Dump terminals
        TerminalList.Dump(strm,'Terminals - Special',      [tsSpecial]);
        TerminalList.Dump(strm,'Terminals - Normal',       [tsTerminal]);
        TerminalList.Dump(strm,'Terminals - Ignored',      [tsTerminalIgnored]);
        TerminalList.Dump(strm,'Terminals - Keywords',     [tsKeyword]);
        TerminalList.Dump(strm,'Terminals - Symbols',      [tsSymbol]);
        TerminalList.Dump(strm,'Terminals - Virtual',      [tsVirtual]);
        TerminalList.Dump(strm,'Terminals - Non-Terminals',[tsNonTerminal]);
        // Dump NFA
        NFA.Dump(strm,TerminalList);
        // Dump the records
        NFAList.Dump(strm);
        DFAList.Dump(strm,NFAList.Dictionary,'DFA LIST (RENAMED)',True);
        // Dump the rules
        RuleList.Dump(strm,TerminalList);
        // Dump the follow sets
        FirstSetList.Dump(strm, 'First Set',  TerminalList);
        FollowSetList.Dump(strm,'Follow Set', TerminalList);
        LastSetList.Dump(strm,  'Last Set',   TerminalList);
        // Dump the Expanded list
        ExpandedList.Dump(strm);
        // Dump the canonical table
        CanonicalTable.Dump(strm);
        // Dump the output table
        ParserOutput.Dump(strm);
      finally
        strm.Free;
      end;
    end;
end;


procedure TLacParser.DumpXML;
var strm: TFileStream;
begin
  if XMLFilename <> '' then
    begin
      Monitor(ltVerbose,'Creating XML file');
      strm := TFileStream.Create(XMLFilename,fmCreate);
      try
        // Header items
        WriteLnStringToStream(strm,'<?xml version="1.0"?>');
        WriteLnStringToStream(strm,'<Lacogen>');
        // Dump parameters
        gParameterList.DumpXML(strm);
        // Dump character set
        CSlist.DumpXML(strm);
        // Dump terminals
        TerminalList.DumpXML(strm);
        // Dump NFA
        NFAList.DumpXML(strm);
        // Dump DFA
        DFAList.DumpXML(strm,NFAList.Dictionary);
        // Dump the rules
        RuleList.DumpXML(strm,TerminalList);
        // Dump the follow sets
        FirstSetList.DumpXML(strm, 'FirstSet',  TerminalList);
        FollowSetList.DumpXML(strm,'FollowSet', TerminalList);
        LastSetList.DumpXML(strm,  'LastSet',   TerminalList);
        // Dump the Expanded list
        ExpandedList.DumpXML(strm);
        // Dump the canonical table
        CanonicalTable.DumpXML(strm);
        // Dump the output table
        ParserOutput.DumpXML(strm);
        WriteLnStringToStream(strm,'</Lacogen>');
      finally
        strm.Free;
      end;
    end;
end;

procedure TLacParser.InitRun;
begin
  inherited InitRun;
  ParserOutput.Clear;
  CanonicalTable.Clear;
  ExpandedList.Clear;
  LastSetList.Clear;
  FollowSetList.Clear;
  FirstSetList.Clear;
  RuleList.Clear;
  DFAList.Clear;
  NFAList.Clear;
  TerminalList.Clear;
  CSList.Clear;
end;

procedure TLacParser.MinimiseCharSets;
begin
  NFA.MinimiseCharSets;
end;

procedure TLacParser.ProcessFile;
var strm: TFileStream;
begin
    InitRun;
    FInputLine := -1;
    FInputLineSave := -1;
    try
      strm := TFileStream.Create(SrcFilename,fmOpenRead,fmShareDenyWrite);
    except
      Monitor(ltError,'Could not open filename ' + SrcFilename);
    end;
    try
      {
      Monitor(ltInfo,'Processing file ' + SrcFilename + ' Pass 1 dictionary creation','');
      Dictionary.PopulateFromUTF8stream(strm);
      strm.Position := 0;
      }
      Monitor(ltInfo,'Processing file ' + SrcFilename);
      FInputLine := 1;
      FInputLineSave := 1;
      Parse(strm);
//      CSList.Sort(@csListCompare);  // Sort the character set list and owned sets
    finally
      strm.Free;
    end;
  LineMax := InputLine;
  FInputLine := -1;
  FInputLineSave := -1;
end;

{ ACTION PROCEDURES }


function TLacParser.ActCopy: TLCGParserStackEntry;
begin
  // Propagate the reduction through to the result
  Result.Buf := ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActIgnore: TLCGParserStackEntry;
begin
  // Do nothing
  Result.Buf := '';
end;

function TLacParser.ActOptProcHashTerminal: TLCGParserStackEntry;
begin
  Result.Buf := '#' + ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActParameterDefBoolean: TLCGParserStackEntry;
begin
  gParameterList.Parameter(ParserStack[ParserSP-3].Buf).AsString := ParserStack[ParserSP-1].Buf;
  Result.Buf := '';
end;

function TLacParser.ActParameterDefInteger: TLCGParserStackEntry;
begin
  gParameterList.Parameter(ParserStack[ParserSP-3].Buf).AsString := ParserStack[ParserSP-1].Buf;
  Result.Buf := '';
end;

function TLacParser.ActParameterDefNonTerminal: TLCGParserStackEntry;
begin
  gParameterList.Parameter(ParserStack[ParserSP-3].Buf).AsString := StripQuotes(ParserStack[ParserSP-1].Buf);
  Result.Buf := '';
end;

function TLacParser.ActParameterDefString: TLCGParserStackEntry;
begin
  gParameterList.Parameter(ParserStack[ParserSP-3].Buf).AsString := StripQuotes(ParserStack[ParserSP-1].Buf);
  Result.Buf := '';
end;

function TLacParser.ActParameterDefString2: TLCGParserStackEntry;
begin
//  gParameterList.Parameter(ParserStack[ParserSP-3].Buf).AsString := StripQuotes(ParserStack[ParserSP-1].Buf);
// @@@@@ Need to add the code here
  Result.Buf := '';
end;

function TLacParser.ActRuleAtomNonTerminal: TLCGParserStackEntry;
var literal: TString;
    oindex: integer;
    obj:  TTerminalDefObject;
begin
  literal := ParserStack[ParserSP-1].Buf;
  if Length(literal) = 0 then
    Monitor(ltError,'Attempt to create blank non-terminal');
  // Check if in the terminal table, add if not
  oindex := TerminalList.IndexOf(literal);
  if oindex >= 0 then
    obj := TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := literal;
      obj.FTokenName        := NonTerminalNameToToken(literal);
      obj.FFirst            := NO_STATE_IDENTIFIER;
      obj.FLast             := NO_STATE_IDENTIFIER;
      obj.FTerminalStyle    := tsNonTerminal;
      obj.FNodeNumber       := TerminalList.Add(obj);
    end;
  obj.FUsed := true;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function TLacParser.ActRuleAtomStringLiteral: TLCGParserStackEntry;
var literal: TString;
    frag: TNFAFragment;
    oindex: integer;
    obj:  TTerminalDefObject;
begin
  literal := StripQuotes(ParserStack[ParserSP-1].Buf);
  if Length(literal) = 0 then
    Monitor(ltError,'Attempt to create blank terminal');
  // Check if in the terminal table, add if not
  oindex := TerminalList.IndexOf(literal);
  if oindex >= 0 then
    obj := TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      frag := NFA.FragFromString(literal);
      NFA.FragAttach(0,frag);
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := frag.FriendlyName;
      obj.FFirst            := frag.First;
      obj.FLast             := frag.Last;
      obj.FNodeNumber := TerminalList.Add(obj);
      if IsLetter(literal[1]) then
        obj.FTerminalStyle := tsKeyword
      else
        obj.FTerminalStyle := tsSymbol;
      NFA.StateList.Items[frag.Last].IsAccepting := True;
      NFA.StateList.Items[frag.Last].AcceptToken := obj.FNodeNumber;
      NFA.StateList.Items[frag.Last].TerminalStyle := obj.FTerminalStyle;
    end;
  obj.FUsed := True;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function TLacParser.ActRuleAtomTerminal: TLCGParserStackEntry;
var literal: string;
    frag: TNFAFragment;
    oindex: integer;
    obj:  TTerminalDefObject;
begin
  literal := ParserStack[ParserSP-1].Buf;
  if literal = '' then
    Monitor(ltError,'Attempt to create blank terminal');
  // Check if in the terminal table, add if not
  oindex := TerminalList.IndexOf(literal);
  if TerminalList.IndexOf(literal) >= 0 then
    obj := TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      frag := NFA.FragFromString(literal);
      NFA.FragAttach(0,frag);
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := frag.FriendlyName;
      obj.FFirst            := frag.First;
      obj.FLast             := frag.Last;
      obj.FNodeNumber := TerminalList.Add(obj);
      if IsLetter(literal[1]) then
        obj.FTerminalStyle := tsKeyword
      else
        obj.FTerminalStyle := tsSymbol;
      NFA.StateList.Items[frag.Last].IsAccepting := True;
      NFA.StateList.Items[frag.Last].AcceptToken := obj.FNodeNumber;
      NFA.StateList.Items[frag.Last].TerminalStyle := obj.FTerminalStyle;
    end;
  obj.FUsed := True;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function TLacParser.ActRuleBodyRuleAtom: TLCGParserStackEntry;
begin
  Result.Buf := ParserStack[ParserSP-2].Buf + ':' + ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActRuleDef: TLCGParserStackEntry;
begin
  RuleList.CreateRuleFromString(ParserStack[ParserSP-3].Buf,
                                        ParserStack[ParserSP-1].Buf,
                                        TerminalList);
  Result.Buf := '';
end;

function TLacParser.ActRuleListOrOptProc: TLCGParserStackEntry;
begin
  Result.Buf := ParserStack[ParserSP-3].Buf + '|' + ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActRuleListOrRule: TLCGParserStackEntry;
begin
  Result.Buf := ParserStack[ParserSP-3].Buf + '|' + ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActRuleRuleBodyOptProc: TLCGParserStackEntry;
begin
  Result.Buf := ParserStack[ParserSP-2].Buf + ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActSetAdd: TLCGParserStackEntry;
var charset:  TCharSet;
    charset2: TCharSet;
    charset3: TCharSet;
begin
  charset :=  CSList.AllocateCharSetTemp;
  charset2 := CSList.FindMustByName(ParserStack[ParserSP-3].Buf);
  charset3 := CSList.FindMustByName(ParserStack[ParserSP-1].Buf);
  charset.AddSet(charset2);
  charset.AddSet(charset3);
  charset.Used  := True;
  charset2.Used := True;
  charset3.Used := True;
  Result.Buf := charset.Name;
end;

function TLacParser.ActSetDefine: TLCGParserStackEntry;
var charset:  TCharSet;
    charset2: TCharSet;
begin
  charset  := CSList.AllocateCharSet(ParserStack[ParserSP-3].Buf);
  charset2 := CSList.FindMustByName(ParserStack[ParserSP-1].Buf);
  charset.AddSet(charset2);
  Result.Buf := ParserStack[ParserSP-3].Buf;
end;

function TLacParser.ActSetLiteral: TLCGParserStackEntry;
var charset: TCharSet;
begin
  charset := CSList.AllocateCharSetTemp;
  charset.SetLiteral(ParserStack[ParserSP-1].Buf);
  Result.Buf := charset.Name;
end;

function TLacParser.ActSetSub: TLCGParserStackEntry;
var charset:  TCharSet;
    charset2: TCharSet;
    charset3: TCharSet;
begin
  charset  := CSList.AllocateCharSetTemp;
  charset2 := CSList.FindMustByName(ParserStack[ParserSP-3].Buf);
  charset3 := CSList.FindMustByName(ParserStack[ParserSP-1].Buf);
  charset.AddSet(charset2);
  charset.SubtractSet(charset3);
  charset.Used  := True;
  charset2.Used := True;
  charset3.Used := True;
  Result.Buf := charset.Name;
end;

function TLacParser.ActSetUse: TLCGParserStackEntry;
var charset: TCharSet;
begin
  charset := CSList.FindMustByName(ParserStack[ParserSP-1].Buf);
  charset.Used := True;
  Result.Buf := ParserStack[ParserSP-1].Buf;
end;

function TLacParser.ActTerminalBracketBracket: TLCGParserStackEntry;
begin
  Result.Buf := ParserStack[ParserSP-2].Buf;
end;

function TLacParser.ActTerminalBracketMixed: TLCGParserStackEntry;
// Create a terminal fragment from a string literal which is
// case insensitive e.g. "LDA" should match "lda", "Lda", "LdA" etc.
var frag:    TNFAFragment;
begin
  frag := NFA.FragFromStringCI(StripQuotes(ParserStack[ParserSP-2].Buf));
  Result.Buf := NFAFragmentToString(frag);
end;

function TLacParser.ActTerminalBracketSetLiteral: TLCGParserStackEntry;
// Create a terminal fragment from a character set literal, e.g. [0-9]
var charset: TCharSet;
    frag:    TNFAFragment;
begin
  charset := CSList.AllocateCharSetTemp;
  charset.SetLiteral(ParserStack[ParserSP-1].Buf);
  frag := NFA.FragFromCharset(charset);
  Result.Buf := NFAFragmentToString(frag);
end;

function TLacParser.ActTerminalBracketSetName: TLCGParserStackEntry;
var cs2:     TCharSet;
    frag:    TNFAFragment;
begin
  cs2 := CSList.FindMustByName(ParserStack[ParserSP-1].Buf);
  cs2.Used := True;
  frag := NFA.FragFromCharset(cs2);
  Result.Buf := NFAFragmentToString(frag);
end;

function TLacParser.ActTerminalBracketTerminal: TLCGParserStackEntry;
// Search for a terminal in the list, otherwise...
// Create a terminal fragment from a terminal e.g. goto
var frag:    TNFAFragment;
    index:   integer;
begin
  // Check if the id already exists in the terminal list
  index := TerminalList.IndexOf(ParserStack[ParserSP-1].Buf);
  if index >= 0 then
    begin
      TerminalList[index].FUsed := True;
      frag.FriendlyName := TerminalList[index].FTerminalFriendly;
      frag.First        := TerminalList[index].FFirst;
      frag.Last         := TerminalList[index].FLast;
    end
  else
    begin
      frag := NFA.FragFromString(ParserStack[ParserSP-1].Buf);
    end;
  Result.Buf := NFAFragmentToString(frag);
end;

function TLacParser.ActTerminalClosedBracketedOpt: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  _frag2 := NFA.Closure01(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function TLacParser.ActTerminalClosedBracketedPlus: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  _frag2 := NFA.Closure1n(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function TLacParser.ActTerminalClosedBracketedStar: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  _frag2 := NFA.Closure0n(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function TLacParser.ActTerminalConcatenate: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _fragx: TNFAFragment;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  _frag2 := StringToNFAFragment(ParserStack[ParserSP-1].Buf);
  _fragx := NFA.FragAppend(_frag1,_frag2);
  Result.Buf := NFAFragmentToString(_fragx);
end;

function TLacParser.ActTerminalDef: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-1].Buf);
  NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := ParserStack[ParserSP-3].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := TerminalList.Add(obj);
  obj.FTerminalStyle := tsTerminal;
  NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function TLacParser.ActTerminalDefIgnore: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := ParserStack[ParserSP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := TerminalList.Add(obj);
  obj.FTerminalStyle := tsTerminalIgnored;
  obj.FIgnore        := true;
  obj.FUsed          := true;
  NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function TLacParser.ActTerminalDefKeyword: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := ParserStack[ParserSP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber       := TerminalList.Add(obj);
  obj.FTerminalStyle    := tsKeyword;
  obj.FIgnore           := False;
  obj.FUsed             := False;
  NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function TLacParser.ActTerminalDefSymbol: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-2].Buf);
  NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := ParserStack[ParserSP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := TerminalList.Add(obj);
  obj.FTerminalStyle := tsSymbol;
  obj.FIgnore        := False;
  obj.FUsed          := False;
  NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function TLacParser.ActTerminalDefVirtual: TLCGParserStackEntry;
var obj:    TTerminalDefObject;
begin
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := ParserStack[ParserSP-2].Buf;
  obj.FTerminalFriendly := obj.FTerminalName;
  obj.FFirst            := NO_STATE_IDENTIFIER;
  obj.FLast             := NO_STATE_IDENTIFIER;
  obj.FNodeNumber       := TerminalList.Add(obj);
  obj.FTerminalStyle    := tsVirtual;
  obj.FIgnore           := False;
  obj.FUsed             := False;
  Result.Buf := '';
end;

function TLacParser.ActTerminalOr: TLCGParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _fragx: TNFAFragment;
begin
  _frag1 := StringToNFAFragment(ParserStack[ParserSP-3].Buf);
  _frag2 := StringToNFAFragment(ParserStack[ParserSP-1].Buf);
  _fragx := NFA.FragOr(_frag1,_frag2);
  Result.Buf := NFAFragmentToString(_fragx);
end;

function TLacParser.ActTerminalStringLiteral: TLCGParserStackEntry;
// Create a terminal fragment from a string literal e.g. ":="
var frag:    TNFAFragment;
begin
  frag := NFA.FragFromString(StripQuotes(ParserStack[ParserSP-1].Buf));
  Result.Buf := NFAFragmentToString(frag);
end;

function TLacParser.PROC_XXACCE_XCONTE_XEOFX: TLCGParserStackEntry;
begin
  Result := ActIgnore;
end;

function TLacParser.MyReduce(Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry;
begin
  if Assigned(ProcList[RuleIndex]) then
    Result := ProcList[RuleIndex]()
  else
    raise Exception.CreateFmt('No procedure for rule index %d (%s)',[RuleIndex,RuleProcs[RuleIndex]]);
end;

procedure TLacParser.RegisterFuncs;
  procedure ProcRegister(_funcname: string; _func: TLacParserProc);
  var i: integer;
  begin
    for i := 0 to Rules-1 do
      if _funcname = RuleProcs[i] then
        ProcList[i] := _func;
  end;
begin
  SetLength(ProcList,Rules);
  ProcRegister('ActCopy',                        @ActCopy);
  ProcRegister('ActIgnore',                      @ActIgnore);
  ProcRegister('ActOptProcHashTerminal',         @ActOptProcHashTerminal);
  ProcRegister('ActParameterDefBoolean',         @ActParameterDefBoolean);
  ProcRegister('ActParameterDefInteger',         @ActParameterDefInteger);
  ProcRegister('ActParameterDefNonTerminal',     @ActParameterDefNonTerminal);
  ProcRegister('ActParameterDefString',          @ActParameterDefString);
  ProcRegister('ActParameterDefString2',         @ActParameterDefString2);
  ProcRegister('ActRuleAtomNonTerminal',         @ActRuleAtomNonTerminal);
  ProcRegister('ActRuleAtomStringLiteral',       @ActRuleAtomStringLiteral);
  ProcRegister('ActRuleAtomTerminal',            @ActRuleAtomTerminal);
  ProcRegister('ActRuleBodyRuleAtom',            @ActRuleBodyRuleAtom);
  ProcRegister('ActSetDefine',                   @ActSetDefine);
  ProcRegister('ActSetAdd',                      @ActSetAdd);
  ProcRegister('ActSetSub',                      @ActSetSub);
  ProcRegister('ActSetUse',                      @ActSetUse);
  ProcRegister('ActSetLiteral',                  @ActSetLiteral);
  ProcRegister('ActTerminalDef',                 @ActTerminalDef);
  ProcRegister('ActTerminalDefIgnore',           @ActTerminalDefIgnore);
  ProcRegister('ActTerminalDefKeyword',          @ActTerminalDefKeyword);
  ProcRegister('ActTerminalDefSymbol',           @ActTerminalDefSymbol);
  ProcRegister('ActTerminalDefVirtual',          @ActTerminalDefVirtual);
  ProcRegister('ActTerminalOr',                  @ActTerminalOr);
  ProcRegister('ActTerminalConcatenate',         @ActTerminalConcatenate);
  ProcRegister('ActTerminalClosedBracketedOpt',  @ActTerminalClosedBracketedOpt);
  ProcRegister('ActTerminalClosedBracketedPlus', @ActTerminalClosedBracketedPlus);
  ProcRegister('ActTerminalClosedBracketedStar', @ActTerminalClosedBracketedStar);
  ProcRegister('ActTerminalBracketBracket',      @ActTerminalBracketBracket);
  ProcRegister('ActTerminalBracketMixed',        @ActTerminalBracketMixed);
  ProcRegister('ActTerminalBracketTerminal',     @ActTerminalBracketTerminal);
  ProcRegister('ActTerminalStringLiteral',       @ActTerminalStringLiteral);
  ProcRegister('ActTerminalBracketSetName',      @ActTerminalBracketSetName);
  ProcRegister('ActTerminalBracketSetLiteral',   @ActTerminalBracketSetLiteral);
  ProcRegister('ActRuleDef',                     @ActRuleDef);
  ProcRegister('ActRuleListOrRule',              @ActRuleListOrRule);
  ProcRegister('ActRuleListOrOptProc',           @ActRuleListOrOptProc);
  ProcRegister('ActRuleRuleBodyOptProc',         @ActRuleRuleBodyOptProc);
  ProcRegister('PROC_XXACCE_XCONTE_XEOFX',       @PROC_XXACCE_XCONTE_XEOFX);
end;

procedure TLacParser.SetVerbose(_v: TVerbose);
begin
  FVerbose := _v;
  case _v of
   vbNone:        FLogLevel := ltInfo;
   vbVerbose:     FLogLevel := ltVerbose;
   vbWarAndPeace: FLogLevel := ltWarAndPeace;
  end;  // case
end;

end.


