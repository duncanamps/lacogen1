unit lacogen5_procs;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, uparser_deployment, uparser;

// Forward declarations

procedure LCG_Error_Handler(_ptr: pointer; _logtype: TLogType; const _msg: string);

function ActCopy(_ptr: pointer):                        TParserStackEntry;
function ActIgnore(_ptr: pointer):                      TParserStackEntry;
function ActOptProcHashTerminal(_ptr: pointer):         TParserStackEntry;
function ActParameterDefBoolean(_ptr: pointer):         TParserStackEntry;
function ActParameterDefInteger(_ptr: pointer):         TParserStackEntry;
function ActParameterDefNonTerminal(_ptr: pointer):     TParserStackEntry;
function ActParameterDefString(_ptr: pointer):          TParserStackEntry;
function ActRuleAtomNonTerminal(_ptr: pointer):         TParserStackEntry;
function ActRuleAtomStringLiteral(_ptr: pointer):       TParserStackEntry;
function ActRuleAtomTerminal(_ptr: pointer):            TParserStackEntry;
function ActRuleBodyRuleAtom(_ptr: pointer):            TParserStackEntry;
function ActSetDefine(_ptr: pointer):                   TParserStackEntry;
function ActSetAdd(_ptr: pointer):                      TParserStackEntry;
function ActSetSub(_ptr: pointer):                      TParserStackEntry;
function ActSetUse(_ptr: pointer):                      TParserStackEntry;
function ActSetLiteral(_ptr: pointer):                  TParserStackEntry;
function ActTerminalDef(_ptr: pointer):                 TParserStackEntry;
function ActTerminalDefIgnore(_ptr: pointer):           TParserStackEntry;
function ActTerminalDefKeyword(_ptr: pointer):          TParserStackEntry;
function ActTerminalDefSymbol(_ptr: pointer):          TParserStackEntry;
function ActTerminalOr(_ptr: pointer):                  TParserStackEntry;
function ActTerminalConcatenate(_ptr: pointer):         TParserStackEntry;
function ActTerminalClosedBracketedOpt(_ptr: pointer):  TParserStackEntry;
function ActTerminalClosedBracketedPlus(_ptr: pointer): TParserStackEntry;
function ActTerminalClosedBracketedStar(_ptr: pointer): TParserStackEntry;
function ActTerminalBracketBracket(_ptr: pointer):      TParserStackEntry;
function ActTerminalBracketTerminal(_ptr: pointer):     TParserStackEntry;
function ActTerminalStringLiteral(_ptr: pointer):       TParserStackEntry;
function ActTerminalBracketSetName(_ptr: pointer):      TParserStackEntry;
function ActTerminalBracketSetLiteral(_ptr: pointer):   TParserStackEntry;
function ActRuleDef(_ptr: pointer):                     TParserStackEntry;
function ActRuleListOrRule(_ptr: pointer):              TParserStackEntry;
function ActRuleListOrOptProc(_ptr: pointer):           TParserStackEntry;
function ActRuleRuleBodyOptProc(_ptr: pointer):         TParserStackEntry;
function PROC_XXACCE_XCONTE_XEOFX(_ptr: pointer):       TParserStackEntry;
function PROC_XBADRU_MONKEY(_ptr: pointer):             TParserStackEntry;

implementation

uses
  lacogen5_parser, uparameters, uparser_utility, uparser_terminal,
  uparser_exception, uparser_types, uparser_nfa;


function PROC_XBADRU_MONKEY(_ptr: pointer):             TParserStackEntry;
begin
  Result.Buf := '';
end;

procedure LCG_Error_Handler(_ptr: pointer; _logtype: TLogType; const _msg: string);
var _parser: TLacParser;
begin
  // Get the parser object from the pointer
  _parser := TLacParser(_ptr);
  // Set up the line numbers
  gLogObject.Line   := LCG_Line;
  gLogObject.Column := LCG_Column;
  // Finally register the error
  gLogObject.Log(_logtype,_msg);
end;

function ActCopy(_ptr: pointer): TParserStackEntry;
begin
  // Propagate the reduction through to the result
  Result.Buf := LCG_Stack[LCG_SP-1].Buf;
end;

function ActIgnore(_ptr: pointer): TParserStackEntry;
begin
  // Do nothing
  Result.Buf := '';
end;

function ActOptProcHashTerminal(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := '#' + LCG_Stack[LCG_SP-1].Buf;
end;

function ActParameterDefBoolean(_ptr: pointer): TParserStackEntry;
begin
  gParameterList.Parameter(LCG_Stack[LCG_SP-3].Buf).AsString := LCG_Stack[LCG_SP-1].Buf;
  Result.Buf := '';
end;

function ActParameterDefInteger(_ptr: pointer): TParserStackEntry;
begin
  gParameterList.Parameter(LCG_Stack[LCG_SP-3].Buf).AsString := LCG_Stack[LCG_SP-1].Buf;
  Result.Buf := '';
end;

function ActParameterDefNonTerminal(_ptr: pointer): TParserStackEntry;
begin
  gParameterList.Parameter(LCG_Stack[LCG_SP-3].Buf).AsString := StripQuotes(LCG_Stack[LCG_SP-1].Buf);
  Result.Buf := '';
end;

function ActParameterDefString(_ptr: pointer): TParserStackEntry;
begin
  gParameterList.Parameter(LCG_Stack[LCG_SP-3].Buf).AsString := StripQuotes(LCG_Stack[LCG_SP-1].Buf);
  Result.Buf := '';
end;

function ActRuleAtomNonTerminal(_ptr: pointer): TParserStackEntry;
var literal: string;
    oindex: integer;
    obj:  TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  literal := LCG_Stack[LCG_SP-1].Buf;
  if literal = '' then
    gLogObject.Log(ltError,'Attempt to create blank non-terminal');
  // Check if in the terminal table, add if not
  oindex := _parser.TerminalList.IndexOf(literal);
  if oindex >= 0 then
    obj := _parser.TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := literal;
      obj.FTokenName        := NonTerminalNameToToken(literal);
      obj.FFirst            := NO_STATE_IDENTIFIER;
      obj.FLast             := NO_STATE_IDENTIFIER;
      obj.FTerminalStyle    := tsNonTerminal;
      obj.FNodeNumber       := _parser.TerminalList.Add(obj);
    end;
  obj.FUsed := true;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function ActRuleAtomStringLiteral(_ptr: pointer): TParserStackEntry;
var literal: string;
    frag: TNFAFragment;
    oindex: integer;
    obj:  TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  literal := StripQuotes(LCG_Stack[LCG_SP-1].Buf);
  if literal = '' then
    gLogObject.Log(ltError,'Attempt to create blank terminal');
  // Check if in the terminal table, add if not
  oindex := _parser.TerminalList.IndexOf(literal);
  if oindex >= 0 then
    obj := _parser.TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      frag := _parser.NFA.FragFromString(literal);
      _parser.NFA.FragAttach(0,frag);
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := frag.FriendlyName;
      obj.FFirst            := frag.First;
      obj.FLast             := frag.Last;
      obj.FNodeNumber := _parser.TerminalList.Add(obj);
      if IsLetter(literal[1]) then
        obj.FTerminalStyle := tsKeyword
      else
        obj.FTerminalStyle := tsSymbol;
      _parser.NFA.StateList.Items[frag.Last].IsAccepting := True;
      _parser.NFA.StateList.Items[frag.Last].AcceptToken := obj.FNodeNumber;
      _parser.NFA.StateList.Items[frag.Last].TerminalStyle := obj.FTerminalStyle;
    end;
  obj.FUsed := True;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function ActRuleAtomTerminal(_ptr: pointer): TParserStackEntry;
var literal: string;
    frag: TNFAFragment;
    oindex: integer;
    obj:  TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  literal := LCG_Stack[LCG_SP-1].Buf;
  if literal = '' then
    gLogObject.Log(ltError,'Attempt to create blank terminal');
  // Check if in the terminal table, add if not
  oindex := _parser.TerminalList.IndexOf(literal);
  if _parser.TerminalList.IndexOf(literal) >= 0 then
    obj := _parser.TerminalList.Items[oindex]
  else
    begin
      obj := TTerminalDefObject.Create;
      frag := _parser.NFA.FragFromString(literal);
      _parser.NFA.FragAttach(0,frag);
      obj.FTerminalName     := literal;
      obj.FTerminalFriendly := frag.FriendlyName;
      obj.FFirst            := frag.First;
      obj.FLast             := frag.Last;
      obj.FNodeNumber := _parser.TerminalList.Add(obj);
      if IsLetter(literal[1]) then
        obj.FTerminalStyle := tsKeyword
      else
        obj.FTerminalStyle := tsSymbol;
      _parser.NFA.StateList.Items[frag.Last].IsAccepting := True;
      _parser.NFA.StateList.Items[frag.Last].AcceptToken := obj.FNodeNumber;
      _parser.NFA.StateList.Items[frag.Last].TerminalStyle := obj.FTerminalStyle;
    end;
  obj.FUsed := True;
  Result.Buf := IntToStr(obj.FNodeNumber);
end;

function ActRuleBodyRuleAtom(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := LCG_Stack[LCG_SP-2].Buf + ':' + LCG_Stack[LCG_SP-1].Buf;
end;

function ActRuleDef(_ptr: pointer): TParserStackEntry;
var _parser: TLacParser;
begin
_parser := TLacParser(_ptr);
  _parser.RuleList.CreateRuleFromString(LCG_Stack[LCG_SP-3].Buf,
                                        LCG_Stack[LCG_SP-1].Buf,
                                        _parser.TerminalList);
  Result.Buf := '';
end;

function ActRuleListOrOptProc(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := LCG_Stack[LCG_SP-3].Buf + '|' + LCG_Stack[LCG_SP-1].Buf;
end;

function ActRuleListOrRule(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := LCG_Stack[LCG_SP-3].Buf + '|' + LCG_Stack[LCG_SP-1].Buf;
end;

function ActRuleRuleBodyOptProc(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := LCG_Stack[LCG_SP-2].Buf + LCG_Stack[LCG_SP-1].Buf;
end;

function ActSetAdd(_ptr: pointer): TParserStackEntry;
var charset:  TCharSetObject;
    charset2: TCharSetObject;
    charset3: TCharSetObject;
begin
  charset := TCharSetObject.Create(CSList.AllocTemp,False);
  CSList.Add(charset);
  charset2 := CSList.FindMust(LCG_Stack[LCG_SP-3].Buf);
  charset3 := CSList.FindMust(LCG_Stack[LCG_SP-1].Buf);
  charset.OpAdd(charset2);
  charset.OpAdd(charset3);
  charset.Used  := True;
  charset2.Used := True;
  charset3.Used := True;
  Result.Buf := charset.Name;
end;

function ActSetDefine(_ptr: pointer): TParserStackEntry;
var charset: TCharSetObject;
    charset2: TCharSetObject;
begin
  charset := TCharSetObject.Create(LCG_Stack[LCG_SP-3].Buf,False);
  CSList.Add(charset);
  charset2 := CSList.FindMust(LCG_Stack[LCG_SP-1].Buf);
  charset.OpAdd(charset2);
  Result.Buf := LCG_Stack[LCG_SP-3].Buf;
end;

function ActSetLiteral(_ptr: pointer): TParserStackEntry;
var charset: TCharSetObject;
begin
  charset := TCharSetObject.Create(CSList.AllocTemp,False);
  CSList.Add(charset);
  charset.SetLiteral(LCG_Stack[LCG_SP-1].Buf);
  Result.Buf := charset.Name;
end;

function ActSetSub(_ptr: pointer): TParserStackEntry;
var charset: TCharSetObject;
    charset2: TCharSetObject;
    charset3: TCharSetObject;
begin
  charset := TCharSetObject.Create(CSList.AllocTemp,False);
  CSList.Add(charset);
  charset2 := CSList.FindMust(LCG_Stack[LCG_SP-3].Buf);
  charset3 := CSList.FindMust(LCG_Stack[LCG_SP-1].Buf);
  charset.OpAdd(charset2);
  charset.OpSubtract(charset3);
  charset.Used  := True;
  charset2.Used := True;
  charset3.Used := True;
  Result.Buf := charset.Name;
end;

function ActSetUse(_ptr: pointer): TParserStackEntry;
var charset: TCharSetObject;
begin
  charset := CSList.FindMust(LCG_Stack[LCG_SP-1].Buf);
  charset.Used := True;
  Result.Buf := LCG_Stack[LCG_SP-1].Buf;
end;

function ActTerminalBracketBracket(_ptr: pointer): TParserStackEntry;
begin
  Result.Buf := LCG_Stack[LCG_SP-2].Buf;
end;

function ActTerminalBracketSetLiteral(_ptr: pointer): TParserStackEntry;
// Create a terminal fragment from a character set literal, e.g. [0-9]
var charset: TCharSetObject;
    frag:    TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  charset := TCharSetObject.Create(CSList.AllocTemp,False);
  CSList.Add(charset);
  charset.SetLiteral(LCG_Stack[LCG_SP-1].Buf);
  frag := _parser.NFA.FragFromCharset(charset.CharacterSet);
  Result.Buf := NFAFragmentToString(frag);
end;

function ActTerminalBracketSetName(_ptr: pointer): TParserStackEntry;
var charset: TCharSetObject;
    cs2:     TCharSetObject;
    frag:    TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  cs2 := CSList.FindMust(LCG_Stack[LCG_SP-1].Buf);
  charset := TCharSetObject.Create(CSList.AllocTemp,False);
  CSList.Add(charset);
  charset.OpAdd(cs2);
  cs2.Used := True;
  frag := _parser.NFA.FragFromCharset(charset.CharacterSet);
  Result.Buf := NFAFragmentToString(frag);
end;

function ActTerminalBracketTerminal(_ptr: pointer): TParserStackEntry;
// Search for a terminal in the list, otherwise...
// Create a terminal fragment from a terminal e.g. goto
var frag:    TNFAFragment;
    _parser: TLacParser;
    index:   integer;
begin
  _parser := TLacParser(_ptr);
  // Check if the id already exists in the terminal list
  index := _parser.TerminalList.IndexOf(LCG_Stack[LCG_SP-1].Buf);
  if index >= 0 then
    begin
      _parser.TerminalList[index].FUsed := True;
      frag.FriendlyName := _parser.TerminalList[index].FTerminalFriendly;
      frag.First        := _parser.TerminalList[index].FFirst;
      frag.Last         := _parser.TerminalList[index].FLast;
    end
  else
    begin
      frag := _parser.NFA.FragFromString(LCG_Stack[LCG_SP-1].Buf);
    end;
  Result.Buf := NFAFragmentToString(frag);
end;

function ActTerminalClosedBracketedOpt(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _frag2 := _parser.NFA.Closure01(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function ActTerminalClosedBracketedPlus(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _frag2 := _parser.NFA.Closure1n(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function ActTerminalClosedBracketedStar(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _frag2 := _parser.NFA.Closure0n(_frag1);
  Result.Buf := NFAFragmentToString(_frag2);
end;

function ActTerminalConcatenate(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _fragx: TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _frag2 := StringToNFAFragment(LCG_Stack[LCG_SP-1].Buf);
  _fragx := _parser.NFA.FragAppend(_frag1,_frag2);
  Result.Buf := NFAFragmentToString(_fragx);
end;

function ActTerminalDef(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-1].Buf);
  _parser.NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := LCG_Stack[LCG_SP-3].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := _parser.TerminalList.Add(obj);
  obj.FTerminalStyle := tsTerminal;
  _parser.NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  _parser.NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  _parser.NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function ActTerminalDefIgnore(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _parser.NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := LCG_Stack[LCG_SP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := _parser.TerminalList.Add(obj);
  obj.FTerminalStyle := tsTerminal;
  obj.FIgnore        := true;
  obj.FUsed          := true;
  _parser.NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  _parser.NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  _parser.NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function ActTerminalDefKeyword(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _parser.NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := LCG_Stack[LCG_SP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := _parser.TerminalList.Add(obj);
  obj.FTerminalStyle := tsKeyword;
  obj.FIgnore        := False;
  obj.FUsed          := False;
  _parser.NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  _parser.NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  _parser.NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function ActTerminalDefSymbol(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    obj:    TTerminalDefObject;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-2].Buf);
  _parser.NFA.FragAttach(0,_frag1);
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := LCG_Stack[LCG_SP-4].Buf;
  obj.FTerminalFriendly := _frag1.FriendlyName;
  obj.FFirst            := _frag1.First;
  obj.FLast             := _frag1.Last;
  obj.FNodeNumber := _parser.TerminalList.Add(obj);
  obj.FTerminalStyle := tsSymbol;
  obj.FIgnore        := False;
  obj.FUsed          := False;
  _parser.NFA.StateList.Items[_frag1.Last].IsAccepting := True;
  _parser.NFA.StateList.Items[_frag1.Last].AcceptToken := obj.FNodeNumber;
  _parser.NFA.StateList.Items[_frag1.Last].TerminalStyle := obj.FTerminalStyle;
  Result.Buf := '';
end;

function ActTerminalOr(_ptr: pointer): TParserStackEntry;
var _frag1: TNFAFragment;
    _frag2: TNFAFragment;
    _fragx: TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  _frag1 := StringToNFAFragment(LCG_Stack[LCG_SP-3].Buf);
  _frag2 := StringToNFAFragment(LCG_Stack[LCG_SP-1].Buf);
  _fragx := _parser.NFA.FragOr(_frag1,_frag2);
  Result.Buf := NFAFragmentToString(_fragx);
end;

function ActTerminalStringLiteral(_ptr: pointer): TParserStackEntry;
// Create a terminal fragment from a string literal e.g. ":="
var frag:    TNFAFragment;
    _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  frag := _parser.NFA.FragFromString(StripQuotes(LCG_Stack[LCG_SP-1].Buf));
  Result.Buf := NFAFragmentToString(frag);
end;

function ActUndefined(_ptr: pointer): TParserStackEntry;
var _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  gLogObject.Log(ltInternal,'Procedure code has not been defined yet');
  Result.Buf := '';
end;

function PROC_XXACCE_XCONTE_XEOFX(_ptr: pointer): TParserStackEntry;
var _parser: TLacParser;
begin
  _parser := TLacParser(_ptr);
  Result := ActIgnore(_parser);
end;

end.

