unit uparser_rule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uparser_types, uparser_terminal;

type
  //
  // TRuleAtom specifies the most granular part of a rule which can be a
  // terminal or non-terminal
  //
  TRuleAtom = TTokenIdentifier;

  //
  // TRule specifies the list of atoms for each rule. It's allowed for this to
  // be empty. TRule also holds the procedure which can be specified along with
  // the non-terminal used as the parent
  //
  TRule = class(specialize TFPGList<TRuleAtom>)
    protected
      FRuleID:   string;           // Name of the rule based on the atoms
      FRuleProc: string;           // Name of the rule procedure
      FRuleHead: TTokenIdentifier; // Token ID of the head of the rule
      FUsed:     boolean;
    public
      function  AsText(_terminals: TTerminalList; _withproc: boolean = True): string;
      function  BaseFragment(id: TTokenIdentifier; _terminals: TTerminalList): string;
      function  BaseID(_terminals: TTerminalList): string;
      function  BaseProc(_terminals: TTerminalList): string;
      procedure CreateRuleFromString(const nonterm: string; s: string);
      property RuleID:   string           read FRuleID   write FRuleID;
      property RuleProc: string           read FRuleProc write FRuleProc;
      property RuleHead: TTokenIdentifier read FRuleHead write FRuleHead;
      property Used: boolean read FUsed write FUsed;
  end;

  //
  // TRuleList is a list of all the rules
  //
  TRuleList = class(specialize TFPGObjectList<TRule>)
    protected
      FSuggestions: TStringList;
    public
      constructor Create;
      destructor Destroy; override;
      procedure CreateRuleFromString(const nonterm: string; const s: string; _terminals: TTerminalList);
      procedure Dump(_strm: TStream; _terminals: TTerminalList);
      procedure DumpXML(_strm: TStream; _terminals: TTerminalList);
      function  SuggestID(_rule: TRule; _terminals: TTerminalList): string;
      function  SuggestProc(_rule: TRule; _terminals: TTerminalList): string;
  end;


  var
    startnonterminal: string;


implementation

uses
  uparser_utility, strutils, htmlelements, uparser_exception,
  deployment_parser_module;

{ TRule code }

function TRule.AsText(_terminals: TTerminalList; _withproc: boolean): string;
var i: integer;
    ruleno: integer;
begin
  Result := _terminals.Items[RuleHead].FTerminalName + ' :';
  for i := 0 to Count-1 do
    begin
      ruleno := Items[i];
      Result := Result + ' ' + _terminals.Items[ruleno].FTerminalName;
    end;
  if _withproc then
    Result := Result + ' # ' + RuleProc;
end;

function TRule.BaseFragment(id: TTokenIdentifier; _terminals: TTerminalList): string;
begin
  Result := UpperCase(_terminals.Items[id].FTerminalName);
  Result := StringReplace(Result,'<','',[rfReplaceAll]);
  Result := StringReplace(Result,'>','',[rfReplaceAll]);
  Result := StringReplace(Result,'.','_',[rfReplaceAll]);
  Result := StringAsText(Result);
  {
  for i := 1 to Length(Result) do
    begin
      case Result[i] of
        '0'..'9',
        'A'..'Z': ; // Do nothing
        otherwise Result[i] := 'X';
      end;
    end;
  }
end;

function TRule.BaseID(_terminals: TTerminalList): string;
var i: integer;
begin
  Result := BaseFragment(RuleHead,_terminals);
  for i := 0 to Count-1 do
    Result := Result + '_' + BaseFragment(Items[i],_terminals);
end;

function TRule.BaseProc(_terminals: TTerminalList): string;
begin
  Result := 'PROC_' + BaseID(_terminals);
end;

procedure TRule.CreateRuleFromString(const nonterm: string; s: string);
var sl: TStringList;
    i:  integer;
begin
  // Strip out # procname if present
  if Pos('#',s) > 0 then
    begin
      RuleProc := Copy(s,Pos('#',s)+1,9999);
      s := LeftStr(s,Pos('#',s)-1);
    end;
  // Now we just have term:nonterm:term:etc
  RuleHead := StrToInt(nonterm);
  sl := TStringList.Create;
  try
    sl.Delimiter := ':';
    sl.DelimitedText := s;
    for i := 0 to sl.Count-1 do
      Add(StrToInt(sl[i]));
  finally
    sl.Free;
  end;
end;

{ TRuleList code}

constructor TRuleList.Create;
begin
  inherited Create;
  FSuggestions := TStringList.Create;
end;

destructor TRuleList.Destroy;
begin
  FSuggestions.Free;
  inherited Destroy;
end;

procedure TRuleList.CreateRuleFromString(const nonterm: string; const s: string; _terminals: TTerminalList);
var rule: TRule;
    s2:   string;
    part: string;
begin
  if Pos('|',s) > 0 then
    begin
      s2 := s;
      while Pos('|',s2) > 0 do
        begin
          part := LeftStr(s2,Pos('|',s2)-1);
          s2 := RightStr(s2,Length(s2)-Pos('|',s2));
          CreateRuleFromString(nonterm,part,_terminals);
        end;
      CreateRuleFromString(nonterm,s2,_terminals);
    end
  else
    begin // Just has a single rule
      rule := TRule.Create;
      Add(rule);
      rule.CreateRuleFromString(nonterm,s);
      {
      if _terminals.Items[StrToInt(nonterm)].FTerminalName = startnonterminal then
        rule.Add(_terminals.IndexOf('(EOF)'));
      }
      if rule.RuleID = '' then
        rule.RuleID := SuggestID(rule,_terminals);
      if rule.RuleProc = '' then
        rule.RuleProc := SuggestProc(rule,_terminals);
    end;
end;

procedure TRuleList.Dump(_strm: TStream; _terminals: TTerminalList);
var i: TRuleIdentifier;
    utext: string;
begin
  WriteLnStringToStreamUnderlined(_strm,'RULES');
  for i := 0 to Count-1 do
    begin
      utext := '';
      if not Items[i].Used then
        utext := '*UNUSED* ';
      WriteLnStringToStream(_strm,IntToStr(i)+': ' + utext + Items[i].AsText(_terminals));
    end;
  WriteLnStringToStream(_strm,'');
end;

procedure TRuleList.DumpXML(_strm: TStream; _terminals: TTerminalList);
var i: TRuleIdentifier;
begin
  WriteLnStringToStream(_strm,'  <Rules>');
  for i := 0 to Count-1 do
    WriteLnStringToStream(_strm,'    <Rule><Index>' + IntToStr(i)+'</Index><Content>' + EscapeHTML(Items[i].AsText(_terminals)) + '</Content></Rule>');
  WriteLnStringToStream(_strm,'  </Rules>');
end;

function TRuleList.SuggestID(_rule: TRule; _terminals: TTerminalList): string;
CONST MAX_NUMBER = 9999;
var rulebase: string;
    i:        integer;
    ok:       boolean;
begin
  rulebase := _rule.BaseID(_terminals);
  if FSuggestions.IndexOf(rulebase) >= 0 then
    begin // Flip to numbered mode
      ok := False;
      i := 0;
      while not OK do
        begin
          if i > MAX_NUMBER then
            raise Exception.Create('Overflow in TRuleList::SuggestID()');
          Result := Format('%s_%4.4d',[rulebase,i]);
          if FSuggestions.IndexOf(Result) < 0 then
            ok := True
          else
            Inc(i);
        end;
    end
  else
    Result := rulebase;
  FSuggestions.Add(Result);
end;

function TRuleList.SuggestProc(_rule: TRule; _terminals: TTerminalList): string;
CONST MAX_NUMBER = 9999;
var rulebase: string;
    i:        integer;
    ok:       boolean;
begin
  rulebase := _rule.BaseProc(_terminals);
  if FSuggestions.IndexOf(rulebase) >= 0 then
    begin // Flip to numbered mode
      ok := False;
      i := 0;
      while not OK do
        begin
          if i > MAX_NUMBER then
            raise Exception.Create('Overflow in TRuleList::SuggestProc()');
          Result := Format('%s_%4.4d',[rulebase,i]);
          if FSuggestions.IndexOf(Result) < 0 then
            ok := True
          else
            Inc(i);
        end;
    end
  else
    Result := rulebase;
  FSuggestions.Add(Result);
end;

end.

