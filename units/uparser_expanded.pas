unit uparser_expanded;

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

interface

uses
  Classes, SysUtils, uparser_rule, uparser_terminal, uparser_types, Generics.Collections;

type

  TExpandedEntry = class(TObject)
    protected
      FRule: TRule;
      FTerminalList: TTerminalList;
    public
      RuleNo: TTokenIdentifier;   // Index number of the rule, 0..rules-1
      Head:   TTokenIdentifier;   // Symbol table index of the head of the rule
      Next:   TTokenIdentifier;   // Next symbol in the rule or -1 if no next
      Offset: integer;            // Offset into the rule table 0..n-1
      RLen:   integer;            // Length of the rule table (could be zero)
      Accept: boolean;            // True if this is the accepting state
      constructor Create(_ruleno: integer; _offset: integer; _rule: TRule; _terminals: TTerminalList);
      function AsText: string;
      function AsTextFormatted: string;
      function RuleText: string;
  end;

  TExpandedList = class(specialize TObjectList<TExpandedEntry>)
    public
      procedure AddRule(_ruleno: integer; _rule: TRule; _terminals: TTerminalList; _accept: boolean);
      function  AsText(_ruleno: integer): string;
      function  AsTextFormatted(_ruleno: integer): string;
      function  Bump(_index: integer): integer;
      procedure Dump(_strm: TStream);
      procedure DumpXML(_strm: TStream);
      function  IndexOf(_ruleno: integer; _offset: integer): integer; overload;
      procedure ExpandRules(_rules: TRuleList; _terminals: TTerminalList);
      function  TableContains(_head: TTokenIdentifier): boolean;
      function  UnBump(_index: integer): integer;
  end;

implementation

uses
  uparser_utility, htmlelements, uparser_exception,
  deployment_parser_module_12;

{ TExpandedEntry code }

constructor TExpandedEntry.Create(_ruleno: integer; _offset: integer; _rule: TRule; _terminals: TTerminalList);
begin
  inherited Create;
  FRule         := _rule;
  FTerminalList := _terminals;
  RuleNo := _ruleno;
  Head   := FRule.RuleHead;
  if _offset = FRule.Count then
    Next := NO_TOKEN_IDENTIFIER
  else
    Next := FRule.Items[_offset];
  Offset := _offset;
  RLen   := _rule.Count;
end;

function TExpandedEntry.AsText: string;
var i: integer;
    AccStr: string;
begin
  if Accept then
    AccStr := 'Y'
  else
    AccStr := 'n';
  Result := Format('RuleNo=%5.5d,Head=%5.5d,Next=%5.5d,Offs=%2d,acc=%s ',[RuleNo,Head,Next,Offset,AccStr]);
  Result := Result + MakePrintable(FTerminalList.TerminalName(Head)) + ' : ';
  for i := 0 to Offset-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
  Result := Result + '● ';
  for i := Offset to FRule.Count-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
end;

function TExpandedEntry.AsTextFormatted: string;
var i: integer;
begin
  Result := '';
  for i := 0 to Offset-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
  Result := Result + '● ';
  for i := Offset to FRule.Count-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
end;

function TExpandedEntry.RuleText: string;
var i: integer;
begin
  Result := MakePrintable(FTerminalList.TerminalName(Head)) + ' : ';
  for i := 0 to Offset-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
  Result := Result + '. ';
  for i := Offset to FRule.Count-1 do
    Result := Result + MakePrintable(FTerminalList.TerminalName(FRule.items[i])) + ' ';
end;

{ TExpandedList code }

procedure TExpandedList.AddRule(_ruleno: integer; _rule: TRule; _terminals: TTerminalList; _accept: boolean);
var i: integer;
    obj: TExpandedEntry;
begin
  for i := 0 to _rule.Count do
    begin
      obj := TExpandedEntry.Create(_ruleno,i,_rule,_terminals);
      if _accept and (i = _rule.Count) then
        obj.Accept := true;
      Add(obj);
    end;
  _rule.Used := True;
end;

function TExpandedList.AsText(_ruleno: integer): string;
begin
  Result := Items[_ruleno].AsText;
end;

function TExpandedList.AsTextFormatted(_ruleno: integer): string;
begin
  Result := Items[_ruleno].AsTextFormatted;
end;

function TExpandedList.Bump(_index: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    begin
      if (Items[i].Offset = Items[_index].Offset + 1) and
         (Items[i].RuleNo = Items[_index].RuleNo) then
        Exit(i);
    end;
end;

procedure TExpandedList.Dump(_strm: TStream);
var i: integer;
begin
  WriteLnStringToStreamUnderlined(_strm,'Expanded Rule List');
  for i := 0 to Count-1 do
    WriteLnStringToStream(_strm,Format('%5d: ',[i]) + Items[i].AsText);
  WriteLnStringToStream(_strm,'');
end;

procedure TExpandedList.DumpXML(_strm: TStream);
var i: integer;
  function BoolToYN(_b: boolean): string;
  begin
    if _b then
      Result := 'Yes'
    else
      Result := '';
  end;
begin
  WriteLnStringToStream(_strm,'  <ExpandedRules>');
  for i := 0 to Count-1 do
    WriteLnStringToStream(_strm,'    <Rule><Index>' + IntToStr(i) + '</Index>' +
                                '<RuleIndex>' + IntToStr(Items[i].RuleNo) + '</RuleIndex>' +
                                '<Head>'    + IntToStr(Items[i].Head)   + '</Head>' +
                                '<Next>'    + IntToStr(Items[i].Next)   + '</Next>' +
                                '<Offset>'  + IntToStr(Items[i].Offset) + '</Offset>' +
                                '<Length>'  + IntToStr(Items[i].RLen)   + '</Length>' +
                                '<Accept>'  + BoolToYN(Items[i].Accept) + '</Accept>' +
                                '<Display>' + EscapeHTML(Items[i].AsTextFormatted) + '</Display>' +
                                '</Rule>');
  WriteLnStringToStream(_strm,'  </ExpandedRules>');
end;

procedure TExpandedList.ExpandRules(_rules: TRuleList; _terminals: TTerminalList);
var expanding: boolean;
    i:  integer;
    j:  TRuleIdentifier;
    next: TTokenIdentifier;
    found: boolean;
begin
  expanding := True;  // Assume expanding for now
  while expanding do
    begin
      expanding := False; // But we have to prove we are expanding...
      for i := 0 to Count-1 do
        begin
          next := Items[i].Next;
          if (next <> NO_TOKEN_IDENTIFIER) and
             (_terminals.TerminalStyle(next) in [tsNonTerminal]) and
             (not TableContains(next)) then
            begin
             Found := False;
             for j := 0 to _rules.Count-1 do
               if next = _rules[j].RuleHead then
                 begin
                   AddRule(j,_rules[j],Items[i].FTerminalList,False);
                   Expanding := True;
                   Found := True;
                 end;
             if not Found then
               raise Exception.Create('Could not find the definition of non-terminal ' + MakePrintable(_terminals.TerminalName(next)));
            end;
        end;
    end;
end;

function TExpandedList.IndexOf(_ruleno: integer; _offset: integer): integer;
var i: integer;
begin
  for i := 0 to Count-1 do
    if (Items[i].RuleNo = _ruleno) and (Items[i].Offset = _offset) then
      Exit(i);
  Exit(-1);
end;

function TExpandedList.TableContains(_head: TTokenIdentifier): boolean;
var i: integer;
    h: TTokenIdentifier;
begin
  Result := False;
  for i := 0 to Count-1 do
    begin
      h := Items[i].Head;
      if h = _head then
        Exit(True);
    end;
end;

function TExpandedList.UnBump(_index: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    begin
      if (Items[i].Offset = Items[_index].Offset - 1) and
         (Items[i].RuleNo = Items[_index].RuleNo) then
        Exit(i);
    end;
end;

end.

