unit uparser_combinations;

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

//
// Contains the list of all the different combinations we can use for each
// rule. For example, <expr> ::= <expr> + <mulop> has four combinations of
// the red dot and the rule itself:
//
//   <expr> ::= • <expr> + <mulop>
//   <expr> ::= <expr> • + <mulop>
//   <expr> ::= <expr> + • <mulop>
//   <expr> ::= <expr> + <mulop> •

interface

uses
  Classes, SysUtils, fgl, uparser_element, uparser_rule;


type
  TComboObject = class(TObject)
    protected
      ruleno:  integer;            // Rule number for 0..n
      reddot:  integer;            // Red dot position from 0..m
      head:    TParserNonTerminal; // Head of the rule
      prefix:  TParserProduction;  // Production elements before the dot
      suffix:  TParserProduction;  // Production elements after the dot
      next:    TParserElement;     // Next element after the red dot
      used:    boolean;            // Check to see if this combo gets used
    end;

  TComboList = class(specialize TFPGObjectList<TComboObject>)
    public
      procedure AddRules(const _rules: TParserRuleList);
  end;

implementation

procedure TComboList.AddRules(const _rules: TParserRuleList);
//
// Adds the rules by splitting into all the combinations
//
var i,j: integer;
    combo: TComboObject;
begin
  for i := 0 to _rules.Count-1 do
    for j := 0 to _rules[i].Production.Count-1 do
      begin
        combo := TComboObject.Create;
        Add(combo);
      end;
end;

end.

