unit uparser_combinations;

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

