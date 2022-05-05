unit uparser_nfa;

{$mode objfpc}{$H+}

//
// NFA routines to deal with the NFA and fragments
//

interface

uses
  Classes, SysUtils, uparser_nfastate, uparser_types, ucharset32,
  uparser_terminal, deployment_parser_types_12;

type

  TNFAFragment = record
    FriendlyName: TString;
    First:        TStateIdentifier;
    Last:         TStateIdentifier;
  end;

  TNFA = class(TObject)
    protected
      FStateList: TStateList;
      function  GetState(AIndex: integer): TState;
    public
      Start: TStateIdentifier;
      constructor Create;
      destructor Destroy; override;
      function  AllocateState(_isaccepting: boolean = False; _accepttoken: TTokenIdentifier = 0): TStateIdentifier;
      function  Closure01(const frag1: TNFAFragment): TNFAFragment;
      function  Closure0n(const frag1: TNFAFragment): TNFAFragment;
      function  Closure1n(const frag1: TNFAFragment): TNFAFragment;
//      procedure CombineCharSets(_cslist: TCharSetList);
      procedure Dump(_strm: TStream; _terminals: TTerminalList);
      function  FragAppend(const frag1, frag2: TNFAFragment): TNFAFragment;
      procedure FragAttach(_id: TStateIdentifier; const frag: TNFAFragment);
      function  FragFromCharset(cs: TCharSet): TNFAFragment;
      function  FragFromString(const s: TString): TNFAFragment;
      function  FragFromStringCI(const s: TString): TNFAFragment;
      function  FragOr(const frag1, frag2: TNFAFragment): TNFAFragment;
      function  FragSplit(const frag1, frag2: TNFAFragment): TNFAFragment;
      procedure MinimiseCharSets;
      property  Items[AIndex: integer]: TState read GetState;
      property  StateList: TStateList read FStateList write FStateList;
  end;


function NFAFragmentToString(_frag: TNFAFragment): TString;
function StringToNFAFragment(_s: TString): TNFAFragment;


var
  NFA: TNFA;


implementation

uses
  uparser_utility, deployment_parser_module_12;

{ Utility code }

function NFAFragmentToString(_frag: TNFAFragment): TString;
begin
  Result := IntToStr(_frag.First) + #9 + IntToStr(_frag.Last) + #9 + _frag.FriendlyName;
end;

function StringToNFAFragment(_s: TString): TNFAFragment;
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := #9;
    sl.DelimitedText := _s;
    Result.First        := StrToInt(sl[0]);
    Result.Last         := StrToInt(sl[1]);
    Result.FriendlyName := sl[2];
  finally
    sl.Free;
  end;
end;

{ TNFA code }

constructor TNFA.Create;
begin
  inherited Create;
  FStateList := TStateList.Create;
end;

destructor TNFA.Destroy;
begin
  FStateList.Free;
  inherited Destroy;
end;

function TNFA.AllocateState(_isaccepting: boolean; _accepttoken: TTokenIdentifier): TStateIdentifier;
begin
  AllocateState := FStateList.AllocateState(_isaccepting,_accepttoken);
end;

function TNFA.Closure01(const frag1: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := '(' + frag1.FriendlyName + ')?';
  result.First := FStateList.AllocateState();
  result.Last  := FStateList.AllocateState();
  FStateList.Items[result.First].AddAction(frag1.First,atEpsilon);
  FStateList.Items[result.First].AddAction(result.Last,atEpsilon);
  FStateList.Items[frag1.Last].AddAction(result.Last,atEpsilon);
end;

function TNFA.Closure0n(const frag1: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := '(' + frag1.FriendlyName + ')*';
  result.First := FStateList.AllocateState();
  result.Last  := FStateList.AllocateState();
  FStateList.Items[result.First].AddAction(frag1.First,atEpsilon);
  FStateList.Items[result.First].AddAction(result.Last,atEpsilon);
  FStateList.Items[frag1.Last].AddAction(result.Last,atEpsilon);
  FStateList.Items[frag1.Last].AddAction(frag1.First,atEpsilon);
end;

function TNFA.Closure1n(const frag1: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := '(' + frag1.FriendlyName + ')+';
  result.First := FStateList.AllocateState();
  result.Last  := FStateList.AllocateState();
  FStateList.Items[result.First].AddAction(frag1.First,atEpsilon);
  FStateList.Items[frag1.Last].AddAction(result.Last,atEpsilon);
  FStateList.Items[frag1.Last].AddAction(frag1.First,atEpsilon);
end;

{
procedure TNFA.CombineCharSets(_cslist: TCharSetList);
begin
  StateList.CombineCharsets(_cslist);
end;
}

procedure TNFA.Dump(_strm: TStream; _terminals: TTerminalList);
begin
  FStateList.Dump(_strm,_terminals);
end;

function TNFA.FragAppend(const frag1, frag2: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := frag1.FriendlyName+'&'+frag2.FriendlyName;
  result.First := frag1.First;
  result.Last  := frag2.Last;
  // Now join the two
  FStateList.Items[frag1.Last].AddAction(frag2.First,atEpsilon);
end;

procedure TNFA.FragAttach(_id: TStateIdentifier; const frag: TNFAFragment);
begin
  FStateList.Items[_id].AddAction(frag.First,atEpsilon);;
end;

function TNFA.FragFromCharset(cs: TCharSet): TNFAFragment;
var c: TChar;
begin
  result.FriendlyName := '';
  result.First := FStateList.AllocateState();
  result.Last  := FStateList.AllocateState();
  for c in TSetOfChar do
    if c in cs.FSet then
      FStateList.Items[result.First].AddAction(result.Last,atCharacter,c);
  result.FriendlyName := cs.AsText([cslBrackets,cslRanges,cslTranslate]);
end;

function TNFA.FragFromString(const s: TString): TNFAFragment;
var states: array of TStateIdentifier;
    i:      integer;
    L:      integer;
begin
  L := Length(s);
  // Set up the states array
  SetLength(states,L+1);
  for i := 0 to L do
    states[i] := FStateList.AllocateState();
  // Fill in the actions
  for i := 0 to L-1 do
    FStateList.Items[states[i]].AddAction(states[i+1],atCharacter,s[i+1]);
  // Finish off
  FragFromString.FriendlyName := MakeFriendly(s);
  FragFromString.First        := states[0];
  FragFromString.Last         := states[L];
end;

function TNFA.FragFromStringCI(const s: TString): TNFAFragment;
var states: array of TStateIdentifier;
    i:      integer;
    L:      integer;
begin
  L := Length(s);
  // Set up the states array
  SetLength(states,L+1);
  for i := 0 to L do
    states[i] := FStateList.AllocateState();
  // Fill in the actions
  for i := 0 to L-1 do
    begin
      FStateList.Items[states[i]].AddAction(states[i+1],atCharacter,s[i+1]);
      if IsLetter(s[i+1]) then
          FStateList.Items[states[i]].AddAction(states[i+1],atCharacter,OppositeCase(s[i+1]));
    end;
  // Finish off
  FragFromStringCI.FriendlyName := 'MIXED(' + UpperCase(MakeFriendly(s)) + ')';
  FragFromStringCI.First        := states[0];
  FragFromStringCI.Last         := states[L];
end;

function TNFA.FragOr(const frag1, frag2: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := frag1.FriendlyName+'|'+frag2.FriendlyName;
  result.First := FStateList.AllocateState();
  result.Last  := FStateList.AllocateState();
  FStateList.Items[result.First].AddAction(frag1.First,atEpsilon);;
  FStateList.Items[result.First].AddAction(frag2.First,atEpsilon);;
  FStateList.Items[frag1.Last].AddAction(result.Last,atEpsilon);;
  FStateList.Items[frag2.Last].AddAction(result.Last,atEpsilon);;
end;

function TNFA.FragSplit(const frag1, frag2: TNFAFragment): TNFAFragment;
begin
  result.FriendlyName := frag1.FriendlyName+'>'+frag2.FriendlyName;
  result.First := FStateList.AllocateState();
  FStateList.Items[result.First].AddAction(frag1.First,atEpsilon);;
  FStateList.Items[result.First].AddAction(frag2.First,atEpsilon);;
end;

function TNFA.GetState(AIndex: integer): TState;
begin
  GetState := FStateList[AIndex];
end;

procedure TNFA.MinimiseCharSets;
begin
//  StateList.MinimiseCharsets;
end;

end.

