unit uparser_canonical;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uparser_expanded, uparser_types, uparser_terminal,
  uparser_lalrsets, uparser_exception;

type
  TCanonicalTableEntry = class(TObject)
    public
      State:        TStateIdentifier;  // State number
      IndexExp:     integer;           // Index into expanded table
      Next:         TTokenIdentifier;  // The next token or FFFF
      IsEpsilon:    boolean;           // True of an empty rule
      Shift:        TStateIdentifier;  // SHIFT - new state number for terminal
      Reduce:       TRuleIdentifier;   // REDUCE - rule number, index into TRuleList
      PGoto:        TStateIdentifier;  // GOTO - new state for non-terminal
      Accept:       boolean;           // ACCEPT - true if this is the accepting state
      Expanded:     boolean;           // True if this entry has been expanded
  end;

  TCanonicalTable = class(specialize TFPGObjectList<TCanonicalTableEntry>)
    protected
      FExpanded:    TExpandedList;
      FTerminals:   TTerminalList;
    public
      procedure AssignActions(_state: TStateIdentifier);
      function  AssignState(_expanded: integer; _state: TStateIdentifier; _next: TTokenIdentifier): TStateIdentifier;
      procedure CheckRR(FollowSetList: TParserSetList);
      procedure CheckSR(FollowSetList: TParserSetList);
      procedure CreateFromExpanded(_start: TTokenIdentifier; _expanded: TExpandedList; _terminals: TTerminalList);
      procedure Dump(_strm: TStream);
      procedure DumpXML(_strm: TStream);
      procedure ExpandStates;
      function  HighestState: TStateIdentifier;
      procedure ProcessState(_state: TStateIdentifier);
      function  StateCanonExists(_state: TStateIdentifier; _canon: integer): boolean;
      function  StateExists(_ruleno: integer; _offset: integer): TStateIdentifier;
  end;

function ParserCompareFunc(const Item1, Item2: TCanonicalTableEntry): Integer;

implementation

uses
  uparser_utility, htmlelements;

{ Utility code }

function ParserCompareFunc(const Item1, Item2: TCanonicalTableEntry): Integer;
begin
  if Item1.State > Item2.State then
    Exit(1)
  else if Item1.State < Item2.State then
    Exit(-1)
  else if Item1.IndexExp > Item2.IndexExp then
    Exit(1)
  else if Item1.IndexExp < Item2.IndexExp then
    Exit(-1)
  else
    Exit(0);
end;

{ TCanonicalTable }

procedure TCanonicalTable.AssignActions(_state: TStateIdentifier);
var entry:    TCanonicalTableEntry;
    expanded: TExpandedEntry;
    newstate: TStateIdentifier;
begin
  // Do accept / reduce / terminals first
  for entry in Self do
    if entry.State = _state then
      begin // Matches our state to be allocated
        expanded := FExpanded.Items[entry.IndexExp];
        if expanded.Accept then  // Check for accepting state first
          entry.Accept := True
        else if expanded.Offset = expanded.RLen then
          // We are at the end of the rule so need to reduce on that rule number
          entry.Reduce := expanded.RuleNo
        else
          begin
            // Check if we can process this item
            if (not (FTerminals.Items[entry.next].FTerminalStyle in [tsNonTerminal])) then
              begin
                // See if a new state exists for the next item, create if not
                newstate := AssignState(FExpanded.Bump(entry.IndexExp),entry.State,entry.next);
                entry.Shift := newstate;
              end;
          end;
      end;
  // Now check for non-terminals
  for entry in Self do
    if entry.State = _state then
      begin // Matches our state to be allocated
        expanded := FExpanded.Items[entry.IndexExp];
        if (entry.next <> NO_STATE_IDENTIFIER) and
           (FTerminals.Items[entry.next].FTerminalStyle in [tsNonTerminal]) then
          begin
            newstate := AssignState(FExpanded.Bump(entry.IndexExp),entry.State,entry.next);
            entry.PGoto := newstate;
          end;
      end;
end;

function TCanonicalTable.AssignState(_expanded: integer; _state: TStateIdentifier; _next: TTokenIdentifier): TStateIdentifier;
var entry:    TCanonicalTableEntry;
    newstate: integer;
    i:        integer;
begin
  newstate := NO_STATE_IDENTIFIER;
  // Precheck for existing parent state
  for i := 0 to Count-1 do
    if (Items[i].State = _state) and (Items[i].next = _next) and (Items[i].PGoto <> NO_STATE_IDENTIFIER) then
      Exit(Items[i].PGoto);
  for i := 0 to Count-1 do
    if Items[i].IndexExp = _expanded then
      begin
        newstate := i;
        break;
      end;
  if newstate <> NO_STATE_IDENTIFIER then
    Result := Items[newstate].State
  else
    begin
      // Need to create new records for every matching state + next
      // situation, e.g. State 0 with three records where next is <expr>
      // for example
      newstate := HighestState + 1;
      for i := 0 to Count-1 do
        if (Items[i].State = _state) and (Items[i].Next = _next) then
          begin
            entry := TCanonicalTableEntry.Create;
            entry.State     := newstate;
            entry.IndexExp  := FExpanded.Bump(Items[i].IndexExp);
            entry.Next      := FExpanded[entry.IndexExp].Next;
            entry.IsEpsilon := False;
            entry.Shift     := NO_STATE_IDENTIFIER;
            entry.Reduce    := NO_STATE_IDENTIFIER;
            entry.PGoto     := NO_STATE_IDENTIFIER;
            entry.Accept    := False;
            entry.Expanded  := False;
            if FExpanded[entry.IndexExp].RLen = 0 then
              entry.IsEpsilon := True;
            Add(entry);
          end;
      Result := newstate;
    end;
end;
 
procedure TCanonicalTable.CheckRR(FollowSetList: TParserSetList);
var istate:   TStateIdentifier;
    i,j:      TStateIdentifier;
    k:        integer;
    m,n:      integer;
    head1:    TTokenIdentifier;
    head2:    TTokenIdentifier;
    follow1:  TParserSet;
    follow2:  TParserSet;
    intersect: boolean;
begin
  //
  // Check for Reduce/Reduce conflicts
  //
  // If a state contains more than one reduce action, it could potentially
  // be a R/R conflict. For example:
  //
  //   A : a .
  //   B : b .
  //
  // It's only really a conflict if Follow(A) intersects Follow(B). In this
  // situation we issue a warning. Later steps to create the output table
  // will simply overwrite the earlier production with the latest one.
  //
  for istate := 0 to HighestState do
    begin
      // State loop
      for i := 0 to Count-1 do
        if (Items[i].State = istate) and (Items[i].Reduce <> NO_STATE_IDENTIFIER) then
          begin
            // Inner loop as we have a reduction item
            head1 := FExpanded[Items[i].IndexExp].Head;
            for j := i+1 to Count-1 do
              if (Items[j].State = istate) and (Items[j].Reduce <> NO_STATE_IDENTIFIER) then
                begin
                  // We have a reduce rule on both "i" and "j" so let's find
                  // if the follow sets of the head items intersect
                  head2 := FExpanded[Items[j].IndexExp].Head;
                  m := FollowSetList.IndexOf(head1);
                  n := FollowSetList.IndexOf(head2);
                  if (m >= 0) and (n >= 0) then  // Don't proceed if Follow(A) or Follow(B) are empty
                    begin
                      follow1 := FollowSetList.Items[m];
                      follow2 := FollowSetList.Items[n];
                      intersect := False;
                      for k := 0 to Length(follow1.PSet)-1 do
                        if follow2.IndexOf(follow1.PSet[k]) >= 0 then
                          intersect := True;
                      if intersect then
                        raise Exception.Create(Format('Reduce/Reduce conflict in state %d for rules (%s) (%s)',[istate,FExpanded[Items[i].IndexExp].RuleText,FExpanded[Items[j].IndexExp].RuleText]));
                    end;

                end;
          end;
    end;
end;

procedure TCanonicalTable.CheckSR(FollowSetList: TParserSetList);
var istate:   TStateIdentifier;
    i,j:      TStateIdentifier;
    k:        integer;
    head:     TTokenIdentifier;
    next_tok: TTokenIdentifier;
    follow:   TParserSet;
begin
  //
  // Check for Shift/Reduce conflicts
  //
  // If a state contains both shift and reduce actions, it could potentially
  // be an S/R conflict. For example:
  //
  //   A : b .
  //   X : X . a
  //
  // It's only really a S/R conlict if a is in Follow(A). In this situation
  // we issue a warning. Later steps to create the output table will prioritise
  // the shift action over the reduce action.
  //
  for istate := 0 to HighestState do
    begin
      // State loop
      for i := 0 to Count-1 do
        if (Items[i].State = istate) and (Items[i].Reduce <> NO_STATE_IDENTIFIER) then
          begin
            // Inner loop as we have a reduction item
            head := FExpanded[Items[i].IndexExp].Head;
            for j := 0 to Count-1 do
              if (Items[j].State = istate) and (Items[j].Shift <> NO_STATE_IDENTIFIER) then
                begin
                  // We have a reduce rule on "i" and a shift on "j" so let's
                  // find the next item to shift and check it with the follow
                  // set of the reduce rule head
                  next_tok := FExpanded[Items[j].IndexExp].Next;
                  k := FollowSetList.IndexOf(head);
                  if k >= 0 then  // Follow(A) is empty so ignore
                    begin
                      follow := FollowSetList.Items[k];
                      if follow.IndexOf(next_tok) >= 0 then
                        raise Exception.Create(Format('Shift/Reduce conflict in state %d for rules (%s) (%s)',[istate,FExpanded[Items[j].IndexExp].RuleText,FExpanded[Items[i].IndexExp].RuleText]));
                    end;
                end;
          end;
    end;
end;

procedure TCanonicalTable.CreateFromExpanded(_start: TTokenIdentifier; _expanded: TExpandedList; _terminals: TTerminalList);
var canon:  TExpandedEntry;
    obj:    TCanonicalTableEntry;
    state:  integer;
begin
  FExpanded := _expanded;
  FTerminals := _terminals;
  // Create state zero by taking all the items where the head is the start
  // non-terminal
  for canon in _expanded do
    if (canon.Head = _start) and (canon.Offset = 0) then
      begin
        obj := TCanonicalTableEntry.Create;
        obj.State      := 0;  // Our initial state is always zero
        obj.IndexExp  := _expanded.IndexOf(canon);
        obj.Next       := canon.Next;
        obj.IsEpsilon  := False;
        obj.Shift      := NO_STATE_IDENTIFIER;
        obj.Reduce     := NO_STATE_IDENTIFIER;
        obj.PGoto      := NO_STATE_IDENTIFIER;
        obj.Accept     := False;
        obj.Expanded   := False;
        if FExpanded.Items[obj.IndexExp].RLen = 0 then
          obj.IsEpsilon := True;
        Add(obj);
      end;
  // Now we have the initial state, perform the allocations and expansions
  state := 0;
  repeat
    ProcessState(state);
    Inc(state);
  until state > HighestState;
end;

procedure TCanonicalTable.Dump(_strm: TStream);
var entry: TCanonicalTableEntry;
    i:     integer;

  procedure Write5(_x: TStateIdentifier);
  begin
    if _x = NO_STATE_IDENTIFIER then
      WriteStringToStream(_strm,'    - ')
    else
      WriteStringToStream(_strm,Format('%5d ',[_x]));
  end;

  procedure WriteB(_b: boolean);
  begin
    if _b  then
      WriteStringToStream(_strm,'True  ')
    else
      WriteStringToStream(_strm,'False ');
  end;

  procedure WriteE(_b: boolean);
  begin
    if _b  then
      WriteStringToStream(_strm,'Y ')
    else
      WriteStringToStream(_strm,'  ');
  end;

begin
  WriteLnStringToStreamUnderlined(_strm,'Parser Table');
  WriteLnStringToStream(_strm,'');
  WriteLnStringToStream(_strm,'Entry State Canon  Next ε Shift Reduc PGoto Accep Expan AsText');
  WriteLnStringToStream(_strm,'----- ----- ----- ----- - ----- ----- ----- ----- ----- ------');
  i := 0;
  for entry in Self do
    begin
      Write5(i); // Index number should go here
      Write5(entry.State);
      Write5(entry.IndexExp);
      Write5(entry.Next);
      WriteE(entry.IsEpsilon);
      Write5(entry.Shift);
      Write5(entry.Reduce);
      Write5(entry.PGoto);
      WriteB(entry.Accept);
      WriteB(entry.Expanded);
      WriteLnStringToStream(_strm,FExpanded.AsText(entry.IndexExp));
      Inc(i);
    end;
  WriteLnStringToStream(_strm,'');
end;

procedure TCanonicalTable.DumpXML(_strm: TStream);
var entry: TCanonicalTableEntry;
    i:     integer;
    s:     string;

  function Write5(_x: TStateIdentifier): string;
  begin
    if _x = NO_STATE_IDENTIFIER then
      Result := ''
    else
      Result := IntToStr(_x);
  end;

  procedure WriteB(_b: boolean);
  begin
    if _b  then
      WriteStringToStream(_strm,'True  ')
    else
      WriteStringToStream(_strm,'False ');
  end;

  function WriteE(_b: boolean): string;
  begin
    if _b  then
      Result := 'Y'
    else
      Result := '';
  end;

begin
  i := 0;
  WriteLnStringToStream(_strm,'  <CanonicalList>');
  for entry in Self do
    begin
      s := '';
      s := s + '    <Entry>';
      s := s + '<Index>'     + IntToStr(i)               + '</Index>';
      s := s + '<State>'     + IntToStr(entry.State)     + '</State>';
      s := s + '<IndexExp>'  + IntToStr(entry.IndexExp)  + '</IndexExp>';
      s := s + '<Next>'      + Write5(entry.Next)        + '</Next>';
      s := s + '<IsEpsilon>' + WriteE(entry.IsEpsilon)   + '</IsEpsilon>';
      s := s + '<Shift>'     + Write5(entry.Shift)       + '</Shift>';
      s := s + '<Reduce>'    + Write5(entry.Reduce)      + '</Reduce>';
      s := s + '<Goto>'      + Write5(entry.PGoto)       + '</Goto>';
      s := s + '<Accept>'    + WriteE(entry.Accept)      + '</Accept>';
      s := s + '<Expanded>'  + EscapeHTML(FExpanded.AsTextFormatted(entry.IndexExp)) + '</Expanded>';
      s := s + '</Entry>';
      WriteLnStringToStream(_strm,s);
      Inc(i);
    end;
  WriteLnStringToStream(_strm,'  </CanonicalList>');
end;

procedure TCanonicalTable.ExpandStates;
var iter: TCanonicalTableEntry;
    entry: TCanonicalTableEntry;
    canon: TExpandedEntry;
    expanding: boolean;
begin
  expanding := True;
  while expanding do
    begin
      expanding := False;
      for iter in Self do
        if not iter.Expanded then
          // Expand this rule into lower level items
          begin
            for canon in FExpanded do
              if (canon.Offset = 0) and
 //                (canon.Next <> NO_TOKEN_IDENTIFIER) and
                 (canon.Head = iter.Next) and
  //               (canon.Head <> canon.Next) and
                 (not StateCanonExists(iter.State,FExpanded.IndexOf(canon))) then
                begin
                  expanding := True;
                  entry := TCanonicalTableEntry.Create;
                  entry.State     := iter.State;
                  entry.IndexExp := FExpanded.IndexOf(canon);
                  entry.Next      := canon.Next;
                  entry.IsEpsilon := (canon.RLen = 0);
                  entry.Shift     := NO_STATE_IDENTIFIER;
                  entry.Reduce    := NO_STATE_IDENTIFIER;
                  entry.PGoto     := NO_STATE_IDENTIFIER;
                  entry.Accept    := False;
                  entry.Expanded  := False;
                  {
                  if (FTerminals[entry.Next].FTerminalStyle = tsNonTerminal) and
                     ((FExpanded[entry.IndexExp].Offset = 0) and
                      (FExpanded[iter.IndexExp].Offset > 0)) or
                     ((FExpanded[entry.IndexExp].offset = 0) and
                      (FExpanded[entry.IndexExp].Offset = 0) and
                      (entry.Next = iter.Next)) then
                    begin
                      entry.PGoto     := iter.PGoto;
                    end;
                  }
                  Add(entry);
                end;
            iter.Expanded := True;
          end;
    end;
end;

function TCanonicalTable.HighestState: TStateIdentifier;
var entry: TCanonicalTableEntry;
begin
  if Count = 0 then
    Exit(NO_STATE_IDENTIFIER);
  Result := 0;
  for entry in Self do
    if entry.State > Result then
      Result := entry.State;
end;

procedure TCanonicalTable.ProcessState(_state: TStateIdentifier);
begin
  ExpandStates;
  AssignActions(_state); // Do allocations for initial state
end;

function TCanonicalTable.StateCanonExists(_state: TStateIdentifier; _canon: integer): boolean;
var iter: TCanonicalTableEntry;
begin
  for iter in Self do
    if (iter.State = _state) and (iter.IndexExp = _canon) then
      Exit(True);
  Exit(False);
end;

function TCanonicalTable.StateExists(_ruleno: integer; _offset: integer): TStateIdentifier;
var entry: TCanonicalTableEntry;
    ci:    integer;
begin
  Result := NO_STATE_IDENTIFIER;
  ci := FExpanded.IndexOf(_ruleno,_offset);
  if ci < 0 then
    raise Exception.Create(Format('Error expanded rule table entry for rule %d and offset %d not found',[_ruleno,_offset]));
  for entry in Self do
    if entry.IndexExp = ci then
      Exit(entry.State);
end;

end.

