unit uparser_dfalist;

{$mode objfpc}{$H+}

//------------------------------------------------------------------------------
//
//  Manage the DFA list
//
//  Constructs the DFA from an NFA list
//
//------------------------------------------------------------------------------

interface

uses
  Classes, SysUtils, fgl, uparser_nfaset, uparser_types,
  uparser_nfalist;

type
  TStateID = record
    NFASet:   TNFASet;
    ID:       TStateIdentifier;
  end;

  TDFAListRow = class(TObject)
    public
      CheckSum:     UINT32;
      CheckSumDone: boolean;
      StateID:      TStateID;
      IsAccepting:  boolean;
      AcceptToken:  TTokenIdentifier;
      IsExpanded:   boolean;
      NextState:    array of TStateID;
      StateCount:   integer;
      constructor Create(_size: integer);
      destructor Destroy; override;
      procedure CalcChecksum;
  end;

  TDFAList = class(specialize TFPGObjectList<TDFAListRow>)
    public
      constructor Create;
      procedure CreateFromNFAList(_nfalist: TNFAList);
      procedure Dump(_strm: TStream; _dictionary: TNFADictionary; const title: string; use_intid: boolean = False);
      procedure DumpXML(_strm: TStream; _dictionary: TNFADictionary);
      procedure EnumerateStateIDs(_dictionary: TNFADictionary);
      function  ExpandEntries(_nfalist: TNFAList): boolean;
      procedure Minimise(_dictionary: TNFADictionary);
      procedure RenameSets(_dictionary: TNFADictionary; _from: TNFASet; _to: TNFASet);
      procedure RenameStateID(_dictionary: TNFADictionary; _set: TNFASet; _id: TStateIdentifier);
      function  StateIDindex(_stateid: TNFASet): integer;
  end;

implementation

uses
  uparser_utility, strutils, uparameters, uparser_exception,
  deployment_parser_types, deployment_parser_module;

{ TDFAListRow }

constructor TDFAListRow.Create(_size: integer);
var i: integer;
begin
  inherited Create;
  StateCount := _size;
  SetLength(NextState,StateCount);
  StateID.NFASet := nil;
  StateID.ID     := NO_STATE_IDENTIFIER;
  for i := 0 to StateCount-1 do
    begin
      NextState[i].NFASet := nil;
      NextState[i].ID     := NO_STATE_IDENTIFIER;
    end;
  IsExpanded := False;
  IsAccepting := False;
  AcceptToken := NO_TOKEN_IDENTIFIER;
end;

destructor TDFAListRow.Destroy;
var i: integer;
begin
  for i := 0 to StateCount-1 do
    if Assigned(NextState[i].NFASet) then
      NextState[i].NFASet.Free;
  if Assigned(StateID.NFASet) then
    StateID.NFASet.Free;
  inherited Destroy;
end;


procedure TDFAListRow.CalcChecksum;
var csum:   UINT32;
    i:      integer;

  procedure Roll(val: UINT32);
  var roller: UINT64;
  begin
    roller := (UINT64(csum) * 65521 + val);
    roller := roller xor (roller shr 32);
    csum := UINT32(roller);
  end;

begin
  csum := 0;
  Roll(AcceptToken);
  for i := 0 to Length(NextState)-1 do
    if Assigned(NextState[i].NFASet) then
      Roll(NextState[i].NFASet.Checksum);
  CheckSum := csum;
  CheckSumDone := True;
end;

{ TDFAList }

constructor TDFAList.Create;
begin
  inherited Create;
end;

procedure TDFAList.CreateFromNFAList(_nfalist: TNFAList);
var c:           integer;
    first_empty: boolean;
    nset:        TNFASet;
    entry:       TDFAListRow;
begin
  if _nfalist.Count = 0 then
    raise Exception.Create('NFA list is empty');
  // Special case check first state to see if letter actions or just all epsilons
  // If it's all epsilons we skip the first row and use the epsilon values from
  // the first record of the NFA list as the initial StateID
  first_empty := true;
  for c := 0 to _nfalist.Dictionary.Count-1 do
    if _nfalist.Items[0].NextState[c] <> NO_STATE_IDENTIFIER then
      first_empty := false;

  nset := TNFASet.Create(True);
  if first_empty then
    nset.SetAdd(_nfalist.Items[0].Epsilons)
  else
    nset.Add(0);
  // Add the first record
  entry := TDFAListRow.Create(_nfalist.Dictionary.Count);
  Add(entry);
  entry.StateID.NFASet := nset;
  while ExpandEntries(_nfalist) do ;
end;

procedure TDFAList.Dump(_strm: TStream; _dictionary: TNFADictionary; const title: string; use_intid: boolean = False);
var i:      integer;
    ci:     integer;
    s:      string;
    ent:    TDFAListRow;
    csizes: array of integer;
    csize:  integer;
    cmax:   integer;
    tsize:  integer;
    arr:    TStringArray;

begin
  arr := _dictionary.Titles;
  // First calculate the sizes for each column
  SetLength(csizes,_dictionary.Count);
  cmax := 0;
  for ci := 0 to _dictionary.Count-1 do
    begin
      csizes[ci] := 0;
      for i := 0 to Count-1 do
        if Items[i].NextState[ci].NFASet <> Nil then
          begin
            if use_intid then
              csize := Length(IntToStr(Items[i].NextState[ci].ID))
            else
              csize := Length(Items[i].NextState[ci].NFASet.AsString);
            if csize > cmax then
              cmax := csize;
          end;
      if (cmax = 0) or ((cmax > 0) and (cmax < Length(arr[ci]))) then
        cmax := Length(arr[ci]);
      csizes[ci] := cmax;
    end;
  // Calculate the length of the StateID
  if use_intid then
    tsize := 7
  else
    begin
      tsize := 0;
      for i := 0 to Count-1 do
        begin
          csize := Length(Items[i].StateID.NFASet.AsString);
          if csize > tsize then
            tsize := csize;
        end;
      if tsize < 7 then
        tsize := 7;
    end;
  // Now do the dump
  WriteLnStringToStream(_strm,title);
  WriteLnStringToStream(_strm,StringOfChar('=',Length(title)));
  WriteLnStringToStream(_strm,'');
  // Gather titles
  s := PadRight('StateID',tsize);
  s := s + ' ACCEPT';
  for ci := 0 to Length(arr)-1 do
    if csizes[ci] > 0 then
      s := s + ' ' + PadRight(arr[ci],csizes[ci]);
  WriteLnStringToStream(_strm,s);
  // Gather underlines
  s := StringOfChar('-',tsize);
  s := s + ' ------';
  for ci := 0 to Length(arr)-1 do
    if csizes[ci] > 0 then
      s := s + ' ' + StringOfChar('-',csizes[ci]);
  WriteLnStringToStream(_strm,s);
  // Now the main list
  for i := 0 to Count-1 do
    begin
      ent := Items[i];
      if use_intid then
        s := PadRight(IntToStr(Items[i].StateID.ID),tsize)
      else
        s := PadRight(Items[i].StateID.NFASet.AsString,tsize);
      if ent.IsAccepting then
        s := s + Format(' %6d',[ent.AcceptToken])
      else
        s := s + StringOfChar(' ',7);
      for ci := 0 to Length(arr)-1 do
        if csizes[ci] > 0 then
          if use_intid then
            begin
              if Items[i].NextState[ci].ID <> NO_STATE_IDENTIFIER then
                s := s + ' ' + PadRight(IntToStr(Items[i].NextState[ci].ID),csizes[ci])
              else
                s := s + ' ' + StringOfChar(' ',csizes[ci]);
            end
          else
            begin
              if Assigned(Items[i].NextState[ci].NFASet) then
                s := s + ' ' + PadRight(Items[i].NextState[ci].NFASet.AsString,csizes[ci])
              else
                s := s + ' ' + StringOfChar(' ',csizes[ci]);
            end;
      WriteLnStringToStream(_strm,s);
    end;
  // Finally a blank line
  WriteLnStringToStream(_strm,'');
end;

procedure TDFAList.DumpXML(_strm: TStream; _dictionary: TNFADictionary);
var i:      integer;
    ci:     integer;
    s:      string;
    ent:    TDFAListRow;
    arr:    TStringArray;

begin
  arr := _dictionary.TitlesAsXML;
  WriteLnStringToStream(_strm,'  <DFA>');
  // Now the main list
  for i := 0 to Count-1 do
    begin
      ent := Items[i];
      s := '    <Entry><State>' + IntToStr(Items[i].StateID.ID)+'</State>';
      s := s + '<Accept>';
      if ent.IsAccepting then
        s := s + IntToStr(ent.AcceptToken);
      s := s + '</Accept>';
      for ci := 0 to Length(arr)-1 do
        begin
          s := s + '<' + StringAsXMLHeading(arr[ci]) + '>';
          if Items[i].NextState[ci].ID <> NO_STATE_IDENTIFIER then
            s := s + IntToStr(Items[i].NextState[ci].ID);
          s := s + '</' + StringAsXMLHeading(arr[ci]) + '>';
        end;
      s := s + '</Entry>';
      WriteLnStringToStream(_strm,s);
    end;
  WriteLnStringToStream(_strm,'  </DFA>');
end;

procedure TDFAList.EnumerateStateIDs(_dictionary: TNFADictionary);
var id: TStateIdentifier;
begin
  for id := 0 to Count-1 do
    begin
      Items[id].StateID.ID := id;
      RenameStateID(_dictionary,Items[id].StateID.NFASet,id);
    end;
end;

function TDFAList.ExpandEntries(_nfalist: TNFAList): boolean;
var dest:  TNFASet;
    i:     integer;
    j:     integer;
    k:     integer;
    ci:    integer;
    cx:    integer;
    entry: TDFAListRow;
    nset:  TNFASet;
begin
  Result := False;
  dest := TNFASet.Create;
  try
    for i := 0 to Count-1 do
      if not Items[i].IsExpanded then
        begin
          Result := True;
          if _nfalist.AcceptToken(Items[i].StateID.NFASet) <> NO_TOKEN_IDENTIFIER then
            begin
              Items[i].AcceptToken := _nfalist.AcceptToken(Items[i].StateID.NFASet);
              Items[i].IsAccepting := True;
            end;
          // Expand all the entries
          for ci := 0 to _nfalist.Dictionary.Count-1 do
            begin
              dest.Clear;
              // Do the straight characters and sets first
              for j := 0 to Items[i].StateID.NFASet.Count-1 do
                begin
                  k := Items[i].StateID.NFASet.Items[j];
                  if _nfalist.Items[k].NextState[ci] <> NO_STATE_IDENTIFIER then
                    dest.SetAdd(_nfalist.Items[_nfalist.Items[k].NextState[ci]].Epsilons);
                end;
              // Now look for set membership
              if _nfalist.Dictionary.Items[ci].DictType = ndtCharacter then
                for cx := 0 to _nfalist.Dictionary.Count-1 do
                  if (_nfalist.Dictionary.Items[cx].DictType = ndtCharRange) and
                     (_nfalist.Dictionary.Items[ci].Character >= _nfaList.Dictionary.Items[cx].Character) and
                     (_nfalist.Dictionary.Items[ci].Character <= _nfaList.Dictionary.Items[cx].CharacterTo) then
                      for j := 0 to Items[i].StateID.NFASet.Count-1 do
                        begin
                          k := Items[i].StateID.NFASet.Items[j];
                          if _nfalist.Items[k].NextState[cx] <> NO_STATE_IDENTIFIER then
                            dest.SetAdd(_nfalist.Items[_nfalist.Items[k].NextState[cx]].Epsilons);
                        end;
              if dest.Count > 0 then
                begin
                  Items[i].NextState[ci].NFASet := TNFASet.Create(True);
                  Items[i].NextState[ci].NFASet.SetAdd(dest);
                  if StateIDindex(dest) < 0 then
                    begin
                      entry := TDFAListRow.Create(_nfalist.Dictionary.Count);
                      Add(entry);
                      nset := TNFASet.Create(True);
                      nset.SetAdd(dest);
                      entry.StateID.NFASet := nset;
                    end;
                end;
            end;
          // Mark as expanded
          Items[i].IsExpanded := True;
        end;
  finally
    dest.Free;
  end;
end;

procedure TDFAList.Minimise(_dictionary: TNFADictionary);
// Go through and combine rows which have identical results
// Do them in pairs
// Cannon minimise empty sets as they are normally accept conditions only
var minimising: boolean;
    i,j:        integer;
    rowmatch:   boolean;
    rm_allowed: boolean;
    c:          integer;
    tmpset:     TNFASet;
begin
  minimising := True;
  while minimising do
    begin
      minimising := False; // Assume false for now
      for i := 0 to Count-2 do
        begin
          if minimising then
            break;
          if not Items[i].CheckSumDone then
            Items[i].CalcChecksum;
          for j := i+1 to Count-1 do
            begin
              if minimising then
                break;
              if not Items[j].CheckSumDone then
                Items[j].CalcChecksum;
              if Items[i].CheckSum = Items[j].CheckSum then
                begin
                  rowmatch := True; // Assume there's a match until we hit a mismatch
                  rm_allowed := False;
                  for c := 0 to _dictionary.Count-1 do
                    if (Assigned(Items[i].NextState[c].NFASet) and (not Assigned(Items[j].NextState[c].NFASet))) or
                       ((not Assigned(Items[i].NextState[c].NFASet)) and Assigned(Items[j].NextState[c].NFASet)) then
                      rowmatch := false
                    else if Items[i].AcceptToken <> Items[j].AcceptToken then
                      rowmatch := false
                    else if Assigned(Items[i].NextState[c].NFASet) and (not Items[i].NextState[c].NFASet.Matches(Items[j].NextState[c].NFASet)) then
                      rowmatch := false;
                    {
                    if Assigned(Items[i].NextState[c].NFASet) or Assigned(Items[j].NextState[c].NFASet) then
                      rm_allowed := True;
                    }
                    rm_allowed := True;
                    {
                    if not rowmatch then
                      break;
                    }
                    if rowmatch and rm_allowed then
                      begin // We have a match
                        minimising := True;
                        tmpset := TNFASet.Create(True);
                        try
                          // Combine the two sets
                          tmpset.SetAdd(Items[i].StateID.NFASet);
                          tmpset.SetAdd(Items[j].StateID.NFASet);
                          RenameSets(_dictionary,Items[i].StateID.NFASet,tmpset);
                          RenameSets(_dictionary,Items[j].StateID.NFASet,tmpset);
                          // Delete the record we don't need
                          Delete(j);
                          Items[i].StateID.NFASet.SetAdd(tmpset);
                          Items[i].CheckSumDone := False;
                        finally
                          tmpset.Free;
                        end;
                      end;
                  end;
            end;
        end;
    end;
end;

procedure TDFAList.RenameSets(_dictionary: TNFADictionary; _from: TNFASet; _to: TNFASet);
var i: integer;
    c: integer;
begin
  for i := 0 to Count-1 do
    for c := 0 to _dictionary.Count-1 do
      if Assigned(Items[i].NextState[c].NFASet) then
        if Items[i].NextState[c].NFASet.Matches(_from) then
          begin
            Items[i].NextState[c].NFASet.SetAdd(_to);
            Items[i].CheckSumDone := False;
          end;
end;

procedure TDFAList.RenameStateID(_dictionary: TNFADictionary; _set: TNFASet; _id: TStateIdentifier);
var i: integer;
    ci: integer;
begin
  for i := 0 to Count-1 do
    for ci := 0 to _dictionary.Count-1 do
      if Assigned(Items[i].NextState[ci].NFASet) then
        if Items[i].NextState[ci].NFASet.Matches(_set) then
          Items[i].NextState[ci].ID := _id;
end;

function TDFAList.StateIDindex(_stateid: TNFASet): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].StateID.NFASet.Matches(_stateid) then
      Exit(i);
end;

end.

