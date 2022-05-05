unit uparser_lalrsets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uparser_types, uparser_terminal, fgl;

type
  TParserSetArray = array of TTokenIdentifier;

  TParserSet = class(TObject)
    protected
      FItem:  TTokenIdentifier;
      FPSet:  TParserSetArray;
    public
      function  Add(_setitem: TTokenIdentifier): boolean;
      function  AsTerminalText(_terminals: TTerminalList): string;
      function  AsText(_terminals: TTerminalList): string;
      function  IndexOf(_setitem: TTokenIdentifier): integer;
      property Item: TTokenIdentifier read FItem write FItem;
      property PSet: TParserSetArray  read FPSet write FPSet;
  end;

  TParserSetList = class(specialize TFPGObjectList<TParserSet>)
    public
      procedure Add(_item: TTokenIdentifier; _setitem: TTokenIdentifier);
      function  AddSet2(_arr1, _arr2: TParserSetArray): integer;
      procedure Append(_item: TTokenIdentifier);
      procedure Dump(_strm: TStream; const title: string; _terminals: TTerminalList);
      procedure DumpXML(_strm: TStream; const title: string; _terminals: TTerminalList);
      procedure ExpandFirst;
      procedure ExpandFollow(_firstsetlist, _lastsetlist: TParserSetList);
      procedure ExpandLast;
      function  IndexOf(_item: TTokenIdentifier): integer;
      function  TotalItems: integer;
  end;

implementation

uses
  uparser_utility, htmlelements, deployment_parser_module_12;

{ TParserSet code }

function TParserSet.Add(_setitem: TTokenIdentifier): boolean;
var len: integer;
begin
  Result := False;
  if IndexOf(_setitem) < 0 then
    begin  // Not already there
      len := Length(FPSet);
      SetLength(FPSet,len+1);
      FPSet[len] := _setitem;
      Result := True;
    end;
end;

function TParserSet.AsText(_terminals: TTerminalList): string;
var i: integer;
begin
  Result := MakePrintable(_terminals.TerminalName(FItem)) + ' -> ';
  for i := 0 to Length(FPSet)-1 do
    begin
      if i > 0 then
        Result := Result + ' | ';
      Result := Result + MakePrintable(_terminals.TerminalName(FPSet[i]));
    end;
end;

function TParserSet.AsTerminalText(_terminals: TTerminalList): string;
var i: integer;
    added: boolean;
begin
  added := False;
  Result := MakePrintable(_terminals.TerminalName(FItem)) + ' -> ';
  for i := 0 to Length(FPSet)-1 do
    if not (_terminals.TerminalStyle(FPSet[i]) in [tsNonTerminal]) then
      begin
        if added then
          Result := Result + ' | ';
        Result := Result + MakePrintable(_terminals.TerminalName(FPSet[i]));
        added := True;
      end;
end;

function TParserSet.IndexOf(_setitem: TTokenIdentifier): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(FPSet)-1 do
    if FPSet[i] = _setitem then
      Exit(i);
end;

{ TParserSetList code }

procedure TParserSetList.Add(_item: TTokenIdentifier; _setitem: TTokenIdentifier);
var obj: TParserSet;
    idx: integer;
begin
  // Check if the parent exists first
  idx := IndexOf(_item);
  if idx < 0 then
    begin
      obj := TParserSet.Create;
      obj.FItem := _item;
      idx := inherited Add(obj);
    end;
  if _setitem <> NO_TOKEN_IDENTIFIER then
    Items[idx].Add(_setitem);
end;

function TParserSetList.AddSet2(_arr1, _arr2: TParserSetArray): integer;
var i, j: integer;
    obj:  TParserSet;
    idx: integer;
begin
  Result := 0;
  for i := 0 to Length(_arr1)-1 do
    begin
      idx := IndexOf(_arr1[i]);
      if idx < 0 then
        begin
          obj := TParserSet.Create;
          obj.FItem := _arr1[i];
          idx := inherited Add(obj);
        end;
      for j := 0 to Length(_arr2)-1 do
        if Items[idx].Add(_arr2[j]) then
          Inc(Result);
    end;
end;

procedure TParserSetList.Append(_item: TTokenIdentifier);
var i,j: integer;
    src: integer;
begin
  // Look for sets that contain _item and append the contents of the follow set
  // for _item to that set. Used to expand follow lists where null rules are
  // in use.
  //
  // e.g. if {A} is the follow set of A
  // and  {B} is the follow set of B
  // If B -> ;  (null rule) then if {A} contains B, {A} <- {A}+{B}
  src := IndexOf(_item);
  if src < 0 then
    Exit;
  for i := 0 to Count-1 do
    if (i <> src) and (Items[i].IndexOf(_item) >= 0) then
      // Items[i] contains _item, we can add the set
      for j := 0 to Length(Items[src].PSet)-1 do
        Add(Items[i].Item,Items[src].PSet[j]);
end;

procedure TParserSetList.Dump(_strm: TStream; const title: string; _terminals: TTerminalList);
var i: integer;
begin
 WriteLnStringToStreamUnderlined(_strm,title);
 for i := 0 to Count-1 do
   WriteLnStringToStream(_strm,Items[i].AsText(_terminals));
 WriteLnStringToStream(_strm,'');
end;

procedure TParserSetList.DumpXML(_strm: TStream; const title: string; _terminals: TTerminalList);
var i: integer;
begin
 WriteLnStringToStream(_strm,'  <' + title + '>');
 for i := 0 to Count-1 do
   WriteLnStringToStream(_strm,'    <Entry><Index>' + IntToStr(i) + '</Index><Set>' + EscapeHTML(Items[i].AsText(_terminals)) + '</Set></Entry>');
 WriteLnStringToStream(_strm,'  </' + title + '>');
end;

procedure TParserSetList.ExpandFirst;
var i: integer;
    j: integer;
    k: integer;
    m: integer;
    added: boolean;
begin
  repeat
    added := False;
    for i := 0 to Count-1 do
      for j := 0 to Length(Items[i].PSet)-1 do
        begin
          k := IndexOf(Items[i].PSet[j]);
          if k >=0 then // Possible expansion
            for m := 0 to Length(Items[k].PSet) - 1 do
              if Items[i].IndexOf(Items[k].PSet[m]) < 0 then
                begin
                  Items[i].Add(Items[k].PSet[m]);
                  added := True;
                end;
        end;
  until not added;
end;

procedure TParserSetList.ExpandFollow(_firstsetlist, _lastsetlist: TParserSetList);
var i: integer;
    j: integer;
    k: integer;
    m: integer;
    added: boolean;
begin
  repeat
    added := False;
    // Expand first sets
    for i := 0 to Count-1 do
      for j := 0 to Length(Items[i].PSet)-1 do
        begin
          k := _firstsetlist.IndexOf(Items[i].PSet[j]);
          if k >=0 then // Possible expansion
            for m := 0 to Length(_firstsetlist.Items[k].PSet) - 1 do
              if Items[i].IndexOf(_firstsetlist.Items[k].PSet[m]) < 0 then
                begin
                  Items[i].Add(_firstsetlist.Items[k].PSet[m]);
                  added := True;
                end;
        end;
    // Expand last sets
    for i := 0 to Count-1 do
      begin
        k := _lastsetlist.IndexOf(Items[i].Item);
        if k >=0 then // Possible expansion
          begin
            if AddSet2(_lastsetlist.Items[k].PSet,Items[i].PSet) > 0 then
              added := True;
          end;
      end;
  until not added;
end;

procedure TParserSetList.ExpandLast;
var i: integer;
    j: integer;
    k: integer;
    added: boolean;
    obj: TParserSet;
begin
  repeat
    added := False;
    for i := 0 to Count-1 do
      begin
        k := Length(Items[i].PSet)-1;
        if (k >= 0) and (IndexOf(Items[i].PSet[k]) < 0) then
          begin
            obj := TParserSet.Create;
            obj.Item := Items[i].PSet[k];
            SetLength(obj.FPSet,Length(Items[i].PSet));
            for j := 0 to Length(Items[i].PSet)-1 do
              obj.PSet[j] := Items[i].PSet[j];
            inherited Add(obj);
            added := True;
          end;
      end;
    {
    for i := 0 to Count-1 do
      for j := 0 to Length(Items[i].PSet)-1 do
        begin
          k := IndexOf(Items[i].PSet[j]);
          if k >=0 then // Possible expansion
            for m := 0 to Length(Items[k].PSet) - 1 do
              if Items[i].IndexOf(Items[k].PSet[m]) < 0 then
                begin
                  Items[i].Add(Items[k].PSet[m]);
                  added := True;
                end;
        end;
    }
  until not added;
end;

function TParserSetList.IndexOf(_item: TTokenIdentifier): integer;
var i: integer;
begin
  for i := 0 to Count-1 do
    if Items[i].FItem = _item then
      Exit(i);
  Exit(-1);
end;

function TParserSetList.TotalItems: integer;
var st: TParserSet;
begin
  Result := 0;
  for st in Self do
    Result := Result + Length(st.FPSet);
end;


end.

