unit uparser_terminal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uparser_types, deployment_parser_types_12,
  deployment_parser_module_12;

type
  TTerminalStyle = (tsSpecial,tsTerminal,tsTerminalIgnored,tsKeyword,tsSymbol,tsVirtual,tsNonTerminal, tsAccept);

  TTerminalStyleSet = set of TTerminalStyle;

  TTerminalDefObject = class(TObject)
    public
      FNodeNumber:       integer;
      FTerminalName:     TString;
      FTerminalFriendly: TString;
      FTokenName:        TString;
      FFirst:            TStateIdentifier;
      FLast:             TStateIdentifier;
      FTerminalStyle:    TTerminalStyle;
      FIgnore:           boolean;
      FUsed:             boolean;
      procedure Dump(_strm: TStream);
      procedure DumpXML(_strm: TStream; _index: integer);
  end;

  TTerminalList = class(specialize TFPGObjectList<TTerminalDefObject>)
    protected
      FParser: TLCGParser;
    public
      constructor Create(_parser: TLCGParser);
      function  Add(obj: TTerminalDefObject): integer;
      procedure Clear;
      procedure Dump(_strm: TStream; const _title: string = 'Terminal Definitions'; _allowed: TTerminalStyleSet = [tsTerminal..tsVirtual]);
      procedure DumpXML(_strm: TStream);
      function  IndexOf(const _name: TString): integer;
      function  TerminalName(_index: integer): TString;
      function  TerminalStyle(_index: integer): TTerminalStyle;
      function  TokenName(_index: integer): TString;
  end;

implementation

uses
  uparser_utility, htmlelements;

{ TTerminalDefObject code }

procedure TTerminalDefObject.Dump(_strm: TStream);
var utext: string;
    FFirstStr: string;
    FLastStr:  string;

  function StateToStr(_st: TStateIdentifier): string;
  begin
    if _st = NO_STATE_IDENTIFIER then
      Result := '-'
    else
      Result := IntToStr(_st);
  end;

begin
  utext := '';
  if (not FUsed) then
    utext := '*UNUSED* ';
  FFirstStr := StateToStr(FFirst);
  FLastStr  := StateToStr(FLast);
  if FTerminalName <> FTerminalFriendly then
    WriteLnStringToStream(_strm,Format('%5d: [%s,%s] %s',[FNodeNumber,FFirstStr,FLastStr,utext]) + MakePrintable(FTerminalName) + ' = ' + MakePrintable(FTerminalFriendly))
  else
    WriteLnStringToStream(_strm,Format('%5d: [%s,%s] %s',[FNodeNumber,FFirstStr,FLastStr,utext]) + MakePrintable(FTerminalName));
end;

procedure TTerminalDefObject.DumpXML(_strm: TStream; _index: integer);
var termtype: string;
    content:  string;
begin
  case FTerminalStyle of
    tsSpecial:         termtype := 'Special';
    tsTerminal:        termtype := 'Terminal';
    tsTerminalIgnored: termtype := 'Terminal ignored';
    tsKeyword:         termtype := 'Keyword';
    tsSymbol:          termtype := 'Symbol';
    tsVirtual:         termtype := 'Virtual';
    tsNonTerminal:     termtype := 'Non terminal';
    tsAccept:          termtype := 'Accept';
    otherwise
      raise Exception.Create('Terminal style not catered for');
  end;
  content := '';
  if FTerminalName <> FTerminalFriendly then
    content := MakePrintable(FTerminalFriendly);
  WriteLnStringToStream(_strm,'    <Terminal><Index>' + IntToStr(_index) + '</Index><Name>' + EscapeHTML(MakePrintable(FTerminalName)) + '</Name><Type>' + EscapeHTML(termtype) + '</Type><Content>' + EscapeHTML(content) + '</Content></Terminal>');
end;

{ TTerminalList code }

constructor TTerminalList.Create(_parser: TLCGParser);
begin
  inherited Create;
  FParser := _parser;
  Clear;
end;

function TTerminalList.Add(obj: TTerminalDefObject): integer;
begin
  if IndexOf(obj.FTerminalName) >= 0 then
    FParser.Monitor(ltWarning,'Terminal ' + obj.FTerminalName + ' has been defined earlier');
  Result := inherited Add(obj);
end;

procedure TTerminalList.Clear;
var obj: TTerminalDefObject;
begin
  inherited Clear;
  // Create the (Error) token
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := '(Error)';
  obj.FTerminalFriendly := obj.FTerminalName;
  obj.FTerminalStyle    := tsSpecial;
  obj.FFirst            := NO_STATE_IDENTIFIER;
  obj.FLast             := NO_STATE_IDENTIFIER;
  obj.FUsed             := True;
  obj.FNodeNumber := Add(obj);
  // Create the (EOF) token
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := '(EOF)';
  obj.FTerminalFriendly := obj.FTerminalName;
  obj.FTerminalStyle    := tsSpecial;
  obj.FFirst            := NO_STATE_IDENTIFIER;
  obj.FLast             := NO_STATE_IDENTIFIER;
  obj.FUsed             := True;
  obj.FNodeNumber := Add(obj);
  // Create the (Comment) token
  obj := TTerminalDefObject.Create;
  obj.FTerminalName     := '(Comment)';
  obj.FTerminalFriendly := obj.FTerminalName;
  obj.FTerminalStyle    := tsSpecial;
  obj.FFirst            := NO_STATE_IDENTIFIER;
  obj.FLast             := NO_STATE_IDENTIFIER;
  obj.FUsed             := True;
  obj.FIgnore           := True;
  obj.FNodeNumber := Add(obj);
end;

procedure TTerminalList.Dump(_strm: TStream; const _title: string; _allowed: TTerminalStyleSet);
var i: integer;
begin
  WriteLnStringToStreamUnderlined(_strm,_title);
  for i := 0 to Count-1 do
    if Items[i].FTerminalStyle in _allowed then
      Items[i].Dump(_strm);
  WriteLnStringToStream(_strm,'');
end;

procedure TTerminalList.DumpXML(_strm: TStream);
var i: integer;
begin
  WriteLnStringToStream(_strm,'  <Terminals>');
  for i := 0 to Count-1 do
    Items[i].DumpXML(_strm,i);
  WriteLnStringToStream(_strm,'  </Terminals>');
end;

function TTerminalList.IndexOf(const _name: TString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].FTerminalName = _name then
      Exit(i);
end;

function TTerminalList.TerminalName(_index: integer): TString;
begin
  if _index < 0 then
    Result := 'Epsilon'
  else
    Result := Items[_index].FTerminalName;
end;

function TTerminalList.TerminalStyle(_index: integer): TTerminalStyle;
begin
  Result := Items[_index].FTerminalStyle;
end;

function TTerminalList.TokenName(_index: integer): TString;
begin
  if _index < 0 then
    Result := 'Epsilon'
  else
    begin
      if Length(Items[_index].FTokenName) = 0 then
        begin // Make up the token name
          Items[_index].FTokenName := MakePrintable(Items[_index].FTerminalName);
        end;
      Result := Items[_index].FTokenName;
    end;
end;

end.

