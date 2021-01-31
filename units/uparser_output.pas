unit uparser_output;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, uparser_types, uparser_terminal, uparser_canonical,
  uparser_expanded, uparser_lalrsets, uparser_rule, deployment_parser_module;

type
  TParserOutputType = (potUndefined,potError,potShift,potGoto,potReduce,potAccept);

  TParserOutputEntry = record
    OutputType:  TParserOutputType;
    Destination: TStateIdentifier;
    FromEpsilon: boolean;
  end;

  TParserOutputRow = class(TObject)
    public
      State:  TStateIdentifier;
      Accept: boolean;
      FCols:  array of TParserOutputEntry;
  end;

  TParserOutput = class(specialize TFPGObjectList<TParserOutputRow>)
    protected
      FTerminals: TTerminalList;
    public
      procedure CreateOutput(_parent: TLCGParser; _parser: TCanonicalTable; _terminals: TTerminalList; _expanded: TExpandedList; _follows: TParserSetList);
      procedure Dump(_strm: TStream);
      procedure DumpXML(_strm: TStream);
  end;

implementation

uses
  uparser_utility, strutils, uparameters, htmlelements,
  uparser_exception, deployment_parser_types;

{ TParserOutput }

procedure TParserOutput.CreateOutput(_parent: TLCGParser; _parser: TCanonicalTable; _terminals: TTerminalList; _expanded: TExpandedList; _follows: TParserSetList);
var row:    TParserOutputRow;
    i:      TStateIdentifier;
    j:      integer;
    _state: TStateIdentifier;
    ci:     integer;
    follow: TParserSet;
    prev:   TStateIdentifier;
    next:   TStateIdentifier;
    col:    TStateIdentifier;
begin
  Clear;
  FTerminals := _terminals;
  // Set up all the rows and columns
  for i := 0 to _parser.HighestState do
    begin
      row := TParserOutputRow.Create;
      row.State := i;
      row.Accept := False;
      SetLength(row.FCols,_terminals.Count);
      for j := 0 to _terminals.Count-1 do
        begin
          row.FCols[j].OutputType  := potError;
          row.FCols[j].Destination := NO_STATE_IDENTIFIER;
          row.FCols[j].FromEpsilon := False;
        end;
      Add(row);
    end;
  // Accept state
  for i := 0 to _parser.Count-1 do
    if _parser.Items[i].Accept then
      begin
        _state := _parser.Items[i].State;
        Items[_state].Accept := True;
        for j := 0 to FTerminals.Count-1 do
          begin
            Items[_state].FCols[j].OutputType  := potAccept;
            Items[_state].FCols[j].Destination := NO_STATE_IDENTIFIER;
          end;
      end;
  // Reductions for epsilons first
  for i := 0 to _parser.Count-1 do
    if (_parser.Items[i].Reduce <> NO_STATE_IDENTIFIER) and
       (_parser.Items[i].IsEpsilon) then
      begin
        _state := _parser.Items[i].State;
        for j := 0 to FTerminals.Count-1 do
          begin
            Items[_state].FCols[j].OutputType  := potReduce;
            Items[_state].FCols[j].Destination := _parser.Items[i].Reduce;
            Items[_state].FCols[j].FromEpsilon := True;
          end;
      end;
  // Reductions for not epsilons
  for i := 0 to _parser.Count-1 do
    if (_parser.Items[i].Reduce <> NO_STATE_IDENTIFIER) and
       (not _parser.Items[i].IsEpsilon) then
      begin
        // Find out what went before and only fill in the reduction entries
        // for the follow set of the prior token
        ci := _parser.Items[i].IndexExp;
        ci := _expanded.UnBump(ci);
        if ci < 0 then
          raise Exception.Create('Unable to unbump');
        prev := _expanded.Items[ci].Next;
        follow := _follows.Items[_follows.IndexOf(prev)];
        _state := _parser.Items[i].State;
        for j := 0 to Length(follow.PSet)-1 do
          begin
            col := follow.PSet[j];
            if (Items[_state].FCols[col].OutputType = potReduce) and (not Items[_state].FCols[col].FromEpsilon) then
              begin // Ignore as this box has already been filled in
                if _parser.Items[i].Reduce <> Items[_state].FCols[col].Destination then
                  _parent.Monitor(ltWarAndPeace,
                                  'Ignoring reduction on state %d to rule %d as there is an existing rule %d',
                                  [_state,_parser.Items[i].Reduce,Items[_state].FCols[col].Destination]);
              end
            else
              begin
                Items[_state].FCols[col].OutputType := potReduce;
                Items[_state].FCols[col].Destination := _parser.Items[i].Reduce;
              end;
          end;
      end;
  // Non-terminals
  for i := 0 to _parser.Count-1 do
    if (_parser.Items[i].PGoto <> NO_STATE_IDENTIFIER) then
      begin
        ci := _parser.Items[i].IndexExp;
        next := _expanded.Items[ci].Next;
        _state := _parser.Items[i].State;
        Items[_state].FCols[next].OutputType := potGoto;
        Items[_state].FCols[next].Destination := _parser.Items[i].PGoto;
      end;
  // Terminals
  for i := 0 to _parser.Count-1 do
    if (_parser.Items[i].Shift <> NO_STATE_IDENTIFIER) then
      begin
        ci := _parser.Items[i].IndexExp;
        next := _expanded.Items[ci].Next;
        _state := _parser.Items[i].State;
        Items[_state].FCols[next].OutputType := potShift;
        Items[_state].FCols[next].Destination := _parser.Items[i].Shift;
      end;
end;

procedure TParserOutput.Dump(_strm: TStream);
var s:       string;
    heading: string;
    widths:  array of integer;
    i:       integer;
    row:     TParserOutputRow;
begin
  SetLength(widths,FTerminals.Count);
  WriteLnStringToStreamUnderlined(_strm,'Output Table');
  WriteLnStringToStream(_strm,'');
  // Column headings
  s := 'State';
  for i := 0 to FTerminals.Count-1 do
    begin
      heading := MakePrintable(FTerminals.Items[i].FTerminalName);
      if Length(heading) > 6 then
        widths[i] := Length(heading)
      else
        widths[i] := 6;
      s := s + ' ' + PadLeft(MakePrintable(FTerminals.Items[i].FTerminalName),widths[i]);
    end;
  WriteLnStringToStream(_strm,s);
  // Underlining
  s := '-----';
  for i := 0 to FTerminals.Count-1 do
    s := s + ' ' + StringOfChar('-',widths[i]);
  WriteLnStringToStream(_strm,s);
  // Main part
  for row in Self do
    begin
      s := Format('%5d',[row.State]);
      for i := 0 to FTerminals.Count-1 do
        case row.FCols[i].OutputType of
          potUndefined: s := s + ' ' + PadLeft('?',widths[i]);
          potError:     s := s + ' ' + PadLeft('.',widths[i]);
          potShift:     s := s + ' ' + PadLeft('S' + IntToStr(row.FCols[i].Destination),widths[i]);
          potGoto:      s := s + ' ' + PadLeft('G' + IntToStr(row.FCols[i].Destination),widths[i]);
          potReduce:    s := s + ' ' + PadLeft('R' + IntToStr(row.FCols[i].Destination),widths[i]);
          potAccept:    s := s + ' ' + PadLeft('ACC',widths[i]);
        end;
      WriteLnStringToStream(_strm,s);
    end;
  // Finally...
  WriteLnStringToStream(_strm,'');
end;

procedure TParserOutput.DumpXML(_strm: TStream);
var s:       string;
    i:       integer;
    row:     TParserOutputRow;
begin
  WriteLnStringToStream(_strm,'  <ParserTable>');
  // Main part
  for row in Self do
    begin
      s := '    <Entry><State>' + IntToStr(row.State) + '</State>';
      for i := 0 to FTerminals.Count-1 do
        if Fterminals.Items[i].FTerminalName <> ACCEPT_CONDITION then
          begin
            s := s + '<' + EscapeHTML(StringAsXMLHeading(FTerminals.Items[i].FTerminalName)) + '>';
            case row.FCols[i].OutputType of
              potUndefined: s := s + '?';
              potError:     s := s + '';
              potShift:     s := s + 'S' + IntToStr(row.FCols[i].Destination);
              potGoto:      s := s + 'G' + IntToStr(row.FCols[i].Destination);
              potReduce:    s := s + 'R' + IntToStr(row.FCols[i].Destination);
              potAccept:    s := s + 'ACC';
            end;
            s := s + '</' + EscapeHTML(StringAsXMLHeading(FTerminals.Items[i].FTerminalName)) + '>';
          end;
      s := s + '</Entry>';
      WriteLnStringToStream(_strm,s);
    end;
  // Finally...
  WriteLnStringToStream(_strm,'  </ParserTable>');
end;

end.

