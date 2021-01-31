unit deployment_parser_module;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

//
//  Deployment module for the parser, also includes the Lexer
//

interface

uses
  Classes, SysUtils, fgl, deployment_parser_types;

type

  TLCGParser = class; // Preliminary declaration

  TLCGMonitorProc = procedure (Parser: TLCGParser; LogType: TLCGLogType; const Message: string) of object;
  TLCGReduceProc = function (Parser: TLCGParser; RuleIndex: UINT32): TLCGParserStackEntry of object;
  TLCGParserProc = function (Parser: TLCGParser): TLCGParserStackEntry of object;
  TLCGTokenProc = procedure (Parser: TLCGParser; TokenIndex: UINT32) of object;

  TLCGParameter = record
    Name:  string;
    Value: string;
    class operator = (a,b: TLCGParameter): boolean;
  end;

  TLCGParameterList = class(specialize TFPGList<TLCGParameter>)
    public
      procedure LoadFromStream(Stream: TStream);
  end;

  TLCGDictionaryItem = record
      Character:   TChar;
      CharacterTo: TChar;
      class operator = (a,b: TLCGDictionaryItem): boolean;
    end;

  TLCGDictionary = class(specialize TFPGList<TLCGDictionaryItem>)
    private
      FIndices: array[char] of TLCGStateIdentifier;
    public
      function  CharToDictIndex(c: TChar): TLCGStateIdentifier;
      function  CharToDictIndexSlow(c: TChar): TLCGStateIdentifier;
      procedure LoadFromStream(Stream: TStream);
  end;

  TLCGTokenItem = record
      Ignore: boolean;
      Name:   string;
  end;

  TLCGTokenItems = array of TLCGTokenItem;

  TLCGDFARow = record
      Index:       integer;
      AcceptToken: UINT32;
      Transitions: array of UINT32;
      class operator = (a,b: TLCGDFARow): boolean;
  end;

  TLCGDFA = class(specialize TFPGList<TLCGDFARow>)
    private
      FDictCount: integer;
    public
      procedure LoadFromStream(Stream: TStream);
      property DictCount: integer read FDictCount write FDictCount;
  end;

  TLCGRule = record
      HeadToken: TLCGTokenIdentifier;
      RuleCount: UINT32;
      RuleID:    string;
      RuleText:  string;
      ProcName:  string;
      class operator = (a,b: TLCGRule): boolean;
  end;

  TLCGRules = class(specialize TFPGList<TLCGRule>)
    public
      procedure LoadFromStream(Stream: TStream);
  end;

  TLCGLALREntry = record
    OutputType:  TLCGParserOutputType;
    Destination: UINT32;
  end;

  TLCGLALRRow = array of TLCGLALREntry;

  TLCGLALR = class(specialize TFPGList<TLCGLALRRow>)
    private
      FTokenCount: integer;
    public
      procedure LoadFromStream(Stream: TStream);
      property TokenCount: integer read FTokenCount write FTokenCount;
  end;

  TLCGParser = class(TObject)
    private
      FBufferIncrement: integer;
      FBufferOverflow:  boolean;
      FDFA:             TLCGDFA;
      FDictionary:      TLCGDictionary;
      FFetched:         boolean;
      FLALR:            TLCGLALR;
      FLexBufBlock:     integer;
      FLexBufSize:      integer;
      FLexerBuffer:     array of char;
      FLexerBufIndex:   integer;
      FLexerBufRemain:  integer;
      FLexerMode:       TLCGLexerMode;
      FLexerState:      UINT32;
      FLoaded:          boolean;
      FParameterList:   TLCGParameterList;
      FParserStack:     TLCGParserStack;
      FParserSP:        integer;
      FRules:           TLCGRules;
      FStartTime:       TDateTime;
      FStream:          TStream;
      FTabSize:         integer;
      FToken:           UINT32;
      FTokenBuf:        array of char;
      FTokenBufSize:    integer;
      FTokenCount:      integer;
      FTokens:          TLCGTokenItems;
      FWrongCharacter:  TChar;
      procedure CheckStackSize;
      procedure InitStart;
      function  GetRuleProcs: TStringArray;
      function  GetRules: integer;
      function  Lexer:   TLCGTokenIdentifier;
      function  LexGet:  TChar;
      function  LexPeek: TChar;
      procedure LexReadChunk;
      procedure LexShiftBuffer;
      function  ParserPeek: TLCGTokenIdentifier;
      procedure Pop;
      procedure Push(_entry: TLCGParserStackEntry);
      procedure Push(_state: TLCGStateIdentifier; _token: TLCGTokenIdentifier; _buf: array of char);
      procedure Reduce(ruleindex: TLCGStateIdentifier);
      procedure SetLexBufBlock(_size: integer);
      procedure SetTokenBufSize(_size: integer);
      function  Tos: TLCGParserStackEntry;             inline;
      function  TosState: TLCGStateIdentifier;         inline;
    protected
      FInputColumn:     integer;
      FInputColumnSave: integer;
      FInputLine:       integer;
      FInputLineSave:   integer;
      FLogLevel:        TLCGLogType;
      FOnMonitor:       TLCGMonitorProc;
      FOnReduce:        TLCGReduceProc;
      FOnToken:         TLCGTokenProc;
    public
      constructor Create;
      destructor Destroy; override;
      procedure InitRun;
      procedure LoadFromFile(const Filename: string);
      procedure LoadFromResource(const ResourceName: string);
      procedure LoadFromStream(Stream: TStream);
      procedure LoadTokensFromStream(Stream: TStream);
      procedure Monitor(LogType: TLCGLogType; const Message: string); virtual;
      procedure Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const); virtual;
      procedure Parse(Stream: TStream);
      property InputColumn:  integer           read FInputColumnSave;
      property InputLine:    integer           read FInputLineSave;
      property LexBufBlock:  integer           read FLexBufBlock write SetLexBufBlock;
      property LogLevel:     TLCGLogType       read FLogLevel    write FLogLevel;
      property OnMonitor:    TLCGMonitorProc   read FOnMonitor   write FOnMonitor;
      property OnToken:      TLCGTokenProc     read FOnToken     write FOnToken;
      property OnReduce:     TLCGReduceProc    read FOnReduce    write FOnReduce;
      property Parameters:   TLCGParameterList read FParameterList;
      property ParserStack:  TLCGParserStack   read FParserStack;
      property ParserSP:     integer           read FParserSP;
      property RuleProcs:    TStringArray      read GetRuleProcs;
      property Rules:        integer           read GetRules;
      property StartTime:    TDateTime         read FStartTime;
      property TokenBufSize: integer           read FTokenBufSize write SetTokenBufSize;
  end;

// function Char32ToUTF8String(const c: TChar; _translate: boolean = False): UTF8String;
function CharAsText(c32: TChar): TString;
//function Hex2Dec32(h: TString): UINT32;
//function IntToStr32(ival: integer): TString;
//function RightStr32(s32: TString; cnt: integer): TString;
//function String32ToUTF8String(const s: TString): UTF8String;
//function StrToInt32(const s: TString): integer;


implementation

{$IFDEF WINDOWS}
  uses Windows; // For definition of RT_RCDATA
{$ENDIF}

const
  LACOBJ_MAGIC_WORD = $0143414C;

  LOW_CHARS: array[0..32] of String =
    ('NUL','SOH','STX','ETX','EOT','ENQ','ACK','BEL',
     'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI',
     'DLE','DC1','DC2','DC3','DC4','NAK','SYN','ETB',
     'CAN','EM', 'SUB','ESC','FS', 'GS', 'RS', 'US',
     'SP');

  HIGH_CHARS: array[127..159] of string =
    ('DEL','PAD','HOP','BPH','NBH','IND','NEL','SSA',
     'ESA','HTS','HTJ','VTS','PLD','PLU','RI', 'SS2',
     'SS3','DCS','PU1','PU2','STS','CCH','MW', 'SPA',
     'EPA','SOS','SGCI','SCI','CSI','ST','OSC','PM',
     'APC');



{ Utility routines }

class operator TLCGParameter.= (a,b: TLCGParameter): boolean;
begin
  Result := (a.Name = b.Name);
end;

class operator TLCGDictionaryItem.= (a,b: TLCGDictionaryItem): boolean;
begin
  if (a.Character <> b.Character) then
    Result := False
  else
    Result := (a.CharacterTo = b.CharacterTo);
end;

class operator TLCGDFARow.= (a,b: TLCGDFARow): boolean;
begin
  Result := (a.Index = b.Index);
end;

class operator TLCGRule.= (a,b: TLCGRule): boolean;
begin
  Result := False;
  if (a.HeadToken = b.HeadToken) and
     (a.RuleCount = b.RuleCount) and
     (a.RuleID    = b.RuleID) and
     (a.RuleText  = b.RuleText) and
     (a.ProcName  = b.ProcName) then
    Result := True;
end;

function ReadUINT8(Stream: TStream): UINT8;
begin
  Stream.Read(Result{%H-},1);
end;

function ReadUINT16(Stream: TStream): UINT16;
begin
  Stream.Read(Result{%H-},2);
end;

function ReadUINT32(Stream: TStream): UINT32;
begin
  Stream.Read(Result{%H-},4);
end;

function ReadUINTX(Stream: TStream; _mb: UINT8): UINT32;
begin
  Result := 0;
  Stream.Read(Result,_mb);
end;

function ReadUINTZ(Stream: TStream; _mb: UINT8): UINT32;
begin
  result := 0;
  Stream.Read(Result,_mb);
  if ((Result = $000000FF) and (_mb = 1)) or
     ((Result = $0000FFFF) and (_mb = 2)) or
     ((Result = $00FFFFFF) and (_mb = 3)) then
    Result := $7FFFFFFF;
end;

function ReadStr(Stream: TStream): string;
var slen: UINT16;
begin
  slen := ReadUINT16(Stream);
  if slen = 0 then
    Result := ''
  else
    begin
      Result := StringOfChar('?',slen);
      Stream.Read(Result[1],slen);
    end;
  slen := ReadUINT8(Stream); // Get ride of the zero byte at the end
end;

procedure ExpectUINT8(Stream: TStream; _u: UINT8);
var _v: UINT8;
    _pos: Int64;
begin
  _pos := Stream.Position;
  Stream.Read(_v{%H-},sizeof(_v));
  if _u <> _v then
    raise LCGErrorException.CreateFmt('File corrupt or wrong version, expected %2.2X at position %d, found %2.2X',[_u,_pos,_v]);
end;

procedure ExpectUINT32(Stream: TStream; _u: UINT32);
var _v: UINT32;
    _pos: Int64;
begin
  _pos := Stream.Position;
  Stream.Read(_v{%H-},sizeof(_v));
  if _u <> _v then
    raise LCGErrorException.CreateFmt('File corrupt or wrong version, expected %8.8X at position %d, found %8.8X',[_u,_pos,_v]);
end;

{ Character and string handling routines }

{
function Char32ToUTF8String(const c: TChar; _translate: boolean): UTF8String;
begin
  if _translate then
    Result := CharAsText(c)
  else if c < $80 then
    Result := Chr(c)              // ASCII
  else if c < $800 then
    Result := Chr($C0 + (c shr 6)) +
              Chr($80 + (c and $3f))
  else if c < $10000 then
    Result := Chr($E0 + (c shr 12)) +
              Chr($80 + ((c shr 6) and $3f)) +
              Chr($80 + (c and $3f))
  else if c < UNICODE_MAXIMUM_CHARACTER then
    Result := Chr($F0 + (c shr 18)) +
              Chr($80 + ((c shr 12) and $3f)) +
              Chr($80 + ((c shr 6) and $3f)) +
              Chr($80 + (c and $3f))
  else
    Result := Char32ToUTF8String(UNICODE_ERROR_CHARACTER); // Illegal character. Recursive!
end;
}

function CharAsText(c32: TChar): TString;
begin
  if Ord(c32) < 33 then
    Result := '{' + LOW_CHARS[Ord(c32)] + '}'
  else if (Ord(c32) >= 127) and (Ord(c32) <= 159) then
    Result := '{' + HIGH_CHARS[Ord(c32)] + '}'
  else if Ord(c32) >= 160 then
    Result := Format('{\x%4.4x}',[Ord(c32)])
  else
    Result := c32; // Char32ToUTF8String(c32);
end;
{
function Hex2Dec32(h: TString): UINT32;
var i: integer;
    mask: UINT32;
    c:    TChar;
begin
  Result := 0;
  for i := 0 to Length(h)-1 do
    begin
      Result := Result shl 4;
      c := h[i];
      if (c >= $0030) and (c <= $0039) then
        mask := c - $0030
      else if (c >= $0041) and (c <= $0046) then
        mask := c - $0041 + 10
      else if (c >= $0061) and (c <= $0066) then
        mask := c - $0061 + 10
      else
        raise Exception.CreateFmt('"%s" is not a hex character',[CharAsText(c)]);
    end;
end;

function IntToStr32(ival: integer): TString;
var s: string;
    slen: integer;
    i:    integer;
begin
  s := IntToStr(ival);
  slen := Length(s);
  SetLength(result,slen);
  for i := 0 to slen-1 do
    Result[i] := Ord(s[i+1]);
end;

function RightStr32(s32: TString; cnt: integer): TString;
var i: integer;
    j: integer;
    slen: integer;
begin
  slen := Length(s32);
  if cnt > slen then
    cnt := slen;
  SetLength(Result,cnt);
  i := slen - cnt;
  j := 0;
  while i < slen do
    begin
      Result[j] := s32[i];
      Inc(i);
      Inc(j);
    end;
end;

function String32ToUTF8String(const s: TString): UTF8String;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(s)-1 do
    if s[i] = 0 then
      Exit
    else
      Result := Result + Char32ToUTF8String(s[i]);
end;

function StrToInt32(const s: TString): integer;
var s2: string;
begin
  s2 := String32ToUTF8String(s);
  Result := StrToInt(s2);
end;

function UTF8Bytes(c: char): integer;
begin
  Result := -1;
  if Ord(c) < $80 then
    Result := 1
  else if Ord(c) < $C2 then
    Result := -1   // Illegal value
  else if Ord(c) < $E0 then
    Result := 2
  else if Ord(c) < $F0 then
    Result := 3
  else if Ord(c) > $F4 then
    Result := -1  // Illegal value
  else
    Result := 4;
end;

function UTF8BufToChar32(p: PChar; var consumed: integer; maxlen: integer): TChar;
const UM1 = UNICODE_MAXIMUM_CODEBYTE;   // Maximum first byte allowed for UTF-8
      UM2 = UM1 + 1;                    // First illegal byte
var i:         integer; // Input pointer
    state:     integer; // State for state machine
    uni:       TChar; // The Unicode character we build up
    opp:       integer; // The output pointer
    FCapacity: integer; // The capacity of the output string
    b:         Byte;    // Input byte
    done:      boolean; // True when it's time to quit
//
// Encoding:
//
//   1 byte = $000000-$00007F = 0aaabbbb
//   2 byte = $000080-$0007FF = 110aaabb 10bbcccc
//   3 byte = $000800-$00FFFF = 1110aaaa 10bbbbcc 10ccdddd
//   4 byte = $010000-$10FFFF = 11110abb 10bbcccc 10ddddee 10eeffff
//
// State machine:
//
//   0  = Entry point
//   1  = Single character ASCII       Valid $00-$7F
//   2  = 1st character of 2 byte set  Valid $C2-$DF
//   3  = 2nd character of 2 byte set
//   4  = 1st character of 3 byte set  Valid $E0-$EF
//   5  = 2nd character of 3 byte set
//   6  = 3rd character of 3 byte set
//   7  = 1st character of 4 byte set  Valid $F0-$F4
//   8  = 2nd character of 4 byte set
//   9  = 3rd character of 4 byte set
//   10 = 4th character of 4 byte set
//   11 = Error condition
//   12 = Good result, result holds the Unicode character
//
// Error recovery is handled by outputting a $FFFD character. Error conditions
// handled are:
//
//   A: Invalid bytes in code table
//   B: Unexpected continuation bytes
//   C: Non-continuation byte before end of character
//   D: String ending before end of character sequence
//   E: Overlong encoding
//   F: Invalid code point
//
//
begin
  i      := 0;
  state  := 0;
  uni    := 0;
  opp    := 0;
  Result := UNICODE_ERROR_CHARACTER;
  done   := False;
  while not done do
    begin
      if i < maxlen then
        b := Ord((p+i)^);
      case state of
        0:  case b of
              $00..$7F: state := 1;   // ASCII
              $80..$BF: state := 11;  // Error B: Unexpected continuation byte
              $C0..$C1: state := 11;  // Error A/E: Invalid byte in code table (overlong encoding)
              $C2..$DF: state := 2;   // Valid 1st character of 2 byte sequence
              $E0..$EF: state := 4;   // Valid 1st character of 3 byte sequence
              $F0..UM1: state := 7;   // Valid 1st character of 4 byte sequence
              UM2..$FF: state := 11;  // Error A/F: Invalid byte in code table (4 byte for > $10FFFF)
            end; // Case
        1:  begin  // ASCII
              uni := b;
              Inc(i);
              state := 12;
            end;
        2:  begin  // 1/2
              uni := b and $1F;
              Inc(i);
              state := 3;
            end;
        3:  if (b and $C0) = $80 then // 2/2
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 12;
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        4:  begin  // 1/3
              uni := b and $0F;
              Inc(i);
              state := 5;
            end;
        5:  if (b and $C0) = $80 then // 2/3
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 6;
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        6:  if (b and $C0) = $80 then // 3/3
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 12;
                if uni < $0800 then
                  state := 11; // Error E: Overlong encoding
                if (uni >= $D800) and (uni <= $DFFF) then
                  state := 11; // Error F: Invalid code point
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        7:  begin  // 1/4
              uni := b and $07;
              Inc(i);
              state := 8;
            end;
        8:  if (b and $C0) = $80 then // 2/4
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 9;
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        9:  if (b and $C0) = $80 then // 3/4
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 10;
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        10: if (b and $C0) = $80 then // 4/4
              begin
                uni := (uni shl 6) or (b and $3F);
                Inc(i);
                state := 12;
                if uni < $010000 then
                  state := 11; // Error E: Overlong encoding
                if uni > UNICODE_MAXIMUM_CHARACTER then
                  state := 11; // Error F: Invalid code point
              end
            else
              state := 11; // Error C: Non-continuation byte before end of character
        11: begin // Error condition
              uni := UNICODE_ERROR_CHARACTER;  // Get the universal "bad character" representation
              state := 12;   // Process as a good character
            end;
        12: begin
              Result := uni;
              done := True;
            end;
      end; // case
    end; // while
  consumed := i;
end;
}
{ TLCGParameterList }

procedure TLCGParameterList.LoadFromStream(Stream: TStream);
var recs: UINT32;
    i:    UINT32;
    param: TLCGParameter;
begin
  Clear;
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$01);                  // Header block identifier
  recs := ReadUINT32(Stream);
  for i := 0 to recs-1 do
    begin
      ExpectUINT8(Stream,$11);             // parameter record identifier
      param.Name  := ReadStr(Stream);
      param.value := ReadStr(Stream);
      Add(param);
    end;
end;

{ TLCGDictionary }

function TLCGDictionary.CharToDictIndex(c: TChar): TLCGStateIdentifier;
begin
  Result := FIndices[c];
end;

function TLCGDictionary.CharToDictIndexSlow(c: TChar): TLCGStateIdentifier;
var i: TLCGStateIdentifier;
    c32: UINT32;
    c1, c2: UINT32;
begin
  Result := PREDEFINED_EMPTY_STATE;
  c32 := Ord(c);
  for i := 0 to Count-1 do
    begin
      c1 := Ord(Items[i].Character);
      c2 := Ord(Items[i].CharacterTo);
      if (c32 >= c1) and (c32 <= c2) then
        Exit(i);
    end;
end;

procedure TLCGDictionary.LoadFromStream(Stream: TStream);
var recs:      UINT32;
    i:         UINT32;
    indexsize: UINT8;
    rectype:   UINT8;
    _pos:      int64;
    cval:      TCharN;
    rec:       TLCGDictionaryItem;
    c:         char;
begin
  Clear;
  cval := 0;
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$02);                 // Header block identifier
  indexsize := ReadUINT8(Stream);
  recs := ReadUINTX(Stream,indexsize);
  for i := 0 to recs-1 do
    begin
      _pos := Stream.Position;
      rectype := ReadUINT8(Stream);
      case rectype of
        $21: begin
               cval := ReadUINTX(Stream,indexsize);
               rec.Character :=   Chr(cval);
               rec.CharacterTo := Chr(cval);
               Add(rec);
             end;
        $22: begin
               cval := ReadUINTX(Stream,indexsize);
               rec.Character := Chr(cval);
               cval := ReadUINTX(Stream,indexsize);
               rec.CharacterTo := Chr(cval);
               Add(rec);
             end;
        otherwise
          raise LCGErrorException.CreateFmt('File corrupt or wrong version, expected 21 or 22 at position %d, found %2.2X',[_pos,rectype]);
      end;  // Case
    end;
  for c in char do
    FIndices[c] := CharToDictIndexSlow(c);
end;

{ TLCGDFA }

procedure TLCGDFA.LoadFromStream(Stream: TStream);
var recs:      UINT32;
    i:         UINT32;
    j:         integer;
    indexsize: UINT8;
    rec:       TLCGDFARow;
begin
  Clear;
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$04);                 // DFA block identifier
  indexsize := ReadUINT8(Stream);
  recs := ReadUINT32(Stream);
  for i := 0 to recs-1 do
    begin
      ExpectUINT8(Stream,$41);             // DFA record identifier
      SetLength(rec.Transitions,FDictCount);
      rec.AcceptToken := ReadUINT32(Stream);
      for j := 0 to FDictCount-1 do
        rec.Transitions[j] := ReadUINTZ(Stream,indexsize);
      Add(rec);
    end;
end;

{ TLCGRules }

procedure TLCGRules.LoadFromStream(Stream: TStream);
var recs:      UINT32;
    i:         UINT32;
    rec:       TLCGRule;
begin
  Clear;
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$05);                 // Rules block identifier
  recs := ReadUINT32(Stream);
  for i := 0 to recs-1 do
    begin
      ExpectUINT8(Stream,$51);             // Rules record identifier
      rec.HeadToken := ReadUINT32(Stream);
      rec.RuleCount := ReadUINT32(Stream);
      rec.RuleID    := ReadStr(Stream);
      rec.RuleText  := ReadStr(Stream);
      rec.ProcName  := ReadStr(Stream);
      Add(rec);
    end;
end;

{ TLCGLALR }

procedure TLCGLALR.LoadFromStream(Stream: TStream);
var recs:      UINT32;
    i:         UINT32;
    j:         integer;
    indexsize: UINT8;
    rec:       TLCGLALRRow;
begin
  Clear;
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$06);                 // LALR block identifier
  indexsize := ReadUINT8(Stream);
  recs := ReadUINT32(Stream);
  for i := 0 to recs-1 do
    begin
      ExpectUINT8(Stream,$61);             // LALR record identifier
      SetLength(rec,FTokenCount);
      for j := 0 to FTokenCount-1 do
        begin
          rec[j].OutputType  := TLCGParserOutputType(ReadUINT8(Stream));
          rec[j].Destination := ReadUINTZ(Stream,indexsize);
        end;
      Add(rec);
    end;
end;

{ TLacParser }

constructor TLCGParser.Create;
begin
  inherited Create;
  FParameterList := TLCGParameterList.Create;
  FDictionary    := TLCGDictionary.Create;
  FDFA           := TLCGDFA.Create;
  FRules         := TLCGRules.Create;
  FLALR          := TLCGLALR.Create;
  FLogLevel  := ltInfo; // Maximum log level
  FOnMonitor := nil;
  FOnReduce  := nil;
  FOnToken   := nil;
  InitStart;
end;

destructor TLCGParser.Destroy;
begin
  FLALR.Free;
  FRules.Free;
  FDFA.Free;
  FDictionary.Free;
  FParameterList.Free;
  inherited Destroy;
end;

procedure TLCGParser.CheckStackSize;
begin
  if FParserSP >= Length(FParserStack) then
	begin
	  SetLength(FParserStack,Length(FParserStack)*2);
	end;
end;

function TLCGParser.GetRuleProcs: TStringArray;
var i: integer;
begin
  SetLength(Result,FRules.Count);
  for i := 0 to FRules.Count-1 do
    Result[i] := FRules.Items[i].ProcName;
end;

function TLCGParser.GetRules: integer;
begin
  Result := FRules.Count;
end;

procedure TLCGParser.InitRun;
begin
  // Set up lexer variables
  FFetched         := False;
  FInputLine       := 1;
  FInputColumn     := 1;
  FInputLineSave   := 1;
  FInputColumnSave := 1;
  FLexerMode       := lmStart;
  FLexerState      := 0;
  FLexerBufIndex   := 0;
  FLexerBufRemain  := 0;
  // Set up parser variables
  FParserSP := 0;
  FToken := 0 {Error};
  // Other stuff
  FStartTime := Now;
end;

procedure TLCGParser.InitStart;
begin
  FLoaded := False;
  // Reset the tables
  FParameterList.Clear;
  FDictionary.Clear;
  SetLength(FTokens,0);
  FDFA.Clear;
  FRules.Clear;
  FLALR.Clear;
  // Set up some sizes
  SetLength(FParserStack,PARSER_STACK_SIZE_DEFAULT);
  LexBufBlock  := LEXBUF_BLOCK_SIZE_DEFAULT;
  TokenBufSize := TOKEN_BUF_SIZE_DEFAULT;
  FTabSize := 4;
end;

function TLCGParser.Lexer: TLCGTokenIdentifier;
var pk:        TChar;             // Peek character
    pki:       TLCGStateIdentifier; // Peek character turned into a dictionary index
    newstate:  TLCGStateIdentifier; // New state that we will move to
    tokendone: boolean;
    bptr:      integer;
  	valid: 	   boolean;
begin
  valid := False;
  while not valid do
    begin
      FBufferOverflow := False;
      Result := 0;
      bptr := 0;
      FTokenBuf[bptr] := #0;
      tokendone := False;
  	  if FLexerMode = lmEOF then
  	    Exit(PREDEFINED_TOKEN_EOF);
      // Token loop
  	  while not tokendone do
  	    begin
  		  pk := LexPeek;
          pki := FDictionary.CharToDictIndex(pk);
          if pki = PREDEFINED_EMPTY_STATE then
            newstate := pki
          else
            newstate := FDFA.Items[FLexerState].Transitions[pki];
  		  if newstate = PREDEFINED_EMPTY_STATE then
  		    tokendone := True
  		  else
  		    begin
  			  pk := LexGet;
  			  if bptr < (FTokenBufSize - 2) then
  			    begin
  				  FTokenBuf[bptr] := pk;
  				  Inc(bptr);
  			    end
  			  else
  			    begin
  				  FBufferOverflow := True;
  			    end;
  		    end;
  		  if not tokendone then
  		    FLexerState := newstate;
  		  if FLexerMode = lmEOF then
  		    tokendone := True;
  	    end;
      // End of token loop
      if FDFA.Items[FLexerState].AcceptToken <> PREDEFINED_EMPTY_TOKEN then
  	    Result := FDFA.Items[FLexerState].AcceptToken
  	  else
  	    begin
          FTokenBuf[bptr] := #0;
  		  Result := PREDEFINED_TOKEN_ERROR;
  		  if FLexerMode <> lmEOF then
  		    LexGet; // Bin the broken character
          FWrongCharacter := pk;
  	    end;
  	  FLexerState := 0;
  	  FTokenBuf[bptr] := #0;
  	  valid := not FTokens[Result].Ignore;
    end;
end;

function TLCGParser.LexGet: TChar;
begin
  FInputColumnSave := FInputColumn;
  FInputLineSave   := FInputLine;
  Result := LexPeek;
  FLexerBufIndex := FLexerBufIndex + FBufferIncrement;
  FLexerBufRemain := FLexerBufRemain - FBufferIncrement;
  if (FLexerBufRemain <= 0) then
    FLexerMode := lmEOF;
  if Result = #9 then // Tab character
    repeat
      Inc(FInputColumn);
    until ((FInputColumn-1) mod FTabSize) = 0
  else if Result = #10 then // Newline character
    begin
      Inc(FInputLine);
      FInputColumn := 1;
    end
  else
    Inc(FInputColumn);
end;

function TLCGParser.LexPeek: TChar;
begin
  if FLexerMode = lmStart then
	begin
	  LexReadChunk;
	end;
  if FLexerMode = lmOperating then
    begin
      if (FLexerBufRemain <= LEXBUF_MIN) then
        begin
    	  // Move the buffer down if required
          LexShiftBuffer;
    	  // Try and read a chunk
          LexReadChunk;
    	end;
    end;
  // Sanity check
  if FLexerBufRemain = 0 then
    Monitor(ltInternal,'Attempt to call LexPeek() when buffer is empty');
  // We have characters in the buffer, do the peek
  // @@@@@ THIS IS THE POINT AT WHICH WE WOULD CONVERT FROM UTF8 TO
  // @@@@@ 32 BIT CHARACTER
  // Process at this point to create a 32 bit character from UTF8 if required

{$IFDEF LCG8}
  FBufferIncrement := 1;
  Result := FLexerBuffer[FLexerBufIndex];
{$ENDIF}
{
  FBufferIncrement := UTF8Bytes(FLexerBuffer[FLexerBufIndex]);
  if FLexerBufRemain < FBufferIncrement then
    Result := UNICODE_ERROR_CHARACTER
  else
    begin
      maxbytes := FBufferIncrement;
      Result := UTF8BufToChar32(@FLexerBuffer[FLexerBufIndex],FBufferIncrement,maxbytes);
    end;
}
end;

procedure TLCGParser.LexReadChunk;
begin
  FLexerBufRemain := FLexerBufRemain + FStream.Read(FLexerBuffer[FLexerBufRemain],FLexBufBlock);
//if FLexerBufRemain > 0 then
  if FStream.Position < FStream.Size then
    FLexerMode := lmOperating
  else if FLexerBufRemain > 0 then
    FLexerMode := lmFileDone
  else
    FLexerMode := lmEOF;
end;

procedure TLCGParser.LexShiftBuffer;
var i: integer;
begin
  if (FLexerBufIndex > 0) and (FLexerBufRemain > 0) and (FLexerMode < lmFileDone) then
    begin
      for i := 0 to FLexerBufRemain-1 do
		FLexerBuffer[i] := FLexerBuffer[FLexerBufIndex+i];
      FLexerBufIndex := 0;
    end;
end;


procedure TLCGParser.LoadFromFile(const Filename: string);
var FileStream: TFileStream;
begin
  if not FileExists(Filename) then
    Monitor(ltError,'File ' + Filename + ' does not exist');
  FileStream := TFileStream.Create(Filename,fmOpenRead,fmShareDenyWrite);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TLCGParser.LoadFromResource(const ResourceName: string);
var Resource: TResourceStream;
begin
  Resource := TResourceStream.Create(HInstance,ResourceName,RT_RCDATA);
  if Resource = nil then
    Monitor(ltInternal,'Resource ' + ResourceName + ' does not exist');
  try
    LoadFromStream(Resource);
  finally
    Resource.Free;
  end;
end;

procedure TLCGParser.LoadFromStream(Stream: TStream);
begin
  // Read all the items in
  FParameterList.LoadFromStream(Stream);
  FDictionary.LoadFromStream(Stream);
  LoadTokensFromStream(Stream);
  FDFA.DictCount := FDictionary.Count;
  FDFA.LoadFromStream(Stream);
  FRules.LoadFromStream(Stream);
  FTokenCount := Length(FTokens);
  FLALR.TokenCount := FTokenCount;
  FLALR.LoadFromStream(Stream);
  FLoaded := True;
end;

procedure TLCGParser.LoadTokensFromStream(Stream: TStream);
var recs: UINT32;
    i:    UINT32;
    bval: UINT8;
    rec:  TLCGTokenItem;
begin
  SetLength(FTokens,0);
  ExpectUINT32(Stream,LACOBJ_MAGIC_WORD);  // Magic word
  ExpectUINT8(Stream,$03);                 // Token block identifier
  recs := ReadUINT32(Stream);
  SetLength(FTokens,recs);
  for i := 0 to recs-1 do
    begin
      ExpectUINT8(Stream,$31);             // Token record identifier
      bval := ReadUINT8(Stream);
      rec.Ignore := (bval <> 0);
      rec.Name := ReadStr(Stream);
      FTokens[i] := rec;
    end;
end;

procedure TLCGParser.Monitor(LogType: TLCGLogType; const Message: string);
begin
  if Assigned(FOnMonitor) and (LogType <= FLogLevel) then
    FOnMonitor(Self,LogType,Message);
  case LogType of
    ltError:    raise LCGErrorException.Create(Message);
    ltInternal: raise LCGInternalException.Create(Message);
  end;  // Case
end;

procedure TLCGParser.Monitor(LogType: TLCGLogType; const Message: string; const Args: array of const);
begin
  Monitor(LogType,Format(Message,Args));
end;

procedure TLCGParser.Parse(Stream: TStream);
var done: boolean;
    pk:   TLCGTokenIdentifier;
    empty: array of char;
begin
  if not FLoaded then
    Monitor(ltInternal,'Parser tables not loaded');
  FStream := Stream;
  InitRun;
  SetLength(empty,0);
  Push(0,FTokenCount-1,empty);    // TERMINAL_COUNT-1 will always be the <$accept> token
  // Main parsing routine
  done := false;
  while not done do
    begin
	  pk := ParserPeek;
      if pk = PREDEFINED_TOKEN_ERROR then
        Monitor(ltError,'Unexpected character %s in input',[CharAsText(FWrongCharacter)]);
      case FLALR.Items[TosState][pk].OutputType of
        potUndefined: begin
    					Monitor(ltError,'Undefined parser table action for state %d and token %s',[TosState,FTokens[pk].Name]);
  	                    done := True;
  		              end;
        potError:     begin
  					    Monitor(ltError,'Unexpected token %s in input',[FTokens[pk].Name]);
  					    done := True;
  				      end;
  	    potShift:     begin
                        FFetched := False; // Clear the buffer to force a new character
                        Push(FLALR.Items[TosState][pk].Destination,pk,FTokenbuf);
                      end;
        potGoto:      begin // Unexpected as this should follow a reduce
  					    Monitor(ltError,'Unexpected goto from table');
  					    done := True;
  				      end;
  	    potReduce:    Reduce(FLALR.Items[TosState][pk].Destination);
  	    potAccept:    begin
  					    Pop;
  				        done := True;
  				      end;
      end; // case LALR_actions...
  end;
end;

function TLCGParser.ParserPeek: TLCGTokenIdentifier;
begin
  if (not FFetched) and (FToken <> PREDEFINED_TOKEN_EOF) then
    begin
      FToken := Lexer;
	  FFetched := True;
	end;
  Result := FToken;
end;

procedure TLCGParser.Pop;
begin
  if FParserSP <= 0 then
    Monitor(ltInternal,'Stack empty');
  Dec(FParserSP);
end;

procedure TLCGParser.Push(_entry: TLCGParserStackEntry);
begin
  if FParserSP >= PARSER_STACK_SIZE_MAX then
    Monitor(ltError,'PARSER_STACK_SIZE_MAX (%d) exceeded',[PARSER_STACK_SIZE_MAX]);
  CheckStackSize;
  FParserStack[FParserSP] := _entry;
  Inc(FParserSP);
end;

procedure TLCGParser.Push(_state: TLCGStateIdentifier; _token: TLCGTokenIdentifier; _buf: array of char);
var _entry: TLCGParserStackEntry;
begin
  _entry.State  := _state;
  _entry.Token  := _token;
  _entry.Buf    := _buf;
  Push(_entry);
end;

procedure TLCGParser.Reduce(ruleindex: TLCGStateIdentifier);
var reduction: TLCGParserStackEntry;
    i:		   integer;
	rcount:    integer;
    headrule:  TLCGTokenIdentifier;
begin
  rcount := FRules.Items[ruleindex].RuleCount;
  try
    if Assigned(FOnReduce) then
      reduction := FOnReduce(Self,ruleindex);
  except
    on E:LCGErrorException    do raise ; // Nothing
    on E:LCGInternalException do raise ; // Nothing
    on E:Exception do Monitor(ltError,E.Message);
  end;
  // Pop all the elements which make up the rule
  for i := 0 to rcount-1 do
    Pop;
  // Now work out the next state
  headrule := FRules.Items[ruleindex].HeadToken;
  if FLALR.Items[TosState][headrule].OutputType <> potGoto then
	Monitor(ltError,'Goto expected but not found in table');
  reduction.State := FLALR.Items[TosState][headrule].Destination;
  reduction.Token := FRules.Items[ruleindex].HeadToken;
  // And push the reduction onto the stack
  Push(reduction);
end;

procedure TLCGParser.SetLexBufBlock(_size: integer);
begin
  // Check to ensure we're not in the middle of something
  if FLexerBufRemain > 0 then
    Monitor(ltError,'Attempt to set lexer buffer size while in the middle of an activity');
  FLexBufBlock := _size;
  FLexBufSize  := _size + LEXBUF_MIN;
  SetLength(FLexerBuffer,FLexBufSize);
end;

procedure TLCGParser.SetTokenBufSize(_size: integer);
begin
  FTokenBufSize := _size;
  SetLength(FTokenBuf,_size);
end;

function TLCGParser.Tos: TLCGParserStackEntry;
begin
  Result := FParserStack[FParserSP-1];
end;

function TLCGParser.TosState: TLCGStateIdentifier;
begin
  Result := Tos.State;
end;

end.


