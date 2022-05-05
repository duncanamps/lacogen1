unit uparser_utility;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uparser_types, deployment_parser_types_12;

function BoolToString(_b: boolean): string;
function BytesInANumber(_n: uint64): integer;
function CalculateFileHeaderCheck(const fileheader: TDFAFileHeader): UINT32;
function CharAsText(ch: char; underscores: boolean = True): string;
function CharAsXMLHeading(ch: char): string;
function DataType(_n: uint64): string;
function DigitsInANumber(_num: integer): integer;
function IsLetter(ch: char): boolean;
function IsWhiteSpace(c: char): boolean;
function MakeFriendly(const s: TString): TString;
function MakeFriendlyChar(c: TChar): TString;
function MakePrintable(const s: TString): string;
function MakePrintable(c: TChar): string;
function MakeXMLHeading(const s: string): string;
function NonTerminalNameToToken(const s: string): string;
function OppositeCase(ch: char): char;
function Pull(var s: string): string;
function StringAsText(const s: string; underscores: boolean = True): string;
function StringAsXMLHeading(const s: string): string;
function StripAngleBrackets(const s: string): string;
function StripQuotes(const s: string): string;
procedure WriteStringToStream(_strm: TStream; const s: string);
procedure WriteLnStringToStream(_strm: TStream; const s: string);
procedure WriteLnStringToStreamUnderlined(_strm: TStream; const s: string);

implementation

uses
  uparser_exception, deployment_parser_module_12, htmlelements;

function BoolToString(_b: boolean): string;
begin
  if _b then
    Result := 'True'
  else
    Result := 'False';
end;

function BytesInANumber(_n: uint64): integer;
begin
  if _n < 256 then
    Exit(1);
  if _n < 65536 then
    Exit(2);
  if _n < $100000000 then
    Exit(4);
  Exit(8);
end;

function CalculateFileHeaderCheck(const fileheader: TDFAFileHeader): UINT32;
var spinner: UINT32;
    tmp:     UINT32;
  procedure Spin(_uval: UINT32);
  begin
    tmp := spinner shr (32-5);
    spinner := (spinner shl 5) or tmp;
    spinner := spinner xor _uval;
  end;
begin
  spinner := fileheader.signature;
  Spin(fileheader.tokensize);
  Spin(fileheader.statesize);
  Spin(fileheader.charsize);
  Spin(fileheader.records);
  CalculateFileHeaderCheck := spinner;
end;

function CharAsText(ch: char; underscores: boolean): string;
begin
  case ch of
    #0:  Result := '_NUL_';
    #1:  Result := '_SOH_';
    #2:  Result := '_STX_';
    #3:  Result := '_ETX_';
    #4:  Result := '_EOT_';
    #5:  Result := '_ENQ_';
    #6:  Result := '_ACK_';
    #7:  Result := '_BEL_';
    #8:  Result := '_BS_';
    #9:  Result := '_TAB_';
    #10: Result := '_LF_';
    #11: Result := '_VT_';
    #12: Result := '_FF_';
    #13: Result := '_CR_';
    #14: Result := '_SO_';
    #15: Result := '_SI_';
    #16: Result := '_DLE_';
    #17: Result := '_DC1_';
    #18: Result := '_DC2_';
    #19: Result := '_DC3_';
    #20: Result := '_DC4_';
    #21: Result := '_NAK_';
    #22: Result := '_SYN_';
    #23: Result := '_ETB_';
    #24: Result := '_CAN_';
    #25: Result := '_EM_';
    #26: Result := '_SUB_';
    #27: Result := '_ESC_';
    #28: Result := '_FS_';
    #29: Result := '_GS_';
    #30: Result := '_RS_';
    #31: Result := '_US_';
    #32: Result := '_SPC_';
    '!': Result := '_EXCLAIM_';
    '"': Result := '_DQUOTE_';
    '#': Result := '_HASH_';
    '$': Result := '_DOLLAR_';
    '%': Result := '_PERCENT_';
    '&': Result := '_AMPERSAND_';
    #39: Result := '_SQUOTE_';
    '(': Result := '_LBRACKET_';
    ')': Result := '_RBRACKET_';
    '*': Result := '_ASTERISK_';
    '+': Result := '_PLUS_';
    ',': Result := '_COMMA_';
    '-': Result := '_HYPHEN_';
    '.': Result := '_PERIOD_';
    '/': Result := '_SLASH_';
    ':': Result := '_COLON_';
    ';': Result := '_SEMICOLON_';
    '<': Result := '_LABRACKET_';
    '=': Result := '_EQUALS_';
    '>': Result := '_RABRACKET_';
    '?': Result := '_QUESTION_';
    '@': Result := '_AT_';
    '[': Result := '_LSBRACKET_';
    '\': Result := '_BACKSLASH_';
    ']': Result := '_RSBRACKET_';
    '^': Result := '_CARET_';
    '_': Result := '_UNDERSCORE_';
    '`': Result := '_TICK_';
    '{': Result := '_LCBRACKET_';
    '|': Result := '_RULE_';
    '}': Result := '_RCBRACKET_';
    '~': Result := '_TILDE_';
    #127: Result := '_DEL_';
    #128..#255: Result := '_' + IntToStr(Ord(ch)) + '_';
    otherwise
      Result := ch;
  end;
  if (ch <> '_') and (not underscores) then
    Result := StringReplace(Result,'_','',[rfReplaceAll]);
end;

function CharAsXMLHeading(ch: char): string;
begin
  case ch of
    #0:  Result := '_NUL_';
    #1:  Result := '_SOH_';
    #2:  Result := '_STX_';
    #3:  Result := '_ETX_';
    #4:  Result := '_EOT_';
    #5:  Result := '_ENQ_';
    #6:  Result := '_ACK_';
    #7:  Result := '_BEL_';
    #8:  Result := '_BS_';
    #9:  Result := '_TAB_';
    #10: Result := '_LF_';
    #11: Result := '_VT_';
    #12: Result := '_FF_';
    #13: Result := '_CR_';
    #14: Result := '_SO_';
    #15: Result := '_SI_';
    #16: Result := '_DLE_';
    #17: Result := '_DC1_';
    #18: Result := '_DC2_';
    #19: Result := '_DC3_';
    #20: Result := '_DC4_';
    #21: Result := '_NAK_';
    #22: Result := '_SYN_';
    #23: Result := '_ETB_';
    #24: Result := '_CAN_';
    #25: Result := '_EM_';
    #26: Result := '_SUB_';
    #27: Result := '_ESC_';
    #28: Result := '_FS_';
    #29: Result := '_GS_';
    #30: Result := '_RS_';
    #31: Result := '_US_';
    #32: Result := '_SPC_';
    '!': Result := '_EXCLAIM_';
    '"': Result := '_DQUOTE_';
    '#': Result := '_HASH_';
    '$': Result := '_DOLLAR_';
    '%': Result := '_PERCENT_';
    '&': Result := '_AMPERSAND_';
    #39: Result := '_SQUOTE_';
    '(': Result := '_LBRACKET_';
    ')': Result := '_RBRACKET_';
    '*': Result := '_ASTERISK_';
    '+': Result := '_PLUS_';
    ',': Result := '_COMMA_';
    '-': Result := '_HYPHEN_';
    '.': Result := '_PERIOD_';
    '/': Result := '_SLASH_';
    ':': Result := '_COLON_';
    ';': Result := '_SEMICOLON_';
    '<': Result := '_LABRACKET_';
    '=': Result := '_EQUALS_';
    '>': Result := '_RABRACKET_';
    '?': Result := '_QUESTION_';
    '@': Result := '_AT_';
    '[': Result := '_LSBRACKET_';
    '\': Result := '_BACKSLASH_';
    ']': Result := '_RSBRACKET_';
    '^': Result := '_CARET_';
//  '_': Result := '_UNDERSCORE_';
    '`': Result := '_TICK_';
    '{': Result := '_LCBRACKET_';
    '|': Result := '_RULE_';
    '}': Result := '_RCBRACKET_';
    '~': Result := '_TILDE_';
    #127: Result := '_DEL_';
    #128..#255: Result := '_' + IntToStr(Ord(ch)) + '_';
    otherwise
      Result := ch;
  end;
end;

function DataType(_n: uint64): string;
var b: integer;
begin
  Result := 'error';
  b := BytesInANumber(_n);
  case b of
    1: Result := 'uint8';
    2: Result := 'uint16';
    4: Result := 'uint32';
    8: Result := 'uint64';
  end;
end;

function DigitsInANumber(_num: integer): integer;
begin
  if _num < 10 then
    Result := 1
  else if _num < 100 then
    Result := 2
  else if _num < 1000 then
    Result := 3
  else if _num < 10000 then
    Result := 4
  else if _num < 100000 then
    Result := 5
  else if _num < 1000000 then
    Result := 6
  else if _num < 10000000 then
    Result := 7
  else
    Result := 8; // Default to 8, we won't need numbers that big
end;

function IsLetter(ch: char): boolean;
begin
  IsLetter := (ch in ['A'..'Z','a'..'z']);
end;

function IsWhiteSpace(c: char): boolean;
begin
  result := c in [#9,#10,#13,' ',#160];
end;

function MakeFriendly(const s: TString): TString;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + MakeFriendlyChar(s[i]);
end;

function MakeFriendlyChar(c: TChar): TString;
begin
  if (c >= Chr(33)) and (c <= Chr(126)) then
    Result := c
  else
    Result := '{' + IntToStr(Ord(c)) + '}';
end;


function MakePrintable(const s: TString): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + MakeFriendlyChar(s[i]);
end;

function MakePrintable(c: TChar): string;
begin
  if (c >= Chr(33)) and (c <= Chr(126)) then
    Result := c
  else
    Result := '{' + IntToStr(Ord(c)) + '}';
end;

function MakeXMLHeading(const s: string): string;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + CharAsText(s[i]);
end;

function NonTerminalNameToToken(const s: string): string;
begin
  Result := StripAngleBrackets(s);
  Result := StringReplace(Result,' ','_',[rfReplaceAll]);
end;

function OppositeCase(ch: char): char;
begin
  if (ch in ['A'..'Z']) then
    result := Chr(Ord(ch)+32)
  else if (ch in ['a'..'z']) then
    result := Chr(Ord(ch)-32)
  else
    raise Exception.Create('Attempt to take opposite case of non-letter');
end;

function Pull(var s: string): string;
begin
  Result := '';
  s := Trim(s);
  while (s <> '') and (not IsWhiteSpace(s[1])) do
    begin
      Result := Result + s[1];
      Delete(s,1,1);
    end;
  Trim(s);
end;

function StringAsText(const s: string; underscores: boolean): string;
var i: integer;
begin
  if s = '(Error)' then
    Exit('__ERROR__');
  if s = '(EOF)' then
    Exit('__EOF__');
  if s = '(Comment)' then
    Exit('__COMMENT__');
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + CharAsText(s[i],underscores);
end;

function StringAsXMLHeading(const s: string): string;
var i: integer;
begin
  if s = '(Error)' then
    Exit('__ERROR__');
  if s = '(EOF)' then
    Exit('__EOF__');
  if s = '(Comment)' then
    Exit('__COMMENT__');
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + CharAsXMLHeading(s[i]);
  if Result[1] in ['0'..'9'] then
    Result := 'X' + Result;
  Result := EscapeHTML(Result);
end;

function StripAngleBrackets(const s: string): string;
begin
  // Strip the '<' and '>' from the name if needed
  Result := s;
  if (Result <> '') and (Result[1] = '<') then
    Delete(Result,1,1);
  if (Result <> '') and (Result[Length(Result)] = '>') then
    Delete(Result,Length(Result),1);
end;

function StripQuotes(const s: string): string;
begin
  // Strip the '"' and '"' from the name if needed
  Result := s;
  if (Result <> '') and (Result[1] = '"') then
    Delete(Result,1,1);
  if (Result <> '') and (Result[Length(Result)] = '"') then
    Delete(Result,Length(Result),1);
end;

procedure WriteStringToStream(_strm: TStream; const s: string);
begin
  _strm.Write(s[1],Length(s));
end;

procedure WriteLnStringToStream(_strm: TStream; const s: string);
begin
  WriteStringToStream(_strm,s + #13 + #10);
end;

procedure WriteLnStringToStreamUnderlined(_strm: TStream; const s: string);
begin
  WriteStringToStream(_strm,s + #13 + #10);
  WriteStringToStream(_strm,StringOfChar('=',Length(s)) + #13 + #10);
end;

end.

