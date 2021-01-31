unit ucharset_literal;

{$mode objfpc}{$H+}

//
// Decode the literal strings used to make up a character set. For example:
//
//   [0123456789]  // The digits
//   [0-9]         // Digits done a different way
//   [^0-9]        // Anything but digits
//   [A-Za-z]      // Letters
//   [\t]          // Tab
//   [\n]          // Newline
//   [\r]          // Carriage return
//   [\x00]        // Character 0 (hex)
//   [\x20]        // Space character
//   [\x30-\x39]   // Digits done another different way
//   [\x20ac]      // 16 bit character (Euro sign in this case)
//   [\\]          // Backslash character
//   [\]]          // Closing square bracket
//   [\-]          // Hyphen character
//   [\^]          // Caret character
//
// Supply it with a character set for the work to be done.
//
// See gold\lacogen0_setliteral.txt for a dump of the grammer, DFA and parser
// information
//



interface

uses
  Classes, SysUtils, ucharset, uset32;

procedure MakeSetLiteral(cs: TSet32; const s: string);

implementation

uses
  strutils, uparser_exception, uparser_types;

const
  MAX_STACK = 100;

  SYM_EOF            = 0;
  SYM_ERROR          = 1;
  SYM_HYPHEN         = 2;
  SYM_LBRACKET       = 3;
  SYM_EMPTYSET       = 4;
  SYM_RBRACKET       = 5;
  SYM_CARET          = 6;
  SYM_CHARESCAPED    = 7;
  SYM_CHARHEX        = 8;
  SYM_CHARNONESCAPED = 9;
  SYM_CHARACTER      = 10;
  SYM_GRAMMAR        = 11;
  SYM_LITERALDEF     = 12;
  SYM_LITERALDEFS    = 13;

type
  TToken = record
    Sym:  integer;
    Data: string;
  end;

  TStackRec = record
    State: integer;
    Sym:   integer;
    Data:  string;
  end;

var
  DFAstate:       integer;
  LALRstate:      integer;
  tok:            TToken;
  strm:           TStringStream;
  stack:          array[0..MAX_STACK-1] of TStackRec;
  stackpointer:   integer; // Points to TOS + 1
  charset:        TSet32;
  ParserAccepted: boolean;
  checkproduction: boolean;




procedure Push(_state: integer; _tok: TToken);
begin
  stack[stackpointer].State := _state;
  stack[stackpointer].Sym   := _tok.Sym;
  stack[stackpointer].Data  := _tok.Data;
  Inc(stackpointer);
  if stackpointer >= MAX_STACK then
    raise Exception.Create('MAX_STACK exceeded');
end;

procedure Pop;
begin
  if stackpointer <= 0 then
    raise Exception.Create('Stack empty');
  Dec(stackpointer);
end;

function PeekNextChar: char;
begin
  if strm.Position = strm.Size then
    PeekNextChar := #26
  else
    begin
      strm.Read(Result,1);
      strm.Position := strm.Position-1;
    end;
end;

function NextChar: char;
begin
  if strm.Position = strm.Size then
    NextChar := #26
  else
    strm.Read(Result,1);
end;

procedure LexerGoto(_newstate: integer);
begin
  tok.Data := tok.Data + NextChar;
  DFAstate := _newstate;
end;

procedure Fetch;
var accept: boolean;
begin
  DFAstate := 0;
  tok.Data := '';
  accept := False;
  while not accept do
    case DFAstate of
      0: case PeekNextChar of
           '-': LexerGoto(1);
           ']': LexerGoto(2);
           '^': LexerGoto(3);
           ' '..',',
           '.'..'Z',
           '_'..'~': LexerGoto(4);
           '[': LexerGoto(5);
           '\': LexerGoto(7);
           #26: begin
                  accept := True;
                  tok.Sym := SYM_EOF;
                end
           else begin
             accept := True;
             tok.Sym := SYM_ERROR;
           end;
         end; // case 0
      1: begin
           accept := True;
           tok.Sym := SYM_HYPHEN;
         end;
      2: begin
           accept := True;
           tok.Sym := SYM_RBRACKET;
         end;
      3: begin
           accept := True;
           tok.Sym := SYM_CARET;
         end;
      4: begin
           accept := True;
           tok.Sym := SYM_CHARNONESCAPED;
         end;
      5: case PeekNextChar of
           ']': LexerGoto(6);
           else begin
             accept := True;
             tok.Sym := SYM_LBRACKET;
           end;
         end;
      6: begin
           accept := True;
           tok.Sym := SYM_EMPTYSET;
         end;
      7: case PeekNextChar of
           '-','[','\',
           ']','^',
           'n','r','t',
           'N','R','T': LexerGoto(8);
           'X','x':     LexerGoto(9);
           else begin
             accept := True;
             tok.Sym := SYM_ERROR;
           end;
         end;
      8: begin
           accept := True;
           tok.Sym := SYM_CHARESCAPED;
         end;
      9: case PeekNextChar of
           '0'..'9',
           'A'..'F',
           'a'..'f':  LexerGoto(10);
           else begin
             accept := True;
             tok.Sym := SYM_ERROR;
           end;
         end;
      10: case PeekNextChar of
            '0'..'9',
            'A'..'F',
            'a'..'f':  LexerGoto(11);
            else begin
              accept := True;
              tok.Sym := SYM_ERROR;
            end;
          end;
      11: case PeekNextChar of
            '0'..'9',
            'A'..'F',
            'a'..'f':  LexerGoto(12);
            else begin
              accept := True;
              tok.Sym := SYM_CHARHEX;
            end;
          end;
      12: case PeekNextChar of
            '0'..'9',
            'A'..'F',
            'a'..'f':  LexerGoto(13);
            else begin
              accept := True;
              tok.Sym := SYM_ERROR;
            end;
          end;
      13: begin
            accept := True;
            tok.Sym := SYM_CHARHEX;
          end;
    end;
end;

function Peek: TToken;
begin
  if tok.Sym < 0 then
    Fetch;
  Peek := tok;
end;

function StackTop: TStackRec;
begin
  if stackpointer <= 0 then
    raise Exception.Create('Stack empty');
  StackTop := stack[stackpointer-1];
end;

procedure ParserShift(_newstate: integer);
begin
  tok.Sym := -1; // Reset the token so a new one gets pulled
  LALRstate := _newstate;
end;

procedure ParserGoto(_newstate: integer);
begin
  LALRstate := _newstate;
end;

procedure ErrorUnexpected;
begin
  raise Exception.Create(Format('Unexpected token %d value %s at state %d',[StackTop.Sym,StackTop.Data,StackTop.State]));
end;

procedure CheckCharacterRange(const s: string);
{$IFDEF CS_8BIT}
var v: word;
{$ENDIF}
begin
{$IFDEF CS_8BIT}
  v := StrToInt(s);
  if v > $ff then
    raise LCGErrorException.Create('Attempt to create a 16 bit character in an 8 bit codespace');
{$ENDIF}
end;

procedure ParserAccept({%H-}_rule: integer);
begin
  Pop; // Pop the last token
  ParserAccepted := True;
end;

procedure ParserReduce0;
begin
  { @@@@@ Remove from the grammar
  // <Grammar> ::= [ ^ <LiteralDefs> ]
  Pop;  // Pop the last token off the stack
  Pop;  // Pop the ']'
  Pop;  // The <LiteralDefs>
  Pop;  // And finally the '^'
  charset.OpNegate;
  stack[stackpointer-1].Sym  := SYM_GRAMMAR;
  stack[stackpointer-1].Data := '<Grammar>';
  checkproduction := True;
  }
end;

procedure ParserReduce1;
begin
  // <Grammar> ::= [ <LiteralDefs> ]
  Pop;  // Pop the last token off the stack
  Pop;  // Pop the ']'
  Pop;  // And the <LiteralDefs>
  stack[stackpointer-1].Sym  := SYM_GRAMMAR;
  stack[stackpointer-1].Data := '<Grammar>';
  checkproduction := True;
end;

procedure ParserReduce2;
begin
  // <Grammar> ::= []
  Pop;  // Pop the last token off the stack
  stack[stackpointer-1].Sym  := SYM_GRAMMAR;
  stack[stackpointer-1].Data := '<Grammar>';
  checkproduction := True;
end;

procedure ParserReduce3;
begin
  // <LiteralDefs> ::= <LiteralDefs> <LiteralDef>
  Pop;  // Pop the last token off the stack
  Pop;  // And the literal def
  stack[stackpointer-1].Sym  := SYM_LITERALDEFS;
  stack[stackpointer-1].Data := '<LiteralDefs>';
  checkproduction := True;
end;

procedure ParserReduce4;
begin
  // <LiteralDefs> ::= <LiteralDef>
  Pop;  // Pop the last token off the stack
  stack[stackpointer-1].Sym  := SYM_LITERALDEFS;
  stack[stackpointer-1].Data := '<LiteralDefs>';
  checkproduction := True;
end;

procedure ParserReduce5;
var rangefrom, rangeto: TSet32Item;
    i: TSet32Item;
begin
  // <LiteralDef> ::= <Character> - <Character>
  Pop;  // Pop the last token off the stack
  CheckCharacterRange(stack[stackpointer-3].Data);
  CheckCharacterRange(stack[stackpointer-1].Data);
  rangefrom := StrToInt(stack[stackpointer-3].Data);
  rangeto   := StrToInt(stack[stackpointer-1].Data);
  for i := rangefrom to rangeto do
    charset.AddElement(i);
  Pop; // Pop two more tokens (the '-' and the second <character>
  Pop;
  stack[stackpointer-1].Sym  := SYM_LITERALDEF;
  stack[stackpointer-1].Data := '<LiteralDef>';
  checkproduction := True;
end;

procedure ParserReduce6;
begin
  // <LiteralDef> ::= <Character>
  Pop;  // Pop the last token off the stack
  CheckCharacterRange(stack[stackpointer-1].Data);
  charset.AddElement(StrToInt(stack[stackpointer-1].Data));
  stack[stackpointer-1].Sym  := SYM_LITERALDEF;
  stack[stackpointer-1].Data := '<LiteralDef>';
  checkproduction := True;
end;

procedure ParserReduce7;
begin
  // <Character> ::= CharNonEscaped
  Pop;  // Pop the last token off the stack
  stack[stackpointer-1].Data := IntToStr(Ord(stack[stackpointer-1].Data[1]));
  stack[stackpointer-1].Sym := SYM_CHARACTER;
  checkproduction := True;
end;

procedure ParserReduce8;
var ival: integer;
begin
  // <Character> ::= CharHex
  Pop;  // Pop the last token off the stack
  ival := Hex2Dec(RightStr(stack[stackpointer-1].Data,Length(stack[stackpointer-1].Data)-2));
  stack[stackpointer-1].Data := IntToStr(ival);
  stack[stackpointer-1].Sym := SYM_CHARACTER;
  checkproduction := True;
end;

procedure ParserReduce9;
begin
  // <Character> ::= CharEscaped
  Pop;  // Pop the last token off the stack
  case Rightstr(stack[stackpointer-1].Data,1) of
    'n','N': stack[stackpointer-1].Data := '10';
    'r','R': stack[stackpointer-1].Data := '13';
    't','T': stack[stackpointer-1].Data := '9';
    '-','[','\',']','^': stack[stackpointer-1].Data := IntToStr(Ord(Rightstr(stack[stackpointer-1].Data,1)[1]));
  end; // case
  stack[stackpointer-1].Sym := SYM_CHARACTER;
  checkproduction := True;
end;

procedure CheckState;
begin
  if checkproduction then
    begin
      checkproduction := False;
      LALRstate := StackTop.State;
    end
  else
    Push(LALRState,Peek);
  case LALRstate of
    0: case StackTop.Sym of
          SYM_LBRACKET: ParserShift(1);
          SYM_EMPTYSET: ParserShift(2);
          SYM_GRAMMAR:  ParserGoto(3);
          else ErrorUnexpected;
       end; // Case 0
    1: case StackTop.Sym of
         SYM_CARET:          ParserShift(4);
         SYM_CHARESCAPED:    ParserShift(5);
         SYM_CHARHEX:        ParserShift(6);
         SYM_CHARNONESCAPED: ParserShift(7);
         SYM_CHARACTER:      ParserGoto(8);
         SYM_LITERALDEF:     ParserGoto(9);
         SYM_LITERALDEFS:    ParserGoto(10);
         else ErrorUnexpected;
       end; // Case 1
    2: case StackTop.Sym of
         SYM_EOF: ParserReduce2;
         else ErrorUnexpected;
       end; // Case 2
    3: case StackTop.Sym of
         SYM_EOF: ParserAccept(0);
         else ErrorUnexpected;
       end; // Case 3
    4: case StackTop.Sym of
         SYM_CHARESCAPED:    ParserShift(5);
         SYM_CHARHEX:        ParserShift(6);
         SYM_CHARNONESCAPED: ParserShift(7);
         SYM_CHARACTER:      ParserGoto(8);
         SYM_LITERALDEF:     ParserGoto(9);
         SYM_LITERALDEFS:    ParserGoto(11);
         else ErrorUnexpected;
       end; // Case 4
    5: case StackTop.Sym of
         SYM_HYPHEN:         ParserReduce9;
         SYM_RBRACKET:       ParserReduce9;
         SYM_CHARESCAPED:    ParserReduce9;
         SYM_CHARHEX:        ParserReduce9;
         SYM_CHARNONESCAPED: ParserReduce9;
         else ErrorUnexpected;
       end; // Case 5
    6: case StackTop.Sym of
         SYM_HYPHEN:         ParserReduce8;
         SYM_RBRACKET:       ParserReduce8;
         SYM_CHARESCAPED:    ParserReduce8;
         SYM_CHARHEX:        ParserReduce8;
         SYM_CHARNONESCAPED: ParserReduce8;
         else ErrorUnexpected;
       end; // Case 6
    7: case StackTop.Sym of
         SYM_HYPHEN:         ParserReduce7;
         SYM_RBRACKET:       ParserReduce7;
         SYM_CHARESCAPED:    ParserReduce7;
         SYM_CHARHEX:        ParserReduce7;
         SYM_CHARNONESCAPED: ParserReduce7;
         else ErrorUnexpected;
       end; // Case 7
    8: case StackTop.Sym of
         SYM_HYPHEN:         ParserShift(12);
         SYM_RBRACKET:       ParserReduce6;
         SYM_CHARESCAPED:    ParserReduce6;
         SYM_CHARHEX:        ParserReduce6;
         SYM_CHARNONESCAPED: ParserReduce6;
         else ErrorUnexpected;
       end; // Case 8
    9: case StackTop.Sym of
         SYM_RBRACKET:       ParserReduce4;
         SYM_CHARESCAPED:    ParserReduce4;
         SYM_CHARHEX:        ParserReduce4;
         SYM_CHARNONESCAPED: ParserReduce4;
         else ErrorUnexpected;
       end; // Case 9
    10: case StackTop.Sym of
          SYM_RBRACKET:       ParserShift(13);
          SYM_CHARESCAPED:    ParserShift(5);
          SYM_CHARHEX:        ParserShift(6);
          SYM_CHARNONESCAPED: ParserShift(7);
          SYM_CHARACTER:      ParserGoto(8);
          SYM_LITERALDEF:     ParserGoto(14);
          else ErrorUnexpected;
        end; // Case 10
    11: case StackTop.Sym of
          SYM_RBRACKET:       ParserShift(15);
          SYM_CHARESCAPED:    ParserShift(5);
          SYM_CHARHEX:        ParserShift(6);
          SYM_CHARNONESCAPED: ParserShift(7);
          SYM_CHARACTER:      ParserGoto(8);
          SYM_LITERALDEF:     ParserGoto(14);
          else ErrorUnexpected;
        end; // Case 11
    12: case StackTop.Sym of
          SYM_CHARESCAPED:    ParserShift(5);
          SYM_CHARHEX:        ParserShift(6);
          SYM_CHARNONESCAPED: ParserShift(7);
          SYM_CHARACTER:      ParserGoto(16);
          else ErrorUnexpected;
        end; // Case 12
    13: case StackTop.Sym of
          SYM_EOF: ParserReduce1;
          else ErrorUnexpected;
        end; // Case 13
    14: case StackTop.Sym of
          SYM_RBRACKET:       ParserReduce3;
          SYM_CHARESCAPED:    ParserReduce3;
          SYM_CHARHEX:        ParserReduce3;
          SYM_CHARNONESCAPED: ParserReduce3;
          else ErrorUnexpected;
        end; // Case 14
    15: case StackTop.Sym of
          SYM_EOF: ParserReduce0;
          else ErrorUnexpected;
        end; // Case 15
    16: case StackTop.Sym of
          SYM_RBRACKET:       ParserReduce5;
          SYM_CHARESCAPED:    ParserReduce5;
          SYM_CHARHEX:        ParserReduce5;
          SYM_CHARNONESCAPED: ParserReduce5;
          else ErrorUnexpected;
        end; // Case 16
  end;
end;


procedure Parse;
begin
  repeat
    CheckState;
  until ParserAccepted;
end;

procedure MakeSetLiteral(cs: TSet32; const s: string);
begin
  strm := TStringStream.Create(s);
  try
    tok.Sym         := -1;
    tok.Data        := '';
    DFAstate        := 0;
    LALRstate       := 0;
    ParserAccepted  := False;
    checkproduction := False;
    charset         := cs;
    Parse;
  finally
    strm.Free;
  end;
end;

end.

