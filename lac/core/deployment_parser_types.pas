unit deployment_parser_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;


const

  // Some defaults

  LEXBUF_BLOCK_SIZE_DEFAULT = 4096;      // Size of each block in bytes to read into the lexer
  LEXBUF_MIN                = 64;        // Minimum number of characters to keep in lexer buffer for comment lookahead
  PARSER_STACK_SIZE_DEFAULT = 100;       // Default, the parser stack size will grow if needed
  PARSER_STACK_SIZE_MAX     = 800;       // Maximum
  PREDEFINED_EMPTY_STATE    = $7FFFFFFF;
  PREDEFINED_EMPTY_TOKEN    = $7FFFFFFF;
  PREDEFINED_TOKEN_ERROR    = 0;
  PREDEFINED_TOKEN_EOF      = 1;
  PREDEFINED_TOKEN_COMMENT  = 2;
  TOKEN_BUF_SIZE_DEFAULT    = 1024;      // The maximum size of a token in characters
  UNICODE_ERROR_CHARACTER   = $00FFFD;
  UNICODE_MAXIMUM_CHARACTER = $10FFFF;
  UNICODE_MAXIMUM_CODEBYTE  = $F4;

{$IFDEF UTF8}
  STRING_START = 0;
  STRING_END   = -1;
{$ELSE}
  STRING_START = 1;
  STRING_END   = 0;
{$ENDIF}


type

  // Error handling stuff


  TLCGLogType = (ltInternal,ltError,ltWarning,ltInfo,ltVerbose,ltWarAndPeace,ltDebug);

  LCGErrorException = class(Exception);      // Exception for trapped errors
  LCGInternalException = class(Exception);   // Exception for internal errors
                                             // which shouldn't happen...


  // Some data types

{$IFDEF LCG32}
  TChar   = UINT32;
  TCharN  = UINT32;
  TString = array of UINT32;
{$ENDIF}
{$IFDEF LCG16}
  TChar   = widechar;
  TCharN  = UINT16;
  TString = widestring;
{$ENDIF}
{$IFDEF LCG8}
  TChar   = char;
  TCharN  = UINT8;
  TString = AnsiString;
{$ENDIF}

  TLCGStateIdentifier = UINT32;
  TLCGTokenIdentifier = UINT32;
  TStringArray = array of TString;

  TLCGLexerMode = (lmStart,lmOperating,lmFiledone,lmEOF);

  TLCGParserOutputType = (potUndefined,potError,potShift,potGoto,potReduce,potAccept);


  // Lexer info record, use this to pass stuff back

  TLCGLexerInfo = record
    Row:      integer;
    Col:      integer;
    Overflow: boolean;
    Next:     char;     // Next character in the queue
    Buf:      TString;   // Buffer which caused error
  end;

  // Stack items

  TLCGParserStackEntry = record
      State:     TLCGStateIdentifier;
      Token:	 TLCGTokenIdentifier;
      Buf:       TString;
    end;

  TLCGParserStack = array of TLCGParserStackEntry;

  // Calling routine

implementation

end.

