unit uparser_types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

// ===== CONSTANT DEFINITIONS =====

const

// Character set constants

// Bits are accessed by an index split into a low index (for the bit masks)
// and a high index for the machine word itself to be selected
//
// e.g. Assume 16 bit words, and we and bit 18
// We would AND the 18 with the IndexMask to give 2 - the index into the bits
// table. We would use IndexShift to shift 18 >> 4 to give 1 and that is the
// word in the zero-based word array. The 2 for the index mask is the index
// into the bit table which goes:
//
//   0000000000000001
//   0000000000000010
//   0000000000000100 etc.
//
// It's not defined here. WordCount8 and WordCount16 define the number of n bit
// words used to make up a whole set for 8 bit and 16 bit characters
// respectively. CS_8BIT and CS_16BIT later on map one of these onto WordCount.
//

{$IFDEF CPU16}
  {$DEFINE WORDCONFIG}
    IndexMask   = WORD($0000000F);
    IndexShift  = 4;
    WordBits    = 16;
    WordCount8  = 16;
    WordCount16 = 4096;
    WordMask    = WORD($000000000000FFFF);
{$ENDIF}

{$IFDEF CPU32}
  {$DEFINE WORDCONFIG}
    IndexMask   = DWORD($0000001F);
    IndexShift  = 5;
    WordBits    = 32;
    WordCount8  = 8;
    WordCount16 = 2048;
    WordMask    = DWORD($00000000FFFFFFFF);
{$ENDIF}

{$IFDEF CPU64}
  {$DEFINE WORDCONFIG}
    IndexMask   = QWORD($0000003F);
    IndexShift  = 6;
    WordBits    = 64;
    WordCount8  = 4;
    WordCount16 = 1024;
    WordMask    = QWORD($FFFFFFFFFFFFFFFF);
{$ENDIF}

{$IFNDEF WORDCONFIG}
  {$ERROR CPU type not catered for}
{$ENDIF}

// ===== TYPE DEFINITIONS =====

type

  // NFA types

  TActionType      = (atCharacter{,atCharRange},atEpsilon);
//  TCharSet         = set of char;
  TNFAIdentifier   = uint32; // WORD=65535 NFA states, DWORD=4294967295 states
  TStateIdentifier = uint32;
  TTokenIdentifier = uint32;
  TRuleIdentifier  = uint32;

  // DFA types

  TDFAFileHeader = record
    signature: UINT32;
    tokensize: UINT32;
    statesize: UINT32;
    charsize:  UINT32;
    records:   UINT32;
    checkval:  UINT32;
  end;

// Further constants based on type definitions above

const
  NO_STATE_IDENTIFIER = $7FFFFFFF;
  NO_TOKEN_IDENTIFIER = $7FFFFFFF;
  ACCEPT_CONDITION = '<$accept>';
  DIGITS_LINE      = 5;
  DIGITS_COLUMN    = 3;


implementation

end.

