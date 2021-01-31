unit uset32;

{$mode objfpc}{$H+}
{$UNDEF HASH_COLLISION_CHECKS}

//------------------------------------------------------------------------------
//
//  uset32.pas  32 bit sets
//
//  Data types used:
//
//    TSet32Item - the basic cardinal element used in a set. Currently this is
//        defined as uint32 which gives sets with a limit of 4 billion different
//        items
//
//    TSet32Array - A dynamic array of TSet32Item. Used to move set data around
//        and provide information for reporting
//
//    TSet32StorageElement - The natural word size of the machine. This is used
//        to store the bit elements used for the set
//
//    TSet32 - The set itself. A collection of bits which represent set
//        membership of one of the dictionary items recorded by
//        TDictionary32Items
//
//    TDictionary32Items - An entity which contains the comlete dictionary.
//                         Each set will contain a subset of these items
//
//    TDictionary32 - The dictionary itself. It serves to own the list of
//        dictionary items as well as a list of all the sets
//
//    TSet32HashRecord - An entry in the hash table
//
//    TSet32HashTable - A hash table to look up into the dictionary
//
//  The dictionary must be populated completely before any sets can be created
//  or used. Once the sets start being used, the dictionary can no longer be
//  modified. There are helper routines to create the dictionary from a text
//  file, which may contain UTF-8 character encoding.
//
//  Duncan Munro
//  09/04/2020
//
//------------------------------------------------------------------------------


interface

uses
  Classes, SysUtils, fgl;

const

  // Constants to define bit masks and other stuff for sets
  {$IFDEF CPU16}
    {$DEFINE WORDCONFIG}
      SetFlipMask    = WORD($FFFF);
      SetIndexMask   = WORD($000F);
      SetIndexShift  = 4;
      SetWordBits    = 16;
  {$ENDIF}

  {$IFDEF CPU32}
    {$DEFINE WORDCONFIG}
      SetFlipMask    = DWORD($FFFFFFFF);
      SetIndexMask   = DWORD($0000001F);
      SetIndexShift  = 5;
      SetWordBits    = 32;
  {$ENDIF}

  {$IFDEF CPU64}
    {$DEFINE WORDCONFIG}
      SetFlipMask    = QWORD($FFFFFFFFFFFFFFFF);
      SetIndexMask   = QWORD($000000000000003F);
      SetIndexShift  = 6;
      SetWordBits    = 64;
  {$ENDIF}

  {$IFNDEF WORDCONFIG}
    {$ERROR CPU type not catered for}
  {$ENDIF}


type

  // The basic set item is a 32 bit unsigned integer
  TSet32Item = uint32;

  TSet32Array = array of TSet32Item;

  // Hash table is used to speed up finds in the dictionary
  TSet32HashRecord = record
    Used:  boolean;
    Key:   TSet32Item;
    Index: integer;
  end;

  // Hash table
  TSet32HashTable = class(TObject)
    private
      FCapacity:  integer;
      FCount:     integer;
      FHashTable: array of TSet32HashRecord;
{$IFDEF HASH_COLLISION_CHECKS}
      FStatBumps:    integer;
      FStatSearches: integer;
{$ENDIF}
    public
      constructor Create;
      destructor Destroy; override;
      procedure Add(_key: TSet32Item; _index: integer);
      procedure Clear;
      function  Find(_key: TSet32Item): integer;
      procedure Grow;
      function  HashValue(_key: TSet32Item): integer;
      property Capacity: integer read FCapacity;
      property Count:integer     read FCount;
{$IFDEF HASH_COLLISION_CHECKS}
      property StatBumps:    integer read FStatBumps;
      property StatSearches: integer read FStatSearches;
{$ENDIF}
  end;

  // TSet32StorageElement is the data type used to store the bitmaps that make up
  // the set
{$IFDEF CPU16} TSet32StorageElement = WORD; {$ENDIF}
{$IFDEF CPU32} TSet32StorageElement = DWORD; {$ENDIF}
{$IFDEF CPU64} TSet32StorageElement = QWORD; {$ENDIF}

  TDict32ResizeProc = procedure (_size: integer) of object;

  // Preliminary declaration of TSet32 class, it's defined in full later on
  TSet32 = class;

  // Main dictionary object. This contains the list of dictionary items and
  // the list of sets.
  TDictionary32 = class(specialize TFPGObjectList<TSet32>)
    private
      FDictCapacity: integer;
      FDictCount:    integer;
      FDictHash:     TSet32HashTable;
      FDictItems:    array of TSet32Item; // Could be out of order, G H J A B C etc.
      FTempIndex:  integer;  // Ascending index used for allocating temporary sets
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddElement(_item: TSet32Item);
      function  AsList: TSet32Array;
      function  AsListSorted: TSet32Array;
      function  AssignSet(const _name: string; _temp: boolean = False): TSet32;
      function  AssignTemporarySet: TSet32;
      function  BitIndex(_item: TSet32Item): integer;
      procedure Delete(const _name: string);
      procedure Delete(_obj: TSet32);
      procedure DeleteTemporary;
      procedure DictAddElement(_element: TSet32Item);
      procedure DictClear;
      function  DictFind(_element: TSet32Item; var _index: integer): boolean;
      procedure DictSetCapacity(_capacity: integer);
      procedure DictResize(_size: integer);
      procedure Dump(_strm: TStream);
      function  FindByName(const _name: string): TSet32;
      function  FindMustByName(const _name: string): TSet32;
      function  IndexOfByName(const _name: string): integer;
      function  ListAsText(_arr: TSet32Array): string;
      procedure PopulateFromArray(_array: array of TSet32Item);
      procedure PopulateFromRange(_lo, _hi: TSet32Item);
      procedure PopulateFromUTF8file(const _filename: string);
      procedure PopulateFromUTF8stream(_strm: TStream);
      property DictCapacity: integer           read FDictCapacity write DictSetCapacity;
      property DictCount:    integer           read FDictCount    write FDictCount;
  end;

  // The set itself. A collection of bits organised as machine words. For
  // example, on a 64 bit host this will used 64 bit QWORDs to make up the
  // set. For 32 bit, it will be DWORDs and so on.
  TSet32 = class(TObject)
    private
      FName:       string;
      FParent:     TDictionary32;
      FPredefined: boolean;
      FSetArray:   array of TSet32StorageElement;
      FTemporary:  boolean;
      FUsed:       boolean;
      FWords:      integer;
      function WordOffset(_index: integer): integer;           inline;
      function BitMask(_index: integer): TSet32StorageElement; inline;
    public
      constructor Create(const _name: string; _parent: TDictionary32; _elements: integer; _temp: boolean = False);
      procedure AddElement(_item: TSet32Item);         inline;
      function  AsList: TSet32Array;
      function  AsListSorted: TSet32Array;
      function  AsText: string;
      function  CalculateSetSize: integer;             inline;
      procedure Clear;
      function  Contains(_item: TSet32Item): boolean;  inline;
      procedure DifferenceWith(_other: TSet32);        inline;
      procedure Dump(_strm: TStream);
      function  Empty: boolean;                        inline;
      procedure IntersectWith(_other: TSet32);         inline;
      procedure Resize(_size: integer);
      procedure SetLiteral(const s: string);
      procedure SubtractElement(_item: TSet32Item);    inline;
      procedure UnionWith(_other: TSet32);             inline;
      property Name:       string  read FName;
      property Predefined: boolean read FPredefined;
      property Temporary:  boolean read FTemporary;
      property Used:       boolean read FUsed write FUsed;
  end;

function Dictionary32Compare(const a,b: TSet32): integer;

implementation

uses
  lacogen10_setliteral_procs, uparser_exception, uchar32, uparser_utility;

const
  // Hash table constants
  HASH_TABLE_INITIAL_SIZE = 1021; // Initial hash table size, NOTE: prime number
  HASH_TABLE_MARGIN = 2;          // Always have count * margin <= capacity
  HASH_TABLE_GROWTH = 4;          // Multiply factor for hash table if we resize
  DICT_ITEMS_INITIAL_SIZE = SetWordBits;  // Initial size of dictionary list
  DICT_ITEMS_GROWTH       = 2;    // Multiply factor for dict list if we resize

  BlankHash: TSet32HashRecord = (
      Used:  False;
      Key:   0;
      Index: 0;
    );

var
  tmpi: integer; // Temporary iterator to create bit masks
  tmpmask: TSet32StorageElement;
  set_bit_masks: array[0..SetWordBits-1] of TSet32StorageElement;


//------------------------------------------------------------------------------
//
//  Helper routines
//
//------------------------------------------------------------------------------

function Dictionary32Compare(const a,b: TSet32): integer;
begin
  if a.Name > b.Name then
    Result := 1
  else if a.Name < b.Name then
    Result := -1
  else
    Result := 0;
end;

function IsPrime(_value: integer): boolean;
var divisor: integer;
begin
  // Returns True if a number is prime, false if not
  // Get rid of easy ones first
  if _value < 4 then
    Exit(False);
  if (_value mod 2) = 0 then
    Exit(False);
  // Now loop through
  divisor := 3;
  while (divisor*divisor) < _value do
    begin
      if (_value mod divisor) = 0 then
        Exit(False);
      Inc(divisor,2);
    end;
  // If we got this far, it must be prime
  Exit(True);
end;

function NextPrime(_value: integer): integer;
begin
  // Returns the next prime number >= _value
  // First check if this is an odd number; if not, make it so
  if (_value mod 2) = 0 then
    Inc(_value);
  while not IsPrime(_value) do
    Inc(_value,2);
  Result := _value;
end;


procedure MyQuickSort(_arr: TSet32Array; lo, hi: integer);
var tmp: TSet32Item;
    p:   integer;

  procedure Swap(a,b: integer);
  begin
    tmp := _arr[a];
    _arr[a] := _arr[b];
    _arr[b] := tmp;
  end;

  function Partition(low,high: integer): integer;
  var pivot: TSet32Item;
      i,j:   integer;
  begin
    pivot := _arr[high];
    i := low;
    for j := low to high do
      if _arr[j] < pivot then
        begin
          Swap(i,j);
          Inc(i);
        end;
    Swap(i,high);
    Result := i;
  end;

begin
  if (lo < hi) then
    begin
      p := Partition(lo,hi);
      MyQuickSort(_arr,lo,p-1);
      MyQuickSort(_arr,p+1,hi);
    end;
end;


//------------------------------------------------------------------------------
//
//  TSet32HashTable
//
//------------------------------------------------------------------------------

constructor TSet32HashTable.Create;
begin
  inherited Create;
{$IFDEF HASH_COLLISION_CHECKS}
  FStatSearches := 0;
  FStatBumps := 0;
{$ENDIF}
  FCapacity := HASH_TABLE_INITIAL_SIZE;
  SetLength(FHashTable,FCapacity);
  Clear;
end;

destructor TSet32HashTable.Destroy;
begin
  inherited Destroy;
end;

procedure TSet32HashTable.Add(_key: TSet32Item; _index: integer);
var ix: integer;
    hash: integer;
begin
  // Try and find the item first
  ix := Find(_key);
  if ix >= 0 then
    begin
      if FHashTable[ix].Index <> _index then
        raise Exception.Create(Format('Attempting to add key/index %d/%d to ' +
            'the hash table, already contains key with index %d',
            [_key,_index,FHashTable[ix].Index]));
    end
  else
    begin  // Item doesn't exist, we are free to add
      // See if the table needs a resize before this add
      if (FCount+1) * HASH_TABLE_MARGIN >= FCapacity then
        Grow;
      // Add the record
      hash := HashValue(_key);
{$IFDEF HASH_COLLISION_CHECKS}
      Inc(FStatSearches);
{$ENDIF}
      while FHashTable[hash].Used do
        begin
          hash := (hash + 1) mod FCapacity;
{$IFDEF HASH_COLLISION_CHECKS}
          Inc(FStatBumps);
{$ENDIF}
        end;
      FHashTable[hash].Used  := True;
      FHashTable[hash].Key   := _key;
      FHashTable[hash].Index := _index;
      Inc(FCount);
    end;
end;

procedure TSet32HashTable.Clear;
var i: integer;
begin
  for i := 0 to FCapacity-1 do
    FHashTable[i] := BlankHash;
  FCount := 0;
end;

function TSet32HashTable.Find(_key: TSet32Item): integer;
var hash:integer;
begin
  Result := -1;
  // Turn a key into an index. Return -1 if not found
  hash := HashValue(_key);
  while (FHashTable[hash].Used) and (FHashTable[hash].Key <> _key) do
    hash := (hash + 1) mod FCapacity;
  if (FHashTable[hash].Used) and (FHashTable[hash].Key = _key) then
    Result := FHashTable[hash].Index;
end;

procedure TSet32HashTable.Grow;
var i,j: integer;
    tmp: array of TSet32HashRecord;
begin
  // First make a temporary copy of the data
  SetLength(tmp,Fcount);
  j := 0;
  for i := 0 to FCapacity-1 do
    if FHashTable[i].Used then
      begin
        tmp[j] := FHashTable[i];
        Inc(j);
        if (j > FCount) then
          raise Exception.Create('Number of used items in hash table exceeds registered count');
      end;
  // Grow the table and reset
  FCapacity := NextPrime(FCapacity * HASH_TABLE_GROWTH);
  SetLength(FHashTable,FCapacity);
  Clear;
  // Finally put all the temporary items back into the table
  j := Length(tmp);
  for i := 0 to j-1 do
    Add(tmp[i].Key,tmp[i].Index);
end;

function TSet32HashTable.HashValue(_key: TSet32Item): integer;
begin
  _key := _key * 65521; // Spread the key out a bit
  Result := _key mod TSet32Item(FCapacity);
end;

//------------------------------------------------------------------------------
//
//  TSet32
//
//------------------------------------------------------------------------------

constructor TSet32.Create(const _name: string; _parent: TDictionary32; _elements: integer; _temp: boolean);
begin
  // Construct the set. _parent is the dictionary which owns this item and
  // _elements is the number of elements in the dictionary
  inherited Create;
  FName      := _name;
  FParent    := _parent;
  FTemporary := _temp;
  Resize(_elements);
//  Clear;  // Ensure the set is empty
end;

function TSet32.BitMask(_index: integer): TSet32StorageElement;
begin
  Result := set_bit_masks[_index and SetIndexMask];
end;

procedure TSet32.AddElement(_item: TSet32Item);
var index: integer;
begin
  index := FParent.BitIndex(_item);
  FSetArray[WordOffset(index)] := FSetArray[WordOffset(index)] or BitMask(index);
end;

function TSet32.AsList: TSet32Array;
var i: integer;
    j: integer;
begin
  SetLength(Result,CalculateSetSize);
  j := 0;
  for i := 0 to FParent.DictCount-1 do
    if (FSetArray[WordOffset(i)] and BitMask(i)) <> 0 then
      begin
        Result[j] := FParent.FDictItems[i];
        Inc(j);
      end;
end;

function TSet32.AsListSorted: TSet32Array;
begin
  Result := AsList;
  MyQuickSort(Result,0,Length(Result)-1);
end;

function TSet32.AsText: string;
begin
  Result := FParent.ListAsText(AsListSorted);
end;

function TSet32.CalculateSetSize: integer;
var i: integer;
begin
  Result := 0;
  for i := 0 to FParent.DictCount-1 do
    if (FSetArray[WordOffset(i)] and BitMask(i)) <> 0 then
      Inc(Result);
end;

procedure TSet32.Clear;
var i: integer;
begin
  for i := 0 to FWords-1 do
    FSetArray[i] := 0;
end;

function TSet32.Contains(_item: TSet32Item): boolean;
var index: integer;
begin
  index := FParent.BitIndex(_item);
  Result := (FSetArray[WordOffset(index)] and BitMask(index)) <> 0;
end;

procedure TSet32.DifferenceWith(_other: TSet32);
var i: integer;
begin
  for i := 0 to FWords-1 do
    FSetArray[i] := FSetArray[i] and (_other.FSetArray[i] xor SetFlipMask);
end;

procedure TSet32.Dump(_strm: TStream);
var s: string;
begin
  if FPredefined then
    s := '. ' + FName + ' = '
  else
    s := FName + ' = ';
  if not Used then
    s := '*UNUSED* ' + s;
  s := s + AsText;
  WriteLnStringToStream(_strm,s);
end;

function TSet32.Empty: boolean;
var i: integer;
begin
  Result := True;
  for i := 0 to FWords-1 do
    if FSetArray[i] <> 0 then
      Exit(False);
end;

procedure TSet32.IntersectWith(_other: TSet32);
var i: integer;
begin
  for i := 0 to FWords-1 do
    FSetArray[i] := FSetArray[i] and _other.FSetArray[i];
end;

procedure TSet32.Resize(_size: integer);
begin
  FWords := (_size + SetWordBits - 1) div SetWordBits;
  SetLength(FSetArray,FWords);
end;

procedure TSet32.SetLiteral(const s: string);
begin
  LCG_MakeSetLiteral(Self,s);
end;

procedure TSet32.SubtractElement(_item: TSet32Item);
var index: integer;
begin
  index := FParent.BitIndex(_item);
  FSetArray[WordOffset(index)] := FSetArray[WordOffset(index)] and (BitMask(index) xor SetFlipMask);
end;

procedure TSet32.UnionWith(_other: TSet32);
var i: integer;
begin
  for i := 0 to FWords-1 do
    FSetArray[i] := FSetArray[i] or _other.FSetArray[i];
end;

function TSet32.WordOffset(_index: integer): integer;
begin
  Result := _index shr SetIndexShift;
end;

//------------------------------------------------------------------------------
//
//  TDictionary32
//
//------------------------------------------------------------------------------

constructor TDictionary32.Create;
begin
  inherited Create;
  FDictHash := TSet32HashTable.Create;
  DictClear;
  FTempIndex := 0;
end;

destructor TDictionary32.Destroy;
begin
  FDictHash.Free;
  inherited Destroy;
end;

procedure TDictionary32.AddElement(_item: TSet32Item);
begin
  // Add a single element to a dictionary
  // Takes care of expanding the set sizes if required
  DictAddElement(_item);
end;

function TDictionary32.AsList: TSet32Array;
var i: integer;
begin
  SetLength(Result,DictCount);
  for i := 0 to DictCount-1 do
    Result[i] := FDictItems[i];
end;

function TDictionary32.AsListSorted: TSet32Array;
begin
  Result := AsList;
  MyQuickSort(Result,0,Length(Result)-1);
end;

function TDictionary32.AssignSet(const _name: string; _temp: boolean): TSet32;
begin
  Result := TSet32.Create(_name,Self,DictCapacity,_temp);
  Add(Result);
end;

function TDictionary32.AssignTemporarySet: TSet32;
begin
  Result := AssignSet(Format('__TMPSET_%8.8X__',[FTempIndex]),True);
  Inc(FTempIndex);
end;

function TDictionary32.BitIndex(_item: TSet32Item): integer;
begin
  Result := -1;
  if not DictFind(_item,Result) then
    // Add the item to the dictionary
    begin
      DictAddElement(_item);
      if not DictFind(_item,Result) then
        raise Exception.Create(Format('Attempt to find bit index of item %u not in dictionary',[_item]));
    end;
end;

procedure TDictionary32.Delete(const _name: string);
var index: integer;
begin
  index := IndexOfByName(_name);
  if index < 0 then
    raise Exception.Create('Attempt to delete set ' + _name + ' which does not exist');
  inherited Delete(index);
end;

procedure TDictionary32.Delete(_obj: TSet32);
var index: integer;
begin
  index := IndexOf(_obj);
  if index < 0 then
    raise Exception.Create('Attempt to delete set ' + _obj.Name + ' which does not exist');
  inherited Delete(index);
end;

procedure TDictionary32.DeleteTemporary;
var index: integer;
begin
  for index := Count-1 downto 0 do
    if Items[index].Temporary then
      inherited Delete(index);
end;

procedure TDictionary32.DictAddElement(_element: TSet32Item);
var index: integer;
begin
  index := -1;
  if not DictFind(_element,index) then
    begin
      // Resize may be required
      if (DictCount+1) > DictCapacity then
        begin
          DictCapacity := DictCapacity * DICT_ITEMS_GROWTH;
          SetLength(FDictItems,DictCapacity);
        end;
      // Do the DictAddElement, index reflects the position
      FDictItems[DictCount] := _element;
      // DictAddElement to the hash table as well
      FDictHash.Add(_element,DictCount);
      // Finally bump the counter
      DictCount := DictCount + 1;
    end;
end;

procedure TDictionary32.DictClear;
begin
  DictCount := 0;
  DictCapacity := DICT_ITEMS_INITIAL_SIZE;
  SetLength(FDictItems,DictCapacity);
  FDictHash.Clear;
end;

function TDictionary32.DictFind(_element: TSet32Item; var _index: integer): boolean;
begin
  _index := FDictHash.Find(_element);
  Result := (_index >= 0);
end;

function TDictionary32.ListAsText(_arr: TSet32Array): string;
var s: string;
    i: integer;
    j: integer;
    ci: TChar32;
    cj: TChar32;
    range_count: integer;
begin
  s := '[';
  i := 0;
  While i <= Length(_arr)-1 do
    begin
      ci := _arr[i];
      j := i;
      while (j < Length(_arr)-1) and (_arr[j+1] = _arr[j]+1) do
        Inc(j);
      cj := _arr[j];
      range_count := cj-ci;
      if range_count >= 3 then
        begin
          s := s + uchar32.CharAsText(ci) +
               '-' +
               uchar32.CharAsText(cj);
          i := j;
        end
      else
        s := s + uchar32.CharAsText(ci);
      Inc(i);
    end;
  s := s + ']';
  Result := s;
end;

procedure TDictionary32.PopulateFromArray(_array: array of TSet32Item);
var acount: integer;
    i:      integer;
begin
  acount := Length(_array);
  for i := 0 to acount-1 do
    DictAddElement(_array[i]);
//  Sort;
end;

procedure TDictionary32.PopulateFromRange(_lo, _hi: TSet32Item);
var i: TSet32Item;
begin
  for i := _lo to _hi do
    DictAddElement(i);
//  Sort;
end;

procedure TDictionary32.PopulateFromUTF8file(const _filename: string);
var strm: TFileStream;
begin
  strm := TFileStream.Create(_filename,fmOpenRead,fmShareDenyWrite);
  try
    PopulateFromUTF8stream(strm);
  finally
    strm.Free;
  end;
end;

procedure TDictionary32.PopulateFromUTF8stream(_strm: TStream);
const STREAMBUF_SIZE = 4096;
      MINBUF_SIZE = 4;
      TOTALBUF_SIZE = STREAMBUF_SIZE+MINBUF_SIZE;
var buf:                array[0..TOTALBUF_SIZE-1] of BYTE;
    bptr:               integer;
    bcontains:          integer;
    strmbytes:          integer;
    filebytesavailable: boolean;
    ch:                 TSet32Item;
    chsize:             integer;

  function BufferContains: integer;
  begin
    Result := (bcontains-bptr);
  end;

  procedure ReadBlockIfRequired;
  var i: integer;
  begin
    if (filebytesavailable) and (BufferContains < MINBUF_SIZE) then
      begin
        if bptr > 0 then
          begin
            for i := 0 to BufferContains-1 do
              buf[i] := buf[bptr+i];
            bcontains := bcontains-bptr;
            bptr := 0
          end;
        strmbytes := _strm.Read(buf[bptr],STREAMBUF_SIZE);
        bcontains := bcontains + strmbytes;
        filebytesavailable := (strmbytes > 0);
      end;
  end;

begin
  bcontains := 0;
  bptr := 0;
  filebytesavailable := True;
  repeat
    ReadBlockIfRequired;
{%H-}if (buf[bptr] and $C0) = $C0 then
      begin  // Start of UTF8 sequence
        chsize := 1;
        if (buf[bptr] and $E0) = $C0 then
          chsize := 2
        else if (buf[bptr] and $F0) = $E0 then
          chsize := 3
        else if (buf[bptr] and $F8) = $F0 then
          chsize := 4;
        if BufferContains < chsize then
          raise Exception.Create('End of stream reached in the middle of a UTF-8 character');
        case chsize of
          1: raise Exception.Create(Format('Misformed UTF-8 character at stream position %d',[_strm.Position-BufferContains]));
          2: ch := ((buf[bptr] and $1F) shl 6)  or ( buf[bptr+1] and $3F);
          3: ch := ((buf[bptr] and $0F) shl 12) or ((buf[bptr+1] and $3F) shl 6)  or ( buf[bptr+2] and $3F);
          4: ch := ((buf[bptr] and $07) shl 18) or ((buf[bptr+1] and $3F) shl 12) or ((buf[bptr+2] and $3F) shl 6) or (buf[bptr+3] and $3F);
        end;
        DictAddElement(ch);
        bptr := bptr + chsize;
      end
    else
      begin
        DictAddElement(buf[bptr]);
        Inc(bptr);
      end;
  until BufferContains = 0;
//  Sort;
end;

procedure TDictionary32.DictSetCapacity(_capacity: integer);
begin
  FDictCapacity := _capacity;
    DictResize(FDictCapacity);
end;

procedure TDictionary32.DictResize(_size: integer);
var obj: TSet32;
begin
  // Dictionary has been resized. Need to go through any sets, if declared,
  // and resize them too
  for obj in Self do
    obj.Resize(_size);
end;

procedure TDictionary32.Dump(_strm: TStream);
var i: integer;
begin
  WriteLnStringToStreamUnderlined(_strm,'Character Sets');
  WriteLnStringToStream(_strm,'*DICTIONARY*=' + ListAsText(AsListSorted));
  for i := 0 to Count-1 do
    Items[i].Dump(_strm);
  WriteLnStringToStream(_strm,'');
end;

function TDictionary32.FindByName(const _name: string): TSet32;
var ix: integer;
begin
  Result := nil;
  ix := IndexOfByName(_name);
  if ix >= 0 then
    Result := Items[ix];
end;

function TDictionary32.FindMustByName(const _name: string): TSet32;
begin
  Result := FindByName(_name);
  if Result = nil then
    raise LCGErrorException.Create('Could not find character set ' + _name);
end;

function TDictionary32.IndexOfByName(const _name: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].FName = _name then
      Exit(i);
end;

//------------------------------------------------------------------------------
//
//  Initialization / finalization
//
//------------------------------------------------------------------------------

initialization
  tmpmask := 1;
  for tmpi := 0 to SetWordBits-1 do
    begin
      set_bit_masks[tmpi] := tmpmask;
      tmpmask := tmpmask shl 1;
    end;


end.

