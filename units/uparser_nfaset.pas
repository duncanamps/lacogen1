unit uparser_nfaset;

{$mode objfpc}{$H+}


//
// Generic sorted list of unique items
//

interface

uses
  Classes, SysUtils, uparser_types;

type
  TNFASet = class(TObject)
    protected
      FArraySize: integer;  // Maximum number of elements available in the array
      FChecksum:  DWORD;    // Checksum for matching purposes
      FCount:     integer;  // Current number of elements in the array
      FDirty:     boolean;  // Dirty if the checksum may be compromised
      FUnique:    boolean;  // True if we want a unique set
      procedure CalculateChecksum;
      function  GetChecksum: DWORD;
      procedure SetArraySize(_value: integer);
      procedure SetCount(_value: integer);
      property ArraySize: integer read FArraySize write SetArraySize;
    public
      Items: array of TNFAIdentifier;
      constructor Create(Unique: boolean = False);
      procedure Add(_value: TNFAIdentifier);
      function  AsString: string;
      procedure Clear;
      procedure Insert(_value: TNFAIdentifier; _index: integer);
      function  Find(_value: TNFAIdentifier; var _index: integer): boolean;
      function  Matches(another: TNFASet): boolean;
      procedure SetAdd(another: TNFASet);
      property Count: integer read FCount write SetCount;
      property Checksum: DWORD read GetChecksum;
  end;

implementation

uses
  uparser_exception;

constructor TNFASet.Create(Unique: boolean);
begin
  FCount := 0;
  FDirty := True;
  FUnique := Unique;
  SetArraySize(4);
end;

procedure TNFASet.Add(_value: TNFAIdentifier);
var index: integer;
begin
  index := 0; // Keep compiler quiet
  if Find(_value,index) then
    if FUnique then
      raise Exception.Create('Attempt to add non-unique value');
  if (Count > 0) and (_value > Items[index]) then
    Inc(Index);
  Insert(_value,index);
end;

function TNFASet.AsString: string;
var i: integer;
begin
  result := '';
  for i := 0 to Count-1 do
    begin
      if i > 0 then
        result := result + ',';
      result := result + IntToStr(Items[i]);
    end;
end;

procedure TNFASet.CalculateChecksum;
var i: integer;
    checkfrag: DWORD;
begin
  FChecksum := FCount;
  for i := 0 to FCount-1 do
    begin
      checkfrag := (FChecksum and $f8000000) shr 27;
      FCheckSum := ((FChecksum shl 5) or checkfrag) xor Items[i];
    end;
  FDirty := False;
end;

procedure TNFASet.Clear;
begin
  Count := 0;
  FDirty := True;
end;

function TNFASet.Find(_value: TNFAIdentifier; var _index: integer): boolean;
var l: integer;
    r: integer;
begin
  _index := 0;
  l := 0;
  r := FCount-1;
  while l <= r do
    begin
      _index := l + (r - l) div 2;
      if Items[_index] = _value then
        Exit(True);
      if Items[_index] < _value then
        l := _index + 1
      else
        r := _index - 1;
    end;
  Exit(False);
end;

function TNFASet.GetChecksum: DWORD;
begin
  if FDirty then
    CalculateChecksum;
  GetChecksum := FChecksum;
end;

procedure TNFASet.Insert(_value: TNFAIdentifier; _index: integer);
var i: integer;
begin
  FDirty := True;
  Count := Count + 1;
  for i := Count-2 downto _index do
    Items[i+1] := Items[i];
  Items[_index] := _value;
end;

function TNFASet.Matches(another: TNFASet): boolean;
var i: integer;
begin
  if Count <> another.Count then
    Exit(False); // Lists are not the same size
  if Checksum <> another.Checksum then
    Exit(False); // Lists are not the same checksum
  for i := 0 to Count-1 do
    if Items[i] <> another.Items[i] then
      Exit(False);
  Exit(True);
end;

procedure TNFASet.SetAdd(another: TNFASet);
var i: integer;
    index: integer;
begin
  FDirty := True;
  index := -1; // Keep the compiler quiet
  for i := 0 to another.Count-1 do
    if not Find(another.Items[i],index) then
      Add(another.Items[i]);
end;

procedure TNFASet.SetArraySize(_value: integer);
begin
  if _value = 0 then
    raise Exception.Create('Cannot set array size to zero');
  SetLength(Items,_value);
  FArraySize := _Value;
  if FCount > FArraySize then
    FCount := FArraySize;
end;

procedure TNFASet.SetCount(_value: integer);
begin
  if FArraySize < _value then
    ArraySize := FArraySize * 2;
  FCount := _value;
end;

end.

