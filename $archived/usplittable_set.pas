unit usplittable_set;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fgl, ucharset32, deployment_parser_types;

{
type
  TFragMatch = (fmExact, fmBothInRange,fmStartInRange,fmEndInRange,fmOutside);

  TSplittableRange = record
    First:  TChar;
    Last:   TChar;
    Used:   boolean;
    class operator = (a,b: TSplittableRange): boolean;
  end;

  TSplittableSet = class(specialize TFPGList<TSplittableRange>)
    public
      constructor Create;
      procedure DeleteUnused;
      procedure Dump(_strm: TStream);
      procedure Explode;
      procedure MarkCharUsed(_char: TChar);
      procedure MarkRangeUsed(_charfrom, _charto: TChar);
      procedure MarkUnused;
//      function  MatchFragment(_me: TSplittableRange; _frag: TCharSetFragment): TFragMatch;
      procedure SplitWithChar(_char: TChar);
//      procedure SplitWithFrag(_frag: TCharSetFragment);
      procedure SplitWithRange(_charfrom, _charto: TChar);
  end;
  }


implementation

uses
  uparser_utility;

{ Utility code }

{
class operator TSplittableRange.= (a,b: TSplittableRange): boolean;
begin
  Result := (a.First = b.First) and
            (a.Last  = b.Last);
end;
}
{ TSplittableSet }

{
constructor TSplittableSet.Create;
begin
  inherited Create;
end;

procedure TSplittableSet.DeleteUnused;
var i: integer;
begin
  for i := Count-1 downto 0 do
    if not Items[i].Used then
      Delete(i);
end;

procedure TSplittableSet.Dump(_strm: TStream);
var r: TSplittableRange;
begin
  WriteLnStringToStreamUnderlined(_strm,'FRAGMENTED CHARACTERS AND SETS');
  for r in Self do
    begin
      WriteLnStringToStream(_strm,Format('%s,%s',[MakePrintable(r.First),MakePrintable(r.Last)]));
    end;
  WriteLnStringToStream(_strm,'');
end;

procedure TSplittableSet.Explode;
// Take the set and turn short ranges into single characters for greater
// efficiency within the Lexer as it's always quicker to check a character
// than a range. The constant MIN_RANGE shows the minimum number of characters
// in a range, any less than this will be exploded into single characters
const MIN_RANGE = 3;
var i: integer;
    total: integer;
    r:     TSplittableRange;
    z:     TSplittableRange;
begin
  total := Count;
  i := 0;
  while i < total do
    begin
      r := Items[i];
      if (r.Last > r.First) and ((Ord(r.Last) - Ord(r.First) + 1) < MIN_RANGE) then
        begin // Split the first character off
          z.First := r.First;
          z.Last  := r.First;
          Insert(i,z);
          Inc(total);
          r.First := Chr(Ord(r.First) + 1);
          Items[i+1] := r;
        end;
      Inc(i);
    end;
end;

procedure TSplittableSet.MarkCharUsed(_char: TChar);
var i: integer;
    r: TSplittableRange;
begin
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      if (r.First = _char) and (r.Last = _char) then
        begin
          r.Used := True;
          Items[i] := r;
          Exit;
        end;
    end;
end;

procedure TSplittableSet.MarkRangeUsed(_charfrom, _charto: TChar);
var i: integer;
    r: TSplittableRange;
begin
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      if (r.First >= _charfrom) and (r.Last <= _charto) then
        begin
          r.Used := True;
          Items[i] := r;
        end;
    end;
end;

procedure TSplittableSet.MarkUnused;
var i: integer;
    r: TSplittableRange;
begin
  for i := 0 to Count-1 do
    begin
      r := Items[i];
      r.Used := False;
      Items[i] := r;
    end;
end;

{
function TSplittableSet.MatchFragment(_me: TSplittableRange; _frag: TCharSetFragment): TFragMatch;
begin
  if (_frag.First = _me.First) and (_frag.Last = _me.Last) then
    Result := fmExact
  else if (_frag.First >= _me.First) and (_frag.Last <= _me.Last) then
    Result := fmBothInRange
  else if (_frag.First >= _me.First) and (_frag.First <= _me.Last) and (_frag.Last > _me.Last) then
    Result := fmStartInRange
  else if (_frag.First < _me.First) and (_frag.Last >= _me.First) and (_frag.Last <= _me.Last) then
    Result := fmEndInRange
  else
    Result := fmOutside;
end;
}

procedure TSplittableSet.SplitWithChar(_char: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _char;
  frag.Last  := _char;
  SplitWithFrag(frag);
end;

procedure TSplittableSet.SplitWithFrag(_frag: TCharSetFragment);
//
// The main splitting routine
//
var i: integer;
    match: TFragMatch;
    afrag: TSplittableRange;
    total: integer;
begin
  i := 0;
  total := Count;
  while i < total do
    begin
       match := MatchFragment(Items[i],_frag);
       case match of
         fmExact:         ; // Do nothing
         fmBothInRange:   begin
                            if (_frag.First > Items[i].First) then
                              begin
                                afrag.First := _frag.First;
                                afrag.Last  := Items[i].Last;
                                Insert(i+1,afrag);
                                Inc(total);
                                afrag := Items[i];
                                afrag.Last := Chr(Ord(_frag.First)-1);
                                Items[i] := afrag;
                              end;
                            if (_frag.Last < Items[i].Last) then
                              begin
                                afrag.First := Chr(Ord(_frag.Last) + 1);
                                afrag.Last  := Items[i].Last;
                                Insert(i+1,afrag);
                                Inc(total);
                                afrag := Items[i];
                                afrag.Last := _frag.Last;
                                Items[i] := afrag;
                              end;
                          end;
         fmStartInRange:  if (_frag.First > Items[i].First) then
                            begin
                              afrag.First := _frag.First;
                              afrag.Last  := Items[i].Last;
                              Insert(i+1,afrag);
                              Inc(total);
                              afrag := Items[i];
                              afrag.Last := Chr(Ord(_frag.First) - 1);
                              Items[i] := afrag;
                            end;
         fmEndInRange:    if (_frag.Last < Items[i].Last) then
                            begin
                              afrag.First := Chr(Ord(_frag.Last) + 1);
                              afrag.Last  := Items[i].Last;
                              Insert(i+1,afrag);
                              Inc(total);
                              afrag := Items[i];
                              afrag.Last := _frag.Last;
                              Items[i] := afrag;
                            end;
         fmOutside:       ; // Do nothing
       end; // case
       Inc(i);
    end;
end;

procedure TSplittableSet.SplitWithRange(_charfrom, _charto: TChar);
var frag: TCharSetFragment;
begin
  frag.First := _charfrom;
  frag.Last  := _charto;
  SplitWithFrag(frag);
end;
}

end.

