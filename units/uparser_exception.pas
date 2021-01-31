unit uparser_exception;

{$mode objfpc}{$H+}

//
// Deals with exception and logging
//

interface

uses
  Classes, SysUtils, {uparser_deployment,} deployment_parser_types;

type

  TMonitorProc = procedure(_lt: TLCGLogType; _elapsed: TDateTime; const _msg: string; const _context: string; _line, _col: integer) of object;

const
  LogTitles: array[TLCGLogType] of string = ('Fatal',
                                             'Error',
                                             'Warning',
                                             'Info',
                                             'Info',
                                             'Info',
                                             'Debug');

implementation

end.

