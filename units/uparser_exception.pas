unit uparser_exception;

{
    LaCoGen - LAzarus COmpiler GENerator
    Copyright (C)2020-2022 Duncan Munro

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

    Contact: Duncan Munro  duncan@duncanamps.com
}

{$mode objfpc}{$H+}

//
// Deals with exception and logging
//

interface

uses
  Classes, SysUtils, {uparser_deployment,} deployment_parser_types_12;

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

