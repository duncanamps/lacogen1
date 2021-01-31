unit uglobal_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniPropStorage;

// Global declarations

var
  // The options
  EditorFontName:     string;
  EditorFontSize:     integer;
  EditorLastFile:     string;
  EditorLoadLastFile: boolean;
  EditorMRUCount:     integer;
  EditorTabSize:      integer;

procedure GlobalOptionsLoad(_prop: TIniPropStorage);
procedure GlobalOptionsSave(_prop: TIniPropStorage);

implementation

const
  sEditorFontName     = 'EditorFontName';
  sEditorFontSize     = 'EditorFontSize';
  sEditorLastFile     = 'EditorLastFile';
  sEditorLoadLastFile = 'EditorLoadLastFile';
  sEditorMRUCount     = 'EditorMRUCount';
  sEditorTabSize      = 'EditorTabSize';

  dEditorFontName     = '';
  dEditorFontSize     = -1;
  dEditorLastFile     = '';
  dEditorLoadLastFile = True;
  dEditorMRUCount     = 10;
  dEditorTabSize      = 4;

procedure GlobalOptionsLoad(_prop: TIniPropStorage);
begin
  _prop.Restore;
  EditorFontName     := _prop.ReadString (sEditorFontName,     dEditorFontName);
  EditorFontSize     := _prop.ReadInteger(sEditorFontSize,     dEditorFontSize);
  EditorLastFile     := _prop.ReadString (sEditorLastFile,     dEditorLastFile);
  EditorLoadLastFile := _prop.ReadBoolean(sEditorLoadLastFile, dEditorLoadLastFile);
  EditorMRUCount     := _prop.ReadInteger(sEditorMRUCount,     dEditorMRUCount);
  EditorTabSize      := _prop.ReadInteger(sEditorTabSize,      dEditorTabSize);
end;

procedure GlobalOptionsSave(_prop: TIniPropStorage);
begin
  _prop.WriteString (sEditorFontName,    EditorFontName);
  _prop.WriteInteger(sEditorFontSize,    EditorFontSize);
  _prop.WriteString (sEditorLastFile,    EditorLastFile);
  _prop.WriteBoolean(sEditorLoadLastFile,EditorLoadLastFile);
  _prop.WriteInteger(sEditorMRUCount,    EditorMRUCount);
  _prop.WriteInteger(sEditorTabSize,     EditorTabSize);
  _prop.Save;
end;

end.

