{ EN13906-1 Helical Compression Spring Designer

  Copyright (C) 2022-2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit Setting;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Inifiles, SysUtils;

var
  ClientFile:  TIniFile = nil;
  PrinterFile: TIniFile = nil;

procedure OpeSettingFile;
procedure CloseSettingFile;

implementation

procedure OpeSettingFile;
begin
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DecimalSeparator  := '.';
  ClientFile  := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'client.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);
  PrinterFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'printer.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);
end;

procedure CloseSettingFile;
begin
  ClientFile.Destroy;
  PrinterFile.Destroy
end;

end.

