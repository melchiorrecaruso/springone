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

