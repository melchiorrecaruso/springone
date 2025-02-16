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

unit ApplicationFrm3;

{$mode objfpc}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, Forms, ExtCtrls,
  Graphics, IniFiles, LibLink, Menus,  Spin, StdCtrls, SysUtils;

type

  { TApplicationForm3 }

  TApplicationForm3 = class(TForm)
    Bevel: TBevel;
    CycleFrequencyLabel: TLabel;
    CycleFrequency: TFloatSpinEdit;
    CycleFrequencyUnit: TComboBox;
    LoadTypeLabel: TLabel;
    LoadType: TComboBox;
    StressDirectionLabel: TLabel;
    StressDirection: TComboBox;
    TemperatureLabel: TLabel;
    Temperature: TFloatSpinEdit;
    TemperatureUnit: TComboBox;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;


var
  ApplicationForm3: TApplicationForm3;


implementation

{$R *.lfm}

uses
  SpringSolvers, MaterialFrm, Math, BaseUtils, Setting;

// TApplicationForm3

procedure TApplicationForm3.FormCreate(Sender: TObject);
begin
  Top    := ClientFile.ReadInteger('ApplicationForm3', 'Top',    Top);
  Left   := ClientFile.ReadInteger('ApplicationForm3', 'Left',   Left);
  Height := ClientFile.ReadInteger('ApplicationForm3', 'Height', Height);
  Width  := ClientFile.ReadInteger('ApplicationForm3', 'Width',  Width);

  Clear;
end;

procedure TApplicationForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('ApplicationForm3', 'Top',    Top);
    ClientFile.WriteInteger('ApplicationForm3', 'Left',   Left);
    ClientFile.WriteInteger('ApplicationForm3', 'Height', Height);
    ClientFile.WriteInteger('ApplicationForm3', 'Width',  Width);
  end;
end;

procedure TApplicationForm3.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TApplicationForm3.Clear;
begin
  LoadType       .ItemIndex := 0;
  Temperature    .Value     := 20;
  TemperatureUnit.ItemIndex := 0;
  StressDirection.ItemIndex := 0;
end;

procedure TApplicationForm3.Load(IniFile: TIniFile);
begin
  LoadType       .ItemIndex := IniFile.ReadInteger('TApplicationForm3', 'LoadType',        0);
  CycleFrequency .Value     := IniFile.ReadFloat  ('TApplicationForm3', 'CycleFrequency',  1);
  Temperature    .Value     := IniFile.ReadFloat  ('TApplicationForm3', 'Temperature',     0);
  TemperatureUnit.ItemIndex := IniFile.ReadInteger('TApplicationForm3', 'TemperatureUnit', 0);
  StressDirection.ItemIndex := IniFile.ReadInteger('TApplicationForm3', 'StressDirection', 0);
end;

procedure TApplicationForm3.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TApplicationForm3', 'LoadType',        LoadType       .ItemIndex);
  IniFile.WriteFloat  ('TApplicationForm3', 'CycleFrequency',  CycleFrequency .Value    );
  IniFile.WriteFloat  ('TApplicationForm3', 'Temperature',     Temperature    .Value    );
  IniFile.WriteInteger('TApplicationForm3', 'TemperatureUnit', TemperatureUnit.ItemIndex);
  IniFile.WriteInteger('TApplicationForm3', 'StressDirection', StressDirection.ItemIndex);
end;

procedure TApplicationForm3.SaveToSolver;
begin
  case LoadType.ItemIndex of
   0: SpringSolver.DynamicLoad := True;
   1: SpringSolver.DynamicLoad := False;
  end;
  {$IFDEF MODULE3}
  SpringSolver.StressInCoilingDirection := StressDirection.ItemIndex = 0;
  {$ENDIF}
end;

end.

