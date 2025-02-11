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

unit ApplicationFrm1;

{$mode objfpc}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, Forms, ExtCtrls,
  Graphics, IniFiles, Menus, Spin, StdCtrls, SysUtils;

type

  { TApplicationForm1 }

  TApplicationForm1 = class(TForm)
    Bevel: TBevel;
    CycleFrequencyLabel: TLabel;
    CycleFrequency: TFloatSpinEdit;
    CycleFrequencyUnit: TComboBox;
    LoadTypeLabel: TLabel;
    LoadType: TComboBox;
    SeatingCoefficentLabel: TLabel;
    SeatingCoefficent: TComboBox;
    TemperatureLabel: TLabel;
    Temperature: TFloatSpinEdit;
    TemperatureUnit: TComboBox;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  public
    procedure ClearForm;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;


var
  ApplicationForm1: TApplicationForm1;


implementation

{$R *.lfm}

uses
  ADim, LibLink, SpringSolvers, BaseUtils, Setting;

// TApplicationForm1

procedure TApplicationForm1.FormCreate(Sender: TObject);
begin
  ApplicationForm1.Top    := ClientFile.ReadInteger('ApplicationForm1', 'Top', ApplicationForm1.Top);
  ApplicationForm1.Left   := ClientFile.ReadInteger('ApplicationForm1', 'Left', ApplicationForm1.Left);
  ApplicationForm1.Height := ClientFile.ReadInteger('ApplicationForm1', 'Height', ApplicationForm1.Height);
  ApplicationForm1.Width  := ClientFile.ReadInteger('ApplicationForm1', 'Width', ApplicationForm1.Width);
  ClearForm;
end;

procedure TApplicationForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('ApplicationForm1', 'Top', ApplicationForm1.Top);
    ClientFile.WriteInteger('ApplicationForm1', 'Left', ApplicationForm1.Left);
    ClientFile.WriteInteger('ApplicationForm1', 'Height', ApplicationForm1.Height);
    ClientFile.WriteInteger('ApplicationForm1', 'Width', ApplicationForm1.Width);
  end;
end;

procedure TApplicationForm1.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TApplicationForm1.ClearForm;
begin
  LoadType.ItemIndex := 0;
  Temperature.Value := 20;
  TemperatureUnit.ItemIndex := 0;
  SeatingCoefficent.ItemIndex := 1;
end;

procedure TApplicationForm1.Load(IniFile: TIniFile);
begin
  LoadType.ItemIndex          := IniFile.ReadInteger('TApplicationForm1', 'LoadType', 0);
  CycleFrequency.Value        := IniFile.ReadFloat  ('TApplicationForm1', 'CycleFrequency', 1);
  Temperature.Value           := IniFile.ReadFloat  ('TApplicationForm1', 'Temperature', 0);
  TemperatureUnit.ItemIndex   := IniFile.ReadInteger('TApplicationForm1', 'TemperatureUnit', 0);
  SeatingCoefficent.ItemIndex := IniFile.ReadInteger('TApplicationForm1', 'SeatingCoefficent', 0);
end;

procedure TApplicationForm1.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TApplicationForm1', 'LoadType', LoadType.ItemIndex);
  IniFile.WriteFloat  ('TApplicationForm1', 'CycleFrequency', CycleFrequency.Value);
  IniFile.WriteFloat  ('TApplicationForm1', 'Temperature', Temperature.Value);
  IniFile.WriteInteger('TApplicationForm1', 'TemperatureUnit', TemperatureUnit.ItemIndex);
  IniFile.WriteInteger('TApplicationForm1', 'SeatingCoefficent', SeatingCoefficent.ItemIndex);
end;

procedure TApplicationForm1.SaveToSolver;
begin
  case LoadType.ItemIndex of
    0: SpringSolver.DynamicLoad := True;
    1: SpringSolver.DynamicLoad := False;
    else raise Exception.Create('ApplicationForm: Unknow load type selected.');
  end;
  {$IFDEF MODULE1}
  SpringSolver.SeatingCoefficent := StrToFloat(SeatingCoefficent.Text);
  case TemperatureUnit.ItemIndex of
    0: SpringSolver.Temperature := Temperature.Value * degC;
    1: SpringSolver.Temperature := Temperature.Value * degF;
  else raise Exception.Create('ApplicationForm: Unknow unit of measurement selected for temperature.');
  end;
  {$ENDIF}
  {$IFDEF MODULE3}
  {$ENDIF}
end;

end.

