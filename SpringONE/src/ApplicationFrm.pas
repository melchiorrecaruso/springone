{ EN13906-1 Helical Compression Spring Designer

  Copyright (C) 2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit ApplicationFrm;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, Forms, ExtCtrls,
  Graphics, IniFiles, Menus,  Spin, StdCtrls, SysUtils;

type

  { TApplicationForm }

  TApplicationForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;


var
  ApplicationForm: TApplicationForm;


implementation

{$R *.lfm}

uses
  EN13906, MaterialFrm, Math, UtilsBase;

// TApplicationForm

procedure TApplicationForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TApplicationForm.Clear;
begin
  LoadType         .ItemIndex := 0;
  Temperature      .Value     := 20;
  TemperatureUnit  .ItemIndex := 0;
  SeatingCoefficent.ItemIndex := 1;
end;

procedure TApplicationForm.Load(IniFile: TIniFile);
begin
  LoadType         .ItemIndex := TryTextToInt  (IniFile.ReadString('TApplicationForm', 'LoadType',          '0'));
  CycleFrequency   .Value     := TryTextToFloat(IniFile.ReadString('TApplicationForm', 'CycleFrequency',    '1'));
  Temperature      .Value     := TryTextToFloat(IniFile.ReadString('TApplicationForm', 'Temperature',       '0'));
  TemperatureUnit  .ItemIndex := TryTextToInt  (IniFile.ReadString('TApplicationForm', 'TemperatureUnit',   '0'));
  SeatingCoefficent.ItemIndex := TryTextToInt  (IniFile.ReadString('TApplicationForm', 'SeatingCoefficent', '0'));
end;

procedure TApplicationForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TApplicationForm', 'LoadType',          LoadType         .ItemIndex);
  IniFile.WriteFloat  ('TApplicationForm', 'CycleFrequency',    CycleFrequency   .Value    );
  IniFile.WriteFloat  ('TApplicationForm', 'Temperature',       Temperature      .Value    );
  IniFile.WriteInteger('TApplicationForm', 'TemperatureUnit',   TemperatureUnit  .ItemIndex);
  IniFile.WriteInteger('TApplicationForm', 'SeatingCoefficent', SeatingCoefficent.ItemIndex);
end;

procedure TApplicationForm.SaveToSolver;
begin
  case LoadType.ItemIndex of
   0: SOLVER.DynamicLoad := True;
   1: SOLVER.DynamicLoad := False;
  end;
  SOLVER.SeatingCoefficent := TryTextToFloat(Self.SeatingCoefficent.Text);
end;

procedure TApplicationForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

end.

