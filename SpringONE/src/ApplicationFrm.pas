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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, Menus,
  Buttons, ComCtrls, EN13906_1, EN10270, EN15800, IniFiles, UtilsBase;

type

  { TApplicationForm }

  TApplicationForm = class(TForm)
    Bevel: TBevel;
    CycleFrequencyLabel: TLabel;
    Temperature: TFloatSpinEdit;
    CycleFrequency: TFloatSpinEdit;
    TemperatureLabel: TLabel;
    LoadType: TComboBox;
    LoadTypeLabel: TLabel;
    TemperatureUnit: TComboBox;
    SeatingCoefficent: TComboBox;
    SeatingCoefficentLabel: TLabel;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    CycleFrequencyUnit: TComboBox;
    procedure CycleFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  ApplicationForm: TApplicationForm;

implementation

{$R *.lfm}

uses
  Math;

{ TApplicationForm }

procedure TApplicationForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TApplicationForm.CycleFrequencyChange(Sender: TObject);
begin
  TFloatSpinEdit(Sender).Value := Max(1,  TFloatSpinEdit(Sender).Value);
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
  LoadType         .ItemIndex := IniFile.ReadInteger('TApplicationForm', 'LoadType',          0);
  CycleFrequency   .Value     := IniFile.ReadFloat  ('TApplicationForm', 'CycleFrequency',    1);
  Temperature      .Value     := IniFile.ReadFloat  ('TApplicationForm', 'Temperature',       0);
  TemperatureUnit  .ItemIndex := IniFile.ReadInteger('TApplicationForm', 'TemperatureUnit',   0);
  SeatingCoefficent.ItemIndex := IniFile.ReadInteger('TApplicationForm', 'SeatingCoefficent', 0);
end;

procedure TApplicationForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TApplicationForm', 'LoadType',          LoadType         .ItemIndex);
  IniFile.WriteFloat  ('TApplicationForm', 'CycleFrequency',    CycleFrequency   .Value    );
  IniFile.WriteFloat  ('TApplicationForm', 'Temperature',       Temperature      .Value    );
  IniFile.WriteInteger('TApplicationForm', 'TemperatureUnit',   TemperatureUnit  .ItemIndex);
  IniFile.WriteInteger('TApplicationForm', 'SeatingCoefficent', SeatingCoefficent.ItemIndex);
end;

end.

