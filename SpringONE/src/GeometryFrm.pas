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

unit GeometryFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, IniFiles;

type

  { TGeometryForm }

  TGeometryForm = class(TForm)
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    ApplyBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CoilDiameter: TFloatSpinEdit;
    CoilDiameterIndex: TComboBox;
    CoilDiameterUnit: TComboBox;
    EndCoilType: TComboBox;
    EndCoilTypeLabel: TLabel;
    InactiveCoil1: TFloatSpinEdit;
    InactiveCoil1Label: TLabel;
    InactiveCoil2: TFloatSpinEdit;
    InactiveCoil2Label: TLabel;
    ActiveCoil: TFloatSpinEdit;
    ActiveCoilLabel: TLabel;
    LengthL0: TFloatSpinEdit;
    LengthL0Label: TLabel;
    LengthL0Unit: TComboBox;
    LengthL1: TFloatSpinEdit;
    LengthL1Label: TLabel;
    LengthL1Unit: TComboBox;
    LengthL2: TFloatSpinEdit;
    LengthL2Label: TLabel;
    LengthL2Unit: TComboBox;
    WireDiameter: TFloatSpinEdit;
    WireDiameterLabel: TLabel;
    WireDiameterUnit: TComboBox;
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure WireDiameterChange(Sender: TObject);
  private

  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  GeometryForm: TGeometryForm;

implementation

{$R *.lfm}

uses
  MainFrm, Math;

{ TGeometryForm }

procedure TGeometryForm.ApplyBtnClick(Sender: TObject);
begin
  MainForm.Solve();
end;

procedure TGeometryForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TGeometryForm.WireDiameterChange(Sender: TObject);
begin
  TFloatSpinEdit(Sender).Value := Max(0, TFloatSpinEdit(Sender).Value);
end;

procedure TGeometryForm.Clear;
begin
  WireDiameter     .Value     := 0;
  WireDiameterUnit .ItemIndex := 0;
  CoilDiameterIndex.ItemIndex := 1;
  CoilDiameter     .Value     := 0;
  CoilDiameterUnit .ItemIndex := 0;
  ActiveCoil       .Value     := 0;
  InactiveCoil1    .Value     := 0;
  InactiveCoil2    .Value     := 0;
  LengthL0         .Value     := 0;
  LengthL0Unit     .ItemIndex := 0;
  LengthL1         .Value     := 0;
  LengthL1Unit     .ItemIndex := 0;
  LengthL2         .Value     := 0;
  LengthL2Unit     .ItemIndex := 0;
  EndCoilType      .ItemIndex := 1;
end;

procedure TGeometryForm.Load(IniFile: TIniFile);
begin
  WireDiameter     .Value     := IniFile.ReadFloat  ('TGeometryForm', 'WireDiameter',      0);
  WireDiameterUnit .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'WireDiameterUnit',  0);
  CoilDiameterIndex.ItemIndex := IniFile.ReadInteger('TGeometryForm', 'CoilDiameterIndex', 0);
  CoilDiameter     .Value     := IniFile.ReadFloat  ('TGeometryForm', 'CoilDiameter',      0);
  CoilDiameterUnit .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'CoilDiameterUnit',  0);
  ActiveCoil       .Value     := IniFile.ReadFloat  ('TGeometryForm', 'ActiveCoil',        0);
  InactiveCoil1    .Value     := IniFile.ReadFloat  ('TGeometryForm', 'InactiveCoil1',     0);
  InactiveCoil2    .Value     := IniFile.ReadFloat  ('TGeometryForm', 'InactiveCoil2',     0);
  LengthL0         .Value     := IniFile.ReadFloat  ('TGeometryForm', 'LengthL0',          0);
  LengthL0Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'LengthL0Unit',      0);
  LengthL1         .Value     := IniFile.ReadFloat  ('TGeometryForm', 'LengthL1',          0);
  LengthL1Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'LengthL1Unit',      0);
  LengthL2         .Value     := IniFile.ReadFloat  ('TGeometryForm', 'LengthL2',          0);
  LengthL2Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'LengthL2Unit',      0);
  EndCoilType      .ItemIndex := IniFile.ReadInteger('TGeometryForm', 'EndCoilType',       0);
end;

procedure TGeometryForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TGeometryForm', 'WireDiameter',      WireDiameter     .Value    );
  IniFile.WriteInteger('TGeometryForm', 'WireDiameterUnit',  WireDiameterUnit .ItemIndex);
  IniFile.WriteInteger('TGeometryForm', 'CoilDiameterIndex', CoilDiameterIndex.ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm', 'CoilDiameter',      CoilDiameter     .Value    );
  IniFile.WriteInteger('TGeometryForm', 'CoilDiameterUnit',  CoilDiameterUnit .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm', 'ActiveCoil',        ActiveCoil       .Value    );
  IniFile.WriteFloat  ('TGeometryForm', 'InactiveCoil1',     InactiveCoil1    .Value    );
  IniFile.WriteFloat  ('TGeometryForm', 'InactiveCoil2',     InactiveCoil2    .Value    );
  IniFile.WriteFloat  ('TGeometryForm', 'LengthL0',          LengthL0         .Value    );
  IniFile.WriteInteger('TGeometryForm', 'LengthL0Unit',      LengthL0Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm', 'LengthL1',          LengthL1         .Value    );
  IniFile.WriteInteger('TGeometryForm', 'LengthL1Unit',      LengthL1Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm', 'LengthL2',          LengthL2         .Value    );
  IniFile.WriteInteger('TGeometryForm', 'LengthL2Unit',      LengthL2Unit     .ItemIndex);
  IniFile.WriteInteger('TGeometryForm', 'EndCoilType',       EndCoilType      .ItemIndex);
end;

end.

