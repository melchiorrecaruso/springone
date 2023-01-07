{ EN13906-3 Helical Torsion Spring Designer

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

unit GeometryFrmThree;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, DividerBevel, EN13906,
  ExtCtrls, Forms, Graphics, IniFiles, Spin, StdCtrls, SysUtils;

type

  { TGeometryFormThree }

  TGeometryFormThree = class(TForm)
    ActiveCoil: TFloatSpinEdit;
    ActiveCoilLabel: TLabel;
    ApplyBtn: TBitBtn;
    Bevel1: TBevel;
    CancelBtn: TBitBtn;
    CoilDiameter: TFloatSpinEdit;
    CoilDiameterIndex: TComboBox;
    CoilDiameterUnit: TComboBox;
    DistanceBetweenCoilsUnit: TComboBox;
    DividerBevel2: TDividerBevel;
    BendInnerRadius2Unit: TComboBox;
    BendInnerRadius2UnitLabel: TLabel;
    BendInnerRadius2: TFloatSpinEdit;
    Leg2Length: TFloatSpinEdit;
    Leg2LengthLabel: TLabel;
    Leg1LengthUnit: TComboBox;
    Leg2LengthUnit: TComboBox;
    Leg2Type: TComboBox;
    Leg2TypeLabel: TLabel;
    LeverArmLength2Label: TLabel;
    LeverArmLength1Unit: TComboBox;
    BendInnerRadius1Unit: TComboBox;
    Leg1Type: TComboBox;
    DividerBevel1: TDividerBevel;
    Leg1Length: TFloatSpinEdit;
    LeverArmLength1: TFloatSpinEdit;
    BendInnerRadius1: TFloatSpinEdit;
    DistanceBetweenCoils: TFloatSpinEdit;
    DistanceBetweenCoilsLabel: TLabel;
    AngleAlpha1: TFloatSpinEdit;
    AngleAlpha1Label: TLabel;
    AngleAlpha1Unit: TComboBox;
    AngleAlpha2: TFloatSpinEdit;
    AngleAlpha2Label: TLabel;
    AngleAlpha2Unit: TComboBox;
    LeverArmLength1Label: TLabel;
    Leg1TypeLabel: TLabel;
    BendInnerRadius1UnitLabel: TLabel;
    Leg1LengthLabel: TLabel;
    LeverArmLength2Unit: TComboBox;
    LeverArmLength2: TFloatSpinEdit;
    OkBtn: TBitBtn;
    WireDiameter: TFloatSpinEdit;
    WireDiameterLabel: TLabel;
    WireDiameterUnit: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  private

  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;

var
  GeometryFormThree: TGeometryFormThree;

implementation

{$R *.lfm}

uses
  UtilsBase;

{ TGeometryFormThree }

procedure TGeometryFormThree.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TGeometryFormThree.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TGeometryFormThree.Clear;
begin
  WireDiameter            .Value     := 0;
  WireDiameterUnit        .ItemIndex := 0;
  CoilDiameterIndex       .ItemIndex := 1;
  CoilDiameter            .Value     := 0;
  CoilDiameterUnit        .ItemIndex := 0;
  ActiveCoil              .Value     := 0;
  DistanceBetweenCoils    .Value     := 0;
  DistanceBetweenCoilsUnit.ItemIndex := 0;
  AngleAlpha1             .Value     := 0;
  AngleAlpha1Unit         .ItemIndex := 0;
  AngleAlpha2             .Value     := 0;
  AngleAlpha2Unit         .ItemIndex := 0;
  Leg1Length              .Value     := 0;
  Leg1LengthUnit          .ItemIndex := 0;
  Leg2Length              .Value     := 0;
  Leg2LengthUnit          .ItemIndex := 0;
  Leg1Type                .ItemIndex := 0;
  Leg2Type                .ItemIndex := 0;
  LeverArmLength1         .Value     := 0;
  LeverArmLength1Unit     .ItemIndex := 0;
  LeverArmLength2         .Value     := 0;
  LeverArmLength2Unit     .ItemIndex := 0;
  BendInnerRadius1        .Value     := 0;
  BendInnerRadius1Unit    .ItemIndex := 0;
  BendInnerRadius2        .Value     := 0;
  BendInnerRadius2Unit    .ItemIndex := 0;
end;

procedure TGeometryFormThree.Load(IniFile: TIniFile);
begin
  WireDiameter            .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'WireDiameter',              '0'));
  WireDiameterUnit        .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'WireDiameterUnit',          '0'));
  CoilDiameterIndex       .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'CoilDiameterIndex',         '0'));
  CoilDiameter            .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'CoilDiameter',              '0'));
  CoilDiameterUnit        .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'CoilDiameterUnit',          '0'));
  ActiveCoil              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'ActiveCoil',                '0'));
  DistanceBetweenCoils    .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'DistanceBetweenCoils',      '0'));
  DistanceBetweenCoilsUnit.ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'DistanceBetweenCoilsUnit',  '0'));
  AngleAlpha1             .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'AngleAlpha1',               '0'));
  AngleAlpha1Unit         .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'AngleAlpha1Unit',           '0'));
  AngleAlpha2             .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'AngleAlpha2',               '0'));
  AngleAlpha2Unit         .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'AngleAlpha2Unit',           '0'));
  Leg1Length              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'Leg1Length',                '0'));
  Leg1LengthUnit          .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'Leg1LengthUnit',            '0'));
  Leg2Length              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'Leg2Length',                '0'));
  Leg2LengthUnit          .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'Leg2LengthUnit',            '0'));
  Leg1Type                .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'Leg1Type',                  '0'));
  Leg2Type                .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'Leg2Type',                  '0'));
  LeverArmLength1         .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'LeverArmLength1',           '0'));
  LeverArmLength1Unit     .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'LeverArmLength1Unit',       '0'));
  LeverArmLength2         .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'LeverArmLength2',           '0'));
  LeverArmLength2Unit     .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'LeverArmLength2Unit',       '0'));
  BendInnerRadius1        .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'BendInnerRadius1',          '0'));
  BendInnerRadius1Unit    .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'BendInnerRadius1Unit',      '0'));
  BendInnerRadius2        .Value     := TryTextToFloat(IniFile.ReadString('TGeometryFormThree', 'BendInnerRadius2',          '0'));
  BendInnerRadius2Unit    .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryFormThree', 'BendInnerRadius2Unit',      '0'));
end;

procedure TGeometryFormThree.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TGeometryFormThree', 'WireDiameter',              WireDiameter            .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'WireDiameterUnit',          WireDiameterUnit        .ItemIndex);
  IniFile.WriteInteger('TGeometryFormThree', 'CoilDiameterIndex',         CoilDiameterIndex       .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'CoilDiameter',              CoilDiameter            .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'CoilDiameterUnit',          CoilDiameterUnit        .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'ActiveCoil',                ActiveCoil              .Value    );
  IniFile.WriteFloat  ('TGeometryFormThree', 'DistanceBetweenCoils',      DistanceBetweenCoils    .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'DistanceBetweenCoilsUnit',  DistanceBetweenCoilsUnit.ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'AngleAlpha1',               AngleAlpha1             .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'AngleAlpha1Unit',           AngleAlpha1Unit         .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'AngleAlpha2',               AngleAlpha2             .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'AngleAlpha2Unit',           AngleAlpha2Unit         .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'Leg1Length',                Leg1Length              .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'Leg1LengthUnit',            Leg1LengthUnit          .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'Leg2Length',                Leg2Length              .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'Leg2LengthUnit',            Leg2LengthUnit          .ItemIndex);
  IniFile.WriteInteger('TGeometryFormThree', 'Leg1Type',                  Leg1Type                .ItemIndex);
  IniFile.WriteInteger('TGeometryFormThree', 'Leg2Type',                  Leg2Type                .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'LeverArmLength1',           LeverArmLength1         .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'LeverArmLength1Unit',       LeverArmLength1Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'LeverArmLength2',           LeverArmLength2         .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'LeverArmLength2Unit',       LeverArmLength2Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'BendInnerRadius1',          BendInnerRadius1        .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'BendInnerRadius1Unit',      BendInnerRadius1Unit    .ItemIndex);
  IniFile.WriteFloat  ('TGeometryFormThree', 'BendInnerRadius2',          BendInnerRadius2        .Value    );
  IniFile.WriteInteger('TGeometryFormThree', 'BendInnerRadius2Unit',      BendInnerRadius2Unit    .ItemIndex);
end;

procedure TGeometryFormThree.SaveToSolver;
begin

end;

end.

