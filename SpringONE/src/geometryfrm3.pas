{ EN13906-3 Helical Torsion Spring Designer

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit GeometryFrm3;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, DividerBevel, EN13906,
  ExtCtrls, Forms, Graphics, IniFiles, Spin, StdCtrls, SysUtils;

type

  { TGeometryForm3 }

  TGeometryForm3 = class(TForm)
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
    BendRadiusLegBUnit: TComboBox;
    BendRadiusLegBLabel: TLabel;
    BendRadiusLegB: TFloatSpinEdit;
    LengthLegB: TFloatSpinEdit;
    LengthLegBLabel: TLabel;
    LengthLegAUnit: TComboBox;
    LengthLegBUnit: TComboBox;
    TypeLegB: TComboBox;
    TypeLegBLabel: TLabel;
    LengthArmLegBLabel: TLabel;
    LengthArmLegAUnit: TComboBox;
    BendRadiusLegAUnit: TComboBox;
    TypeLegA: TComboBox;
    DividerBevel1: TDividerBevel;
    LengthLegA: TFloatSpinEdit;
    LengthArmLegA: TFloatSpinEdit;
    BendRadiusLegA: TFloatSpinEdit;
    DistanceBetweenCoils: TFloatSpinEdit;
    DistanceBetweenCoilsLabel: TLabel;
    AngleAlpha1: TFloatSpinEdit;
    AngleAlpha1Label: TLabel;
    AngleAlpha1Unit: TComboBox;
    AngleAlpha2: TFloatSpinEdit;
    AngleAlpha2Label: TLabel;
    AngleAlpha2Unit: TComboBox;
    LengthArmLegALabel: TLabel;
    TypeLegALabel: TLabel;
    BendRadiusLegALabel: TLabel;
    LengthLegALabel: TLabel;
    LengthArmLegBUnit: TComboBox;
    LengthArmLegB: TFloatSpinEdit;
    OkBtn: TBitBtn;
    WireDiameter: TFloatSpinEdit;
    WireDiameterLabel: TLabel;
    WireDiameterUnit: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure TypeLegAChange(Sender: TObject);
    procedure TypeLegBChange(Sender: TObject);
  private

  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;

var
  GeometryForm3: TGeometryForm3;

implementation

{$R *.lfm}

uses
  ConvUtils, ADim, MainFrm, UtilsBase;

{ TGeometryForm3 }

procedure TGeometryForm3.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TGeometryForm3.Clear;
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
  LengthLegA              .Value     := 0;
  LengthLegAUnit          .ItemIndex := 0;
  LengthLegB              .Value     := 0;
  LengthLegBUnit          .ItemIndex := 0;
  TypeLegA                .ItemIndex := 0;
  TypeLegB                .ItemIndex := 0;
  LengthArmLegA         .Value     := 0;
  LengthArmLegAUnit     .ItemIndex := 0;
  LengthArmLegB         .Value     := 0;
  LengthArmLegBUnit     .ItemIndex := 0;
  BendRadiusLegA        .Value     := 0;
  BendRadiusLegAUnit    .ItemIndex := 0;
  BendRadiusLegB        .Value     := 0;
  BendRadiusLegBUnit    .ItemIndex := 0;

  TypeLegAChange(Self);
  TypeLegBChange(Self);
end;

procedure TGeometryForm3.Load(IniFile: TIniFile);
begin
  WireDiameter            .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'WireDiameter',              '0'));
  WireDiameterUnit        .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'WireDiameterUnit',          '0'));
  CoilDiameterIndex       .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'CoilDiameterIndex',         '0'));
  CoilDiameter            .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'CoilDiameter',              '0'));
  CoilDiameterUnit        .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'CoilDiameterUnit',          '0'));
  ActiveCoil              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'ActiveCoil',                '0'));
  DistanceBetweenCoils    .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'DistanceBetweenCoils',      '0'));
  DistanceBetweenCoilsUnit.ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'DistanceBetweenCoilsUnit',  '0'));
  AngleAlpha1             .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'AngleAlpha1',               '0'));
  AngleAlpha1Unit         .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'AngleAlpha1Unit',           '0'));
  AngleAlpha2             .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'AngleAlpha2',               '0'));
  AngleAlpha2Unit         .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'AngleAlpha2Unit',           '0'));
  LengthLegA              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'Leg1Length',                '0'));
  LengthLegAUnit          .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'Leg1LengthUnit',            '0'));
  LengthLegB              .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'Leg2Length',                '0'));
  LengthLegBUnit          .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'Leg2LengthUnit',            '0'));
  TypeLegA                .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'Leg1Type',                  '0'));
  TypeLegB                .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'Leg2Type',                  '0'));
  LengthArmLegA           .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'LeverArmLength1',           '0'));
  LengthArmLegAUnit       .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'LeverArmLength1Unit',       '0'));
  LengthArmLegB           .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'LeverArmLength2',           '0'));
  LengthArmLegBUnit       .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'LeverArmLength2Unit',       '0'));
  BendRadiusLegA          .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'BendInnerRadius1',          '0'));
  BendRadiusLegAUnit      .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'BendInnerRadius1Unit',      '0'));
  BendRadiusLegB          .Value     := TryTextToFloat(IniFile.ReadString('TGeometryForm3', 'BendInnerRadius2',          '0'));
  BendRadiusLegBUnit      .ItemIndex := TryTextToInt  (IniFile.ReadString('TGeometryForm3', 'BendInnerRadius2Unit',      '0'));

  TypeLegAChange(Self);
  TypeLegBChange(Self);
end;

procedure TGeometryForm3.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TGeometryForm3', 'WireDiameter',              WireDiameter            .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'WireDiameterUnit',          WireDiameterUnit        .ItemIndex);
  IniFile.WriteInteger('TGeometryForm3', 'CoilDiameterIndex',         CoilDiameterIndex       .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'CoilDiameter',              CoilDiameter            .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'CoilDiameterUnit',          CoilDiameterUnit        .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'ActiveCoil',                ActiveCoil              .Value    );
  IniFile.WriteFloat  ('TGeometryForm3', 'DistanceBetweenCoils',      DistanceBetweenCoils    .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'DistanceBetweenCoilsUnit',  DistanceBetweenCoilsUnit.ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'AngleAlpha1',               AngleAlpha1             .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'AngleAlpha1Unit',           AngleAlpha1Unit         .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'AngleAlpha2',               AngleAlpha2             .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'AngleAlpha2Unit',           AngleAlpha2Unit         .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'Leg1Length',                LengthLegA              .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'Leg1LengthUnit',            LengthLegAUnit          .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'Leg2Length',                LengthLegB              .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'Leg2LengthUnit',            LengthLegBUnit          .ItemIndex);
  IniFile.WriteInteger('TGeometryForm3', 'Leg1Type',                  TypeLegA                .ItemIndex);
  IniFile.WriteInteger('TGeometryForm3', 'Leg2Type',                  TypeLegB                .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'LeverArmLength1',           LengthArmLegA           .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'LeverArmLength1Unit',       LengthArmLegAUnit       .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'LeverArmLength2',           LengthArmLegB           .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'LeverArmLength2Unit',       LengthArmLegBUnit       .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'BendInnerRadius1',          BendRadiusLegA          .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'BendInnerRadius1Unit',      BendRadiusLegAUnit      .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm3', 'BendInnerRadius2',          BendRadiusLegB          .Value    );
  IniFile.WriteInteger('TGeometryForm3', 'BendInnerRadius2Unit',      BendRadiusLegBUnit      .ItemIndex);
end;

procedure TGeometryForm3.SaveToSolver;
begin
  case WireDiameterUnit.ItemIndex of
    0: SOLVER3.WireDiameter := WireDiameter.Value*mm;
    1: SOLVER3.WireDiameter := WireDiameter.Value*25.4*mm;
  end;

  case CoilDiameterUnit.ItemIndex of
    0: SOLVER3.Dm := CoilDiameter.Value*mm;
    1: SOLVER3.Dm := CoilDiameter.Value*25.4*mm;
  end;

  case CoilDiameterIndex.ItemIndex of
    0: SOLVER3.Dm := SOLVER3.Dm + SOLVER3.WireDiameter;  // Input Di
    1: SOLVER3.Dm := SOLVER3.Dm;                         // Input Dm
    2: SOLVER3.Dm := SOLVER3.Dm - SOLVER3.WireDiameter;  // Input De
  end;

  SOLVER3.ActiveColis := ActiveCoil.Value;

  case DistanceBetweenCoilsUnit.ItemIndex of
    0: SOLVER3.CoilsGap := DistanceBetweenCoils.Value*mm;
    1: SOLVER3.CoilsGap := DistanceBetweenCoils.Value*25.4*mm;
  end;

  case AngleAlpha1Unit.ItemIndex of
    0: SOLVER3.Alpha1 := AngleAlpha1.Value*deg;
    1: SOLVER3.Alpha1 := AngleAlpha1.Value*rad;
  end;

  case AngleAlpha2Unit.ItemIndex of
    0: SOLVER3.Alpha2 := AngleAlpha2.Value*deg;
    1: SOLVER3.Alpha2 := AngleAlpha2.Value*rad;
  end;

  // Leg-A
  case LengthLegAUnit.ItemIndex of
    0: SOLVER3.LengthLegA := LengthLegA.Value*mm;
    1: SOLVER3.LengthLegA := LengthLegA.Value*25.4*mm;
  end;

  case LengthArmLegAUnit.ItemIndex of
    0: SOLVER3.LengthArmLegA := LengthArmLegA.Value*mm;
    1: SOLVER3.LengthArmLegA := LengthArmLegA.Value*25.4*mm;
  end;

  SOLVER3.BentLegA  := TypeLegA.ItemIndex in [2, 3];
  SOLVER3.FixedEndA := TypeLegA.ItemIndex in [0, 2];
  if SOLVER3.BentLegA then
    case BendRadiusLegAUnit.ItemIndex of
      0: SOLVER3.BendRadiusLegA := BendRadiusLegA.Value*mm;
      1: SOLVER3.BendRadiusLegA := BendRadiusLegA.Value*25.4*mm;
    end;

  // Leg-B
  case LengthLegBUnit.ItemIndex of
    0: SOLVER3.LengthLegB := LengthLegB.Value*mm;
    1: SOLVER3.LengthLegB := LengthLegB.Value*25.4*mm;
  end;

  case LengthArmLegBUnit.ItemIndex of
    0: SOLVER3.LengthArmLegB := LengthArmLegB.Value*mm;
    1: SOLVER3.LengthArmLegB := LengthArmLegB.Value*25.4*mm;
  end;

  SOLVER3.BentLegB  := TypeLegB.ItemIndex in [2, 3];
  SOLVER3.FixedEndB := TypeLegB.ItemIndex in [0, 2];
  if SOLVER3.BentLegB then
    case BendRadiusLegBUnit.ItemIndex of
      0: SOLVER3.BendRadiusLegB := BendRadiusLegB.Value*mm;
      1: SOLVER3.BendRadiusLegB := BendRadiusLegB.Value*25.4*mm;
    end;
end;

procedure TGeometryForm3.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TGeometryForm3.TypeLegAChange(Sender: TObject);
begin
  BendRadiusLegA.Enabled := TypeLegA.ItemIndex in [2, 3];
  if not BendRadiusLegA.Enabled then
  begin
    BendRadiusLegA.Value := 0;
  end;
end;

procedure TGeometryForm3.TypeLegBChange(Sender: TObject);
begin
  BendRadiusLegB.Enabled := TypeLegB.ItemIndex in [2, 3];
  if not BendRadiusLegB.Enabled then
  begin
    BendRadiusLegB.Value := 0;
  end;
end;

procedure TGeometryForm3.OkBtnClick(Sender: TObject);
begin
  MainForm.Solve();
end;

end.

