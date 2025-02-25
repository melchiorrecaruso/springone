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

unit QualityFrm3;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, LibLink, IniFiles, Spin,StdCtrls, SysUtils;

type

  { TQualityForm3 }

  TQualityForm3 = class(TForm)
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    GradeOnBendRadiiLB: TLabel;
    GradeOnAnglesOfBendsLegsLB: TLabel;
    GradeOnBendRadii: TComboBox;
    GradeOnAnglesOfBendsLegs: TComboBox;
    ToleranceOnWireDiameter: TFloatSpinEdit;
    GradeOnCoilDiameter: TComboBox;
    GradeOnCoilDiameterLB: TLabel;
    ToleranceOnWireDiameterLB: TLabel;
    GradeOnRelativeEndAgle: TComboBox;
    GradeOnRelativeEndAgleLB: TLabel;
    GradeOnLegLengths: TComboBox;
    GradeOnLegLengthsLB: TLabel;
    GradeOnTorqueT1: TComboBox;
    GradeOnTorqueT1LB: TLabel;
    GradeOnTorqueT2: TComboBox;
    GradeOnTorqueT2LB: TLabel;
    GradeOnLengthLk: TComboBox;
    GradeOnLengthLkLB: TLabel;
    ToleranceOnWireDiameterUnit: TComboBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  QualityForm3: TQualityForm3;


implementation

{$R *.lfm}

uses
  ADim,
  SpringTolerances,
  GeometryFrm3,
  BaseUtils,
  Setting;

// TQualityForm3

procedure TQualityForm3.FormCreate(Sender: TObject);
begin
  QualityForm3.Top    := ClientFile.ReadInteger('QualityForm3', 'Top',    QualityForm3.Top);
  QualityForm3.Left   := ClientFile.ReadInteger('QualityForm3', 'Left',   QualityForm3.Left);
  QualityForm3.Height := ClientFile.ReadInteger('QualityForm3', 'Height', QualityForm3.Height);
  QualityForm3.Width  := ClientFile.ReadInteger('QualityForm3', 'Width',  QualityForm3.Width);

  Clear;
end;

procedure TQualityForm3.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('QualityForm3', 'Top',    QualityForm3.Top);
    ClientFile.WriteInteger('QualityForm3', 'Left',   QualityForm3.Left);
    ClientFile.WriteInteger('QualityForm3', 'Height', QualityForm3.Height);
    ClientFile.WriteInteger('QualityForm3', 'Width',  QualityForm3.Width);
  end;
end;

procedure TQualityForm3.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TQualityForm3.Clear;
begin
  ToleranceOnWireDiameter    .Value     := 0;
  ToleranceOnWireDiameterUnit.ItemIndex := 0;
  GradeOnCoilDiameter        .ItemIndex := 1;
  GradeOnLengthLk            .ItemIndex := 1;
  GradeOnTorqueT1            .ItemIndex := 1;
  GradeOnTorqueT2            .ItemIndex := 1;
  GradeOnRelativeEndAgle     .ItemIndex := 1;
  GradeOnLegLengths          .ItemIndex := 1;
end;

procedure TQualityForm3.Load(IniFile: TIniFile);
begin
  ToleranceOnWireDiameter    .Value     := IniFile.ReadFloat  ('TQualityForm3', 'ToleranceOnWireDiameter',     0);
  ToleranceOnWireDiameterUnit.ItemIndex := IniFile.ReadInteger('TQualityForm3', 'ToleranceOnWireDiameterUnit', 0);
  GradeOnCoilDiameter        .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnCoilDiameter',         0);
  GradeOnLengthLk            .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnLengthLk',             0);
  GradeOnTorqueT1            .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnTorqueT1',             0);
  GradeOnTorqueT2            .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnTorqueT2',             0);
  GradeOnRelativeEndAgle     .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnRelativeEndAgle',      0);
  GradeOnLegLengths          .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnLegLengths',           0);
  GradeOnBendRadii           .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnBendRadii',            0);
  GradeOnAnglesOfBendsLegs   .ItemIndex := IniFile.ReadInteger('TQualityForm3', 'GradeOnAnglesOfBendsLegs'   , 0);
end;

procedure TQualityForm3.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TQualityForm3', 'ToleranceOnWireDiameter',     ToleranceOnWireDiameter    .Value    );
  IniFile.WriteInteger('TQualityForm3', 'ToleranceOnWireDiameterUnit', ToleranceOnWireDiameterUnit.ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnCoilDiameter',         GradeOnCoilDiameter        .ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnLengthLk',             GradeOnLengthLk            .ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnTorqueT1',             GradeOnTorqueT1            .ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnTorqueT2',             GradeOnTorqueT2            .ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnRelativeEndAgle',      GradeOnRelativeEndAgle     .ItemIndex);
  IniFile.WriteInteger('TQualityForm3', 'GradeOnLegLengths',           GradeOnLegLengths          .ItemIndex);
  IniFile.ReadInteger ('TQualityForm3', 'GradeOnBendRadii',            GradeOnBendRadii           .ItemIndex);
  IniFile.ReadInteger ('TQualityForm3', 'GradeOnAnglesOfBendsLegs',    GradeOnAnglesOfBendsLegs   .ItemIndex);
end;

procedure TQualityForm3.SaveToSolver;
begin
  if Assigned(GeometryForm3) then
  begin
    SpringSolver.WireDiameterTolerance := 0*m;
    case ToleranceOnWireDiameterUnit.ItemIndex of
      0: SpringSolver.WireDiametertolerance := ToleranceOnWireDiameter.Value*mm;
      1: SpringSolver.WireDiametertolerance := ToleranceOnWireDiameter.Value*inch;
    end;
  end;

  case GradeOnCoilDiameter.ItemIndex of
    0: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade1;
    1: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade2;
    2: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade3;
  end;
  case GradeOnLengthLk.ItemIndex of
    0: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade1;
    1: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade2;
    2: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade3;
  end;
  case GradeOnTorqueT1.ItemIndex of
    0: SpringTolerance.QualityGradeOnTorque1 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnTorque1 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnTorque1 := QualityGrade3;
  end;
  case GradeOnTorqueT2.ItemIndex of
    0: SpringTolerance.QualityGradeOnTorque2 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnTorque2 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnTorque2 := QualityGrade3;
  end;
  case GradeOnRelativeEndAgle.ItemIndex of
    0: SpringTolerance.QualityGradeOnRelativeEndAngle := QualityGrade1;
    1: SpringTolerance.QualityGradeOnRelativeEndAngle := QualityGrade2;
    2: SpringTolerance.QualityGradeOnRelativeEndAngle := QualityGrade3;
  end;
  case GradeOnLegLengths.ItemIndex of
    0: SpringTolerance.QualityGradeOnLegLengths := QualityGrade1;
    1: SpringTolerance.QualityGradeOnLegLengths := QualityGrade2;
    2: SpringTolerance.QualityGradeOnLegLengths := QualityGrade3;
  end;
  case GradeOnBendRadii.ItemIndex of
    0: SpringTolerance.QualityGradeOnBendRadii := QualityGrade1;
    1: SpringTolerance.QualityGradeOnBendRadii := QualityGrade2;
    2: SpringTolerance.QualityGradeOnBendRadii := QualityGrade3;
  end;
  case GradeOnAnglesOfBendsLegs.ItemIndex of
    0: SpringTolerance.QualityGradeOnAnglesOfBendOnLegs := QualityGrade1;
    1: SpringTolerance.QualityGradeOnAnglesOfBendOnLegs := QualityGrade2;
    2: SpringTolerance.QualityGradeOnAnglesOfBendOnLegs := QualityGrade3;
  end;
end;

end.

