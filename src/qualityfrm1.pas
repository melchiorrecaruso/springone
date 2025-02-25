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

unit QualityFrm1;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, LibLink, IniFiles, Spin,StdCtrls, SysUtils;

type

  { TQualityForm1 }

  TQualityForm1 = class(TForm)
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ToleranceOnWireDiameter: TFloatSpinEdit;
    GradeOnCoilDiameter: TComboBox;
    GradeOnCoilDiameterLB: TLabel;
    ToleranceOnWireDiameterLabel: TLabel;
    GradeOnEccentricitye1: TComboBox;
    GradeOnEccentricitye1LB: TLabel;
    GradeOnEccentricitye2: TComboBox;
    GradeOnEccentricitye2LB: TLabel;
    GradeOnLoadF1: TComboBox;
    GradeOnLoadF1LB: TLabel;
    GradeOnLoadF2: TComboBox;
    GradeOnLoadF2LB: TLabel;
    GradeOnLengthL0: TComboBox;
    GradeOnLengthL0LB: TLabel;
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
  QualityForm1: TQualityForm1;


implementation

{$R *.lfm}

uses
  ADim,
  SpringTolerances,
  GeometryFrm1,
  BaseUtils,
  Setting;

// TQualityForm1

procedure TQualityForm1.FormCreate(Sender: TObject);
begin
  QualityForm1.Top    := ClientFile.ReadInteger('QualityForm1', 'Top',    QualityForm1.Top);
  QualityForm1.Left   := ClientFile.ReadInteger('QualityForm1', 'Left',   QualityForm1.Left);
  QualityForm1.Height := ClientFile.ReadInteger('QualityForm1', 'Height', QualityForm1.Height);
  QualityForm1.Width  := ClientFile.ReadInteger('QualityForm1', 'Width',  QualityForm1.Width);

  Clear;
end;

procedure TQualityForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('QualityForm1', 'Top',    QualityForm1.Top);
    ClientFile.WriteInteger('QualityForm1', 'Left',   QualityForm1.Left);
    ClientFile.WriteInteger('QualityForm1', 'Height', QualityForm1.Height);
    ClientFile.WriteInteger('QualityForm1', 'Width',  QualityForm1.Width);
  end;
end;

procedure TQualityForm1.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TQualityForm1.Clear;
begin
  ToleranceOnWireDiameter    .Value     := 0;
  ToleranceOnWireDiameterUnit.ItemIndex := 0;
  GradeOnCoilDiameter        .ItemIndex := 1;
  GradeOnLengthL0            .ItemIndex := 1;
  GradeOnLoadF1              .ItemIndex := 1;
  GradeOnLoadF2              .ItemIndex := 1;
  GradeOnEccentricitye1      .ItemIndex := 1;
  GradeOnEccentricitye2      .ItemIndex := 1;
end;

procedure TQualityForm1.Load(IniFile: TIniFile);
begin
  ToleranceOnWireDiameter    .Value     := IniFile.ReadFloat  ('TQualityForm1', 'ToleranceOnWireDiameter',     0);
  ToleranceOnWireDiameterUnit.ItemIndex := IniFile.ReadInteger('TQualityForm1', 'ToleranceOnWireDiameterUnit', 0);
  GradeOnCoilDiameter        .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnCoilDiameter',         0);
  GradeOnLengthL0            .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnLengthL0',             0);
  GradeOnLoadF1              .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnLoadF1',               0);
  GradeOnLoadF2              .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnLoadF2',               0);
  GradeOnEccentricitye1      .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnEccentricitye1',       0);
  GradeOnEccentricitye2      .ItemIndex := IniFile.ReadInteger('TQualityForm1', 'GradeOnEccentricitye2',       0);
end;

procedure TQualityForm1.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TQualityForm1', 'ToleranceOnWireDiameter',     ToleranceOnWireDiameter    .Value    );
  IniFile.WriteInteger('TQualityForm1', 'ToleranceOnWireDiameterUnit', ToleranceOnWireDiameterUnit.ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnCoilDiameter',         GradeOnCoilDiameter        .ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnLengthL0',             GradeOnLengthL0            .ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnLoadF1',               GradeOnLoadF1              .ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnLoadF2',               GradeOnLoadF2              .ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnEccentricitye1',       GradeOnEccentricitye1      .ItemIndex);
  IniFile.WriteInteger('TQualityForm1', 'GradeOnEccentricitye2',       GradeOnEccentricitye2      .ItemIndex);
end;

procedure TQualityForm1.SaveToSolver;
begin
  if Assigned(GeometryForm1) then
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
  case GradeOnLengthL0.ItemIndex of
    0: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade1;
    1: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade2;
    2: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade3;
  end;
  case GradeOnLoadF1.ItemIndex of
    0: SpringTolerance.QualityGradeOnLoad1 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnLoad1 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnLoad1 := QualityGrade3;
  end;
  case GradeOnLoadF2.ItemIndex of
    0: SpringTolerance.QualityGradeOnLoad2 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnLoad2 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnLoad2 := QualityGrade3;
  end;
  case GradeOnEccentricitye1.ItemIndex of
    0: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade1;
    1: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade2;
    2: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade3;
  end;
  case GradeOnEccentricitye2.ItemIndex of
    0: SpringTolerance.QualityGradeOnParallelism := QualityGrade1;
    1: SpringTolerance.QualityGradeOnParallelism := QualityGrade2;
    2: SpringTolerance.QualityGradeOnParallelism := QualityGrade3;
  end;
end;

end.

