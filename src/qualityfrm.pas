{ EN13906-1 Helical Compression Spring Designer

  Copyright (C) 2022-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit QualityFrm;

{$mode ObjFPC}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, LibLink, IniFiles, Spin,StdCtrls, SysUtils;

type

  { TQualityForm }

  TQualityForm = class(TForm)
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    ToleranceWireDiameter: TFloatSpinEdit;
    ToleranceCoilDiameter: TComboBox;
    ToleranceCoilDiameterLabel: TLabel;
    ToleranceWireDiameterLabel: TLabel;
    ToleranceEccentricitye1: TComboBox;
    ToleranceEccentricitye1Label: TLabel;
    ToleranceEccentricitye2: TComboBox;
    ToleranceEccentricitye2Label: TLabel;
    ToleranceLoadF1: TComboBox;
    ToleranceLoadF1Label: TLabel;
    ToleranceLoadF2: TComboBox;
    ToleranceLoadF2Label: TLabel;
    ToleranceLengthL0: TComboBox;
    ToleranceLengthL0Label: TLabel;
    ToleranceWireDiameterUnit: TComboBox;
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
  QualityForm: TQualityForm;


implementation

{$R *.lfm}

uses
  ADim,
  SpringTolerances,
  {$IFDEF MODULE1} GeometryFrm1, {$ENDIF}
  {$IFDEF MODULE3} GeometryFrm3, {$ENDIF}
  baseutils, Setting;

// TQualityForm

procedure TQualityForm.FormCreate(Sender: TObject);
begin
  QualityForm.Top    := ClientFile.ReadInteger('QualityForm', 'Top',    QualityForm.Top);
  QualityForm.Left   := ClientFile.ReadInteger('QualityForm', 'Left',   QualityForm.Left);
  QualityForm.Height := ClientFile.ReadInteger('QualityForm', 'Height', QualityForm.Height);
  QualityForm.Width  := ClientFile.ReadInteger('QualityForm', 'Width',  QualityForm.Width);

  Clear;
end;

procedure TQualityForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate <> wsMaximized then
  begin
    ClientFile.WriteInteger('QualityForm', 'Top',    QualityForm.Top);
    ClientFile.WriteInteger('QualityForm', 'Left',   QualityForm.Left);
    ClientFile.WriteInteger('QualityForm', 'Height', QualityForm.Height);
    ClientFile.WriteInteger('QualityForm', 'Width',  QualityForm.Width);
  end;
end;

procedure TQualityForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TQualityForm.Clear;
begin
  ToleranceWireDiameter    .Value     := 0;
  ToleranceWireDiameterUnit.ItemIndex := 0;
  ToleranceCoilDiameter    .ItemIndex := 1;
  ToleranceLengthL0        .ItemIndex := 1;
  ToleranceLoadF1          .ItemIndex := 1;
  ToleranceLoadF2          .ItemIndex := 1;
  ToleranceEccentricitye1  .ItemIndex := 1;
  ToleranceEccentricitye2  .ItemIndex := 1;
end;

procedure TQualityForm.Load(IniFile: TIniFile);
begin
  ToleranceWireDiameter    .Value     := IniFile.ReadFloat  ('TQualityForm', 'ToleranceWireDiameter',     0);
  ToleranceWireDiameterUnit.ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceWireDiameterUnit', 0);
  ToleranceCoilDiameter    .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceCoilDiameter',     0);
  ToleranceLengthL0        .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceLengthL0',         0);
  ToleranceLoadF1          .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceLoadF1',           0);
  ToleranceLoadF2          .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceLoadF2',           0);
  ToleranceEccentricitye1  .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceEccentricitye1',   0);
  ToleranceEccentricitye2  .ItemIndex := IniFile.ReadInteger('TQualityForm', 'ToleranceEccentricitye2',   0);
end;

procedure TQualityForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TQualityForm', 'ToleranceWireDiameter',     ToleranceWireDiameter    .Value    );
  IniFile.WriteInteger('TQualityForm', 'ToleranceWireDiameterUnit', ToleranceWireDiameterUnit.ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceCoilDiameter',     ToleranceCoilDiameter    .ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceLengthL0',         ToleranceLengthL0        .ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceLoadF1',           ToleranceLoadF1          .ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceLoadF2',           ToleranceLoadF2          .ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceEccentricitye1',   ToleranceEccentricitye1  .ItemIndex);
  IniFile.WriteInteger('TQualityForm', 'ToleranceEccentricitye2',   ToleranceEccentricitye2  .ItemIndex);
end;

procedure TQualityForm.SaveToSolver;
begin
  {$IFDEF MODULE1}
  if Assigned(GeometryForm1) then
  begin
    SpringSolver.WireDiameterTolerance := 0*m;
    case ToleranceWireDiameterUnit.ItemIndex of
      0: SpringSolver.WireDiametertolerance := ToleranceWireDiameter.Value*mm;
      1: SpringSolver.WireDiametertolerance := ToleranceWireDiameter.Value*inch;
    end;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  if Assigned(GeometryForm3) then
  begin
    SpringSolver.WireDiameterTolerance := 0*m;
    case ToleranceWireDiameterUnit.ItemIndex of
      0: SpringSolver.WireDiametertolerance := ToleranceWireDiameter.Value*mm;
      1: SpringSolver.WireDiametertolerance := ToleranceWireDiameter.Value*inch;
    end;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  case ToleranceCoilDiameter.ItemIndex of
    0: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade1;
    1: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade2;
    2: SpringTolerance.QualityGradeOnCoilDiameter := QualityGrade3;
  end;
  case ToleranceLengthL0.ItemIndex of
    0: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade1;
    1: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade2;
    2: SpringTolerance.QualityGradeOnFreeBodyLength := QualityGrade3;
  end;
  case ToleranceLoadF1.ItemIndex of
    0: SpringTolerance.QualityGradeOnLoad1 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnLoad1 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnLoad1 := QualityGrade3;
  end;
  case ToleranceLoadF2.ItemIndex of
    0: SpringTolerance.QualityGradeOnLoad2 := QualityGrade1;
    1: SpringTolerance.QualityGradeOnLoad2 := QualityGrade2;
    2: SpringTolerance.QualityGradeOnLoad2 := QualityGrade3;
  end;
  case ToleranceEccentricitye1.ItemIndex of
    0: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade1;
    1: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade2;
    2: SpringTolerance.QualityGradeOnPerpendicularity := QualityGrade3;
  end;
  case ToleranceEccentricitye2.ItemIndex of
    0: SpringTolerance.QualityGradeOnParallelism := QualityGrade1;
    1: SpringTolerance.QualityGradeOnParallelism := QualityGrade2;
    2: SpringTolerance.QualityGradeOnParallelism := QualityGrade3;
  end;
  {$ENDIF}
end;

end.

