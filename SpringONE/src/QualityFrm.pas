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

unit QualityFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, ExtCtrls, IniFiles;

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
    procedure FormCreate(Sender: TObject);
    procedure ToleranceWireDiameterChange(Sender: TObject);
  private

  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  QualityForm: TQualityForm;

implementation

{$R *.lfm}

uses
  Math, UtilsBase;

procedure TQualityForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TQualityForm.ToleranceWireDiameterChange(Sender: TObject);
begin
  TFloatSpinEdit(Sender).Value := Max(0, TFloatSpinEdit(Sender).Value);
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
  ToleranceWireDiameter    .Value     := TryTextToFloat(IniFile.ReadString('TQualityForm', 'ToleranceWireDiameter',     '0'));
  ToleranceWireDiameterUnit.ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceWireDiameterUnit', '0'));
  ToleranceCoilDiameter    .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceCoilDiameter',     '0'));
  ToleranceLengthL0        .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceLengthL0',         '0'));
  ToleranceLoadF1          .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceLoadF1',           '0'));
  ToleranceLoadF2          .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceLoadF2',           '0'));
  ToleranceEccentricitye1  .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceEccentricitye1',   '0'));
  ToleranceEccentricitye2  .ItemIndex := TryTextToInt  (IniFile.ReadString('TQualityForm', 'ToleranceEccentricitye2',   '0'));
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

end.

