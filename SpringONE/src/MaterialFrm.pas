{ EN13906-1 Helical Compression Spring Designer

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

unit MaterialFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, IniFiles, LibLink, Spin, StdCtrls, SysUtils;

type

  { TMaterialForm }

  TMaterialForm = class(TForm)
    Bevel1: TBevel;
    ApplyBtn: TBitBtn;
    CoilingType: TComboBox;
    CoilingTypeLabel: TLabel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    MaterialDensity: TFloatSpinEdit;
    MaterialDensityLabel: TLabel;
    MaterialDensityUnit: TComboBox;
    Material: TComboBox;
    MaterialLabel: TLabel;
    ShearModulus: TFloatSpinEdit;
    ShearModulusLabel: TLabel;
    ShearModulusUnit: TComboBox;
    TensileStrength: TFloatSpinEdit;
    TensileStrengthLabel: TLabel;
    TensileStrengthUnit: TComboBox;
    YoungModulus: TFloatSpinEdit;
    YoungModulusLabel: TLabel;
    YoungModulusUnit: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure Change(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;


var
  MaterialForm: TMaterialForm;


implementation

{$R *.lfm}

uses
  ADim,
  {$IFDEF MODULE1} ApplicationFrm1, {$ENDIF}
  {$IFDEF MODULE3} ApplicationFrm3, {$ENDIF}
  {$IFDEF MODULE1} GeometryFrm1, {$ENDIF}
  {$IFDEF MODULE3} GeometryFrm3, {$ENDIF}
  EN10270, EN13906, EN15800,
  MainFrm,
  ProductionFrm,
  QualityFrm,
  UtilsBase;

// TMaterialForm

procedure TMaterialForm.FormCreate(Sender: TObject);
var
  i: longint;
  m: string;
begin
  Clear;
  // Load materials from database
  Material.Items.Clear;
  Material.Sorted := True;
  Material.Items.Add('');
  for i := 0 to MAT.Count -1 do
  begin
    m := MAT.Items[i];
    if Material.Items.IndexOf(m) = -1 then
    begin
      Material.Items.Add(m);
    end;
  end;
  Material.ItemIndex := 1;
  Change(Sender);
end;

procedure TMaterialForm.Change(Sender: TObject);
begin
  YoungModulus   .Enabled := Material.Text = '';
  ShearModulus   .Enabled := YoungModulus.Enabled;
  TensileStrength.Enabled := YoungModulus.Enabled;
  MaterialDensity.Enabled := YoungModulus.Enabled;

  YoungModulusUnit   .Enabled := YoungModulus.Enabled;
  ShearModulusUnit   .Enabled := YoungModulus.Enabled;
  TensileStrengthUnit.Enabled := YoungModulus.Enabled;
  MaterialDensityUnit.Enabled := YoungModulus.Enabled;

  if Assigned(QualityForm) then
  begin
    QualityForm.ToleranceWireDiameter.Enabled := YoungModulus.Enabled;
  end;

  MAT.Clear;
  if Material.Text <> '' then
  begin
    YoungModulusUnit   .ItemIndex := 0; // MPa
    ShearModulusUnit   .ItemIndex := 0; // MPa
    TensileStrengthUnit.ItemIndex := 0; // MPa
    MaterialDensityUnit.ItemIndex := 0; // kg/m3

    YoungModulus   .Value := 0;
    ShearModulus   .Value := 0;
    TensileStrength.Value := 0;
    MaterialDensity.Value := 0;

    {$IFDEF MODULE1}
    GeometryForm1.SaveToSolver;
    if SpringSolver.WireDiameter.Value > 0 then
    {$ENDIF}
    {$IFDEF MODULE3}
    GeometryForm3.SaveToSolver;
    if SpringSolver.WireDiameter.Value > 0 then
    {$ENDIF}
    begin
      MAT.SetItem(Material.Text,
        {$IFDEF MODULE1} SpringSolver.WireDiameter, {$ENDIF}
        {$IFDEF MODULE3} SpringSolver.WireDiameter, {$ENDIF}
        {$IFDEF MODULE1} ApplicationForm1.Temperature.Value, {$ENDIF}
        {$IFDEF MODULE3} ApplicationForm3.Temperature.Value, {$ENDIF}
        ProductionForm.WireSurface.Text);

      if MAT.ItemIndex = -1 then
        MAT.SetItem(Material.Text,
          {$IFDEF MODULE1} SpringSolver.WireDiameter, {$ENDIF}
          {$IFDEF MODULE3} SpringSolver.WireDiameter, {$ENDIF}
          {$IFDEF MODULE1} ApplicationForm1.Temperature.Value,  {$ENDIF}
          {$IFDEF MODULE3} ApplicationForm3.Temperature.Value,  {$ENDIF}
          '');

      if MAT.ItemIndex <> -1 then
      begin
        YoungModulus   .Value := MAT.YoungModulusE20  .Value([pMega]);
        ShearModulus   .Value := MAT.ShearModulusG20  .Value([pMega]);
        TensileStrength.Value := MAT.TensileStrengthRm.Value([pMega]);
        MaterialDensity.Value := MAT.DensityRho.Value;

        WireTolerance.Search(Material.Text,
          {$IFDEF MODULE1} GeometryForm1.WireDiameter.Value*mm); {$ENDIF}
          {$IFDEF MODULE3} GeometryForm3.WireDiameter.Value*mm); {$ENDIF}

        if Assigned(QualityForm) then
        begin
          case QualityForm.ToleranceWireDiameterUnit.ItemIndex of
            0: QualityForm.ToleranceWireDiameter.Value := WireTolerance.Value.Value([pMilli]);
            1: QualityForm.ToleranceWireDiameter.Value := WireTolerance.Value.Value([pMilli])/25.4;
          end;
        end;
      end;
    end;

  end;
end;

procedure TMaterialForm.Clear;
begin
  Material.ItemIndex            := 0;
  YoungModulus       .Value     := 0;
  YoungModulusUnit   .ItemIndex := 0;
  ShearModulus       .Value     := 0;
  ShearModulusUnit   .ItemIndex := 0;
  TensileStrength    .Value     := 0;
  TensileStrengthUnit.ItemIndex := 0;
  MaterialDensity    .Value     := 0;
  MaterialDensityUnit.ItemIndex := 0;
  CoilingType        .ItemIndex := 0;
end;

procedure TMaterialForm.Load(IniFile: TIniFile);
begin
  Material           .ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'Material',            '0'));
  YoungModulus       .Value     := TryTextToFloat(IniFile.ReadString('TMaterialForm', 'YoungModulus',        '0'));
  YoungModulusUnit   .ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'YoungModulusUnit',    '0'));
  ShearModulus       .Value     := TryTextToFloat(IniFile.ReadString('TMaterialForm', 'ShearModulus',        '0'));
  ShearModulusUnit   .ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'ShearModulusUnit',    '0'));
  TensileStrength    .Value     := TryTextToFloat(IniFile.ReadString('TMaterialForm', 'TensileStrength',     '0'));
  TensileStrengthUnit.ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'TensileStrengthUnit', '0'));
  MaterialDensity    .Value     := TryTextToFloat(IniFile.ReadString('TMaterialForm', 'MaterialDensity',     '0'));
  MaterialDensityUnit.ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'MaterialDensityUnit', '0'));
  CoilingType        .ItemIndex := TryTextToInt  (IniFile.ReadString('TMaterialForm', 'CoilingType',         '0'));
end;

procedure TMaterialForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TMaterialForm', 'Material',            Material           .ItemIndex);
  IniFile.WriteFloat  ('TMaterialForm', 'YoungModulus',        YoungModulus       .Value    );
  IniFile.WriteInteger('TMaterialForm', 'YoungModulusUnit',    YoungModulusUnit   .ItemIndex);
  IniFile.WriteFloat  ('TMaterialForm', 'ShearModulus',        ShearModulus       .Value    );
  IniFile.WriteInteger('TMaterialForm', 'ShearModulusUnit',    ShearModulusUnit   .ItemIndex);
  IniFile.WriteFloat  ('TMaterialForm', 'TensileStrength',     TensileStrength    .Value    );
  IniFile.WriteInteger('TMaterialForm', 'TensileStrengthUnit', TensileStrengthUnit.ItemIndex);
  IniFile.WriteFloat  ('TMaterialForm', 'MaterialDensity',     MaterialDensity    .Value    );
  IniFile.WriteInteger('TMaterialForm', 'MaterialDensityUnit', MaterialDensityUnit.ItemIndex);
  IniFile.WriteInteger('TMaterialForm', 'CoilingType',         CoilingType        .ItemIndex);
end;

procedure TMaterialForm.SaveToSolver;
begin
  Change(nil);
  if Material.Text = '' then
  begin
    case YoungModulusUnit.ItemIndex of
      {$IFDEF MODULE1} 0: SpringSolver.YoungModulus := YoungModulus.Value*MPa; {$ENDIF}
      {$IFDEF MODULE3} 0: SpringSolver.YoungModulus := YoungModulus.Value*MPa; {$ENDIF}
    end;
    case ShearModulusUnit.ItemIndex of
      {$IFDEF MODULE1} 0: SpringSolver.ShearModulus := ShearModulus.Value*MPa; {$ENDIF}
      {$IFDEF MODULE3} 0: SpringSolver.ShearModulus := ShearModulus.Value*MPa; {$ENDIF}
    end;
  end else
  begin
    {$IFDEF MODULE1}
    SpringSolver.YoungModulus := MAT.YoungModulusE;
    SpringSolver.ShearModulus := MAT.ShearModulusG;
    {$ENDIF}
    {$IFDEF MODULE3}
    SpringSolver.YoungModulus := MAT.YoungModulusE;
    SpringSolver.ShearModulus := MAT.ShearModulusG;
    {$ENDIF}
  end;

  case MaterialForm.TensileStrengthUnit.ItemIndex of
    {$IFDEF MODULE1} 0: SpringSolver.TensileStrengthRm := MaterialForm.TensileStrength.Value*MPa; {$ENDIF}
    {$IFDEF MODULE3} 0: SpringSolver.TensileStrengthRm := MaterialForm.TensileStrength.Value*MPa; {$ENDIF}
  end;

  case MaterialForm.MaterialDensityUnit.ItemIndex of
    {$IFDEF MODULE1} 0: SpringSolver.MaterialDensity := MaterialForm.MaterialDensity.Value*kg/m3; {$ENDIF}
    {$IFDEF MODULE3} 0: SpringSolver.MaterialDensity := MaterialForm.MaterialDensity.Value*kg/m3; {$ENDIF}
  end;

  case MaterialForm.CoilingType.ItemIndex of
    {$IFDEF MODULE1}
    0: SpringSolver.ColdCoiled := True;
    1: SpringSolver.ColdCoiled := False;
    {$ENDIF}
    {$IFDEF MODULE3}
    0: SpringSolver.ColdCoiled := True;
    1: SpringSolver.ColdCoiled := False;
    {$ENDIF}
  end;
end;

procedure TMaterialForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TMaterialForm.ApplyBtnClick(Sender: TObject);
begin
  MainForm.Solve();
end;

end.

