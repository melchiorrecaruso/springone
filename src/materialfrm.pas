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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);    
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
  {$IFDEF MODULE1} QualityFrm1, {$ENDIF}
  {$IFDEF MODULE3} QualityFrm3, {$ENDIF}
  SpringTolerances,
  SpringMaterials,
  SpringSolvers,
  ProductionFrm,
  MainFrm,
  Setting,
  BaseUtils;

// TMaterialForm

procedure TMaterialForm.FormCreate(Sender: TObject);
var
  i: longint;
  m: string;
begin
  MaterialForm.Top    := ClientFile.ReadInteger('MaterialForm', 'Top',    MaterialForm.Top);
  MaterialForm.Left   := ClientFile.ReadInteger('MaterialForm', 'Left',   MaterialForm.Left);
  MaterialForm.Height := ClientFile.ReadInteger('MaterialForm', 'Height', MaterialForm.Height);
  MaterialForm.Width  := ClientFile.ReadInteger('MaterialForm', 'Width',  MaterialForm.Width);

  Clear;
  // Load materials from database
  Material.Items.Clear;
  Material.Sorted := True;
  Material.Items.Add('');
  for i := 0 to MAT.Count -1 do
  begin
    m := MAT.Names[i];
    if Material.Items.IndexOf(m) = -1 then
    begin
      Material.Items.Add(m);
    end;
  end;
  Material.ItemIndex := 1;
  Change(Sender);
end;

procedure TMaterialForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if WindowState = wsNormal then
  begin
    ClientFile.WriteInteger('MaterialForm', 'Top',    MaterialForm.Top);
    ClientFile.WriteInteger('MaterialForm', 'Left',   MaterialForm.Left);
    ClientFile.WriteInteger('MaterialForm', 'Height', MaterialForm.Height);
    ClientFile.WriteInteger('MaterialForm', 'Width',  MaterialForm.Width);
  end;
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

  {$IFDEF MODULE1}
  if Assigned(QualityForm1) then
  begin
    QualityForm1.ToleranceWireDiameter.Enabled := YoungModulus.Enabled;
  end;
  {$ENDIF}
  {$IFDEF MODULE3}
  if Assigned(QualityForm3) then
  begin
    QualityForm3.ToleranceOnWireDiameter.Enabled := YoungModulus.Enabled;
  end;
  {$ENDIF}

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
    if GreaterThanZero(SpringSolver.WireDiameter) then
    {$ENDIF}
    {$IFDEF MODULE3}
    GeometryForm3.SaveToSolver;
    if GreaterThanZero(SpringSolver.WireDiameter) then
    {$ENDIF}
    begin
      MAT.Load(Material.Text,
        {$IFDEF MODULE1} SpringSolver.WireDiameter, {$ENDIF}
        {$IFDEF MODULE3} SpringSolver.WireDiameter, {$ENDIF}
        {$IFDEF MODULE1} (ApplicationForm1.Temperature.Value*degC), {$ENDIF}
        {$IFDEF MODULE3} (ApplicationForm3.Temperature.Value*degC), {$ENDIF}
        ProductionForm.WireSurface.Text);

      if MAT.Name = '' then
        MAT.Load(Material.Text,
          {$IFDEF MODULE1} SpringSolver.WireDiameter, {$ENDIF}
          {$IFDEF MODULE3} SpringSolver.WireDiameter, {$ENDIF}
          {$IFDEF MODULE1} (ApplicationForm1.Temperature.Value*degC), {$ENDIF}
          {$IFDEF MODULE3} (ApplicationForm3.Temperature.Value*degC), {$ENDIF}
          '');

      if MAT.Name <> '' then
      begin
        YoungModulus   .Value := PascalUnit.ToFloat(MAT.YoungModulusE20, [pMega]);
        ShearModulus   .Value := PascalUnit.ToFloat(MAT.ShearModulusG20, [pMega]);
        TensileStrength.Value := PascalUnit.ToFloat(MAT.TensileStrengthRm, [pMega]);
        MaterialDensity.Value := KilogramPerCubicMeterUnit.ToFloat(MAT.DensityRho);

        SpringSolver.WireDiameterTolerance := WireTolerance(MAT.Regulation, MAT.WireDiameter);
        {$IFDEF MODULE1}
        if Assigned(QualityForm1) then
          case QualityForm1.ToleranceWireDiameterUnit.ItemIndex of
            0: QualityForm1.ToleranceWireDiameter.Value := MeterUnit.ToFloat(SpringSolver.WireDiameterTolerance, [pMilli]);
            1: QualityForm1.ToleranceWireDiameter.Value := InchUnit.ToFloat(SpringSolver.WireDiameterTolerance);
          end;
        {$ENDIF}
        {$IFDEF MODULE3}
        if Assigned(QualityForm3) then
          case QualityForm3.ToleranceOnWireDiameterUnit.ItemIndex of
            0: QualityForm3.ToleranceOnWireDiameter.Value := MeterUnit.ToFloat(SpringSolver.WireDiameterTolerance, [pMilli]);
            1: QualityForm3.ToleranceOnWireDiameter.Value := InchUnit.ToFloat(SpringSolver.WireDiameterTolerance);
          end;
        {$ENDIF}
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
  Material           .ItemIndex := IniFile.ReadInteger('TMaterialForm', 'Material',            0);
  YoungModulus       .Value     := IniFile.ReadFloat  ('TMaterialForm', 'YoungModulus',        0);
  YoungModulusUnit   .ItemIndex := IniFile.ReadInteger('TMaterialForm', 'YoungModulusUnit',    0);
  ShearModulus       .Value     := IniFile.ReadFloat  ('TMaterialForm', 'ShearModulus',        0);
  ShearModulusUnit   .ItemIndex := IniFile.ReadInteger('TMaterialForm', 'ShearModulusUnit',    0);
  TensileStrength    .Value     := IniFile.ReadFloat  ('TMaterialForm', 'TensileStrength',     0);
  TensileStrengthUnit.ItemIndex := IniFile.ReadInteger('TMaterialForm', 'TensileStrengthUnit', 0);
  MaterialDensity    .Value     := IniFile.ReadFloat  ('TMaterialForm', 'MaterialDensity',     0);
  MaterialDensityUnit.ItemIndex := IniFile.ReadInteger('TMaterialForm', 'MaterialDensityUnit', 0);
  CoilingType        .ItemIndex := IniFile.ReadInteger('TMaterialForm', 'CoilingType',         0);
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

