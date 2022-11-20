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

unit MaterialFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, EN10270, EN15800, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, ExtCtrls, Buttons, IniFiles;

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
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  MaterialForm: TMaterialForm;

implementation

{$R *.lfm}

uses
  ApplicationFrm, GeometryFrm, MainFrm, Math, ProductionFrm, QualityFrm, UtilsBase;

{ TMaterialForm }

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

procedure TMaterialForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TMaterialForm.Change(Sender: TObject);
begin
  YoungModulus       .Enabled := Material.Text = '';
  ShearModulus       .Enabled := YoungModulus.Enabled;
  TensileStrength    .Enabled := YoungModulus.Enabled;
  MaterialDensity    .Enabled := YoungModulus.Enabled;

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
    YoungModulusUnit    .ItemIndex := 0; // N/mm2
    ShearModulusUnit    .ItemIndex := 0; // N/mm2
    TensileStrengthUnit .ItemIndex := 0; // N/mm2
    MaterialDensityUnit .ItemIndex := 0; // kg/dm3

    YoungModulus        .Value := 0;
    ShearModulus        .Value := 0;
    TensileStrength     .Value := 0;
    MaterialDensity     .Value := 0;

    if GeometryForm.WireDiameter.Value > 0 then
    begin
      MAT.SetItem(Material.Text, GeometryForm.WireDiameter.Value,
        ApplicationForm.Temperature.Value, ProductionForm.WireSurface.Text);

      if MAT.ItemIndex = -1 then
      begin
        MAT.SetItem(Material.Text, GeometryForm.WireDiameter.Value,
          ApplicationForm.Temperature.Value, '');
      end;

      if MAT.ItemIndex <> -1 then
      begin
        YoungModulus   .Value := MAT.YoungModulusE20;
        ShearModulus   .Value := MAT.ShearModulusG20;
        TensileStrength.Value := MAT.TensileStrengthRm;
        MaterialDensity.Value := MAT.DensityRho;

        SpringTolerance.WireDiameterTolerance.Search(Material.Text, GeometryForm.WireDiameter.Value);
        if Assigned(QualityForm) then
        begin
          QualityForm.ToleranceWireDiameter.Value := SpringTolerance.WireDiameterTolerance.Value;
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

procedure TMaterialForm.ApplyBtnClick(Sender: TObject);
begin
  MainForm.Solve();
end;



end.

