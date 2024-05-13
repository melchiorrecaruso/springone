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

unit GeometryFrm1;

{$mode ObjFPC}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, IniFiles, Spin, StdCtrls, SysUtils;

type

  { TGeometryForm1 }

  TGeometryForm1 = class(TForm)
    ActiveCoilLabel: TLabel;
    ActiveCoil: TFloatSpinEdit;
    Bevel1: TBevel;
    CoilDiameterIndex: TComboBox;
    CoilDiameter: TFloatSpinEdit;
    CoilDiameterUnit: TComboBox;
    EndCoilTypeLabel: TLabel;
    EndCoilType: TComboBox;
    InactiveCoil1Label: TLabel;
    InactiveCoil1: TFloatSpinEdit;
    InactiveCoil2Label: TLabel;
    InactiveCoil2: TFloatSpinEdit;
    LengthL0Label: TLabel;
    LengthL0: TFloatSpinEdit;
    LengthL0Unit: TComboBox;
    LengthL1Label: TLabel;
    LengthL1: TFloatSpinEdit;
    LengthL1Unit: TComboBox;
    LengthL2Label: TLabel;
    LengthL2: TFloatSpinEdit;
    LengthL2Unit: TComboBox;
    WireDiameterLabel: TLabel;
    WireDiameter: TFloatSpinEdit;
    WireDiameterUnit: TComboBox;
    ApplyBtn: TBitBtn;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
  private

  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure SaveToSolver;
  end;


var
  GeometryForm1: TGeometryForm1;


implementation

{$R *.lfm}

uses
  ADim, LibLink, MainFrm, baseutils, Setting;

// TGeometryForm1

procedure TGeometryForm1.FormCreate(Sender: TObject);
begin
  GeometryForm1.Top    := ClientFile.ReadInteger('GeometryForm1', 'Top',    GeometryForm1.Top);
  GeometryForm1.Left   := ClientFile.ReadInteger('GeometryForm1', 'Left',   GeometryForm1.Left);
  GeometryForm1.Height := ClientFile.ReadInteger('GeometryForm1', 'Height', GeometryForm1.Height);
  GeometryForm1.Width  := ClientFile.ReadInteger('GeometryForm1', 'Width',  GeometryForm1.Width);

  Clear;
end;

procedure TGeometryForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('GeometryForm1', 'Top',    GeometryForm1.Top);
    ClientFile.WriteInteger('GeometryForm1', 'Left',   GeometryForm1.Left);
    ClientFile.WriteInteger('GeometryForm1', 'Height', GeometryForm1.Height);
    ClientFile.WriteInteger('GeometryForm1', 'Width',  GeometryForm1.Width);
  end;
end;

procedure TGeometryForm1.Clear;
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

procedure TGeometryForm1.Load(IniFile: TIniFile);
begin
  WireDiameter     .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'WireDiameter',      0);
  WireDiameterUnit .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'WireDiameterUnit',  0);
  CoilDiameterIndex.ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'CoilDiameterIndex', 0);
  CoilDiameter     .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'CoilDiameter',      0);
  CoilDiameterUnit .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'CoilDiameterUnit',  0);
  ActiveCoil       .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'ActiveCoil',        0);
  InactiveCoil1    .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'InactiveCoil1',     0);
  InactiveCoil2    .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'InactiveCoil2',     0);
  LengthL0         .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'LengthL0',          0);
  LengthL0Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'LengthL0Unit',      0);
  LengthL1         .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'LengthL1',          0);
  LengthL1Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'LengthL1Unit',      0);
  LengthL2         .Value     := IniFile.ReadFloat  ('TGeometryForm1', 'LengthL2',          0);
  LengthL2Unit     .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'LengthL2Unit',      0);
  EndCoilType      .ItemIndex := IniFile.ReadInteger('TGeometryForm1', 'EndCoilType',       0);
end;

procedure TGeometryForm1.Save(IniFile: TIniFile);
begin
  IniFile.WriteFloat  ('TGeometryForm1', 'WireDiameter',      WireDiameter     .Value    );
  IniFile.WriteInteger('TGeometryForm1', 'WireDiameterUnit',  WireDiameterUnit .ItemIndex);
  IniFile.WriteInteger('TGeometryForm1', 'CoilDiameterIndex', CoilDiameterIndex.ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm1', 'CoilDiameter',      CoilDiameter     .Value    );
  IniFile.WriteInteger('TGeometryForm1', 'CoilDiameterUnit',  CoilDiameterUnit .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm1', 'ActiveCoil',        ActiveCoil       .Value    );
  IniFile.WriteFloat  ('TGeometryForm1', 'InactiveCoil1',     InactiveCoil1    .Value    );
  IniFile.WriteFloat  ('TGeometryForm1', 'InactiveCoil2',     InactiveCoil2    .Value    );
  IniFile.WriteFloat  ('TGeometryForm1', 'LengthL0',          LengthL0         .Value    );
  IniFile.WriteInteger('TGeometryForm1', 'LengthL0Unit',      LengthL0Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm1', 'LengthL1',          LengthL1         .Value    );
  IniFile.WriteInteger('TGeometryForm1', 'LengthL1Unit',      LengthL1Unit     .ItemIndex);
  IniFile.WriteFloat  ('TGeometryForm1', 'LengthL2',          LengthL2         .Value    );
  IniFile.WriteInteger('TGeometryForm1', 'LengthL2Unit',      LengthL2Unit     .ItemIndex);
  IniFile.WriteInteger('TGeometryForm1', 'EndCoilType',       EndCoilType      .ItemIndex);
end;

procedure TGeometryForm1.SaveToSolver;
begin
  case WireDiameterUnit.ItemIndex of
    0: SpringSolver.WireDiameter := WireDiameter.Value*mm;
    1: SpringSolver.WireDiameter := WireDiameter.Value*25.4*mm;
  end;

  case CoilDiameterUnit.ItemIndex of
    0: SpringSolver.Dm := CoilDiameter.Value*mm;
    1: SpringSolver.Dm := CoilDiameter.Value*25.4*mm;
  end;

  case CoilDiameterIndex.ItemIndex of
    0: SpringSolver.Dm := SpringSolver.Dm + SpringSolver.WireDiameter;  // Input Di
    1: SpringSolver.Dm := SpringSolver.Dm;                              // Input Dm
    2: SpringSolver.Dm := SpringSolver.Dm - SpringSolver.WireDiameter;  // Input De
  end;

  SpringSolver.ActiveColis := ActiveCoil.Value;
  SpringSolver.TotalCoils  := ActiveCoil.Value + InactiveCoil1.Value + InactiveCoil2.Value;

  SpringSolver.ClosedEnds := EndCoilType.ItemIndex in [0, 1];
  SpringSolver.GroundEnds := EndCoilType.ItemIndex in [   1];

  case LengthL0Unit.ItemIndex of
    0: SpringSolver.LengthL0 := LengthL0.Value*mm;
    1: SpringSolver.LengthL0 := LengthL0.Value*25.4*mm;
  end;

  case LengthL1Unit.ItemIndex of
    0: SpringSolver.LengthL1 := LengthL1.Value*mm;
    1: SpringSolver.LengthL1 := LengthL1.Value*25.4*mm;
  end;

  case LengthL2Unit.ItemIndex of
    0: SpringSolver.LengthL2 := LengthL2.Value*mm;
    1: SpringSolver.LengthL2 := LengthL2.Value*25.4*mm;
  end;
end;

procedure TGeometryForm1.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinEdit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TGeometryForm1.ApplyBtnClick(Sender: TObject);
begin
  MainForm.Solve();
end;

end.

