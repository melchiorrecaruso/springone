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

unit ProductionFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls, Spin, DividerBevel, IniFiles;

type

  { TProductionForm }

  TProductionForm = class(TForm)
    ACaseLB: TLabel;
    BCaseLb: TLabel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    CancelBtn: TBitBtn;
    CCaseLB: TLabel;
    DirectionCoils: TComboBox;
    BurringEnds: TComboBox;
    DividerBevel: TDividerBevel;
    L0nAndDeDi: TRadioButton;
    nAndd: TRadioButton;
    nAndDeDi: TRadioButton;
    L0: TRadioButton;
    L0nAndd: TRadioButton;
    WireSurface: TComboBox;
    LengthLsUnit: TComboBox;
    LengthLs: TFloatSpinEdit;
    DirectionCoilsLB: TLabel;
    BurringEndsLB: TLabel;
    WireSurfaceLB: TLabel;
    LengthLsLB: TLabel;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure EditingDone(Sender: TObject);

  private

  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  ProductionForm: TProductionForm;

implementation

{$R *.lfm}

uses
  Math, UtilsBase;

{ TProductionForm }

procedure TProductionForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TProductionForm.EditingDone(Sender: TObject);
begin
  TFloatSpinEdit(Sender).Value := Max(0, TFloatSpinEdit(Sender).Value);
end;

procedure TProductionForm.Clear;
begin
  DirectionCoils.ItemIndex := 0;
  BurringEnds   .ItemIndex := 0;
  WireSurface   .ItemIndex := 0;
  LengthLs      .Value     := 0;
  L0            .Checked   := True;
  nAndd         .Checked   := False;
  nAndDeDi      .Checked   := False;
  L0nAndd       .Checked   := False;
  L0nAndDeDi    .Checked   := False;
end;

procedure TProductionForm.Load(IniFile: TIniFile);
begin
  DirectionCoils.ItemIndex := TryTextToInt  (IniFile.ReadString('TProductionForm', 'DirectionCoilsIndex', '0'));
  BurringEnds   .ItemIndex := TryTextToInt  (IniFile.ReadString('TProductionForm', 'BurringEndsIndex',    '0'));
  WireSurface   .ItemIndex := TryTextToInt  (IniFile.ReadString('TProductionForm', 'WireSurfaceIndex',    '0'));
  LengthLsUnit  .ItemIndex := TryTextToInt  (IniFile.ReadString('TProductionForm', 'LengthLsIndex',       '0'));
  LengthLs      .Value     := TryTextToFloat(IniFile.ReadString('TProductionForm', 'LengthLs',            '0'));
  L0            .Checked   :=                IniFile.ReadBool  ('TProductionForm', 'L0',                  True);
  nAndd         .Checked   :=                IniFile.ReadBool  ('TProductionForm', 'nAndd',              False);
  nAndDeDi      .Checked   :=                IniFile.ReadBool  ('TProductionForm', 'nAndDeDi',           False);
  L0nAndd       .Checked   :=                IniFile.ReadBool  ('TProductionForm', 'L0nAndd',            False);
  L0nAndDeDi    .Checked   :=                IniFile.ReadBool  ('TProductionForm', 'L0nAndDeDi',         False);
end;

procedure TProductionForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteInteger('TProductionForm', 'DirectionCoilsIndex', DirectionCoils.ItemIndex );
  IniFile.WriteInteger('TProductionForm', 'BurringEndsIndex',    BurringEnds   .ItemIndex);
  IniFile.WriteInteger('TProductionForm', 'WireSurfaceIndex',    WireSurface   .ItemIndex);
  IniFile.WriteInteger('TProductionForm', 'LengthLsIndex',       LengthLsUnit  .ItemIndex);
  IniFile.WriteFloat  ('TProductionForm', 'LengthLs',            LengthLs      .Value);
  IniFile.WriteBool   ('TProductionForm', 'L0',                  L0            .Checked);
  IniFile.WriteBool   ('TProductionForm', 'nAndd',               nAndd         .Checked);
  IniFile.WriteBool   ('TProductionForm', 'nAndDeDi',            nAndDeDi      .Checked);
  IniFile.WriteBool   ('TProductionForm', 'L0nAndd',             L0nAndd       .Checked);
  IniFile.WriteBool   ('TProductionForm', 'L0nAndDeDi',          L0nAndDeDi    .Checked);
end;

end.

