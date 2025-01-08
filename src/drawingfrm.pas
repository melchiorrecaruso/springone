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

unit DrawingFrm;

{$mode ObjFPC}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, Springsolvers, ExtCtrls,
  Forms, Graphics, IniFiles, LibLink, Spin, StdCtrls, SysUtils;

type

  { TDrawingForm }

  TDrawingForm = class(TForm)
    Bevel1: TBevel;
    L0Btn: TBitBtn;
    L1Btn: TBitBtn;
    L2Btn: TBitBtn;
    LcBtn: TBitBtn;
    SpringLength: TFloatSpinEdit;

    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    SpringLengthLabel: TLabel;
    procedure BtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  private

  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;


var
  DrawingForm: TDrawingForm;


implementation

{$R *.lfm}

uses
  ADim, Setting;

// TDrawingForm

procedure TDrawingForm.FormCreate(Sender: TObject);
begin
  DrawingForm.Top    := ClientFile.ReadInteger('DrawingForm', 'Top',    DrawingForm.Top);
  DrawingForm.Left   := ClientFile.ReadInteger('DrawingForm', 'Left',   DrawingForm.Left);
  DrawingForm.Height := ClientFile.ReadInteger('DrawingForm', 'Height', DrawingForm.Height);
  DrawingForm.Width  := ClientFile.ReadInteger('DrawingForm', 'Width',  DrawingForm.Width);

  Clear;
end;

procedure TDrawingForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('DrawingForm', 'Top',    DrawingForm.Top);
    ClientFile.WriteInteger('DrawingForm', 'Left',   DrawingForm.Left);
    ClientFile.WriteInteger('DrawingForm', 'Height', DrawingForm.Height);
    ClientFile.WriteInteger('DrawingForm', 'Width',  DrawingForm.Width);
  end;
end;

procedure TDrawingForm.FormResize(Sender: TObject);
begin
  L0Btn.Constraints.MinWidth := (SpringLength.Width - 30) div 4;
  L1Btn.Constraints.MinWidth := L0Btn.Constraints.MinWidth;
  L2Btn.Constraints.MinWidth := L0Btn.Constraints.MinWidth;
  LcBtn.Constraints.MinWidth := L0Btn.Constraints.MinWidth;

  L0Btn.Constraints.MaxWidth := L0Btn.Constraints.MinWidth;
  L1Btn.Constraints.MaxWidth := L0Btn.Constraints.MinWidth;
  L2Btn.Constraints.MaxWidth := L0Btn.Constraints.MinWidth;
  LcBtn.Constraints.MaxWidth := L0Btn.Constraints.MinWidth;
end;

procedure TDrawingForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinedit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;

procedure TDrawingForm.Clear;
begin
  // nothing to do
end;

procedure TDrawingForm.BtnClick(Sender: TObject);
begin
  {$IFDEF MODULE1}
  if Sender = L0Btn  then SpringLength.Value := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
  if Sender = L1Btn  then SpringLength.Value := MeterUnit.ToFloat(SpringSolver.LengthL1, [pMilli]);
  if Sender = L2Btn  then SpringLength.Value := MeterUnit.ToFloat(SpringSolver.LengthL2, [pMilli]);
  if Sender = LcBtn  then SpringLength.Value := MeterUnit.ToFloat(SpringSolver.LengthLc, [pMilli]);
  {$ENDIF}
end;

procedure TDrawingForm.Load(IniFile: TIniFile);
begin
  // nothing to do
end;

procedure TDrawingForm.Save(IniFile: TIniFile);
begin
  // nothing to do
end;

end.

