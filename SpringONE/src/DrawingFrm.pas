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

unit DrawingFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, ComCtrls, Controls, Dialogs, EN13906_1,
  ExtCtrls, Forms, Graphics, Spin, StdCtrls, SysUtils;

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
    procedure SpinEditChange(Sender: TObject);
  private

  public

  end;

var
  DrawingForm: TDrawingForm;

implementation

{$R *.lfm}

{ TDrawingForm }

procedure TDrawingForm.BtnClick(Sender: TObject);
begin
  if Sender = L0Btn  then SpringLength.Value := SpringSolver.LengthL0;
  if Sender = L1Btn  then SpringLength.Value := SpringSolver.LengthL1;
  if Sender = L2Btn  then SpringLength.Value := SpringSolver.LengthL2;
  if Sender = LcBtn  then SpringLength.Value := SpringSolver.LengthLc;
end;

procedure TDrawingForm.SpinEditChange(Sender: TObject);
begin
  if TFloatSpinedit(Sender).Value < 0 then TFloatSpinEdit(Sender).Value := 0;
end;


end.

