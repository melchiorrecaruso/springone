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

unit AboutFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls, ExtCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AboutCopyrigthLB: TLabel;
    AboutDescriptionLB: TLabel;
    AboutLicenseLB: TLabel;
    AboutLinkLB: TLabel;
    AboutNameLB: TLabel;
    AboutVersionLB: TLabel;
    Image: TImage;
    procedure AboutLinkLBClick(Sender: TObject);
    procedure AboutLinkLBMouseLeave(Sender: TObject);
    procedure AboutLinkLBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf;

{ TAboutForm }

procedure TAboutForm.AboutLinkLBClick(Sender: TObject);
begin
  openurl('https://github.com/melchiorrecaruso');
end;

procedure TAboutForm.AboutLinkLBMouseLeave(Sender: TObject);
begin
  AboutLinkLB.Font.Color := clDefault;
end;

procedure TAboutForm.AboutLinkLBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  AboutLinkLB.Font.Color := clBlue;
end;

end.

