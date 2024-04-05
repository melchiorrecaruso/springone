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

unit AboutFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, StdCtrls, SysUtils;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    AboutCopyrigthLabel: TLabel;
    AboutDescriptionLabel: TLabel;
    AboutLicenseLabel: TLabel;
    AboutLinkLabel: TLabel;
    AboutNameLabel: TLabel;
    AboutVersionLabel: TLabel;
    Image: TImage;
    procedure AboutLinkLabelClick(Sender: TObject);
    procedure AboutLinkLabelMouseLeave(Sender: TObject);
    procedure AboutLinkLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;


var
  AboutForm: TAboutForm;


implementation

{$R *.lfm}

uses
  LCLIntf, Setting;

// TAboutForm

procedure TAboutForm.AboutLinkLabelClick(Sender: TObject);
begin
  OpenURL('https://github.com/melchiorrecaruso');
end;

procedure TAboutForm.AboutLinkLabelMouseLeave(Sender: TObject);
begin
  AboutLinkLabel.Font.Color := clDefault;
end;

procedure TAboutForm.AboutLinkLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  AboutLinkLabel.Font.Color := clBlue;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  AboutForm.Top    := ClientFile.ReadInteger('AboutForm', 'Top',    AboutForm.Top);
  AboutForm.Left   := ClientFile.ReadInteger('AboutForm', 'Left',   AboutForm.Left);
  AboutForm.Height := ClientFile.ReadInteger('AboutForm', 'Height', AboutForm.Height);
  AboutForm.Width  := ClientFile.ReadInteger('AboutForm', 'Width',  AboutForm.Width);
end;

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate <> wsMaximized then
  begin
    ClientFile.WriteInteger('AboutForm', 'Top',    AboutForm.Top);
    ClientFile.WriteInteger('AboutForm', 'Left',   AboutForm.Left);
    ClientFile.WriteInteger('AboutForm', 'Height', AboutForm.Height);
    ClientFile.WriteInteger('AboutForm', 'Width',  AboutForm.Width);
  end;
end;

end.

