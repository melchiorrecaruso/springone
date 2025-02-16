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

unit TextFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, ExtCtrls, Forms,
  Graphics, IniFiles, StdCtrls, SysUtils;

type

  { TTextForm }

  TTextForm = class(TForm)
    Bevel1: TBevel;
    AuthorNameLabel: TLabel;
    AuthorName: TEdit;
    CompanyNameLabel: TLabel;
    CompanyName: TEdit;
    DrawingNameLabel: TLabel;
    DrawingName: TEdit;
    DrawingNumberLabel: TLabel;
    DrawingNumber: TEdit;
    Note1Label: TLabel;
    Note1: TEdit;
    Note2Label: TLabel;
    Note2: TEdit;
    CancelBtn: TBitBtn;
    OkBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public
    procedure Clear;
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
  end;


var
  TextForm: TTextForm;


implementation

{$R *.lfm}

uses
  Setting;

// TTextForm

procedure TTextForm.FormCreate(Sender: TObject);
begin
  TextForm.Top    := ClientFile.ReadInteger('TextForm', 'Top',    TextForm.Top);
  TextForm.Left   := ClientFile.ReadInteger('TextForm', 'Left',   TextForm.Left);
  TextForm.Height := ClientFile.ReadInteger('TextForm', 'Height', TextForm.Height);
  TextForm.Width  := ClientFile.ReadInteger('TextForm', 'Width',  TextForm.Width);

  Clear;
end;

procedure TTextForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('TextForm', 'Top',    TextForm.Top);
    ClientFile.WriteInteger('TextForm', 'Left',   TextForm.Left);
    ClientFile.WriteInteger('TextForm', 'Height', TextForm.Height);
    ClientFile.WriteInteger('TextForm', 'Width',  TextForm.Width);
  end;
end;

procedure TTextForm.Clear;
begin
  DrawingName  .Text := '';
  DrawingNumber.Text := '';
  AuthorName   .Text := '';
  CompanyName  .Text := '';
  Note1        .Text := '';
  Note2        .Text := '';
end;

procedure TTextForm.Load(IniFile: TIniFile);
begin
  DrawingName  .Text := IniFile.ReadString('TDrawingTextForm', 'DrawingName',   '');
  DrawingNumber.Text := IniFile.ReadString('TDrawingTextForm', 'DrawingNumber', '');
  AuthorName   .Text := IniFile.ReadString('TDrawingTextForm', 'AuthorName',    '');
  CompanyName  .Text := IniFile.ReadString('TDrawingTextForm', 'CompanyName',   '');
  Note1        .Text := IniFile.ReadString('TDrawingTextForm', 'Note1',         '');
  Note2        .Text := IniFile.ReadString('TDrawingTextForm', 'Note2',         '');
end;

procedure TTextForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteString('TDrawingTextForm', 'DrawingName',   DrawingName  .Text);
  IniFile.WriteString('TDrawingTextForm', 'DrawingNumber', DrawingNumber.Text);
  IniFile.WriteString('TDrawingTextForm', 'AuthorName',    AuthorName   .Text);
  IniFile.WriteString('TDrawingTextForm', 'CompanyName',   CompanyName  .Text);
  IniFile.WriteString('TDrawingTextForm', 'Note1',         Note1        .Text);
  IniFile.WriteString('TDrawingTextForm', 'Note2',         Note2        .Text);
end;

end.

