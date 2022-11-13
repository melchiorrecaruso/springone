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

unit DrawingTextFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ExtCtrls, DividerBevel, IniFiles;

type

  { TDrawingTextForm }

  TDrawingTextForm = class(TForm)
    AuthorName: TEdit;
    AuthorNameLabel: TLabel;
    Bevel1: TBevel;
    OkBtn: TBitBtn;
    CancelBtn: TBitBtn;
    CompanyName: TEdit;
    Note1: TEdit;
    Note2: TEdit;
    CompanyNameLabel: TLabel;
    Note1Label: TLabel;
    Note2Label: TLabel;
    DrawingName: TEdit;
    DrawingNameLabel: TLabel;
    DrawingNumber: TEdit;
    DrawingNumberLabel: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Load(IniFile: TIniFile);
    procedure Save(IniFile: TIniFile);
    procedure Clear;
  end;

var
  DrawingTextForm: TDrawingTextForm;

implementation

{$R *.lfm}

procedure TDrawingTextForm.FormCreate(Sender: TObject);
begin
  Clear;
end;

procedure TDrawingTextForm.Clear;
begin
  DrawingName  .Text := '';
  DrawingNumber.Text := '';
  AuthorName   .Text := '';
  CompanyName  .Text := '';
  Note1        .Text := '';
  Note2        .Text := '';
end;

procedure TDrawingTextForm.Load(IniFile: TIniFile);
begin
  DrawingName  .Text := IniFile.ReadString('TDrawingTextForm', 'DrawingName',   '');
  DrawingNumber.Text := IniFile.ReadString('TDrawingTextForm', 'DrawingNumber', '');
  AuthorName   .Text := IniFile.ReadString('TDrawingTextForm', 'AuthorName',    '');
  CompanyName  .Text := IniFile.ReadString('TDrawingTextForm', 'CompanyName',   '');
  Note1        .Text := IniFile.ReadString('TDrawingTextForm', 'Note1',         '');
  Note2        .Text := IniFile.ReadString('TDrawingTextForm', 'Note2',         '');
end;

procedure TDrawingTextForm.Save(IniFile: TIniFile);
begin
  IniFile.WriteString('TDrawingTextForm', 'DrawingName',   DrawingName  .Text);
  IniFile.WriteString('TDrawingTextForm', 'DrawingNumber', DrawingNumber.Text);
  IniFile.WriteString('TDrawingTextForm', 'AuthorName',    AuthorName   .Text);
  IniFile.WriteString('TDrawingTextForm', 'CompanyName',   CompanyName  .Text);
  IniFile.WriteString('TDrawingTextForm', 'Note1',         Note1        .Text);
  IniFile.WriteString('TDrawingTextForm', 'Note2',         Note2        .Text);
end;

end.

