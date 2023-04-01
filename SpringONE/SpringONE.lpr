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

program SpringONE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, Forms, ApplicationFrm1, DrawingFrm, TextFrm, AboutFrm,
  GeometryFrm1, GeometryFrm3, MainFrm, MaterialFrm, ProductionFrm, QualityFrm,
  ReportFrm, ApplicationFrm3, Compozer, SpringLib, LazControls, SysUtils,
  UtilsBase;

{$R *.res}

begin
  ApplicationName := 'SpringONE';
  ApplicationVer  := 'SpringONE v0.36';

  RequireDerivedFormResource:=True;
  Application.Title:='SpringOne';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TApplicationForm1, ApplicationForm1);
  Application.CreateForm(TApplicationForm3, ApplicationForm3);
  Application.CreateForm(TDrawingForm, DrawingForm);
  Application.CreateForm(TGeometryForm1, GeometryForm1);
  Application.CreateForm(TGeometryForm3, GeometryForm3);
  Application.CreateForm(TMaterialForm, MaterialForm);
  Application.CreateForm(TProductionForm, ProductionForm);
  Application.CreateForm(TQualityForm, QualityForm);
  Application.CreateForm(TReportForm, ReportForm);
  Application.CreateForm(TTextForm, TextForm);
  Application.Run;
end.

