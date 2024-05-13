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

unit ReportFrm;

{$mode ObjFPC}{$H+}
{$i defines.inc}

interface

uses
  Buttons, Classes, Controls, Dialogs, SpringTolerances, SpringSolvers, IniFiles,
  SpringMaterials, Forms, Graphics, LibLink, StdCtrls, Printers, PrintersDlgs,
  SysUtils;

type

  { TReportForm }

  TReportForm = class(TForm)
    Memo: TMemo;
    CloseBtn: TBitBtn;
    PrintBtn: TBitBtn;
    PrintDialog: TPrintDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
  private
    PageRowCount: longint;
    AddRowAtBegin: boolean;
    AddRowAtEnd: boolean;
  public
    procedure CreateReport;
  end;


var
  ReportForm: TReportForm;


implementation

{$R *.lfm}

uses
   ApplicationFrm1, TextFrm, MaterialFrm, QualityFrm,

   {$IFDEF MODULE1} GeometryFrm1, {$ENDIF}
   {$IFDEF MODULE3} GeometryFrm3, {$ENDIF}

   ADim, BaseUtils, Setting;

// TReportForm

procedure TReportForm.CreateReport;
var
  i: longint;
  S: TStringList;
begin
  S := TStringList.Create;
  // PAG.1
  S.Add(Format('   +--------------------------------------------------------------------+',[]));
  S.Add(Format('   ¦  %-15s Helical Compression Spring (EN13906-1)            ¦',[ApplicationVer]));
  S.Add(Format('   ¦  %-64s  ¦',[DateTimeToStr(Now)]));
  S.Add(Format('   +--------------------------------------------------------------------+',[]));
  S.Add(Format('   ¦  Description  : %-49s  ¦',[TextForm.DrawingName  .Text]));
  S.Add(Format('   ¦  No.          : %-49s  ¦',[TextForm.DrawingNumber.Text]));
  S.Add(Format('   ¦  Author       : %-49s  ¦',[TextForm.AuthorName   .Text]));
  S.Add(Format('   ¦  Company Name : %-49s  ¦',[TextForm.CompanyName  .Text]));
  S.Add(Format('   ¦  Note 1       : %-49s  ¦',[TextForm.Note1        .Text]));
  S.Add(Format('   ¦  Note 2       : %-49s  ¦',[TextForm.Note2        .Text]));
  S.Add(Format('   +--------------------------------------------------------------------+',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MAIN DIMENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire diameter                      d         %s', [GetString(SpringSolver.WireDiameter, SpringSolver.WireDiameterTolerance)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inside diameter                    Di        %s', [GetString(SpringSolver.Di)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                 Dm        %s', [GetString(SpringSolver.Dm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Outside diameter                   De        %s', [GetString(SpringSolver.De)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Number of active coils             n         %0.2f', [SpringSolver.ActiveColis]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  {$IFDEF MODULE1}
  S.Add(Format('   Inactive end coils lower side                %0.2f', [GeometryForm1.InactiveCoil1.Value]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inactive end coils upper side                %0.2f', [GeometryForm1.InactiveCoil2.Value]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Total number of coils              nt        %0.2f', [SpringSolver.TotalCoils]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring ends                                  %s', [GeometryForm1.EndCoilType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LENGTHS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Free length                        L0        %s', [GetString(SpringSolver.LengthL0, SpringTolerance.ToleranceFreeBodyLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F1           L1        %s', [GetString(SpringSolver.LengthL1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F2           L2        %s', [GetString(SpringSolver.LengthL2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring length               Ln        %s', [GetString(SpringSolver.LengthLn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Solid length of spring             Lc        %s', [GetString(SpringSolver.LengthLc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MATERIAL',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Description                                  %s', [MaterialForm.Material.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Modulus of elasticity              E         %s', [GetString(SpringSolver.YoungModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear modulus                      G         %s', [GetString(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Density                            rho       %s', [GetString(SpringSolver.MaterialDensity)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Tensile strength                   Rm        %s', [GetString(SpringSolver.TensileStrengthRm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.shear stress                   tauz      %s', [GetString(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Operating temperature              Temp      %s', [GetString(SpringSolver.Temperature)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear-modulus at working temp.     GTemp     %s', [GetString(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.2
  S.Add(Format('   SPRING TRAVELS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stroke                             sh        %s', [GetString(SpringSolver.StrokeSh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                      s1        %s', [GetString(SpringSolver.StrokeS1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                      s2        %s', [GetString(SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring travel               sn        %s', [GetString(SpringSolver.StrokeSn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel to Lc                sc        %s', [GetString(SpringSolver.StrokeSc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Travel until buckling              sk        %s', [GetString(SpringSolver.DeflectionSk)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LOAD',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L1                  F1        %s', [GetString(SpringSolver.LoadF1, SpringTolerance.ToleranceOnLoad1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L2                  F2        %s', [GetString(SpringSolver.LoadF2, SpringTolerance.ToleranceOnLoad2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Ln                  Fn        %s', [GetString(SpringSolver.LoadFn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Lc                  Fc        %s', [GetString(SpringSolver.LoadFc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   TENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F1                 tau 1     %s', [GetString(SpringSolver.TorsionalStressTau1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F2                 tau 2     %s', [GetString(SpringSolver.TorsionalStressTau2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fn                 tau n     %s', [GetString(SpringSolver.TorsionalStressTaun)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fc                 tau c     %s', [GetString(SpringSolver.TorsionalStressTauc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stress coefficient                 k         %0.2f', [SpringSolver.CorrectionFactorK]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F1       tau k1    %s', [GetString(SpringSolver.TorsionalStressTauk1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F2       tau k2    %s', [GetString(SpringSolver.TorsionalStressTauk2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Variation between loads            tau kh    %s', [GetString(SpringSolver.TorsionalStressTaukh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm. shear stress                  tau z     %s', [GetString(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   QUALITY GRADE',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation De,Di,Dm :                     %s', [QualityForm.ToleranceCoilDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation L0       :                     %s', [QualityForm.ToleranceLengthL0.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F1       :                     %s', [QualityForm.ToleranceLoadF1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F2       :                     %s', [QualityForm.ToleranceLoadF2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e1       :                     %s', [QualityForm.ToleranceEccentricitye1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e2       :                     %s', [QualityForm.ToleranceEccentricitye2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation d        :                   ± %s', [GetString(QualityForm.ToleranceWireDiameter.Value*mm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.3
  S.Add(Format('   DETAILS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Manufacturing                                %s',[MaterialForm.CoilingType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Load type                                    %s',[ApplicationForm1.LoadType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Seating application coefficient    nu        %0.2f', [SpringSolver.SeatingCoefficent]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of surface line          e1        %s', [GetString(SpringSolver.EccentricityE1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of parallelism           e2        %s', [GetString(SpringSolver.EccentricityE2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring rate                        R         %s', [GetString(SpringSolver.SpringRateR)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Weight of the spring               m         %s', [GetString(SpringSolver.Mass)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Natural frequency spring           fe        %s', [GetString(SpringSolver.NaturalFrequency)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Increase in coil diameter at Lc    ΔDe       %s', [GetString(SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                 Dmc       %s', [GetString(SpringSolver.Dm + SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum clearance of active coils  Sa        %s', [GetString(SpringSolver.SumOfMinimumGapsSa)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Actual clearance of active coils   sc-s2     %s', [GetString(SpringSolver.StrokeSc - SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Coiling ratio                      w         %0.3f', [SpringSolver.SpringIndexW]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                        tan α     %0.3f', [SpringSolver.PitchRatio]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch                              P         %s', [GetString(SpringSolver.Pitch)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if SpringSolver.Pitch.Value <> 0 then
    S.Add(Format('   Pitch ratio                        Dm/P      %0.3f', [SpringSolver.Dm / SpringSolver.Pitch]))
  else
    S.Add(Format('   Pitch ratio                        Dm/P      ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if SpringSolver.Dm.Value <> 0 then
    S.Add(Format('   Lean grade                         lambda    %0.3f', [SpringSolver.LengthL0 / SpringSolver.Dm]))
  else
    S.Add(Format('   Lean grade                         lambda    ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring energy between s1 and s2    W12       %s', [GetString(SpringSolver.SpringWorkW12)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire length                        L         %s', [GetString(SpringSolver.WireLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if SpringSolver.Dm.Value> 0 then
    S.Add(Format('   Pressure                           R/D       %s', [GetString(SpringSolver.SpringRateR / SpringSolver.Dm)]))
  else
    S.Add(Format('   Pressure                           R/D       ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Max.diameter of bedding mandrel    DiMin     %s', [GetString(SpringSolver.DiMin)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum diameter of bore           DeMax     %s', [GetString(SpringSolver.DeMax)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   REMARKS, WARNINGS AND ERROR MESSAGES',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  {$ENDIF}

  for i := 0 to ErrorMessage.Count   -1 do S.Add(Format('   %s', [ErrorMessage  [i]]));
  for i := 0 to WarningMessage.Count -1 do S.Add(Format('   %s', [WarningMessage[i]]));

  Memo.Clear;
  for i := 0 to S.Count -1 do
  begin
    Memo.Lines.Add(S[i]);
  end;
  S.Destroy;
end;

procedure TReportForm.PrintBtnClick(Sender: TObject);
var
  I, J: longint;
  S: TStringList;
begin
  if PrintDialog.Execute then
  begin
    Printer.Title := '';
    Printer.RawMode:= True;
    Printer.BeginDoc;

    I := 0;
    J := 0;
    S := TStringList.Create;
    while (I < Memo.Lines.Count) do
    begin
      if AddRowAtBegin then S.Add('');
      if AddRowAtEnd   then S.Add('');
      while (S.Count < PageRowCount) and
            (I  <  Memo.Lines.Count) do
      begin
        S.Add(Memo.Lines[I]);
        Inc(I);
      end;
      Inc(J);
      S.Add('');
      S.Add(Format('%75s', [Format('pag. %d',[J])]));

      if AddRowAtEnd then S.Delete(0);
      if AddRowAtEnd then S.Add('');
      Printer.Write(S.Text);
      S.Clear;
    end;
    S.Destroy;
    Printer.EndDoc;
  end;
end;

procedure TReportForm.FormCreate(Sender: TObject);
begin
  ReportForm.Top    := ClientFile.ReadInteger('ReportForm', 'Top',    ReportForm.Top);
  ReportForm.Left   := ClientFile.ReadInteger('ReportForm', 'Left',   ReportForm.Left);
  ReportForm.Height := ClientFile.ReadInteger('ReportForm', 'Height', ReportForm.Height);
  ReportForm.Width  := ClientFile.ReadInteger('ReportForm', 'Width',  ReportForm.Width);

  PageRowCount      := ClientFile.ReadInteger('ReportForm', 'PageRowCount',     65);
  AddRowAtBegin     := ClientFile.ReadBool   ('ReportForm', 'AddRowAtBegin', False);
  AddRowAtEnd       := ClientFile.ReadBool   ('ReportForm', 'AddRowAtEnd',   False);

  if AddRowAtBegin then Dec(PageRowCount);
  if AddRowAtEnd   then Dec(PageRowCount);
end;

procedure TReportForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('ReportForm', 'Top',    ReportForm.Top);
    ClientFile.WriteInteger('ReportForm', 'Left',   ReportForm.Left);
    ClientFile.WriteInteger('ReportForm', 'Height', ReportForm.Height);
    ClientFile.WriteInteger('ReportForm', 'Width',  ReportForm.Width);
  end;
  ClientFile.WriteInteger('ReportForm', 'PageRowCount',  PageRowCount);
  ClientFile.WriteBool   ('ReportForm', 'AddRowAtBegin', AddRowAtBegin);
  ClientFile.WriteBool   ('ReportForm', 'AddRowAtEnd',   AddRowAtEnd);
end;

end.

