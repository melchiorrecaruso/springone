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

unit ReportFrm;

{$mode ObjFPC}{$H+}

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

  public
    procedure CreateReport;
  end;


var
  ReportForm: TReportForm;


implementation

{$R *.lfm}

uses
   ApplicationFrm1, TextFrm, MaterialFrm,

   {$IFDEF MODULE1} GeometryFrm1, {$ENDIF}
   {$IFDEF MODULE3} GeometryFrm3, {$ENDIF}
   {$IFDEF MODULE1} QualityFrm1,  {$ENDIF}
   {$IFDEF MODULE3} QualityFrm3,  {$ENDIF}

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
  S.Add(Format('   Wire diameter                      d         %s', [GetLengthString(SpringSolver.WireDiameter, SpringSolver.WireDiameterTolerance)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inside diameter                    Di        %s', [GetLengthString(SpringSolver.Di)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                 Dm        %s', [GetLengthString(SpringSolver.Dm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Outside diameter                   De        %s', [GetLengthString(SpringSolver.De, SpringTolerance.ToleranceOnCoilDiameter)]));
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
  S.Add(Format('   Free length                        L0        %s', [GetLengthString(SpringSolver.LengthL0, SpringTolerance.ToleranceFreeBodyLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F1           L1        %s', [GetLengthString(SpringSolver.LengthL1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F2           L2        %s', [GetLengthString(SpringSolver.LengthL2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring length               Ln        %s', [GetLengthString(SpringSolver.LengthLn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Solid length of spring             Lc        %s', [GetLengthString(SpringSolver.LengthLc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MATERIAL',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Description                                  %s', [MaterialForm.Material.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Modulus of elasticity              E         %s', [GetPressureString(SpringSolver.YoungModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear modulus                      G         %s', [GetPressureString(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Density                            rho       %s', [GetDensityString(SpringSolver.MaterialDensity)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Tensile strength                   Rm        %s', [GetPressureString(SpringSolver.TensileStrengthRm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.shear stress                   tauz      %s', [GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Operating temperature              Temp      %s', [GetTemperatureString(SpringSolver.Temperature)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear-modulus at working temp.     GTemp     %s', [GetPressureString(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.2
  S.Add(Format('   SPRING TRAVELS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stroke                             sh        %s', [GetLengthString(SpringSolver.StrokeSh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                      s1        %s', [GetLengthString(SpringSolver.StrokeS1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                      s2        %s', [GetLengthString(SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring travel               sn        %s', [GetLengthString(SpringSolver.StrokeSn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel to Lc                sc        %s', [GetLengthString(SpringSolver.StrokeSc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Travel until buckling              sk        %s', [GetLengthString(SpringSolver.DeflectionSk)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LOAD',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L1                  F1        %s', [GetForceString(SpringSolver.LoadF1, SpringTolerance.ToleranceOnLoad1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L2                  F2        %s', [GetForceString(SpringSolver.LoadF2, SpringTolerance.ToleranceOnLoad2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Ln                  Fn        %s', [GetForceString(SpringSolver.LoadFn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Lc                  Fc        %s', [GetForceString(SpringSolver.LoadFc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   TENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F1                 tau 1     %s', [GetPressureString(SpringSolver.TorsionalStressTau1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F2                 tau 2     %s', [GetPressureString(SpringSolver.TorsionalStressTau2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fn                 tau n     %s', [GetPressureString(SpringSolver.TorsionalStressTaun)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fc                 tau c     %s', [GetPressureString(SpringSolver.TorsionalStressTauc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stress coefficient                 k         %0.2f', [SpringSolver.CorrectionFactorK]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F1       tau k1    %s', [GetPressureString(SpringSolver.TorsionalStressTauk1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F2       tau k2    %s', [GetPressureString(SpringSolver.TorsionalStressTauk2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Variation between loads            tau kh    %s', [GetPressureString(SpringSolver.TorsionalStressTaukh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm. shear stress                  tau z     %s', [GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   QUALITY GRADE',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation De,Di,Dm :                     %s', [QualityForm1.GradeOnCoilDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation L0       :                     %s', [QualityForm1.GradeOnLengthL0.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F1       :                     %s', [QualityForm1.GradeOnLoadF1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F2       :                     %s', [QualityForm1.GradeOnLoadF2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e1       :                     %s', [QualityForm1.GradeOnEccentricitye1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e2       :                     %s', [QualityForm1.GradeOnEccentricitye2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation d        :                   ± %s', [GetLengthString(QualityForm1.ToleranceOnWireDiameter.Value * mm)]));
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
  S.Add(Format('   Deviation of surface line          e1        %s', [GetLengthString(SpringSolver.EccentricityE1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of parallelism           e2        %s', [GetLengthString(SpringSolver.EccentricityE2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring rate                        R         %s', [GetStiffnessString(SpringSolver.SpringRateR)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Weight of the spring               m         %s', [GetMassString(SpringSolver.Mass)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Natural frequency spring           fe        %s', [GetFrequencyString(SpringSolver.NaturalFrequency)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Increase in coil diameter at Lc    ΔDe       %s', [GetLengthString(SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                 Dmc       %s', [GetLengthString(SpringSolver.Dm + SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum clearance of active coils  Sa        %s', [GetLengthString(SpringSolver.SumOfMinimumGapsSa)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Actual clearance of active coils   sc-s2     %s', [GetLengthString(SpringSolver.StrokeSc - SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Coiling ratio                      w         %0.3f', [SpringSolver.SpringIndexW]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                        tan α     %0.3f', [ScalarUnit.Tofloat(SpringSolver.PitchRatio)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch                              P         %s', [GetLengthString(SpringSolver.Pitch)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if GreaterThanZero(SpringSolver.Pitch) then
    S.Add(Format('   Pitch ratio                        Dm/P      %0.3f', [ScalarUnit.ToFloat(SpringSolver.Dm / SpringSolver.Pitch)]))
  else
    S.Add(Format('   Pitch ratio                        Dm/P      ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if GreaterThanZero(SpringSolver.Dm) then
    S.Add(Format('   Lean grade                         lambda    %0.3f', [ScalarUnit.ToFloat(SpringSolver.LengthL0 / SpringSolver.Dm)]))
  else
    S.Add(Format('   Lean grade                         lambda    ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring energy between s1 and s2    W12       %s', [GetEnergyString(SpringSolver.SpringWorkW12)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire length                        L         %s', [GetLengthString(SpringSolver.WireLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  if GreaterThanZero(SpringSolver.Dm) then
    S.Add(Format('   Pressure                           R/D       %s', [GetPressureString(SpringSolver.SpringRateR / SpringSolver.Dm)]))
  else
    S.Add(Format('   Pressure                           R/D       ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Max.diameter of bedding mandrel    DiMin     %s', [GetLengthString(SpringSolver.DiMin)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum diameter of bore           DeMax     %s', [GetLengthString(SpringSolver.DeMax)]));
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
  I, PageIndex: longint;
  XPos, YPos, YInc: longint;
begin
  if PrintDialog.Execute then
  begin
    PageIndex := 1;
    Printer.BeginDoc;
    Printer.Canvas.Font.Name   := Memo.Font.Name;
    Printer.Canvas.Font.Height := Printer.PageHeight div 80;

    XPos := Printer.XDPI;
    YPos := Printer.YDPI;
    YInc := Printer.Canvas.TextHeight('T');

    for I := 0 to Memo.Lines.Count - 1 do
    begin
      if (YPos + YInc) >= (Printer.PageHeight - Printer.YDPI) then
      begin
         Printer.Canvas.TextOut(XPos, Printer.PageHeight - Printer.YDPI + YInc, Format('%73s', [Format('pag. %d',[PageIndex])]));
         Printer.NewPage;

         YPos := Printer.YDPI;
         Inc(PageIndex);
      end;

      Printer.Canvas.TextOut(XPos, YPos, Memo.Lines[I]);
      YPos := YPos + YInc;
    end;
    Printer.Canvas.TextOut(XPos, Printer.PageHeight - Printer.YDPI + YInc, Format('%73s', [Format('pag. %d',[PageIndex])]));
    Printer.EndDoc;
  end;
end;

procedure TReportForm.FormCreate(Sender: TObject);
begin
  ReportForm.Top    := ClientFile.ReadInteger('ReportForm', 'Top',    ReportForm.Top);
  ReportForm.Left   := ClientFile.ReadInteger('ReportForm', 'Left',   ReportForm.Left);
  ReportForm.Height := ClientFile.ReadInteger('ReportForm', 'Height', ReportForm.Height);
  ReportForm.Width  := ClientFile.ReadInteger('ReportForm', 'Width',  ReportForm.Width);
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
end;

end.

