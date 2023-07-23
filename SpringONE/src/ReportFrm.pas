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

unit ReportFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Buttons, Classes, Controls, Dialogs, EN10270, EN13906, EN15800,
  Forms, Graphics, LibLink, StdCtrls, SysUtils, UtilsBase;

type

  { TReportForm }

  TReportForm = class(TForm)
    CloseBtn: TBitBtn;
    PrintBtn: TBitBtn;
    Memo: TMemo;
  private
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

   ADim;

// TReportForm

procedure TReportForm.CreateReport;
const
  RowCount = 65;
var
  i: longint;
  PageIndex: longint;
  PageCount: longint;
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
  S.Add(Format('   Wire diameter                     d        mm          %s',[TryFormatFloat('%s', '---', SpringSolver.WireDiameter.Value([pMilli])), TryFormatFloat(' ± %s', '', (SpringSolver.WireDiameterMax - SpringSolver.WireDiameter).Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inside diameter                   Di       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.Di.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dm       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.Dm.Value([pMilli])), TryFormatFloat(' ± %s', '',SpringTolerance.ToleranceOnCoilDiameter.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Outside diameter                  De       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.De.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Number of active coils            n                    %s',[TryFormatFloat('%s', '---', SpringSolver.ActiveColis)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  {$IFDEF MODULE1}
  S.Add(Format('   Inactive end coils lower side                          %s',[TryFormatFloat('%s', '---', GeometryForm1.InactiveCoil1.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inactive end coils upper side                          %s',[TryFormatFloat('%s', '---', GeometryForm1.InactiveCoil2.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));


  S.Add(Format('   Total number of coils             nt                   %s',[TryFormatFloat('%s', '---', SpringSolver.TotalCoils)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));



  S.Add(Format('   Spring ends %58s',[GeometryForm1.EndCoilType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LENGTHS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Free length                       L0       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.LengthL0.Value([pMilli ])), TryFormatFloat(' ± %s', '', SpringTolerance.ToleranceFreeBodyLength.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F1          L1       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.LengthL1.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F2          L2       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.LengthL2.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring length              Ln       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.LengthLn.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Solid length of spring            Lc       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.LengthLc.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MATERIAL',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Description %58s',[MaterialForm.Material.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Modulus of elasticity             E        MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.YoungModulus.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear modulus                     G        MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.ShearModulus.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Density                           rho      g/dm^3      %s',[TryFormatFloat('%s', '---', SpringSolver.MaterialDensity.Value([pNone, pNone]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Tensile strength                  Rm       MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TensileStrengthRm.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.shear stress                  tauz     MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.AdmStaticTorsionalStressTauz.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Operating temperature             Temp    °C           %s',[TryFormatFloat('%s', '---', ApplicationForm1.Temperature.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear-modulus at working temp.    GTemp    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.ShearModulus.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.2
  S.Add(Format('   SPRING TRAVELS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stroke                            sh       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.StrokeSh.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s1       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.StrokeS1.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s2       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.StrokeS2.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring travel              sn       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.StrokeSn.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel to Lc               sc       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.StrokeSc.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Travel until buckling             sk       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.DeflectionSk.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LOAD',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L1                 F1       N           %s',[TryFormatFloat('%s', '---', TNewtonUnitId.From(SpringSolver.LoadF1).Value), TryFormatFloat(' ± %s', '', TNewtonUnitId.From(SpringTolerance.ToleranceOnLoad1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L2                 F2       N           %s',[TryFormatFloat('%s', '---', TNewtonUnitId.From(SpringSolver.LoadF2).Value), TryFormatFloat(' ± %s', '', TNewtonUnitId.From(SpringTolerance.ToleranceOnLoad2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Ln                 Fn       N           %s',[TryFormatFloat('%s', '---', TNewtonUnitId.From(SpringSolver.LoadFn).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Lc                 Fc       N           %s',[TryFormatFloat('%s', '---', TNewtonUnitId.From(SpringSolver.LoadFc).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   TENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F1                tau 1    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTau1.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F2                tau 2    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTau2.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fn                tau n    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTaun.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fc                tau c    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTauc.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stress coefficient                k                    %s',[TryFormatFloat('%s', '---', SpringSolver.CorrectionFactorK)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F1      tau k1   MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTauk1.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F2      tau k2   MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTauk2.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Variation between loads           tau kh   MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.TorsionalStressTaukh.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm. shear stress                 tau z    MPa         %s',[TryFormatFloat('%s', '---', SpringSolver.AdmStaticTorsionalStressTauz.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   QUALITY GRADE',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation De,Di,Dm : %s',[QualityForm.ToleranceCoilDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation L0       : %s',[QualityForm.ToleranceLengthL0.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F1       : %s',[QualityForm.ToleranceLoadF1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F2       : %s',[QualityForm.ToleranceLoadF2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e1       : %s',[QualityForm.ToleranceEccentricitye1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e2       : %s',[QualityForm.ToleranceEccentricitye2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation d        : ±%s mm',[QualityForm.ToleranceWireDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('',[]));
  // PAG.3
  S.Add(Format('   DETAILS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Manufacturing %56s',[MaterialForm.CoilingType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Load type     %56s',[ApplicationForm1.LoadType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Seating application coefficient   nu                   %s',[TryFormatFloat('%s', '---', SpringSolver.SeatingCoefficent)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of surface line         e1       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.EccentricityE1.Value([pMega]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of parallelism          e2       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.EccentricityE2.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring rate                       R        N/mm        %s',[TryFormatFloat('%s', '---', SpringSolver.SpringRateR.Value([pNone, pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Weight of the spring              m        g           %s',[TryFormatFloat('%s', '---', SpringSolver.Mass.Value([pNone]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Natural frequency spring          fe       Hz          %s',[TryFormatFloat('%s', '---', SpringSolver.NaturalFrequency.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Increase in coil diameter at Lc   deltaDe  mm          %s',[TryFormatFloat('%s', '---', SpringSolver.DeltaDe.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dmc      mm          %s',[TryFormatFloat('%s', '---', (SpringSolver.Dm + SpringSolver.DeltaDe).Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum clearance of active coils Sa       mm          %s',[TryFormatFloat('%s', '---', SpringSolver.SumOfMinimumGapsSa.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Actual clearance of active coils  sc-s2    mm          %s',[TryFormatFloat('%s', '---', (SpringSolver.StrokeSc - SpringSolver.StrokeS2).Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Coiling ratio                     w                    %s',[TryFormatFloat('%s', '---', SpringSolver.SpringIndexW)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                       tan alfa             %s',[TryFormatFloat('%s', '---', SpringSolver.PitchRatio)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch                             P        mm          %s',[TryFormatFloat('%s', '---', SpringSolver.Pitch.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                       Dm/P                 %s',[TryFormatFloatDiv('%s', '---', SpringSolver.Dm.Value([pMilli]), SpringSolver.Pitch.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Lean grade                        lambda               %s',[TryFormatFloatDiv('%s', '---', SpringSolver.LengthL0.Value([pMilli]), SpringSolver.Dm.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring energy between s1 and s2   W12      Nmm         %s',[TryFormatFloat('%s', '---', SpringSolver.SpringWorkW12.Value([]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire length                       L        mm          %s',[TryFormatFloat('%s', '---', SpringSolver.WireLength.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pressure                          R/D      MPa         %s',[TryFormatFloatDiv('%s', '---', SpringSolver.SpringRateR.Value([pNone, pMilli]), SpringSolver.Dm.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Max.diameter of bedding mandrel   Di min   mm          %s',[TryFormatFloat('%s', '---', SpringSolver.DiMin.Value([pMilli]))]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum diameter of bore          De max   mm          %s',[TryFormatFloat('%s', '---', SpringSolver.DeMax.Value([pMilli]))]));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   REMARKS, WARNINGS AND ERROR MESSAGES',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

  {$ENDIF}

  for i := 0 to ErrorMessage.Count   -1 do S.Add(Format('   %s', [ErrorMessage  [i]]));
  for i := 0 to WarningMessage.Count -1 do S.Add(Format('   %s', [WarningMessage[i]]));
  while (S.Count mod RowCount) <> 0 do S.Add('');

  PageIndex := 1;
  PageCount := S.Count div RowCount;

  Memo.Clear;
  for i := 0 to S.Count -1 do
  begin
    Memo.Lines.Add(S[i]);
    if (i > 0) and (((i + 1) mod RowCount) = 0) then
    begin
      Memo.Lines.Add(Format('%73s', [Format('pag. %d/%d',[PageIndex, PageCount])]));
      Inc(PageIndex);
    end;
  end;
  S.Destroy;
end;

end.

