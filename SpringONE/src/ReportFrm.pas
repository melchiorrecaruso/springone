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
  Forms, Graphics, StdCtrls, SysUtils, UtilsBase;

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
   ApplicationFrm1, TextFrm, GeometryFrm1, MaterialFrm, QualityFrm, ADim;

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
  S.Add(Format('   Wire diameter                     d        mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.WireDiameter).Value), TryFormatFloat(' ± %s', '', mm.From(SOLVER1.WireDiameterMax - SOLVER1.WireDiameter).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inside diameter                   Di       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.Di).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dm       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.Dm).Value), TryFormatFloat(' ± %s', '', mm.From(TOL.CoilDiameterTolerance).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Outside diameter                  De       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.De).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Number of active coils            n                    %s',[TryFormatFloat('%s', '---', SOLVER1.ActiveColis)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inactive end coils lower side                          %s',[TryFormatFloat('%s', '---', GeometryForm1.InactiveCoil1.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inactive end coils upper side                          %s',[TryFormatFloat('%s', '---', GeometryForm1.InactiveCoil2.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));


  S.Add(Format('   Total number of coils             nt                   %s',[TryFormatFloat('%s', '---', SOLVER1.TotalCoils)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));



  S.Add(Format('   Spring ends %58s',[GeometryForm1.EndCoilType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LENGTHS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Free length                       L0       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.LengthL0).Value), TryFormatFloat(' ± %s', '', mm.From(TOL.LengthL0Tolerance).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F1          L1       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.LengthL1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F2          L2       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.LengthL2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring length              Ln       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.LengthLn).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Solid length of spring            Lc       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.LengthLc).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MATERIAL',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Description %58s',[MaterialForm.Material.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Modulus of elasticity             E        MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.YoungModulus).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear modulus                     G        MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.ShearModulus).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Density                           rho      kg/dm^3     %s',[TryFormatFloat('%s', '---', (kg/m3).From(SOLVER1.MaterialDensity).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Tensile strength                  Rm       MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TensileStrengthRm).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.shear stress                  tauz     MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.AdmStaticTorsionalStressTauz).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Operating temperature             Temp    °C           %s',[TryFormatFloat('%s', '---', ApplicationForm1.Temperature.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear-modulus at working temp.    GTemp    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.ShearModulus).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.2
  S.Add(Format('   SPRING TRAVELS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stroke                            sh       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeSh).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s1       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeS1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s2       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeS2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring travel              sn       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeSn).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel to Lc               sc       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeSc).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Travel until buckling             sk       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.DeflectionSk).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LOAD',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L1                 F1       N           %s',[TryFormatFloat('%s', '---', N.From(SOLVER1.LoadF1).Value), TryFormatFloat(' ± %s', '', N.From(TOL.LoadF1Tolerance).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L2                 F2       N           %s',[TryFormatFloat('%s', '---', N.From(SOLVER1.LoadF2).Value), TryFormatFloat(' ± %s', '', N.From(TOL.LoadF2Tolerance).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Ln                 Fn       N           %s',[TryFormatFloat('%s', '---', N.From(SOLVER1.LoadFn).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Lc                 Fc       N           %s',[TryFormatFloat('%s', '---', N.From(SOLVER1.LoadFc).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   TENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F1                tau 1    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTau1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F2                tau 2    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTau2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fn                tau n    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTaun).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fc                tau c    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTauc).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stress coefficient                k                    %s',[TryFormatFloat('%s', '---', SOLVER1.CorrectionFactorK)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F1      tau k1   MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTauk1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F2      tau k2   MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTauk2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Variation between loads           tau kh   MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.TorsionalStressTaukh).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm. shear stress                 tau z    MPa         %s',[TryFormatFloat('%s', '---', MPa.From(SOLVER1.AdmStaticTorsionalStressTauz).Value)]));
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
  S.Add(Format('   Seating application coefficient   nu                   %s',[TryFormatFloat('%s', '---', SOLVER1.SeatingCoefficent)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of surface line         e1       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.EccentricityE1).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of parallelism          e2       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.EccentricityE2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring rate                       R        N/mm        %s',[TryFormatFloat('%s', '---', (N/mm).From(SOLVER1.SpringRateR).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Weight of the spring              m        g           %s',[TryFormatFloat('%s', '---', g.From(SOLVER1.Mass).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Natural frequency spring          fe       Hz          %s',[TryFormatFloat('%s', '---', Hz.From(SOLVER1.NaturalFrequency).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Increase in coil diameter at Lc   deltaDe  mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.DeltaDe).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dmc      mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.Dm + SOLVER1.DeltaDe).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum clearance of active coils Sa       mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.SumOfMinimumGapsSa).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Actual clearance of active coils  sc-s2    mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.StrokeSc - SOLVER1.StrokeS2).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Coiling ratio                     w                    %s',[TryFormatFloat('%s', '---', SOLVER1.SpringIndexW)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                       tan alfa             %s',[TryFormatFloat('%s', '---', SOLVER1.PitchRatio)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch                             P        mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.Pitch).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                       Dm/P                 %s',[TryFormatFloatDiv('%s', '---', mm.From(SOLVER1.Dm).Value, mm.From(SOLVER1.Pitch).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Lean grade                        lambda               %s',[TryFormatFloatDiv('%s', '---', mm.From(SOLVER1.LengthL0).Value, mm.From(SOLVER1.Dm).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring energy between s1 and s2   W12      Nmm         %s',[TryFormatFloat('%s', '---', J.From(SOLVER1.SpringWorkW12).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire length                       L        mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.WireLength).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pressure                          R/D      MPa         %s',[TryFormatFloatDiv('%s', '---', (N/mm).From(SOLVER1.SpringRateR).Value, mm.From(SOLVER1.Dm).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Max.diameter of bedding mandrel   Di min   mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.DiMin).Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum diameter of bore          De max   mm          %s',[TryFormatFloat('%s', '---', mm.From(SOLVER1.DeMax).Value)]));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   REMARKS, WARNINGS AND ERROR MESSAGES',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));

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

