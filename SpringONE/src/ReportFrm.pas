{ EN13906-1 Helical Compression Spring Designer

  Copyright (C) 2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  Forms, Graphics, LibLink, StdCtrls, SysUtils;

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

   ADim, UtilsBase;

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
  S.Add(Format('   Wire diameter                     d       %s', [TryFormat(SpringSolver.WireDiameter,
                                                                             SpringSolver.WireDiameterMax -
                                                                             SpringSolver.WireDiameter)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inside diameter                   Di      %s', [TryFormat(SpringSolver.Di)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dm      %s', [TryFormat(SpringSolver.Dm,
                                                                             SpringTolerance.ToleranceOnCoilDiameter)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Outside diameter                  De      %s', [TryFormat(SpringSolver.De)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Number of active coils            n              %s', [TryFormat(SpringSolver.ActiveColis)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  {$IFDEF MODULE1}
  S.Add(Format('   Inactive end coils lower side                    %s', [TryFormat(GeometryForm1.InactiveCoil1.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Inactive end coils upper side                    %s', [TryFormat(GeometryForm1.InactiveCoil2.Value)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Total number of coils             nt             %s', [TryFormat(SpringSolver.TotalCoils)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring ends                       %s', [GeometryForm1.EndCoilType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LENGTHS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Free length                       L0      %s', [TryFormat(SpringSolver.LengthL0,
                                                                             SpringTolerance.ToleranceFreeBodyLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F1          L1      %s', [TryFormat(SpringSolver.LengthL1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring length at load F2          L2      %s', [Tryformat(SpringSolver.LengthL2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring length              Ln      %s', [TryFormat(SpringSolver.LengthLn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Solid length of spring            Lc      %s', [TryFormat(SpringSolver.LengthLc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   MATERIAL',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Description                       %s', [MaterialForm.Material.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Modulus of elasticity             E       %s', [TryFormat(SpringSolver.YoungModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear modulus                     G       %s', [TryFormat(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Density                           rho     %s', [TryFormat(SpringSolver.MaterialDensity)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Tensile strength                  Rm      %s', [TryFormat(SpringSolver.TensileStrengthRm)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.shear stress                  tauz    %s', [TryFormat(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Operating temperature             Temp    %s', [TryFormat(SpringSolver.Temperature)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear-modulus at working temp.    GTemp   %s', [TryFormat(SpringSolver.ShearModulus)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  // PAG.2
  S.Add(Format('   SPRING TRAVELS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stroke                            sh      %s', [TryFormat(SpringSolver.StrokeSh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s1      %s', [TryFormat(SpringSolver.StrokeS1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel                     s2      %s', [TryFormat(SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Usable spring travel              sn      %s', [TryFormat(SpringSolver.StrokeSn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring travel to Lc               sc      %s', [TryFormat(SpringSolver.StrokeSc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Travel until buckling             sk      %s', [TryFormat(SpringSolver.DeflectionSk)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   SPRING LOAD',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L1                 F1      %s', [TryFormat(SpringSolver.LoadF1, SpringTolerance.ToleranceOnLoad1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at L2                 F2      %s', [TryFormat(SpringSolver.LoadF2, SpringTolerance.ToleranceOnLoad2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Ln                 Fn      %s', [TryFormat(SpringSolver.LoadFn)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring load at Lc                 Fc      %s', [TryFormat(SpringSolver.LoadFc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   TENSIONS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F1                tau-1   %s', [TryFormat(SpringSolver.TorsionalStressTau1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at F2                tau-2   %s', [TryFormat(SpringSolver.TorsionalStressTau2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fn                tau-n   %s', [TryFormat(SpringSolver.TorsionalStressTaun)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Shear stress at Fc                tau-c   %s', [TryFormat(SpringSolver.TorsionalStressTauc)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Stress coefficient                k              %s', [TryFormat(SpringSolver.CorrectionFactorK)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F1      tau-k1  %s', [TryFormat(SpringSolver.TorsionalStressTauk1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Corrected shear stress at F2      tau-k2  %s', [TryFormat(SpringSolver.TorsionalStressTauk2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Variation between loads           tau-kh  %s', [TryFormat(SpringSolver.TorsionalStressTaukh)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm. shear stress                 tau-z   %s', [TryFormat(SpringSolver.AdmStaticTorsionalStressTauz)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('   QUALITY GRADE',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation De,Di,Dm : %s', [QualityForm.ToleranceCoilDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation L0       : %s', [QualityForm.ToleranceLengthL0.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F1       : %s', [QualityForm.ToleranceLoadF1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation F2       : %s', [QualityForm.ToleranceLoadF2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e1       : %s', [QualityForm.ToleranceEccentricitye1.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation e2       : %s', [QualityForm.ToleranceEccentricitye2.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Adm.deviation d        : ±%s mm', [QualityForm.ToleranceWireDiameter.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('',[]));
  S.Add(Format('',[]));
  // PAG.3
  S.Add(Format('   DETAILS',[]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Manufacturing                     %s',[MaterialForm.CoilingType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Load type                         %s',[ApplicationForm1.LoadType.Text]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Seating application coefficient   nu             %s', [TryFormat(SpringSolver.SeatingCoefficent)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of surface line         e1      %s', [TryFormat(SpringSolver.EccentricityE1)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Deviation of parallelism          e2      %s', [TryFormat(SpringSolver.EccentricityE2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring rate                       R       %s', [TryFormat(SpringSolver.SpringRateR)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Weight of the spring              m       %s', [TryFormat(SpringSolver.Mass)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Natural frequency spring          fe      %s', [TryFormat(SpringSolver.NaturalFrequency)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Increase in coil diameter at Lc   ΔDe     %s', [TryFormat(SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Mean coil diameter                Dmc     %s', [TryFormat(SpringSolver.Dm + SpringSolver.DeltaDe)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum clearance of active coils Sa      %s', [TryFormat(SpringSolver.SumOfMinimumGapsSa)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Actual clearance of active coils  sc-s2   %s', [TryFormat(SpringSolver.StrokeSc - SpringSolver.StrokeS2)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Coiling ratio                     w              %s', [TryFormat(SpringSolver.SpringIndexW)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch ratio                       tan α          %s', [TryFormat(SpringSolver.PitchRatio)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Pitch                             P       %s', [TryFormat(SpringSolver.Pitch)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  if SpringSolver.Pitch.Value <> 0 then
    S.Add(Format('   Pitch ratio                       Dm/P           %s', [TryFormat(SpringSolver.Dm / SpringSolver.Pitch)]))
  else
    S.Add(Format('   Pitch ratio                       Dm/P           ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  if SpringSolver.Dm.Value <> 0 then
    S.Add(Format('   Lean grade                        lambda         %s', [TryFormat(SpringSolver.LengthL0 / SpringSolver.Dm)]))
  else
    S.Add(Format('   Lean grade                        lambda         ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Spring energy between s1 and s2   W12     %s', [TryFormat(SpringSolver.SpringWorkW12)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Wire length                       L       %s', [TryFormat(SpringSolver.WireLength)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  if SpringSolver.Dm.Value> 0 then
    S.Add(Format('   Pressure                          R/D     %s', [TryFormat(SpringSolver.SpringRateR / SpringSolver.Dm)]))
  else
    S.Add(Format('   Pressure                          R/D     ---', []));

  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Max.diameter of bedding mandrel   DiMin   %s', [TryFormat(SpringSolver.DiMin)]));
  S.Add(Format('   ----------------------------------------------------------------------',[]));
  S.Add(Format('   Minimum diameter of bore          DeMax   %s', [TryFormat(SpringSolver.DeMax)]));
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

