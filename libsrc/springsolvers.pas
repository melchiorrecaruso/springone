{ Helical Spring Designer

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

unit SpringSolvers;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, SpringMaterials, SpringTolerances,
  Math, BaseUtils, SysUtils, ADim;

type
  TCompressionSpringSolver = class
  private
    FCheck: boolean;
    FClosedEnds: boolean;
    FColdCoiled: boolean;
    FCycleFrequency: TQuantity;
    Fd: TQuantity;
    FdTolerance: TQuantity;
    FDe: TQuantity;
    FDeMax: TQuantity;
    FDi: TQuantity;
    FDiMin: TQuantity;
    FDm: TQuantity;
    FDeltaDe: TQuantity;
    FDynamicLoad: boolean;
    FDynamicSafetyFactor: TQuantity;
    FLoadF1: TQuantity;
    FLoadF2: TQuantity;
    FLoadFn: TQuantity;
    FLoadFc: TQuantity;
    FE: TQuantity;
    Fe1: TQuantity;
    Fe2: TQuantity;
    FG: TQuantity;
    FGroundEnds: boolean;
    Fk: double;
    FLengthL0: TQuantity;
    FLengthL1: TQuantity;
    FLengthL2: TQuantity;
    FLengthLc: TQuantity;
    FLengthLn: TQuantity;
    FMass: TQuantity;
    Fn: double;
    FNaturalFrequency: TQuantity;
    FNumOfCycles: TQuantity;
    Fnt: double;
    Fnu: double;
    fPitch: TQuantity;
    fPitchRatio: TQuantity;
    fR: TQuantity;
    fRho: TQuantity;
    fRm: TQuantity;
    FStrokeS1: TQuantity;
    FStrokeS2: TQuantity;
    FStrokeSh: TQuantity;
    FStrokeSc: TQuantity;
    FStrokeSn: TQuantity;
    FSa: TQuantity;
    FStaticSafetyFactor: double;
    FSk: TQuantity;
    FBucklingStability: boolean;
    FTau1: TQuantity;
    FTau2: TQuantity;
    FTauh: TQuantity;
    FTauc: TQuantity;
    FTaun: TQuantity;
    FTauk1: TQuantity;
    FTauk2: TQuantity;
    FTaukh: TQuantity;
    FTaukc: TQuantity;
    FTaukn: TQuantity;
    FTauhz: TQuantity;
    FTauoz: TQuantity;
    FTauz: TQuantity;
    FTemperature: TQuantity;
    Fw: double;
    FW0n: TQuantity;
    FW12: TQuantity;
    FWireLength: TQuantity;
    procedure PreCheck;
    procedure PostCheck(ASpringTolerance: TEN15800);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve(ASpringTolerance: TEN15800);
    procedure Clear;
    function GetR(const aTemperature: TQuantity): TQuantity;
    function GetF1(const aTemperature: TQuantity): TQuantity;
    function GetF2(const aTemperature: TQuantity): TQuantity;
    function GetTau(const aLoadF: TQuantity): TQuantity;
    function GetTauk(const aLoadF: TQuantity): TQuantity;
    procedure GetBucklingCurve(var aPoints: ArrayOfTPointF);
  public
    property AdmDynamicTorsionalStressRangeTauhz: TQuantity read fTauhz;
    property AdmDynamicTorsionalStressTauoz: TQuantity read fTauoz;
    property AdmStaticTorsionalStressTauz: TQuantity read fTauz;
    property ActiveColis: double read fn write fn;
    property Check: boolean read fCheck;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;
    property ColdCoiled: boolean read FColdCoiled write FColdCoiled;
    property CycleFrequency:TQuantity read FCycleFrequency write FCycleFrequency;
    property De: TQuantity read FDe;
    property DeMax: TQuantity read FDeMax;
    property StrokeS1: TQuantity read FStrokeS1;
    property StrokeS2: TQuantity read FStrokeS2;
    property StrokeSh: TQuantity read FStrokeSh;
    property StrokeSc: TQuantity read FStrokeSc;
    property StrokeSn: TQuantity read FStrokeSn;
    property DeflectionSk: TQuantity read fSk;
    property DeltaDe: TQuantity read FDeltaDe;

    property Di: TQuantity read FDi;
    property DiMin: TQuantity read FDiMin;
    property Dm: TQuantity read FDm write FDm;
    property DynamicLoad: boolean read fDynamicLoad write fDynamicLoad;
    property DynamicSafetyFactor: TQuantity read fDynamicSafetyFactor;
    property LoadF1: TQuantity read FLoadF1;
    property LoadF2: TQuantity read FLoadF2;
    property LoadFc: TQuantity read FLoadFc;
    property LoadFn: TQuantity read FLoadFn;
    property EccentricityE1: TQuantity read fe1;
    property EccentricityE2: TQuantity read fe2;
    property GroundEnds: boolean read fGroundEnds write fGroundEnds;
    property CorrectionFactorK: double read fk;
    property LengthL0: TQuantity read FLengthL0 write FLengthL0;
    property LengthL1: TQuantity read FLengthL1 write FLengthL1;
    property LengthL2: TQuantity read FLengthL2 write FLengthL2;
    property LengthLc: TQuantity read FLengthLc;
    property LengthLn: TQuantity read FLengthLn;
    property Mass: TQuantity read fMass;
    property MaterialDensity: TQuantity read fRho write fRho;
    property NaturalFrequency: TQuantity read fNaturalFrequency;
    property NumOfCycles: TQuantity read fNumOfCycles;
    property Pitch: TQuantity read fPitch;
    property PitchRatio: TQuantity read fPitchRatio;
    property SeatingCoefficent: double read fnu write fnu;
    property ShearModulus: TQuantity read fG write fG;
    property SpringIndexW: double read fw;
    property SpringRateR: TQuantity read fR;
    property SpringWorkW0n: TQuantity read fW0n;
    property SpringWorkW12: TQuantity read fW12;
    property StaticSafetyFactor: double read fStaticSafetyFactor;
    property SumOfMinimumGapsSa: TQuantity read fSa;
    property TensileStrengthRm: TQuantity read fRm write fRm;
    property TorsionalStressTau1: TQuantity read fTau1;
    property TorsionalStressTau2: TQuantity read fTau2;
    property TorsionalStressTauc: TQuantity read fTauc;
    property TorsionalStressTauh: TQuantity read fTauh;
    property TorsionalStressTaun: TQuantity read fTaun;
    property TorsionalStressTauk1: TQuantity read fTauk1;
    property TorsionalStressTauk2: TQuantity read fTauk2;
    property TorsionalStressTaukc: TQuantity read fTaukc;
    property TorsionalStressTaukh: TQuantity read fTaukh;
    property TorsionalStressTaukn: TQuantity read fTaukn;
    property TotalCoils: double read fnt write fnt;
    property Temperature: TQuantity read fTemperature write fTemperature;
    property YoungModulus: TQuantity read fE write fE;
    property WireDiameter: TQuantity read Fd write Fd;
    property WireDiameterTolerance: TQuantity read FdTolerance write FdTolerance;
    property WireLength: TQuantity read fWireLength;
  end;

  TTorsionSpringSolver = class
  private
    fa: TQuantity;
    falpha1: TQuantity;
    falpha2: TQuantity;
    falphan: TQuantity;

    falpha1coil: TQuantity;
    falpha2coil: TQuantity;
    falphancoil: TQuantity;
    fBendRadiusLegA: TQuantity;
    fBendRadiusLegB: TQuantity;
    fBentLegA: boolean;
    fBentLegB: boolean;

    fbeta1: TQuantity;
    fbeta2: TQuantity;
    fbetan: TQuantity;

    fcA, fcB: TQuantity;
    fCheck: boolean;

    fClosedCoils: boolean;
    fColdCoiled: boolean;
    fd: TQuantity;
    fdTolerance: TQuantity;

    fDe: TQuantity;
    fDi: TQuantity;
    fDm: TQuantity;

    fDynamicLoad: boolean;
    fDynamicSafetyFactor: TQuantity;

    fE: TQuantity;
    fFixedEndA: boolean;
    fFixedEndB: boolean;
    fForceA1: TQuantity;
    fForceA2: TQuantity;
    fForceB1: TQuantity;
    fForceB2: TQuantity;

    fG: TQuantity;
    fLengthLegA: TQuantity;
    fLengthLegB: TQuantity;
    fLengthArmLegA: TQuantity;
    fLengthArmLegB: TQuantity;

    fMass: TQuantity;
    fNaturalFrequency: TQuantity;
    fNumOfCycles: TQuantity;
    fn: double;
    fq: double;

    fRho: TQuantity;
    fRm: TQuantity;
    fRMR: TQuantity;

    fStaticSafetyFactor: double;
    fStressInCoilingDirection: boolean;

    fSigma1: TQuantity;
    fSigma2: TQuantity;
    fSigman: TQuantity;

    fSigmaq1: TQuantity;
    fSigmaq2: TQuantity;
    fSigmaqn: TQuantity;

    fSigmaqh: TQuantity;


    fSigmaz: TQuantity;
    fSigmaoz: TQuantity;
    fSigmahz: TQuantity;

    fTorqueT1: TQuantity;
    fTorqueT2: TQuantity;
    fTorqueTn: TQuantity;

    fw: double;
    fWireLength: TQuantity;

    fW0n,
    fW12: TQuantity;

    function CalcRadialEndFlex(ArmLength: TQuantity): TQuantity;
    function CalcTangentialEndFlex(ArmLength: TQuantity): TQuantity;
    procedure PreCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Solve(ASpringTolerance: TDIN2194);

    function AlphaCoil(const Alpha: TQuantity): TQuantity;
    function Lk(const Alpha: TQuantity): TQuantity;
    function InnerCoilDiameter(const Alpha: TQuantity): TQuantity;


    function GetSigma (const aTorque: TQuantity): TQuantity;
    function GetSigmaq(const aTorque: TQuantity): TQuantity;

  public
    property ActiveColis: double read fn write fn;
    property AdmDynamicBendingStressRangeSigmahz: TQuantity read fSigmahz;
    property AdmDynamicBendingStressSigmaoz: TQuantity read fSigmaoz;
    property AdmStaticBendingStressSigmaz: TQuantity read fSigmaz;

    property Alpha1: TQuantity read fAlpha1 write fAlpha1;
    property Alpha2: TQuantity read fAlpha2 write fAlpha2;
    property Alphan: TQuantity read fAlphan;

    property BendRadiusLegA: TQuantity read fBendRadiusLegA write fBendRadiusLegA;
    property BendRadiusLegB: TQuantity read fBendRadiusLegB write fBendRadiusLegB;
    property BentLegA: boolean read fBentLegA write fBentLegA;
    property BentLegB: boolean read fBentLegB write fBentLegB;

    property Check: boolean read fCheck;
    property ColdCoiled: boolean read fColdCoiled write fColdCoiled;

    property De: TQuantity read fDe;
    property Di: TQuantity read fDi;
    property Dm: TQuantity read fDm write fDm;

    property DynamicLoad: boolean read fDynamicLoad write fDynamicLoad;
    property DynamicSafetyFactor: TQuantity read fDynamicSafetyFactor;

    property FixedEndA: boolean read fFixedEndA write fFixedEndA;
    property FixedEndB: boolean read fFixedEndB write fFixedEndB;


    property LengthLegA: TQuantity read fLengthLegA write fLengthLegA;
    property LengthLegB: TQuantity read fLengthLegB write fLengthLegB;

    property LengthArmLegA: TQuantity read fLengthArmLegA write fLengthArmLegA;
    property LengthArmLegB: TQuantity read fLengthArmLegB write fLengthArmLegB;

    property Mass: TQuantity read FMass;
    property MaterialDensity: TQuantity read fRho write fRho;
    property NaturalFrequency: TQuantity read fNaturalFrequency;

    property CoilsGap: TQuantity read fa write fa;
    property CorrectionFactorQ: double read fq;

    property ShearModulus: TQuantity read fG write fG;
    property StaticSafetyFactor: double read fStaticSafetyFactor;

    property StressInCoilingDirection: boolean read fStressInCoilingDirection write fStressInCoilingDirection;

    property TorqueT1: TQuantity read fTorqueT1;
    property TorqueT2: TQuantity read fTorqueT2;
    property TorqueTn: TQuantity read fTorqueTn;
    property SpringRateRMR: TQuantity read fRMR;
    property SpringWorkW0n: TQuantity read fW0n;
    property SpringWorkW12: TQuantity read fW12;

    property TensileStrengthRm: TQuantity read fRm write fRm;
    property BendingStressSigma1: TQuantity read fSigma1;
    property BendingStressSigma2: TQuantity read fSigma2;
    property BendingStressSigman: TQuantity read fSigman;

    property BendingStressSigmaq1: TQuantity read fSigmaq1;
    property BendingStressSigmaq2: TQuantity read fSigmaq2;
    property BendingStressSigmaqn: TQuantity read fSigmaqn;

    property BendingStressSigmaqH: TQuantity read fSigmaqh;

    property YoungModulus: TQuantity read fE write fE;
    property WireDiameter: TQuantity read fd write fd;
    property WireDiameterTolerance: TQuantity read FdTolerance write FdTolerance;

    property WireLength: TQuantity read fWireLength;
  end;

implementation

// TCompressionSpringSolver

constructor TCompressionSpringSolver.Create;
begin
  inherited Create;
end;

destructor TCompressionSpringSolver.Destroy;
begin
  inherited Destroy;
end;

procedure TCompressionSpringSolver.Clear;
begin
  fCheck               := False;
  fClosedEnds          := True;
  FColdCoiled          := True;
  Fd                   := 0*m;
  FDe                  := 0*m;
  FDeMax               := 0*m;
  FDeltaDe             := 0*m;
  FDi                  := 0*m;
  FDiMin               := 0*m;
  FDm                  := 0*m;
  FdTolerance          := 0*m;
  fDynamicLoad         := False;
  fDynamicSafetyFactor := 0;
  fE                   := 0*Pa;
  fe1                  := 0*m;
  fe2                  := 0*m;
  FLoadF1              := 0*N;
  FLoadF2              := 0*N;
  FLoadFn              := 0*N;
  FLoadFc              := 0*N;
  fG                   := 0*Pa;
  fGroundEnds          := True;
  fk                   := 0;
  FLengthL0            := 0*m;
  FLengthL1            := 0*m;
  FLengthL2            := 0*m;
  FLengthLc            := 0*m;
  FLengthLn            := 0*m;
  fMass                := 0*kg;
  fn                   := 0;
  fNaturalFrequency    := 0*Hz;
  fnu                  := 0;
  fNumOfCycles         := 0;
  fnt                  := 0;
  fPitch               := 0*m;
  fPitchRatio          := 0;
  fR                   := 0*N/m;
  fRho                 := 0*kg/m3;
  fRm                  := 0*Pa;
  FStrokeS1            := 0*m;
  FStrokeS2            := 0*m;
  FStrokeSh            := 0*m;
  FStrokeSn            := 0*m;
  FStrokeSc            := 0*m;

  fSa                  := 0*m;
  fSk                  := 0*m;
  FBucklingStability   := False;

  fStaticSafetyFactor  := 0;
  fTau1                := 0*Pa;
  fTau2                := 0*Pa;
  fTauc                := 0*Pa;
  fTauh                := 0*Pa;
  fTauk1               := 0*Pa;
  fTauk2               := 0*Pa;
  fTaukc               := 0*Pa;
  fTaukh               := 0*Pa;
  fTaukn               := 0*Pa;
  fTauhz               := 0*Pa;
  fTaun                := 0*Pa;
  fTauoz               := 0*Pa;
  fTauz                := 0*Pa;
  fw                   := 0;
  fW0n                 := 0*J;
  fW12                 := 0*J;
  fWireLength          := 0*m;
end;

procedure TCompressionSpringSolver.PreCheck;
begin
  fCheck := True;
  // Controllo validità dati inseriti
  if LessThanOrEqualToZero(Fd)  then ErrorMessage.Add('EN13906-1: Wire diameter "d" unassigned.');
  if LessThanOrEqualToZero(FDm) then ErrorMessage.Add('EN13906-1: Coil diameter "Dm" unassigned.');
  if LessThanOrEqualToZero(fE)  then ErrorMessage.Add('EN13906-1: Young''s modulus "E" unassigned.');
  if LessThanOrEqualToZero(fG)  then ErrorMessage.Add('EN13906-1: Shear modulus "G" unassigned.');
  if LessThanOrEqualToZero(FLengthL0) then ErrorMessage.Add('EN13906-1: Spring length "L0" unassigned.');
  if LessThanOrEqualToZero(FLengthL1) then ErrorMessage.Add('EN13906-1: Spring length "L1" unassigned.');
  if LessThanOrEqualToZero(FLengthL2) then ErrorMessage.Add('EN13906-1: Spring length "L2" unassigned.');
  if fn  <= 0 then ErrorMessage.Add('EN13906-1: Number of active coil "n" unassigned.');
  if fnt <= 0 then ErrorMessage.Add('EN13906-1: Number of total coil "nt" unassigned.');
  if fnu <= 0 then ErrorMessage.Add('EN13906-1: Seat coefficent "nu" unassigned.');
  if LessThanOrEqualToZero(fRm)  then ErrorMessage.Add('EN13906-1: Tensile strength "Rm" unassigned.');
  if LessThanOrEqualToZero(fRho) then ErrorMessage.Add('EN13906-1: Material density "rho" unassigned.');

  if FLengthL0 <= FLengthL1 then ErrorMessage.Add('EN13906-1: Wrong L0 and L1 values, L0 must be > L1.');
  if FLengthL1 <= FLengthL2 then ErrorMessage.Add('EN13906-1: Wrong L1 and L2 values, L1 must be > L2.');

  fCheck := ErrorMessage.Count = 0;

  // Poisson's ratio
  if fCheck then
  begin
    if (fE / (2 * fG) - 1) > 0.45 then WarningMessage.Add('EN13906-1: Wrong values for Young''s modulus "E" and Shear modulus "G".');
    if (fE / (2 * fG) - 1) < 0.10 then WarningMessage.Add('EN13906-1: Wrong values for Young''s modulus "E" and Shear modulus "G".');
  end;

  // Scopo e campo di applicazione della norma
  if fCheck then
  begin
    fw := ScalarUnit.ToFloat(FDm / Fd);
    if FColdCoiled then
    begin
      if Fd > (20*mm)  then WarningMessage.Add('EN13906-1: Wire diameter > 20mm.');
      if fn < (2)      then WarningMessage.Add('EN13906-1: Number of active coils < 2.');
      if fw < (4)      then WarningMessage.Add('EN13906-1: Spring Index < 4.');
      if fw > (20)     then WarningMessage.Add('EN13906-1: Spring Index > 20.');
    end else
    begin
      if Fd < (8*mm)   then WarningMessage.Add('EN13906-1: Wire diameter < 8mm.');
      if Fd > (100*mm) then WarningMessage.Add('EN13906-1: Wire diameter > 100mm.');
      if fn < (3)      then WarningMessage.Add('EN13906-1: Number of active coils < 3.');
      if fw < (3)      then WarningMessage.Add('EN13906-1: Spring Index < 3.');
      if fw > (12)     then WarningMessage.Add('EN13906-1: Spring Index > 12.');
    end;

    if (fClosedEnds = True) then
    begin
      if (FColdCoiled = False) and (fGroundEnds = False) then
        ErrorMessage.Add('EN13906-1: Length "Lc" undefined.');
    end else
    begin
      if (FColdCoiled <> False) and (fGroundEnds <> False) then
        ErrorMessage.Add('EN13906-1: Length "Lc" undefined.');
    end;

    if (fClosedEnds <> GroundEnds = True) then
    begin
      WarningMessage.Add('EN13906-1: "DeltaDE" undefined.');
    end;

    fCheck := ErrorMessage.Count = 0;
  end;
end;

procedure TCompressionSpringSolver.Solve(ASpringTolerance: TEN15800);
var
  mDe: TQuantity;
  mSk: TQuantity;
  mTau: TQuantity;
  Tauh7, Tauh6, Tauh5: TQuantity;
begin
  PreCheck;
  // Calcolo molla a compressione ad elica cilindrica fabbricate con filo sezione circolare.
  if fCheck then
  begin
    FDe := FDm + Fd;
    FDi := FDm - Fd;
    // Spring index
    fw  := ScalarUnit.ToFloat(FDm / Fd);
    // Fattore di correzione delle tensioni
    fk := (fw + 0.5) / (fw - 0.75);
  end;

  // Calcolo lunghezza a spire bloccate
  if fCheck then
  begin
    // La lunghezza a spire cloccate "Lc" è:
    // - per molle avvolte a freddo con estremità chiuse e molate:
    //   Lc <= nt*dmax
    // - per molle avvolte a freddo con estremità chiuse, non molate:
    //   Lc <= (nt + 1.5)*dmax
    // - per molle avvolte a caldo con estremità chiuse e molate:
    //   Lc <= (nt-0.3)*dmax
    // - per molle avvolte a caldo con estremità aperte e non molate:
    //   Lc <= (nt + 1.1)*dmax.
    if (fClosedEnds = True) then
    begin
      if (FColdCoiled = True) and (fGroundEnds = True) then
        FLengthLc := fnt * (Fd + FdTolerance)
      else
        if (FColdCoiled = True) and (fGroundEnds = False) then
          FLengthLc := (fnt + 1.5) * (Fd + FdTolerance)
        else
          if (FColdCoiled = False) and (fGroundEnds = True) then
            FLengthLc := (fnt - 0.3) * (Fd + FdTolerance)
          else
            FLengthLc := (0*mm);
    end else
    begin
      if (FColdCoiled = False) and (fGroundEnds = False) then
        FLengthLc := (fnt + 1.1) * (Fd + FdTolerance)
      else
        FLengthLc := (0*mm);
    end;

    if FLengthLc <= (0*mm)    then ErrorMessage.Add('EN13906-1: Lc value, Lc must be > 0.');
    if FLengthLc >  FLengthL0 then ErrorMessage.Add('EN13906-1: L0 or nt value, Lc must be < L0.');
    if FLengthLc >  FLengthL1 then ErrorMessage.Add('EN13906-1: L1 or nt value, Lc must be < L1.');
    if FLengthLc >  FLengthL2 then ErrorMessage.Add('EN13906-1: L2 or nt value, Lc must be < L2.');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo lunghezza minima ammissibile della molla
  if fCheck then
  begin
    // Per ottenere la lunghezza minima ammissibile della molla "Ln" basta sommare la lunghezza
    // a spire bloccate "Lc" e la somma delle distanze minime fra due successive spire attive "Sa":
    if FColdCoiled then
      fSa := fn * (0.0015 * (FDm*(FDm/Fd)) + 0.1*Fd)
    else
      fSa := 0.02 * fn * (FDm + Fd);
    // Nel caso di molle assoggettate a carico dinamico, il valore "Sa" viene raddoppiato rispetto al
    // caso di carico statico per molle avvolte a caldo YoungModulus moltiplicato per 1.5 per molle avvolte a freddo.
    if fDynamicLoad then
    begin
      if FColdCoiled then
        fSa := fSa * 1.5
      else
        fSa := fSa * 2.0;
    end;
    FLengthLn := FLengthLc + fSa;

    if FLengthLn < FLengthLc then ErrorMessage  .Add('EN13906-1: Wrong Ln value, Ln must be > Lc.');
    if FLengthLn > FLengthL0 then ErrorMessage  .Add('EN13906-1: Wrong Ln value, Ln must be < L0.');
    if FLengthLn > FLengthL1 then WarningMessage.Add('EN13906-1: Wrong Ln value, Ln must be < L1.');
    if FLengthLn > FLengthL2 then WarningMessage.Add('EN13906-1: Wrong Ln value, Ln must be < L2.');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo delle frecce della molla
  if fCheck then
  begin
    FStrokeS1 := FLengthL0 - FLengthL1;
    FStrokeS2 := FLengthL0 - FLengthL2;
    FStrokeSh := FStrokeS2 - FStrokeS1;
    FStrokeSn := FLengthL0 - FLengthLn;
    FStrokeSc := FLengthL0 - FLengthLc;
  end;

  // Calcolo del passo della molla
  if fCheck then
  begin
    fPitch      := FStrokeSc/fn + Fd;
    fPitchRatio := ScalarUnit.ToFloat(FDm/fPitch);
  end;

  // Calcolo incremento del diamtro esterno della molla sotto carico
  if fCheck then
  begin
    // Quando la molla è sottoposta a compressione, il diametro della spira aumenta leggermente.
    // L'incremento del diamtro esterno della molla sotto carico, DeltaDe, è determinato usando la
    // formula sottostante, valida per molla a pacco Lc e per estremità della molla
    // liberamente appoggiate.
    if (fClosedEnds) and (GroundEnds) then
      mDe := (FStrokeSc + fn * Fd) / fn
    else
      if (not fClosedEnds) and (not GroundEnds) then
        mDe := (FStrokeSc + (fn + 1.5) * Fd) / fn
      else
        // negli altri casi le considero i terminali aperti non molati
        mDe := 0*m;

    if EqualToZero(mDe) then
    begin
      WarningMessage.Add('EN13906-1: "DeltaDe" undefined. Please change kind spring ends.');
    end else
    begin
      FDeltaDe := 0.1 * ((mDe*mDe) - 0.8 * mDe*Fd - 0.2 * Fd*Fd) / FDm;
    end;

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo carichi della molla
  if fCheck then
  begin
    // Calcolo rigidezza della molla:
    fR := (fG/(8*fn))*(QuarticPower(Fd)/CubicPower(FDm));

    // Calcolo carichi della molla:
    FLoadF1 := fR * FStrokeS1;
    FLoadF2 := fR * FStrokeS2;
    FLoadFn := fR * FStrokeSn;
    FLoadFc := fR * FStrokeSc;
  end;

  // Calcolo lavoro molla W0n e W12
  if fCheck then
  begin
    fW0n := (FLoadFn / 2) * (FStrokeSn);
    fW12 := (FLoadF2 + FLoadF1) / 2 * (FStrokeS2 - FStrokeS1);
  end;

  // Calcolo tolleranze Dm, Lo, F1, F1 e valori eccentricità e1 ed e2.
  if fCheck then
  begin
    ASpringTolerance.WireDiameter   := Fd;
    ASpringTolerance.CoilDiameter   := FDm;
    ASpringTolerance.Load1          := FLoadF1;
    ASpringTolerance.Load2          := FLoadF2;
    ASpringTolerance.FreeBodyLength := FLengthL0;
    ASpringTolerance.NumActiveCoils := fn;
    ASpringTolerance.SpringIndex    := fw;
    ASpringTolerance.SpringRate     := fR;
    ASpringTolerance.Solve;

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo diametro minimo e massimo della molla in esercizio:
  if fCheck then
  begin
    FDiMin := FDi - ASpringTolerance.ToleranceOnCoilDiameter - (FdTolerance);
    FDeMax := FDe + ASpringTolerance.ToleranceOnCoilDiameter + (FdTolerance) + FDeltaDe;
  end;

  // Calcolo errore perpendicolarità e parallelismo
  if fCheck then
  begin
    fe1 := ASpringTolerance.ToleranceOnPerpendicularity;
    fe2 := ASpringTolerance.ToleranceOnParallelism;
  end;

  // Calcolo lunghezza sviluppo del filo della molla
  if fCheck then
  begin
    fWireLength := pi * FDm * fnt;
  end;

  // Calcolo massa della molla
  if fCheck then
  begin
    fMass := FRho * ((pi/4*(Fd*Fd)) * FWireLength);
  end;

  // Calcolo frequenza naturale del primo ordine della molla
  if fCheck then
  begin
    // La frequenza naturale del primo ordine della molla, avente entrambe le estremità vincolate ed
    // eccitare periodicamente ad una estremità durante il funzionamento, è determinata mediante la
    // seguente formula:
    // fNaturalFrequency := (Fd/FDm) / (2*pi*sqrt(2)*fn) * (Sqrt(FG.Value/FRho.Value)*(m/s))/FDm;

    fNaturalFrequency := 2/ ( (fn*FDm*FDm) / (2*pi*sqrt(2)*fd) / SquareRoot(FG/FRho) );
  end;

  // Calcolo lunghezza instabilità molla
  if fCheck then
  begin
    // Certe molle hanno la tendenza a essere instabili; la lunghezza critica di una molla alla quale
    // inizia WireLength'instalilità, è chiamata lunghezza di instabilità "Lk" e la freccia della molla fino al
    // punto di instabilità è detta freccia della molla sotto il carico di instabilità DeflectionSk.
    // WireLength'influeza dell'alloggiamento delle estremità della molla viene tenuta in considerazione per
    // mezzo del coefficente di appoggio "v", che è indicato sotto per i più comuni titpi di
    // all'oggiamneto:
    // - base incastrata e punto di applicazione forza libero e flottante,          v = 2
    // - base flottante  e punto di applicazione della forza guidato e flottante,   v = 1
    // - base incastrata e punto di applicazione della forza libero non flottante,  v = 1
    // - base incastrata e punto di applicazione della forza guidato ma flottante,  v = 0.7
    // - base incastrata e punto di applicazione della forza guidato non flottante, v = 0.5
    // La freccia della molla sotto il carico di instabilità viene determinata con la seguente formula:
    mSk := 1 - (1 - fG/fE) / (0.5 + fG/fE) * SquareRoot(pi*FDm/fnu/FLengthL0);

    FBucklingStability := mSk < 0;
    if FBucklingStability = False then
    begin
      fSk := FLengthL0 * (0.5 / (1 - fG/fE) * (1 - SquareRoot(mSk)));
      // La sicurezza contro l'instabilità è raggiunta in teoria per un valore immaginario
      // della radice quadrata e per Sk/s > 1.
      if not ((fSk / FStrokeSc) > 1) then WarningMessage.Add('EN13906-1: Buckling!');
    end;
    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo sollecitazioni torsionali Tau
  if fCheck then
  begin
    // Mentre la sollecitazione torsionale "tau" deve essere adottata per il calcolo delle molle
    // caricate staticamente o quasi staticamente, per le molle caricate dinamicamente si deve
    // impiegare la sollecitazione torsionale corretta "tauk".
    fTau1  := (fG * Fd * FStrokeS1) / (Pi * fn * (FDm*FDm));
    fTau2  := (fG * Fd * FStrokeS2) / (Pi * fn * (FDm*FDm));
    fTauh  := fTau2 - fTau1;
    fTaun  := (fG * Fd * FStrokeSn) / (Pi * fn * (FDm*FDm));
    fTauc  := (fG * Fd * FStrokeSc) / (Pi * fn * (FDm*FDm));
    fTauk1 := fTau1 * fk;
    fTauk2 := fTau2 * fk;
    fTaukh := fTauh * fk;
    fTaukn := fTaun * fk;
    fTaukc := fTauc * fk;
  end;

  // Calcolo tolleranze delle tensioni Tau1 e Tau2
  if fCheck then
  begin
    //fToleranceTau1  := 8 * FDm * (FLoadF1 + fToleranceF1) / (Pi * Power(Fd, 3)) - fTau1;
    //fToleranceTau2  := 8 * FDm * (FLoadF2 + fToleranceF2) / (Pi * Power(Fd, 3)) - fTau2;
    //fToleranceTauk1 := fToleranceTau1 * fK;
    //fToleranceTauk2 := fToleranceTau2 * fK;
  end;

  // Calcolo tensione statica ammissibile
  if fCheck then
  begin
    // Per esigenze di fabbricazione deve essere possibile comprimere le molle fino alla lunghezza
    // alla quale tutte le spire sono a contatto. La sollecitazione torsionale ammissibile non
    // corretta a spire bloccate, tczul, è tczul = 0.56*Rm. Il valore di Rm (valore minimo della
    // resistenza a trazione) è ottenibile dalle norme pertinenti. I valori della resistenza usati nei
    // calcoli devono essere i valori di resistenza a trazione relativi alla condizione di
    // rinvenimento o di invecchiamento artificiale.
    fTauz := 0.56 * fRm;
  end;

  // Calcolo coefficente di sicurezza statico
  if fCheck then
  begin
    fStaticSafetyFactor := ScalarUnit.ToFloat(fTauz / fTauc);

    if fStaticSafetyFactor < 1 then
      WarningMessage.Add('EN13906-1: Static safety factor < 1 !');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo numero di cicli e coefficente di sicurezza a fatica
  if fCheck then
  begin
    if MAT.Name = '' then
      WarningMessage.Add('EN13906-1: Spring material data unavailable (dyn).');

    if GreaterThanZero(MAT.TorsionalStressTauStar ) and
       GreaterThanZero(MAT.TorsionalStressTauYield) and
       GreaterThanZero(MAT.TorsionalStressTauOE7  ) and
       GreaterThanZero(MAT.TorsionalStressTauUE7  ) then
    begin
      fTauoz := MAT.TorsionalStressTauYield;
      fTauhz := 0*Pa;

      if fTauk1 >= MAT.TorsionalStressTauUE7 then
        Tauh7 := fTauoz - fTauk1
      else
        Tauh7 := MAT.TorsionalStressTauOE7 + (MAT.TorsionalStressTauYield - MAT.TorsionalStressTauOE7) / MAT.TorsionalStressTauUE7 * fTauk1 - fTauk1;

      if fTauk1 >= MAT.TorsionalStressTauUE6 then
        Tauh6 := fTauoz - fTauk1
      else
        Tauh6 := MAT.TorsionalStressTauOE6 + (MAT.TorsionalStressTauYield - MAT.TorsionalStressTauOE6) / MAT.TorsionalStressTauUE6 * fTauk1 - fTauk1;

      if fTauk1 >= MAT.TorsionalStressTauUE5 then
        Tauh5 := fTauoz - fTauk1
      else
        Tauh5 := MAT.TorsionalStressTauOE5 + (MAT.TorsionalStressTauYield - MAT.TorsionalStressTauOE5) / MAT.TorsionalStressTauUE5 * fTauk1 - fTauk1;

      if fTaukh <= Tauh7 then
      begin
        fNumOfCycles := MAXFLOAT;
      end else
      begin
        if fTaukh <= Tauh6 then
        begin
          mTau := (fTaukh - Tauh7)/(Tauh6 - Tauh7);
          fNumOfCycles := MAT.NumOfCyclesE7 - mTau*(MAT.NumOfCyclesE7 - MAT.NumOfCyclesE6);
        end else
        begin
          if fTaukh <= Tauh5 then
          begin
            mTau := (fTaukh - Tauh6)/(Tauh5 - Tauh6);
            fNumOfCycles := MAT.NumOfCyclesE6 - mTau*(MAT.NumOfCyclesE6 - MAT.NumOfCyclesE5);
          end;
        end;
      end;
      fTauhz := Tauh7;

      fDynamicSafetyFactor := Tauh7 / fTaukh;
    end;
  end;
  PostCheck(ASpringTolerance);
end;

function TCompressionSpringSolver.GetR(const aTemperature: TQuantity): TQuantity;
begin
  if fCheck then
    Result := MAT.GetG(aTemperature) * (QuarticPower(Fd)/ (8*fn*CubicPower(FDm)))
  else
    Result := 0*N/m;
end;

function TCompressionSpringSolver.GetF1(const aTemperature: TQuantity): TQuantity;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS1
  else
    Result := 0*N;
end;

function TCompressionSpringSolver.GetF2(const aTemperature: TQuantity): TQuantity;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS2
  else
    Result := 0*N;
end;

function TCompressionSpringSolver.GetTau(const aLoadF: TQuantity): TQuantity;
begin
  if NotEqualToZero(Fd) then
    Result := 8 * (aLoadF)/(Fd*Fd*Pi) * (FDm/Fd)
  else
    Result := 0*Pa;
end;

function TCompressionSpringSolver.GetTauk(const aLoadF: TQuantity): TQuantity;
begin
  Result := GetTau(aLoadF) * fk;
end;

procedure TCompressionSpringSolver.GetBucklingCurve(var aPoints: ArrayOfTPointF);
var
  Value: TQuantity;
  Y, DY: TQuantity;
  C1, C2: TQuantity;
begin
  C1 := fE/(2*(fE-fG));
  C2 := (2*Pi*Pi*(fE-fG)/(2*fG+fE));

  Y  := 0.950;
  DY := 0.001;
  repeat
    try
      Value := SquareRoot(C2/(1-SquarePower(1-Y/C1)));
      SetLength(aPoints, Length(aPoints) + 1);
      aPoints[High(aPoints)].x := ScalarUnit.ToFloat(Value);
      aPoints[High(aPoints)].y := ScalarUnit.ToFloat(Y);
      Y := Y - DY;
    except
    end;
  until (Value > (1.25*(fnu*FLengthL0/FDm))) or (Y < 0);
end;

procedure TCompressionSpringSolver.PostCheck(ASpringTolerance: TEN15800);
begin
  if (fnt - fn) < 2 then WarningMessage.Add('EN13906-1: Inactive end coild < 1.');

  if (FLoadF2 + ASpringTolerance.ToleranceOnLoad2) > FLoadFn then WarningMessage.Add('EN13906-1: Load F2 + TOL. > Fn.');
  if (FLoadF2 + ASpringTolerance.ToleranceOnLoad2) > FLoadFc then WarningMessage.Add('EN13906-1: Load F2 + TOL. > Fc.');
end;

// TTorsionSpringSolver

constructor TTorsionSpringSolver.Create;
begin
  inherited Create;
end;

destructor TTorsionSpringSolver.Destroy;
begin
  inherited Destroy;
end;

procedure TTorsionSpringSolver.Clear;
begin
  fa              := 0*m;
  falpha1         := 0*rad;
  falpha2         := 0*rad;
  falpha1coil     := 0*rad;
  falpha2coil     := 0*rad;
  fBendRadiusLegA := 0*m;
  fBendRadiusLegB := 0*m;

  fBentLegA       := False;
  fBentLegB       := False;

  fbeta1          := 0*rad;
  fbeta2          := 0*rad;

  fFixedEndA      := False;
  fFixedEndB      := False;
  fClosedCoils    := True;
  fDynamicLoad    := False;
  fDynamicSafetyFactor := 0;

  fMass             := 0*kg;
  fNaturalFrequency := 0;
  fNumOfCycles      := 0;

  fTorqueT1       := 0*N*m;
  fTorqueT2       := 0*N*m;
  fTorqueTn       := 0*N*m;
  fq              := 0;
  fLengthArmLegA  := 0*m;
  fLengthArmLegB  := 0*m;

  fSigma1         := 0*Pa;
  fSigma2         := 0*Pa;
  fSigman         := 0*Pa;
  fSigmaz         := 0*Pa;

  fSigmaq1        := 0*Pa;
  fSigmaq2        := 0*Pa;

  fStressInCoilingDirection := True;
end;

procedure TTorsionSpringSolver.PreCheck;
var
  i: longint;
begin
  fCheck := True;
  if LessThanOrEqualToZero(fd)  then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if LessThanOrEqualToZero(fDm) then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if LessThanOrEqualToZero(fE)  then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if LessThanOrEqualToZero(fG)  then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if LessThanZero(fa)           then ErrorMessage.Add('Wrong coil distance "a" value.');
  if fn <= 0                    then ErrorMessage.Add('Number of active coil "n" unassigned.');


  if fColdCoiled then
    if (fd > (20*mm)) then ErrorMessage.Add('Wire diameter > 20.')
  else
    if (fd > (10*mm)) then ErrorMessage.Add('Wire diameter > 10.');

  if fn <= 2 then ErrorMessage.Add('Number of active coil "n" < 2.');

  fCheck := ErrorMessage.Count = 0;

  if fCheck then
  begin
    fw := ScalarUnit.ToFloat(fDm/fd);
    if fColdCoiled then
    begin
      if (fw <  4) then ErrorMessage.Add('Spring index "w" < 4.');
      if (fw > 20) then ErrorMessage.Add('Spring index "w" > 20.');
    end else
    begin
      if (fw <  4) then ErrorMessage.Add('"Spring index w" < 4.');
      if (fw > 12) then ErrorMessage.Add('Spring index "w" > 12.');
    end;
    fCheck := ErrorMessage.Count = 0;
  end;
end;

function TTorsionSpringSolver.CalcRadialEndFlex(ArmLength: TQuantity): TQuantity;
begin
  Result := Power(2*ArmLength/fDm -1, 3)/(24*pi*fn*Power(ArmLength/fDm, 2));
end;

function TTorsionSpringSolver.CalcTangentialEndFlex(ArmLength: TQuantity): TQuantity;
begin
  Result := (4*Power(ArmLength/fDm, 2) -1)/(12*pi*fn*(ArmLength/fDm));
end;

procedure TTorsionSpringSolver.Solve(ASpringTolerance: TDIN2194);
var
  i: longint;
  mSigma: TQuantity;
  Sigmah7, Sigmah6, Sigmah5: TQuantity;
begin
  PreCheck;
  if fCheck then
  begin
    fDe := fDm + fd;
    fDi := fDm - fd;

    // calculate q factor
    fq := 1;
    if (fDynamicLoad) or (not fStressInCoilingDirection) then
    begin
    // formula approssimativa secondo EN 13906-3
    fq := (fw + 0.07)/(fw - 0.75);
    // formula di Göhner
    // fq := 1 + 0.87 / fw + 0.642 / Sqr(fw);
    end;

    if fBentLegA then
    begin
      if GreaterThanZero(fBendRadiusLegA) then
        fq := ScalarUnit.ToFloat(Max(fq, (2*fBendRadiusLegA/fd + 1.07)/(2*fBendRadiusLegA/fd + 0.25)))
      else
        ErrorMessage.Add('Bending radius leg "A" is zero.');
    end;
    fCheck := ErrorMessage.Count = 0;

    if fBentLegB then
    begin
      if GreaterThanZero(fBendRadiusLegB) then
        fq := ScalarUnit.ToFloat(Max(fq, (2*fBendRadiusLegB/fd + 1.07)/(2*fBendRadiusLegB/fd + 0.25)))
      else
        ErrorMessage.Add('Bending radius leg "B" is zero.');
    end;
    fCheck := ErrorMessage.Count = 0;

    if fCheck then
    begin
      // calculate leg A bending ratio
      fcA := 0;
      if (not fFixedEndA) then
      begin
        if fBentLegA then
          fcA := CalcRadialEndFlex(fLengthArmLegA)
        else
          fcA := CalcTangentialEndFlex(fLengthArmLegA);
      end;

      // calculate leg B bending ratio
      fcB := 0;
      if (not fFixedEndB) then
      begin
        if fBentLegB then
          fcB := CalcRadialEndFlex(fLengthArmLegB)
        else
          fcB := CalcTangentialEndFlex(fLengthArmLegB);
      end;
    end;

    // Calcolo lunghezza sviluppo del filo della molla
    if fCheck then
    begin
      fWireLength := fLengthLegA + pi*fDm*fn + fLengthLegB;
    end;

    // Calcolo massa della molla
    if fCheck then
    begin
      fMass := FRho * ((pi/4*(Fd*Fd)) * FWireLength);
    end;

    if fCheck then
    begin
      // calculate coil angle
      falpha1coil := falpha1/(1 + fcA + fcB);
      falpha2coil := falpha2/(1 + fcA + fcB);

      // calculate legs bending angle
      fbeta1 := falpha1coil*(fcA + fcB);
      fbeta2 := falpha2coil*(fcA + fcB);
      fbetan := falpha2coil*(fcA + fcB);

      // calculate spring rate
      fRMR := fE*QuarticPower(fd)/(64*fDm*fn);

      // calculate torques
      fTorqueT1 := fRMR*falpha1coil;
      fTorqueT2 := fRMR*falpha2coil;

      // calculate reaction A
      if GreaterThanZero(fLengthArmLegA) then
      begin
        fForceA1 := fTorqueT1/fLengthArmLegA;
        fForceA2 := fTorqueT2/fLengthArmLegA;
      end;

      // calculate reaction B
      if GreaterThanZero(fLengthArmLegB) then
      begin
        fForceB1 := fTorqueT1/fLengthArmLegB;
        fForceB2 := fTorqueT2/fLengthArmLegB;
      end;
    end;

    // Calcolo tolleranze Dm, Lk, T1, T1
    if fCheck then
    begin
      ASpringTolerance.WireDiameter     := fd;
      ASpringTolerance.MeanCoilDiameter := fDm;
      ASpringTolerance.ActiveCoils      := fn;
      ASpringTolerance.LegLength1       := LengthLegA;
      ASpringTolerance.LegLength2       := LengthLegB;

      //ASpringTolerance.Torque1          := FLoadF1;
      //ASpringTolerance.Torque2          := FLoadF2;
      //ASpringTolerance.FreeBodyLength := FLengthL0;
      //ASpringTolerance.NumActiveCoils := fn;
      //ASpringTolerance.SpringIndex    := fw;
      //ASpringTolerance.SpringRate     := fR;
      ASpringTolerance.Solve;

      fCheck := ErrorMessage.Count = 0;
    end;

    // calculate stress 1 & 2
    if fCheck then
    begin
      fSigmaz  := 0.7*fRm;
      fSigma1  := (32*fTorqueT1)/(pi*CubicPower(fd));
      fSigma2  := (32*fTorqueT2)/(pi*CubicPower(fd));
      fSigmaq1 := fq*fSigma1;
      fSigmaq2 := fq*fSigma2;
      fSigmaqh := fSigmaq2 - fSigmaq1;
    end;

    // calculate stress n
    if Check then
    begin
      fSigmaqn    := fSigmaz;
      fSigman     := fSigmaqn/fq;
      falphancoil := (2*pi*fn*fSigman/fE)*(fDm/fd);
      falphan     := falphancoil*(1 + fcA + fcB);
      fTorqueTn   := fRMR*falphancoil;
    end;

    // Calcolo lavoro molla W0n e W12
    if fCheck then
    begin
      fW12 := (FTorqueT2 + FTorqueT1) / 2 * (FAlpha2 - FAlpha1);
      fW0n := (FTorqueTn / 2) * (FAlphan);
    end;

    // Calcolo frequenza naturale del primo ordine della molla
    if fCheck then
    begin
      // La frequenza naturale del primo ordine della molla, avente entrambe le estremità vincolate ed
      // eccitare periodicamente ad una estremità durante il funzionamento, è determinata mediante la
      // seguente formula:

      // fNaturalFrequency := 2/ ( (fn*FDm*FDm) / (2*pi*sqrt(2)*fd) / SquareRoot(FG/FRho) );
    end;

    // Calcolo coefficente di sicurezza statico
    if fCheck then
    begin
      fStaticSafetyFactor := ScalarUnit.ToFloat(fSigmaz / fSigmaq2);

      if fStaticSafetyFactor < 1 then
        WarningMessage.Add('EN13906-1: Static safety factor < 1 !');

      fCheck := ErrorMessage.Count = 0;
    end;

    // Calcolo numero di cicli e coefficente di sicurezza a fatica
    if fCheck then
    begin
      if MAT.Name = '' then
        WarningMessage.Add('EN13906-1: Spring material data unavailable (dyn).');

      if GreaterThanZero(MAT.BendingStressSigmaStar ) and
         GreaterThanZero(MAT.BendingStressSigmaYield) and
         GreaterThanZero(MAT.BendingStressSigmaOE7  ) and
         GreaterThanZero(MAT.BendingStressSigmaUE7  ) then
      begin
        fSigmaoz := MAT.BendingStressSigmaYield;
        fSigmahz := 0*Pa;

        if fSigmaq1 >= MAT.BendingStressSigmaUE7 then
          Sigmah7 := fSigmaoz - fSigmaq1
        else
          Sigmah7 := MAT.BendingStressSigmaOE7 + (MAT.BendingStressSigmaYield - MAT.BendingStressSigmaOE7) / MAT.BendingStressSigmaUE7 * fSigmaq1 - fSigmaq1;

        if fSigmaq1 >= MAT.BendingStressSigmaUE6 then
          Sigmah6 := fSigmaoz - fSigmaq1
        else
          Sigmah6 := MAT.BendingStressSigmaOE6 + (MAT.BendingStressSigmaYield - MAT.BendingStressSigmaOE6) / MAT.BendingStressSigmaUE6 * fSigmaq1 - fSigmaq1;

        if fSigmaq1 >= MAT.BendingStressSigmaUE5 then
          Sigmah5 := fSigmaoz - fSigmaq1
        else
          Sigmah5 := MAT.BendingStressSigmaOE5 + (MAT.BendingStressSigmaYield - MAT.BendingStressSigmaOE5) / MAT.BendingStressSigmaUE5 * fSigmaq1 - fSigmaq1;

        if fSigmaqh <= Sigmah7 then
        begin
          fNumOfCycles := MAXFLOAT;
        end else
        begin
          if fSigmaqh <= Sigmah6 then
          begin
            mSigma := (fSigmaqh - Sigmah7)/(Sigmah6 - Sigmah7);
            fNumOfCycles := MAT.NumOfCyclesE7 - mSigma*(MAT.NumOfCyclesE7 - MAT.NumOfCyclesE6);
          end else
          begin
            if fSigmaqh <= Sigmah5 then
            begin
              mSigma := (fSigmaqh - Sigmah6)/(Sigmah5 - Sigmah6);
              fNumOfCycles := MAT.NumOfCyclesE6 - mSigma*(MAT.NumOfCyclesE6 - MAT.NumOfCyclesE5);
            end;
          end;
        end;
        fSigmahz := Sigmah7;

        fDynamicSafetyFactor := Sigmah7 / fSigmaqh;
      end;
    end;
  end;

  for i := 0 to ErrorMessage.Count -1 do Writeln(ErrorMessage[i]);
end;

function TTorsionSpringSolver.AlphaCoil(const Alpha: TQuantity): TQuantity;
begin
  Result := 0*rad;
  if fCheck then
  begin
    Result := Alpha/(1 + fcA + fcB);
  end;
end;

function TTorsionSpringSolver.Lk(const Alpha: TQuantity): TQuantity;
var
  fdMax: TQuantity;
begin
  Result := 0*m;
  if fCheck then
  begin
    fdMax  := fd + fdTolerance;
    if GreaterThanZero(fa) then
    begin
      Result := fn*(fa + fdMax) + fdMax;
    end;
    Result := Max(Result, fdMax*(fn + 1.0 + Alpha/2/pi));
  end;
end;

function TTorsionSpringSolver.InnerCoilDiameter(const Alpha: TQuantity): TQuantity;
begin
  result := fDm*fn/(fn + Alpha/2/pi) - fd;
end;

function TTorsionSpringSolver.GetSigma(const aTorque: TQuantity): TQuantity;
begin
  Result := (32*aTorque)/(pi*CubicPower(fd));
end;

function TTorsionSpringSolver.GetSigmaq(const aTorque: TQuantity): TQuantity;
begin
  Result := GetSigma(aTorque)*fq;
end;

end.
