{ EN13906 Helical Spring Designer

  Copyright (C) 2022-2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit EN13906;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, EN10270, EN15800, Math, UtilsBase, SysUtils, Dim;

type
  TCompressionSpringSolver = class
  private
    fCheck: boolean;
    fClosedEnds: boolean;
    FColdCoiled: boolean;
    FCycleFrequency: THertz;
    Fd: TMeters;
    Fdmax: TMeters;
    FDe: TMeters;
    FDeMax: TMeters;
    FDi: TMeters;
    FDiMin: TMeters;
    FDm: TMeters;
    FDeltaDe: TMeters;
    fDynamicLoad: boolean;
    fDynamicSafetyFactor: double;
    FLoadF1: TNewtons;
    FLoadF2: TNewtons;
    FLoadFn: TNewtons;
    FLoadFc: TNewtons;
    fE: TPascals;
    fe1: TMeters;
    fe2: TMeters;
    fG: TPascals;
    fGroundEnds: boolean;
    fk: double;
    FLengthL0: TMeters;
    FLengthL1: TMeters;
    FLengthL2: TMeters;
    FLengthLc: TMeters;
    FLengthLn: TMeters;
    fMass: TKilograms;
    fn: double;
    fNaturalFrequency: THertz;
    fNumOfCycles: double;
    fnt: double;
    fnu: double;
    fPitch: TMeters;
    fPitchRatio: double;
    fR: TNewtonsPerMeter;
    fRho: TKilogramsPerCubicMeter;
    fRm: TPascals;
    FStrokeS1: TMeters;
    FStrokeS2: TMeters;
    FStrokeSh: TMeters;
    FStrokeSc: TMeters;
    FStrokeSn: TMeters;
    fSa: TMeters;
    fStaticSafetyFactor: double;
    fSk: TMeters;
    fTau1: TPascals;
    fTau2: TPascals;
    fTauh: TPascals;
    fTauc: TPascals;
    fTaun: TPascals;
    fTauk1: TPascals;
    fTauk2: TPascals;
    fTaukh: TPascals;
    fTaukc: TPascals;
    fTaukn: TPascals;
    fTauhz: TPascals;
    fTauoz: TPascals;
    fTauz: TPascals;
    fw: double;
    fW0n: TJoules;
    fW12: TJoules;
    fWireLength: TMeters;
    procedure PreCheck;
    procedure PostCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
    function GetR(const aTemperature: double): TNewtonsPerMeter;
    function GetF1(const aTemperature: double): TNewtons;
    function GetF2(const aTemperature: double): TNewtons;
    function GetTau(const aLoadF: TNewtons): TPascals;
    function GetTauk(const aLoadF: TNewtons): TPascals;
    procedure GetBucklingCurve(var aPoints: ArrayOfTPointF);
  public
    property AdmDynamicTorsionalStressRangeTauhz: TPascals read fTauhz;
    property AdmDynamicTorsionalStressTauoz: TPascals read fTauoz;
    property AdmStaticTorsionalStressTauz: TPascals read fTauz;
    property ActiveColis: double read fn write fn;
    property Check: boolean read fCheck;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;
    property ColdCoiled: boolean read FColdCoiled write FColdCoiled;
    property CycleFrequency:THertz read FCycleFrequency write FCycleFrequency;
    property De: TMeters read FDe;
    property DeMax: TMeters read FDeMax;
    property StrokeS1: TMeters read FStrokeS1;
    property StrokeS2: TMeters read FStrokeS2;
    property StrokeSh: TMeters read FStrokeSh;
    property StrokeSc: TMeters read FStrokeSc;
    property StrokeSn: TMeters read FStrokeSn;
    property DeflectionSk: TMeters read fSk;
    property DeltaDe: TMeters read FDeltaDe;

    property Di: TMeters read FDi;
    property DiMin: TMeters read FDiMin;
    property Dm: TMeters read FDm write FDm;
    property DynamicLoad: boolean read fDynamicLoad write fDynamicLoad;
    property DynamicSafetyFactor: double read fDynamicSafetyFactor;
    property LoadF1: TNewtons read FLoadF1;
    property LoadF2: TNewtons read FLoadF2;
    property LoadFc: TNewtons read FLoadFc;
    property LoadFn: TNewtons read FLoadFn;
    property EccentricityE1: TMeters read fe1;
    property EccentricityE2: TMeters read fe2;
    property GroundEnds: boolean read fGroundEnds write fGroundEnds;
    property CorrectionFactorK: double read fk;
    property LengthL0: TMeters read FLengthL0 write FLengthL0;
    property LengthL1: TMeters read FLengthL1 write FLengthL1;
    property LengthL2: TMeters read FLengthL2 write FLengthL2;
    property LengthLc: TMeters read FLengthLc;
    property LengthLn: TMeters read FLengthLn;
    property Mass: TKilograms read fMass;
    property MaterialDensity: TKilogramsPerCubicMeter read fRho write fRho;
    property NaturalFrequency: THertz read fNaturalFrequency;
    property NumOfCycles: double read fNumOfCycles;
    property Pitch: TMeters read fPitch;
    property PitchRatio: double read fPitchRatio;
    property SeatingCoefficent: double read fnu write fnu;
    property ShearModulus: TPascals read fG write fG;
    property SpringIndexW: double read fw;
    property SpringRateR: TNewtonsPerMeter read fR;
    property SpringWorkW0n: TJoules read fW0n;
    property SpringWorkW12: TJoules read fW12;
    property StaticSafetyFactor: double read fStaticSafetyFactor;
    property SumOfMinimumGapsSa: TMeters read fSa;
    property TensileStrengthRm: TPascals read fRm write fRm;
    property TorsionalStressTau1: TPascals read fTau1;
    property TorsionalStressTau2: TPascals read fTau2;
    property TorsionalStressTauc: TPascals read fTauc;
    property TorsionalStressTauh: TPascals read fTauh;
    property TorsionalStressTaun: TPascals read fTaun;
    property TorsionalStressTauk1: TPascals read fTauk1;
    property TorsionalStressTauk2: TPascals read fTauk2;
    property TorsionalStressTaukc: TPascals read fTaukc;
    property TorsionalStressTaukh: TPascals read fTaukh;
    property TorsionalStressTaukn: TPascals read fTaukn;
    property TotalCoils: double read fnt write fnt;
    property YoungModulus: TPascals read fE write fE;
    property WireDiameter: TMeters read Fd write Fd;
    property WireDiameterMax: TMeters read Fdmax write Fdmax;
    property WireLength: TMeters read fWireLength;
  end;

  TTorsionSpringSolver = class
  private
    fa: TMeters;
    falpha1: TRadians;
    falpha2: TRadians;
    falpha1coil: TRadians;
    falpha2coil: TRadians;
    fbeta1: TRadians;
    fbeta2: TRadians;
    fCheck: boolean;
    fClampedEndA: boolean;
    fClampedEndB: boolean;
    fClosedCoils: boolean;
    fColdCoiled: boolean;
    fd: TMeters;

    fDe: TMeters;
    fDi: TMeters;
    fDm: TMeters;
    fdmax: TMeters;

    fE: TPascals;
    fG: TPascals;
    fLk: array [0..2] of TMeters;
    fM1: TJoules;
    fM2: TJoules;
    fn: double;
    fq: double;
    fRA: TMeters;
    fRB: TMeters;
    fRadialEndA: boolean;
    fRadialEndB: boolean;
    fRMR: TJoules;

    fw: double;

    function CalcRadialEndFlex(ArmLength: TMeters): double;
    function CalcTangentialEndFlex(ArmLength: TMeters): double;
    procedure PreCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Solve;

  public


    property ActiveColis: double read fn write fn;
    property Alpha1: TRadians read fAlpha1 write fAlpha1;
    property Alpha2: TRadians read fAlpha2 write fAlpha2;


    property Check: boolean read fCheck;
    property ColdCoiled: boolean read fColdCoiled write fColdCoiled;

    property De: TMeters read fDe;
    property Di: TMeters read fDi;
    property Dm: TMeters read fDm;

    property Pitch: TMeters read fa;
    property ShearModulus: TPascals read fG write fG;
    property YoungModulus: TPascals read fE write fE;
    property WireDiameter: TMeters read fd write fd;
    property WireDiameterMax: TMeters read fdmax write fdmax;
  end;


var
  SOLVER: TCompressionSpringSolver;


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
  FDeMax               := 0*mm;
  FDeltaDe             := 0*mm;
  FDi                  := 0*mm;
  FDm                  := 0*mm;
  Fdmax                := 0*mm;
  fDynamicLoad         := False;
  fDynamicSafetyFactor := 0;
  fE                   := 0*MPa;
  fe1                  := 0*mm;
  fe2                  := 0*mm;
  FLoadF1              := 0*N;
  FLoadF2              := 0*N;
  FLoadFn              := 0*N;
  FLoadFc              := 0*N;
  fG                   := 0*MPa;
  fGroundEnds          := True;
  fk                   := 0;
  FLengthL0            := 0*mm;
  FLengthL1            := 0*mm;
  FLengthL2            := 0*mm;
  FLengthLc            := 0*mm;
  FLengthLn            := 0*mm;
  fMass                := 0*kg;
  fn                   := 0;
  fNaturalFrequency    := 0*Hz;
  fnu                  := 0;
  fNumOfCycles         := 0;
  fnt                  := 0;
  fPitch               := 0*mm;
  fPitchRatio          := 0;
  fR                   := 0*(N/m);
  fRho                 := 0*(kg/m3);
  fRm                  := 0*MPa;
  FStrokeS1            := 0*mm;
  FStrokeS2            := 0*mm;
  FStrokeSh            := 0*mm;
  FStrokeSn            := 0*mm;
  FStrokeSc            := 0*mm;

  fSa                  := 0*mm;

  fSk                  := 0*mm;

  fStaticSafetyFactor  := 0;
  fTau1                := 0*MPa;
  fTau2                := 0*MPa;
  fTauc                := 0*MPa;
  fTauh                := 0*MPa;
  fTauk1               := 0*MPa;
  fTauk2               := 0*MPa;
  fTaukc               := 0*MPa;
  fTaukh               := 0*MPa;
  fTaukn               := 0*MPa;
  fTauhz               := 0*MPa;
  fTaun                := 0*MPa;
  fTauoz               := 0*MPa;
  fTauz                := 0*MPa;
  fw                   := 0;
  fW0n                 := 0*J;
  fW12                 := 0*J;
  fWireLength          := 0*mm;
end;

procedure TCompressionSpringSolver.PreCheck;
begin
  fCheck := True;
  // Controllo validità dati inseriti
  if Fd.Value        <= 0  then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if FDm.Value       <= 0 then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE.Value        <= 0 then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG.Value        <= 0 then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if FLengthL0.Value <= 0 then ErrorMessage.Add('Spring length "L0" unassigned.');
  if FLengthL1.Value <= 0 then ErrorMessage.Add('Spring length "L1" unassigned.');
  if FLengthL2.Value <= 0 then ErrorMessage.Add('Spring length "L2" unassigned.');
  if fn              <= 0 then ErrorMessage.Add('Number of active coil "n" unassigned.');
  if fnt             <= 0 then ErrorMessage.Add('Number of total coil "nt" unassigned.');
  if fnu             <= 0 then ErrorMessage.Add('Seat coefficent "nu" unassigned.');
  if fRm.Value       <= 0 then ErrorMessage.Add('Tensile strength "Rm" unassigned.');
  if fRho.Value      <= 0 then ErrorMessage.Add('Material density "rho" unassigned.');

  if FLengthL0 <= FLengthL1 then ErrorMessage.Add('Wrong L0 and L1 values, L0 must be > L1.');
  if FLengthL1 <= FLengthL2 then ErrorMessage.Add('Wrong L1 and L2 values, L1 must be > L2.');

  fCheck := ErrorMessage.Count = 0;

  // Poisson's ratio
  if fCheck then
  begin
    if (fE / (2 * fG) - 1) > 0.45 then ErrorMessage.Add('Wrong values for Young''s modulus "E" and Shear modulus "G".');
    if (fE / (2 * fG) - 1) < 0.10 then ErrorMessage.Add('Wrong values for Young''s modulus "E" and Shear modulus "G".');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Scopo e campo di applicazione della norma
  if fCheck then
  begin
    fw := FDm / Fd;
    if FColdCoiled then
    begin
      if Fd > (20*mm) then WarningMessage.Add('Wire diameter > 20mm.');
      if fn < (2)     then WarningMessage.Add('Number of active coils < 2.');
      if fw < (4)     then WarningMessage.Add('Spring Index < 4.');
      if fw > (20)    then WarningMessage.Add('Spring Index > 20.');
    end else
    begin
      if Fd < (8*mm)   then WarningMessage.Add('Wire diameter < 8mm.');
      if Fd > (100*mm) then WarningMessage.Add('Wire diameter > 100mm.');
      if fn < (3)      then WarningMessage.Add('Number of active coils < 3.');
      if fw < (3)      then WarningMessage.Add('Spring Index < 3.');
      if fw > (12)     then WarningMessage.Add('Spring Index > 12.');
    end;

    if (fClosedEnds = True) then
    begin
      if (FColdCoiled = False) and (fGroundEnds = False) then
        ErrorMessage.Add('Length "Lc" undefined.');
    end else
    begin
      if (FColdCoiled <> False) and (fGroundEnds <> False) then
        ErrorMessage.Add('Length "Lc" undefined.');
    end;

    if (fClosedEnds <> GroundEnds = True) then
    begin
      ErrorMessage.Add('"DeltaDE" undefined.');
    end;

    fCheck := ErrorMessage.Count = 0;
  end;
end;

procedure TCompressionSpringSolver.Solve;
var
  mDe: TMeters;
  mSk: double;
  mTau: double;
  Tauh7, Tauh6, Tauh5: TPascals;
begin
  PreCheck;
  // Norma EN13906-1:2013
  // Calcolo molla a compressione ad elica cilindrica fabbricate con filo sezione circolare.

  if fCheck then
  begin
    FDe := FDm + Fd;
    FDi := FDm - Fd;
    // Spring index
    fw  := FDm / Fd;
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
        FLengthLc := fnt * Fdmax
      else
        if (FColdCoiled = True) and (fGroundEnds = False) then
          FLengthLc := (fnt + 1.5) * Fdmax
        else
          if (FColdCoiled = False) and (fGroundEnds = True) then
            FLengthLc := (fnt - 0.3) * Fdmax
          else
           FLengthLc := (0*mm);
    end else
    begin
      if (FColdCoiled = False) and (fGroundEnds = False) then
        FLengthLc := (fnt + 1.1) * Fdmax
      else
        FLengthLc := (0*mm);
    end;

    if FLengthLc <= (0*mm)    then ErrorMessage.Add('Wrong Lc value, Lc must be > 0.');
    if FLengthLc >  FLengthL0 then ErrorMessage.Add('Wrong L0 or nt value, Lc must be < L0.');
    if FLengthLc >  FLengthL1 then ErrorMessage.Add('Wrong L1 or nt value, Lc must be < L1.');
    if FLengthLc >  FLengthL2 then ErrorMessage.Add('Wrong L2 or nt value, Lc must be < L2.');

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

    if FLengthLn < FLengthLc then ErrorMessage  .Add('Error: Wrong Ln value, Ln must be > Lc.');
    if FLengthLn > FLengthL0 then ErrorMessage  .Add('Error: Wrong Ln value, Ln must be < L0.');
    if FLengthLn > FLengthL1 then WarningMessage.Add('Warning: Wrong Ln value, Ln must be < L1.');
    if FLengthLn > FLengthL2 then WarningMessage.Add('Warning: Wrong Ln value, Ln must be < L2.');

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
    fPitchRatio := FDm/fPitch;
  end;

  // Calcolo incremento del diamtro esterno della molla sotto carico
  if fCheck then
  begin
    // Quando la molla è sottoposta a compressione, il diametro della spira aumenta leggermente.
    // L'incremento del diamtro esterno della molla sotto carico, DeltaDe, è determinato usando la
    // formula sottostante, valida per molla a pacco LengthLc YoungModulus per estremità della molla
    // liberamente appoggiate.
    if (fClosedEnds) and (GroundEnds) then
      mDe := (FStrokeSc + fn * Fd) / fn
    else
      if (not fClosedEnds) and (not GroundEnds) then
        mDe := (FStrokeSc + (fn + 1.5) * Fd) / fn
      else
        mDe := 0*mm;

    if mDe = (0*mm) then
    begin
      WarningMessage.Add('DeltaDe undefined. Please change kind spring ends.');
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
    fR := (fG/(8*fn))*(Fd*Fd*Fd*Fd/FDm/FDm/FDm);
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
    TOL.WireDiameter   := Fd;
    TOL.CoilDiameterDm := FDm;
    TOL.LoadF1         := FLoadF1;
    TOL.LoadF2         := FLoadF2;
    TOL.LengthL0       := FLengthL0;
    TOL.ActiveCoils    := fn;
    TOL.SpringRateR    := fR;
    TOL.SpringIndexW   := fw;
    TOL.Solve;

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo diametro minimo e massimo della molla in esercizio:
  if fCheck then
  begin
    FDiMin := FDi - TOL.CoilDiameterTolerance - (Fdmax - Fd);
    FDeMax := FDe + TOL.CoilDiameterTolerance + (Fdmax - Fd) + FDeltaDe;
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
    fNaturalFrequency := (Fd/FDm) / (2*pi*sqrt(2)*fn) * (Sqrt(FG.Value/FRho.Value)*(m/s))/FDm;
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
    mSk := 1 - (1 - fG/fE) / (0.5 + fG/fE) * Sqr((pi*FDm.Value)/(fnu*FLengthL0.Value));

    if mSk < 0 then
    begin
      ErrorMessage.Add('Wrong G and E values, unable to calculate Sk.');
    end else
    begin
      fSk := FLengthL0 * (0.5 / (1 - fG/fE) * (1 - Sqrt(mSk)));
    end;
    // La sicurezza contro l'instabilità è raggiunta in teoria per un valore immaginario della radice
    // quadrata e per Sk/s > 1.
    if not ((fSk / FStrokeSc) > 1) then WarningMessage.Add('Warning: Buckling!.');

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
    fStaticSafetyFactor := fTauz / fTauc;

    if fStaticSafetyFactor < 1 then
      ErrorMessage.Add('Static safety factor < 1 !');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo numero di cicli e coefficente di sicurezza a fatica
  if fCheck then
  begin
    if MAT.ItemIndex = -1 then
      WarningMessage.Add('Warning: spring material data unavailable (dyn).');

    if (MAT.TorsionalStressTauStar  > 0*MPa) and
       (MAT.TorsionalStressTauYield > 0*MPa) and
       (MAT.TorsionalStressTauOE7   > 0*MPa) and
       (MAT.TorsionalStressTauUE7   > 0*MPa) then
    begin
      fTauoz := MAT.TorsionalStressTauYield;
      fTauhz := 0*MPa;

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
  PostCheck;
end;

function TCompressionSpringSolver.GetR(const aTemperature: double): TNewtonsPerMeter;
begin
  if fCheck then
    Result := MAT.GetG(aTemperature) * (Fd*Fd*Fd*Fd / (8*fn*FDm*FDm*FDm))
  else
    Result.Value := 0;
end;

function TCompressionSpringSolver.GetF1(const aTemperature: double): TNewtons;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS1
  else
    Result.Value := 0;
end;

function TCompressionSpringSolver.GetF2(const aTemperature: double): TNewtons;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS2
  else
    Result.Value := 0;
end;

function TCompressionSpringSolver.GetTau(const aLoadF: TNewtons): TPascals;
begin
  if Fd.Value <> 0 then
    Result := 8 * (aLoadF)/(Fd*Fd*Pi) * (FDm/Fd)
  else
    Result.Value := 0;
end;

function TCompressionSpringSolver.GetTauk(const aLoadF: TNewtons): TPascals;
begin
  Result := GetTau(aLoadF) * fk;
end;

procedure TCompressionSpringSolver.GetBucklingCurve(var aPoints: ArrayOfTPointF);
var
  Value: single;
  Y, DY: single;
begin
  try
    Y  := 0.980;
    DY := 0.001;
    repeat
      Value := Pi/(Sqrt((1-Sqr(1-Y*2*(1-fG/fE)))*(0.5+fG/fE)/(1-fG/fE)));

      SetLength(aPoints, Length(aPoints) + 1);
      aPoints[High(aPoints)].x := Value;
      aPoints[High(aPoints)].y := Y;
      Y := Y - DY;
    until Value > (1.25*(fnu*FLengthL0/FDm));
  except
    aPoints := nil;
  end;
end;

procedure TCompressionSpringSolver.PostCheck;
begin
  if (SOLVER.fnt - SOLVER.fn) < 2 then WarningMessage.Add('Warning: inactive end coild < 1');

  if (FLoadF2 + TOL.LoadF2Tolerance) > FLoadFn then WarningMessage.Add('Warning: Load F2 max > Fn');
  if (FLoadF2 + TOL.LoadF2Tolerance) > FLoadFc then WarningMessage.Add('Warning: Load F2 max > Fc');
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
  fa.Value          := 0;
  falpha1.Value     := 0;
  falpha2.Value     := 0;
  falpha1coil.Value := 0;
  falpha2coil.Value := 0;
  fbeta1.Value      := 0;
  fbeta2.Value      := 0;
  fClampedEndA      := False;
  fClampedEndB      := False;
  fClosedCoils      := True;
  fLk[0].Value      := 0;
  fLk[1].Value      := 0;
  fLk[2].Value      := 0;
  fM1.Value         := 0;
  fM2.Value         := 0;
  fq                := 0;
  fRA.Value         := 0;
  fRB.Value         := 0;
  fRadialEndA       := False;
  fRadialEndB       := False;
end;

procedure TTorsionSpringSolver.PreCheck;
begin
  fCheck := True;
  if fd.Value  <= 0 then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if fDm.Value <= 0 then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE.Value  <= 0 then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG.Value  <= 0 then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if fn        <= 0 then ErrorMessage.Add('Number of active coil "n" unassigned.');

  if fColdCoiled then
    if (fd > (20*mm)) then ErrorMessage.Add('Wire diameter > 20.')
  else
    if (fd > (10*mm)) then ErrorMessage.Add('Wire diameter > 10.');

  if fn <= 2 then ErrorMessage.Add('Number of active coil "n" < 2.');

  fCheck := ErrorMessage.Count = 0;

  if fCheck then
  begin
    fw := fDm/fd;
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

function TTorsionSpringSolver.CalcRadialEndFlex(ArmLength: TMeters): double;
begin
  Result := Power(2*ArmLength/fDm -1, 3)/(24*pi*fn*Power(ArmLength/fDm, 2));
end;

function TTorsionSpringSolver.CalcTangentialEndFlex(ArmLength: TMeters): double;
begin
  Result := (4*Power(ArmLength/fDm, 2) -1)/(12*pi*fn*(ArmLength/fDm));
end;

procedure TTorsionSpringSolver.Solve;
var
  cA, cB: double;
begin
  PreCheck;

  if fCheck then
  begin
    fDe  := fDm + fd;
    fDi  := fDm - fd;
    fRMR := (fE*fd*fd*fd*(fd/fDm))*(64/fn);

    if fClosedCoils then
    begin
      fLk[0] := (fn + 1.5)*fdmax;
      fLk[1] := (fn + 1.5 + falpha1/(2*pi*rad))*fdmax;
      fLk[2] := (fn + 1.5 + falpha2/(2*pi*rad))*fdmax;
    end else
    begin
      fLk[0] := fn*(fa + fdmax) + fdmax;
    end;
    fq := (fw +0.07) / (fw -0.75);

    cA := 0;
    if (not fClampedEndA) then
    begin
      if fRadialEndA then
        cA := CalcRadialEndFlex(fRA)
      else
        cA := CalcTangentialEndFlex(fRA);
    end;

    cB := 0;
    if (not fClampedEndB) then
    begin
      if fRadialEndB then
        cB := CalcRadialEndFlex(fRB)
      else
        cB := CalcTangentialEndFlex(fRB);
    end;

    falpha1coil := falpha1/(1 + cA + cB);
    falpha2coil := falpha2/(1 + cA + cB);
    fbeta1      := falpha1*(cA + cB);
    fbeta2      := falpha2*(cA + cB);


    fM1 := (fE*fd*fd*fd*(fd/fDm))*(falpha1coil/(64*rad*fn));
    fM2 := (fE*fd*fd*fd*(fd/fDm))*(falpha2coil/(64*rad*fn));







  end;

end;


initialization
begin
  SOLVER := TCompressionSpringSolver.Create;
end;

finalization
begin
  SOLVER.Destroy;
end;

end.
