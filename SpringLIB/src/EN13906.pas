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
  BGRABitmapTypes, Classes, EN10270, EN15800, Math, UtilsBase, SysUtils, UnitOfMeasurement;

type
  TCompressionSpringSolver = class
  private
    fCheck: boolean;
    fClosedEnds: boolean;
    FColdCoiled: boolean;
    Fd: TLength;
    Fdmax: TLength;
    FDe: TLength;
    FDeMax: TLength;
    FDi: TLength;
    FDiMin: TLength;
    FDm: TLength;
    FDeltaDe: TLength;
    fDynamicLoad: boolean;
    fDynamicSafetyFactor: double;
    FLoadF1: TForce;
    FLoadF2: TForce;
    FLoadFn: TForce;
    FLoadFc: TForce;
    fE: TPressure;
    fe1: TLength;
    fe2: TLength;
    fG: TPressure;
    fGroundEnds: boolean;
    fk: double;
    FLengthL0: TLength;
    FLengthL1: TLength;
    FLengthL2: TLength;
    FLengthLc: TLength;
    FLengthLn: TLength;
    fMass: TMass;
    fn: double;
    fNaturalFrequency: TFrequency;
    fNumOfCycles: double;
    fnt: double;
    fnu: double;
    fPitch: TLength;
    fPitchRatio: double;
    fR: TStiffness;
    fRho: TDensity;
    fRm: TPressure;
    FStrokeS1: TLength;
    FStrokeS2: TLength;
    FStrokeSh: TLength;
    FStrokeSc: TLength;
    FStrokeSn: TLength;
    fSa: TLength;
    fStaticSafetyFactor: double;
    fSk: TLength;
    fTau1: TPressure;
    fTau2: TPressure;
    fTauh: TPressure;
    fTauc: TPressure;
    fTaun: TPressure;
    fTauk1: TPressure;
    fTauk2: TPressure;
    fTaukh: TPressure;
    fTaukc: TPressure;
    fTaukn: TPressure;
    fTauhz: TPressure;
    fTauoz: TPressure;
    fTauz: TPressure;
    fw: double;
    fW0n: TWork;
    fW12: TWork;
    fWireLength: TLength;
    procedure PreCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
    function GetR(const aTemperature: double): TStiffness;
    function GetF1(const aTemperature: double): TForce;
    function GetF2(const aTemperature: double): TForce;
    function GetTau(const aLoadF: TForce): TPressure;
    function GetTauk(const aLoadF: TForce): TPressure;
    procedure GetBucklingCurve(var aPoints: ArrayOfTPointF);
  public
    property AdmDynamicTorsionalStressRangeTauhz: TPressure read fTauhz;
    property AdmDynamicTorsionalStressTauoz: TPressure read fTauoz;
    property AdmStaticTorsionalStressTauz: TPressure read fTauz;
    property ActiveColis: double read fn write fn;
    property Check: boolean read fCheck;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;
    property ColdCoiled: boolean read FColdCoiled write FColdCoiled;
    property De: TLength read FDe;
    property DeMax: TLength read FDeMax;
    property StrokeS1: TLength read FStrokeS1;
    property StrokeS2: TLength read FStrokeS2;
    property StrokeSh: TLength read FStrokeSh;
    property StrokeSc: TLength read FStrokeSc;
    property StrokeSn: TLength read FStrokeSn;
    property DeflectionSk: TLength read fSk;
    property DeltaDe: TLength read FDeltaDe;

    property Di: TLength read FDi;
    property DiMin: TLength read FDiMin;
    property Dm: TLength read FDm write FDm;
    property DynamicLoad: boolean read fDynamicLoad write fDynamicLoad;
    property DynamicSafetyFactor: double read fDynamicSafetyFactor;
    property LoadF1: TForce read FLoadF1;
    property LoadF2: TForce read FLoadF2;
    property LoadFc: TForce read FLoadFc;
    property LoadFn: TForce read FLoadFn;
    property EccentricityE1: TLength read fe1;
    property EccentricityE2: TLength read fe2;
    property GroundEnds: boolean read fGroundEnds write fGroundEnds;
    property CorrectionFactorK: double read fk;
    property LengthL0: TLength read FLengthL0 write FLengthL0;
    property LengthL1: TLength read FLengthL1 write FLengthL1;
    property LengthL2: TLength read FLengthL2 write FLengthL2;
    property LengthLc: TLength read FLengthLc;
    property LengthLn: TLength read FLengthLn;
    property Mass: TMass read fMass;
    property MaterialDensity: TDensity read fRho write fRho;
    property NaturalFrequency: TFrequency read fNaturalFrequency;
    property NumOfCycles: double read fNumOfCycles;
    property Pitch: TLength read fPitch;
    property PitchRatio: double read fPitchRatio;
    property SeatingCoefficent: double read fnu write fnu;
    property ShearModulus: TPressure read fG write fG;
    property SpringIndexW: double read fw;
    property SpringRateR: TStiffness read fR;
    property SpringWorkW0n: TWork read fW0n;
    property SpringWorkW12: TWork read fW12;
    property StaticSafetyFactor: double read fStaticSafetyFactor;
    property SumOfMinimumGapsSa: TLength read fSa;
    property TensileStrengthRm: TPressure read fRm write fRm;
    property TorsionalStressTau1: TPressure read fTau1;
    property TorsionalStressTau2: TPressure read fTau2;
    property TorsionalStressTauc: TPressure read fTauc;
    property TorsionalStressTauh: TPressure read fTauh;
    property TorsionalStressTaun: TPressure read fTaun;
    property TorsionalStressTauk1: TPressure read fTauk1;
    property TorsionalStressTauk2: TPressure read fTauk2;
    property TorsionalStressTaukc: TPressure read fTaukc;
    property TorsionalStressTaukh: TPressure read fTaukh;
    property TorsionalStressTaukn: TPressure read fTaukn;
    property TotalCoils: double read fnt write fnt;
    property YoungModulus: TPressure read fE write fE;
    property WireDiameter: TLength read Fd write Fd;
    property WireDiameterMax: TLength read Fdmax write Fdmax;
    property WireLength: TLength read fWireLength;
  end;

  TTorsionSpringSolver = class
  private
    fa: double;
    falpha1: double;
    falpha2: double;
    falpha1coil: double;
    falpha2coil: double;
    fbeta1: double;
    fbeta2: double;
    fCheck: boolean;
    fClampedEndA: boolean;
    fClampedEndB: boolean;
    fClosedCoils: boolean;
    fColdCoiled: boolean;
    fd: double;

    fDe: double;
    fDi: double;
    fDm: double;
    fdmax: double;

    fE: double;
    fG: double;
    fLk: array [0..2] of double;
    fM1: double;
    fM2: double;
    fn: double;
    fq: double;
    fRA: double;
    fRB: double;
    fRadialEndA: boolean;
    fRadialEndB: boolean;
    fRMR: double;

    fw: double;

    function CalcRadialEndFlex(ArmLength: double): double;
    function CalcTangentialEndFlex(ArmLength: double): double;
    procedure PreCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Solve;

  public


    property ActiveColis: double read fn write fn;
    property Alpha1: double read fAlpha1 write fAlpha1;
    property Alpha2: double read fAlpha2 write fAlpha2;


    property Check: boolean read fCheck;
    property ColdCoiled: boolean read fColdCoiled write fColdCoiled;

    property De: double read fDe;
    property Di: double read fDi;
    property Dm: double read fDm;

    property Pitch: double read fa;
    property ShearModulus: double read fG write fG;
    property YoungModulus: double read fE write fE;
    property WireDiameter: double read fd write fd;
    property WireDiameterMax: double read fdmax write fdmax;
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
  Fd                   := 0*mm;
  FDe                  := 0*mm;
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
  fR                   := 0*N_m;
  fRho                 := 0*kg_m3;
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
  fW0n                 := 0*Nm;
  fW12                 := 0*Nm;
  fWireLength          := 0*mm;
end;

procedure TCompressionSpringSolver.PreCheck;
begin
  fCheck := True;
  // Controllo validità dati inseriti
  if Fd         <= (0*mm)    then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if FDm        <= (0*mm)    then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE         <= (0*MPa)   then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG         <= (0*MPa)   then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if FLengthL0  <= (0*mm)    then ErrorMessage.Add('Spring length "L0" unassigned.');
  if FLengthL1  <= (0*mm)    then ErrorMessage.Add('Spring length "L1" unassigned.');
  if FLengthL2  <= (0*mm)    then ErrorMessage.Add('Spring length "L2" unassigned.');
  if fn         <= (0)       then ErrorMessage.Add('Number of active coil "n" unassigned.');
  if fnt        <= (0)       then ErrorMessage.Add('Number of total coil "nt" unassigned.');
  if fnu        <= (0)       then ErrorMessage.Add('Seat coefficent "nu" unassigned.');
  if fRm        <= (0*MPa)   then ErrorMessage.Add('Tensile strength "Rm" unassigned.');
  if fRho       <= (0*kg_m3) then ErrorMessage.Add('Material density "rho" unassigned.');

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
  mDe: TLength;
  mSk: double;
  mTau: double;
  Tauh7, Tauh6, Tauh5: TPressure;
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
      FDeltaDe := (mDe*mDe)/mDe;


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
    fWireLength := FDm * pi * fnt;
  end;

  // Calcolo massa della molla
  if fCheck then
  begin
    fMass := FRho * ((pi*(Fd*Fd)/4) * FWireLength);
  end;

  // Calcolo frequenza naturale del primo ordine della molla
  if fCheck then
  begin
    // La frequenza naturale del primo ordine della molla, avente entrambe le estremità vincolate ed
    // eccitare periodicamente ad una estremità durante il funzionamento, è determinata mediante la
    // seguente formula:
    fNaturalFrequency := (3560*Fd/FDm) / (fn) * ((Sqrt(FG.Value/FRho.Value)*m_s)/FDm);
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

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo sollecitazioni torsionali Tau
  if fCheck then
  begin
    // Mentre la sollecitazione torsionale "tau" deve essere adottata per il calcolo delle molle
    // caricate staticamente o quasi staticamente, per le molle caricate dinamicamente si deve
    // impiegare la sollecitazione torsionale corretta "tauk".
    fTau1  := (fG * Fd * FStrokeS1) / (Pi * fn * (Power(FDm.Value, 2)*m2));
    fTau2  := (fG * Fd * FStrokeS2) / (Pi * fn * (Power(FDm.Value, 2)*m2));
    fTauh  := fTau2 - fTau1;
    fTaun  := (fG * Fd * FStrokeSn) / (Pi * fn * (Power(FDm.Value, 2)*m2));
    fTauc  := (fG * Fd * FStrokeSc) / (Pi * fn * (Power(FDm.Value, 2)*m2));
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
end;

function TCompressionSpringSolver.GetR(const aTemperature: double): TStiffness;
begin
  if fCheck then
    Result := MAT.GetG(aTemperature) * ((Power(Fd.Value, 4)*m4) / (8* Power(FDm.Value, 3)*m3 * fn))
  else
    Result := 0*N_mm;
end;

function TCompressionSpringSolver.GetF1(const aTemperature: double): TForce;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS1
  else
    Result := 0*N;
end;

function TCompressionSpringSolver.GetF2(const aTemperature: double): TForce;
begin
  if fCheck then
    Result := GetR(aTemperature) * FStrokeS2
  else
    Result := 0*N;
end;

function TCompressionSpringSolver.GetTau(const aLoadF: TForce): TPressure;
begin
  try
    Result := 8 * (aLoadF) * (FDm / (Pi * Power(Fd.Value, 3)*m3));
  except
    Result := 0*MPa;
  end;
end;

function TCompressionSpringSolver.GetTauk(const aLoadF: TForce): TPressure;
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

      SeTLength(aPoints, Length(aPoints) + 1);
      aPoints[High(aPoints)].x := Value;
      aPoints[High(aPoints)].y := Y;
      Y := Y - DY;
    until Value > (1.25*(fnu*FLengthL0/FDm));
  except
    aPoints := nil;
  end;
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
  fa           := 0;
  falpha1      := 0;
  falpha2      := 0;
  falpha1coil  := 0;
  falpha2coil  := 0;
  fbeta1       := 0;
  fbeta2       := 0;
  fClampedEndA := False;
  fClampedEndB := False;
  fClosedCoils := True;
  fLk[0]       := 0;
  fLk[1]       := 0;
  fLk[2]       := 0;
  fM1          := 0;
  fM2          := 0;
  fq           := 0;
  fRA          := 0;
  fRB          := 0;
  fRadialEndA  := False;
  fRadialEndB  := False;
end;

procedure TTorsionSpringSolver.PreCheck;
begin
  fCheck := True;
  if fd   <= 0    then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if fDm  <= 0    then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE   <= 0    then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG   <= 0    then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if fn   <= 0    then ErrorMessage.Add('Number of active coil "n" unassigned.');

  if fColdCoiled then
    if (fd > 20) then ErrorMessage.Add('Wire diameter > 20.')
  else
    if (fd > 10) then ErrorMessage.Add('Wire diameter > 10.');

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

function TTorsionSpringSolver.CalcRadialEndFlex(ArmLength: double): double;
begin
  Result := Power(2*ArmLength/fDm -1, 3)/(24*pi*fn*Power(ArmLength/fDm, 2));
end;

function TTorsionSpringSolver.CalcTangentialEndFlex(ArmLength: double): double;
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
    fRMR := (Power(fd, 4)) *fE/(3667*fDm*fn);

    if fClosedCoils then
    begin
      fLk[0] := (fn + 1.5              )*fdmax;
      fLk[1] := (fn + 1.5 + falpha1/360)*fdmax;
      fLk[2] := (fn + 1.5 + falpha2/360)*fdmax;
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


    fM1 := Power(fd, 4)*fE*falpha1coil/(3667*fDm*fn);
    fM2 := Power(fd, 4)*fE*falpha2coil/(3667*fDm*fn);







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
