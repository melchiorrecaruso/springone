{ EN13906 Helical Spring Designer

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

unit EN13906;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, EN10270, EN15800, Math, UtilsBase, SysUtils;

type
  TCompressionSpringSolver = class
  private
    fCheck: boolean;
    fClosedEnds: boolean;
    fColdCoiled: boolean;
    fd: double;
    fdmax: double;
    fDe: double;
    fDeMax: double;
    fDi: double;
    fDiMin: double;
    fDm: double;
    fDeltaDe: double;
    fDynamicLoad: boolean;
    fDynamicSafetyFactor: double;
    fF1: double;
    fF2: double;
    fFn: double;
    fFc: double;
    fE: double;
    fe1: double;
    fe2: double;
    fG: double;
    fGroundEnds: boolean;
    fk: double;
    fL0: double;
    fL1: double;
    fL2: double;
    fLc: double;
    fLn: double;
    fMass: double;
    fn: double;
    fNaturalFrequency: double;
    fNumOfCycles: double;
    fnt: double;
    fnu: double;
    fPitch: double;
    fPitchRatio: double;
    fR: double;
    fRho: double;
    fRm: double;
    fS1: double;
    fS2: double;
    fSh: double;
    fSc: double;
    fSn: double;
    fSa: double;
    fStaticSafetyFactor: double;
    fSk: double;
    fTau1: double;
    fTau2: double;
    fTauh: double;
    fTauc: double;
    fTaun: double;
    fTauk1: double;
    fTauk2: double;
    fTaukh: double;
    fTaukc: double;
    fTaukn: double;
    fTauhz: double;
    fTauoz: double;
    fTauz: double;
    fw: double;
    fW0n: double;
    fW12: double;
    fWireLength: double;
    procedure PreCheck;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
    function GetR(const aTemperature: double): double;
    function GetF1(const aTemperature: double): double;
    function GetF2(const aTemperature: double): double;
    function GetTau(const aLoadF: double): double;
    function GetTauk(const aLoadF: double): double;
  public
    property AdmDynamicTorsionalStressRangeTauhz: double read fTauhz;
    property AdmDynamicTorsionalStressTauoz: double read fTauoz;
    property AdmStaticTorsionalStressTauz: double read fTauz;
    property ActiveColis: double read fn write fn;
    property Check: boolean read fCheck;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;
    property ColdCoiled: boolean read fColdCoiled write fColdCoiled;
    property De: double read fDe;
    property DeMax: double read fDeMax;
    property DeflectionS1: double read fS1;
    property DeflectionS2: double read fS2;
    property DeflectionSh: double read fSh;
    property DeflectionSc: double read fSc;
    property DeflectionSn: double read fSn;
    property DeflectionSk: double read fSk;
    property DeltaDe: double read fDeltaDe;

    property Di: double read fDi;
    property DiMin: double read fDiMin;
    property Dm: double read fDm write fDm;
    property DynamicLoad: boolean read fDynamicLoad write fDynamicLoad;
    property DynamicSafetyFactor: double read fDynamicSafetyFactor;
    property LoadF1: double read fF1;
    property LoadF2: double read fF2;
    property LoadFc: double read fFc;
    property LoadFn: double read fFn;
    property EccentricityE1: double read fe1;
    property EccentricityE2: double read fe2;
    property GroundEnds: boolean read fGroundEnds write fGroundEnds;
    property CorrectionFactorK: double read fk;
    property LengthL0: double read fL0 write fL0;
    property LengthL1: double read fL1 write fL1;
    property LengthL2: double read fL2 write fL2;
    property LengthLc: double read fLc;
    property LengthLn: double read fLn;
    property Mass: double read fMass;
    property MaterialDensity: double read fRho write fRho;
    property NaturalFrequency: double read fNaturalFrequency;
    property NumOfCycles: double read fNumOfCycles;
    property Pitch: double read fPitch;
    property PitchRatio: double read fPitchRatio;
    property SeatingCoefficent: double read fnu write fnu;
    property ShearModulus: double read fG write fG;
    property SpringIndexW: double read fw;
    property SpringRateR: double read fR;
    property SpringWorkW0n: double read fW0n;
    property SpringWorkW12: double read fW12;
    property StaticSafetyFactor: double read fStaticSafetyFactor;
    property SumOfMinimumGapsSa: double read fSa;
    property TensileStrengthRm: double read fRm write fRm;
    property TorsionalStressTau1: double read fTau1;
    property TorsionalStressTau2: double read fTau2;
    property TorsionalStressTauc: double read fTauc;
    property TorsionalStressTauh: double read fTauh;
    property TorsionalStressTaun: double read fTaun;
    property TorsionalStressTauk1: double read fTauk1;
    property TorsionalStressTauk2: double read fTauk2;
    property TorsionalStressTaukc: double read fTaukc;
    property TorsionalStressTaukh: double read fTaukh;
    property TorsionalStressTaukn: double read fTaukn;
    property TotalCoils: double read fnt write fnt;
    property YoungModulus: double read fE write fE;
    property WireDiameter: double read fd write fd;
    property WireDiameterMax: double read fdmax write fdmax;
    property WireLength: double read fWireLength;
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
  {$IFDEF ONE}
  SOLVER: TCompressionSpringSolver;
  {$ENDIF}
  {$IFDEF THREE}
  SOLVER: TTorsionSpringSolver;
  {$ENDIF}

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
  fColdCoiled          := True;
  fd                   := 0;
  fDe                  := 0;
  fDeltaDe             := 0;
  fDi                  := 0;
  fDm                  := 0;
  fdmax                := 0;
  fDynamicLoad         := False;
  fDynamicSafetyFactor := 0;
  fE                   := 0;
  fe1                  := 0;
  fe2                  := 0;
  fG                   := 0;
  fF1                  := 0;
  fF2                  := 0;
  fFn                  := 0;
  fFc                  := 0;
  fG                   := 0;
  fGroundEnds          := True;
  fk                   := 0;
  fL0                  := 0;
  fL1                  := 0;
  fL2                  := 0;
  fLc                  := 0;
  fLn                  := 0;
  fMass                := 0;
  fn                   := 0;
  fNaturalFrequency    := 0;
  fnu                  := 0;
  fNumOfCycles         := 0;
  fnt                  := 0;
  fPitch               := 0;
  fPitchRatio          := 0;
  fR                   := 0;
  fRho                 := 0;
  fRm                  := 0;
  fS1                  := 0;
  fS2                  := 0;
  fSh                  := 0;
  fSa                  := 0;
  fSc                  := 0;
  fSk                  := 0;
  fSn                  := 0;
  fStaticSafetyFactor  := 0;
  fTau1                := 0;
  fTau2                := 0;
  fTauc                := 0;
  fTauh                := 0;
  fTauk1               := 0;
  fTauk2               := 0;
  fTaukc               := 0;
  fTaukh               := 0;
  fTaukn               := 0;
  fTauhz               := 0;
  fTaun                := 0;
  fTauoz               := 0;
  fTauz                := 0;
  fw                   := 0;
  fW0n                 := 0;
  fW12                 := 0;
  fWireLength          := 0;
end;

procedure TCompressionSpringSolver.PreCheck;
begin
  fCheck := True;
  // Controllo validità dati inseriti
  if fd   <= 0 then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if fDm  <= 0 then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE   <= 0 then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG   <= 0 then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if fL0  <= 0 then ErrorMessage.Add('Spring length "L0" unassigned.');
  if fL1  <= 0 then ErrorMessage.Add('Spring length "L1" unassigned.');
  if fL2  <= 0 then ErrorMessage.Add('Spring length "L2" unassigned.');
  if fn   <= 0 then ErrorMessage.Add('Number of active coil "n" unassigned.');
  if fnt  <= 0 then ErrorMessage.Add('Number of total coil "nt" unassigned.');
  if fnu  <= 0 then ErrorMessage.Add('Seat coefficent "nu" unassigned.');
  if fRm  <= 0 then ErrorMessage.Add('Tensile strength "Rm" unassigned.');
  if fRho <= 0 then ErrorMessage.Add('Material density "rho" unassigned.'); 

  if LengthL0 <= LengthL1 then ErrorMessage.Add('Wrong L0 and L1 values, L0 must be > L1.');
  if LengthL1 <= LengthL2 then ErrorMessage.Add('Wrong L1 and L2 values, L1 must be > L2.');

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
    fw := fDm / fd;
    if fColdCoiled then
    begin
      if fd > 20 then WarningMessage.Add('Wire diameter > 20mm.');
      if fn < 2  then WarningMessage.Add('Number of active coils < 2.');
      if fw < 4  then WarningMessage.Add('Spring Index < 4.');
      if fw > 20 then WarningMessage.Add('Spring Index > 20.');
    end else
    begin
      if fd < 8   then WarningMessage.Add('Wire diameter < 8mm.');
      if fd > 100 then WarningMessage.Add('Wire diameter > 100mm.');
      if fn < 3   then WarningMessage.Add('Number of active coils < 3.');
      if fw < 3   then WarningMessage.Add('Spring Index < 3.');
      if fw > 12  then WarningMessage.Add('Spring Index > 12.');
    end;

    if (fClosedEnds = True) then
    begin
      if (fColdCoiled = False) and (fGroundEnds = False) then
        ErrorMessage.Add('Length "Lc" undefined.');
    end else
    begin
      if (fColdCoiled <> False) and (fGroundEnds <> False) then
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
  m: double;
  Tauh7, Tauh6, Tauh5: double;
begin
  PreCheck;
  // Norma EN13906-1:2013
  // Calcolo molla a compressione ad elica cilindrica fabbricate con filo sezione circolare.

  if fCheck then
  begin
    fDe := fDm + fd;
    fDi := fDm - fd;
    // Spring index
    fw  := fDm / fd;
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
      if (fColdCoiled = True) and (fGroundEnds = True) then
        fLc := fnt * fdmax
      else
        if (fColdCoiled = True) and (fGroundEnds = False) then
          fLc := (fnt + 1.5) * fdmax
        else
          if (fColdCoiled = False) and (fGroundEnds = True) then
            fLc := (fnt - 0.3) * fdmax
          else
           fLc := 0;
    end else
    begin
      if (fColdCoiled = False) and (fGroundEnds = False) then
        fLc := (fnt + 1.1) * fdmax
      else
        fLc := 0;
    end;

    if fLc <= 0   then ErrorMessage.Add('Wrong Lc value, Lc must be > 0.');
    if fLc >  fL0 then ErrorMessage.Add('Wrong L0 or nt value, Lc must be < L0.');
    if fLc >  fL1 then ErrorMessage.Add('Wrong L1 or nt value, Lc must be < L1.');
    if fLc >  fL2 then ErrorMessage.Add('Wrong L2 or nt value, Lc must be < L2.');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo lunghezza minima ammissibile della molla
  if fCheck then
  begin
    // Per ottenere la lunghezza minima ammissibile della molla "Ln" basta sommare la lunghezza
    // a spire bloccate "Lc" e la somma delle distanze minime fra due successive spire attive "Sa":
    if fColdCoiled then
      fSa := fn * (0.0015 * (power(fDm, 2) / fd) + 0.1 * fd)
    else
      fSa := 0.02 * fn * (fDm + fd);
    // Nel caso di molle assoggettate a carico dinamico, il valore "Sa" viene raddoppiato rispetto al
    // caso di carico statico per molle avvolte a caldo YoungModulus moltiplicato per 1.5 per molle avvolte a freddo.
    if fDynamicLoad then
    begin
      if fColdCoiled then
        fSa := fSa * 1.5
      else
        fSa := fSa * 2.0;
    end;
    fLn := fLc + fSa;

    if fLn < fLc then ErrorMessage  .Add('Error: Wrong Ln value, Ln must be > Lc.');
    if fLn > fL0 then ErrorMessage  .Add('Error: Wrong Ln value, Ln must be < L0.');
    if fLn > fL1 then WarningMessage.Add('Warning: Wrong Ln value, Ln must be < L1.');
    if fLn > fL2 then WarningMessage.Add('Warning: Wrong Ln value, Ln must be < L2.');

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo delle frecce della molla
  if fCheck then
  begin
    fS1 := fL0 - fL1;
    fS2 := fL0 - fL2;
    fSh := fS2 - fS1;
    fSn := fL0 - fLn;
    fSc := fL0 - fLc;
  end;

  // Calcolo del passo della molla
  if fCheck then
  begin
    fPitch := fSc / fn + fd;
    fPitchRatio := fDm / fPitch;
  end;

  // Calcolo incremento del diamtro esterno della molla sotto carico
  if fCheck then
  begin
    // Quando la molla è sottoposta a compressione, il diametro della spira aumenta leggermente.
    // L'incremento del diamtro esterno della molla sotto carico, DeltaDe, è determinato usando la
    // formula sottostante, valida per molla a pacco LengthLc YoungModulus per estremità della molla
    // liberamente appoggiate.
    if (fClosedEnds) and (GroundEnds) then
      m := (fSc + fn * fd) / fn
    else
      if (not fClosedEnds) and (not GroundEnds) then
        m := (fSc + (fn + 1.5) * fd) / fn
      else
        m := 0;

    if m = 0 then
    begin
      WarningMessage.Add('DeltaDe undefined. Please change kind spring ends.');
    end else
    begin
      fDeltaDe := 0.1 * (power(m, 2) - 0.8 * m * fd - 0.2 * power(fd, 2)) / fDm;
    end;

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo carichi della molla
  if fCheck then
  begin
    // Calcolo rigidezza della molla:
    fR := (fG * power(fd, 4)) / (8 * power(fDm, 3) * fn);
    // Calcolo carichi della molla:
    fF1 := fR * fS1;
    fF2 := fR * fS2;
    fFn := fR * fSn;
    fFc := fR * fSc;
  end;

  // Calcolo lavoro molla W0n e W12
  if fCheck then
  begin
    fW0n := (fFn + 0)   / 2 * (fSn - 0);
    fW12 := (fF2 + fF1) / 2 * (fS2 - fS1);
  end;

  // Calcolo tolleranze Dm, Lo, F1, F1 e valori eccentricità e1 ed e2.
  if fCheck then
  begin
    TOL.WireDiameter   := fd;
    TOL.CoilDiameterDm := fDm;
    TOL.LoadF1         := fF1;
    TOL.LoadF2         := fF2;
    TOL.LengthL0       := fL0;
    TOL.ActiveCoils    := fn;
    TOL.SpringRateR    := fR;
    TOL.SpringIndexW   := fw;
    TOL.Solve;

    fCheck := ErrorMessage.Count = 0;
  end;

  // Calcolo diametro minimo e massimo della molla in esercizio:
  if fCheck then
  begin
    fDiMin := fDi - TOL.CoilDiameterTolerance - (fdmax - fd);
    fDeMax := fDe + TOL.CoilDiameterTolerance + (fdmax - fd) + fDeltaDe;
  end;

  // Calcolo lunghezza sviluppo del filo della molla
  if fCheck then
  begin
    fWireLength := fDm * pi * fnt;
  end;

  // Calcolo massa della molla
  if fCheck then
  begin
    fMass := pi * sqr(fd)/4 * (fRho/1000) * fWireLength;
  end;

  // Calcolo frequenza naturale del primo ordine della molla
  if fCheck then
  begin
    // La frequenza naturale del primo ordine della molla, avente entrambe le estremità vincolate ed
    // eccitare periodicamente ad una estremità durante il funzionamento, è determinata mediante la
    // seguente formula:
    fNaturalFrequency := (3560 * fd) / (fn * Power(fDm, 2)) * Sqrt(fG / fRho);
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
    m := (1 - (1 - fG / fE) / (0.5 + fG / fE) * sqr((pi * fDm) / (fnu * fL0)));

    if m < 0 then
    begin
      ErrorMessage.Add('Wrong G and E values, unable to calculate Sk.');
    end else
    begin
      fSk := fL0 * 0.5 / (1 - fG / fE) * (1 - sqrt(m));
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
    fTau1  := (fG * fd * fS1) / (Pi * fn * Power(fDm, 2));
    fTau2  := (fG * fd * fS2) / (Pi * fn * Power(fDm, 2));
    fTauh  := fTau2 - fTau1;
    fTaun  := (fG * fd * fSn) / (Pi * fn * Power(fDm, 2));
    fTauc  := (fG * fd * fSc) / (Pi * fn * Power(fDm, 2));
    fTauk1 := fTau1 * fk;
    fTauk2 := fTau2 * fk;
    fTaukh := fTauh * fk;
    fTaukn := fTaun * fk;
    fTaukc := fTauc * fk;
  end;

  // Calcolo tolleranze delle tensioni Tau1 e Tau2
  if fCheck then
  begin
    //fToleranceTau1  := 8 * fDm * (fF1 + fToleranceF1) / (Pi * Power(fd, 3)) - fTau1;
    //fToleranceTau2  := 8 * fDm * (fF2 + fToleranceF2) / (Pi * Power(fd, 3)) - fTau2;
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

    if (MAT.TorsionalStressTauStar  > 0) and
       (MAT.TorsionalStressTauYield > 0) and
       (MAT.TorsionalStressTauOE7   > 0) and
       (MAT.TorsionalStressTauUE7   > 0) then
    begin
      fTauoz := MAT.TorsionalStressTauYield;
      fTauhz := 0;

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
          m := (MAT.NumOfCyclesE7 - MAT.NumOfCyclesE6)/(Tauh6 - Tauh7);
          fNumOfCycles := MAT.NumOfCyclesE7 - m*(fTaukh - Tauh7);
        end else
        begin
          if fTaukh <= Tauh5 then
          begin
            m := (MAT.NumOfCyclesE6 - MAT.NumOfCyclesE5)/(Tauh5 - Tauh6);
            fNumOfCycles := MAT.NumOfCyclesE6 - m*(fTaukh - Tauh6);
          end;
        end;
      end;
      fTauhz := Tauh7;

      fDynamicSafetyFactor := Tauh7 / fTaukh;
    end;

  end;
end;

function TCompressionSpringSolver.GetR(const aTemperature: double): double;
begin
  if fCheck then
    Result := (MAT.GetG(aTemperature) * Power(fd, 4)) / (8 * Power(fDm, 3) * fn)
  else
    Result := 0;
end;

function TCompressionSpringSolver.GetF1(const aTemperature: double): double;
begin
  if fCheck then
    Result := GetR(aTemperature) * fS1
  else
    Result := 0;
end;

function TCompressionSpringSolver.GetF2(const aTemperature: double): double;
begin
  if fCheck then
    Result := GetR(aTemperature) * fS2
  else
    Result := 0;
end;

function TCompressionSpringSolver.GetTau(const aLoadF: double): double;
begin
  Result  := 8 * fDm * (aLoadF) / (Pi * Power(fd, 3));
end;

function TCompressionSpringSolver.GetTauk(const aLoadF: double): double;
begin
  Result := GetTau(aLoadF) * fk;
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
  if fd   <= 0 then ErrorMessage.Add('Wire diameter "d" unassigned.');
  if fDm  <= 0 then ErrorMessage.Add('Coil diameter "Dm" unassigned.');
  if fE   <= 0 then ErrorMessage.Add('Young''s modulus "E" unassigned.');
  if fG   <= 0 then ErrorMessage.Add('Shear modulus "G" unassigned.');
  if fn   <= 0 then ErrorMessage.Add('Number of active coil "n" unassigned.');

  if fColdCoiled then
    if (fd > 20) then ErrorMessage.Add('Wire diameter > 20.')
  else
    if fd > 10 then ErrorMessage.Add('Wire diameter > 10.');

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
    fRMR := Power(fd, 4)*fE/(3667*fDm*fn);

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
  {$IFDEF ONE}
  SOLVER := TCompressionSpringSolver.Create;
  {$ENDIF}
  {$IFDEF THREE}
  SOLVER := TTorsionSpringSolver.Create;
  {$ENDIF}
end;

finalization
begin
  SOLVER.Destroy;
end;

end.
