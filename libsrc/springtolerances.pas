{ Helical Compression Spring Production Tolerance

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

unit SpringTolerances;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, Math, SysUtils, BaseUtils, ADim;

type
  TQualityGrade   = (QualityGrade1, QualityGrade2, QualityGrade3);

  TEN15800 = class
  private
    fAD: TQuantity;
    fAF1: TQuantity;
    fAF2: TQuantity;
    fAL0: TQuantity;
    fFactorAlphaF: TQuantity;
    fWireDiameter:  TQuantity;
    fCoilDiameterDm: TQuantity;
    fLoadF1:  TQuantity;
    fLoadF2:  TQuantity;
    fE1: TQuantity;
    fE2: TQuantity;
    fFactorKF: double;
    fLengthL0: TQuantity;
    fActiveCoils: double;
    fSpringIndexW: double;
    fSpringRateR: TQuantity;
    fQualityGradeOnDm: TQualityGrade;
    fQualityGradeOnL0: TQualityGrade;
    fQualityGradeOnF1: TQualityGrade;
    fQualityGradeOnF2: TQualityGrade;
    fQualityGradeOnE1: TQualityGrade;
    fQualityGradeOnE2: TQualityGrade;

    procedure SetWireDiameter(const AValue: TQuantity);
    procedure SetCoilDiameter(const AValue: TQuantity);
    procedure SetFreeBodyLength(const AValue: TQuantity);
    procedure SetNumActiveCoils(const AValue: double);
    procedure SetSpringIndex(const AValue: double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
  public
    property WireDiameter: TQuantity read fWireDiameter write SetWireDiameter;
    property CoilDiameter: TQuantity read fCoilDiameterDm write SetCoilDiameter;
    property Load1: TQuantity read fLoadF1 write fLoadF1;
    property Load2: TQuantity read fLoadF2 write fLoadF2;
    property FreeBodyLength: TQuantity read fLengthL0 write SetFreeBodyLength;
    property NumActiveCoils: double read fActiveCoils write SetNumActiveCoils;
    property SpringIndex: double read fSpringIndexW write fSpringIndexW;
    property SpringRate: TQuantity read fSpringRateR write fSpringRateR;

    property QualityGradeOnCoilDiameter: TQualityGrade read fQualityGradeOnDm write fQualityGradeOnDm;
    property QualityGradeOnFreeBodyLength: TQualityGrade read fQualityGradeOnL0 write fQualityGradeOnL0;
    property QualityGradeOnLoad1: TQualityGrade read fQualityGradeOnF1 write fQualityGradeOnF1;
    property QualityGradeOnLoad2: TQualityGrade read fQualityGradeOnF2 write fQualityGradeOnF2;
    property QualityGradeOnPerpendicularity: TQualityGrade read fQualityGradeOnE1 write fQualityGradeOnE1;
    property QualityGradeOnParallelism: TQualityGrade read fQualityGradeOnE2 write fQualityGradeOnE2;

    property ToleranceOnCoilDiameter: TQuantity read fAD;
    property ToleranceOnLoad1: TQuantity read fAF1;
    property ToleranceOnLoad2: TQuantity read fAF2;
    property ToleranceFreeBodyLength: TQuantity read fAL0;
    property ToleranceOnPerpendicularity: TQuantity read fE1;
    property ToleranceOnParallelism: TQuantity read fE2;
  end;

  TDIN2194 = class
  private
    fMaterialID: string;
    fWireDiameter: TQuantity;
    fCoilDiameter: TQuantity;
    fActiveCoils: double;
    fSpringIndex: double;

    fLegLength1: TQuantity;
    fLegLength2: TQuantity;

    fBendRadius1: TQuantity;
    fBendRadius2: TQuantity;

    fQualityGradeOnDm: TQualityGrade;
    fQualityGradeOnTorqueT1: TQualityGrade;
    fQualityGradeOnTorqueT2: TQualityGrade;
    fQualityGradeOnRelativeEndAngle: TQualityGrade;
    fQualityGradeOnFreeBodyLength: TQualityGrade;
    fQualityGradeOnLegLengths: TQualityGrade;
    fQualityGradeOnBendRadii: TQualityGrade;
    fQualityGradeOnBendAngles: TQualityGrade;

    fToleranceOnCoilDiameter: TQuantity;
    fToleranceOnTorque1: TQuantity;
    fToleranceOnTorque2: TQuantity;
    fToleranceOnRelativeEndAngle: TQuantity;
    fToleranceOnFreeBodyLength: TQuantity;

    fToleranceOnLegLength1: TQuantity;
    fToleranceOnLegLength2: TQuantity;
    fToleranceOnBendRadius1: TQuantity;
    fToleranceOnBendRadius2: TQuantity;

    fToleranceOnBendAngle1: TQuantity;
    fToleranceOnBendAngle2: TQuantity;

    function LegLengthCoefficent(LegLength: TQuantity): double;
    function QualityFactor(AQualityGrade: TQualityGrade): double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Solve;
  public
    property MaterialID: string read fMaterialID write fMaterialID;
    property ActiveCoils: double read fActiveCoils write fActiveCoils;
    property WireDiameter: TQuantity read fWireDiameter write fWireDiameter;
    property MeanCoilDiameter: TQuantity read fCoilDiameter write fCoilDiameter;

    property LegLength1: TQuantity read fLegLength1 write fLegLength1;
    property LegLength2: TQuantity read fLegLength2 write fLegLength2;

    property SpringIndex: double read fSpringIndex;

    property QualityGradeOnCoilDiameter: TQualityGrade read fQualityGradeOnDm write fQualityGradeOnDm;
    property QualityGradeOnFreeBodyLength: TQualityGrade read fQualityGradeOnFreeBodyLength write fQualityGradeOnFreeBodyLength;
    property QualityGradeOnTorque1: TQualityGrade read fQualityGradeOnTorqueT1 write fQualityGradeOnTorqueT1;
    property QualityGradeOnTorque2: TQualityGrade read fQualityGradeOnTorqueT2 write fQualityGradeOnTorqueT2;
    property QualityGradeOnRelativeEndAngle: TQualityGrade read fQualityGradeOnRelativeEndAngle write fQualityGradeOnRelativeEndAngle;

    property ToleranceOnCoilDiameter: TQuantity read fToleranceOnCoilDiameter;
    property ToleranceOnFreeBodyLength: TQuantity read fToleranceOnFreeBodyLength;

    property ToleranceOnTorque1: TQuantity read fToleranceOnTorque1;
    property ToleranceOnTorque2: TQuantity read fToleranceOnTorque2;
  end;


  function WireTolerance(const aWireRegulation: TRegulation; const aWireDiameter: TQuantity): TQuantity;


implementation

uses
  LCLType;

const
  AD_TABLE : array[0..16] of record DmMin, DmMax, Q1W48, Q1W814, Q1W1420, Q2W48, Q2W814, Q2W1420, Q3W48, Q3W814, Q3W1420: double; end = (
    (DmMin: 0.63; DmMax: 1.00; Q1W48:0.05; Q1W814:0.07; Q1W1420:0.10; Q2W48:0.07; Q2W814:0.10; Q2W1420:0.15; Q3W48:0.10; Q3W814:0.15; Q3W1420:0.20),
    (DmMin: 1.00; DmMax: 1.60; Q1W48:0.05; Q1W814:0.07; Q1W1420:0.10; Q2W48:0.08; Q2W814:0.10; Q2W1420:0.15; Q3W48:0.15; Q3W814:0.20; Q3W1420:0.30),
    (DmMin: 1.60; DmMax: 2.50; Q1W48:0.07; Q1W814:0.10; Q1W1420:0.15; Q2W48:0.10; Q2W814:0.15; Q2W1420:0.20; Q3W48:0.20; Q3W814:0.30; Q3W1420:0.40),
    (DmMin: 2.50; DmMax: 4.00; Q1W48:0.10; Q1W814:0.10; Q1W1420:0.15; Q2W48:0.15; Q2W814:0.20; Q2W1420:0.25; Q3W48:0.30; Q3W814:0.40; Q3W1420:0.50),
    (DmMin: 4.00; DmMax: 6.30; Q1W48:0.10; Q1W814:0.15; Q1W1420:0.20; Q2W48:0.20; Q2W814:0.25; Q2W1420:0.30; Q3W48:0.40; Q3W814:0.50; Q3W1420:0.60),
    (DmMin: 6.30; DmMax: 10.0; Q1W48:0.15; Q1W814:0.15; Q1W1420:0.20; Q2W48:0.25; Q2W814:0.30; Q2W1420:0.35; Q3W48:0.50; Q3W814:0.60; Q3W1420:0.70),
    (DmMin: 10.0; DmMax: 16.0; Q1W48:0.15; Q1W814:0.20; Q1W1420:0.25; Q2W48:0.30; Q2W814:0.35; Q2W1420:0.40; Q3W48:0.60; Q3W814:0.70; Q3W1420:0.80),
    (DmMin: 16.0; DmMax: 25.0; Q1W48:0.20; Q1W814:0.25; Q1W1420:0.30; Q2W48:0.35; Q2W814:0.45; Q2W1420:0.50; Q3W48:0.70; Q3W814:0.90; Q3W1420:1.00),
    (DmMin: 25.0; DmMax: 31.5; Q1W48:0.25; Q1W814:0.30; Q1W1420:0.35; Q2W48:0.40; Q2W814:0.50; Q2W1420:0.60; Q3W48:0.80; Q3W814:1.00; Q3W1420:1.20),
    (DmMin: 31.5; DmMax: 40.0; Q1W48:0.25; Q1W814:0.30; Q1W1420:0.35; Q2W48:0.50; Q2W814:0.60; Q2W1420:0.70; Q3W48:1.00; Q3W814:1.20; Q3W1420:1.50),
    (DmMin: 40.0; DmMax: 50.0; Q1W48:0.30; Q1W814:0.40; Q1W1420:0.50; Q2W48:0.60; Q2W814:0.80; Q2W1420:0.90; Q3W48:1.20; Q3W814:1.50; Q3W1420:1.80),
    (DmMin: 50.0; DmMax: 63.0; Q1W48:0.40; Q1W814:0.50; Q1W1420:0.60; Q2W48:0.80; Q2W814:1.00; Q2W1420:1.10; Q3W48:1.50; Q3W814:2.00; Q3W1420:2.30),
    (DmMin: 63.0; DmMax: 80.0; Q1W48:0.50; Q1W814:0.70; Q1W1420:0.80; Q2W48:1.00; Q2W814:1.20; Q2W1420:1.40; Q3W48:1.80; Q3W814:2.40; Q3W1420:2.80),
    (DmMin: 80.0; DmMax:100.0; Q1W48:0.60; Q1W814:0.80; Q1W1420:0.90; Q2W48:1.20; Q2W814:1.50; Q2W1420:1.70; Q3W48:2.30; Q3W814:3.00; Q3W1420:3.50),
    (DmMin:100.0; DmMax:125.0; Q1W48:0.70; Q1W814:1.00; Q1W1420:1.10; Q2W48:1.40; Q2W814:1.90; Q2W1420:2.20; Q3W48:2.80; Q3W814:3.70; Q3W1420:4.40),
    (DmMin:125.0; DmMax:160.0; Q1W48:0.90; Q1W814:1.20; Q1W1420:1.40; Q2W48:1.80; Q2W814:2.30; Q2W1420:2.70; Q3W48:3.50; Q3W814:4.60; Q3W1420:5.40),
    (DmMin:160.0; DmMax:200.0; Q1W48:1.20; Q1W814:1.50; Q1W1420:1.70; Q2W48:2.10; Q2W814:2.90; Q2W1420:3.30; Q3W48:4.20; Q3W814:5.70; Q3W1420:6.60));

const
  MinWireDiameter : TQuantity = ({$IFNDEF ADIMOFF} FID: MeterId; FValue: 0.07/1000 {$ELSE} 0.07/1000 {$ENDIF});
  MaxWireDiameter : TQuantity = ({$IFNDEF ADIMOFF} FID: MeterId; FValue:   16/1000 {$ELSE}   16/1000 {$ENDIF});
  MinCoilDiameter : TQuantity = ({$IFNDEF ADIMOFF} FID: MeterId; FValue: 0.63/1000 {$ELSE} 0.63/1000 {$ENDIF});
  MaxCoilDiameter : TQuantity = ({$IFNDEF ADIMOFF} FID: MeterId; FValue:  200/1000 {$ELSE}  200/1000 {$ENDIF});
  MaxFreeLength   : TQuantity = ({$IFNDEF ADIMOFF} FID: MeterId; FValue:  630/1000 {$ELSE}  630/1000 {$ENDIF});
  MinActiveCoils  =  2;
  MinSpringIndex  =  4;
  MaxSpringIndex  = 20;

const
  WIRETOL_TABLE : array[0..29] of record WireReg: TRegulation; DMin, DMax, Tolerance: double; end = (
    (WireReg:EN10270P1; DMin: 0.0499; DMax: 0.0900; Tolerance:0.0030),
    (WireReg:EN10270P1; DMin: 0.0900; DMax: 0.1600; Tolerance:0.0040),
    (WireReg:EN10270P1; DMin: 0.1600; DMax: 0.2500; Tolerance:0.0050),
    (WireReg:EN10270P1; DMin: 0.2500; DMax: 0.6300; Tolerance:0.0080),
    (WireReg:EN10270P1; DMin: 0.6300; DMax: 0.7500; Tolerance:0.0100),
    (WireReg:EN10270P1; DMin: 0.7500; DMax: 1.0000; Tolerance:0.0150),
    (WireReg:EN10270P1; DMin: 1.0000; DMax: 1.2000; Tolerance:0.0200),
    (WireReg:EN10270P1; DMin: 1.2000; DMax: 1.7000; Tolerance:0.0200),
    (WireReg:EN10270P1; DMin: 1.7000; DMax: 2.6000; Tolerance:0.0250),
    (WireReg:EN10270P1; DMin: 2.6000; DMax: 4.0000; Tolerance:0.0300),
    (WireReg:EN10270P1; DMin: 4.0000; DMax: 5.3000; Tolerance:0.0350),
    (WireReg:EN10270P1; DMin: 5.3000; DMax: 7.0000; Tolerance:0.0400),
    (WireReg:EN10270P1; DMin: 7.0000; DMax: 9.0000; Tolerance:0.0450),
    (WireReg:EN10270P1; DMin: 9.0000; DMax:10.0000; Tolerance:0.0500),
    (WireReg:EN10270P1; DMin:10.0000; DMax:11.0000; Tolerance:0.0700),
    (WireReg:EN10270P1; DMin:11.0000; DMax:14.0000; Tolerance:0.0800),
    (WireReg:EN10270P1; DMin:14.0000; DMax:18.0000; Tolerance:0.0900),
    (WireReg:EN10270P1; DMin:18.0000; DMax:20.0000; Tolerance:0.1000),
    (WireReg:EN10270P2; DMin: 0.4990; DMax: 0.8500; Tolerance:0.0100),
    (WireReg:EN10270P2; DMin: 0.8500; DMax: 1.0500; Tolerance:0.0150),
    (WireReg:EN10270P2; DMin: 1.0500; DMax: 1.7000; Tolerance:0.0200),
    (WireReg:EN10270P2; DMin: 1.7000; DMax: 3.0000; Tolerance:0.0250),
    (WireReg:EN10270P2; DMin: 3.0000; DMax: 4.2000; Tolerance:0.0300),
    (WireReg:EN10270P2; DMin: 4.2000; DMax: 6.0000; Tolerance:0.0350),
    (WireReg:EN10270P2; DMin: 6.0000; DMax: 7.5000; Tolerance:0.0400),
    (WireReg:EN10270P2; DMin: 7.5000; DMax: 9.0000; Tolerance:0.0450),
    (WireReg:EN10270P2; DMin: 9.0000; DMax:11.0000; Tolerance:0.0500),
    (WireReg:EN10270P2; DMin:11.0000; DMax:13.0000; Tolerance:0.0700),
    (WireReg:EN10270P2; DMin:13.0000; DMax:16.0000; Tolerance:0.0800),
    (WireReg:EN10270P2; DMin:16.0000; DMax:17.0000; Tolerance:0.0900));

// TEN15800

constructor TEN15800.Create;
begin
  inherited Create;
  Clear;
end;

destructor TEN15800.Destroy;
begin
  inherited Destroy;
end;

procedure TEN15800.Clear;
begin
  fAD               := 0*m;
  fAF1              := 0*N;
  fAF2              := 0*N;
  fAL0              := 0*m;
  fFactorAlphaF     := 0*N;
  fWireDiameter     := 0*m;
  fCoilDiameterDm   := 0*m;
  fLoadF1           := 0*N;
  fLoadF2           := 0*N;
  fE1               := 0*m;
  fE2               := 0*m;
  fFactorKF         := 0;
  fLengthL0         := 0*m;
  fActiveCoils      := 0;
  fSpringIndexW     := 0;
  fSpringRateR      := 0*N/m;
  fQualityGradeOnDm := QualityGrade2;
  fQualityGradeOnL0 := QualityGrade2;
  fQualityGradeOnF1 := QualityGrade2;
  fQualityGradeOnF2 := QualityGrade2;
  fQualityGradeOnE1 := QualityGrade2;
  fQualityGradeOnE2 := QualityGrade2;
end;

procedure TEN15800.SetWireDiameter(const AValue: TQuantity);
begin
  if AValue < MinWireDiameter then WarningMessage.Add(Format('EN15800: Wire diameter < %s.', [MeterUnit.ToString(MinWireDiameter, 5, 5, [pMilli])]));
  if AValue > MaxWireDiameter then WarningMessage.Add(Format('EN15800: Wire diameter > %s.', [Meterunit.ToString(MaxWireDiameter, 5, 5, [pMilli])]));

  fWireDiameter := Max(Min(AValue, MaxWireDiameter), MinWireDiameter);
end;

procedure TEN15800.SetCoilDiameter(const AValue: TQuantity);
begin
  if AValue < MinCoilDiameter then WarningMessage.Add(Format('EN15800: Mean coil diameter < %s.', [MeterUnit.ToString(MinCoilDiameter, 5, 5, [pMilli])]));
  if AValue > MaxCoilDiameter then WarningMessage.Add(Format('EN15800: Mean coil diameter > %s.', [MeterUnit.ToString(MaxCoilDiameter, 5, 5, [pMilli])]));

  fCoilDiameterDm := Max(Min(AValue, MaxCoilDiameter), MinCoilDiameter);
end;

procedure TEN15800.SetFreeBodyLength(const AValue: TQuantity);
begin
  if AValue > MaxFreeLength then WarningMessage.Add(Format('EN15800: Length of unloaded spring > %s.', [MeterUnit.ToString(MaxFreeLength, 5, 5, [pMilli])]));

  fLengthL0 := Min(AValue, MaxFreeLength);
end;

procedure TEN15800.SetNumActiveCoils(const AValue: double);
begin
  if AValue < MinActiveCoils then WarningMessage.Add(Format('EN15800: Number of active coils < %d.', [MinActiveCoils]));

  fActiveCoils := Math.Max(AValue, MinActiveCoils);
end;

procedure TEN15800.SetSpringIndex(const AValue: double);
begin
  if AValue < MinSpringIndex then WarningMessage.Add(Format('EN15800: Spring Index < %d.', [MinSpringIndex]));
  if AValue > MaxSpringIndex then WarningMessage.Add(Format('EN15800: Spring Index > %d.', [MaxSpringIndex]));

  fSpringIndexW := Math.Max(Math.Min(AValue, MaxSpringIndex), MinSpringIndex);
end;

procedure TEN15800.Solve;
var
  Check:  boolean;
  I, Index:  longint;
begin
  Check := True;
  // Scopo e campo di applicazione
  if fWireDiameter   <= 0*m then WarningMessage.Add(Format('EN15800: Wire diameter unassigned.', []));
  if fCoilDiameterDm <= 0*m then WarningMessage.Add(Format('EN15800: Mean coil diameter unassigned.', []));
  if fLengthL0       <= 0*m then WarningMessage.Add(Format('EN15800: Length of unloaded spring unassigned.', []));
  if fActiveCoils    <= 0   then WarningMessage.Add(Format('EN15800: Number of active coils unassigned.', []));
  if fSpringIndexW   <= 0   then WarningMessage.Add(Format('EN15800: Spring Index unassigned.', []));


  Check := ErrorMessage.Count = 0;
  if Check then
  begin
    // Tolleranza AD sul diametro medio di avvolgimento Dm di una molla libera
    Index := -1;

    for I := 0 to 16 do
    begin
      if (fCoilDiameterDm >= (AD_TABLE[I].DmMin*mm)) and
         (fCoilDiameterDm <= (AD_TABLE[I].DmMax*mm)) then
      begin
        Index := I;
        Break;
      end;
    end;

    if (fSpringIndexW >=  4.0) and (fSpringIndexW <=  8.0) then
    begin
      case fQualityGradeOnDm of
        QualityGrade1: fAD := AD_TABLE[Index].Q1W48*mm;
        QualityGrade2: fAD := AD_TABLE[Index].Q2W48*mm;
        QualityGrade3: fAD := AD_TABLE[Index].Q3W48*mm;
      end;
    end else
    if (fSpringIndexW >   8.0) and (fSpringIndexW <= 14.0) then
    begin
      case fQualityGradeOnDm of
        QualityGrade1: fAD := AD_TABLE[Index].Q1W814*mm;
        QualityGrade2: fAD := AD_TABLE[Index].Q2W814*mm;
        QualityGrade3: fAD := AD_TABLE[Index].Q3W814*mm;
      end;
    end else
    if (fSpringIndexW > 14.0) and (fSpringIndexW <= 20.0) then
    begin
      case fQualityGradeOnDm of
        QualityGrade1: fAD := AD_TABLE[Index].Q1W1420*mm;
        QualityGrade2: fAD := AD_TABLE[Index].Q2W1420*mm;
        QualityGrade3: fAD := AD_TABLE[Index].Q3W1420*mm;
      end;
    end;
    // alphaF
    fFactorAlphaF := 65.92*Power(1000*MeterUnit.ToFloat(FWireDiameter), 3.3)/Power(1000*MeterUnit.ToFloat(FCoilDiameterDm), 1.6)*
      (-0.84*IntPower(fSpringIndexW/10, 3) + 3.781*IntPower(fSpringIndexW/10, 2) - 4.244*(fSpringIndexW/10) + 2.274)*N;
    // kF
    fFactorKF := 1/(3*Sqr(fActiveCoils))+8/(5*fActiveCoils)+0.803;
    // Tolleranza AF1 sul carico della molla Load1 ad una lunghezza data della molla L1
    case fQualityGradeOnF1 of
      QualityGrade1: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1/100))*0.63;
      QualityGrade2: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1/100))*1.00;
      QualityGrade3: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1/100))*1.60;
    end;
    // Tolleranza AF2 sul carico della molla Load2 ad una lunghezza data della molla L2
    case fQualityGradeOnF2 of
      QualityGrade1: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2/100))*0.63;
      QualityGrade2: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2/100))*1.00;
      QualityGrade3: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2/100))*1.60;
    end;
    // Tolleranza AL0 sulla lunghezza FreeBodyLength di una molla libera
    case fQualityGradeOnL0 of
      QualityGrade1: fAL0 := (fFactorAlphaF*fFactorKF*0.63)/fSpringRateR;
      QualityGrade2: fAL0 := (fFactorAlphaF*fFactorKF*1.00)/fSpringRateR;
      QualityGrade3: fAL0 := (fFactorAlphaF*fFactorKF*1.60)/fSpringRateR;
    end;
    //  ToleranceOnPerpendicularity & ToleranceOnParallelism
    if (fQualityGradeOnE1 = QualityGrade1) or
       (fQualityGradeOnE2 = QualityGrade1) then
    begin
      if (fSpringIndexW            ) > 12 then WarningMessage.Add('EN15800: If "e" Quality Grade = 1 then "w" must be <= 12');
      if (fLengthL0/fCoilDiameterDm) >  5 then WarningMessage.Add('EN15800: If "e" Quality Grade = 1 then "L0/Dm" must be <= 5');
    end;
    case fQualityGradeOnE1 of
      QualityGrade1: fe1 := 0.030*fLengthL0;
      QualityGrade2: fe1 := 0.050*fLengthL0;
      QualityGrade3: fe1 := 0.080*fLengthL0;
    end;
    case fQualityGradeOnE2 of
      QualityGrade1: fe2 := 0.015*(fCoilDiameterDm + fWireDiameter);
      QualityGrade2: fe2 := 0.030*(fCoilDiameterDm + fWireDiameter);
      QualityGrade3: fe2 := 0.060*(fCoilDiameterDm + fWireDiameter);
    end;
  end;
end;

// TWireTolerance

function WireTolerance(const aWireRegulation: TRegulation; const aWireDiameter: TQuantity): TQuantity;
var
  I: longint;
begin
  Result := 0*m;
  for I := Low(WIRETOL_TABLE) to High(WIRETOL_TABLE) do
  begin
    if (aWireRegulation >= (WIRETOL_TABLE[i].WireReg)) and
       (aWireDiameter   >  (WIRETOL_TABLE[I].DMin*mm)) and
       (aWireDiameter   <= (WIRETOL_TABLE[I].DMax*mm)) then
    begin
      Result := WIRETOL_TABLE[I].Tolerance*mm;
      Break;
    end;
  end;
end;

// TDIN2194

constructor TDIN2194.Create;
begin
  inherited Create;
  Clear;
end;

destructor TDIN2194.Destroy;
begin
  inherited Destroy;
end;

procedure TDIN2194.Clear;
begin
  fWireDiameter := 0*m;
  fCoilDiameter := 0*m;
  fActiveCoils  := 0;
  fSpringIndex  := 0;

  fLegLength1   := 0*m;
  fLegLength2   := 0*m;

  fBendRadius1  := 0*m;
  fBendRadius2  := 0*m;

  fQualityGradeOnDm               := QualityGrade2;
  fQualityGradeOnTorqueT1         := QualityGrade2;
  fQualityGradeOnTorqueT2         := QualityGrade2;
  fQualityGradeOnRelativeEndAngle := QualityGrade2;
  fQualityGradeOnFreeBodyLength   := QualityGrade2;
  fQualityGradeOnLegLengths       := QualityGrade2;

  fToleranceOnCoilDiameter     := 0*m;
  fToleranceOnTorque1          := 0*N*m;
  fToleranceOnTorque2          := 0*N*m;
  fToleranceOnRelativeEndAngle := 0*deg;
  fToleranceOnFreeBodyLength   := 0*m;

  fToleranceOnLegLength1  := 0*m;
  fToleranceOnLegLength2  := 0*m;
  fToleranceOnBendRadius1 := 0*m;
  fToleranceOnBendRadius2 := 0*m;

  fToleranceOnBendAngle1  := 0*deg;
  fToleranceOnBendAngle2  := 0*deg;
end;

procedure TDIN2194.Solve;
var
  d, Dm, Kf, Ks: double;
  Check: boolean;
begin
  if fWireDiameter <  (0.07*mm) then ErrorMessage.Add('Wire diameter d < 0.07mm.');
  if fWireDiameter >  (17  *mm) then ErrorMessage.Add('Wire diameter d > 17 mm.');
  if fCoilDiameter <= (0   *mm) then ErrorMessage.Add('Mean coil diameter unassigned.');
  if fCoilDiameter >  (340 *mm) then ErrorMessage.Add('Mean coil diameter Dm > 340 mm.');
  if fActiveCoils  <  (2      ) then ErrorMessage.Add('Number of active coils n < 2.');

  // leg lengths
  if (fLegLength1 <  (0.5*mm)) then ErrorMessage.Add('Leg length1 is < 0.5 mm.');
  if (fLegLength2 <  (0.5*mm)) then ErrorMessage.Add('Leg length2 is < 0.5 mm.');

  if (fLegLength1 > (1000*mm)) then ErrorMessage.Add('Leg length1 is > 1000 mm.');
  if (fLegLength2 > (1000*mm)) then ErrorMessage.Add('Leg length2 is > 1000 mm.');

  Check := ErrorMessage.Count = 0;
  if Check then
  begin
    fSpringIndex := ScalarUnit.ToFloat(fCoilDiameter/fWireDiameter);
    if SpringIndex < 4  then ErrorMessage.Add('Spring index w < 4.');
    if SpringIndex > 20 then ErrorMessage.Add('Spring index w > 20.');
  end;

  Check := ErrorMessage.Count = 0;
  if Check then
  begin
    d  := MeterUnit.ToFloat(fWireDiameter, [pMilli]);
    Dm := MeterUnit.ToFloat(fCoilDiameter, [pMilli]);

    // tolerance on coil diameter (unloaded spring)
    fToleranceOnCoilDiameter := (0.025 * Dm/Power(d, 0.17) * QualityFactor(fQualityGradeOnDm))*mm;

    // tolerance on spring torque
    Ks := 90;
    if Pos('EN10270-1', fMaterialID) > 0 then Ks := 104;
    if Pos('EN10270-2', fMaterialID) > 0 then Ks := 104;
    if Pos('EN10270-3', fMaterialID) > 0 then Ks := 90;
    if Pos('EN12166',   fMaterialID) > 0 then Ks := 54;

    fToleranceOnTorque1 := (1.3*Ks* Power(d, 3)/(Power(fActiveCoils, 0.24)*Sqrt(fSpringIndex)))*QualityFactor(fQualityGradeOnTorqueT1)*J;
    fToleranceOnTorque2 := (1.3*Ks* Power(d, 3)/(Power(fActiveCoils, 0.24)*Sqrt(fSpringIndex)))*QualityFactor(fQualityGradeOnTorqueT2)*J;

    // tolerance on relative end angle (unloaded spring)
    fToleranceOnRelativeEndAngle := 2.4*Power(fActiveCoils, 0.76)*Sqrt(fSpringIndex)*QualityFactor(fQualityGradeOnRelativeEndAngle)*deg;

    // tolerance on body length
    Kf := 0.803 + 8/(5*fActiveCoils) - 1/(3*Sqr(fActiveCoils));

    fToleranceOnFreeBodyLength := 0.06*Power(d, 0.83)*(1 + 0.001*Power(fSpringIndex, 2.5))*fActiveCoils*kf*QualityFactor(fQualityGradeOnFreeBodyLength)*mm;

    // tolerance on leg lengths (unloaded spring)

    fToleranceOnLegLength1 := (0.2*d + QualityFactor(fQualityGradeOnLegLengths))*LegLengthCoefficent(fLegLength1)*mm;
    fToleranceOnLegLength2 := (0.2*d + QualityFactor(fQualityGradeOnLegLengths))*LegLengthCoefficent(fLegLength2)*mm;

    // tolerance on bend radii (unloaded spring)

    fToleranceOnBendRadius1 := (0.6 + 0.2*MeterUnit.ToFloat(fBendRadius1, [pMilli]))*QualityFactor(fQualityGradeOnBendRadii)*mm;
    fToleranceOnBendRadius2 := (0.6 + 0.2*MeterUnit.ToFloat(fBendRadius2, [pMilli]))*QualityFactor(fQualityGradeOnBendRadii)*mm;

    // tolerance on angles of bends on leg

    fToleranceOnBendAngle1 := 4*sqrt(MeterUnit.ToFloat(fBendRadius1, [pMilli])/d)*QualityFactor(fQualityGradeOnBendAngles)*deg;
    fToleranceOnBendAngle2 := 4*sqrt(MeterUnit.ToFloat(fBendRadius2, [pMilli])/d)*QualityFactor(fQualityGradeOnBendAngles)*deg;
  end;

  if fToleranceOnFreeBodyLength > (630*mm) then ErrorMessage.Add('Free body length Lk > 630 mm.');
end;

function TDIN2194.LegLengthCoefficent(LegLength: TQuantity): double;
var
  LegLen: double;
begin
  LegLen := MeterUnit.ToFloat(LegLength, [pMilli]);
  if (LegLen >= 0.5) and (LegLen <=    6) then result := 0.3 else
  if (LegLen >    6) and (LegLen <=   30) then result := 0.8 else
  if (LegLen >   30) and (LegLen <=  120) then result := 1.3 else
  if (LegLen >  120) and (LegLen <=  400) then result := 1.9 else
  if (LegLen >  400) and (LegLen <= 1000) then result := 3.2 else result := 0;
end;

function TDIN2194.QualityFactor(AQualityGrade: TQualityGrade): double;
begin
  case AQualityGrade of
    QualityGrade1: Result := 0.63;
    QualityGrade2: Result := 1.00;
    QualityGrade3: Result := 1.60;
  end;
end;

end.

