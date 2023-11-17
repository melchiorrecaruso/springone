{ Helical Compression Spring Production Tolerance

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

unit springtolerances;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, CsvDocument, Math, SysUtils, UtilsBase, ADim;

type
  TQualityGrade = (QualityGrade1 = 0, QualityGrade2, QualityGrade3);

type
  TWireTolerance = class
  private
    fTolFile: TCsvDocument;
    fValue: TMeters;
  public
    constructor Create;
    destructor Destroy; override;
    function Search(const aID: string; const aWireDiameter: TMeters): boolean;
    procedure Clear;
  public
    property Value: TMeters read fValue;
  end;

  TEN15800 = class
  private
    fAD: TMeters;
    fAF1: TNewtons;
    fAF2: TNewtons;
    fAL0: TMeters;
    fFactorAlphaF: TNewtons;
    fWireDiameter:  TMeters;
    fCoilDiameterDm: TMeters;
    fLoadF1:  TNewtons;
    fLoadF2:  TNewtons;
    fE1: TMeters;
    fE2: TMeters;
    fFactorKF: double;
    fLengthL0: TMeters;
    fActiveCoils: double;
    fSpringIndexW: double;
    fSpringRateR: TNewtonsPerMeter;
    fQualityGradeOnDm: TQualityGrade;
    fQualityGradeOnL0: TQualityGrade;
    fQualityGradeOnF1: TQualityGrade;
    fQualityGradeOnF2: TQualityGrade;
    fQualityGradeOnE1: TQualityGrade;
    fQualityGradeOnE2: TQualityGrade;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
  public
    property WireDiameter: TMeters read fWireDiameter write fWireDiameter;
    property CoilDiameter: TMeters read fCoilDiameterDm write fCoilDiameterDm;
    property Load1: TNewtons read fLoadF1 write fLoadF1;
    property Load2: TNewtons read fLoadF2 write fLoadF2;
    property FreeBodyLength: TMeters read fLengthL0 write fLengthL0;
    property ActiveCoils: double read fActiveCoils write fActiveCoils;
    property SpringIndex: double read fSpringIndexW write fSpringIndexW;
    property SpringRate: TNewtonsPerMeter read fSpringRateR write fSpringRateR;

    property QualityGradeOnCoilDiameter: TQualityGrade read fQualityGradeOnDm write fQualityGradeOnDm;
    property QualityGradeOnFreeBodyLength: TQualityGrade read fQualityGradeOnL0 write fQualityGradeOnL0;
    property QualityGradeOnLoad1: TQualityGrade read fQualityGradeOnF1 write fQualityGradeOnF1;
    property QualityGradeOnLoad2: TQualityGrade read fQualityGradeOnF2 write fQualityGradeOnF2;
    property QualityGradeOnPerpendicularity: TQualityGrade read fQualityGradeOnE1 write fQualityGradeOnE1;
    property QualityGradeOnParallelism: TQualityGrade read fQualityGradeOnE2 write fQualityGradeOnE2;

    property ToleranceOnCoilDiameter: TMeters read fAD;
    property ToleranceOnLoad1: TNewtons read fAF1;
    property ToleranceOnLoad2: TNewtons read fAF2;
    property ToleranceFreeBodyLength: TMeters read fAL0;
    property ToleranceOnPerpendicularity: TMeters read fE1;
    property ToleranceOnParallelism: TMeters read fE2;
  end;

  TDIN2194 = class
  private
    fMaterialID: string;
    fWireDiameter: TMeters;
    fCoilDiameter: TMeters;
    fFreeBodyLengthLk: TMeters;
    fActiveCoils: double;
    fSpringIndex: double;

    fLegLength1: TMeters;
    fLegLength2: TMeters;
    fLegLength3: TMeters;

    fBendRadius1: TMeters;
    fBendRadius2: TMeters;
    fBendRadius3: TMeters;


    fQualityGradeOnDm: TQualityGrade;
    fQualityGradeOnTorqueT1: TQualityGrade;
    fQualityGradeOnTorqueT2: TQualityGrade;
    fQualityGradeOnRelativeEndAngle: TQualityGrade;
    fQualityGradeOnLk0: TQualityGrade;
    fQualityGradeOnLegLengths: TQualityGrade;
    fQualityGradeOnBendRadii: TQualityGrade;
    fQualityGradeOnBendAngles: TQualityGrade;


    fToleranceOnCoilDiameter: TMeters;
    fToleranceOnTorque1: TJoules;
    fToleranceOnTorque2: TJoules;
    fToleranceOnRelativeEndAngle: TRadians;
    fToleranceOnFreeBodyLength: TMeters;

    fToleranceOnLegLength1: TMeters;
    fToleranceOnLegLength2: TMeters;
    fToleranceOnLegLength3: TMeters;
    fToleranceOnBendRadius1: TMeters;
    fToleranceOnBendRadius2: TMeters;
    fToleranceOnBendRadius3: TMeters;

    fToleranceOnBendAngle1: TRadians;
    fToleranceOnBendAngle2: TRadians;
    fToleranceOnBendAngle3: TRadians;

    function LegLengthCoefficent(LegLength: TMeters): double;
    function QualityFactor(AQualityGrade: TQualityGrade): double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Solve;
  public
    property MaterialID: string read fMaterialID write fMaterialID;
    property ActiveCoils: double read fActiveCoils write fActiveCoils;
    property WireDiameter: TMeters read fWireDiameter write fWireDiameter;
    property CoilDiameter: TMeters read fCoilDiameter write fCoilDiameter;
    property FreeBodyLengthLk: TMeters read fFreeBodyLengthLk;

    property SpringIndex: double read fSpringIndex;

    property QualityGradeOnCoilDiameter: TQualityGrade read fQualityGradeOnDm write fQualityGradeOnDm;
    property QualityGradeOnTorque1: TQualityGrade read fQualityGradeOnTorqueT1 write fQualityGradeOnTorqueT1;
    property QualityGradeOnTorque2: TQualityGrade read fQualityGradeOnTorqueT2 write fQualityGradeOnTorqueT2;
    property QualityGradeOnRelativeEndAngle: TQualityGrade read fQualityGradeOnRelativeEndAngle write fQualityGradeOnRelativeEndAngle;

    property ToleranceOnCoilDiameter: TMeters read fToleranceOnCoilDiameter;
    property ToleranceFreeBodyLength: TMeters read fToleranceOnFreeBodyLength;

    property ToleranceOnTorque1: TJoules read fToleranceOnTorque1;
    property ToleranceOnTorque2: TJoules read fToleranceOnTorque2;
  end;


implementation

uses
  LCLType;

const
  AD_TABLE : array[0..16, 0..10] of single = (
     ( 0.63,  1.00, 0.05, 0.07, 0.10, 0.07, 0.10, 0.15, 0.10, 0.15, 0.20),
     ( 1.00,  1.60, 0.05, 0.07, 0.10, 0.08, 0.10, 0.15, 0.15, 0.20, 0.30),
     ( 1.60,  2.50, 0.07, 0.10, 0.15, 0.10, 0.15, 0.20, 0.20, 0.30, 0.40),
     ( 2.50,  4.00, 0.10, 0.10, 0.15, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50),
     ( 4.00,  6.30, 0.10, 0.15, 0.20, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60),
     ( 6.30,  10.0, 0.15, 0.15, 0.20, 0.25, 0.30, 0.35, 0.50, 0.60, 0.70),
     ( 10.0,  16.0, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.60, 0.70, 0.80),
     ( 16.0,  25.0, 0.20, 0.25, 0.30, 0.35, 0.45, 0.50, 0.70, 0.90, 1.00),
     ( 25.0,  31.5, 0.25, 0.30, 0.35, 0.40, 0.50, 0.60, 0.80, 1.00, 1.20),
     ( 31.5,  40.0, 0.25, 0.30, 0.35, 0.50, 0.60, 0.70, 1.00, 1.20, 1.50),
     ( 40.0,  50.0, 0.30, 0.40, 0.50, 0.60, 0.80, 0.90, 1.20, 1.50, 1.80),
     ( 50.0,  63.0, 0.40, 0.50, 0.60, 0.80, 1.00, 1.10, 1.50, 2.00, 2.30),
     ( 63.0,  80.0, 0.50, 0.70, 0.80, 1.00, 1.20, 1.40, 1.80, 2.40, 2.80),
     ( 80.0, 100.0, 0.60, 0.80, 0.90, 1.20, 1.50, 1.70, 2.30, 3.00, 3.50),
     (100.0, 125.0, 0.70, 1.00, 1.10, 1.40, 1.90, 2.20, 2.80, 3.70, 4.40),
     (125.0, 160.0, 0.90, 1.20, 1.40, 1.80, 2.30, 2.70, 3.50, 4.60, 5.40),
     (160.0, 200.0, 1.20, 1.50, 1.70, 2.10, 2.90, 3.30, 4.20, 5.70, 6.60));

// TEN15800

constructor TEN15800.Create;
begin
  inherited Create;
end;

destructor TEN15800.Destroy;
begin
  inherited Destroy;
end;

procedure TEN15800.Clear;
begin
  fAD.SetZero;
  fAF1.SetZero;
  fAF2.SetZero;
  fAL0.SetZero;
  fFactorAlphaF.SetZero;
  fWireDiameter.SetZero;
  fCoilDiameterDm.SetZero;
  fLoadF1.SetZero;
  fLoadF2.SetZero;
  fE1.SetZero;
  fE2.SetZero;
  fFactorKF := 0;
  fLengthL0.SetZero;
  fActiveCoils := 0;
  fSpringRateR.SetZero;
  fSpringIndexW := 0;

  fQualityGradeOnDm := QualityGrade2;
  fQualityGradeOnL0 := QualityGrade2;
  fQualityGradeOnF1 := QualityGrade2;
  fQualityGradeOnF2 := QualityGrade2;
  fQualityGradeOnE1 := QualityGrade2;
  fQualityGradeOnE2 := QualityGrade2;
end;

procedure TEN15800.Solve;
var
  Check:  boolean;
  Index:  longint;
  Index1: longint;
  Index2: longint;
begin
  Check := True;
  // Scopo e campo di applicazione
  if fWireDiameter   < (0.07*mm) then ErrorMessage.Add('Wire diameter < 0.07 mm.');
  if fWireDiameter   >   (16*mm) then ErrorMessage.Add('Wire diameter > 16 mm.');
  if fCoilDiameterDm < (0.63*mm) then ErrorMessage.Add('Mean coil diameter < 0.63 mm.');
  if fCoilDiameterDm >  (200*mm) then ErrorMessage.Add('Mean coil diameter > 200 mm.');
  if fLengthL0       >  (630*mm) then ErrorMessage.Add('Length of unloaded spring > 630 mm.');
  if fActiveCoils    <       (2) then ErrorMessage.Add('Number of active coils < 2.');
  if fSpringIndexW   <       (4) then ErrorMessage.Add('Spring Index < 4.');
  if fSpringIndexW   >      (20) then ErrorMessage.Add('Spring Index > 20.');

  Check := ErrorMessage.Count = 0;

  if Check then
  begin
    // Tolleranza AD sul diametro medio di avvolgimento CoilDiameter di una molla libera
    Index1 := 16;
    if (fCoilDiameterDm > (0.63*mm)) and (fCoilDiameterDm <= (200*mm)) then
    begin
      for Index := 0 to 16 do
        if (fCoilDiameterDm > (AD_TABLE[Index][0]*mm)) and (fCoilDiameterDm <= (AD_TABLE[Index][1]*mm)) then
        begin
          Index1 := Index;
          Break;
        end;
    end;

    Index2 := 10;
    if (fSpringIndexW >=  4.0) and (fSpringIndexW <=  8.0) then Index2 := 2 + 3*Ord(fQualityGradeOnDm) else
    if (fSpringIndexW >   8.0) and (fSpringIndexW <= 14.0) then Index2 := 3 + 3*Ord(fQualityGradeOnDm) else
    if (fSpringIndexW >  14.0) and (fSpringIndexW <= 20.0) then Index2 := 4 + 3*Ord(fQualityGradeOnDm);
    fAD := AD_TABLE[Index1][Index2]*mm;
    // alphaF
    fFactorAlphaF := (65.92*(Power(1000*FWireDiameter.Value, 3.3))/(Power(1000*FCoilDiameterDm.Value, 1.6))*
      (-0.84*Power(fSpringIndexW/10, 3) + 3.781*Power(fSpringIndexW/10,2) - 4.244*(fSpringIndexW/10) + 2.274))*N;
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
    if (fQualityGradeOnE1 = QualityGrade1) or (fQualityGradeOnE2 = QualityGrade1) then
    begin
      if (fSpringIndexW            ) > 12 then WarningMessage.Add('If "e" Quality Grade = 1 then "w" must be <= 12');
      if (fLengthL0/fCoilDiameterDm) >  5 then WarningMessage.Add('If "e" Quality Grade = 1 then "L0/Dm" must be <= 5');
    end;
    case fQualityGradeOnE1 of
      QualityGrade1: fE1 := 0.030*fLengthL0;
      QualityGrade2: fE1 := 0.050*fLengthL0;
      QualityGrade3: fE1 := 0.080*fLengthL0;
    end;
    case fQualityGradeOnE2 of
      QualityGrade1: fE2 := 0.015*(fCoilDiameterDm + fWireDiameter);
      QualityGrade2: fE2 := 0.030*(fCoilDiameterDm + fWireDiameter);
      QualityGrade3: fE2 := 0.060*(fCoilDiameterDm + fWireDiameter);
    end;
  end;
end;

// TWireTolerance

constructor TWireTolerance.Create;
var
  Resource: TResourceStream;
begin
  inherited Create;
  fTolFile := TCsvDocument.Create;
  fTolFile.Delimiter := ';';
  try
    Resource := TResourceStream.Create(HInstance, 'WIRETOL', RT_RCDATA);
    Resource.Position := 0;
    fTolFile.LoadFromStream(Resource);
  finally
    Resource.Free;
  end;
  Clear;
end;

destructor TWireTolerance.Destroy;
begin
  fTolFile.Destroy;
  inherited Destroy;
end;

procedure TWireTolerance.Clear;
begin
  fValue := 0*mm;
end;

function TWireTolerance.Search(const aID: string; const aWireDiameter: TMeters): boolean;
var
  i: longint;
begin
  Clear;
  for i := 0 to fTolFile.RowCount -1 do
  begin
    if Pos(fTolFile.Cells[0, 0], aID) = 1 then
      if (aWireDiameter >  StrToFloat(fTolFile.Cells[1, i])*mm) and
         (aWireDiameter <= StrToFloat(fTolFile.Cells[2, i])*mm) then
      begin
        fValue := StrToFloat(fTolFile.Cells[3, i])*mm;
        Break;
      end;
  end;
  Result := (fValue > (0*mm));
end;

// TDIN2194

constructor TDIN2194.Create;
begin
  inherited Create;
end;

destructor TDIN2194.Destroy;
begin
  inherited Destroy;
end;

procedure TDIN2194.Clear;
begin
  fQualityGradeOnDm               := QualityGrade2;
  fQualityGradeOnTorqueT1         := QualityGrade2;
  fQualityGradeOnTorqueT2         := QualityGrade2;
  fQualityGradeOnRelativeEndAngle := QualityGrade2;
  fQualityGradeOnLk0              := QualityGrade2;
  fQualityGradeOnLegLengths       := QualityGrade2;
end;

procedure TDIN2194.Solve;
var
  d, Dm, Kf, Ks: double;
  Check: boolean;
begin
  if fWireDiameter        <  (0.07*mm) then ErrorMessage.Add('Wire diameter d < 0.07mm.');
  if fWireDiameter        >  (17  *mm) then ErrorMessage.Add('Wire diameter d > 17 mm.');
  if fCoilDiameter.Value  <= (0      ) then ErrorMessage.Add('Mean coil diameter unassigned.');
  if fCoilDiameter        >  (340 *mm) then ErrorMessage.Add('Mean coil diameter Dm > 340 mm.');
  if fActiveCoils         <  (2      ) then ErrorMessage.Add('Number of active coils n < 2.');

  // leg lengths
  if (fLegLength1 <  (0.5*mm)) then ErrorMessage.Add('Leg length1 is < 0.5 mm.');
  if (fLegLength2 <  (0.5*mm)) then ErrorMessage.Add('Leg length2 is < 0.5 mm.');
  if (fLegLength3 <  (0.5*mm)) then ErrorMessage.Add('Leg length3 is < 0.5 mm.');

  if (fLegLength1 < (1000*mm)) then ErrorMessage.Add('Leg length1 is > 1000 mm.');
  if (fLegLength2 < (1000*mm)) then ErrorMessage.Add('Leg length2 is > 1000 mm.');
  if (fLegLength3 < (1000*mm)) then ErrorMessage.Add('Leg length3 is > 1000 mm.');

  if (fLegLength1 < (1000*mm)) then ErrorMessage.Add('Leg length1 is > 1000 mm.');
  if (fLegLength2 < (1000*mm)) then ErrorMessage.Add('Leg length2 is > 1000 mm.');
  if (fLegLength3 < (1000*mm)) then ErrorMessage.Add('Leg length3 is > 1000 mm.');

  Check := ErrorMessage.Count = 0;
  if Check then
  begin
    fSpringIndex := fWireDiameter/fCoilDiameter;
    if SpringIndex < 4  then ErrorMessage.Add('Spring index w < 4.');
    if SpringIndex > 20 then ErrorMessage.Add('Spring index w > 20.');
  end;

  Check := ErrorMessage.Count = 0;
  if Check then
  begin
    d  := fWireDiameter.Value([pMilli]);
    Dm := fCoilDiameter.Value([pMilli]);

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

    fToleranceOnFreeBodyLength := 0.06*Power(d, 0.83)*(1 + 0.001*Power(fSpringIndex, 2.5))*fActiveCoils*kf*QualityFactor(fQualityGradeOnLk0)*mm;

    // tolerance on leg lengths (unloaded spring)

    fToleranceOnLegLength1 := (0.2*d + QualityFactor(fQualityGradeOnLegLengths))*LegLengthCoefficent(fLegLength1)*mm;
    fToleranceOnLegLength2 := (0.2*d + QualityFactor(fQualityGradeOnLegLengths))*LegLengthCoefficent(fLegLength2)*mm;
    fToleranceOnLegLength3 := (0.2*d + QualityFactor(fQualityGradeOnLegLengths))*LegLengthCoefficent(fLegLength3)*mm;

    // tolerance on bend radii (unloaded spring)

    fToleranceOnBendRadius1 := (0.6 + 0.2*fBendRadius1.Value([pMilli]))*QualityFactor(fQualityGradeOnBendRadii)*mm;
    fToleranceOnBendRadius2 := (0.6 + 0.2*fBendRadius2.Value([pMilli]))*QualityFactor(fQualityGradeOnBendRadii)*mm;
    fToleranceOnBendRadius3 := (0.6 + 0.2*fBendRadius3.Value([pMilli]))*QualityFactor(fQualityGradeOnBendRadii)*mm;

    // tolerance on angles of bends on leg

    fToleranceOnBendAngle1 := 4*sqrt(fBendRadius1.Value([pMilli])/d)*QualityFactor(fQualityGradeOnBendAngles)*deg;
    fToleranceOnBendAngle2 := 4*sqrt(fBendRadius2.Value([pMilli])/d)*QualityFactor(fQualityGradeOnBendAngles)*deg;
    fToleranceOnBendAngle3 := 4*sqrt(fBendRadius3.Value([pMilli])/d)*QualityFactor(fQualityGradeOnBendAngles)*deg;
  end;

  if fFreeBodyLengthLk > (630*mm) then ErrorMessage.Add('Free body length Lk > 630 mm.');
end;

function TDIN2194.LegLengthCoefficent(LegLength: TMeters): double;
var
  LegLen: double;
begin
  LegLen := LegLength.Value([pMilli]);
  if (LegLen >= 0.5) and (LegLen <=    6) then result := 0.3 else
  if (LegLen >    6) and (LegLen <=   30) then result := 0.8 else
  if (LegLen >   30) and (LegLen <=  120) then result := 1.3 else
  if (LegLen >  120) and (LegLen <=  400) then result := 1.9 else
  if (LegLen >  400) and (LegLen <= 1000) then result := 3.2 else Result := 0;
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

