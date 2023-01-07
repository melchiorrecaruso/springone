{ EN15800 Helical Compression Spring Production Tolerance

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

unit EN15800;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, CsvDocument, Math, SysUtils, UtilsBase;

type
  TWireDiameterToleranceDB = class
  private
    fTolFile: TCsvDocument;
    fValue: double;
  public
    constructor Create;
    destructor Destroy; override;
    function Search(const aID: string; const aWireDiameter: double): boolean;
    procedure Clear;
  public
    property Value: double read fValue;
  end;

  TToleranceDB = class
  private
    fCheck: boolean;
    fAD: double;
    fAF1: double;
    fAF2: double;
    fAL0: double;
    fFactorAlphaF: double;
    fWireDiameter:  double;
    fWireDiameterTolerance: TWireDiameterToleranceDB;
    fCoilDiameterDm: double;
    fLoadF1:  double;
    fLoadF2:  double;
    fEccentricityE1: double;
    fEccentricityE2: double;
    fFactorKF: double;
    fLengthL0: double;
    fActiveCoils: double;
    fDmQualityGrade: longint;
    fL0QualityGrade: longint;
    fF1QualityGrade: longint;
    fF2QualityGrade: longint;
    fE1QualityGrade: longint;
    fE2QualityGrade: longint;
    fSpringRateR: double;
    fSpringIndexW: double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Solve;
    procedure Clear;
  public
    property WireDiameter: double  write fWireDiameter;
    property WireDiameterTolerance: TWireDiameterToleranceDB read fWireDiameterTolerance;
    property CoilDiameterDm: double write fCoilDiameterDm;
    property EccentricityE1: double read fEccentricityE1;
    property EccentricityE2: double read fEccentricityE2;
    property LoadF1: double read fLoadF1 write fLoadF1;
    property LoadF2: double read fLoadF2 write fLoadF2;
    property LengthL0: double write fLengthL0;
    property ActiveCoils: double write fActiveCoils;
    property DmQualityGrade: longint read fDmQualityGrade write fDmQualityGrade;
    property L0QualityGrade: longint read fL0QualityGrade write fL0QualityGrade;
    property F1QualityGrade: longint read fF1QualityGrade write fF1QualityGrade;
    property F2QualityGrade: longint read fF2QualityGrade write fF2QualityGrade;
    property E1QualityGrade: longint read fE1QualityGrade write fE1QualityGrade;
    property E2QualityGrade: longint read fE2QualityGrade write fE2QualityGrade;
    property SpringRateR: double write fSpringRateR;
    property CoilDiameterTolerance: double read fAD;
    property LoadF1Tolerance: double read fAF1;
    property LoadF2Tolerance: double read fAF2;
    property LengthL0Tolerance: double read fAL0;
    property SpringIndexW: double write fSpringIndexW;
  end;


var
  TOL: TToleranceDB;

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

// TToleranceDB

constructor TToleranceDB.Create;
begin
  inherited Create;
  fWireDiameterTolerance := TWireDiameterToleranceDB.Create;
end;

destructor TToleranceDB.Destroy;
begin
  fWireDiameterTolerance.Destroy;
  inherited Destroy;
end;

procedure TToleranceDB.Clear;
begin
  fCheck          := False;
  fAD             := 0;
  fAF1            := 0;
  fAF2            := 0;
  fAL0            := 0;
  fFactorAlphaF   := 0;
  fWireDiameter   := 0;
  fCoilDiameterDm := 0;
  fLoadF1         := 0;
  fLoadF2         := 0;
  fEccentricityE1 := 0;
  fEccentricityE2 := 0;
  fFactorKF       := 0;
  fLengthL0       := 0;
  fActiveCoils    := 0;
  fSpringRateR    := 0;
  fSpringIndexW   := 0;
  fDmQualityGrade := 0;
  fL0QualityGrade := 0;
  fF1QualityGrade := 0;
  fF2QualityGrade := 0;
  fE1QualityGrade := 0;
  fE2QualityGrade := 0;
end;

procedure TToleranceDB.Solve;
var
  Index:  longint;
  Index1: longint;
  Index2: longint;
begin
  // EN15800
  fCheck := True;
  // Scopo e campo di applicazione
  if fWireDiameter   < 0.07 then ErrorMessage.Add('Wire diameter < 0.07 mm.');
  if fWireDiameter   >   16 then ErrorMessage.Add('Wire diameter > 16 mm.');
  if fCoilDiameterDm < 0.63 then ErrorMessage.Add('Mean coil diameter < 0.63 mm.');
  if fCoilDiameterDm >  200 then ErrorMessage.Add('Mean coil diameter > 200 mm.');
  if fLengthL0       >  630 then ErrorMessage.Add('Length of unloaded spring > 630 mm.');
  if fActiveCoils    <    2 then ErrorMessage.Add('Number of active coils < 2.');
  if fSpringIndexW   <    4 then ErrorMessage.Add('Spring Index < 4.');
  if fSpringIndexW   >   20 then ErrorMessage.Add('Spring Index > 20.');

  if not fDmQualityGrade in [1, 2, 3] then ErrorMessage.Add('Dm quality grade value must be 1, 2 or 3.');
  if not fF1QualityGrade in [1, 2, 3] then ErrorMessage.Add('F1 quality grade value must be 1, 2 or 3.');
  if not fF2QualityGrade in [1, 2, 3] then ErrorMessage.Add('F2 quality grade value must be 1, 2 or 3.');
  if not fL0QualityGrade in [1, 2, 3] then ErrorMessage.Add('L0 quality grade value must be 1, 2 or 3.');

  fCheck := ErrorMessage.Count = 0;

  if fCheck then
  begin
    // Tolleranza AD sul diametro medio di avvolgimento CoilDiameterDm di una molla libera
    Index1 := 16;
    if (fCoilDiameterDm > 0.63) and (fCoilDiameterDm <= 200) then
    begin
      for Index := 0 to 16 do
        if (fCoilDiameterDm > AD_TABLE[Index][0]) and (fCoilDiameterDm <= AD_TABLE[Index][1]) then
        begin
          Index1 := Index;
          Break;
        end;
    end;

    Index2 := 10;
    if (fSpringIndexW >=  4.0) and (fSpringIndexW <=  8.0) then Index2 := 2 + 3*(fDmQualityGrade - 1) else
    if (fSpringIndexW >   8.0) and (fSpringIndexW <= 14.0) then Index2 := 3 + 3*(fDmQualityGrade - 1) else
    if (fSpringIndexW >  14.0) and (fSpringIndexW <= 20.0) then Index2 := 4 + 3*(fDmQualityGrade - 1);
    fAD := AD_TABLE[Index1][Index2];
    // alphaF
    fFactorAlphaF := 65.92*(Power(fWireDiameter,3.3)/Power(fCoilDiameterDm,1.6))*(-0.84*Power(fSpringIndexW/10,3)+3.781*Power(fSpringIndexW/10,2)-4.244*(fSpringIndexW/10)+2.274);
    // kF
    fFactorKF := 1/(3*Sqr(fActiveCoils))+8/(5*fActiveCoils)+0.803;
    // Tolleranza AF1 sul carico della molla LoadF1 ad una lunghezza data della molla L1
    case fF1QualityGrade of
      1: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1)/100)*0.63;
      2: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1)/100)*1.00;
      3: fAF1 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF1)/100)*1.60;
    end;
    // Tolleranza AF2 sul carico della molla LoadF2 ad una lunghezza data della molla L2
    case fF2QualityGrade of
      1: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2)/100)*0.63;
      2: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2)/100)*1.00;
      3: fAF2 := (fFactorAlphaF*fFactorKF+(1.5*fLoadF2)/100)*1.60;
    end;
    // Tolleranza AL0 sulla lunghezza LengthL0 di una molla libera
    case fL0QualityGrade of
      1: fAL0 := (fFactorAlphaF*fFactorKF*0.63)/fSpringRateR;
      2: fAL0 := (fFactorAlphaF*fFactorKF*1.00)/fSpringRateR;
      3: fAL0 := (fFactorAlphaF*fFactorKF*1.60)/fSpringRateR;
    end;
    //  EccentricityE1 & EccentricityE2
    if (fE1QualityGrade = 1) or (fE2QualityGrade = 1) then
    begin
      if (fSpringIndexW            ) > 12 then WarningMessage.Add('If "e" Quality Grade = 1 then "w" must be <= 12');
      if (fLengthL0/fCoilDiameterDm) >  5 then WarningMessage.Add('If "e" Quality Grade = 1 then "L0/Dm" must be <= 5');
    end;
    case fE1QualityGrade of
      1: fEccentricityE1 := 0.030*fLengthL0;
      2: fEccentricityE1 := 0.050*fLengthL0;
      3: fEccentricityE1 := 0.080*fLengthL0;
    end;
    case fE2QualityGrade of
      1: fEccentricityE2 := 0.015*(fCoilDiameterDm + fWireDiameter);
      2: fEccentricityE2 := 0.030*(fCoilDiameterDm + fWireDiameter);
      3: fEccentricityE2 := 0.060*(fCoilDiameterDm + fWireDiameter);
    end;
  end;
end;

// TWireDiameterToleranceDB

constructor TWireDiameterToleranceDB.Create;
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

destructor TWireDiameterToleranceDB.Destroy;
begin
  fTolFile.Destroy;
  inherited Destroy;
end;

procedure TWireDiameterToleranceDB.Clear;
begin
  fValue := 0;
end;

function TWireDiameterToleranceDB.Search(const aID: string; const aWireDiameter: double): boolean;
var
  i: longint;
begin
  Clear;
  for i := 0 to fTolFile.RowCount -1 do
  begin
    if Pos(fTolFile.Cells[0, 0], aID) = 1 then
      if (aWireDiameter >  TryTextToFloat(fTolFile.Cells[1, i])) and
         (aWireDiameter <= TryTextToFloat(fTolFile.Cells[2, i])) then
      begin
        fValue := TryTextToFloat(fTolFile.Cells[3, i]);
        Break;
      end;
  end;
  Result := (fValue > 0);
end;


initialization
begin
  TOL := TToleranceDB.Create;
end;

finalization
begin
  TOL.Destroy;
end;

end.

