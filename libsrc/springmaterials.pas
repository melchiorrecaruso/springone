{ Copyright (C) 2022-2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit SpringMaterials;

{$mode ObjFPC}{$H+}

interface

uses
  ADim, Classes, SysUtils, BaseUtils;

type
  TMaterial = class
  private
    FName: string;
    fGrade: TGrade;
    fRegulation: TRegulation;
    fTensileStrengthRm: TQuantity;
    fYoungModulusE20: TQuantity;
    fYoungModulusE: TQuantity;
    fShearModulusG20: TQuantity;
    fShearModulusG: TQuantity;
    fPoissonRatio: double;
    fWireDiameter: TQuantity;
    fDensityRho: TQuantity;
    fTemperature: TQuantity;
    fTemperatureMin: TQuantity;
    fTemperatureMax: TQuantity;
    fTreatment: string;
    fFatigueFactorA: TQuantity;
    fFatigueFactorB: double;
    fTorsionalStressTauStar: TQuantity;
    fTorsionalStressTauUT: TQuantity;
    fTorsionalStressTauYield: TQuantity;
    fTorsionalStressTauOE7: TQuantity;
    fTorsionalStressTauOE6: TQuantity;
    fTorsionalStressTauOE5: TQuantity;
    fTorsionalStressTauOE3: TQuantity;
    fTorsionalStressTauUE7: TQuantity;
    fTorsionalStressTauUE6: TQuantity;
    fTorsionalStressTauUE5: TQuantity;
    fNumOfCyclesE7: double;
    fNumOfCyclesE6: double;
    fNumOfCyclesE5: double;
    fNumOfCyclesE3: double;
    function GetCount: longint;
    function GetName(Index: longint): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetG(const aTemperature: TQuantity): TQuantity;
    function GetE(const aTemperature: TQuantity): TQuantity;
    procedure Load(const aName: string; const aWireDiameter: TQuantity; const aTemperature: TQuantity; const aTreatment: string);
    procedure Clear;
  public
    property Name: string read FName;
    property Grade: TGrade read fGrade;
    property Regulation: TRegulation read fRegulation;
    property Temperature: TQuantity read fTemperature;
    property TemperatureMin: TQuantity read fTemperatureMin;
    property TemperatureMax: TQuantity read fTemperatureMax;
    property Treatment: string read FTreatment;
    property TensileStrengthRm: TQuantity read fTensileStrengthRm;
    property YoungModulusE20: TQuantity read fYoungModulusE20;
    property YoungModulusE: TQuantity read fYoungModulusE;
    property ShearModulusG20: TQuantity read fShearModulusG20;
    property ShearModulusG: TQuantity read fShearModulusG;
    property WireDiameter: TQuantity read fWireDiameter;
    property PoissonRatio: double read fPoissonRatio;
    property DensityRho: TQuantity read fDensityRho;
    property FatigueFactorA: TQuantity read fFatigueFactorA;
    property FatigueFactorB: double read fFatigueFactorB;
    property TorsionalStressTauStar: TQuantity read fTorsionalStressTauStar;
    property TorsionalStressTauUT: TQuantity read fTorsionalStressTauUT;
    property TorsionalStressTauYield: TQuantity read fTorsionalStressTauYield;
    property TorsionalStressTauOE7: TQuantity read fTorsionalStressTauOE7;
    property TorsionalStressTauOE6: TQuantity read fTorsionalStressTauOE6;
    property TorsionalStressTauOE5: TQuantity read fTorsionalStressTauOE5;
    property TorsionalStressTauOE3: TQuantity read fTorsionalStressTauOE3;
    property TorsionalStressTauUE7: TQuantity read fTorsionalStressTauUE7;
    property TorsionalStressTauUE6: TQuantity read fTorsionalStressTauUE6;
    property TorsionalStressTauUE5: TQuantity read fTorsionalStressTauUE5;
    property NumOfCyclesE7: double read fNumOfCyclesE7;
    property NumOfCyclesE6: double read fNumOfCyclesE6;
    property NumOfCyclesE5: double read fNumOfCyclesE5;
    property NumOfCyclesE3: double read fNumOfCyclesE3;
    property Names[Index: longint]: string read GetName;
    property Count: longint read GetCount;
  end;


var
  MAT: TMaterial;


implementation

const
  WIRESPECS_TABLE : array[0..21] of record Name: string; Reg: TRegulation; Grade: TGrade; Treatment: string; DMin,DMax, E20, G20, RHO, RM0, DRM, DR0, RMMAX, DT0, TO0, DTO0, TO1, DTO1, TU1, DTU1, CYCLES, TMIN, TMAX : double; end = (
    (Name:'EN10270-1 class SM';      Reg:EN10270P1; Grade:SM;      Treatment:'';            DMin:0.28; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:1971; DRM:740; DR0:1.0; RMMAX:2370; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-40; TMAX:120),
    (Name:'EN10270-1 class DM';      Reg:EN10270P1; Grade:DM;      Treatment:'';            DMin:0.28; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:1971; DRM:740; DR0:1.0; RMMAX:2370; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-40; TMAX:120),
    (Name:'EN10270-1 class SL';      Reg:EN10270P1; Grade:SL;      Treatment:'';            DMin:0.95; DMax:10.0; E20:206000; G20:81500; RHO:7850; RM0:1712; DRM:660; DR0:1.0; RMMAX:1720; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-40; TMAX:120),
    (Name:'EN10270-1 class SH';      Reg:EN10270P1; Grade:SH;      Treatment:'';            DMin:0.28; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:2220; DRM:820; DR0:1.0; RMMAX:2660; DT0:1.0; TO0:506; DTO0:175; TO1:1118; DTO1:410; TU1:832; DTU1:322; CYCLES:1.00E+07; TMIN:-40; TMAX:120),
    (Name:'EN10270-1 class DH';      Reg:EN10270P1; Grade:DH;      Treatment:'';            DMin:0.05; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:2218; DRM:817; DR0:1.0; RMMAX:2800; DT0:1.0; TO0:506; DTO0:175; TO1:1118; DTO1:410; TU1:832; DTU1:322; CYCLES:1.00E+07; TMIN:-40; TMAX:120),
    (Name:'EN10270-2 class FDC';     Reg:EN10270P2; Grade:FDC;     Treatment:'';            DMin:0.50; DMax:17.0; E20:206000; G20:79500; RHO:7850; RM0:1834; DRM:488; DR0:1.0; RMMAX:1900; DT0:1.0; TO0:375; DTO0:117; TO1: 889; DTO1:263; TU1:669; DTU1:183; CYCLES:1.00E+07; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class FDCrV';   Reg:EN10270P2; Grade:FDCrV;   Treatment:'';            DMin:0.50; DMax:17.0; E20:206000; G20:79500; RHO:7850; RM0:1906; DRM:464; DR0:1.0; RMMAX:2000; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class FDSiCr';  Reg:EN10270P2; Grade:FDSiCr;  Treatment:'';            DMin:0.50; DMax:17.0; E20:206000; G20:79500; RHO:7850; RM0:2103; DRM:435; DR0:1.0; RMMAX:2100; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN: 60; TMAX:250),
    (Name:'EN10270-2 class FDSiCrV'; Reg:EN10270P2; Grade:FDSiCrV; Treatment:'';            DMin:0.50; DMax:17.0; E20:206000; G20:79500; RHO:7850; RM0:2328; DRM:468; DR0:1.0; RMMAX:2280; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class TDC';     Reg:EN10270P2; Grade:TDC;     Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1781; DRM:391; DR0:1.0; RMMAX:1850; DT0:1.0; TO0:375; DTO0:117; TO1: 889; DTO1:263; TU1:669; DTU1:183; CYCLES:1.00E+07; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class TDCrV';   Reg:EN10270P2; Grade:TDCrV;   Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1900; DRM:518; DR0:1.0; RMMAX:1910; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class TDSiCr';  Reg:EN10270P2; Grade:TDSiCr;  Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:2125; DRM:466; DR0:1.0; RMMAX:2080; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class TDSiCrV'; Reg:EN10270P2; Grade:TDSiCrV; Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:2270; DRM:475; DR0:1.0; RMMAX:2230; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class VDC';     Reg:EN10270P2; Grade:VDC;     Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1781; DRM:391; DR0:1.0; RMMAX:1850; DT0:1.0; TO0:553; DTO0:174; TO1: 833; DTO1:226; TU1:418; DTU1: 99; CYCLES:1.00E+07; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class VDCrV';   Reg:EN10270P2; Grade:VDCrV;   Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1900; DRM:518; DR0:1.0; RMMAX:1910; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-60; TMAX:200),
    (Name:'EN10270-2 class VDSiCr';  Reg:EN10270P2; Grade:VDSiCr;  Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:2125; DRM:466; DR0:1.0; RMMAX:2080; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-60; TMAX:250),
    (Name:'EN10270-2 class VDSiCrV'; Reg:EN10270P2; Grade:VDSiCrV; Treatment:'';            DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:2270; DRM:475; DR0:1.0; RMMAX:2230; DT0:1.0; TO0:  0; DTO0:  0; TO1:   0; DTO1:  0; TU1:  0; DTU1:  0; CYCLES:       0; TMIN:-60; TMAX:250),
    (Name:'EN10270-1 class SH';      Reg:EN10270P1; Grade:SH;      Treatment:'Shot peened'; DMin:0.28; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:2220; DRM:820; DR0:1.0; RMMAX:2660; DT0:1.0; TO0:601; DTO0:193; TO1:1119; DTO1:409; TU1:698; DTU1:273; CYCLES:1.00E+07; TMIN:-40; TMAX: 20),
    (Name:'EN10270-1 class DH';      Reg:EN10270P1; Grade:DH;      Treatment:'Shot peened'; DMin:0.05; DMax:20.0; E20:206000; G20:81500; RHO:7850; RM0:2218; DRM:817; DR0:1.0; RMMAX:2800; DT0:1.0; TO0:601; DTO0:193; TO1:1119; DTO1:409; TU1:698; DTU1:273; CYCLES:1.00E+07; TMIN: 40; TMAX:120),
    (Name:'EN10270-2 class FDC';     Reg:EN10270P2; Grade:FDC;     Treatment:'Shot peened'; DMin:0.50; DMax:17.0; E20:206000; G20:79500; RHO:7850; RM0:1834; DRM:488; DR0:1.0; RMMAX:1900; DT0:1.0; TO0:490; DTO0:138; TO1: 881; DTO1:248; TU1:552; DTU1:195; CYCLES:1.00E+07; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class TDC';     Reg:EN10270P2; Grade:TDC;     Treatment:'Shot peened'; DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1781; DRM:391; DR0:1.0; RMMAX:1850; DT0:1.0; TO0:490; DTO0:138; TO1: 881; DTO1:248; TU1:552; DTU1:195; CYCLES:1.00E+07; TMIN:  0; TMAX:  0),
    (Name:'EN10270-2 class VDC';     Reg:EN10270P2; Grade:VDC;     Treatment:'Shot peened'; DMin:0.50; DMax:10.0; E20:206000; G20:79500; RHO:7850; RM0:1781; DRM:391; DR0:1.0; RMMAX:1850; DT0:1.0; TO0:375; DTO0:117; TO1: 889; DTO1:263; TU1:669; DTU1:183; CYCLES:1.00E+07; TMIN:  0; TMAX:  0));

// TMaterial class

constructor TMaterial.Create;
begin
  inherited Create;
  Clear;
end;

destructor TMaterial.Destroy;
begin
  inherited destroy;
end;

procedure TMaterial.Clear;
begin
  fName := '';
end;

function TMaterial.GetCount: longint;
begin
  Result := Length(WIRESPECS_TABLE);
end;

function TMaterial.GetName(Index: longint): string;
begin
  Result := WIRESPECS_TABLE[Index].Name;
end;

function TMaterial.GetG(const aTemperature: TQuantity): TQuantity;
var
  Ratio: double;
begin
  Result := fShearModulusG20;
  Ratio  := 0;
  case fRegulation of
    EN10270P1: Ratio := 0.25/1000;
    EN10270P2: Ratio := 0.25/1000;
    EN10089  : Ratio := 0.25/1000;
    EN10270P3: Ratio := 0.40/1000;
    EN12166  : Ratio := 0.40/1000;
  else ErrorMessage.Add('Unknow regulation');
  end;
  Result := Result * (1 - Ratio*(degC.ToFloat(aTemperature) - 20));
end;

function TMaterial.GetE(const aTemperature: TQuantity): TQuantity;
begin
  Result := GetG(aTemperature) * (2*(1 + fPoissonRatio));
end;

procedure TMaterial.Load(const aName: string; const aWireDiameter: TQuantity; const aTemperature: TQuantity; const aTreatment: string);
var
  TO0, DTO0, TO1, DTO1, TU1, DTU1: TQuantity;
  RM0, DRM, RMMAX: TQuantity;
  DR0, DT0: TQuantity;
  I: longint;
begin
  Clear;

  I := Low(WIRESPECS_TABLE);
  while I in [Low(WIRESPECS_TABLE)..High(WIRESPECS_TABLE)] do
  begin
    if (CompareText(aName,      WIRESPECS_TABLE[I].Name     ) = 0) and
       (CompareText(aTreatment, WIRESPECS_TABLE[I].Treatment) = 0) then
    begin
      if (aWireDiameter >  (WIRESPECS_TABLE[I].DMin*mm)) and
         (aWireDiameter <= (WIRESPECS_TABLE[I].DMax*mm)) then Break;
    end;
    Inc(I);
  end;

  if I in [Low(WIRESPECS_TABLE)..High(WIRESPECS_TABLE)] then
  begin
    FName                 := WIRESPECS_TABLE[I].Name;
    FGrade                := WIRESPECS_TABLE[I].Grade;
    FRegulation           := WIRESPECS_TABLE[I].Reg;

    fWireDiameter         := aWireDiameter;
    fYoungModulusE20      := WIRESPECS_TABLE[I].E20*MPa;
    fShearModulusG20      := WIRESPECS_TABLE[I].G20*MPa;
    fDensityRho           := WIRESPECS_TABLE[I].RHO*kg/m3;

    RM0                   := WIRESPECS_TABLE[I].RM0*MPa;
    DRM                   := WIRESPECS_TABLE[I].DRM*MPa;
    DR0                   := WIRESPECS_TABLE[I].DR0*mm;
    RMMAX                 := WIRESPECS_TABLE[I].RMMAX*MPa;
    fTensileStrengthRm    := RM0-DRM*ADim.Log10(fWireDiameter/DR0);

    if fTensileStrengthRm > RMMAX then
    begin
      fTensileStrengthRm := RMMAX;
    end;
    fTorsionalStressTauUT := fTensileStrengthRm*0.577;

    DT0                   := WIRESPECS_TABLE[I].DT0*mm;
    TO0                   := WIRESPECS_TABLE[I].TO0*MPa;
    DTO0                  := WIRESPECS_TABLE[I].DTO0*MPa;
    TO1                   := WIRESPECS_TABLE[I].TO1*MPa;
    DTO1                  := WIRESPECS_TABLE[I].DTO1*MPa;
    TU1                   := WIRESPECS_TABLE[I].TU1*MPa;
    DTU1                  := WIRESPECS_TABLE[I].DTU1*MPa;

    fTemperature          := aTemperature;
    fTemperatureMin       := WIRESPECS_TABLE[I].TMIN*degC;
    fTemperatureMax       := WIRESPECS_TABLE[I].TMAX*degC;

    fPoissonRatio         := ScalarUnit.ToFloat(fYoungModulusE20/(2*fShearModulusG20)-1);
    fShearModulusG        := GetG(fTemperature);
    fYoungModulusE        := GetE(fTemperature);

    fNumOfCyclesE7        := WIRESPECS_TABLE[I].CYCLES;

    if (DT0 > 0*mm) and (fNumOfCyclesE7 > 0) then
    begin
      fTorsionalStressTauYield := TO1 - DTO1*Log10(aWireDiameter/DT0);
      fTorsionalStressTauOE7   := TO0 - DTO0*Log10(aWireDiameter/DT0);
      fTorsionalStressTauUE7   := TU1 - DTU1*Log10(aWireDiameter/DT0);
      fTorsionalStressTauOE3   := fTorsionalStressTauUT*0.95;
      fTorsionalStressTauStar  := fTorsionalStressTauOE7/(1 - ((fTorsionalStressTauYield - fTorsionalStressTauOE7)/fTorsionalStressTauUE7));

      fNumOfCyclesE6 := fNumOfCyclesE7/10;
      fNumOfCyclesE5 := fNumOfCyclesE7/100;
      fNumOfCyclesE3 := 1000;

      if ((fTorsionalStressTauOE7 > 0*MPa) and (fNumOfCyclesE7 > 0)) and
         ((fTorsionalStressTauOE3 > 0*MPa) and (fNumOfCyclesE3 > 0)) then
      begin
        fFatigueFactorB := LogN(fNumOfCyclesE7/fNumOfCyclesE3, fTorsionalStressTauOE7/fTorsionalStressTauOE3);
        fFatigueFactorA := fTorsionalStressTauOE3/Power(fNumOfCyclesE3, fFatigueFactorB);

        fTorsionalStressTauOE6 := fFatigueFactorA*Power(fNumOfCyclesE6, fFatigueFactorB);
        fTorsionalStressTauOE5 := fFatigueFactorA*Power(fNumOfCyclesE5, fFatigueFactorB);

        fTorsionalStressTauUE6 := fTorsionalStressTauStar*((fTorsionalStressTauYield - fTorsionalStressTauOE6)/
                                                           (fTorsionalStressTauStar  - fTorsionalStressTauOE6));

        fTorsionalStressTauUE5 := fTorsionalStressTauStar*((fTorsionalStressTauYield - fTorsionalStressTauOE5)/
                                                           (fTorsionalStressTauStar  - fTorsionalStressTauOE5));
      end;
    end;
  end;
end;

end.

