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

unit springmaterials;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CsvDocument, LCLType, SysUtils, UtilsBase, ADim;

type
  TMaterialDB = class
  private
    fItemIndex: longint;
    fMatFile: TCsvDocument;
    fSurfaceTreatment: string;
    fTensileStrengthRm: TPascals;
    fYoungModulusE20: TPascals;
    fYoungModulusE: TPascals;
    fShearModulusG20: TPascals;
    fShearModulusG: TPascals;
    fPoissonRatio: double;
    fWireDiameter: TMeters;
    fDensityRho: TKilogramsPerCubicMeter;
    fTemperature: double;
    fTemperatureMin: double;
    fTemperatureMax: double;

    fFatigueFactorA: TPascals;
    fFatigueFactorB: double;
    fTorsionalStressTauStar: TPascals;
    fTorsionalStressTauUT: TPascals;
    fTorsionalStressTauYield: TPascals;
    fTorsionalStressTauOE7: TPascals;
    fTorsionalStressTauOE6: TPascals;
    fTorsionalStressTauOE5: TPascals;
    fTorsionalStressTauOE3: TPascals;
    fTorsionalStressTauUE7: TPascals;
    fTorsionalStressTauUE6: TPascals;
    fTorsionalStressTauUE5: TPascals;
    fNumOfCyclesE7: double;
    fNumOfCyclesE6: double;
    fNumOfCyclesE5: double;
    fNumOfCyclesE3: double;
    function GetItem(Index: longint): string;
    function GetCount: longint;
  public
    constructor Create;
    destructor Destroy; override;
    function GetG(const aTemperature: double): TPascals;
    function GetE(const aTemperature: double): TPascals;
    procedure Clear;

    function Search (const aID: string; const aWireDiameter: TMeters; const aTemperature: double; const aSurfaceTreatment: string): longint;
    function SetItem(const aID: string; const aWireDiameter: TMeters; const aTemperature: double; const aSurfaceTreatment: string): longint;
  public
    property SurfaceTreatment: string read fSurfaceTreatment;
    property Tempetature: double read fTemperature;
    property TempetatureMin: double read fTemperatureMin;
    property TempetatureMax: double read fTemperatureMax;
    property TensileStrengthRm: TPascals read fTensileStrengthRm;
    property YoungModulusE20: TPascals read fYoungModulusE20;
    property YoungModulusE: TPascals read fYoungModulusE;
    property ShearModulusG20: TPascals read fShearModulusG20;
    property ShearModulusG: TPascals read fShearModulusG;
    property WireDiameter: TMeters read fWireDiameter;
    property PoissonRatio: double read fPoissonRatio;
    property DensityRho: TKilogramsPerCubicMeter read fDensityRho;
    property ItemIndex: longint read fItemIndex;
    property Items[Index: longint]: string read GetItem; default;
    property Count: longint read GetCount;

    property FatigueFactorA: TPascals read fFatigueFactorA;
    property FatigueFactorB: double read fFatigueFactorB;
    property TorsionalStressTauStar: TPascals read fTorsionalStressTauStar;
    property TorsionalStressTauUT: TPascals read fTorsionalStressTauUT;
    property TorsionalStressTauYield: TPascals read fTorsionalStressTauYield;
    property TorsionalStressTauOE7: TPascals read fTorsionalStressTauOE7;
    property TorsionalStressTauOE6: TPascals read fTorsionalStressTauOE6;
    property TorsionalStressTauOE5: TPascals read fTorsionalStressTauOE5;
    property TorsionalStressTauOE3: TPascals read fTorsionalStressTauOE3;
    property TorsionalStressTauUE7: TPascals read fTorsionalStressTauUE7;
    property TorsionalStressTauUE6: TPascals read fTorsionalStressTauUE6;
    property TorsionalStressTauUE5: TPascals read fTorsionalStressTauUE5;
    property NumOfCyclesE7: double read fNumOfCyclesE7;
    property NumOfCyclesE6: double read fNumOfCyclesE6;
    property NumOfCyclesE5: double read fNumOfCyclesE5;
    property NumOfCyclesE3: double read fNumOfCyclesE3;
  end;


var
  MAT: TMaterialDB;


implementation

uses
  Math;

// TMaterialDB class

constructor TMaterialDB.Create;
var
  Resource: TResourceStream;
begin
  inherited Create;
  fMatFile := TCsvDocument.Create;
  fMatFile.Delimiter := ';';
  try
    Resource := TResourceStream.Create(HInstance, 'MATERIAL', RT_RCDATA);
    Resource.Position := 0;
    fMatFile.LoadFromStream(Resource);
  finally
    Resource.Free;
  end;
  Clear;
end;

destructor TMaterialDB.Destroy;
begin
  fMatFile.Destroy;
  inherited destroy;
end;

function TMaterialDB.GetCount: longint;
begin
  Result := fMatFile.RowCount;
end;

function TMaterialDB.GetG(const aTemperature: double): TPascals;
var
  Ratio: double;
begin
  Result := fShearModulusG20;
  Ratio  := 0;

  if Pos('EN10270-1', GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000;
  if Pos('EN10270-2', GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000;
  if Pos('EN10089',   GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000;
  if Pos('EN10270-3', GetItem(fItemIndex)) = 1 then Ratio := 0.40/1000;
  if Pos('EN12166',   GetItem(fItemIndex)) = 1 then Ratio := 0.40/1000;

  Result := Result * (1 - Ratio*(aTemperature - 20));
end;

function TMaterialDB.GetE(const aTemperature: double): TPascals;
begin
  Result := GetG(aTemperature) * (2*(1 + fPoissonRatio));
end;

procedure TMaterialDB.Clear;
begin
  fItemIndex                     := -1;
  fFatigueFactorA                := 0*Pa;
  fFatigueFactorB                := 0;
  fSurfaceTreatment              := '';
  fTensileStrengthRm             := 0*Pa;
  fYoungModulusE20               := 0*Pa;
  fYoungModulusE                 := 0*Pa;
  fShearModulusG20               := 0*Pa;
  fShearModulusG                 := 0*Pa;
  fPoissonRatio                  := 0;
  fWireDiameter                  := 0*m;
  fDensityRho                    := 0*kg/m3;
  fTemperature                   := 0;
  fTemperatureMin                := 0;
  fTemperatureMax                := 0;
  fTorsionalStressTauStar        := 0*Pa;
  fTorsionalStressTauYield       := 0*Pa;
  fTorsionalStressTauOE7         := 0*Pa;
  fTorsionalStressTauOE6         := 0*Pa;
  fTorsionalStressTauOE5         := 0*Pa;
  fTorsionalStressTauOE3         := 0*Pa;
  fTorsionalStressTauUE7         := 0*Pa;
  fTorsionalStressTauUE6         := 0*Pa;
  fTorsionalStressTauUE5         := 0*Pa;
  fNumOfCyclesE7                 := 0;
  fNumOfCyclesE6                 := 0;
  fNumOfCyclesE5                 := 0;
  fNumOfCyclesE3                 := 0;
end;

function TMaterialDB.GetItem(Index: longint): string;
begin
  //0	1
  //ID	GRADE
  if Index <> -1 then
    Result := Format('%s Grade %s', [fMatFile.Cells[0, Index], fMatFile.Cells[1, Index]])
  else
    Result := '';
end;

function TMaterialDB.Search(const aID: string; const aWireDiameter: TMeters; const aTemperature: double; const aSurfaceTreatment: string): longint;
var
  i: longint;
begin
  Result := -1;
  for i := 0 to fMatFile.RowCount -1 do
    //2          3   	 4
    //TREATMENT  DMIN	 DMAX
    if (CompareText(aID, GetItem(i)) = 0) and (CompareText(aSurfaceTreatment, fMatFile.Cells[2, i]) = 0)  then
    begin
       if (aWireDiameter >  (StrToFloat(fMatFile.Cells[3, i]))*mm) and
          (aWireDiameter <= (StrToFloat(fMatFile.Cells[4, i]))*mm) then
       begin
         Result := i;
         Break;
       end;
    end;
end;

function TMaterialDB.SetItem(const aID: string; const aWireDiameter: TMeters; const aTemperature: double; const aSurfaceTreatment: string): longint;
var
  TO0, DTO0, TO1, DTO1, TU1, DTU1: TPascals;
  RM0, DRM, RMMAX: TPascals;
  DR0, DT0: TMeters;
begin
  Clear;

  fItemIndex := Search(aID, aWireDiameter, aTemperature, aSurfaceTreatment);
  if fItemIndex <> -1 then
  begin
    //5	  6	7	8	9	10	11	12,0	13,0	14,0	15	16	17	18	19	20
    //E   G	RHO	RM0	DRM	DR0	RMMAX	DT0	TO0	DTO0	TO1	DTO1	TU1	DTU1	TMIN	TMAX

    fSurfaceTreatment     := fMatFile.Cells[2,  fItemIndex];
    fWireDiameter         := aWireDiameter;
    fYoungModulusE20      := StrToFloat(fMatFile.Cells[5,  fItemIndex])*MPa;
    fShearModulusG20      := StrToFloat(fMatFile.Cells[6,  fItemIndex])*MPa;
    fDensityRho           := StrToFloat(fMatFile.Cells[7,  fItemIndex])*kg/m3;

    RM0                   := StrToFloat(fMatFile.Cells[8,  fItemIndex])*MPa;
    DRM                   := StrToFloat(fMatFile.Cells[9,  fItemIndex])*MPa;
    DR0                   := StrToFloat(fMatFile.Cells[10, fItemIndex])*mm;
    RMMAX                 := StrToFloat(fMatFile.Cells[11, fItemIndex])*MPa;
    fTensileStrengthRm    := RM0-DRM*Log10(fWireDiameter/DR0);

    if fTensileStrengthRm > RMMAX then
      fTensileStrengthRm := RMMAX;

    fTorsionalStressTauUT := fTensileStrengthRm*0.577;

    DT0                   := StrToFloat(fMatFile.Cells[12, fItemIndex])*mm;
    TO0                   := StrToFloat(fMatFile.Cells[13, fItemIndex])*MPa;
    DTO0                  := StrToFloat(fMatFile.Cells[14, fItemIndex])*MPa;
    TO1                   := StrToFloat(fMatFile.Cells[15, fItemIndex])*MPa;
    DTO1                  := StrToFloat(fMatFile.Cells[16, fItemIndex])*MPa;
    TU1                   := StrToFloat(fMatFile.Cells[17, fItemIndex])*MPa;
    DTU1                  := StrToFloat(fMatFile.Cells[18, fItemIndex])*MPa;

    fNumOfCyclesE7        := StrToFloat(fMatFile.Cells[19, fItemIndex]);

    fTemperature          := aTemperature;
    fTemperatureMin       := StrToFloat(fMatFile.Cells[20, fItemIndex]);
    fTemperatureMax       := StrToFloat(fMatFile.Cells[21, fItemIndex]);

    fPoissonRatio         := fYoungModulusE20/(2*fShearModulusG20)-1;
    fShearModulusG        := GetG(fTemperature);
    fYoungModulusE        := GetE(fTemperature);

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
  Result := fItemIndex;
end;


initialization
begin
  MAT := TMaterialDB.Create;
end;

finalization
begin
  MAT.Destroy;
end;

end.

