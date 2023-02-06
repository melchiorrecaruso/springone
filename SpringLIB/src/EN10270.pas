{ EN10270

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

unit EN10270;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, CsvDocument, LCLType, SysUtils, UtilsBase, UnitOfMeasurement;

type
  TMaterialDB = class
  private
    fItemIndex: longint;
    fMatFile: TCsvDocument;
    fSurfaceTreatment: string;
    fTensileStrengthRm: TPressure;
    fYoungModulusE20: TPressure;
    fYoungModulusE: TPressure;
    fShearModulusG20: TPressure;
    fShearModulusG: TPressure;
    fPoissonRatio: double;
    fWireDiameter: TLength;
    fDensityRho: TDensity;
    fTemperature: double;
    fTemperatureMin: double;
    fTemperatureMax: double;

    fFatigueFactorA: TPressure;
    fFatigueFactorB: double;
    fTorsionalStressTauStar: TPressure;
    fTorsionalStressTauUT: TPressure;
    fTorsionalStressTauYield: TPressure;
    fTorsionalStressTauOE7: TPressure;
    fTorsionalStressTauOE6: TPressure;
    fTorsionalStressTauOE5: TPressure;
    fTorsionalStressTauOE3: TPressure;
    fTorsionalStressTauUE7: TPressure;
    fTorsionalStressTauUE6: TPressure;
    fTorsionalStressTauUE5: TPressure;
    fNumOfCyclesE7: double;
    fNumOfCyclesE6: double;
    fNumOfCyclesE5: double;
    fNumOfCyclesE3: double;
    function GetItem(Index: longint): string;
    function GetCount: longint;
  public
    constructor Create;
    destructor Destroy; override;
    function GetG(const aTemperature: double): TPressure;
    function GetE(const aTemperature: double): TPressure;
    procedure Clear;

    function Search (const aID: string; const aWireDiameter: TLength; const aTemperature: double; const aSurfaceTreatment: string): longint;
    function SetItem(const aID: string; const aWireDiameter: TLength; const aTemperature: double; const aSurfaceTreatment: string): longint;
  public
    property SurfaceTreatment: string read fSurfaceTreatment;
    property Tempetature: double read fTemperature;
    property TempetatureMin: double read fTemperatureMin;
    property TempetatureMax: double read fTemperatureMax;
    property TensileStrengthRm: TPressure read fTensileStrengthRm;
    property YoungModulusE20: TPressure read fYoungModulusE20;
    property YoungModulusE: TPressure read fYoungModulusE;
    property ShearModulusG20: TPressure read fShearModulusG20;
    property ShearModulusG: TPressure read fShearModulusG;
    property WireDiameter: TLength read fWireDiameter;
    property PoissonRatio: double read fPoissonRatio;
    property DensityRho: TDensity read fDensityRho;
    property ItemIndex: longint read fItemIndex;
    property Items[Index: longint]: string read GetItem; default;
    property Count: longint read GetCount;

    property FatigueFactorA: TPressure read fFatigueFactorA;
    property FatigueFactorB: double read fFatigueFactorB;
    property TorsionalStressTauStar: TPressure read fTorsionalStressTauStar;
    property TorsionalStressTauUT: TPressure read fTorsionalStressTauUT;
    property TorsionalStressTauYield: TPressure read fTorsionalStressTauYield;
    property TorsionalStressTauOE7: TPressure read fTorsionalStressTauOE7;
    property TorsionalStressTauOE6: TPressure read fTorsionalStressTauOE6;
    property TorsionalStressTauOE5: TPressure read fTorsionalStressTauOE5;
    property TorsionalStressTauOE3: TPressure read fTorsionalStressTauOE3;
    property TorsionalStressTauUE7: TPressure read fTorsionalStressTauUE7;
    property TorsionalStressTauUE6: TPressure read fTorsionalStressTauUE6;
    property TorsionalStressTauUE5: TPressure read fTorsionalStressTauUE5;
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

// TMaterialDB class. EN10270-1 and EN10270-2 spring wire materials database.

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

function TMaterialDB.GetG(const aTemperature: double): TPressure;
var
  Ratio: double;
begin
  Result := fShearModulusG20;
// L'influenza della temperatura di esercizio sul modulo di elasticità e sul modulo di elasticità
// tangenziale è illustrata dalla formula seguente, per valori di media generale, per i materiali
// specificati sotto;
  Ratio := 0;

  if Pos('EN10270-1', GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000; // per filo secondo EN10270-1
  if Pos('EN10270-2', GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000; // per filo secondo EN10270-2
  if Pos('EN10089',   GetItem(fItemIndex)) = 1 then Ratio := 0.25/1000; // per filo secondo EN10089
  if Pos('EN10270-3', GetItem(fItemIndex)) = 1 then Ratio := 0.40/1000; // per filo secondo EN10270-3
  if Pos('EN12166',   GetItem(fItemIndex)) = 1 then Ratio := 0.40/1000; // per filo secondo EN12166

  Result := Result * (1 - Ratio*(aTemperature - 20));
end;

function TMaterialDB.GetE(const aTemperature: double): TPressure;
begin
  Result := GetG(aTemperature) * (2*(1 + fPoissonRatio));
end;

procedure TMaterialDB.Clear;
begin
  fItemIndex               := -1;
  fFatigueFactorA          := 0*MPa;
  fFatigueFactorB          := 0;
  fSurfaceTreatment        := '';
  fTensileStrengthRm       := 0*MPa;
  fYoungModulusE20         := 0*MPa;
  fYoungModulusE           := 0*MPa;
  fShearModulusG20         := 0*MPa;
  fShearModulusG           := 0*MPa;
  fPoissonRatio            := 0;
  fWireDiameter            := 0*mm;
  fDensityRho              := 0*kg_m3;
  fTemperature             := 0;
  fTemperatureMin          := 0;
  fTemperatureMax          := 0;
  fTorsionalStressTauStar  := 0*MPa;
  fTorsionalStressTauYield := 0*MPa;
  fTorsionalStressTauOE7   := 0*MPa;
  fTorsionalStressTauOE6   := 0*MPa;
  fTorsionalStressTauOE5   := 0*MPa;
  fTorsionalStressTauOE3   := 0*MPa;
  fTorsionalStressTauUE7   := 0*MPa;
  fTorsionalStressTauUE6   := 0*MPa;
  fTorsionalStressTauUE5   := 0*MPa;
  fNumOfCyclesE7           := 0;
  fNumOfCyclesE6           := 0;
  fNumOfCyclesE5           := 0;
  fNumOfCyclesE3           := 0;
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

function TMaterialDB.Search(const aID: string; const aWireDiameter: TLength; const aTemperature: double; const aSurfaceTreatment: string): longint;
var
  i: longint;
begin
  Result := -1;
  for i := 0 to fMatFile.RowCount -1 do
    //2          3   	 4
    //TREATMENT  DMIN	 DMAX
    if (CompareText(aID, GetItem(i)) = 0) and (CompareText(aSurfaceTreatment, fMatFile.Cells[2, i]) = 0)  then
    begin
       if (aWireDiameter >  (TryTextToFloat(fMatFile.Cells[3, i]))*mm) and
          (aWireDiameter <= (TryTextToFloat(fMatFile.Cells[4, i]))*mm) then
       begin
         Result := i;
         Break;
       end;
    end;
end;

function TMaterialDB.SetItem(const aID: string; const aWireDiameter: TLength; const aTemperature: double; const aSurfaceTreatment: string): longint;
var
  TO0, DTO0, TO1, DTO1, TU1, DTU1: TPressure;
  RM0, DRM, RMMAX: TPressure;
  DR0, DT0: TLength;
begin
  Clear;

  fItemIndex := Search(aID, aWireDiameter, aTemperature, aSurfaceTreatment);
  if fItemIndex <> -1 then
  begin
    //5	  6	7	8	9	10	11	12,0	13,0	14,0	15	16	17	18	19	20
    //E   G	RHO	RM0	DRM	DR0	RMMAX	DT0	TO0	DTO0	TO1	DTO1	TU1	DTU1	TMIN	TMAX

    fSurfaceTreatment     := fMatFile.Cells[2,  fItemIndex];
    fWireDiameter         := aWireDiameter;
    fYoungModulusE20      := TryTextToFloat(fMatFile.Cells[5,  fItemIndex])*MPa;
    fShearModulusG20      := TryTextToFloat(fMatFile.Cells[6,  fItemIndex])*MPa;
    fDensityRho           := TryTextToFloat(fMatFile.Cells[7,  fItemIndex])*kg_dm3;

    RM0                   := TryTextToFloat(fMatFile.Cells[8,  fItemIndex])*MPa;
    DRM                   := TryTextToFloat(fMatFile.Cells[9,  fItemIndex])*MPa;
    DR0                   := TryTextToFloat(fMatFile.Cells[10, fItemIndex])*mm;
    RMMAX                 := TryTextToFloat(fMatFile.Cells[11, fItemIndex])*MPa;
    fTensileStrengthRm    := RM0-DRM*Log10(fWireDiameter/DR0);

    if fTensileStrengthRm > RMMAX then
      fTensileStrengthRm := RMMAX;

    fTorsionalStressTauUT := fTensileStrengthRm*0.577;

    DT0                   := TryTextToFloat(fMatFile.Cells[12, fItemIndex])*mm;
    TO0                   := TryTextToFloat(fMatFile.Cells[13, fItemIndex])*MPa;
    DTO0                  := TryTextToFloat(fMatFile.Cells[14, fItemIndex])*MPa;
    TO1                   := TryTextToFloat(fMatFile.Cells[15, fItemIndex])*MPa;
    DTO1                  := TryTextToFloat(fMatFile.Cells[16, fItemIndex])*MPa;
    TU1                   := TryTextToFloat(fMatFile.Cells[17, fItemIndex])*MPa;
    DTU1                  := TryTextToFloat(fMatFile.Cells[18, fItemIndex])*MPa;

    fNumOfCyclesE7        := TryTextToFloat(fMatFile.Cells[19, fItemIndex]); ;

    fTemperature          := aTemperature;
    fTemperatureMin       := TryTextToFloat(fMatFile.Cells[20, fItemIndex]);
    fTemperatureMax       := TryTextToFloat(fMatFile.Cells[21, fItemIndex]);

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

