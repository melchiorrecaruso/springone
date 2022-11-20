{ EN13906-1 Helical Compression Spring Designer

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
  Classes, CsvDocument, LCLType, SysUtils, UtilsBase;

type
  TMaterialDB = class
  private
    fItemIndex: longint;
    fMatFile: TCsvDocument;
    fSurfaceTreatment: string;
    fTensileStrengthRm: double;
    fYoungModulusE20: double;
    fYoungModulusE: double;
    fShearModulusG20: double;
    fShearModulusG: double;
    fPoissonRatio: double;
    fWireDiameter: double;
    fDensityRho: double;
    fTemperature: double;
    fTemperatureMin: double;
    fTemperatureMax: double;

    fFatigueFactorA: double;
    fFatigueFactorB: double;
    fTorsionalStressTauStar: double;
    fTorsionalStressTauUT: double;
    fTorsionalStressTauYield: double;
    fTorsionalStressTauOE7: double;
    fTorsionalStressTauOE6: double;
    fTorsionalStressTauOE5: double;
    fTorsionalStressTauOE3: double;
    fTorsionalStressTauUE7: double;
    fTorsionalStressTauUE6: double;
    fTorsionalStressTauUE5: double;
    fNumOfCyclesE7: double;
    fNumOfCyclesE6: double;
    fNumOfCyclesE5: double;
    fNumOfCyclesE3: double;
    function GetItem(Index: longint): string;
    function GetCount: longint;
  public
    constructor Create;
    destructor Destroy; override;
    function GetG(const aTemperature: double): double;
    function GetE(const aTemperature: double): double;
    procedure Clear;

    function Search (const aID: string; const aWireDiameter, aTemperature: double; const aSurfaceTreatment: string): longint;
    function SetItem(const aID: string; const aWireDiameter, aTemperature: double; const aSurfaceTreatment: string): longint;
  public
    property SurfaceTreatment: string read fSurfaceTreatment;
    property Tempetature: double read fTemperature;
    property TempetatureMin: double read fTemperatureMin;
    property TempetatureMax: double read fTemperatureMax;
    property TensileStrengthRm: double read fTensileStrengthRm;
    property YoungModulusE20: double read fYoungModulusE20;
    property YoungModulusE: double read fYoungModulusE;
    property ShearModulusG20: double read fShearModulusG20;
    property ShearModulusG: double read fShearModulusG;
    property WireDiameter: double read fWireDiameter;
    property PoissonRatio: double read fPoissonRatio;
    property DensityRho: double read fDensityRho;
    property ItemIndex: longint read fItemIndex;
    property Items[Index: longint]: string read GetItem; default;
    property Count: longint read GetCount;

    property FatigueFactorA: double read fFatigueFactorA;
    property FatigueFactorB: double read fFatigueFactorB;
    property TorsionalStressTauStar: double read fTorsionalStressTauStar;
    property TorsionalStressTauUT: double read fTorsionalStressTauUT;
    property TorsionalStressTauYield: double read fTorsionalStressTauYield;
    property TorsionalStressTauOE7: double read fTorsionalStressTauOE7;
    property TorsionalStressTauOE6: double read fTorsionalStressTauOE6;
    property TorsionalStressTauOE5: double read fTorsionalStressTauOE5;
    property TorsionalStressTauOE3: double read fTorsionalStressTauOE3;
    property TorsionalStressTauUE7: double read fTorsionalStressTauUE7;
    property TorsionalStressTauUE6: double read fTorsionalStressTauUE6;
    property TorsionalStressTauUE5: double read fTorsionalStressTauUE5;
    property NumOfCyclesE7: double read fNumOfCyclesE7;
    property NumOfCyclesE6: double read fNumOfCyclesE6;
    property NumOfCyclesE5: double read fNumOfCyclesE5;
    property NumOfCyclesE3: double read fNumOfCyclesE3;
  end;


var
  MAT : TMaterialDB;


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

function TMaterialDB.GetG(const aTemperature: double): double;
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

function TMaterialDB.GetE(const aTemperature: double): double;
begin
  Result := GetG(aTemperature) * (2*(1 + fPoissonRatio));
end;

procedure TMaterialDB.Clear;
begin
  fItemIndex               := -1;
  fFatigueFactorA          := 0;
  fFatigueFactorB          := 0;
  fSurfaceTreatment        := '';
  fTensileStrengthRm       := 0;
  fYoungModulusE20         := 0;
  fYoungModulusE           := 0;
  fShearModulusG20         := 0;
  fShearModulusG           := 0;
  fPoissonRatio            := 0;
  fWireDiameter            := 0;
  fDensityRho              := 0;
  fTemperature             := 0;
  fTemperatureMin          := 0;
  fTemperatureMax          := 0;
  fTorsionalStressTauStar  := 0;
  fTorsionalStressTauYield := 0;
  fTorsionalStressTauOE7   := 0;
  fTorsionalStressTauOE6   := 0;
  fTorsionalStressTauOE5   := 0;
  fTorsionalStressTauOE3   := 0;
  fTorsionalStressTauUE7   := 0;
  fTorsionalStressTauUE6   := 0;
  fTorsionalStressTauUE5   := 0;
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

function TMaterialDB.Search(const aID: string; const aWireDiameter, aTemperature: double; const aSurfaceTreatment: string): longint;
var
  i: longint;
begin
  Result := -1;
  for i := 0 to fMatFile.RowCount -1 do
    //2          3   	 4
    //TREATMENT  DMIN	 DMAX
    if (CompareText(aID, GetItem(i)) = 0) and (CompareText(aSurfaceTreatment, fMatFile.Cells[2, i]) = 0)  then
    begin
       if (aWireDiameter >  TryTextToFloat(fMatFile.Cells[3, i])) and
          (aWireDiameter <= TryTextToFloat(fMatFile.Cells[4, i])) then
       begin
         Result := i;
         Break;
       end;
    end;
end;

function TMaterialDB.SetItem(const aID: string; const aWireDiameter, aTemperature: double; const aSurfaceTreatment: string): longint;
var
  DT0, TO0, DTO0, TO1, DTO1, TU1, DTU1: double;
  RM0, DRM, DR0, RMMAX: double;
begin
  Clear;

  fItemIndex := Search(aID, aWireDiameter, aTemperature, aSurfaceTreatment);
  if fItemIndex <> -1 then
  begin
    //5	  6	7	8	9	10	11	12,0	13,0	14,0	15	16	17	18	19	20
    //E   G	RHO	RM0	DRM	DR0	RMMAX	DT0	TO0	DTO0	TO1	DTO1	TU1	DTU1	TMIN	TMAX

    fSurfaceTreatment     := fMatFile.Cells[2,  fItemIndex];
    fWireDiameter         := aWireDiameter;
    fYoungModulusE20      := TryTextToFloat(fMatFile.Cells[5,  fItemIndex]);
    fShearModulusG20      := TryTextToFloat(fMatFile.Cells[6,  fItemIndex]);
    fDensityRho           := TryTextToFloat(fMatFile.Cells[7,  fItemIndex]);

    RM0                   := TryTextToFloat(fMatFile.Cells[8,  fItemIndex]);
    DRM                   := TryTextToFloat(fMatFile.Cells[9,  fItemIndex]);
    DR0                   := TryTextToFloat(fMatFile.Cells[10, fItemIndex]);
    RMMAX                 := TryTextToFloat(fMatFile.Cells[11, fItemIndex]);
    fTensileStrengthRm    := Min(RMMAX, RM0-DRM*Log10(fWireDiameter/DR0));
    fTorsionalStressTauUT := fTensileStrengthRm*0.577;

    DT0                   := TryTextToFloat(fMatFile.Cells[12, fItemIndex]);
    TO0                   := TryTextToFloat(fMatFile.Cells[13, fItemIndex]);
    DTO0                  := TryTextToFloat(fMatFile.Cells[14, fItemIndex]);
    TO1                   := TryTextToFloat(fMatFile.Cells[15, fItemIndex]);
    DTO1                  := TryTextToFloat(fMatFile.Cells[16, fItemIndex]);
    TU1                   := TryTextToFloat(fMatFile.Cells[17, fItemIndex]);
    DTU1                  := TryTextToFloat(fMatFile.Cells[18, fItemIndex]);

    fNumOfCyclesE7        := TryTextToFloat(fMatFile.Cells[19, fItemIndex]); ;

    fTemperature          := aTemperature;
    fTemperatureMin       := TryTextToFloat(fMatFile.Cells[20, fItemIndex]);
    fTemperatureMax       := TryTextToFloat(fMatFile.Cells[21, fItemIndex]);

    fPoissonRatio         := fYoungModulusE20/(2*fShearModulusG20)-1;
    fShearModulusG        := GetG(fTemperature);
    fYoungModulusE        := GetE(fTemperature);

    if (DT0 > 0) and (fNumOfCyclesE7 > 0) then
    begin
      fTorsionalStressTauYield := TO1 - DTO1*Log10(aWireDiameter/DT0);
      fTorsionalStressTauOE7   := TO0 - DTO0*Log10(aWireDiameter/DT0);
      fTorsionalStressTauUE7   := TU1 - DTU1*Log10(aWireDiameter/DT0);
      fTorsionalStressTauOE3   := fTorsionalStressTauUT*0.95;
      fTorsionalStressTauStar  := fTorsionalStressTauOE7/(1 - ((fTorsionalStressTauYield - fTorsionalStressTauOE7)/fTorsionalStressTauUE7));

      fNumOfCyclesE6 := fNumOfCyclesE7/10;
      fNumOfCyclesE5 := fNumOfCyclesE7/100;
      fNumOfCyclesE3 := 1000;

      if ((fTorsionalStressTauOE7 > 0) and (fNumOfCyclesE7 > 0)) and
         ((fTorsionalStressTauOE3 > 0) and (fNumOfCyclesE3 > 0)) then
      begin
        fFatigueFactorB := LogN(fNumOfCyclesE7/fNumOfCyclesE3, fTorsionalStressTauOE7/fTorsionalStressTauOE3);
        fFatigueFactorA := fTorsionalStressTauOE3/Power(fNumOfCyclesE3, fFatigueFactorB);

        fTorsionalStressTauOE6 := fFatigueFactorA*Power(fNumOfCyclesE6, fFatigueFactorB);
        fTorsionalStressTauOE5 := fFatigueFactorA*Power(fNumOfCyclesE5, fFatigueFactorB);

        fTorsionalStressTauUE6 := fTorsionalStressTauStar*(fTorsionalStressTauYield - fTorsionalStressTauOE6)/
                                                          (fTorsionalStressTauStar  - fTorsionalStressTauOE6);

        fTorsionalStressTauUE5 := fTorsionalStressTauStar*(fTorsionalStressTauYield - fTorsionalStressTauOE5)/
                                                          (fTorsionalStressTauStar  - fTorsionalStressTauOE5);
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

