{ Helical Compression Spring Designer

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

unit BaseUtils;

{$mode ObjFPC}{$H+}

interface

uses
  ADim, Classes, SysUtils;


type
  TGrade      = (SM, DM, SL, SH, DH, FDC, FDCrV, FDSiCr, FDSiCrV, TDC, TDCrV, TDSiCr, TDSiCrV, VDC, VDCrV, VDSiCr, VDSiCrV);
  TRegulation = (EN10270P1, EN10270P2, EN10089, EN10270P3, EN12166, EN15800);

function GetString(const AValue: double): string;

function GetLengthSymbol(const AQuantity: TQuantity): string;
function GetLengthValue (const AQuantity: TQuantity): double;
function GetLengthString(const AQuantity: TQuantity): string;
function GetLengthString(const AQuantity, ATol: TQuantity): string;

function GetForceSymbol(const AQuantity: TQuantity): string;
function GetForceValue (const AQuantity: TQuantity): double;
function GetForceString(const AQuantity: TQuantity): string;
function GetForceString(const AQuantity, ATol: TQuantity): string;

function GetPressureSymbol(const AQuantity: TQuantity): string;
function GetPressureValue (const AQuantity: TQuantity): double;
function GetPressureString(const AQuantity: TQuantity): string;

function GetStiffnessSymbol(const AQuantity: TQuantity): string;
function GetStiffnessValue (const AQuantity: TQuantity): double;
function GetStiffnessString(const AQuantity: TQuantity): string;

function GetAngularStiffnessSymbol(const AQuantity: TQuantity): string;
function GetAngularStiffnessValue (const AQuantity: TQuantity): double;
function GetAngularStiffnessString(const AQuantity: TQuantity): string;

function GetMassSymbol(const AQuantity: TQuantity): string;
function GetMassValue (const AQuantity: TQuantity): double;
function GetMassString(const AQuantity: TQuantity): string;

function GetEnergySymbol(const AQuantity: TQuantity): string;
function GetEnergyValue (const AQuantity: TQuantity): double;
function GetEnergyString(const AQuantity: TQuantity): string;

function GetFrequencySymbol(const AQuantity: TQuantity): string;
function GetFrequencyValue (const AQuantity: TQuantity): double;
function GetFrequencyString(const AQuantity: TQuantity): string;

function GetDensitySymbol(const AQuantity: TQuantity): string;
function GetDensityValue (const AQuantity: TQuantity): double;
function GetDensityString(const AQuantity: TQuantity): string;

function GetAngleSymbol(const AQuantity: TQuantity): string;
function GetAngleValue (const AQuantity: TQuantity): double;
function GetAngleString(const AQuantity: TQuantity): string;

function GetTorqueSymbol(const AQuantity: TQuantity): string;
function GetTorqueValue (const AQuantity: TQuantity): double;
function GetTorqueString(const AQuantity: TQuantity): string;

function GetTemperatureSymbol(const AQuantity: TQuantity): string;
function GetTemperatureValue (const AQuantity: TQuantity): double;
function GetTemperatureString(const AQuantity: TQuantity): string;

const
  DefaultDigits    = 5;
  DefaultPrecision = 5;

  ApplicationName  = 'SpringONE' ;
  ApplicationVer   = 'SpringONE 0.50';
  ApplicationBuild = 'Build 20250212';

var
  ErrorMessage:    TStringList;
  WarningMessage:  TStringList;

  UseImperialSystem: boolean = False;


implementation

function GetString(const AValue: double): string;
begin
  if AValue >= 100000 then
    Result := FloatToStrF(AValue, ffGeneral, 6, 0)
  else
    if AValue >= 10000 then
      Result := FloatToStrF(AValue, ffGeneral, 5, 0)
    else
      Result := FloatToStrF(AValue, ffGeneral, 4, 0);
end;

function GetLengthSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := InchUnit.GetSymbol([]);
    False: result := MeterUnit.GetSymbol([pMilli]);
  end;
end;

function GetLengthValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := InchUnit.ToFloat(AQuantity);
    False: result := MeterUnit.ToFloat(AQuantity, [pMilli]);
  end;
end;

function GetLengthString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := InchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := MeterUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pMilli]);
    end
  else
    result := '---';
end;

function GetLengthString(const AQuantity, ATol: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := InchUnit.ToString(AQuantity, ATol, DefaultPrecision, DefaultDigits, []);
      False: result := Meterunit.ToString(AQuantity, ATol, DefaultPrecision, DefaultDigits, [pMilli]);
    end
  else
    result := '---';
end;

function GetForceSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundForceUnit.GetSymbol([]);
    False: result := NewtonUnit.GetSymbol([]);
  end;
end;

function GetForceValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundForceUnit.ToFloat(AQuantity);
    False: result := NewtonUnit.ToFloat(AQuantity);
  end;
end;

function GetForceString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundForceUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := NewtonUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
    end
  else
    result := '---';
end;

function GetForceString(const AQuantity, ATol: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundForceUnit.ToString(AQuantity, ATol, DefaultPrecision, DefaultDigits, []);
      False: result := NewtonUnit.ToString(AQuantity, ATol, DefaultPrecision, DefaultDigits, []);
    end
  else
    result := '---';
end;

function GetPressureSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundPerSquareInchUnit.GetSymbol([pKilo]);
    False: result := PascalUnit.GetSymbol([pMega]);
  end;
end;

function GetPressureValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundPerSquareInchUnit.ToFloat(AQuantity, [pKilo]);
    False: result := PascalUnit.ToFloat(AQuantity, [pMega]);
  end;
end;

function GetPressureString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundPerSquareInchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := PascalUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pMega]);
    end
  else
    Result := '---';
end;

function GetStiffnessSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundForcePerInchUnit.GetSymbol([]);
    False: result := NewtonPerMeterUnit.GetSymbol([pNone, pMilli]);
  end;
end;

function GetStiffnessValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundForcePerInchUnit.ToFloat(AQuantity);
    False: result := NewtonPerMeterUnit.ToFloat(AQuantity, [pNone, pMilli]);
  end;
end;

function GetStiffnessString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  Result := PoundForcePerInchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: Result := NewtonPerMeterUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pNone, pMilli]);
    end
  else
    Result := '---';
end;

function GetAngularStiffnessSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.GetSymbol([]);
    False: result := NewtonMeterUnit.GetSymbol([pNone, pMilli]);
  end;
end;

function GetAngularStiffnessValue (const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.ToFloat(AQuantity);
    False: result := NewtonMeterUnit.ToFloat(AQuantity, [pNone, pMilli]);
  end;
end;

function GetAngularStiffnessString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  Result := PoundForceInchPerDegreeUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: Result := NewtonMeterPerDegreeUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pNone, pMilli]);
    end
  else
    Result := '---';
end;

function GetMassSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundUnit.GetSymbol([]);
    False: result := KilogramUnit.GetSymbol([pNone]);
  end;
end;

function GetMassValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundUnit.ToFloat(AQuantity);
    False: result := KilogramUnit.ToFloat(AQuantity);
  end;
end;

function GetMassString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := KilogramUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pNone]);
    end
  else
    Result := '---';
end;

function GetEnergySymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.GetSymbol([]);
    False: result := NewtonMeterUnit.GetSymbol([pNone, pMilli]);
  end;
end;

function GetEnergyValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.ToFloat(AQuantity);
    False: result := NewtonMeterUnit.Tofloat(AQuantity, [pNone, pMilli]);
  end;
end;

function GetEnergyString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundForceInchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := NewtonMeterUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
    end
  else
    Result := '---';
end;

function GetFrequencySymbol(const AQuantity: TQuantity): string;
begin
  result := HertzUnit.GetSymbol([pNone]);
end;

function GetFrequencyValue(const AQuantity: TQuantity): double;
begin
 result := HertzUnit.ToFloat(AQuantity);
end;

function GetFrequencyString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
  begin
    result := HertzUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
  end else
    Result := '---';
end;

function GetDensitySymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundPerCubicInchUnit.GetSymbol([pNone, pNone]);
    False: result := KilogramPerCubicMeterUnit.GetSymbol([pNone, pNone]);
  end;
end;

function GetDensityValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundPerCubicInchUnit.ToFloat(AQuantity);
    False: result := KilogramPerCubicMeterUnit.ToFloat(AQuantity);
  end;
end;

function GetDensityString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundPerCubicInchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := KilogramPerCubicMeterUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, [pNone, pDeci]);
    end
  else
    result := '---';
end;

function GetAngleSymbol(const AQuantity: TQuantity): string;
begin
  result := DegreeUnit.GetSymbol([]);
end;

function GetAngleValue(const AQuantity: TQuantity): double;
begin
  result := DegreeUnit.ToFloat(AQuantity);
end;

function GetAngleString(const AQuantity: TQuantity): string;
begin
  result := DegreeUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
end;

function GetTorqueSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.GetSymbol([]);
    False: result := NewtonMeterUnit.GetSymbol([pNone, pMilli]);
  end;
end;

function GetTorqueValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := PoundForceInchUnit.ToFloat(AQuantity);
    False: result := NewtonMeterUnit.Tofloat(AQuantity, [pNone, pMilli]);
  end;
end;

function GetTorqueString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := PoundForceInchUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := NewtonMeterUnit.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
    end
  else
    Result := '---';
end;

function GetTemperatureSymbol(const AQuantity: TQuantity): string;
begin
  case UseImperialSystem of
    True:  result := degF.GetSymbol([]);
    False: result := degC.GetSymbol([]);
  end;
end;

function GetTemperatureValue(const AQuantity: TQuantity): double;
begin
  case UseImperialSystem of
    True:  result := degF.ToFloat(AQuantity);
    False: result := degC.ToFloat(AQuantity);
  end;
end;

function GetTemperatureString(const AQuantity: TQuantity): string;
begin
  if not EqualToZero(AQuantity) then
    case UseImperialSystem of
      True:  result := degF.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
      False: result := degC.ToString(AQuantity, DefaultPrecision, DefaultDigits, []);
    end
  else
    Result := '---';
end;


function TryTextToFloat(S: string): double;
var
  i: longint;
begin
  for i := 1 to Length(S) do
    if S[i] in ['.',','] then
    begin
      S[i] := DefaultFormatSettings.DecimalSeparator;
    end;

  if not TryStrToFloat(S, Result) then
  begin
    Result := 0;
  end;
end;

function TryFormatBool(const S1, S2: string; Value: boolean): string;
begin
  if Value then
    Result := S1
  else
    Result := S2;
end;


initialization
begin
  ErrorMessage   := TStringList.Create;
  WarningMessage := TStringList.Create;
end;

finalization
begin
  WarningMessage.Destroy;
  ErrorMessage.Destroy;
end;

end.

