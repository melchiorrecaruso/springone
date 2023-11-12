{ Helical Compression Spring Designer

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

unit UtilsBase;

{$mode ObjFPC}{$H+}

interface

uses
  ADim, Classes, SysUtils;

function GetString(const AValue: double): string;

function GetSymbol(const AValue: TMeters): string;
function GetValue (const AValue: TMeters): double;
function GetString(const AValue: TMeters): string;
function GetString(const AValue, ATol: TMeters): string;

function GetSymbol(const AValue: TNewtons): string;
function GetValue (const AValue: TNewtons): double;
function GetString(const AValue: TNewtons): string;
function GetString(const AValue, ATol: TNewtons): string;

function GetSymbol(const AValue: TPascals): string;
function GetValue (const AValue: TPascals): double;
function GetString(const AValue: TPascals): string;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
function GetValue (const AValue: TNewtonsPerMeter): double;
function GetString(const AValue: TNewtonsPerMeter): string;

function GetSymbol(const AValue: TKilograms): string;
function GetValue (const AValue: TKilograms): double;
function GetString(const AValue: TKilograms): string;

function GetSymbol(const AValue: TJoules): string;
function GetValue (const AValue: TJoules): double;
function GetString(const AValue: TJoules): string;

function GetSymbol(const AValue: THertz): string;
function GetValue (const AValue: THertz): double;
function GetString(const AValue: THertz): string;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
function GetValue (const AValue: TKilogramsPerCubicMeter): double;
function GetString(const AValue: TKilogramsPerCubicMeter): string;

function GetSymbol(const AValue: TRadians): string;
function GetValue (const AValue: TRadians): double;
function GetString(const AValue: TRadians): string;

function GetSymbol(const AValue: TKelvins): string;
function GetValue (const AValue: TKelvins): double;
function GetString(const AValue: TKelvins): string;


const
  DefaultDigits    = 5;
  DefaultPrecision = 5;


var
  ApplicationName: string;
  ApplicationVer:  string;

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

function GetSymbol(const AValue: TMeters): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsInchSymbol, []);
    False: result := ADim.GetSymbol(rsMeterSymbol, [pMilli]);
  end;
end;

function GetValue(const AValue: TMeters): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToInch.Value;
    False: result := AValue.Value([pMilli]);
  end;
end;

function GetString(const AValue: TMeters): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToInch.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, [pMilli]);
    end
  else
    result := '---';
end;

function GetString(const AValue, ATol: TMeters): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToInch.ToString(ATol.ToInch, DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(ATol, DefaultPrecision, DefaultDigits, [pMilli]);
    end
  else
    result := '---';
end;

function GetSymbol(const AValue: TNewtons): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsPoundSymbol, []);
    False: result := ADim.GetSymbol(rsNewtonSymbol, [pNone]);
  end;
end;

function GetValue(const AValue: TNewtons): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForce.Value;
    False: result := AValue.Value;
  end;
end;

function GetString(const AValue: TNewtons): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPoundForce.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, []);
    end
  else
    result := '---';
end;

function GetString(const AValue, ATol: TNewtons): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPoundForce.ToString(ATol.ToPoundForce, DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(ATol, DefaultPrecision, DefaultDigits, []);
    end
  else
    result := '---';
end;

function GetSymbol(const AValue: TPascals): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsPoundPerSquareInchSymbol, [pKilo]);
    False: result := ADim.GetSymbol(rsPascalSymbol, [pMega]);
  end;
end;

function GetValue(const AValue: TPascals): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundPerSquareInch.Value([pKilo]);
    False: result := AValue.Value([pMega]);
  end;
end;

function GetString(const AValue: TPascals): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPoundPerSquareInch.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, [pMega]);
    end
  else
    Result := '---';
end;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
begin
  case UseImperialSystem of
    True:  result := Adim.GetSymbol(rsPoundForcePerInchSymbol,[]);
    False: result := ADim.GetSymbol(rsNewtonPerMeterSymbol, [pNone, pMilli]);
  end;
end;

function GetValue(const AValue: TNewtonsPerMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForcePerInch.Value;
    False: result := AValue.Value([pNone, pMilli]);
  end;
end;

function GetString(const AValue: TNewtonsPerMeter): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  Result := AValue.ToPoundForcePerInch.ToString(DefaultPrecision, DefaultDigits, []);
      False: Result := AValue.ToString(DefaultPrecision, DefaultDigits, [pNone, pMilli]);
    end
  else
    Result := '---';
end;

function GetSymbol(const AValue: TKilograms): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsPoundSymbol, []);
    False: result := ADim.GetSymbol(rsKilogramSymbol, [pNone]);
  end;
end;

function GetValue(const AValue: TKilograms): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPound.Value;
    False: result := AValue.Value;
  end;
end;

function GetString(const AValue: TKilograms): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPound.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, [pNone]);
    end
  else
    Result := '---';
end;

function GetSymbol(const AValue: TJoules): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsPoundForceInchSymbol, []);
    False: result := ADim.GetSymbol(rsNewtonMeterSymbol, [pNone, pMilli]);
  end;
end;

function GetValue(const AValue: TJoules): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForceInch.Value;
    False: result := AValue.ToNewtonMeter.Value([pNone, pMilli]);
  end;
end;

function GetString(const AValue: TJoules): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPoundForceInch.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, []);
    end
  else
    Result := '---';
end;

function GetSymbol(const AValue: THertz): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsHertzSymbol, [pNone]);
    False: result := ADim.GetSymbol(rsHertzSymbol, [pNone]);
  end;
end;

function GetValue(const AValue: THertz): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function GetString(const AValue: THertz): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, []);
    end
  else
    Result := '---';
end;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsKilogramPerCubicMeterSymbol, [pNone, pNone]);
    False: result := ADim.GetSymbol(rsKilogramPerCubicMeterSymbol, [pNone, pNone]);
  end;
end;

function GetValue(const AValue: TKilogramsPerCubicMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function GetString(const AValue: TKilogramsPerCubicMeter): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToPoundPerCubicInch.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToString(DefaultPrecision, DefaultDigits, [pNone, pDeci]);
    end
  else
    result := '---';
end;

function GetSymbol(const AValue: TRadians): string;
begin
  result := ADim.GetSymbol(rsDegreeSymbol, []);
end;

function GetValue(const AValue: TRadians): double;
begin
  result := AValue.ToDegree.Value;
end;

function GetString(const AValue: TRadians): string;
begin
  result := AValue.ToString(DefaultPrecision, DefaultDigits, []);
end;

function GetSymbol(const AValue: TKelvins): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(rsDegreeFahrenheitSymbol, []);
    False: result := ADim.GetSymbol(rsDegreeCelsiusSymbol, []);
  end;
end;

function GetValue(const AValue: TKelvins): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToDegreeFahrenheit.Value;
    False: result := AValue.ToDegreeCelsius.Value;
  end;
end;

function GetString(const AValue: TKelvins): string;
begin
  if AValue.Value <> 0 then
    case UseImperialSystem of
      True:  result := AValue.ToDegreeFahrenheit.ToString(DefaultPrecision, DefaultDigits, []);
      False: result := AValue.ToDegreeCelsius.ToString(DefaultPrecision, DefaultDigits, []);
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

