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

function TryFormatFloatSumDiv(const S1, S2 :string; const A0, A1, A2: double): string;
function TryFormatFloatDiv(const S1, S2 :string; const A1, A2: double): string;
function TryFormatFloat(const S1, S2 :string; const A1: double): string;

function TryFormatInt (const S1, S2 :string; Value: longint): string;
function TryFormatText(const S1, S2 :string; const Value: string): string;
function TryFormatBool(const S1, S2: string; Value: boolean): string;
function TryFloatToText(const Value: double): string;
function TryFloatToText(const Value: double; Precision, Digits: longint): string;

function TryTextToFloat(S: string): double;
function TryTextToInt(S: string): longint;


function GetSymbol(const AValue: TMeters): string;
function GetValue (const AValue: TMeters): double;
function TryFormat(const AValue, AToll: TMeters): string;
function TryFormat(const AValue: TMeters): string;
function TryFormat(const AValue: double): string;


function GetSymbol(const AValue: TNewtons): string;
function GetValue (const AValue: TNewtons): double;
function TryFormat(const AValue, AToll: TNewtons): string;
function TryFormat(const AValue: TNewtons): string;

function GetSymbol(const AValue: TPascals): string;
function GetValue (const AValue: TPascals): double;
function TryFormat(const AValue: TPascals): string;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
function GetValue (const AValue: TNewtonsPerMeter): double;
function TryFormat(const AValue: TNewtonsPerMeter): string;

function GetSymbol(const AValue: TKilograms): string;
function GetValue (const AValue: TKilograms): double;
function TryFormat(const AValue: TKilograms): string;

function GetSymbol(const AValue: TJoules): string;
function GetValue (const AValue: TJoules): double;
function TryFormat(const AValue: TJoules): string;

function GetSymbol(const AValue: THertz): string;
function GetValue (const AValue: THertz): double;
function TryFormat(const AValue: THertz): string;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
function GetValue (const AValue: TKilogramsPerCubicMeter): double;
function TryFormat(const AValue: TKilogramsPerCubicMeter): string;

function GetSymbol(const AValue: TRadians): string;
function GetValue (const AValue: TRadians): double;

function GetSymbol(const AValue: TKelvins): string;
function GetValue (const AValue: TKelvins): double;
function TryFormat(const AValue: TKelvins): string;

var
  ApplicationName: string;
  ApplicationVer:  string;

  ErrorMessage:    TStringList;
  WarningMessage:  TStringList;

  UseImperialSystem: boolean = False;

implementation

const
  S1 = '%s %s';
  S2 = '%s ± %s %s';

function TryFormat(const AValue: double): string;
begin
  Result := TryFloatToText(AValue);
end;

function GetSymbol(const AValue: TMeters): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TInchUnit.Symbol, []);
    False: result := ADim.GetSymbol(TMeterUnit.Symbol, [pMilli]);
  end;
end;

function GetValue(const AValue: TMeters): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToInch.Value;
    False: result := AValue.Value([pMilli]);
  end;
end;

function TryFormat(const AValue: TMeters): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function TryFormat(const AValue, AToll: TMeters): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S2, [TryFormat(GetValue(AValue)), TryFormat(GetValue(AToll)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TNewtons): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TPoundUnit.Symbol, []);
    False: result := ADim.GetSymbol(TNewtonUnit.Symbol, [pNone]);
  end;
end;

function GetValue(const AValue: TNewtons): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForce.Value;
    False: result := AValue.Value;
  end;
end;

function TryFormat(const AValue, AToll: TNewtons): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S2, [TryFormat(GetValue(AValue)), TryFormat(GetValue(AToll)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function TryFormat(const AValue: TNewtons): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TPascals): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TPoundPerSquareInchUnit.Symbol, [pKilo]);
    False: result := ADim.GetSymbol(TPascalUnit.Symbol, [pMega]);
  end;
end;

function GetValue(const AValue: TPascals): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundPerSquareInch.Value([pKilo]);
    False: result := AValue.Value([pMega]);
  end;
end;

function TryFormat(const AValue: TPascals): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
begin
  case UseImperialSystem of
    True:  result := Adim.GetSymbol(TPoundForcePerInchUnit.Symbol,[]);
    False: result := ADim.GetSymbol(TNewtonPerMeterUnit.Symbol, [pNone, pMilli]);
  end;
end;

function GetValue(const AValue: TNewtonsPerMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForcePerInch.Value;
    False: result := AValue.Value([pNone, pMilli]);
  end;
end;

function TryFormat(const AValue: TNewtonsPerMeter): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TKilograms): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TPoundUnit.Symbol, []);
    False: result := ADim.GetSymbol(TKilogramUnit.Symbol, [pNone]);
  end;
end;

function GetValue(const AValue: TKilograms): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPound.Value;
    False: result := AValue.Value;
  end;
end;

function TryFormat(const AValue: TKilograms): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TJoules): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TPoundForceInchUnit.Symbol, []);
    False: result := ADim.GetSymbol(TNewtonMeterUnit.Symbol, [pNone, pMilli]);
  end;
end;

function GetValue(const AValue: TJoules): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForceInch.Value;
    False: result := AValue.ToNewtonMeter.Value([pNone, pMilli]);
  end;
end;

function TryFormat(const AValue: TJoules): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: THertz): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(THertzUnit.Symbol, [pNone]);
    False: result := ADim.GetSymbol(THertzUnit.Symbol, [pNone]);
  end;
end;

function GetValue(const AValue: THertz): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function TryFormat(const AValue: THertz): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TKilogramPerCubicMeterUnit.Symbol, [pNone, pNone]);
    False: result := ADim.GetSymbol(TKilogramPerCubicMeterUnit.Symbol, [pNone, pNone]);
  end;
end;

function GetValue(const AValue: TKilogramsPerCubicMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function TryFormat(const AValue: TKilogramsPerCubicMeter): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

function GetSymbol(const AValue: TRadians): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TDegreeUnit.Symbol, []);
    False: result := ADim.GetSymbol(TDegreeUnit.Symbol, []);
  end;
end;

function GetValue(const AValue: TRadians): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToDegree.Value;
    False: result := AValue.ToDegree.Value;
  end;
end;

function GetSymbol(const AValue: TKelvins): string;
begin
  case UseImperialSystem of
    True:  result := ADim.GetSymbol(TDegreeFahrenheitUnit.Symbol, []);
    False: result := ADim.GetSymbol(TDegreeCelsiusUnit.Symbol, []);
  end;
end;

function GetValue(const AValue: TKelvins): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToDegreeFahrenheit.Value;
    False: result := AValue.ToDegreeCelsius.Value;
  end;
end;

function TryFormat(const AValue: TKelvins): string;
begin
  if AValue.Value <> 0 then
    Result := Format(S1, [TryFormat(GetValue(AValue)), GetSymbol(AValue)])
  else
    Result := Format(S1, ['---', GetSymbol(AValue)]);
end;

// ---

function TryFormatFloatSumDiv(const S1, S2 :string; const A0, A1, A2: double): string;
begin
  if A2 <> 0 then
    Result := Format(S1, [TryFloatToText(A0 + A1/A2)])
  else
    Result := S2;
end;

function TryFormatFloatDiv(const S1, S2 :string; const A1, A2: double): string;
begin
  if A2 <> 0 then
    Result := Format(S1, [TryFloatToText(A1/A2)])
  else
    Result := S2;
end;

function TryFormatFloat(const S1, S2 :string; const A1: double): string;
begin
  if A1 <> 0 then
    Result := Format(S1, [TryFloatToText(A1)])
  else
    Result := S2;
end;

function TryFormatInt(const S1, S2 :string; Value: longint): string;
begin
  if Value <> 0 then
    Result := Format(S1, [Value.ToString])
  else
    Result := S2;
end;

function TryFormatText(const S1, S2 :string; const Value: string): string;
begin
  if Value <> '' then
    Result := Format(S1, [Value])
  else
    Result := S2;
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

function TryTextToInt(S: string): longint;
begin
  Result := Trunc(TryTextToFloat(S));
end;

function TryFormatBool(const S1, S2: string; Value: boolean): string;
begin
  if Value then
    Result := S1
  else
    Result := S2;
end;

function TryFloatToText(const Value: double): string;
begin
  if Value >= 100000 then
    Result := FloatToStrF(Value, ffGeneral, 6, 0)
  else
    if Value >= 10000 then
      Result := FloatToStrF(Value, ffGeneral, 5, 0)
    else
      Result := FloatToStrF(Value, ffGeneral, 4, 0);
end;

function TryFloatToText(const Value: double; Precision, Digits: longint): string;
begin
  Result := FloatToStrF(Value, ffGeneral, Precision, Digits);
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

