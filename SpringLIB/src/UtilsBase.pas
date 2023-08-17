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

function GetSymbol(const AValue: TNewtons): string;
function GetValue (const AValue: TNewtons): double;

function GetSymbol(const AValue: TPascals): string;
function GetValue (const AValue: TPascals): double;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
function GetValue (const AValue: TNewtonsPerMeter): double;

function GetSymbol(const AValue: TKilograms): string;
function GetValue (const AValue: TKilograms): double;

function GetSymbol(const AValue: TJoules): string;
function GetValue (const AValue: TJoules): double;

function GetSymbol(const AValue: THertz): string;
function GetValue (const AValue: THertz): double;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
function GetValue (const AValue: TKilogramsPerCubicMeter): double;

function GetSymbol(const AValue: TRadians): string;
function GetValue (const AValue: TRadians): double;


var
  ApplicationName: string;
  ApplicationVer:  string;

  ErrorMessage:    TStringList;
  WarningMessage:  TStringList;

  UseImperialSystem: boolean = False;

implementation

function GetSymbol(const AValue: TMeters): string;
begin
  case UseImperialSystem of
    True:  result := TInchUnit.GetSymbol([]);
    False: result := TMeterUnit.GetSymbol([pMilli]);
  end;
end;

function GetValue(const AValue: TMeters): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToInch.Value;
    False: result := AValue.Value([pMilli]);
  end;
end;

function GetSymbol(const AValue: TNewtons): string;
begin
  case UseImperialSystem of
    True:  result := TPoundUnit.GetSymbol([]);
    False: result := TNewtonUnit.GetSymbol([]);
  end;
end;

function GetValue(const AValue: TNewtons): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForce.Value;
    False: result := AValue.Value;
  end;
end;

function GetSymbol(const AValue: TPascals): string;
begin
  case UseImperialSystem of
    True:  result := TPoundPerSquareInchUnit.GetSymbol([]);
    False: result := TPascalUnit.GetSymbol([pMega]);
  end;
end;

function GetValue(const AValue: TPascals): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundPerSquareInch.Value;
    False: result := AValue.Value([pMega]);
  end;
end;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
begin
  case UseImperialSystem of
    True:  result := TPoundForcePerInchUnit.GetSymbol([]);
    False: result := TNewtonPerMeterUnit.GetSymbol([pNone, pMilli]);
  end;
end;

function GetValue(const AValue: TNewtonsPerMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForcePerInch.Value;
    False: result := AValue.Value([pNone, pMilli]);
  end;
end;

function GetSymbol(const AValue: TKilograms): string;
begin
  case UseImperialSystem of
    True:  result := TPoundUnit.GetSymbol([]);
    False: result := TKilogramUnit.GetSymbol([pNone]);
  end;
end;

function GetValue(const AValue: TKilograms): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPound.Value;
    False: result := AValue.Value;
  end;
end;

function GetSymbol(const AValue: TJoules): string;
begin
  case UseImperialSystem of
    True:  result := TPoundForceInchUnit.GetSymbol([]);
    False: result := TNewtonMeterUnit.GetSymbol([pMilli]);
  end;
end;

function GetValue(const AValue: TJoules): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToPoundForceInch.Value;
    False: result := AValue.ToNewtonMeter.Value([pNone, pMilli]);
  end;
end;

function GetSymbol(const AValue: THertz): string;
begin
  case UseImperialSystem of
    True:  result := THertzUnit.GetSymbol([]);
    False: result := THertzUnit.GetSymbol([]);
  end;
end;

function GetValue(const AValue: THertz): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
begin
  case UseImperialSystem of
    True:  result := TKilogramPerCubicMeterUnit.GetSymbol([pNone, pNone]);
    False: result := TKilogramPerCubicMeterUnit.GetSymbol([pNone, pNone]);
  end;
end;

function GetValue(const AValue: TKilogramsPerCubicMeter): double;
begin
  case UseImperialSystem of
    True:  result := AValue.Value;
    False: result := AValue.Value;
  end;
end;

function GetSymbol(const AValue: TRadians): string;
begin
  case UseImperialSystem of
    True:  result := TDegreeUnit.GetSymbol([]);
    False: result := TDegreeUnit.GetSymbol([]);
  end;
end;

function GetValue(const AValue: TRadians): double;
begin
  case UseImperialSystem of
    True:  result := AValue.ToDegree.Value;
    False: result := AValue.ToDegree.Value;
  end;
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

