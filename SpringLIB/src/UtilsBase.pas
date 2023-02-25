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
  Classes, Dim, SysUtils;

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
    True:  result := 'in';
    False: result := 'mm';
  end;
end;

function GetValue(const AValue: TMeters): double;
begin
  case UseImperialSystem of
    True:  result := mm.From(AValue).Value;
    False: result := mm.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TNewtons): string;
begin
  case UseImperialSystem of
    True:  result := 'lbf';
    False: result := 'N';
  end;
end;

function GetValue(const AValue: TNewtons): double;
begin
  case UseImperialSystem of
    True:  result := N.From(AValue).Value;
    False: result := N.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TPascals): string;
begin
  case UseImperialSystem of
    True:  result := 'psi';
    False: result := 'MPa';
  end;
end;

function GetValue(const AValue: TPascals): double;
begin
  case UseImperialSystem of
    True:  result := MPa.From(AValue).Value;
    False: result := MPa.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TNewtonsPerMeter): string;
begin
  case UseImperialSystem of
    True:  result := 'lbf/in';
    False: result := 'N/mm';
  end;
end;

function GetValue(const AValue: TNewtonsPerMeter): double;
begin
  case UseImperialSystem of
    True:  result := (N/mm).From(AValue).Value;
    False: result := (N/mm).From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TKilograms): string;
begin
  case UseImperialSystem of
    True:  result := 'lb';
    False: result := 'g';
  end;
end;

function GetValue(const AValue: TKilograms): double;
begin
  case UseImperialSystem of
    True:  result := g.From(AValue).Value;
    False: result := g.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TJoules): string;
begin
  case UseImperialSystem of
    True:  result := 'lb';
    False: result := 'J';
  end;
end;

function GetValue(const AValue: TJoules): double;
begin
  case UseImperialSystem of
    True:  result := J.From(AValue).Value;
    False: result := J.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: THertz): string;
begin
  case UseImperialSystem of
    True:  result := 'Hz';
    False: result := 'Hz';
  end;
end;

function GetValue(const AValue: THertz): double;
begin
  case UseImperialSystem of
    True:  result := Hz.From(AValue).Value;
    False: result := Hz.From(AValue).Value;
  end;
end;

function GetSymbol(const AValue: TKilogramsPerCubicMeter): string;
begin
  case UseImperialSystem of
    True:  result := 'kg/m3';
    False: result := 'kg/m3';
  end;
end;

function GetValue(const AValue: TKilogramsPerCubicMeter): double;
begin
  case UseImperialSystem of
    True:  result := (kg/m3).From(AValue).Value;
    False: result := (kg/m3).From(AValue).Value;
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

