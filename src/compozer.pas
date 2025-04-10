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

unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  ADim, BGRABitmap, BGRABitmapTypes, Classes, DateUtils, BaseGraphics,
  Graphics, IniFiles, LibLink, SysUtils, Dialogs;

type
  TCompozerMode = (cmQuick1, cmQuick2, cmQuick3, cmMessages, cmForceChart, cmGoodmanChart,
                   cmBucklingChart, cmLoadF1Chart, cmLoadF2Chart, cmShearModulusChart,
                   cmYoungModulusChart);

type
  TCompozer = class
  private
    FSetting: TIniFile;
    function  LoadFontStyle(const ASection, AIdent: string): TFontStyles;
    function  LoadPenStyle(const ASection, AIdent: string): TPenStyle;
    procedure LoadChart1(Chart: TChart; const ASection: string);
    procedure LoadChart2(Chart: TChart; const ASection, AIdent: string);
    procedure LoadTable(Table: TReportTable; const ASection, AIdent: string);
    procedure LoadSpring(Spring: TSpringDrawing; const ASection, AIdent: string);

    procedure DrawQuickX(var aScreen: TBGRABitmap; const aScreenScale: double; X: longint);
  public

    procedure DrawQuick1(var aScreen: TBGRABitmap; const aScreenScale: double);
    procedure DrawQuick2(var aScreen: TBGRABitmap; const aScreenScale: double);
    procedure DrawQuick3(var aScreen: TBGRABitmap; const aScreenScale: double);
    procedure DrawMessageList(var aScreen: TBGRABitmap; const aScreenScale: double);

    function CreateForceDisplacementChart(const aScreenScale: double): TChart;
    function CreateGoodmanChart(const aScreenScale: double): TChart;
    function CreateBucklingChart(const AScreenScale: double): TChart;
    function CreateLoadF1Chart(const AScreenScale: double): TChart;
    function CreateLoadF2Chart(const AScreenScale: double): TChart;
    function CreateShearModulusChart(const AScreenScale: double): TChart;
    function CreateYoungModulusChart(const AScreenScale: double): TChart;

    function CreateSpringDrawing(const AScreenScale: double): TSpringDrawing;
    function CreateQualityTable(const AScreenScale: double): TReportTable;
    function CreateQuick1Table(const AScreenScale: double): TReportTable;
    function CreateQuick1List1(const AScreenScale: double): TReportTable;
    function CreateQuick1List2(const AScreenScale: double): TReportTable;
    function CreateMessageList(const AScreenScale: double): TReportTable;
    function CreateQuick1AList(const AScreenScale: double): TReportTable;

    constructor Create(ASetting: TIniFile);
    destructor Destroy; override;
  end;


implementation

uses
  springmaterials, springtolerances, baseutils;

// TCompozer //

constructor TCompozer.Create(ASetting: TIniFile);
begin
  inherited Create;
  FSetting := ASetting;
end;

destructor TCompozer.Destroy;
begin
  inherited Destroy;
end;

function TCompozer.LoadFontStyle(const ASection, AIdent: string): TFontStyles;
var
  FontStyle: string;
begin
  Result := [];
  FontStyle := FSetting.ReadString(ASection, AIdent, 'Bold');
  if Pos('Bold', FontStyle) > 0 then Include(Result, fsBold);
  if Pos('Italic', FontStyle) > 0 then Include(Result, fsItalic);
  if Pos('Underline', FontStyle) > 0 then Include(Result, fsUnderline);
  if Pos('StrikeOut', FontStyle) > 0 then Include(Result, fsStrikeOut);
end;

function TCompozer.LoadPenStyle(const ASection, AIdent: string): TPenStyle;
var
  PenStyle: string;
begin
  Result := psSolid;
  PenStyle := FSetting.ReadString(ASection, AIdent, 'Solid');
  if PenStyle = 'Solid' then Result := psSolid;
  if PenStyle = 'Dash' then Result := psDash;
  if PenStyle = 'Dot' then Result := psDot;
  if PenStyle = 'DashDot' then Result := psDashDot;
  if PenStyle = 'DashDotDot' then Result := psDashDotDot;
  if PenStyle = 'InsideFrame' then Result := psInsideFrame;
  if PenStyle = 'Pattern' then Result := psPattern;
  if PenStyle = 'Clear' then Result := psClear;
end;

procedure TCompozer.LoadChart1(Chart: TChart; const ASection: string);
begin
  // Title
  Chart.TitleFontName := FSetting.ReadString(ASection, 'TitleFontName', 'default');
  Chart.TitleFontHeight := FSetting.ReadFloat(ASection, 'TitleFontHeight', 14);
  Chart.TitleFontStyle := LoadFontStyle(ASection, 'TitleFontStyle');
  Chart.TitleFontColor.FromString(FSetting.ReadString(ASection,
    'TitleFontColor', 'Black'));
  // X Axis
  Chart.XAxisFontName := FSetting.ReadString(ASection, 'XAxisFontName', 'default');
  Chart.XAxisFontHeight := FSetting.ReadFloat(ASection, 'XAxisFontHeight', 13);
  Chart.XAxisFontStyle := LoadFontStyle(ASection, 'XAxisFontStyle');
  Chart.XAxisFontColor.FromString(FSetting.ReadString(ASection,
    'XAxisFontColor', 'Black'));
  Chart.XAxisLineColor.FromString(FSetting.ReadString(ASection,
    'XAxisLineColor', 'Black'));
  Chart.XGridLineColor.FromString(FSetting.ReadString(ASection,
    'XGridLineColor', 'Silver'));
  Chart.XAxisLineWidth := FSetting.ReadFloat(ASection, 'XAxisLineWidth', 1.0);
  Chart.XGridLineWidth := FSetting.ReadFloat(ASection, 'XGridLineWidth', 1.0);
  Chart.XAxisLineStyle := LoadPenStyle(ASection, 'XAxisLineStyle');
  Chart.XGridLineStyle := LoadPenStyle(ASection, 'XGridLineStyle');
  // Y Axis
  Chart.YAxisFontName := FSetting.ReadString(ASection, 'YAxisFontName', 'default');
  Chart.YAxisFontHeight := FSetting.ReadFloat(ASection, 'YAxisFontHeight', 13);
  Chart.YAxisFontStyle := LoadFontStyle(ASection, 'YAxisFontStyle');
  Chart.YAxisFontColor.FromString(FSetting.ReadString(ASection,
    'YAxisFontColor', 'Black'));
  Chart.YAxisLineColor.FromString(FSetting.ReadString(ASection,
    'YAxisLineColor', 'Black'));
  Chart.YGridLineColor.FromString(FSetting.ReadString(ASection,
    'YGridLineColor', 'Silver'));
  Chart.YAxisLineWidth := FSetting.ReadFloat(ASection, 'YAxisLineWidth', 1.0);
  Chart.YGridLineWidth := FSetting.ReadFloat(ASection, 'YGridLineWidth', 1.0);
  Chart.YAxisLineStyle := LoadPenStyle(ASection, 'YAxisLineStyle');
  Chart.YGridLineStyle := LoadPenStyle(ASection, 'YGridLineStyle');
  // General
  Chart.Color.FromString(FSetting.ReadString(ASection, 'Color', 'White'));
  Chart.BackgroundColor.FromString(FSetting.ReadString(ASection,
    'BackgroundColor', 'White'));
end;

procedure TCompozer.LoadChart2(Chart: TChart; const ASection, AIdent: string);
begin
  Chart.FontName := FSetting.ReadString(ASection, AIdent + '.FontName', 'default');
  Chart.FontHeight := FSetting.ReadFloat(ASection, AIdent + '.FontHeight', 12);
  Chart.FontStyle := LoadFontStyle(ASection, AIdent + '.FontStyle');
  Chart.FontColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.FontColor', 'Black'));

  Chart.PenWidth := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Chart.PenStyle := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Chart.PenColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.PenColor', 'Black'));

  Chart.TextureBackgroundColor.FromString(
    FSetting.ReadString(ASection, AIdent + '.TextureBackgroundColor', 'White'));

  Chart.TextureColor.FromString(
    FSetting.ReadString(ASection, AIdent + '.TextureColor', 'Black'));

  Chart.TextureWidth := FSetting.ReadInteger(ASection, AIdent + '.TextureWidth', 8);
  Chart.TextureHeight := FSetting.ReadInteger(ASection, AIdent + '.TextureHeight', 8);
  Chart.TexturePenWidth := FSetting.ReadFloat(ASection, AIdent +
    '.TexturePenWidth', 1.0);
end;

procedure TCompozer.LoadTable(Table: TReportTable; const ASection, AIdent: string);
begin
  Table.BackgroundColor.FromString(FSetting.ReadString(ASection,
    AIdent + '.BackgroundColor', 'White'));
  Table.BorderWidth := FSetting.ReadInteger(ASection, AIdent + '.BorderWidth', 0);

  Table.FontName := FSetting.ReadString(ASection, AIdent + '.FontName', 'default');
  Table.FontHeight := FSetting.ReadFloat(ASection, AIdent + '.FontHeight', 12);
  Table.FontStyle := LoadFontStyle(ASection, AIdent + '.FontStyle');
  Table.FontColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.FontColor', 'Black'));
  Table.PenWidth := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Table.PenStyle := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Table.PenColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.PenColor', 'Black'));

  Table.RowSpacer := FSetting.ReadInteger(ASection, AIdent + '.RowSpacer', 0);
  Table.ColumnSpacer := FSetting.ReadInteger(ASection, AIdent + '.ColumnSpacer', 0);
end;

procedure TCompozer.LoadSpring(Spring: TSpringDrawing;
  const ASection, AIdent: string);
begin
  Spring.BackgroundColor.FromString(FSetting.ReadString(ASection,
    AIdent + '.BackgroundColor', 'White'));
  Spring.CenterLineColor.FromString(FSetting.ReadString(ASection,
    AIdent + '.CenterLineColor', 'Black'));
  Spring.CenterLineStyle := LoadPenStyle(ASection, AIdent + '.CenterLineStyle');
  Spring.CenterLineWidth := FSetting.ReadFloat(ASection, AIdent +
    '.CenterLineWidth', 1.0);

  Spring.FontColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.FontColor', 'Black'));
  Spring.FontName := FSetting.ReadString(ASection, AIdent + '.FontName', 'default');
  Spring.FontHeight := FSetting.ReadFloat(ASection, AIdent + '.FontHeight', 13);
  Spring.FontStyle := LoadFontStyle(ASection, AIdent);

  Spring.PenColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.PenColor', 'Black'));
  Spring.PenStyle := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Spring.PenWidth := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);

  Spring.TextureBackgroundColor.FromString(FSetting.ReadString(ASection,
    AIdent + '.TextureBackgroundColor', 'White'));
  Spring.TextureColor.FromString(FSetting.ReadString(ASection, AIdent +
    '.TextureColor', 'Black'));
  Spring.TextureHeight := FSetting.ReadInteger(ASection, AIdent + '.TextureHeight', 8);
  Spring.TextureWidth := FSetting.ReadInteger(ASection, AIdent + '.TextureWidth', 8);
  Spring.TexturePenWidth := FSetting.ReadFloat(ASection, AIdent +
    '.TexturePenWidth', 1.0);
end;

// ---

function TCompozer.CreateForceDisplacementChart(const AScreenScale: double): TChart;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;

  {$IFDEF MODULE1}
  Result.Title := 'Force & Displacement Chart';
  Result.XAxisLabel := 's [' + GetLengthSymbol(SpringSolver.LengthLc) + ']';
  Result.YAxisLabel := 'F [' + GetForceSymbol(SpringSolver.LoadFc) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Drawing bisector line
  if GreaterThanZero(SpringSolver.LoadFc) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetLengthValue(SpringSolver.StrokeSc);
    Points[1].Y := GetForceValue(SpringSolver.LoadFc);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing tolerance lines
  if GreaterThanZero(SpringTolerance.Load1) and
     GreaterThanZero(SpringTolerance.Load2) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'ToleranceLine');
    SetLength(Points, 2);
    Points[0].X := GetLengthValue(SpringSolver.StrokeS1);
    Points[0].Y := GetForceValue(SpringTolerance.Load1 + SpringTolerance.ToleranceOnLoad1);
    Points[1].X := GetLengthValue(SpringSolver.StrokeS2);
    Points[1].Y := GetForceValue(SpringTolerance.Load2 + SpringTolerance.ToleranceOnLoad2);
    Result.AddPolyLine(Points, True, 'Tolerance +');
    Points[0].X := GetLengthValue(SpringSolver.StrokeS1);
    Points[0].Y := GetForceValue(SpringTolerance.Load1 - SpringTolerance.ToleranceOnLoad1);
    Points[1].X := GetLengthValue(SpringSolver.StrokeS2);
    Points[1].Y := GetForceValue(SpringTolerance.Load2 - SpringTolerance.ToleranceOnLoad2);
    Result.AddPolyLine(Points, True, 'Tolerance -');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load-F1
  if GreaterThanZero(SpringSolver.LoadF1) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetForceValue(SpringSolver.LoadF1);
    Points[1].X := GetLengthValue(SpringSolver.StrokeS1);
    Points[1].Y := GetForceValue(SpringSolver.LoadF1);
    Points[2].X := GetLengthValue(SpringSolver.StrokeS1);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F1');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load F2
  if GreaterThanZero(SpringSolver.LoadF2) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetForceValue(SpringSolver.LoadF2);
    Points[1].X := GetLengthValue(SpringSolver.StrokeS2);
    Points[1].Y := GetForceValue(SpringSolver.LoadF2);
    Points[2].X := GetLengthValue(SpringSolver.StrokeS2);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load Fn
  if GreaterThanZero(SpringSolver.LoadFn) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetForceValue(SpringSolver.LoadFn);
    Points[1].X := GetLengthValue(SpringSolver.StrokeSn);
    Points[1].Y := GetForceValue(SpringSolver.LoadFn);
    Points[2].X := GetLengthValue(SpringSolver.StrokeSn);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fn');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load Fc
  if GreaterThanZero(SpringSolver.LoadFc) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetForceValue(SpringSolver.LoadFc);
    Points[1].X := GetLengthValue(SpringSolver.StrokeSc);
    Points[1].Y := GetForceValue(SpringSolver.LoadFc);
    Points[2].X := GetLengthValue(SpringSolver.StrokeSc);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fc');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing label Load-F1
  if GreaterThanZero(SpringSolver.LoadF1) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    Result.AddLabel(0, GetForceValue(SpringSolver.LoadF1),
      32, 0, taLeftJustify, taAlignBottom, 'F1');
  end;
  // Drawing label Load-F2
  if GreaterThanZero(SpringSolver.LoadF2) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    Result.AddLabel(0, GetForceValue(SpringSolver.LoadF2),
      64, 0, taLeftJustify, taAlignBottom, 'F2');
  end;
  // Drawing label Load-Fn
  if GreaterThanZero(SpringSolver.LoadFn) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    Result.AddLabel(0, GetforceValue(SpringSolver.LoadFn),
      96, 0, taLeftJustify, taAlignBottom, 'Fn');
  end;
  // Drawing label Load-Fc
  if GreaterThanZero(SpringSolver.LoadFc) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    Result.AddLabel(0, GetForceValue(SpringSolver.LoadFc),
      128, 0, taLeftJustify, taAlignBottom, 'Fc');
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  Result.Title := 'Torque & Angular displacement Chart';
  Result.XAxisLabel := 'α [' + GetAngleSymbol(SpringSolver.Alpha1) + ']';
  Result.YAxisLabel := 'T [' + GetTorqueSymbol(SpringSolver.TorqueT1) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Drawing bisector line
  if GreaterThanZero(SpringSolver.TorqueT1) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetAngleValue(SpringSolver.Alpha1);
    Points[1].Y := GetTorqueValue(SpringSolver.TorqueT1);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Torque-T1
  if GreaterThanZero(SpringSolver.TorqueT1) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetTorqueValue(SpringSolver.TorqueT1);
    Points[1].X := GetAngleValue(SpringSolver.Alpha1);
    Points[1].Y := GetTorqueValue(SpringSolver.TorqueT1);
    Points[2].X := GetAngleValue(SpringSolver.Alpha1);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'T1');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Torque-T2
  if GreaterThanZero(SpringSolver.TorqueT2) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetTorqueValue(SpringSolver.TorqueT2);
    Points[1].X := GetAngleValue(SpringSolver.Alpha2);
    Points[1].Y := GetTorqueValue(SpringSolver.TorqueT2);
    Points[2].X := GetAngleValue(SpringSolver.Alpha2);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'T2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Torque-Tn
  if GreaterThanZero(SpringSolver.TorqueTn) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetTorqueValue(SpringSolver.TorqueTn);
    Points[1].X := GetAngleValue(SpringSolver.Alphan);
    Points[1].Y := GetTorqueValue(SpringSolver.TorqueTn);
    Points[2].X := GetAngleValue(SpringSolver.Alphan);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Tn');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing label Torque-T1
  if GreaterThanZero(SpringSolver.TorqueT1) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    Result.AddLabel(0, GetTorqueValue(SpringSolver.TorqueT1),
      32, 0, taLeftJustify, taAlignBottom, 'T1');
  end;
  // Drawing label Torque-T2
  if GreaterThanZero(SpringSolver.TorqueT2) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    Result.AddLabel(0, GetTorqueValue(SpringSolver.TorqueT2),
      64, 0, taLeftJustify, taAlignBottom, 'T2');
  end;
  // Drawing label Load-Fn
  if GreaterThanZero(SpringSolver.TorqueTn) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    Result.AddLabel(0, GetTorqueValue(SpringSolver.TorqueTn),
      96, 0, taLeftJustify, taAlignBottom, 'Tn');
  end;
  {$ENDIF}
end;

function TCompozer.CreateGoodmanChart(const AScreenScale: double): TChart;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  if MAT.Name = '' then
    Result.Title := Format('Goodman Chart: %s', ['Custom material'])
  else
    Result.Title := Format('Goodman Chart: %s', [MAT.Name]);

  {$IFDEF MODULE1}
  Result.XAxisLabel := 'tauU [' + GetPressureSymbol(SpringSolver.TensileStrengthRm) + ']';
  Result.YAxisLabel := 'tauO [' + GetPressureSymbol(SpringSolver.TensileStrengthRm) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  // Drawing bisector line
  if GreaterThanZero(SpringSolver.AdmStaticTorsionalStressTauz) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetPressureValue(SpringSolver.AdmStaticTorsionalStressTauz);
    Points[1].Y := GetPressureValue(SpringSolver.AdmStaticTorsionalStressTauz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  Result.XAxisLabel := 'sigma U [' + GetPressureSymbol(SpringSolver.TensileStrengthRm) + ']';
  Result.YAxisLabel := 'sigma O [' + GetPressureSymbol(SpringSolver.TensileStrengthRm) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  // Drawing bisector line
  if GreaterThanZero(SpringSolver.AdmStaticBendingStressSigmaz) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetPressureValue(SpringSolver.AdmStaticBendingStressSigmaz);
    Points[1].Y := GetPressureValue(SpringSolver.AdmStaticBendingStressSigmaz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}


  {$IFDEF MODULE1}
  // Drawing Tauk-Tolerances
  if GreaterThanZero(SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1)) and
     GreaterThanZero(SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad2)) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := GetPressureValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[0].Y := GetPressureValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[1].X := GetPressureValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[1].Y := GetPressureValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[2].X := GetPressureValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[2].Y := GetPressureValue(SpringSolver.TorsionalStressTauk2 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[3].X := GetPressureValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[3].Y := GetPressureValue(SpringSolver.TorsionalStressTauk2 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Sigmaq-Tolerances
  if GreaterThanZero(SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1)) and
     GreaterThanZero(SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque2)) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := GetPressureValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[0].Y := GetPressureValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[1].X := GetPressureValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[1].Y := GetPressureValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[2].X := GetPressureValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[2].Y := GetPressureValue(SpringSolver.BendingStressSigmaq2 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[3].X := GetPressureValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[3].Y := GetPressureValue(SpringSolver.BendingStressSigmaq2 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'sigmaq1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'sigmaq2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Tauk1-Tauk2
  if GreaterThanZero(SpringSolver.TorsionalStressTauk1) and
     GreaterThanZero(SpringSolver.TorsionalStressTauk2) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := GetPressureValue(SpringSolver.TorsionalStressTauk1);
    Points[0].Y := GetPressureValue(SpringSolver.TorsionalStressTauk1);
    Points[1].X := GetPressureValue(SpringSolver.TorsionalStressTauk1);
    Points[1].Y := GetPressureValue(SpringSolver.TorsionalStressTauk2);
    Result.AddPolyLine(Points, False, 'Tauk1-Tauk2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Sigmaq1-Sigmaq2
  if GreaterThanZero(SpringSolver.BendingStressSigmaq1) and
     GreaterThanZero(SpringSolver.BendingStressSigmaq2) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := GetPressureValue(SpringSolver.BendingStressSigmaq1);
    Points[0].Y := GetPressureValue(SpringSolver.BendingStressSigmaq1);
    Points[1].X := GetPressureValue(SpringSolver.BendingStressSigmaq1);
    Points[1].Y := GetPressureValue(SpringSolver.BendingStressSigmaq2);
    Result.AddPolyLine(Points, False, 'Sigmaq1-Sigmaq2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Goodmand curve
  if MAT.Name <> '' then
  begin
    if GreaterThanZero(MAT.TorsionalStressTauOE5  ) and
       GreaterThanZero(MAT.TorsionalStressTauUE5  ) and
       GreaterThanZero(MAT.TorsionalStressTauYield) and
       GreaterThanZero(MAT.TorsionalStressTauUE6  ) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E5');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.TorsionalStressTauOE5);
      Points[1].X := GetPressureValue(MAT.TorsionalStressTauUE5);
      Points[1].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetPressureValue(MAT.TorsionalStressTauUE6);
      Points[2].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E5 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.TorsionalStressTauUE5),
        GetPressureValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E5');
      Points := nil;
    end;

    if GreaterThanZero(MAT.TorsionalStressTauOE6  ) and
       GreaterThanZero(MAT.TorsionalStressTauUE6  ) and
       GreaterThanZero(MAT.TorsionalStressTauYield) and
       GreaterThanZero(MAT.TorsionalStressTauUE7  ) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E6');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.TorsionalStressTauOE6);
      Points[1].X := GetPressureValue(MAT.TorsionalStressTauUE6);
      Points[1].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetPressureValue(MAT.TorsionalStressTauUE7);
      Points[2].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E6 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.TorsionalStressTauUE6),
        GetPressureValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E6');
      Points := nil;
    end;

    if GreaterThanZero(MAT.TorsionalStressTauOE7  ) and
       GreaterThanZero(MAT.TorsionalStressTauUE7  ) and
       GreaterThanZero(MAT.TorsionalStressTauYield) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E7');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.TorsionalStressTauOE7);
      Points[1].X := GetPressureValue(MAT.TorsionalStressTauUE7);
      Points[1].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetPressureValue(MAT.TorsionalStressTauYield);
      Points[2].Y := GetPressureValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E7 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.TorsionalStressTauUE7),
        GetPressureValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E7 Cycles');
      Points := nil;
    end;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Goodmand curve
  if MAT.Name <> '' then
  begin
    if GreaterThanZero(MAT.BendingStressSigmaOE5  ) and
       GreaterThanZero(MAT.BendingStressSigmaUE5  ) and
       GreaterThanZero(MAT.BendingStressSigmaYield) and
       GreaterThanZero(MAT.BendingStressSigmaUE6  ) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E5');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.BendingStressSigmaOE5);
      Points[1].X := GetPressureValue(MAT.BendingStressSigmaUE5);
      Points[1].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Points[2].X := GetPressureValue(MAT.BendingStressSigmaUE6);
      Points[2].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Result.AddPolyLine(Points, False, '1E5 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.BendingStressSigmaUE5),
        GetPressureValue(MAT.BendingStressSigmaYield),
        0, 0, taLeftJustify, taAlignBottom, '1E5');
      Points := nil;
    end;

    if GreaterThanZero(MAT.BendingStressSigmaOE6  ) and
       GreaterThanZero(MAT.BendingStressSigmaUE6  ) and
       GreaterThanZero(MAT.BendingStressSigmaYield) and
       GreaterThanZero(MAT.BendingStressSigmaUE7  ) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E6');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.BendingStressSigmaOE6);
      Points[1].X := GetPressureValue(MAT.BendingStressSigmaUE6);
      Points[1].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Points[2].X := GetPressureValue(MAT.BendingStressSigmaUE7);
      Points[2].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Result.AddPolyLine(Points, False, '1E6 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.BendingStressSigmaUE6),
        GetPressureValue(MAT.BendingStressSigmaYield),
        0, 0, taLeftJustify, taAlignBottom, '1E6');
      Points := nil;
    end;

    if GreaterThanZero(MAT.BendingStressSigmaOE7  ) and
       GreaterThanZero(MAT.BendingStressSigmaUE7  ) and
       GreaterThanZero(MAT.BendingStressSigmaYield) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E7');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetPressureValue(MAT.BendingStressSigmaOE7);
      Points[1].X := GetPressureValue(MAT.BendingStressSigmaUE7);
      Points[1].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Points[2].X := GetPressureValue(MAT.BendingStressSigmaYield);
      Points[2].Y := GetPressureValue(MAT.BendingStressSigmaYield);
      Result.AddPolyLine(Points, False, '1E7 Cycles');
      Result.AddLabel(
        GetPressureValue(MAT.BendingStressSigmaUE7),
        GetPressureValue(MAT.BendingStressSigmaYield),
        0, 0, taLeftJustify, taAlignBottom, '1E7 Cycles');
      Points := nil;
    end;
  end;
  {$ENDIF}
end;

function TCompozer.CreateBucklingChart(const AScreenScale: double): TChart;
var
  X, Y: single;
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Buckling diagram';
  Result.XAxisLabel := 'nu∙L0/D';
  Result.YAxisLabel := 's/L0';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  {$IFDEF MODULE1}
  // Draw buckling curve
  SpringSolver.GetBucklingCurve(Points);
  LoadChart2(Result, 'BucklingChart', 'BucklingCurve');
  Result.XMinF := 0.0;
  Result.YMinF := 0.0;
  Result.YMaxF := 1.0;
  Result.YDeltaF := 0.1;
  Result.YCount  := 10;
  Result.ShowOrigin := True;
  Result.AddPolyLine(Points, False, 'Buckling-Curve');
  Points := nil;

  if GreaterThanZero(SpringSolver.Dm      ) and
     GreaterThanZero(SpringSolver.LengthL0) then
  begin
    LoadChart2(Result, 'BucklingChart', 'Sc');
    X := SpringSolver.SeatingCoefficent * ScalarUnit.ToFloat(SpringSolver.LengthL0 / SpringSolver.Dm);

    if SpringSolver.StrokeSc > SpringSolver.DeflectionSk then
      Y := ScalarUnit.ToFloat(SpringSolver.StrokeSc / SpringSolver.LengthL0)
    else
      Y := ScalarUnit.ToFloat(SpringSolver.DeflectionSk / SpringSolver.LengthL0);

    SetLength(Points, 2);
    Points[0].x := X;
    Points[0].y := 0;
    Points[1].x := X;
    Points[1].y := Y;
    Result.AddPolyLine(Points, False, 'Sc');
    Result.AddDotLabel(X, ScalarUnit.ToFloat(SpringSolver.StrokeSc / SpringSolver.LengthL0),
      5, 10, 0, taLeftJustify, taVerticalCenter, 'Sc');
    Points := nil;
    LoadChart2(Result, 'BucklingChart', 'Sx');
    Result.AddDotLabel(X, ScalarUnit.ToFloat(SpringSolver.StrokeS1 / SpringSolver.LengthL0),
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S1');
    Result.AddDotLabel(X, ScalarUnit.ToFloat(SpringSolver.StrokeS2 / SpringSolver.LengthL0),
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S2');
  end;
  {$ENDIF}
end;

function TCompozer.CreateLoadF1Chart(const AScreenScale: double): TChart;
const
  DeltaTemp : TQuantity = ({$IFNDEF ADIMOFF} FID: KelvinId; FValue: 323.15 {$ELSE} 323.15 {$ENDIF});
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;

  {$IFDEF MODULE1}
  Result.Title := 'Load F1-Temperature Chart';
  Result.XAxisLabel := Format('T [%s]', [GetTemperatureSymbol(0*K)]);
  Result.YAxisLabel := 'F1 [' + GetForceSymbol(SpringSolver.LoadF1) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if GreaterThanZero(SpringSolver.GetF1(MAT.Temperature - DeltaTemp)) and
     GreaterThanZero(SpringSolver.GetF1(MAT.Temperature + DeltaTemp)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := GetTemperatureValue(MAT.Temperature - DeltaTemp);
    Points[0].y := GetForceValue(SpringSolver.GetF1(MAT.Temperature - DeltaTemp));
    Points[1].x := GetTemperatureValue(MAT.Temperature + DeltaTemp);
    Points[1].y := GetforceValue(SpringSolver.GetF1(MAT.Temperature + DeltaTemp));
    Result.AddPolyLine(Points, True, 'F1(T)');
    Points := nil;
    Result.AddDotLabel(GetTemperatureValue(MAT.Temperature), GetForceValue(SpringSolver.GetF1(MAT.Temperature)),
      5, 0, 10, taLeftJustify, taAlignBottom, GetTemperatureString(MAT.Temperature));
  end;
  {$ENDIF}
end;

function TCompozer.CreateLoadF2Chart(const AScreenScale: double): TChart;
const
  DeltaTemp : TQuantity = ({$IFNDEF ADIMOFF} FID: KelvinId; FValue: 323.15 {$ELSE} 323.15 {$ENDIF});
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;

  {$IFDEF MODULE1}
  Result.Title := 'Load F2-Temperature Chart';
  Result.XAxisLabel := Format('T [%s]', [GetTemperatureSymbol(0*K)]);
  Result.YAxisLabel := 'F2 [' + GetForceSymbol(SpringSolver.LoadF2) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if GreaterThanZero(SpringSolver.GetF2(MAT.Temperature - DeltaTemp)) and
     GreaterThanZero(SpringSolver.GetF2(MAT.Temperature + DeltaTemp)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := GetTemperatureValue(MAT.Temperature - DeltaTemp);
    Points[0].y := GetForceValue(SpringSolver.GetF2(MAT.Temperature - DeltaTemp));
    Points[1].x := GetTemperatureValue(MAT.Temperature + DeltaTemp);
    Points[1].y := GetForceValue(SpringSolver.GetF2(MAT.Temperature + DeltaTemp));
    Result.AddPolyLine(Points, True, 'F2(T)');
    Points := nil;
    Result.AddDotLabel(GetTemperatureValue(MAT.Temperature), GetForceValue(SpringSolver.GetF2(MAT.Temperature)),
      5, 0, 10, taLeftJustify, taAlignBottom, GetTemperatureString(MAT.Temperature));
  end;
  {$ENDIF}

end;

function TCompozer.CreateShearModulusChart(const AScreenScale: double): TChart;
const
  DeltaTemp : TQuantity = ({$IFNDEF ADIMOFF} FID: KelvinId; FValue: 323.15 {$ELSE} 323.15 {$ENDIF});
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Shear Modulus G-Temperature Chart';
  Result.XAxisLabel := Format('T [%s]', [GetTemperatureSymbol(0*K)]);
  Result.YAxisLabel := 'G [' + GetPressureSymbol(SpringSolver.ShearModulus) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if GreaterThanZero(MAT.GetG(MAT.Temperature - DeltaTemp)) and
     GreaterThanZero(MAT.GetG(MAT.Temperature + DeltaTemp)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := GetTemperatureValue(MAT.Temperature - DeltaTemp);
    Points[0].y := GetPressureValue(MAT.GetG(MAT.Temperature - DeltaTemp));
    Points[1].x := GetTemperatureValue(MAT.Temperature + DeltaTemp);
    Points[1].y := GetPressureValue(MAT.GetG(MAT.Temperature + DeltaTemp));
    Result.AddPolyLine(Points, True, 'G(T)');
    Points := nil;
    Result.AddDotLabel(GetTemperatureValue(MAT.Temperature), GetPressureValue(MAT.GetG(MAT.Temperature)),
      5, 0, 10, taLeftJustify, taAlignBottom, GetTemperatureString(MAT.Temperature));
  end;
end;

function TCompozer.CreateYoungModulusChart(const AScreenScale: double): TChart;
const
  DeltaTemp : TQuantity = ({$IFNDEF ADIMOFF} FID: KelvinId; FValue: 323.15 {$ELSE} 323.15 {$ENDIF});
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Young Modulus G-Temperature Chart';
  Result.XAxisLabel := Format('T [%s]', [GetTemperatureSymbol(0*K)]);
  Result.YAxisLabel := 'E [' + GetPressureSymbol(SpringSolver.YoungModulus) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if GreaterThanZero(MAT.GetE(MAT.Temperature - DeltaTemp)) and
     GreaterThanZero(MAT.GetE(MAT.Temperature + DeltaTemp)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := GetTemperatureValue(MAT.Temperature - DeltaTemp);
    Points[0].y := GetPressureValue(MAT.GetE(MAT.Temperature - DeltaTemp));
    Points[1].x := GetTemperatureValue(MAT.Temperature + DeltaTemp);
    Points[1].y := GetPressureValue(MAT.GetE(MAT.Temperature + DeltaTemp));
    Result.AddPolyLine(Points, True, 'E(T)');
    Points := nil;

    Result.AddDotLabel(GetTemperatureValue(MAT.Temperature), GetPressureValue(MAT.GetE(MAT.Temperature)),
      5, 0, 10, taLeftJustify, taAlignBottom, GetTemperatureString(MAT.Temperature));
  end;
end;

function TCompozer.CreateSpringDrawing(
  const AScreenScale: double): TSpringDrawing;
begin
  Result := TSpringDrawing.Create;
  LoadSpring(Result, 'SpringDrawing', 'Spring');

  {$IFDEF MODULE1}
  Result.d  := MeterUnit.ToFloat(SpringSolver.WireDiameter, [pMilli]);
  Result.Dm := MeterUnit.ToFloat(SpringSolver.Dm, [pMilli]);
  Result.Lc := MeterUnit.ToFloat(SpringSolver.LengthLc, [pMilli]);
  Result.n  := SpringSolver.ActiveColis;
  Result.nt1 := (SpringSolver.TotalCoils - SpringSolver.ActiveColis) / 2;
  Result.nt2 := (SpringSolver.TotalCoils - SpringSolver.ActiveColis) / 2;
  Result.ClosedEnds := False;
  Result.GroundEnds := False;
  Result.Spacer := DefaultSpacer;
  Result.Scale  := AScreenScale;
  {$ENDIF}
end;

function BoolToText(const TrueSymbol, FalseSymbol: string; Value: boolean): string;
begin
  if Value then
    Result := TrueSymbol
  else
    Result := FalseSymbol;
end;

function TCompozer.CreateQualityTable(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 5;
  {$IFDEF MODULE1}
  Result.RowCount := 7;
  {$ENDIF}
  {$IFDEF MODULE3}
  Result.RowCount := 9;
  {$ENDIF}
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');

  {$IFDEF MODULE1}
  Result[0, 0] := 'Quality Grade';
  Result[1, 0] := 'De, Di';
  Result[2, 0] := 'L0';
  Result[3, 0] := 'F1';
  Result[4, 0] := 'F2';
  Result[5, 0] := 'e1';
  Result[6, 0] := 'e2';

  Result[0, 1] := '1';
  Result[1, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade1);
  Result[2, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade1);
  Result[3, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade1);
  Result[4, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade1);
  Result[5, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade1);
  Result[6, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade1);

  Result[0, 2] := '2';
  Result[1, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade2);
  Result[2, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade2);
  Result[3, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade2);
  Result[4, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade2);
  Result[5, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade2);
  Result[6, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade2);

  Result[0, 3] := '3';
  Result[1, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade3);
  Result[2, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade3);
  Result[3, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade3);
  Result[4, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade3);
  Result[5, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade3);
  Result[6, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade3);

  Result[0, 4] := 'Tol.';
  Result[1, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceOnCoilDiameter));
  Result[2, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceFreeBodyLength));
  Result[3, 4] := '± ' + GetString(GetForceValue(SpringTolerance.ToleranceOnLoad1));
  Result[4, 4] := '± ' + GetString(GetForceValue(SpringTolerance.ToleranceOnLoad2));
  Result[5, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceOnPerpendicularity));
  Result[6, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceOnParallelism));
  {$ENDIF}

  {$IFDEF MODULE3}
  Result[0, 0] := 'Quality Grade';
  Result[1, 0] := 'De, Di';
  Result[2, 0] := 'Lk';
  Result[3, 0] := 'T1';
  Result[4, 0] := 'T2';
  Result[5, 0] := 'α0';
  Result[6, 0] := 'l1, l2';
  Result[7, 0] := 'r1, r2';
  Result[8, 0] := 'φ1, φ2';

  Result[0, 1] := '1';
  Result[1, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade1);
  Result[2, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade1);
  Result[3, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque1 = QualityGrade1);
  Result[4, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque2 = QualityGrade1);
  Result[5, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnRelativeEndAngle = QualityGrade1);
  Result[6, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLegLengths = QualityGrade1);
  Result[7, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnBendRadii = QualityGrade1);
  Result[8, 1] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnAnglesOfBendOnLegs = QualityGrade1);

  Result[0, 2] := '2';
  Result[1, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade2);
  Result[2, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade2);
  Result[3, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque1 = QualityGrade2);
  Result[4, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque2 = QualityGrade2);
  Result[5, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnRelativeEndAngle = QualityGrade2);
  Result[6, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLegLengths = QualityGrade2);
  Result[7, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnBendRadii = QualityGrade2);
  Result[8, 2] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnAnglesOfBendOnLegs = QualityGrade2);

  Result[0, 3] := '3';
  Result[1, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade3);
  Result[2, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade3);
  Result[3, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque1 = QualityGrade3);
  Result[4, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnTorque2 = QualityGrade3);
  Result[5, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnRelativeEndAngle = QualityGrade3);
  Result[6, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnLegLengths = QualityGrade3);
  Result[7, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnBendRadii = QualityGrade3);
  Result[8, 3] := BoolToText('x', ' ', SpringTolerance.QualityGradeOnAnglesOfBendOnLegs = QualityGrade3);

  Result[0, 4] := 'Tol.';
  Result[1, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceOnCoilDiameter));
  Result[2, 4] := '± ' + GetString(GetLengthValue(SpringTolerance.ToleranceOnFreeBodyLength));
  Result[3, 4] := '± ' + GetString(GetTorqueValue(SpringTolerance.ToleranceOnTorque1));
  Result[4, 4] := '± ' + GetString(GetTorqueValue(SpringTolerance.ToleranceOnTorque2));
  Result[5, 4] := '± ' + GetString(GetAngleValue (SpringTolerance.ToleranceOnRelativeEndAngle));
  Result[6, 4] := '± ' + GetString(GetLengthValue(Max(SpringTolerance.ToleranceOnLegLength1, SpringTolerance.ToleranceOnLegLength2)));
  Result[7, 4] := '± ' + GetString(GetLengthValue(Max(SpringTolerance.ToleranceOnBendRadius1, SpringTolerance.ToleranceOnBendRadius2)));
  Result[8, 4] := '± ' + GetString(GetAngleValue (Max(SpringTolerance.ToleranceOnBendAngle1, SpringTolerance.ToleranceOnBendAngle2)));

  {$ENDIF}
end;

function TCompozer.CreateQuick1Table(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  {$IFDEF MODULE1} Result.ColumnCount := 7; {$ENDIF}
  {$IFDEF MODULE3} Result.ColumnCount := 7; {$ENDIF}
  {$IFDEF MODULE1} Result.RowCount    := 6; {$ENDIF}
  {$IFDEF MODULE3} Result.RowCount    := 5; {$ENDIF}
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');
  {$IFDEF MODULE1}
  Result[0, 0] := 'L [' + GetLengthSymbol(0*m) + ']';
  Result[1, 0] := Format('L0: %s', [GetString(GetLengthValue(SpringSolver.LengthL0))]);
  Result[2, 0] := Format('L1: %s', [GetString(GetLengthValue(SpringSolver.LengthL1))]);
  Result[3, 0] := Format('L2: %s', [GetString(GetLengthValue(SpringSolver.LengthL2))]);
  Result[4, 0] := Format('Ln: %s', [GetString(GetLengthValue(SpringSolver.LengthLn))]);
  Result[5, 0] := Format('Lc: %s', [GetString(GetLengthValue(SpringSolver.LengthLc))]);
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 0] := 'angle [' + GetAngleSymbol(0*deg) + ']';
  Result[1, 0] := Format('α0: %s', [GetString(GetAngleValue(0*deg))]);
  Result[2, 0] := Format('α1: %s', [GetString(GetAngleValue(SpringSolver.Alpha1))]);
  Result[3, 0] := Format('α2: %s', [GetString(GetAngleValue(SpringSolver.Alpha2))]);
  Result[4, 0] := Format('αn: %s', [GetString(GetAngleValue(SpringSolver.Alphan))]);
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 1] := 'F [' + GetForceSymbol(SpringSolver.LoadF1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := Format('F1: %s', [GetString(GetForceValue(SpringSolver.LoadF1))]);
  Result[3, 1] := Format('F2: %s', [GetString(GetForceValue(SpringSolver.LoadF2))]);
  Result[4, 1] := Format('Fn: %s', [GetString(GetForceValue(SpringSolver.LoadFn))]);
  Result[5, 1] := Format('Fc: %s', [GetString(GetForceValue(SpringSolver.LoadFc))]);
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 1] := 'torque [' + GetangularStiffnessSymbol(SpringSolver.TorqueT1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := Format('T1: %s', [GetString(GetAngularStiffnessValue(SpringSolver.TorqueT1))]);
  Result[3, 1] := Format('T2: %s', [GetString(GetAngularStiffnessValue(SpringSolver.TorqueT2))]);
  Result[4, 1] := Format('Tn: %s', [GetString(GetAngularStiffnessValue(SpringSolver.TorqueTn))]);
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 2] := 'tau [' + GetPressureSymbol(SpringSolver.TorsionalStressTauk1) + ']';
  Result[1, 2] := '';
  Result[2, 2] := Format('tauk1: %s', [GetString(GetPressureValue(SpringSolver.TorsionalStressTauk1))]);
  Result[3, 2] := Format('tauk2: %s', [GetString(GetPressureValue(SpringSolver.TorsionalStressTauk2))]);
  Result[4, 2] := Format('tau n: %s', [GetString(GetPressureValue(SpringSolver.TorsionalStressTaun ))]);
  Result[5, 2] := Format('tau k: %s', [GetString(GetPressureValue(SpringSolver.TorsionalStressTauc ))]);
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 2] := 'σ [' + GetPressureSymbol(SpringSolver.BendingStressSigmaq1) + ']';
  Result[1, 2] := '';
  Result[2, 2] := Format('σ1: %s', [GetString(GetPressureValue(SpringSolver.BendingStressSigmaq1))]);
  Result[3, 2] := Format('σ2: %s', [GetString(GetPressureValue(SpringSolver.BendingStressSigmaq2))]);
  Result[4, 2] := Format('σn: %s', [GetString(GetPressureValue(SpringSolver.BendingStressSigmaqn))]);
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 3] := 's [' + GetLengthSymbol(SpringSolver.LengthL1) + ']';
  Result[1, 3] := '';
  Result[2, 3] := Format('s1: %s', [GetString(GetLengthValue(SpringSolver.StrokeS1))]);
  Result[3, 3] := Format('s2: %s', [GetString(GetLengthValue(SpringSolver.StrokeS2))]);
  Result[4, 3] := Format('sn: %s', [GetString(GetLengthValue(SpringSolver.StrokeSn))]);
  Result[5, 3] := Format('sc: %s', [GetString(GetLengthValue(SpringSolver.StrokeSc))]);
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 4] := 'tau/tauz';
  if GreaterThanZero(SpringSolver.AdmStaticTorsionalStressTauz) then
  begin
    Result[1, 4] := '';
    Result[2, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTau1 / SpringSolver.AdmStaticTorsionalStressTauz)]);
    Result[3, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTau2 / SpringSolver.AdmStaticTorsionalStressTauz)]);
    Result[4, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTaun / SpringSolver.AdmStaticTorsionalStressTauz)]);
    Result[5, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTauc / SpringSolver.AdmStaticTorsionalStressTauz)]);
  end;
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 3] := 'σ/σz';
  if GreaterThanZero(SpringSolver.AdmStaticBendingStressSigmaz) then
  begin
    Result[1, 3] := '';
    Result[2, 3] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigmaq1/SpringSolver.AdmStaticBendingStressSigmaz)]);
    Result[3, 3] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigmaq2/SpringSolver.AdmStaticBendingStressSigmaz)]);
    Result[4, 3] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigman /SpringSolver.AdmStaticBendingStressSigmaz)]);
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 5] := 'tau/Rm';
  if GreaterThanZero(SpringSolver.TensileStrengthRm) then
  begin
    Result[1, 5] := '';
    Result[2, 5] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTau1 / SpringSolver.TensileStrengthRm)]);
    Result[3, 5] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTau2 / SpringSolver.TensileStrengthRm)]);
    Result[4, 5] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTaun / SpringSolver.TensileStrengthRm)]);
    Result[5, 5] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.TorsionalStressTauc / SpringSolver.TensileStrengthRm)]);
  end;
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 4] := 'σ/Rm';
  if GreaterThanZero(SpringSolver.TensileStrengthRm) then
  begin
    Result[1, 4] := '';
    Result[2, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigmaq1/SpringSolver.TensileStrengthRm)]);
    Result[3, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigmaq1/SpringSolver.TensileStrengthRm)]);
    Result[4, 4] := Format('%0.3f', [ScalarUnit.ToFloat(SpringSolver.BendingStressSigman /SpringSolver.TensileStrengthRm)]);
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 6] := 'De [' + GetLengthSymbol(SpringSolver.De) + ']';
  if GreaterThanZero(SpringSolver.StrokeSc) then
  begin
    Result[1, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.De))]);
    Result[2, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.De + SpringSolver.DeltaDe * SpringSolver.StrokeS1 / SpringSolver.StrokeSc))]);
    Result[3, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.De + SpringSolver.DeltaDe * SpringSolver.StrokeS2 / SpringSolver.StrokeSc))]);
    Result[4, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.De + SpringSolver.DeltaDe * SpringSolver.StrokeSn / SpringSolver.StrokeSc))]);
    Result[5, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.De + SpringSolver.DeltaDe * SpringSolver.StrokeSc / SpringSolver.StrokeSc))]);
  end;
  {$ENDIF}
  {$IFDEF MODULE3}
  begin
    Result[0, 5] := 'Lk [' + GetLengthSymbol(SpringSolver.Lk(0*rad)) + ']';
    Result[1, 5] := Format('%s', [GetString(GetLengthValue(SpringSolver.Lk(0*rad)))]);
    Result[2, 5] := Format('%s', [GetString(GetLengthValue(SpringSolver.Lk(SpringSolver.Alpha1)))]);
    Result[3, 5] := Format('%s', [GetString(GetLengthValue(SpringSolver.Lk(SpringSolver.Alpha2)))]);
    Result[4, 5] := Format('%s', [GetString(GetLengthValue(SpringSolver.Lk(SpringSolver.Alphan)))]);

    Result[0, 6] := 'Di [' + GetLengthSymbol(SpringSolver.InnerCoilDiameter(0*rad)) + ']';
    Result[1, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.InnerCoilDiameter(0*rad)))]);
    Result[2, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.InnerCoilDiameter(SpringSolver.Alpha1)))]);
    Result[3, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.InnerCoilDiameter(SpringSolver.Alpha2)))]);
    Result[4, 6] := Format('%s', [GetString(GetLengthValue(SpringSolver.InnerCoilDiameter(SpringSolver.Alphan)))]);
  end;
  {$ENDIF}
end;

function TCompozer.CreateQuick1List1(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount := 18;
  Result.Zoom := AScreenScale;

  LoadTable(Result, 'Quick1List', 'Table');
  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  Result.Items[0, 0] := 'd';
  Result.Items[0, 1] := '=';
  Result.Items[0, 2] := GetLengthString(SpringSolver.WireDiameter, SpringSolver.WireDiameterTolerance);

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := GetLengthString(SpringSolver.Di);

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := GetLengthString(SpringSolver.Dm, SpringTolerance.ToleranceOnCoilDiameter);

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] := GetLengthString(SpringSolver.De);

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := Format('%0.2f coils', [SpringSolver.ActiveColis]);

  {$IFDEF MODULE1}
  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := Format('%0.2f coils', [SpringSolver.TotalCoils]);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := GetStiffnessString(SpringSolver.SpringRateR);

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := GetLengthString(SpringSolver.De + SpringSolver.DeltaDe);

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := GetLengthString(SpringSolver.DiMin);

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := GetLengthString(SpringSolver.DeMax);

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := GetLengthString(SpringSolver.DeflectionSk);

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := GetLengthString(SpringSolver.WireLength);

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := GetMassString(SpringSolver.Mass);

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := GetEnergyString(SpringSolver.SpringWorkW12);

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := GetEnergyString(SpringSolver.SpringWorkW0n);

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := GetFrequencyString(SpringSolver.NaturalFrequency);

  Result.Items[16, 0] := 'nu';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := Format('%0.2f', [SpringSolver.SeatingCoefficent]);

  Result.Items[17, 0] := 'load';
  Result.Items[17, 1] := '=';
  if SpringSolver.DynamicLoad then
    Result.Items[17, 2] := ('dynamic')
  else
    Result.Items[17, 2] := ('static');
  {$ENDIF}

  {$IFDEF MODULE3}
  Result.Items[ 6, 0] := 'a';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := GetLengthString(SpringSolver.CoilsGap);

  Result.Items[ 7, 0] := 'RMR';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := GetAngularStiffnessString(SpringSolver.SpringRateRMR);

  Result.Items[ 8, 0] := 'Dm/d';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := ScalarUnit.ToString(SpringSolver.Dm/Springsolver.WireDiameter, DefaultPrecision, DefaultDigits, []);

  Result.Items[ 9, 0] := 'q';
  Result.Items[ 9, 1] := '=';
  Result.Items[ 9, 2] := ScalarUnit.ToString(SpringSolver.CorrectionFactorQ, DefaultPrecision, DefaultDigits, []);

  Result.Items[10, 0] := 'm';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := GetMassString(SpringSolver.Mass);

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := GetLengthString(SpringSolver.WireLength);

  Result.Items[12, 0] := 'W12';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := GetEnergyString(SpringSolver.SpringWorkW12);

  Result.Items[13, 0] := 'W0n';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := GetEnergyString(SpringSolver.SpringWorkW0n);

  Result.Items[14, 0] := 'fe';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := GetFrequencyString(SpringSolver.NaturalFrequency);





  Result.Items[15, 0] := 'load';
  Result.Items[15, 1] := '=';
  if SpringSolver.DynamicLoad then
    Result.Items[15, 2] := ('dynamic')
  else
    Result.Items[15, 2] := ('static');
  {$ENDIF}
end;

function TCompozer.CreateQuick1List2(const AScreenScale: double): TReportTable;
var
  i, j, k: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount := 18;
  Result.Zoom := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table');

  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  for i := 0 to Result.RowCount - 1 do
    for j := 0 to Result.ColumnCount - 1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  {$IFDEF MODULE1}
  k := 2;
  Result.Items[0+k, 0] := 'tauk1';
  Result.Items[0+k, 1] := '=';
  Result.Items[0+k, 2] := GetPressureString(SpringSolver.TorsionalStressTauk1);

  Result.Items[1+k, 0] := 'tauk2';
  Result.Items[1+k, 1] := '=';
  Result.Items[1+k, 2] := GetPressureString(SpringSolver.TorsionalStressTauk2);

  Result.Items[2+k, 0] := 'taukh';
  Result.Items[2+k, 1] := '=';
  Result.Items[2+k, 2] := GetPressureString(SpringSolver.TorsionalStressTaukh);

  Result.Items[3+k, 0] := 'E';
  Result.Items[3+k, 1] := '=';
  Result.Items[3+k, 2] := GetPressureString(SpringSolver.YoungModulus);

  Result.Items[4+k, 0] := 'G';
  Result.Items[4+k, 1] := '=';
  Result.Items[4+k, 2] := GetPressureString(SpringSolver.ShearModulus);

  Result.Items[5+k, 0] := 'rho';
  Result.Items[5+k, 1] := '=';
  Result.Items[5+k, 2] := GetDensityString(SpringSolver.MaterialDensity);

  Result.Items[6+k, 0] := 'Rm';
  Result.Items[6+k, 1] := '=';
  Result.Items[6+k, 2] := GetPressureString(SpringSolver.TensileStrengthRm);

  Result.Items[7+k, 0] := 'tauz';
  Result.Items[7+k, 1] := '=';
  Result.Items[7+k, 2] := GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz);

  Result.Items[8+k, 0] := 'ns';
  Result.Items[8+k, 1] := '=';
  Result.Items[8+k, 2] := Format('%0.2f', [SpringSolver.StaticSafetyFactor]);

  if SpringSolver.DynamicLoad then
  begin
    Result.Items[9+k, 0] := 'tauoz';
    Result.Items[9+k, 1] := '=';
    Result.Items[9+k, 2] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressTauoz);

    Result.Items[10+k, 0] := 'tauhz';
    Result.Items[10+k, 1] := '=';
    Result.Items[10+k, 2] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressRangeTauhz);

    Result.Items[11+k, 0] := 'nf';
    Result.Items[11+k, 1] := '=';
    Result.Items[11+k, 2] := Format('%02f', [ScalarUnit.ToFloat(SpringSolver.DynamicSafetyFactor)]);


    Result.Items[13+k, 0] := 'N';
    Result.Items[13+k, 1] := '=';
    Result.Items[13+k, 2] := '---';
    if SpringSolver.NumOfCycles > 0 then
    begin
      Result.Items[13+k, 0] := 'N';
      Result.Items[13+k, 1] := '=';
      Result.Items[13+k, 2] := Format('%0.0f cycles', [ScalarUnit.ToFloat(SpringSolver.NumOfCycles)]);
    end;

    Result.Items[14+k, 0] := 'Nh';
    Result.Items[14+k, 1] := '=';
    Result.Items[14+k, 2] := '---';
    if GreaterThanZero(SpringSolver.CycleFrequency) then
    begin
      Result.Items[14+k, 0] := 'Nh';
      Result.Items[14+k, 1] := '=';
      Result.Items[14+k, 2] := HourUnit.ToString(SpringSolver.NumOfCycles / SpringSolver.CycleFrequency, 5, 5, []);
    end;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  k := 2;
  Result.Items[0+k, 0] := 'sigma q1';
  Result.Items[0+k, 1] := '=';
  Result.Items[0+k, 2] := GetPressureString(SpringSolver.BendingStressSigmaq1);

  Result.Items[1+k, 0] := 'sigma q2';
  Result.Items[1+k, 1] := '=';
  Result.Items[1+k, 2] := GetPressureString(SpringSolver.BendingStressSigmaq2);

  Result.Items[2+k, 0] := 'sigma qh';
  Result.Items[2+k, 1] := '=';
  Result.Items[2+k, 2] := GetPressureString(SpringSolver.BendingStressSigmaqh);

  Result.Items[3+k, 0] := 'E';
  Result.Items[3+k, 1] := '=';
  Result.Items[3+k, 2] := GetPressureString(SpringSolver.YoungModulus);

  Result.Items[4+k, 0] := 'G';
  Result.Items[4+k, 1] := '=';
  Result.Items[4+k, 2] := GetPressureString(SpringSolver.ShearModulus);

  Result.Items[5+k, 0] := 'rho';
  Result.Items[5+k, 1] := '=';
  Result.Items[5+k, 2] := GetDensityString(SpringSolver.MaterialDensity);

  Result.Items[6+k, 0] := 'Rm';
  Result.Items[6+k, 1] := '=';
  Result.Items[6+k, 2] := GetPressureString(SpringSolver.TensileStrengthRm);

  Result.Items[7+k, 0] := 'sigma z';
  Result.Items[7+k, 1] := '=';
  Result.Items[7+k, 2] := GetPressureString(SpringSolver.AdmStaticBendingStressSigmaz);

  Result.Items[8+k, 0] := 'ns';
  Result.Items[8+k, 1] := '=';
  Result.Items[8+k, 2] := Format('%0.2f', [SpringSolver.StaticSafetyFactor]);

  if SpringSolver.DynamicLoad then
  begin
    Result.Items[9+k, 0] := 'sigma oz';
    Result.Items[9+k, 1] := '=';
    Result.Items[9+k, 2] := GetPressureString(SpringSolver.AdmDynamicBendingStressSigmaoz);

    Result.Items[10+k, 0] := 'sigma hz';
    Result.Items[10+k, 1] := '=';
    Result.Items[10+k, 2] := GetPressureString(SpringSolver.AdmDynamicBendingStressRangeSigmahz);

    Result.Items[11+k, 0] := 'nf';
    Result.Items[11+k, 1] := '=';
    Result.Items[11+k, 2] := Format('%02f', [ScalarUnit.ToFloat(SpringSolver.DynamicSafetyFactor)]);
  end;


  {$ENDIF}
end;

function TCompozer.CreateMessageList(const AScreenScale: double): TReportTable;
var
  i, j: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 1;
  Result.RowCount := 1 + ErrorMessage.Count + WarningMessage.Count;
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'MessageList', 'Table');

  Result.Items[0, 0] := BoolToText('Messages:', '',
    (ErrorMessage.Count + WarningMessage.Count) > 0);

  j := 1;
  for i := 0 to ErrorMessage.Count - 1 do
  begin
    Result.Items[j, 0] := ErrorMessage[i];
    Inc(j);
  end;

  for i := 0 to WarningMessage.Count - 1 do
  begin
    Result.Items[j, 0] := WarningMessage[i];
    Inc(j);
  end;
end;

function TCompozer.CreateQuick1AList(const AScreenScale: double): TReportTable;
var
  i, j: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount := 38;
  Result.Zoom := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table');

  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  {$IFDEF MODULE1}
  for i := 0 to Result.RowCount - 1 do
    for j := 0 to Result.ColumnCount - 1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  Result.Items[0, 0] := 'd';
  Result.Items[0, 1] := '=';
  Result.Items[0, 2] := GetLengthString(SpringSolver.WireDiameter, SpringSolver.WireDiameterTolerance);

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := GetLengthString(SpringSolver.Di);

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := GetLengthString(SpringSolver.Dm);

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] := GetLengthString(SpringSolver.De, SpringTolerance.ToleranceOnCoilDiameter);

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := Format('%0.2f coils', [SpringSolver.ActiveColis]);

  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := Format('%0.2f coils', [SpringSolver.TotalCoils]);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := GetStiffnessString(SpringSolver.SpringRateR);

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := GetLengthString(SpringSolver.De + SpringSolver.DeltaDe);

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := GetLengthString(SpringSolver.DiMin);

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := GetLengthString(SpringSolver.DeMax);

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := GetLengthString(SpringSolver.DeflectionSk);

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := GetLengthString(SpringSolver.WireLength);

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := GetMassString(SpringSolver.Mass);

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := GetEnergyString(SpringSolver.SpringWorkW12);

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := GetEnergyString(SpringSolver.SpringWorkW0n);

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := GetFrequencyString(SpringSolver.NaturalFrequency);

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := GetLengthString(SpringSolver.Pitch);

  Result.Items[17, 0] := 'PitchRatio';
  Result.Items[17, 1] := '=';
  Result.Items[17, 2] := Format('%0.4f', [ScalarUnit.ToFloat(SpringSolver.PitchRatio)]);

  Result.Items[18, 0] := 'nu';
  Result.Items[18, 1] := '=';
  Result.Items[18, 2] := Format('%0.1f', [SpringSolver.SeatingCoefficent]);

  Result.Items[19, 0] := 'load';
  Result.Items[19, 1] := '=';
  if SpringSolver.DynamicLoad then
    Result.Items[19, 2] := ('dynamic')
  else
    Result.Items[19, 2] := ('static');

  Result.Items[22, 0] := 'tauk1';
  Result.Items[22, 1] := '=';
  Result.Items[22, 2] := GetPressureString(SpringSolver.TorsionalStressTauk1);

  Result.Items[23, 0] := 'tauk2';
  Result.Items[23, 1] := '=';
  Result.Items[23, 2] := GetPressureString(SpringSolver.TorsionalStressTauk2);

  Result.Items[24, 0] := 'taukh';
  Result.Items[24, 1] := '=';
  Result.Items[24, 2] := GetPressureString(SpringSolver.TorsionalStressTaukh);

  Result.Items[26, 0] := 'E';
  Result.Items[26, 1] := '=';
  Result.Items[26, 2] := GetPressureString(SpringSolver.YoungModulus);

  Result.Items[27, 0] := 'G';
  Result.Items[27, 1] := '=';
  Result.Items[27, 2] := GetPressureString(SpringSolver.ShearModulus);

  Result.Items[28, 0] := 'rho';
  Result.Items[28, 1] := '=';
  Result.Items[28, 2] := GetDensityString(SpringSolver.MaterialDensity);

  Result.Items[29, 0] := 'Rm';
  Result.Items[29, 1] := '=';
  Result.Items[29, 2] := GetPressureString(SpringSolver.TensileStrengthRm);

  Result.Items[30, 0] := 'tauz';
  Result.Items[30, 1] := '=';
  Result.Items[30, 2] := GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz);

  Result.Items[31, 0] := 'ns';
  Result.Items[31, 1] := '=';
  Result.Items[31, 2] := Format('%0.2f', [SpringSolver.StaticSafetyFactor]);

  if SpringSolver.DynamicLoad then
  begin
    Result.Items[33, 0] := 'tauoz';
    Result.Items[33, 1] := '=';
    Result.Items[33, 2] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressTauoz);

    Result.Items[34, 0] := 'tauhz';
    Result.Items[34, 1] := '=';
    Result.Items[34, 2] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressRangeTauhz);

    Result.Items[35, 0] := 'nf';
    Result.Items[35, 1] := '=';
    Result.Items[35, 2] := Format('%0.2f', [ScalarUnit.ToFloat(SpringSolver.DynamicSafetyFactor)]);


    Result.Items[36, 0] := 'N';
    Result.Items[36, 1] := '=';
    Result.Items[36, 2] := '---';
    if SpringSolver.NumOfCycles > 0 then
    begin
      Result.Items[36, 2] := Format('%0.0f cycles', [ScalarUnit.ToFloat(SpringSolver.NumOfCycles)]);
    end;

    Result.Items[37, 0] := 'Nh';
    Result.Items[37, 1] := '=';
    Result.Items[37, 2] := '---';
    if SpringSolver.CycleFrequency > (0*Hz) then
    begin
      Result.Items[37, 2] := HourUnit.ToString(SpringSolver.NumOfCycles / SpringSolver.CycleFrequency, 5, 5, []);
    end;
  end;
  {$ENDIF}
end;

procedure TCompozer.DrawQuick1(var AScreen: TBGRABitmap; const AScreenScale: double);
var
  i: longint;
  Bit: array of TBGRABitmap = nil;
  ForceDiagram: TChart;
  GoodmanDiagram: TChart;
  MessageList: TReportTable;
  Quick1List: TReportTable;
  Quick1Table: TReportTable;
  QualityTable: TReportTable;
  SpringDrawing: TSpringDrawing;
begin
  SetLength(Bit, 9);
  for i := Low(Bit) to High(Bit) do
    Bit[i] := TBGRABitmap.Create;

  // 0-Quick-1 List
  Quick1List := CreateQuick1List1(aScreenScale);
  Bit[0].SetSize(Quick1List.Width, Quick1List.Height);
  Quick1List.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
  Quick1List.Destroy;

  // 1-Quick-2 List
  Quick1List := CreateQuick1List2(aScreenScale);
  Quick1List.Autosize := False;
  Bit[1].SetSize(Quick1List.Width, aScreen.Height - Bit[0].Height);
  Quick1List.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
  Bit[1].Draw(aScreen.Canvas, aScreen.Width - Bit[1].Width, Bit[0].Height, False);
  Quick1List.Destroy;

  // 2-Force & Displacement Chart
  ForceDiagram := CreateForceDisplacementChart(aScreenScale);
  Bit[2].SetSize((aScreen.Width - Bit[1].Width) div 2, aScreen.Height - Bit[0].Height);
  ForceDiagram.Draw(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);
  Bit[2].Draw(aScreen.Canvas, 0, aScreen.Height - Bit[2].Height, True);
  ForceDiagram.Destroy;

  // 3-Goodman Chart
  GoodmanDiagram := CreateGoodmanChart(aScreenScale);
  Bit[3].SetSize(aScreen.Width - Bit[2].Width - Bit[1].Width, Bit[2].Height);
  GoodmanDiagram.Draw(Bit[3].Canvas, Bit[3].Width, Bit[3].Height);
  Bit[3].Draw(aScreen.Canvas, Bit[2].Width, aScreen.Height - Bit[3].Height, True);
  GoodmanDiagram.Destroy;

  // 4-Quick-4 Table
  Quick1Table := CreateQuick1Table(aScreenScale);
  Quick1Table.Autosize := False;
  Bit[4].SetSize(Bit[1].Width + Bit[3].Width - Bit[0].Width, Quick1Table.Height);
  Quick1Table.Draw(Bit[4].Canvas, Bit[4].Width, Bit[4].Height);
  Bit[4].Draw(aScreen.Canvas, aScreen.Width - Bit[4].Width, 0, True);
  Quick1Table.Destroy;

  // 5-Quality Table
  QualityTable := CreateQualityTable(aScreenScale);
  QualityTable.Autosize := False;
  Bit[5].SetSize(QualityTable.Width, Bit[0].Height - Bit[4].Height);
  QualityTable.Draw(Bit[5].Canvas, Bit[5].Width, Bit[5].Height);
  Bit[5].Draw(aScreen.Canvas, aScreen.Width - Bit[4].Width, Bit[4].Height, True);
  QualityTable.Destroy;

  // 6-Message List
  MessageList := CreateMessageList(aScreenScale);
  MessageList.Autosize := False;
  Bit[6].SetSize(Bit[4].Width - Bit[5].Width, Bit[5].Height);
  MessageList.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);
  Bit[6].Draw(aScreen.Canvas, aScreen.Width - Bit[6].Width, Bit[4].Height, True);
  MessageList.Destroy;

  // 7-Spring Drawings
  SpringDrawing := CreateSpringDrawing(AScreenScale);
  {$IFDEF MODULE1}
  SpringDrawing.ClosedEnds := SpringSolver.ClosedEnds;
  SpringDrawing.GroundEnds := SpringSolver.GroundEnds;
  {$ENDIF}
  Bit[6].SetSize(Bit[2].Width div 3, Bit[0].Height);
  Bit[7].SetSize(Bit[2].Width div 3, Bit[0].Height);
  Bit[8].SetSize(Bit[2].Width - Bit[6].Width - Bit[7].Width, Bit[0].Height);

  {$IFDEF MODULE1}
  SpringDrawing.AutoFit := True;
  SpringDrawing.Lx := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
  SpringDrawing.Caption := Format('L0 = %s', [GetString(SpringDrawing.Lx)]);
  SpringDrawing.DrawInProfile(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := MeterUnit.ToFloat(SpringSolver.LengthL1, [pMilli]);
  SpringDrawing.Caption := Format('L1 = %s', [GetString(SpringDrawing.Lx)]);
  SpringDrawing.DrawInProfile(Bit[7].Canvas, Bit[7].Width, Bit[7].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := MeterUnit.ToFloat(SpringSolver.LengthL2, [pMilli]);
  SpringDrawing.Caption := Format('L2 = %s', [GetString(SpringDrawing.Lx)]);
  SpringDrawing.DrawInProfile(Bit[8].Canvas, Bit[8].Width, Bit[8].Height);

  Bit[6].Draw(aScreen.Canvas, 0, 0, True);
  Bit[7].Draw(aScreen.Canvas, Bit[6].Width, 0, True);
  Bit[8].Draw(aScreen.Canvas, Bit[6].Width + Bit[7].Width, 0, True);
  SpringDrawing.Destroy;
  {$ENDIF}

  // Drawing Quick-2 List
  Bit[0].Draw(aScreen.Canvas, Bit[2].Width, 0, True);

  for i := Low(Bit) to High(Bit) do
    Bit[i].Destroy;
  Bit := nil;
end;

procedure TCompozer.DrawQuickX(var aScreen: TBGRABitmap;
  const aScreenScale: double; X: longint);
var
  i, j: longint;
  Bit: array of TBGRABitmap = nil;
  CustomChart: TChart;
  QuickXList: TReportTable;
  MessageList: TReportTable;
  Row: longint;
begin
  SetLength(Bit, 3);
  for i := Low(Bit) to High(Bit) do
    Bit[i] := TBGRABitmap.Create;

  // 0-Quick-X List
  QuickXList := TReportTable.Create;
  QuickXList.ColumnCount := 7;
  QuickXList.RowCount := 26;
  QuickXList.Zoom := aScreenScale;
  LoadTable(QuickXList, 'QuickXList', 'Table');
  QuickXList.ColumnAlignments[0] := taLeftJustify;
  QuickXList.ColumnAlignments[1] := taCenter;
  QuickXList.ColumnAlignments[2] := taLeftJustify;
  QuickXList.ColumnAlignments[3] := taCenter;
  QuickXList.ColumnAlignments[4] := taLeftJustify;
  QuickXList.ColumnAlignments[5] := taCenter;
  QuickXList.ColumnAlignments[6] := taLeftJustify;

  for i := 0 to QuickXList.RowCount - 1 do
    for j := 0 to QuickXList.ColumnCount - 1 do
      QuickXList.Items[i, j] := '  ';

  //012
  Row := 0;
  QuickXList.Items[Row, 0] := 'd';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.WireDiameter, SpringSolver.WireDiameterTolerance);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.Di);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Dm';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.Dm, SpringTolerance.ToleranceOnCoilDiameter);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'De';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.De);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'n';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := Format('%0.2f coils', [SpringSolver.ActiveColis]);

  {$IFDEF MODULE1}
  Inc(Row);
  QuickXList.Items[Row, 0] := 'nt';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := Format('%0.2f colis', [SpringSolver.TotalCoils]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'nu';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := Format('%0.1f', [SpringSolver.SeatingCoefficent]);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.LengthL0, SpringTolerance.ToleranceFreeBodyLength);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L1';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.LengthL1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L2';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.LengthL2);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Ln';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.LengthLn);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Lc';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := GetLengthString(SpringSolver.LengthLc);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'Quality specs.';
  QuickXList.Items[Row, 2] := 'EN15800';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di, Dm, De';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnCoilDiameter) + 1]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnFreeBodyLength) + 1]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnLoad1) + 1]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnLoad2) + 1]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnPerpendicularity) + 1]);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := Format('grade %d', [Ord(SpringTolerance.QualityGradeOnParallelism) + 1]);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'Spring ends';
  QuickXList.Items[Row, 1] := '=';
  if SpringSolver.ClosedEnds then
  begin
    if SpringSolver.GroundEnds then
      QuickXList.Items[Row, 2] := 'Closed and ground'
    else
      QuickXList.Items[Row, 2] := 'Closed';
  end
  else
    QuickXList.Items[Row, 2] := 'Open';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Coiling type';
  QuickXList.Items[Row, 1] := '=';
  if SpringSolver.ColdCoiled then
    QuickXList.Items[Row, 2] := 'Cold coiled'
  else
    QuickXList.Items[Row, 2] := 'Hot coiled';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Load type';
  QuickXList.Items[Row, 1] := '=';
  if SpringSolver.DynamicLoad then
    QuickXList.Items[Row, 2] := 'Dynamic'
  else
    QuickXList.Items[Row, 2] := 'Static';

  // 456
  Row := 2;
  QuickXList.Items[Row, 4] := 'Material';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := Format('%s', [MAT.Name]);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Rm';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.TensileStrengthRm);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'G';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.ShearModulus);

  Inc(Row);
  QuickXList.Items[Row, 4] := Format('G(%s)', [GetTemperatureString(MAT.Temperature)]);
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(MAT.GetG(MAT.Temperature));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'rho';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetDensityString(SpringSolver.MaterialDensity);

  Inc(Row, 3);
  QuickXList.Items[Row, 4] := 'F1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetForceString(SpringSolver.LoadF1, SpringTolerance.ToleranceOnLoad1);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'F2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetForceString(SpringSolver.LoadF2, SpringTolerance.ToleranceOnLoad2);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fn';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetForceString(SpringSolver.LoadFn);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fc';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetForceString(SpringSolver.LoadFc);

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauk1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.TorsionalStressTauk1);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauk2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.TorsionalStressTauk2);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'taukh';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.TorsionalStressTaukh);

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauhz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressRangeTauhz);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauoz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.AdmDynamicTorsionalStressTauoz);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz);

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'Static safety factor';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := Format('%0.2f', [SpringSolver.StaticSafetyFactor]);

  if SpringSolver.DynamicLoad then
  begin
    Inc(Row);
    QuickXList.Items[Row, 4] := 'Dynamic safety factor';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := Format('%0.2f', [ScalarUnit.ToFloat(SpringSolver.DynamicSafetyFactor)]);

    Inc(Row);
    QuickXList.Items[Row, 4] := 'N';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := '---';
    if SpringSolver.NumOfCycles > 0 then
    begin
      QuickXList.Items[Row, 4] := 'N';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := Format('%0.0f cycles', [ScalarUnit.ToFloat(SpringSolver.NumOfCycles)]);
    end;

    Inc(Row);
    QuickXList.Items[Row, 4] := 'Nh';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := '---';
    if GreaterThanZero(SpringSolver.CycleFrequency) then
    begin
      QuickXList.Items[Row, 4] := 'Nh';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := HourUnit.ToString(SpringSolver.NumOfCycles / SpringSolver.CycleFrequency);
    end;
  end;

  // 0-List
  Bit[0].SetSize(QuickXList.Width, QuickXList.Height);
  QuickXList.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
  Bit[0].Draw(aScreen.Canvas, aScreen.Width - Bit[0].Width, 0, True);
  QuickXList.Destroy;

  // 1-Messages
  MessageList := CreateMessageList(aScreenScale);
  MessageList.Autosize := False;
  Bit[1].SetSize(Bit[0].Width, aScreen.Height - Bit[0].Height);
  MessageList.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
  Bit[1].Draw(aScreen.Canvas, aScreen.Width - Bit[0].Width, Bit[0].Height, True);
  MessageList.Destroy;

  // 2-Force or Goodman chart
  if X = 3 then
    CustomChart := CreateGoodmanChart(aScreenScale)
  else
    CustomChart := CreateForceDisplacementChart(aScreenScale);

  Bit[2].SetSize(aScreen.Width - Bit[0].Width, aScreen.Height);
  CustomChart.Draw(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);
  Bit[2].Draw(aScreen.Canvas, 0, 0, True);
  CustomChart.Destroy;
  {$ENDIF}

  for i := Low(Bit) to High(Bit) do
    Bit[i].Destroy;
  Bit := nil;
end;

procedure TCompozer.DrawQuick2(var aScreen: TBGRABitmap; const aScreenScale: double);
begin
  DrawQuickX(aScreen, aScreenScale, 2);
end;

procedure TCompozer.DrawQuick3(var aScreen: TBGRABitmap; const aScreenScale: double);
begin
  DrawQuickX(aScreen, aScreenScale, 3);
end;

procedure TCompozer.DrawMessageList(var aScreen: TBGRABitmap; const aScreenScale: double);
var
  Table: TReportTable;
begin
  Table := CreateMessageList(aScreenScale);
  Table.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  Table.Destroy;
end;

end.
