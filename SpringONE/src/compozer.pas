unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, Classes, GraphBase, Graphics, IniFiles, Math, SysUtils;

procedure DrawQuick1(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);
procedure DrawQuick2(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);
procedure DrawQuick3(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);



function CreateForceDisplacementChart  (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateGoodmanChart            (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateBucklingChart           (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateLoadF1Chart             (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateLoadF2Chart             (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateShearModulusChart       (const aScreenScale: double; aSetting: TIniFile): TChart;
function CreateYoungModulusChart       (const aScreenScale: double; aSetting: TIniFile): TChart;

function CreateQualityTable            (const aScreenScale: double; aSetting: TIniFile): TReportTable;
function CreateQuick1Table             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
function CreateQuick1List              (const aScreenScale: double; aSetting: TIniFile): TReportTable;
function CreateMessageList             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
function CreateGoodmanList             (const aScreenScale: double; aSetting: TIniFile): TReportTable;

function CreateQuick1AList             (const aScreenScale: double; aSetting: TIniFile): TReportTable;





implementation

uses
  EN10270, EN13906, EN15800, UtilsBase, ApplicationFrm;


function LoadFontStyle(const ASection, AIdent: string; ASetting: TIniFile): TFontStyles;
var
  FontStyle: string;
begin
  Result := [];

  FontStyle := ASetting.ReadString(ASection, AIdent , 'Bold');
  if Pos('Bold',      FontStyle) > 0 then Include(Result, fsBold);
  if Pos('Italic',    FontStyle) > 0 then Include(Result, fsItalic);
  if Pos('Underline', FontStyle) > 0 then Include(Result, fsUnderline);
  if Pos('StrikeOut', FontStyle) > 0 then Include(Result, fsStrikeOut);
end;

function LoadPenStyle(const ASection, AIdent: string; ASetting: TIniFile): TPenStyle;
var
  PenStyle: string;
begin
  Result := psSolid;

  PenStyle := ASetting.ReadString(ASection, AIdent , 'Solid');
  if PenStyle = 'Solid'       then Result := psSolid;
  if PenStyle = 'Dash'        then Result := psDash;
  if PenStyle = 'Dot'         then Result := psDot;
  if PenStyle = 'DashDot'     then Result := psDashDot;
  if PenStyle = 'DashDotDot'  then Result := psDashDotDot;
  if PenStyle = 'InsideFrame' then Result := psInsideFrame;
  if PenStyle = 'Pattern'     then Result := psPattern;
  if PenStyle = 'Clear'       then Result := psClear;
end;

procedure LoadChart1(Chart: TChart; const ASection: string; ASetting: TIniFile);
begin
  // Title
  Chart.TitleFontName   := aSetting.ReadString(ASection, 'TitleFontName', 'default');
  Chart.TitleFontHeight := aSetting.ReadFloat(ASection, 'TitleFontHeight', 14);
  Chart.TitleFontStyle  := LoadFontStyle(ASection, 'TitleFontStyle', ASetting);
  Chart.TitleFontColor.FromString(aSetting.ReadString(ASection, 'TitleFontColor', 'Black'));
  // X Axis
  Chart.XAxisFontName   := aSetting.ReadString(ASection, 'XAxisFontName', 'default');
  Chart.XAxisFontHeight := aSetting.ReadFloat (ASection, 'XAxisFontHeight', 13);
  Chart.XAxisFontStyle  := LoadFontStyle(ASection, 'XAxisFontStyle', ASetting);
  Chart.XAxisFontColor.FromString(aSetting.ReadString(ASection, 'XAxisFontColor', 'Black' ));
  Chart.XAxisLineColor.FromString(aSetting.ReadString(ASection, 'XAxisLineColor', 'Black' ));
  Chart.XGridLineColor.FromString(aSetting.ReadString(ASection, 'XGridLineColor', 'Silver'));
  Chart.XAxisLineWidth  := aSetting.ReadFloat(ASection, 'XAxisLineWidth' , 1.0);
  Chart.XGridLineWidth  := aSetting.ReadFloat(ASection, 'XGridLineWidth' , 1.0);
  Chart.XAxisLineStyle  := LoadPenStyle(ASection, 'XAxisLineStyle', ASetting);
  Chart.XGridLineStyle  := LoadPenStyle(ASection, 'XGridLineStyle', ASetting);
  // Y Axis
  Chart.YAxisFontName   := aSetting.ReadString(ASection, 'YAxisFontName', 'default');
  Chart.YAxisFontHeight := aSetting.ReadFloat (ASection, 'YAxisFontHeight', 13);
  Chart.YAxisFontStyle  := LoadFontStyle(ASection, 'YAxisFontStyle' , ASetting);
  Chart.YAxisFontColor.FromString(aSetting.ReadString(ASection, 'YAxisFontColor', 'Black' ));
  Chart.YAxisLineColor.FromString(aSetting.ReadString(ASection, 'YAxisLineColor', 'Black' ));
  Chart.YGridLineColor.FromString(aSetting.ReadString(ASection, 'YGridLineColor', 'Silver'));
  Chart.YAxisLineWidth  := aSetting.ReadFloat(ASection, 'YAxisLineWidth' , 1.0);
  Chart.YGridLineWidth  := aSetting.ReadFloat(ASection, 'YGridLineWidth' , 1.0);
  Chart.YAxisLineStyle  := LoadPenStyle(ASection, 'YAxisLineStyle' , ASetting);
  Chart.YGridLineStyle  := LoadPenStyle(ASection, 'YGridLineStyle' , ASetting);
  // General
  Chart.Color          .FromString(aSetting.ReadString(ASection, 'Color', 'White'));
  Chart.BackgroundColor.FromString(aSetting.ReadString(ASection, 'BackgroundColor', 'White'));
end;

procedure LoadChart2(Chart: TChart; const ASection, AIdent: string; ASetting: TIniFile);
begin
  Chart.FontName   := aSetting.ReadString(ASection,  AIdent + '.FontName', 'default');
  Chart.FontHeight := aSetting.ReadFloat (ASection,  AIdent + '.FontHeight', 12);
  Chart.FontStyle  := LoadFontStyle(ASection, AIdent + '.FontStyle' , ASetting);
  Chart.FontColor.FromString(aSetting.ReadString(ASection, AIdent + '.FontColor', 'Black' ));

  Chart.PenWidth   := aSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Chart.PenStyle   := LoadPenStyle(ASection, AIdent + '.PenStyle', ASetting);
  Chart.PenColor.FromString(aSetting.ReadString (ASection, AIdent + '.PenColor',  'Black'));

  Chart.TextureBackgroundColor.FromString(
    aSetting.ReadString(ASection, AIdent + '.TextureBackgroundColor', 'White' ));

  Chart.TextureColor.FromString(
    aSetting.ReadString(ASection, AIdent + '.TextureColor', 'Black' ));

  Chart.TextureWidth    := aSetting.ReadInteger(ASection, AIdent + '.TextureWidth',      8);
  Chart.TextureHeight   := aSetting.ReadInteger(ASection, AIdent + '.TextureHeight',     8);
  Chart.TexturePenWidth := aSetting.ReadFloat  (ASection, AIdent + '.TexturePenWidth', 1.0);
end;

procedure LoadTable(Table: TReportTable; const ASection, AIdent: string; ASetting: TIniFile);
begin
  Table.BackgroundColor.FromString(aSetting.ReadString(ASection, AIdent + '.BackgroundColor', 'White'));
  Table.BorderWidth:= aSetting.ReadInteger(ASection, AIdent + '.BorderWidth', 0);

  Table.FontName   := aSetting.ReadString(ASection,  AIdent + '.FontName', 'default');
  Table.FontHeight := aSetting.ReadFloat (ASection,  AIdent + '.FontHeight', 12);
  Table.FontStyle  := LoadFontStyle(ASection, AIdent + '.FontStyle' , ASetting);
  Table.FontColor.FromString(aSetting.ReadString(ASection, AIdent + '.FontColor', 'Black' ));
  Table.PenWidth   := aSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Table.PenStyle   := LoadPenStyle(ASection, AIdent + '.PenStyle', ASetting);
  Table.PenColor.FromString(aSetting.ReadString (ASection, AIdent + '.PenColor',  'Black'));

  Table.RowSpacer := aSetting.ReadInteger(ASection, AIdent + '.RowSpacer', 0);
  Table.ColumnSpacer := aSetting.ReadInteger(ASection, AIdent + '.ColumnSpacer', 0); ;
end;

//

function CreateForceDisplacementChart(const AScreenScale: double; ASetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'ForceDisplacementChart';
var
  Points: ArrayOfTPointF = nil;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Force & Displacement Chart';
  Result.XAxisLabel    := 's[mm]';
  Result.YAxisLabel    := 'F[N]';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  // Bisector line
  LoadChart2(Result, Section2, 'BisectorLine', ASetting);
  SetLength(Points, 2);
  Points[0].X := 0;
  Points[0].Y := 0;
  Points[1].X := SOLVER.DeflectionSc;
  Points[1].Y := SOLVER.LoadFc;
  Result.AddPolyLine(Points, True, 'Bisector');
  Points := nil;
  // Tolerance lines
  LoadChart2(Result, Section2, 'ToleranceLine', ASetting);
  SetLength(Points, 2);
  Points[0].X := SOLVER.DeflectionS1;
  Points[0].Y := TOL.LoadF1 + TOL.LoadF1Tolerance;
  Points[1].X := SOLVER.DeflectionS2;
  Points[1].Y := TOL.LoadF2 + TOL.LoadF2Tolerance;
  Result.AddPolyLine(Points, True, 'Tolerance +');
  Points := nil;

  SetLength(Points, 2);
  Points[0].X := SOLVER.DeflectionS1;
  Points[0].Y := TOL.LoadF1 - TOL.LoadF1Tolerance;
  Points[1].X := SOLVER.DeflectionS2;
  Points[1].Y := TOL.LoadF2 - TOL.LoadF2Tolerance;
  Result.AddPolyLine(Points, True, 'Tolerance -');
  Points := nil;
  // Load-F1
  LoadChart2(Result, Section2, 'Load-F1', ASetting);
  SetLength(Points, 3);
  Points[0].X := 0;
  Points[0].Y := SOLVER.LoadF1;
  Points[1].X := SOLVER.DeflectionS1;
  Points[1].Y := SOLVER.LoadF1;
  Points[2].X := SOLVER.DeflectionS1;
  Points[2].Y := 0;
  Result.AddPolyLine(Points, False, 'F1');
  Points := nil;
  // Load F2
  LoadChart2(Result, Section2, 'Load-F2', ASetting);
  SetLength(Points, 3);
  Points[0].X := 0;
  Points[0].Y := SOLVER.LoadF2;
  Points[1].X := SOLVER.DeflectionS2;
  Points[1].Y := SOLVER.LoadF2;
  Points[2].X := SOLVER.DeflectionS2;
  Points[2].Y := 0;
  Result.AddPolyLine(Points, False, 'F2');
  Points := nil;
  // Load Fn
  LoadChart2(Result, Section2, 'Load-Fn', ASetting);
  SetLength(Points, 3);
  Points[0].X := 0;
  Points[0].Y := SOLVER.LoadFn;
  Points[1].X := SOLVER.DeflectionSn;
  Points[1].Y := SOLVER.LoadFn;
  Points[2].X := SOLVER.DeflectionSn;
  Points[2].Y := 0;
  Result.AddPolyLine(Points, False, 'Fn');
  Points := nil;
  // Load Fc
  LoadChart2(Result, Section2, 'Load-Fc', ASetting);
  SetLength(Points, 3);
  Points[0].X := 0;
  Points[0].Y := SOLVER.LoadFc;
  Points[1].X := SOLVER.DeflectionSc;
  Points[1].Y := SOLVER.LoadFc;
  Points[2].X := SOLVER.DeflectionSc;
  Points[2].Y := 0;
  Result.AddPolyLine(Points, False, 'Fc');
  Points := nil;
  // Label Load-F1
  LoadChart2(Result, Section2, 'Load-F1', ASetting);
  Result.AddLabel(0, SOLVER.LoadF1, Trunc(32*AScreenScale), 0, taLeftJustify, taAlignBottom, 'F1');
  // Label Load-F2
  LoadChart2(Result, Section2, 'Load-F2', ASetting);
  Result.AddLabel(0, SOLVER.LoadF2, Trunc(64*AScreenScale), 0, taLeftJustify, taAlignBottom, 'F2');
  // Label Load-Fn
  LoadChart2(Result, Section2, 'Load-Fn', ASetting);
  Result.AddLabel(0, SOLVER.LoadFn, Trunc(96*AScreenScale), 0, taLeftJustify, taAlignBottom, 'Fn');
  // Label Load-Fc
  LoadChart2(Result, Section2, 'Load-Fc', ASetting);
  Result.AddLabel(0, SOLVER.LoadFc, Trunc(128*AScreenScale), 0, taLeftJustify, taAlignBottom, 'Fc');
end;

function CreateGoodmanChart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'GoodmanChart';
var
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  if MAT.ItemIndex <> -1 then
    Result.Title       := Format('Goodman Chart: %s', [MAT.Items[MAT.ItemIndex]])
  else
    Result.Title       := Format('Goodman Chart: %s', ['Custom Material']);
  Result.XAxisLabel    := 'tau U';
  Result.YAxisLabel    := 'tau O';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  // Bisector line
  LoadChart2(Result, Section2, 'BisectorLine', ASetting);
  SetLength(Points, 2);
  Points[0].X := 0;
  Points[0].Y := 0;
  Points[1].X := SOLVER.AdmStaticTorsionalStressTauz;
  Points[1].Y := SOLVER.AdmStaticTorsionalStressTauz;
  Result.AddPolyLine(Points, True, 'Bisector');
  Points := nil;
  // TaulTol
  LoadChart2(Result, Section2, 'TaukTol', ASetting);
  SetLength(Points, 4);
  Points[0].X := Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[0].Y := Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[1].X := Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[1].Y := Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[2].X := Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[2].Y := Solver.TorsionalStressTauk2 + SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[3].X := Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Points[3].Y := Solver.TorsionalStressTauk2 - SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Result.AddPolygon(Points, 'TaukTol');
  Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk1');
  Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk2');
  Points := nil;

  LoadChart2(Result, Section2, 'Tauk', ASetting);
  SetLength(Points, 2);
  Points[0].X := Solver.TorsionalStressTauk1;
  Points[0].Y := Solver.TorsionalStressTauk1;
  Points[1].X := Solver.TorsionalStressTauk1;
  Points[1].Y := Solver.TorsionalStressTauk2;
  Result.AddPolyLine(Points, False, 'Tauk1-Tauk2');
  Points := nil;

  if MAT.ItemIndex <> -1 then
  begin
    LoadChart2(Result, Section2, '1E5', ASetting);
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := MAT.TorsionalStressTauOE5;
    Points[1].X := MAT.TorsionalStressTauUE5;
    Points[1].Y := MAT.TorsionalStressTauYield;
    Points[2].X := MAT.TorsionalStressTauUE6;
    Points[2].Y := MAT.TorsionalStressTauYield;
    Result.AddPolyLine(Points, False, '1E5 Cycles');
    Result.AddLabel(MAT.TorsionalStressTauUE5, MAT.TorsionalStressTauYield,
      0, 0, taLeftJustify, taAlignBottom, '1E5');
    Points := nil;

    LoadChart2(Result, Section2, '1E6', ASetting);
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := MAT.TorsionalStressTauOE6;
    Points[1].X := MAT.TorsionalStressTauUE6;
    Points[1].Y := MAT.TorsionalStressTauYield;
    Points[2].X := MAT.TorsionalStressTauUE7;
    Points[2].Y := MAT.TorsionalStressTauYield;
    Result.AddPolyLine(Points, False, '1E6 Cycles');
    Result.AddLabel(MAT.TorsionalStressTauUE6, MAT.TorsionalStressTauYield,
      0, 0, taLeftJustify, taAlignBottom, '1E6');
    Points := nil;

    LoadChart2(Result, Section2, '1E7', ASetting);
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := MAT.TorsionalStressTauOE7;
    Points[1].X := MAT.TorsionalStressTauUE7;
    Points[1].Y := MAT.TorsionalStressTauYield;
    Points[2].X := MAT.TorsionalStressTauYield;
    Points[2].Y := MAT.TorsionalStressTauYield;
    Result.AddPolyLine(Points, False, '1E7 Cycles');
    Result.AddLabel(MAT.TorsionalStressTauUE7, MAT.TorsionalStressTauYield,
      0, 0, taLeftJustify, taAlignBottom, '1E7 Cycles');
    Points := nil;
  end;
end;

function CreateBucklingChart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'BucklingChart';
var
  I: longint;
  X, Y: single;
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Buckling diagram';
  Result.XAxisLabel    := 'nu*L0/D';
  Result.YAxisLabel    := 's/L0';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  // Buckling Curve
  LoadChart2(Result, Section2, 'BucklingCurve', ASetting);
  Points := nil;
  SOLVER.GetBucklingCurve(Points);

  Result.XMinF   := 0.0;
  Result.YMinF   := 0.0;
  Result.YMaxF   := 1.0;
  Result.YDeltaF := 0.1;
  Result.YCount  := 10;

  Result.AddPolyLine(Points, False, 'Buckling-Curve');
  Points := nil;

  LoadChart2(Result, Section2, 'Sc', ASetting);
  X := SOLVER.SeatingCoefficent*SOLVER.LengthL0/SOLVER.Dm;
  if SOLVER.DeflectionSc > SOLVER.DeflectionSk then
    Y := SOLVER.DeflectionSc / SOLVER.LengthL0
  else
    Y := SOLVER.DeflectionSk / SOLVER.LengthL0;

  SetLength(Points, 2);
  Points[0].x := X;
  Points[0].y := 0;
  Points[1].x := X;
  Points[1].y := Y;
  Result.AddPolyLine(Points, False, 'Sc');
  Result.AddDotLabel(X, SOLVER.DeflectionSc/SOLVER.LengthL0, 5, 10, 0, taLeftJustify, taVerticalCenter, 'Sc');
  Points := nil;

  LoadChart2(Result, Section2, 'Sx', ASetting);
  Result.AddDotLabel(X, SOLVER.DeflectionS1/SOLVER.LengthL0, 5, 10, 0, taLeftJustify, taVerticalCenter, 'S1');
  Result.AddDotLabel(X, SOLVER.DeflectionS2/SOLVER.LengthL0, 5, 10, 0, taLeftJustify, taVerticalCenter, 'S2');
end;

function CreateLoadF1Chart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'LinearChart';
var
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Load F1-Temperature Chart';
  Result.XAxisLabel    := 'T [C°]';
  Result.YAxisLabel    := 'F1 [N]';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  //
  LoadChart2(Result, Section2, 'Line', ASetting);
  SetLength(Points, 2);
  Points[0].x:= MAT.Tempetature - 50;
  Points[0].y:= SOLVER.GetF1(Points[0].x);
  Points[1].x:= MAT.Tempetature + 50;
  Points[1].y:= SOLVER.GetF1(Points[1].x);
  Result.AddPolyLine(Points, True, 'F1(T°)');
  Points := nil;

  Result.AddDotLabel(MAT.Tempetature, SOLVER.GetF1(MAT.Tempetature), 5, 0, 10,
    taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
end;

function CreateLoadF2Chart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'LinearChart';
var
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Load F2-Temperature Chart';
  Result.XAxisLabel    := 'T [C°]';
  Result.YAxisLabel    := 'F2 [N]';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  //
  LoadChart2(Result, Section2, 'Line', ASetting);
  SetLength(Points, 2);
  Points[0].x:= MAT.Tempetature - 50;
  Points[0].y:= SOLVER.GetF2(Points[0].x);
  Points[1].x:= MAT.Tempetature + 50;
  Points[1].y:= SOLVER.GetF2(Points[1].x);
  Result.AddPolyLine(Points, True, 'F2(T°)');
  Points := nil;

  Result.AddDotLabel(MAT.Tempetature, SOLVER.GetF2(MAT.Tempetature), 5, 0, 10,
    taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
end;

function CreateShearModulusChart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'LinearChart';
var
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Shear Modulus G-Temperature Chart';
  Result.XAxisLabel    := 'T [C°]';
  Result.YAxisLabel    := 'G [MPa]';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  //
  LoadChart2(Result, Section2, 'Line', ASetting);
  SetLength(Points, 2);
  Points[0].x:= MAT.Tempetature - 50;
  Points[0].y:= MAT.GetG(Points[0].x);
  Points[1].x:= MAT.Tempetature + 50;
  Points[1].y:= MAT.GetG(Points[1].x);
  Result.AddPolyLine(Points, True, 'G(T°)');
  Points := nil;

  Result.AddDotLabel(MAT.Tempetature, MAT.GetG(MAT.Tempetature), 5, 0, 10,
    taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
end;

function CreateYoungModulusChart(const aScreenScale: double; aSetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'LinearChart';
var
  Points: ArrayOfTPointF;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title         := 'Young Modulus G-Temperature Chart';
  Result.XAxisLabel    := 'T [C°]';
  Result.YAxisLabel    := 'E [MPa]';
  Result.Zoom          := aScreenScale;
  //
  LoadChart1(Result, Section1, ASetting);
  //
  LoadChart2(Result, Section2, 'Line', ASetting);
  SetLength(Points, 2);
  Points[0].x:= MAT.Tempetature - 50;
  Points[0].y:= MAT.GetE(Points[0].x);
  Points[1].x:= MAT.Tempetature + 50;
  Points[1].y:= MAT.GetE(Points[1].x);
  Result.AddPolyLine(Points, True, 'E(T°)');
  Points := nil;

  Result.AddDotLabel(MAT.Tempetature, MAT.GetE(MAT.Tempetature), 5, 0, 10,
    taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
end;

//

function CreateQualityTable(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 5;
  Result.RowCount    := 7;
  Result.Zoom        := aScreenScale;
  LoadTable(Result, 'CommonTable', 'Table', ASetting);

  Result[0, 0] := 'Quality Grade';
  Result[1, 0] := 'De, Di';
  Result[2, 0] := 'L0';
  Result[3, 0] := 'F1';
  Result[4, 0] := 'F2';
  Result[5, 0] := 'e1';
  Result[6, 0] := 'e2';

  Result[0, 1] := '1';
  Result[1, 1] := TryFormatBool('x', ' ', TOL.DmQualityGrade = 1);
  Result[2, 1] := TryFormatBool('x', ' ', TOL.L0QualityGrade = 1);
  Result[3, 1] := TryFormatBool('x', ' ', TOL.F1QualityGrade = 1);
  Result[4, 1] := TryFormatBool('x', ' ', TOL.F2QualityGrade = 1);
  Result[5, 1] := TryFormatBool('x', ' ', TOL.E1QualityGrade = 1);
  Result[6, 1] := TryFormatBool('x', ' ', TOL.E2QualityGrade = 1);

  Result[0, 2] := '2';
  Result[1, 2] := TryFormatBool('x', ' ', TOL.DmQualityGrade = 2);
  Result[2, 2] := TryFormatBool('x', ' ', TOL.L0QualityGrade = 2);
  Result[3, 2] := TryFormatBool('x', ' ', TOL.F1QualityGrade = 2);
  Result[4, 2] := TryFormatBool('x', ' ', TOL.F2QualityGrade = 2);
  Result[5, 2] := TryFormatBool('x', ' ', TOL.E1QualityGrade = 2);
  Result[6, 2] := TryFormatBool('x', ' ', TOL.E2QualityGrade = 2);

  Result[0, 3] := '3';
  Result[1, 3] := TryFormatBool('x', ' ', TOL.DmQualityGrade = 3);
  Result[2, 3] := TryFormatBool('x', ' ', TOL.L0QualityGrade = 3);
  Result[3, 3] := TryFormatBool('x', ' ', TOL.F1QualityGrade = 3);
  Result[4, 3] := TryFormatBool('x', ' ', TOL.F2QualityGrade = 3);
  Result[5, 3] := TryFormatBool('x', ' ', TOL.E1QualityGrade = 3);
  Result[6, 3] := TryFormatBool('x', ' ', TOL.E2QualityGrade = 3);

  Result[0, 4] := 'Tol.';
  Result[1, 4] := TryFormatFloat('%s mm', ' --- ', TOL.CoilDiameterTolerance);
  Result[2, 4] := TryFormatFloat('%s mm', ' --- ', TOL.LengthL0Tolerance);
  Result[3, 4] := TryFormatFloat('%s N',  ' --- ', TOL.LoadF1Tolerance);
  Result[4, 4] := TryFormatFloat('%s N',  ' --- ', TOL.LoadF2Tolerance);
  Result[5, 4] := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE1);
  Result[6, 4] := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE2);
end;

function CreateQuick1Table(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount  := 7;
  Result.RowCount     := 6;
  Result.Zoom         := aScreenScale;
  LoadTable(Result, 'CommonTable', 'Table', ASetting);

  Result[0, 0] := 'L [mm]';
  Result[1, 0] := TryFormatFloat('L0: %s', 'L0: ---', SOLVER.LengthL0);
  Result[2, 0] := TryFormatFloat('L1: %s', 'L1: ---', SOLVER.LengthL1);
  Result[3, 0] := TryFormatFloat('L2: %s', 'L2: ---', SOLVER.LengthL2);
  Result[4, 0] := TryFormatFloat('Ln: %s', 'Ln: ---', SOLVER.LengthLn);
  Result[5, 0] := TryFormatFloat('Lc: %s', 'Lc: ---', SOLVER.LengthLc);

  Result[0, 1] := 'F [N]';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('F1: %s', 'F1: ---', SOLVER.LoadF1);
  Result[3, 1] := TryFormatFloat('F2: %s', 'F2: ---', SOLVER.LoadF2);
  Result[4, 1] := TryFormatFloat('Fn: %s', 'Fn: ---', SOLVER.LoadFn);
  Result[5, 1] := TryFormatFloat('Fc: %s', 'Fc: ---', SOLVER.LoadFc);

  Result[0, 2] := 'tau [MPa]';
  Result[1, 2] := '';
  Result[2, 2] := TryFormatFloat('tauk1: %s', 'tauk1: ---', SOLVER.TorsionalStressTauk1);
  Result[3, 2] := TryFormatFloat('tauk2: %s', 'tauk2: ---', SOLVER.TorsionalStressTauk2);
  Result[4, 2] := TryFormatFloat('tau n: %s', 'tau n: ---', SOLVER.TorsionalStressTaun);
  Result[5, 2] := TryFormatFloat('tau c: %s', 'tau c: ---', SOLVER.TorsionalStressTauc);

  Result[0, 3] := 's [mm]';
  Result[1, 3] := '';
  Result[2, 3] := TryFormatFloat('s1: %s', 's1: ---', SOLVER.DeflectionS1);
  Result[3, 3] := TryFormatFloat('s2: %s', 's2: ---', SOLVER.DeflectionS2);
  Result[4, 3] := TryFormatFloat('sn: %s', 'sn: ---', SOLVER.DeflectionSn);
  Result[5, 3] := TryFormatFloat('sc: %s', 'sc: ---', SOLVER.DeflectionSc);

  Result[0, 4] := 'tau/tauz';
  Result[1, 4] := '';
  Result[2, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1, SOLVER.AdmStaticTorsionalStressTauz);
  Result[3, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2, SOLVER.AdmStaticTorsionalStressTauz);
  Result[4, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun, SOLVER.AdmStaticTorsionalStressTauz);
  Result[5, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc, SOLVER.AdmStaticTorsionalStressTauz);

  Result[0, 5] := 'tau/Rm';
  Result[1, 5] := '';
  Result[2, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1, SOLVER.TensileStrengthRm);
  Result[3, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2, SOLVER.TensileStrengthRm);
  Result[4, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun, SOLVER.TensileStrengthRm);
  Result[5, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc, SOLVER.TensileStrengthRm);

  Result[0, 6] := 'De';
  Result[1, 6] := TryFormatFloat      ('%s', '---', SOLVER.De);
  Result[2, 6] := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionS1, SOLVER.DeflectionSc);
  Result[3, 6] := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionS2, SOLVER.DeflectionSc);
  Result[4, 6] := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionSn, SOLVER.DeflectionSc);
  Result[5, 6] := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionSc, SOLVER.DeflectionSc);
end;

function CreateQuick1List(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount    := 20;
  Result.Zoom        := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table', ASetting);

  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  Result.Items[ 0, 0] := 'd';
  Result.Items[ 0, 1] := '=';
  Result.Items[ 0, 2] := TryFormatFloat('%s', '---', SOLVER.WireDiameter) +
    TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);

  Result.Items[ 1, 0] := 'Di';
  Result.Items[ 1, 1] := '=';
  Result.Items[ 1, 2] := TryFormatFloat('%s mm', '---', SOLVER.Di);

  Result.Items[ 2, 0] := 'Dm';
  Result.Items[ 2, 1] := '=';
  Result.Items[ 2, 2] := TryFormatFloat('%s mm', '---', SOLVER.Dm);

  Result.Items[ 3, 0] := 'De';
  Result.Items[ 3, 1] := '=';
  Result.Items[ 3, 2] := TryFormatFloat('%s', '---', SOLVER.De) +
    TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);

  Result.Items[ 4, 0] := 'n';
  Result.Items[ 4, 1] := '=';
  Result.Items[ 4, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);

  Result.Items[ 5, 0] := 'nt';
  Result.Items[ 5, 1] := '=';
  Result.Items[ 5, 2] := TryFormatFloat('%s coils', '---', SOLVER.TotalCoils);

  Result.Items[ 6, 0] := 'nt';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := TryFormatFloat('%s N/mm',  '---', SOLVER.SpringRateR);

  Result.Items[ 7, 0] := 'Dec';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := TryFormatFloat('%s mm', '---', SOLVER.De + SOLVER.DeltaDe);

  Result.Items[ 8, 0] := 'Di.min';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := TryFormatFloat('%s mm', '---', SOLVER.DiMin);

  Result.Items[ 9, 0] := 'De.max';
  Result.Items[ 9, 1] := '=';
  Result.Items[ 9, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeMax);

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionSk);

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s mm', '---', SOLVER.WireLength);

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s g', '---', SOLVER.Mass);

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s Nmm', '---', SOLVER.SpringWorkW12);

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s Nmm', '---', SOLVER.SpringWorkW0n);

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s Hz', '---' ,SOLVER.NaturalFrequency);

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s mm', '---', SOLVER.Pitch);

  Result.Items[17, 0] := 'PitchRatio';
  Result.Items[17, 1] := '=';
  Result.Items[17, 2] := TryFormatFloat('%s', '---', SOLVER.PitchRatio);

  Result.Items[18, 0] := 'nu';
  Result.Items[18, 1] := '=';
  Result.Items[18, 2] := TryFormatFloat('%s', '---', SOLVER.SeatingCoefficent);

  Result.Items[19, 0] := 'dynamic load';
  Result.Items[19, 1] := '=';

  if SOLVER.DynamicLoad then
    Result.Items[19, 2] := ('true')
  else
    Result.Items[19, 2] := ('false');

end;

function CreateMessageList(const aScreenScale: double; aSetting: TIniFile): TReportTable;
var
  i, j: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount  := 1;
  Result.RowCount     := 1 + ErrorMessage.Count + WarningMessage.Count;
  Result.Zoom         := aScreenScale;
  LoadTable(Result, 'MessageList', 'Table', ASetting);

  Result.Items[0, 0] := TryFormatBool('Messages:', '', (ErrorMessage.Count + WarningMessage.Count) > 0);

  j := 1;
  for i := 0 to ErrorMessage.Count -1 do
  begin
    Result.Items[j, 0] := ErrorMessage[i];
    Inc(j);
  end;

  for i := 0 to WarningMessage.Count -1 do
  begin
    Result.Items[j, 0] := WarningMessage[i];
    Inc(j);
  end;
end;

function CreateGoodmanList(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result := TReportTable.Create;
  Result.RowCount    := 16;
  Result.ColumnCount := 3;
  Result.Zoom        := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table', aSetting);

  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  Result.Items[ 0, 0] := 'tauk1';
  Result.Items[ 0, 1] := '=';
  Result.Items[ 0, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk1);

  Result.Items[ 1, 0] := 'tauk2';
  Result.Items[ 1, 1] := '=';
  Result.Items[ 1, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk2);

  Result.Items[ 2, 0] := 'taukh';
  Result.Items[ 2, 1] := '=';
  Result.Items[ 2, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTaukh);

  Result.Items[ 4, 0] := 'E';
  Result.Items[ 4, 1] := '=';
  Result.Items[ 4, 2] := TryFormatFloat('%s MPa', '---', SOLVER.YoungModulus);

  Result.Items[ 5, 0] := 'G';
  Result.Items[ 5, 1] := '=';
  Result.Items[ 5, 2] := TryFormatFloat('%s MPa', '---', SOLVER.ShearModulus);

  Result.Items[ 6, 0] := 'rho';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := TryFormatFloat('%s kg/dm3', '---', SOLVER.MaterialDensity);

  Result.Items[ 7, 0] := 'Rm';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TensileStrengthRm);

  Result.Items[ 8, 0] := 'tauz';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmStaticTorsionalStressTauz);

  Result.Items[10, 0] := 'ns';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s', '---', SOLVER.StaticSafetyFactor);

  if SOLVER.DynamicLoad then
  begin
    Result.Items[11, 0] := 'tauoz';
    Result.Items[11, 1] := '=';
    Result.Items[11, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressTauoz);

    Result.Items[12, 0] := 'tauhz';
    Result.Items[12, 1] := '=';
    Result.Items[12, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);

    Result.Items[13, 0] := 'nf';
    Result.Items[13, 1] := '=';
    Result.Items[13, 2] := TryFormatFloat('%s', '---', SOLVER.DynamicSafetyFactor);

    if SOLVER.NumOfCycles > 0 then
    begin
      Result.Items[14, 0] := 'N';
      Result.Items[14, 1] := '=';
      Result.Items[14, 2] := TryFormatText('%s cycles', '---', TryFloatToText(SOLVER.NumOfCycles, 2, 0));

      Result.Items[15, 0] := 'Nh';
      Result.Items[15, 1] := '=';
      Result.Items[15, 2] := TryFormatFloatDiv('%s hours', '---', SOLVER.NumOfCycles, 3600*ApplicationForm.CycleFrequency.Value);
    end else
    begin
      Result.Items[14, 0] := 'N';
      Result.Items[14, 1] := '=';
      Result.Items[14, 2] := '---';

      Result.Items[15, 0] := 'Nh';
      Result.Items[15, 1] := '=';
      Result.Items[15, 2] := '---';
    end;
  end;


  (*
  Result := TReportTable.Create;
  Result.ColumnSpacer := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom         := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table', ASetting);


  Result.ColumnCount  := 3;
  Result.RowCount     := 33;
  Result.Items[ 0, 0] := TryFormatFloat   ('d        = %s mm',     'd     = ---', SOLVER.WireDiameter) + TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);
  Result.Items[ 1, 0] := TryFormatFloat   ('Di       = %s mm',     'Di    = ---', SOLVER.Di);
  Result.Items[ 2, 0] := TryFormatFloat   ('Dm       = %s mm',     'Dm    = ---', SOLVER.Dm) + TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);
  Result.Items[ 3, 0] := TryFormatFloat   ('De       = %s mm',     'De    = ---', SOLVER.De);
  Result.Items[ 4, 0] := TryFormatFloat   ('n        = %s coils',  'n     = ---', SOLVER.ActiveColis);
  Result.Items[ 5, 0] := TryFormatFloat   ('nt       = %s colis',  'nt    = ---', SOLVER.TotalCoils);
  Result.Items[ 6, 0] := TryFormatFloatDiv('Dm/d     = %s',        'Dm/d  = ---', SOLVER.Dm, SOLVER.WireDiameter);
  Result.Items[ 7, 0] := TryFormatFloat   ('nu       = %s',        'nu    = ---', SOLVER.SeatingCoefficent);
  Result.Items[ 8, 0] := TryFormatFloat   ('k        = %s',        'k     = ---', SOLVER.CorrectionFactorK);
  Result.Items[ 9, 0] := '';
  Result.Items[10, 0] := TryFormatFloat   ('L        = %s mm',     'L     = ---', SOLVER.WireLength);
  Result.Items[11, 0] := TryFormatFloat   ('rho      = %s kg/dm3', 'rho   = ---', SOLVER.MaterialDensity);
  Result.Items[12, 0] := TryFormatFloat   ('mass     = %s g',      'mass  = ---', SOLVER.Mass);
  Result.Items[13, 0] := TryFormatFloat   ('fe       = %s Hz',     'fe    = ---', SOLVER.NaturalFrequency);
  Result.Items[14, 0] := '';
  Result.Items[15, 0] := TryFormatText    ('Material = %s',        'Material = ---', MAT.Items[MAT.ItemIndex]);
  Result.Items[16, 0] := TryFormatFloat   ('G        = %s MPa',    'G        = ---', SOLVER.ShearModulus);
  Result.Items[17, 0] := TryFormatFloat   ('Rm       = %s MPa',    'Rm       = ---', SOLVER.TensileStrengthRm);
  Result.Items[18, 0] := TryFormatFloat   ('tauz     = %s MPa',    'tauz     = ---', SOLVER.AdmStaticTorsionalStressTauz);
  Result.Items[19, 0] := TryFormatFloat   ('T        = %s C°',     'T        = ---', MAT.Tempetature);
  Result.Items[20, 0] := TryFormatFloat   ('G(T)     = %s MPa',    'G(T)     = ---', MAT.GetG(MAT.Tempetature));
  Result.Items[21, 0] := '';
  Result.Items[22, 0] := TryFormatBool    ('Closed ends    = True', 'Closed ends    = False', SOLVER.ClosedEnds);
  Result.Items[23, 0] := TryFormatBool    ('Ground ends    = True', 'Ground ends    = False', SOLVER.GroundEnds);
  Result.Items[24, 0] := TryFormatBool    ('Cold coiled    = True', 'Cold coiled    = False', SOLVER.ColdCoiled);
  Result.Items[25, 0] := TryFormatBool    ('Dynamic strain = True', 'Dynamic strain = False', SOLVER.DynamicLoad);
  Result.Items[26, 0] := '';
  Result.Items[27, 0] := TryFormatInt     ('EN15800 Quality Grade Dm  = %s', 'EN15800 Quality Grade Dm  = ---', TOL.DmQualityGrade);
  Result.Items[28, 0] := TryFormatInt     ('EN15800 Quality Grade L0  = %s', 'EN15800 Quality Grade L0  = ---', TOL.L0QualityGrade);
  Result.Items[29, 0] := TryFormatInt     ('EN15800 Quality Grade F1  = %s', 'EN15800 Quality Grade F1  = ---', TOL.F1QualityGrade);
  Result.Items[30, 0] := TryFormatInt     ('EN15800 Quality Grade F2  = %s', 'EN15800 Quality Grade F2  = ---', TOL.F2QualityGrade);
  Result.Items[31, 0] := TryFormatInt     ('EN15800 Quality Grade e1  = %s', 'EN15800 Quality Grade e1  = ---', TOL.E1QualityGrade);
  Result.Items[32, 0] := TryFormatInt     ('EN15800 Quality Grade e2  = %s', 'EN15800 Quality Grade e2  = ---', TOL.E2QualityGrade);
  Result.Items[ 0, 1] := '   ';

  Result.Items[ 0, 2] := TryFormatFloat('L0    = %s mm',  'L0    = ---', SOLVER.LengthL0) + TryFormatFloat(' ± %s mm', '', TOL.LengthL0Tolerance);
  Result.Items[ 1, 2] := TryFormatFloat('L1    = %s mm',  'L1    = ---', SOLVER.LengthL1);
  Result.Items[ 2, 2] := TryFormatFloat('L2    = %s mm',  'L2    = ---', SOLVER.LengthL2);
  Result.Items[ 3, 2] := TryFormatFloat('Ln    = %s mm',  'Ln    = ---', SOLVER.LengthLn);
  Result.Items[ 4, 2] := TryFormatFloat('Lc    = %s mm',  'Lc    = ---', SOLVER.LengthLc);
  Result.Items[ 5, 2] := '';
  Result.Items[ 6, 2] := TryFormatFloat('s1    = %s mm',  's1    = ---', SOLVER.DeflectionS1);
  Result.Items[ 7, 2] := TryFormatFloat('s2    = %s mm',  's2    = ---', SOLVER.DeflectionS2);
  Result.Items[ 8, 2] := TryFormatFloat('sh    = %s mm',  'sh    = ---', SOLVER.DeflectionSh);
  Result.Items[ 9, 2] := TryFormatFloat('sn    = %s mm',  'sn    = ---', SOLVER.DeflectionSn);
  Result.Items[10, 2] := TryFormatFloat('sc    = %s mm',  'sc    = ---', SOLVER.DeflectionSc);
  Result.Items[11, 2] := '';
  Result.Items[12, 2] := TryFormatFloat('F1    = %s N',   'F1    = ---', SOLVER.LoadF1) + TryFormatFloat(' ± %s N', '', TOL.LoadF1Tolerance);
  Result.Items[13, 2] := TryFormatFloat('F2    = %s N',   'F2    = ---', SOLVER.LoadF2) + TryFormatFloat(' ± %s N', '', TOL.LoadF2Tolerance);
  Result.Items[14, 2] := TryFormatFloat('Fn    = %s N',   'Fn    = ---', SOLVER.LoadFn);
  Result.Items[15, 2] := TryFormatFloat('Fc    = %s N',   'Fc    = ---', SOLVER.LoadFc);
  Result.Items[16, 2] := '';
  Result.Items[17, 2] := TryFormatFloat('tauk1 = %s MPa', 'tauk1 = ---', SOLVER.TorsionalStressTauk1);
  Result.Items[18, 2] := TryFormatFloat('tauk2 = %s MPa', 'tauk2 = ---', SOLVER.TorsionalStressTauk2);
  Result.Items[19, 2] := TryFormatFloat('taukh = %s MPa', 'taukh = ---', SOLVER.TorsionalStressTaukh);
  Result.Items[20, 2] := '';
  Result.Items[21, 2] := TryFormatFloat('tauoz = %s MPa', 'tauoz = ---', SOLVER.AdmDynamicTorsionalStressTauoz);
  Result.Items[22, 2] := TryFormatFloat('tauhz = %s MPa', 'tauhz = ---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);
  Result.Items[23, 2] := '';
  *)
end;


function CreateQuick1AList(const aScreenScale: double; aSetting: TIniFile): TReportTable;
var
  i, j: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount    := 38;
  Result.Zoom        := aScreenScale;
  LoadTable(Result, 'Quick1List', 'Table', ASetting);

  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  for i := 0 to Result.RowCount -1 do
    for j := 0 to Result.ColumnCount -1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  Result.Items[ 0, 0] := 'd';
  Result.Items[ 0, 1] := '=';
  Result.Items[ 0, 2] := TryFormatFloat('%s', '---', SOLVER.WireDiameter) +
    TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);

  Result.Items[ 1, 0] := 'Di';
  Result.Items[ 1, 1] := '=';
  Result.Items[ 1, 2] := TryFormatFloat('%s mm', '---', SOLVER.Di);

  Result.Items[ 2, 0] := 'Dm';
  Result.Items[ 2, 1] := '=';
  Result.Items[ 2, 2] := TryFormatFloat('%s mm', '---', SOLVER.Dm);

  Result.Items[ 3, 0] := 'De';
  Result.Items[ 3, 1] := '=';
  Result.Items[ 3, 2] := TryFormatFloat('%s', '---', SOLVER.De) +
    TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);

  Result.Items[ 4, 0] := 'n';
  Result.Items[ 4, 1] := '=';
  Result.Items[ 4, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);

  Result.Items[ 5, 0] := 'nt';
  Result.Items[ 5, 1] := '=';
  Result.Items[ 5, 2] := TryFormatFloat('%s coils', '---', SOLVER.TotalCoils);

  Result.Items[ 6, 0] := 'nt';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := TryFormatFloat('%s N/mm',  '---', SOLVER.SpringRateR);

  Result.Items[ 7, 0] := 'Dec';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := TryFormatFloat('%s mm', '---', SOLVER.De + SOLVER.DeltaDe);

  Result.Items[ 8, 0] := 'Di.min';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := TryFormatFloat('%s mm', '---', SOLVER.DiMin);

  Result.Items[ 9, 0] := 'De.max';
  Result.Items[ 9, 1] := '=';
  Result.Items[ 9, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeMax);

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionSk);

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s mm', '---', SOLVER.WireLength);

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s g', '---', SOLVER.Mass);

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s Nmm', '---', SOLVER.SpringWorkW12);

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s Nmm', '---', SOLVER.SpringWorkW0n);

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s Hz', '---' ,SOLVER.NaturalFrequency);

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s mm', '---', SOLVER.Pitch);

  Result.Items[17, 0] := 'PitchRatio';
  Result.Items[17, 1] := '=';
  Result.Items[17, 2] := TryFormatFloat('%s', '---', SOLVER.PitchRatio);

  Result.Items[18, 0] := 'nu';
  Result.Items[18, 1] := '=';
  Result.Items[18, 2] := TryFormatFloat('%s', '---', SOLVER.SeatingCoefficent);

  Result.Items[19, 0] := 'load';
  Result.Items[19, 1] := '=';

  if SOLVER.DynamicLoad then
    Result.Items[19, 2] := ('dynamic')
  else
    Result.Items[19, 2] := ('static');

  Result.Items[22, 0] := 'tauk1';
  Result.Items[22, 1] := '=';
  Result.Items[22, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk1);

  Result.Items[23, 0] := 'tauk2';
  Result.Items[23, 1] := '=';
  Result.Items[23, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk2);

  Result.Items[24, 0] := 'taukh';
  Result.Items[24, 1] := '=';
  Result.Items[24, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTaukh);

  Result.Items[26, 0] := 'E';
  Result.Items[26, 1] := '=';
  Result.Items[26, 2] := TryFormatFloat('%s MPa', '---', SOLVER.YoungModulus);

  Result.Items[27, 0] := 'G';
  Result.Items[27, 1] := '=';
  Result.Items[27, 2] := TryFormatFloat('%s MPa', '---', SOLVER.ShearModulus);

  Result.Items[28, 0] := 'rho';
  Result.Items[28, 1] := '=';
  Result.Items[28, 2] := TryFormatFloat('%s kg/dm3', '---', SOLVER.MaterialDensity);

  Result.Items[29, 0] := 'Rm';
  Result.Items[29, 1] := '=';
  Result.Items[29, 2] := TryFormatFloat('%s MPa', '---', SOLVER.TensileStrengthRm);

  Result.Items[30, 0] := 'tauz';
  Result.Items[30, 1] := '=';
  Result.Items[30, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmStaticTorsionalStressTauz);

  Result.Items[31, 0] := 'ns';
  Result.Items[31, 1] := '=';
  Result.Items[31, 2] := TryFormatFloat('%s', '---', SOLVER.StaticSafetyFactor);

  if SOLVER.DynamicLoad then
  begin
    Result.Items[33, 0] := 'tauoz';
    Result.Items[33, 1] := '=';
    Result.Items[33, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressTauoz);

    Result.Items[34, 0] := 'tauhz';
    Result.Items[34, 1] := '=';
    Result.Items[34, 2] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);

    Result.Items[35, 0] := 'nf';
    Result.Items[35, 1] := '=';
    Result.Items[35, 2] := TryFormatFloat('%s', '---', SOLVER.DynamicSafetyFactor);

    if SOLVER.NumOfCycles > 0 then
    begin
      Result.Items[36, 0] := 'N';
      Result.Items[36, 1] := '=';
      Result.Items[36, 2] := TryFormatText('%s cycles', '---', TryFloatToText(SOLVER.NumOfCycles, 2, 0));

      Result.Items[37, 0] := 'Nh';
      Result.Items[37, 1] := '=';
      Result.Items[37, 2] := TryFormatFloatDiv('%s hours', '---', SOLVER.NumOfCycles, 3600*ApplicationForm.CycleFrequency.Value);
    end else
    begin
      Result.Items[36, 0] := 'N';
      Result.Items[36, 1] := '=';
      Result.Items[36, 2] := '---';

      Result.Items[37, 0] := 'Nh';
      Result.Items[37, 1] := '=';
      Result.Items[37, 2] := '---';
    end;
  end;
end;

procedure DrawQuick1(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);
var
  i: longint;
  Bit: array of TBGRABitmap = nil;
  ForceDiagram: TChart;
  GoodmanDiagram: TChart;
  MessageList: TReportTable;
  Quick1List: TReportTable;
  Quick1Table: TReportTable;
  QualityTable: TReportTable;
begin
  SetLength(Bit, 10);
  for i := Low(Bit) to High(Bit) do
    Bit[i] := TBGRABitmap.Create;

  // Force & Displacement Chart
  ForceDiagram := CreateForceDisplacementChart(aScreenScale, aSetting);
  Bit[0].SetSize(Trunc(aScreen.Width * 0.35), aScreen.Height div 2);
  ForceDiagram.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
  Bit[0].Draw(aScreen.Canvas, 0, 0, False);
  ForceDiagram.Destroy;

  // Goodman Chart
  GoodmanDiagram := CreateGoodmanChart(aScreenScale, aSetting);
  Bit[1].SetSize(Bit[0].Width, Bit[0].Height);
  GoodmanDiagram.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
  Bit[1].Draw(aScreen.Canvas, 0, Bit[0].Height, False);
  GoodmanDiagram.Destroy;

  // Quick-1 List
  Quick1List := CreateQuick1AList(aScreenScale, aSetting);
  while Quick1List.Height < (aScreen.Height) do
  begin
    Quick1List.RowSpacer := Quick1List.RowSpacer + 1;
  end;
  Quick1List.RowSpacer := Quick1List.RowSpacer - 1;
  Bit[2].SetSize(Quick1List.Width, Quick1List.Height);
  Quick1List.Draw(Bit[2].Canvas);
  Bit[2].Draw(aScreen.Canvas, Bit[0].Width, 0, False);
  Quick1List.Destroy;

  // Quick-1 Table
  Quick1Table := CreateQuick1Table(aScreenScale, aSetting);
  while Quick1Table.Width < (aScreen.Width - Bit[0].Width - Bit[2].Width) do
  begin
    Quick1Table.ColumnSpacer := Quick1Table.ColumnSpacer + 1;
  end;
  Quick1Table.ColumnSpacer := Quick1Table.ColumnSpacer - 1;
  Bit[3].SetSize(Quick1Table.Width, Quick1Table.Height);
  Quick1Table.Draw(Bit[3].Canvas);
  Bit[3].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width, 0, False);
  Quick1Table.Destroy;

  // Quality Table
  QualityTable := CreateQualityTable(aScreenScale, aSetting);
  Bit[4].SetSize(QualityTable.Width, QualityTable.Height);
  QualityTable.Draw(Bit[4].Canvas);
  Bit[4].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width, Bit[3].Height, False);
  QualityTable.Destroy;

  // Message List
  MessageList := CreateMessageList(aScreenScale, aSetting);
  Bit[5].SetSize(MessageList.Width, MessageList.Height);
  MessageList.Draw(Bit[5].Canvas);
  Bit[5].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width + Bit[4].Width, Bit[3].Height, False);
  MessageList.Destroy;

  for i := Low(Bit) to High(Bit) do
    Bit[i].Destroy;
  Bit := nil
end;

procedure DrawQuickX(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile; X: longint);
var
  i, j: longint;
  Bit: array of TBGRABitmap = nil;
  CustomChart: TChart;
  QuickXList: TReportTable;
begin
  SetLength(Bit, 2);
  for i := Low(Bit) to High(Bit) do
    Bit[i] := TBGRABitmap.Create;

  // Quick-X List
  QuickXList := TReportTable.Create;
  QuickXList.ColumnCount := 7;
  QuickXList.RowCount    := 33;
  QuickXList.Zoom        := aScreenScale;
  LoadTable(QuickXList, 'QuickXList', 'Table', ASetting);

  for i := 0 to QuickXList.RowCount -1 do
    for j := 0 to QuickXList.ColumnCount -1 do
    begin
      QuickXList.Items[i, j] := ' ';
    end;

  QuickXList.ColumnAlignments[0] := taLeftJustify;
  QuickXList.ColumnAlignments[1] := taCenter;
  QuickXList.ColumnAlignments[2] := taLeftJustify;
  QuickXList.ColumnAlignments[3] := taCenter;
  QuickXList.ColumnAlignments[4] := taLeftJustify;
  QuickXList.ColumnAlignments[5] := taCenter;
  QuickXList.ColumnAlignments[6] := taLeftJustify;

  QuickXList.Items[ 0, 0] := 'd';
  QuickXList.Items[ 0, 1] := '=';
  QuickXList.Items[ 0, 2] := TryFormatFloat('%s mm', '---', SOLVER.WireDiameter) +
    TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);

  QuickXList.Items[ 1, 0] := 'Di';
  QuickXList.Items[ 1, 1] := '=';
  QuickXList.Items[ 1, 2] := TryFormatFloat('%s mm', '---', SOLVER.Di);

  QuickXList.Items[ 2, 0] := 'Dm';
  QuickXList.Items[ 2, 1] := '=';
  QuickXList.Items[ 2, 2] := TryFormatFloat('%s mm', '---', SOLVER.Dm) +
    TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);

  QuickXList.Items[ 3, 0] := 'De';
  QuickXList.Items[ 3, 1] := '=';
  QuickXList.Items[ 3, 2] := TryFormatFloat('%s mm', '---', SOLVER.De);

  QuickXList.Items[ 4, 0] := 'n';
  QuickXList.Items[ 4, 1] := '=';
  QuickXList.Items[ 4, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);

  QuickXList.Items[ 5, 0] := 'nt';
  QuickXList.Items[ 5, 1] := '=';
  QuickXList.Items[ 5, 2] := TryFormatFloat('%s colis', '---', SOLVER.TotalCoils);

  QuickXList.Items[ 6, 0] := 'w';
  QuickXList.Items[ 6, 1] := '=';
  QuickXList.Items[ 6, 2] := TryFormatFloatDiv('%s', '---', SOLVER.Dm, SOLVER.WireDiameter);

  QuickXList.Items[ 7, 0] := 'nu';
  QuickXList.Items[ 7, 1] := '=';
  QuickXList.Items[ 7, 2] := TryFormatFloat('%s', '---', SOLVER.SeatingCoefficent);

  QuickXList.Items[ 8, 0] := 'k';
  QuickXList.Items[ 8, 1] := '=';
  QuickXList.Items[ 8, 2] := TryFormatFloat('%s', '---', SOLVER.CorrectionFactorK);

  QuickXList.Items[10, 0] := 'L0';
  QuickXList.Items[10, 1] := '=';
  QuickXList.Items[10, 2] := TryFormatFloat('%s mm', '---', SOLVER.LengthL0) +
    TryFormatFloat(' ± %s mm', '', TOL.LengthL0Tolerance);

  QuickXList.Items[11, 0] := 'L1';
  QuickXList.Items[11, 1] := '=';
  QuickXList.Items[11, 2] := TryFormatFloat('%s mm', '---', SOLVER.LengthL1);

  QuickXList.Items[12, 0] := 'L2';
  QuickXList.Items[12, 1] := '=';
  QuickXList.Items[12, 2] := TryFormatFloat('%s mm', '---', SOLVER.LengthL2);

  QuickXList.Items[13, 0] := 'Ln';
  QuickXList.Items[13, 1] := '=';
  QuickXList.Items[13, 2] := TryFormatFloat('%s mm', '---', SOLVER.LengthLn);

  QuickXList.Items[14, 0] := 'Lc';
  QuickXList.Items[14, 1] := '=';
  QuickXList.Items[14, 2] := TryFormatFloat('%s mm', '---', SOLVER.LengthLc);

  QuickXList.Items[16, 0] := 's1';
  QuickXList.Items[16, 1] := '=';
  QuickXList.Items[16, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionS1);

  QuickXList.Items[17, 0] := 's2';
  QuickXList.Items[17, 1] := '=';
  QuickXList.Items[17, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionS2);

  QuickXList.Items[18, 0] := 'sh';
  QuickXList.Items[18, 1] := '=';
  QuickXList.Items[18, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionSh);

  QuickXList.Items[19, 0] := 'sn';
  QuickXList.Items[19, 1] := '=';
  QuickXList.Items[19, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionSn);

  QuickXList.Items[20, 0] := 'sc';
  QuickXList.Items[20, 1] := '=';
  QuickXList.Items[20, 2] := TryFormatFloat('%s mm', '---', SOLVER.DeflectionSc);

  QuickXList.Items[22, 0] := 'Spring ends';
  QuickXList.Items[22, 1] := '=';
  if SOLVER.ClosedEnds then
  begin
    if SOLVER.GroundEnds then
      QuickXList.Items[22, 2] := 'Closed and ground'
    else
      QuickXList.Items[22, 2] := 'Closed';
  end else
    QuickXList.Items[22, 2] := 'Open';

  QuickXList.Items[23, 0] := 'Coiling type';
  QuickXList.Items[23, 1] := '=';
  if SOLVER.ColdCoiled then
    QuickXList.Items[23, 2] := 'Cold coiled'
  else
    QuickXList.Items[23, 2] := 'Hot coiled';

  QuickXList.Items[24, 0] := 'Load type';
  QuickXList.Items[24, 1] := '=';
  if SOLVER.DynamicLoad then
    QuickXList.Items[24, 2] := 'Dynamic'
  else
    QuickXList.Items[24, 2] := 'Static';

  QuickXList.Items[26, 0] := 'Tolerances:';

  QuickXList.Items[27, 0] := 'Di, Dm, De';
  QuickXList.Items[27, 1] := ' ';
  QuickXList.Items[27, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.DmQualityGrade);

  QuickXList.Items[28, 0] := 'L0';
  QuickXList.Items[28, 1] := ' ';
  QuickXList.Items[28, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.L0QualityGrade);

  QuickXList.Items[29, 0] := 'F1';
  QuickXList.Items[29, 1] := ' ';
  QuickXList.Items[29, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.F1QualityGrade);

  QuickXList.Items[30, 0] := 'F2';
  QuickXList.Items[30, 1] := ' ';
  QuickXList.Items[30, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.F2QualityGrade);

  QuickXList.Items[31, 0] := 'e1';
  QuickXList.Items[31, 1] := ' ';
  QuickXList.Items[31, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.E1QualityGrade);

  QuickXList.Items[32, 0] := 'e2';
  QuickXList.Items[32, 1] := ' ';
  QuickXList.Items[32, 2] := TryFormatInt('Quality Grade %s EN15800', '---', TOL.E2QualityGrade);

  //

  QuickXList.Items[ 0, 4] := 'Material';
  QuickXList.Items[ 0, 5] := '=';
  QuickXList.Items[ 0, 6] := TryFormatText('%s', '---', MAT.Items[MAT.ItemIndex]);

  QuickXList.Items[ 1, 4] := 'Rm';
  QuickXList.Items[ 1, 5] := '=';
  QuickXList.Items[ 1, 6] := TryFormatFloat('%s MPa', '---', SOLVER.TensileStrengthRm);

  QuickXList.Items[ 2, 4] := 'G';
  QuickXList.Items[ 2, 5] := '=';
  QuickXList.Items[ 2, 6] := TryFormatFloat('%s MPa', '---', SOLVER.ShearModulus);

  QuickXList.Items[ 3, 4] := Format('G(%s°)', [TryFloatToText(MAT.Tempetature)]);
  QuickXList.Items[ 3, 5] := '=';
  QuickXList.Items[ 3, 6] := TryFormatFloat('%s MPa', '---', MAT.GetG(MAT.Tempetature));

  QuickXList.Items[ 4, 4] := 'rho';
  QuickXList.Items[ 4, 5] := '=';
  QuickXList.Items[ 4, 6] := TryFormatFloat('%s kg/dm3', '---', SOLVER.MaterialDensity);

  QuickXList.Items[ 6, 4] := 'L';
  QuickXList.Items[ 6, 5] := '=';
  QuickXList.Items[ 6, 6] := TryFormatFloat('%s mm', '---', SOLVER.WireLength);

  QuickXList.Items[ 7, 4] := 'mass';
  QuickXList.Items[ 7, 5] := '=';
  QuickXList.Items[ 7, 6] := TryFormatFloat('%s g', '---', SOLVER.Mass);

  QuickXList.Items[ 8, 4] := 'fe';
  QuickXList.Items[ 8, 5] := '=';
  QuickXList.Items[ 8, 6] := TryFormatFloat('%s Hz', '---', SOLVER.NaturalFrequency);

  QuickXList.Items[11, 4] := 'F1';
  QuickXList.Items[11, 5] := '=';
  QuickXList.Items[11, 6] := TryFormatFloat('%s N',  '---', SOLVER.LoadF1) +
    TryFormatFloat(' ± %s N', '', TOL.LoadF1Tolerance);

  QuickXList.Items[12, 4] := 'F2';
  QuickXList.Items[12, 5] := '=';
  QuickXList.Items[12, 6] := TryFormatFloat('%s N', '---', SOLVER.LoadF2) +
    TryFormatFloat(' ± %s N', '', TOL.LoadF2Tolerance);

  QuickXList.Items[13, 4] := 'Fn';
  QuickXList.Items[13, 5] := '=';
  QuickXList.Items[13, 6] := TryFormatFloat('%s N', '---', SOLVER.LoadFn);

  QuickXList.Items[14, 4] := 'Fc';
  QuickXList.Items[14, 5] := '=';
  QuickXList.Items[14, 6] := TryFormatFloat('%s N', '---', SOLVER.LoadFc);

  QuickXList.Items[16, 4] := 'tauk1';
  QuickXList.Items[16, 5] := '=';
  QuickXList.Items[16, 6] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk1);

  QuickXList.Items[17, 4] := 'tauk2';
  QuickXList.Items[17, 5] := '=';
  QuickXList.Items[17, 6] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTauk2);

  QuickXList.Items[18, 4] := 'taukh';
  QuickXList.Items[18, 5] := '=';
  QuickXList.Items[18, 6] := TryFormatFloat('%s MPa', '---', SOLVER.TorsionalStressTaukh);

  QuickXList.Items[19, 4] := 'tauhz';
  QuickXList.Items[19, 5] := '=';
  QuickXList.Items[19, 6] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);

  QuickXList.Items[20, 4] := 'tauoz';
  QuickXList.Items[20, 5] := '=';
  QuickXList.Items[20, 6] := TryFormatFloat('%s MPa', '---', SOLVER.AdmDynamicTorsionalStressTauoz);

  QuickXList.Items[21, 4] := 'tauz';
  QuickXList.Items[21, 5] := '=';
  QuickXList.Items[21, 6] := TryFormatFloat('%s MPa', '---', SOLVER.AdmStaticTorsionalStressTauz);




























  Bit[0].SetSize(QuickXList.Width, QuickXList.Height);
  QuickXList.Draw(Bit[0].Canvas);
  Bit[0].Draw(aScreen.Canvas, aScreen.Width - Bit[0].Width, 0, False);
  QuickXList.Destroy;

  if X = 3 then
    CustomChart := CreateGoodmanChart(aScreenScale, aSetting)
  else
    CustomChart := CreateForceDisplacementChart(aScreenScale, aSetting);

  Bit[1].SetSize(aScreen.Width - Bit[0].Width, aScreen.Height);
  CustomChart.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
  Bit[1].Draw(aScreen.Canvas, 0, 0, False);
  CustomChart.Destroy;

  for i := Low(Bit) to High(Bit) do
    Bit[i].Destroy;
  Bit := nil;
end;

procedure DrawQuick2(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);
begin
  DrawQuickX(aScreen, aScreenScale, aSetting, 2);
end;

procedure DrawQuick3(var aScreen: TBGRABitmap; const aScreenScale: double; aSetting: TIniFile);
begin
  DrawQuickX(aScreen, aScreenScale, aSetting, 3);
end;

end.

