unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmapTypes, Classes, GraphBase, Graphics, IniFiles, Math, SysUtils;


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


implementation

uses
  EN10270, EN13906, EN15800, UtilsBase;


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
  Chart.FontStyle  := LoadFontStyle(ASection, AIdent + 'Style' , ASetting);
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

function CreateForceDisplacementChart(const AScreenScale: double; ASetting: TIniFile): TChart;
const
  Section1 = 'Custom';
  Section2 = 'ForceDisplacementChart';
var
  Points: ArrayOfTPointF = nil;
begin
  Result               := TChart.Create;
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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
  Result.LegendEnabled := True;
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

function CreateQualityTable(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result                   := TReportTable.Create('QualityTable', aSetting);
  Result.Spacer            := Trunc(DefaultSpacer*aScreenScale);
  Result.VerticalAlignment := 1;
  Result.Zoom              := aScreenScale;
  Result.RowCount          := 7;
  Result.ColumnCount       := 5;

  Result[0, 0]             := 'Quality Grade';
  Result[1, 0]             := 'De, Di';
  Result[2, 0]             := 'L0';
  Result[3, 0]             := 'F1';
  Result[4, 0]             := 'F2';
  Result[5, 0]             := 'e1';
  Result[6, 0]             := 'e2';

  Result[0, 1]             := '1';
  Result[1, 1]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 1);
  Result[2, 1]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 1);
  Result[3, 1]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 1);
  Result[4, 1]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 1);
  Result[5, 1]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 1);
  Result[6, 1]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 1);

  Result[0, 2]             := '2';
  Result[1, 2]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 2);
  Result[2, 2]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 2);
  Result[3, 2]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 2);
  Result[4, 2]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 2);
  Result[5, 2]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 2);
  Result[6, 2]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 2);

  Result[0, 3]             := '3';
  Result[1, 3]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 3);
  Result[2, 3]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 3);
  Result[3, 3]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 3);
  Result[4, 3]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 3);
  Result[5, 3]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 3);
  Result[6, 3]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 3);

  Result[0, 4]             := ' Tol.';
  Result[1, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.CoilDiameterTolerance);
  Result[2, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.LengthL0Tolerance);
  Result[3, 4]             := TryFormatFloat('%s N',  ' --- ', TOL.LoadF1Tolerance);
  Result[4, 4]             := TryFormatFloat('%s N',  ' --- ', TOL.LoadF2Tolerance);
  Result[5, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE1);
  Result[6, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE2);
end;

function CreateQuick1Table(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result             := TReportTable.Create('ReportTable', aSetting);
  Result.Spacer      := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom        := aScreenScale;
  Result.RowCount    := 6;
  Result.ColumnCount := 7;
  Result[0, 0]       := 'L [mm]';
  Result[1, 0]       := TryFormatFloat('L0: %s', 'L0: ---', SOLVER.LengthL0);
  Result[2, 0]       := TryFormatFloat('L1: %s', 'L1: ---', SOLVER.LengthL1);
  Result[3, 0]       := TryFormatFloat('L2: %s', 'L2: ---', SOLVER.LengthL2);
  Result[4, 0]       := TryFormatFloat('Ln: %s', 'Ln: ---', SOLVER.LengthLn);
  Result[5, 0]       := TryFormatFloat('Lc: %s', 'Lc: ---', SOLVER.LengthLc);

  Result[0, 1]       := 'F [N]';
  Result[1, 1]       := '';
  Result[2, 1]       := TryFormatFloat('F1: %s', 'F1: ---', SOLVER.LoadF1);
  Result[3, 1]       := TryFormatFloat('F2: %s', 'F2: ---', SOLVER.LoadF2);
  Result[4, 1]       := TryFormatFloat('Fn: %s', 'Fn: ---', SOLVER.LoadFn);
  Result[5, 1]       := TryFormatFloat('Fc: %s', 'Fc: ---', SOLVER.LoadFc);

  Result[0, 2]       := 'tau [MPa]';
  Result[1, 2]       := '';
  Result[2, 2]       := TryFormatFloat('tauk1: %s', 'tauk1: ---', SOLVER.TorsionalStressTauk1);
  Result[3, 2]       := TryFormatFloat('tauk2: %s', 'tauk2: ---', SOLVER.TorsionalStressTauk2);
  Result[4, 2]       := TryFormatFloat('tau n: %s', 'tau n: ---', SOLVER.TorsionalStressTaun);
  Result[5, 2]       := TryFormatFloat('tau c: %s', 'tau c: ---', SOLVER.TorsionalStressTauc);

  Result[0, 3]       := 's [mm]';
  Result[1, 3]       := '';
  Result[2, 3]       := TryFormatFloat('s1: %s', 's1: ---', SOLVER.DeflectionS1);
  Result[3, 3]       := TryFormatFloat('s2: %s', 's2: ---', SOLVER.DeflectionS2);
  Result[4, 3]       := TryFormatFloat('sn: %s', 'sn: ---', SOLVER.DeflectionSn);
  Result[5, 3]       := TryFormatFloat('sc: %s', 'sc: ---', SOLVER.DeflectionSc);

  Result[0, 4]       := 'tau/tauz';
  Result[1, 4]       := '';
  Result[2, 4]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1, SOLVER.AdmStaticTorsionalStressTauz);
  Result[3, 4]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2, SOLVER.AdmStaticTorsionalStressTauz);
  Result[4, 4]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun, SOLVER.AdmStaticTorsionalStressTauz);
  Result[5, 4]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc, SOLVER.AdmStaticTorsionalStressTauz);

  Result[0, 5]       := 'tau/Rm';
  Result[1, 5]       := '';
  Result[2, 5]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1, SOLVER.TensileStrengthRm);
  Result[3, 5]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2, SOLVER.TensileStrengthRm);
  Result[4, 5]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun, SOLVER.TensileStrengthRm);
  Result[5, 5]       := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc, SOLVER.TensileStrengthRm);

  Result[0, 6]       := 'De';
  Result[1, 6]       := TryFormatFloat      ('%s', '---', SOLVER.De);
  Result[2, 6]       := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionS1, SOLVER.DeflectionSc);
  Result[3, 6]       := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionS2, SOLVER.DeflectionSc);
  Result[4, 6]       := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionSn, SOLVER.DeflectionSc);
  Result[5, 6]       := TryFormatFloatSumDiv('%s', '---', SOLVER.De, SOLVER.DeltaDe*SOLVER.DeflectionSc, SOLVER.DeflectionSc);
end;

function CreateQuick1List(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result                   := TReportTable.Create('ReportList', aSetting);
  Result.Spacer            := Trunc(DefaultSpacer*aScreenScale);
  Result.VerticalAlignment := 1;
  Result.Zoom              := aScreenScale;

  Result.ColumnCount       := 1;
  Result.RowCount          := 20;
  Result.Items[ 0, 0]      := TryFormatFloat('d      = %s',       'd      = ---', SOLVER.WireDiameter) + TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);
  Result.Items[ 1, 0]      := TryFormatFloat('Di     = %s mm',    'Di     = ---', SOLVER.Di);
  Result.Items[ 2, 0]      := TryFormatFloat('Dm     = %s mm',    'Dm     = ---', SOLVER.Dm);
  Result.Items[ 3, 0]      := TryFormatFloat('De     = %s',       'De     = ---', SOLVER.De) + TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);
  Result.Items[ 4, 0]      := TryFormatFloat('n      = %s coils', 'n      = ---', SOLVER.ActiveColis);
  Result.Items[ 5, 0]      := TryFormatFloat('nt     = %s coils', 'nt     = ---', SOLVER.TotalCoils);
  Result.Items[ 6, 0]      := TryFormatFloat('R      = %s N/mm',  'R      = ---', SOLVER.SpringRateR);
  Result.Items[ 7, 0]      := TryFormatFloat('Dec    = %s mm',    'Dec    = ---', SOLVER.De + SOLVER.DeltaDe);
  Result.Items[ 8, 0]      := TryFormatFloat('Di.min = %s mm',    'Di.min = ---', SOLVER.DiMin);
  Result.Items[ 9, 0]      := TryFormatFloat('De.max = %s mm',    'De.max = ---', SOLVER.DeMax);
  Result.Items[10, 0]      := TryFormatFloat('sk     = %s mm',    'sk     = ---', SOLVER.DeflectionSk);
  Result.Items[11, 0]      := TryFormatFloat('L      = %s mm',    'L      = ---', SOLVER.WireLength);
  Result.Items[12, 0]      := TryFormatFloat('m      = %s g',     'm      = ---', SOLVER.Mass);
  Result.Items[13, 0]      := TryFormatFloat('W12    = %s Nmm',   'W12    = ---', SOLVER.SpringWorkW12);
  Result.Items[14, 0]      := TryFormatFloat('W0n    = %s Nmm',   'W0n    = ---', SOLVER.SpringWorkW0n);
  Result.Items[15, 0]      := TryFormatFloat('fe     = %s Hz',    'fe     = ---' ,SOLVER.NaturalFrequency);
  Result.Items[16, 0]      := TryFormatFloat('Pitch  = %s mm',    'Pitch  = ---', SOLVER.Pitch);
  Result.Items[17, 0]      := TryFormatFloat('PitchRatio = %s',   'PitchRatio = ---', SOLVER.PitchRatio);
  Result.Items[18, 0]      := TryFormatFloat('nu         = %s',   'nu         = ---', SOLVER.SeatingCoefficent);

  if SOLVER.DynamicLoad then
    Result.Items[19, 0] := ('dynamic load = true')
  else
    Result.Items[19, 0] := ('dynamic load = false');
end;

function CreateMessageList(const aScreenScale: double; aSetting: TIniFile): TReportTable;
var
  i, j: longint;
begin
  Result                   := TReportTable.Create('MessageList', aSetting);
  Result.Spacer            := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom              := aScreenScale;
  Result.ColumnCount       := 1;
  Result.RowCount          := 1 + ErrorMessage.Count + WarningMessage.Count;
  Result.VerticalAlignment := 1;
  Result.Items[0, 0]       := TryFormatBool('Messages:', '', (ErrorMessage.Count + WarningMessage.Count) > 0);

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

end.

