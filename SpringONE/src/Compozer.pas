unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, Classes, ADim, GraphBase, Graphics, IniFiles, Math, SysUtils;

type
  TCompozer = class
  private
    FSetting: TIniFile;
    function LoadFontStyle(const ASection, AIdent: string): TFontStyles;
    function LoadPenStyle(const ASection, AIdent: string): TPenStyle;
    procedure LoadChart1(Chart: TChart; const ASection: string);
    procedure LoadChart2(Chart: TChart; const ASection, AIdent: string);
    procedure LoadTable(Table: TReportTable; const ASection, AIdent: string);
    procedure LoadSpring(Spring: TSectionSpringDrawing; const ASection, AIdent: string);

    procedure DrawQuickX(var aScreen: TBGRABitmap; const aScreenScale: double; X: longint);
  public
    constructor Create(aSetting: TIniFile);
    destructor Destroy; override;
    procedure DrawQuick1(var aScreen: TBGRABitmap; const aScreenScale: double);
    procedure DrawQuick2(var aScreen: TBGRABitmap; const aScreenScale: double);
    procedure DrawQuick3(var aScreen: TBGRABitmap; const aScreenScale: double);

    function CreateForceDisplacementChart(const aScreenScale: double): TChart;
    function CreateGoodmanChart(const aScreenScale: double): TChart;
    function CreateBucklingChart(const AScreenScale: double): TChart;
    function CreateLoadF1Chart(const AScreenScale: double): TChart;
    function CreateLoadF2Chart(const AScreenScale: double): TChart;
    function CreateShearModulusChart(const AScreenScale: double): TChart;
    function CreateYoungModulusChart(const AScreenScale: double): TChart;

    function CreateSectionSpringDrawing(const AScreenScale: double): TSectionSpringDrawing;
    function CreateQualityTable(const AScreenScale: double): TReportTable;
    function CreateQuick1Table(const AScreenScale: double): TReportTable;
    function CreateQuick1List1(const AScreenScale: double): TReportTable;
    function CreateQuick1List2(const AScreenScale: double): TReportTable;
    function CreateMessageList(const AScreenScale: double): TReportTable;
    function CreateQuick1AList(const AScreenScale: double): TReportTable;
  end;


implementation

uses
  EN10270, EN13906, EN15800, UtilsBase;

// TCompozer //

constructor TCompozer.Create(aSetting: TIniFile);
begin
  inherited Create;
  FSetting := aSetting;
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

procedure TCompozer.LoadSpring(Spring: TSectionSpringDrawing;
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
  Result.Title := 'Force & Displacement Chart';
  Result.XAxisLabel := 's[' + GetSymbol(SOLVER1.LengthLc) + ']';
  Result.YAxisLabel := 'F[' + GetSymbol(SOLVER1.LoadFc) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Drawing bisector line
  if SOLVER1.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetValue(SOLVER1.StrokeSc);
    Points[1].Y := GetValue(SOLVER1.LoadFc);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  // Drawing tolerance lines
  if (TOL.LoadF1.Value > 0) and (TOL.LoadF2.Value > 0) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'ToleranceLine');
    SetLength(Points, 2);
    Points[0].X := GetValue(SOLVER1.StrokeS1);
    Points[0].Y := GetValue(TOL.LoadF1 + TOL.LoadF1Tolerance);
    Points[1].X := GetValue(SOLVER1.StrokeS2);
    Points[1].Y := GetValue(TOL.LoadF2 + TOL.LoadF2Tolerance);
    Result.AddPolyLine(Points, True, 'Tolerance +');
    Points[0].X := GetValue(SOLVER1.StrokeS1);
    Points[0].Y := GetValue(TOL.LoadF1 - TOL.LoadF1Tolerance);
    Points[1].X := GetValue(SOLVER1.StrokeS2);
    Points[1].Y := GetValue(TOL.LoadF2 - TOL.LoadF2Tolerance);
    Result.AddPolyLine(Points, True, 'Tolerance -');
    Points := nil;
  end;
  // Drawing Load-F1
  if SOLVER1.LoadF1.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SOLVER1.LoadF1);
    Points[1].X := GetValue(SOLVER1.StrokeS1);
    Points[1].Y := GetValue(SOLVER1.LoadF1);
    Points[2].X := GetValue(SOLVER1.StrokeS1);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F1');
    Points := nil;
  end;
  // Drawing Load F2
  if SOLVER1.LoadF2.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SOLVER1.LoadF2);
    Points[1].X := GetValue(SOLVER1.StrokeS2);
    Points[1].Y := GetValue(SOLVER1.LoadF2);
    Points[2].X := GetValue(SOLVER1.StrokeS2);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F2');
    Points := nil;
  end;
  // Drawing Load Fn
  if SOLVER1.LoadFn.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SOLVER1.LoadFn);
    Points[1].X := GetValue(SOLVER1.StrokeSn);
    Points[1].Y := GetValue(SOLVER1.LoadFn);
    Points[2].X := GetValue(SOLVER1.StrokeSn);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fn');
    Points := nil;
  end;
  // Drawing Load Fc
  if SOLVER1.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SOLVER1.LoadFc);
    Points[1].X := GetValue(SOLVER1.StrokeSc);
    Points[1].Y := GetValue(SOLVER1.LoadFc);
    Points[2].X := GetValue(SOLVER1.StrokeSc);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fc');
    Points := nil;
  end;
  // Drawing label Load-F1
  if SOLVER1.LoadF1.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    Result.AddLabel(0, GetValue(SOLVER1.LoadF1),
      32, 0, taLeftJustify, taAlignBottom, 'F1');
  end;
  // Drawing label Load-F2
  if SOLVER1.LoadF2.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    Result.AddLabel(0, GetValue(SOLVER1.LoadF2),
      64, 0, taLeftJustify, taAlignBottom, 'F2');
  end;
  // Drawing label Load-Fn
  if SOLVER1.LoadFn.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    Result.AddLabel(0, GetValue(SOLVER1.LoadFn),
      96, 0, taLeftJustify, taAlignBottom, 'Fn');
  end;
  // Drawing label Load-Fc
  if SOLVER1.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    Result.AddLabel(0, GetValue(SOLVER1.LoadFc),
      128, 0, taLeftJustify, taAlignBottom, 'Fc');
  end;
end;

function TCompozer.CreateGoodmanChart(const AScreenScale: double): TChart;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  case MAT.ItemIndex of
    -1: Result.Title := Format('Goodman Chart: %s', ['Custom material']);
  else  Result.Title := Format('Goodman Chart: %s', [MAT.Items[MAT.ItemIndex]])
  end;
  Result.XAxisLabel := 'tau U';
  Result.YAxisLabel := 'tau O';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Drawing bisector line
  if SOLVER1.AdmStaticTorsionalStressTauz.Value > 0 then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetValue(SOLVER1.AdmStaticTorsionalStressTauz);
    Points[1].Y := GetValue(SOLVER1.AdmStaticTorsionalStressTauz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  // Drawing Tauk-Tolerances
  if (SOLVER1.GetTauk(TOL.LoadF1Tolerance).Value > 0) and
     (SOLVER1.GetTauk(TOL.LoadF2Tolerance).Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := GetValue(SOLVER1.TorsionalStressTauk1 - SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[0].Y := GetValue(SOLVER1.TorsionalStressTauk1 - SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[1].X := GetValue(SOLVER1.TorsionalStressTauk1 + SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[1].Y := GetValue(SOLVER1.TorsionalStressTauk1 + SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[2].X := GetValue(SOLVER1.TorsionalStressTauk1 + SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[2].Y := GetValue(SOLVER1.TorsionalStressTauk2 + SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[3].X := GetValue(SOLVER1.TorsionalStressTauk1 - SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Points[3].Y := GetValue(SOLVER1.TorsionalStressTauk2 - SOLVER1.GetTauk(TOL.LoadF1Tolerance));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk2');
    Points := nil;
  end;
  // Drawing Tauk1-Tauk2
  if (SOLVER1.TorsionalStressTauk1.Value > 0) and
    (SOLVER1.TorsionalStressTauk2.Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := GetValue(SOLVER1.TorsionalStressTauk1);
    Points[0].Y := GetValue(SOLVER1.TorsionalStressTauk1);
    Points[1].X := GetValue(SOLVER1.TorsionalStressTauk1);
    Points[1].Y := GetValue(SOLVER1.TorsionalStressTauk2);
    Result.AddPolyLine(Points, False, 'Tauk1-Tauk2');
    Points := nil;
  end;
  // Drawing Goodmand curve
  if MAT.ItemIndex <> -1 then
  begin
    if (MAT.TorsionalStressTauOE5  .Value > 0) and
       (MAT.TorsionalStressTauUE5  .Value > 0) and
       (MAT.TorsionalStressTauYield.Value > 0) and
       (MAT.TorsionalStressTauUE6  .Value > 0) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E5');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetValue(MAT.TorsionalStressTauOE5);
      Points[1].X := GetValue(MAT.TorsionalStressTauUE5);
      Points[1].Y := GetValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetValue(MAT.TorsionalStressTauUE6);
      Points[2].Y := GetValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E5 Cycles');
      Result.AddLabel(
        GetValue(MAT.TorsionalStressTauUE5),
        GetValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E5');
      Points := nil;
    end;

    if (MAT.TorsionalStressTauOE6  .Value > 0) and
       (MAT.TorsionalStressTauUE6  .Value > 0) and
       (MAT.TorsionalStressTauYield.Value > 0) and
       (MAT.TorsionalStressTauUE7  .Value > 0) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E6');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetValue(MAT.TorsionalStressTauOE6);
      Points[1].X := GetValue(MAT.TorsionalStressTauUE6);
      Points[1].Y := GetValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetValue(MAT.TorsionalStressTauUE7);
      Points[2].Y := GetValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E6 Cycles');
      Result.AddLabel(
        GetValue(MAT.TorsionalStressTauUE6),
        GetValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E6');
      Points := nil;
    end;

    if (MAT.TorsionalStressTauOE7  .Value > 0) and
       (MAT.TorsionalStressTauUE7  .Value > 0) and
       (MAT.TorsionalStressTauYield.Value > 0) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E7');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := GetValue(MAT.TorsionalStressTauOE7);
      Points[1].X := GetValue(MAT.TorsionalStressTauUE7);
      Points[1].Y := GetValue(MAT.TorsionalStressTauYield);
      Points[2].X := GetValue(MAT.TorsionalStressTauYield);
      Points[2].Y := GetValue(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E7 Cycles');
      Result.AddLabel(
        GetValue(MAT.TorsionalStressTauUE7),
        GetValue(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E7 Cycles');
      Points := nil;
    end;
  end;
end;

function TCompozer.CreateBucklingChart(const AScreenScale: double): TChart;
var
  X, Y: single;
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Buckling diagram';
  Result.XAxisLabel := 'nu*L0/D';
  Result.YAxisLabel := 's/L0';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Draw buckling curve
  SOLVER1.GetBucklingCurve(Points);
  if (Length(Points) > 0) then
  begin
    LoadChart2(Result, 'BucklingChart', 'BucklingCurve');
    Result.XMinF := 0.0;
    Result.YMinF := 0.0;
    Result.YMaxF := 1.0;
    Result.YDeltaF := 0.1;
    Result.YCount  := 10;
    Result.AddPolyLine(Points, False, 'Buckling-Curve');
    Points := nil;
  end;

  if (SOLVER1.Dm.Value > 0) and (SOLVER1.LengthL0.Value > 0) then
  begin
    LoadChart2(Result, 'BucklingChart', 'Sc');
    X := SOLVER1.SeatingCoefficent * SOLVER1.LengthL0 / SOLVER1.Dm;

    if SOLVER1.StrokeSc > SOLVER1.DeflectionSk then
      Y := SOLVER1.StrokeSc / SOLVER1.LengthL0
    else
      Y := SOLVER1.DeflectionSk / SOLVER1.LengthL0;

    SetLength(Points, 2);
    Points[0].x := X;
    Points[0].y := 0;
    Points[1].x := X;
    Points[1].y := Y;
    Result.AddPolyLine(Points, False, 'Sc');
    Result.AddDotLabel(X, SOLVER1.StrokeSc / SOLVER1.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'Sc');
    Points := nil;
    LoadChart2(Result, 'BucklingChart', 'Sx');
    Result.AddDotLabel(X, SOLVER1.StrokeS1 / SOLVER1.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S1');
    Result.AddDotLabel(X, SOLVER1.StrokeS2 / SOLVER1.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S2');
  end;
end;

function TCompozer.CreateLoadF1Chart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Load F1-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F1[' + GetSymbol(SOLVER1.LoadF1) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SOLVER1.GetF1(MAT.Tempetature - DeltaTemp).Value > 0) and
     (SOLVER1.GetF1(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(SOLVER1.GetF1(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(SOLVER1.GetF1(Points[1].x));
    Result.AddPolyLine(Points, True, 'F1(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature, GetValue(SOLVER1.GetF1(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateLoadF2Chart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Load F2-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F2[' + GetSymbol(SOLVER1.LoadF2) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SOLVER1.GetF2(MAT.Tempetature - DeltaTemp).Value > 0) and
     (SOLVER1.GetF2(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(SOLVER1.GetF2(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(SOLVER1.GetF2(Points[1].x));
    Result.AddPolyLine(Points, True, 'F2(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature, GetValue(SOLVER1.GetF2(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateShearModulusChart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Shear Modulus G-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'G[' + GetSymbol(SOLVER1.ShearModulus) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (MAT.GetG(MAT.Tempetature - DeltaTemp).Value > 0) and
     (MAT.GetG(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(MAT.GetG(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(MAT.GetG(Points[1].x));
    Result.AddPolyLine(Points, True, 'G(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature, GetValue(MAT.GetG(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateYoungModulusChart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Young Modulus G-Temperature Chart';
  Result.XAxisLabel := 'T [C°]';
  Result.YAxisLabel := 'E [' + GetSymbol(SOLVER1.YoungModulus) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (MAT.GetE(MAT.Tempetature - DeltaTemp).Value > 0) and
     (MAT.GetE(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(MAT.GetE(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(MAT.GetE(Points[1].x));
    Result.AddPolyLine(Points, True, 'E(T°)');
    Points := nil;

    Result.AddDotLabel(MAT.Tempetature, GetValue(MAT.GetE(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateSectionSpringDrawing(
  const AScreenScale: double): TSectionSpringDrawing;
begin
  Result := TSectionSpringDrawing.Create;
  LoadSpring(Result, 'SpringDrawing', 'Spring');
  Result.d  := GetValue(SOLVER1.WireDiameter);
  Result.Dm := GetValue(SOLVER1.Dm);
  Result.Lc := GetValue(SOLVER1.LengthLc);
  Result.n  := SOLVER1.ActiveColis;
  Result.nt1 := (SOLVER1.TotalCoils - SOLVER1.ActiveColis) / 2;
  Result.nt2 := (SOLVER1.TotalCoils - SOLVER1.ActiveColis) / 2;
  Result.ClosedEnds := False;
  Result.GroundEnds := False;
  Result.Spacer := DefaultSpacer;
  Result.Scale  := AScreenScale;
end;

function TCompozer.CreateQualityTable(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 5;
  Result.RowCount := 7;
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');

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
  Result[1, 4] := '± ' + TryFloatToText(GetValue(TOL.CoilDiameterTolerance));
  Result[2, 4] := '± ' + TryFloatToText(GetValue(TOL.LengthL0Tolerance));
  Result[3, 4] := '± ' + TryFloatToText(GetValue(TOL.LoadF1Tolerance));
  Result[4, 4] := '± ' + TryFloatToText(GetValue(TOL.LoadF2Tolerance));
  Result[5, 4] := '± ' + TryFloatToText(GetValue(TOL.EccentricityE1));
  Result[6, 4] := '± ' + TryFloatToText(GetValue(TOL.EccentricityE2));
end;

function TCompozer.CreateQuick1Table(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 7;
  Result.RowCount := 6;
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');
  {$IFDEF MODULE1}
  Result[0, 0] := 'L [' + GetSymbol(SOLVER1.LengthL0) + ']';
  Result[1, 0] := TryFormatFloat('L0: %s', 'L0: ---', GetValue(SOLVER1.LengthL0));
  Result[2, 0] := TryFormatFloat('L1: %s', 'L1: ---', GetValue(SOLVER1.LengthL1));
  Result[3, 0] := TryFormatFloat('L2: %s', 'L2: ---', GetValue(SOLVER1.LengthL2));
  Result[4, 0] := TryFormatFloat('Ln: %s', 'Ln: ---', GetValue(SOLVER1.LengthLn));
  Result[5, 0] := TryFormatFloat('Lc: %s', 'Lc: ---', GetValue(SOLVER1.LengthLc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 0] := 'alpha [' + GetSymbol(0*deg) + ']';
  Result[1, 0] := TryFormatFloat('α0: %s', 'α0: ---', GetValue(0*deg));
  Result[2, 0] := TryFormatFloat('α1: %s', 'α1: ---', GetValue(SOLVER3.Alpha1));
  Result[3, 0] := TryFormatFloat('α2: %s', 'α2: ---', GetValue(SOLVER3.Alpha2));
  Result[4, 0] := TryFormatFloat('αn: %s', 'αn: ---', GetValue(0*deg));
  Result[5, 0] := TryFormatFloat('αc: %s', 'αc: ---', GetValue(0*deg));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 1] := 'F [' + GetSymbol(SOLVER1.LoadF1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('F1: %s', 'F1: ---', GetValue(SOLVER1.LoadF1));
  Result[3, 1] := TryFormatFloat('F2: %s', 'F2: ---', GetValue(SOLVER1.LoadF2));
  Result[4, 1] := TryFormatFloat('Fn: %s', 'Fn: ---', GetValue(SOLVER1.LoadFn));
  Result[5, 1] := TryFormatFloat('Fc: %s', 'Fc: ---', GetValue(SOLVER1.LoadFc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 1] := 'T [' + GetSymbol(SOLVER3.TorqueT1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('T1: %s', 'T1: ---', GetValue(SOLVER1.TorqueT1));
  Result[3, 1] := TryFormatFloat('T2: %s', 'T2: ---', GetValue(SOLVER1.TorqueT2));
  Result[4, 1] := TryFormatFloat('Tn: %s', 'Tn: ---', GetValue(SOLVER1.TorqueTn));
  Result[5, 1] := TryFormatFloat('Tc: %s', 'Tc: ---', GetValue(SOLVER1.TorqueTc));
  {$ENDIF}

  Result[0, 2] := 'tau [' + GetSymbol(SOLVER1.TorsionalStressTauk1) + ']';
  Result[1, 2] := '';
  Result[2, 2] := TryFormatFloat('tauk1: %s', 'tauk1: ---', GetValue(SOLVER1.TorsionalStressTauk1));
  Result[3, 2] := TryFormatFloat('tauk2: %s', 'tauk2: ---', GetValue(SOLVER1.TorsionalStressTauk2));
  Result[4, 2] := TryFormatFloat('tau n: %s', 'tau n: ---', GetValue(SOLVER1.TorsionalStressTaun));
  Result[5, 2] := TryFormatFloat('tau c: %s', 'tau c: ---', GetValue(SOLVER1.TorsionalStressTauc));

  Result[0, 3] := 's [' + GetSymbol(SOLVER1.LengthL1) + ']';
  Result[1, 3] := '';
  Result[2, 3] := TryFormatFloat('s1: %s', 's1: ---', GetValue(SOLVER1.StrokeS1));
  Result[3, 3] := TryFormatFloat('s2: %s', 's2: ---', GetValue(SOLVER1.StrokeS2));
  Result[4, 3] := TryFormatFloat('sn: %s', 'sn: ---', GetValue(SOLVER1.StrokeSn));
  Result[5, 3] := TryFormatFloat('sc: %s', 'sc: ---', GetValue(SOLVER1.StrokeSc));

  Result[0, 4] := 'tau/tauz';
  Result[1, 4] := '';
  Result[2, 4] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTau1),
    GetValue(SOLVER1.AdmStaticTorsionalStressTauz));
  Result[3, 4] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTau2),
    GetValue(SOLVER1.AdmStaticTorsionalStressTauz));
  Result[4, 4] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTaun),
    GetValue(SOLVER1.AdmStaticTorsionalStressTauz));
  Result[5, 4] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTauc),
    GetValue(SOLVER1.AdmStaticTorsionalStressTauz));

  Result[0, 5] := 'tau/Rm';
  Result[1, 5] := '';
  Result[2, 5] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTau1),
    GetValue(SOLVER1.TensileStrengthRm));
  Result[3, 5] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTau2),
    GetValue(SOLVER1.TensileStrengthRm));
  Result[4, 5] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTaun),
    GetValue(SOLVER1.TensileStrengthRm));
  Result[5, 5] := TryFormatFloatDiv('%s', '---', GetValue(SOLVER1.TorsionalStressTauc),
    GetValue(SOLVER1.TensileStrengthRm));

  Result[0, 6] := 'De [' + GetSymbol(SOLVER1.De) + ']';
  Result[1, 6] := TryFormatFloat('%s', '---', GetValue(SOLVER1.De));
  Result[2, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SOLVER1.De),
    GetValue(SOLVER1.DeltaDe) * GetValue(SOLVER1.StrokeS1), GetValue(SOLVER1.StrokeSc));

  Result[3, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SOLVER1.De),
    GetValue(SOLVER1.DeltaDe) * GetValue(SOLVER1.StrokeS2), GetValue(SOLVER1.StrokeSc));

  Result[4, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SOLVER1.De),
    GetValue(SOLVER1.DeltaDe) * GetValue(SOLVER1.StrokeSn), GetValue(SOLVER1.StrokeSc));

  Result[5, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SOLVER1.De),
    GetValue(SOLVER1.DeltaDe) * GetValue(SOLVER1.StrokeSc), GetValue(SOLVER1.StrokeSc));
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
  Result.Items[0, 2] := TryFormatFloat('%s', '---',
    GetValue(SOLVER1.WireDiameter)) +
    TryFormatFloat(' ± %s ' + GetSymbol(SOLVER1.WireDiameter), '',
    GetValue(SOLVER1.WireDiameterMax - SOLVER1.WireDiameter));

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.Di),
    '---', GetValue(SOLVER1.Di));

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.Dm),
    '---', GetValue(SOLVER1.Dm));

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] := TryFormatFloat('%s', '---', GetValue(SOLVER1.De)) +
    TryFormatFloat(' ± %s ' + GetSymbol(TOL.CoilDiameterTolerance), '',
    GetValue(TOL.CoilDiameterTolerance));

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := TryFormatFloat('%s coils', '---', SOLVER1.ActiveColis);

  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := TryFormatFloat('%s coils', '---', SOLVER1.TotalCoils);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.SpringRateR), '---', GetValue(SOLVER1.SpringRateR));

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.De),
    '---', GetValue(SOLVER1.De + SOLVER1.DeltaDe));

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.DiMin),
    '---', GetValue(SOLVER1.DiMin));

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.DeMax),
    '---', GetValue(SOLVER1.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.DeflectionSk),
    '---', GetValue(SOLVER1.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.WireLength),
    '---', GetValue(SOLVER1.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.Mass),
    '---', GetValue(SOLVER1.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.SpringWorkW12),
    '---', GetValue(SOLVER1.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.SpringWorkW0n),
    '---', GetValue(SOLVER1.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s ' + GetSymbol(SOLVER1.NaturalFrequency),
    '---', GetValue(SOLVER1.NaturalFrequency));

  Result.Items[16, 0] := 'nu';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s', '---', SOLVER1.SeatingCoefficent);

  Result.Items[17, 0] := 'load';
  Result.Items[17, 1] := '=';

  if SOLVER1.DynamicLoad then
    Result.Items[17, 2] := ('dynamic')
  else
    Result.Items[17, 2] := ('static');

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

  k := 2;
  Result.Items[0+k, 0] := 'tauk1';
  Result.Items[0+k, 1] := '=';
  Result.Items[0+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk1), '---',
    GetValue (SOLVER1.TorsionalStressTauk1));

  Result.Items[1+k, 0] := 'tauk2';
  Result.Items[1+k, 1] := '=';
  Result.Items[1+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk2), '---',
    GetValue (SOLVER1.TorsionalStressTauk2));

  Result.Items[2+k, 0] := 'taukh';
  Result.Items[2+k, 1] := '=';
  Result.Items[2+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTaukh), '---',
    GetValue (SOLVER1.TorsionalStressTaukh));

  Result.Items[3+k, 0] := 'E';
  Result.Items[3+k, 1] := '=';
  Result.Items[3+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.YoungModulus), '---',
    GetValue (SOLVER1.YoungModulus));

  Result.Items[4+k, 0] := 'G';
  Result.Items[4+k, 1] := '=';
  Result.Items[4+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.ShearModulus), '---',
    GetValue (SOLVER1.ShearModulus));

  Result.Items[5+k, 0] := 'rho';
  Result.Items[5+k, 1] := '=';
  Result.Items[5+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.MaterialDensity), '---',
    GetValue (SOLVER1.MaterialDensity));

  Result.Items[6+k, 0] := 'Rm';
  Result.Items[6+k, 1] := '=';
  Result.Items[6+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TensileStrengthRm), '---',
    GetValue (SOLVER1.TensileStrengthRm));

  Result.Items[7+k, 0] := 'tauz';
  Result.Items[7+k, 1] := '=';
  Result.Items[7+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.AdmStaticTorsionalStressTauz), '---',
    GetValue (SOLVER1.AdmStaticTorsionalStressTauz));

  Result.Items[8+k, 0] := 'ns';
  Result.Items[8+k, 1] := '=';
  Result.Items[8+k, 2] := TryFormatFloat('%s', '---', SOLVER1.StaticSafetyFactor);

  if SOLVER1.DynamicLoad then
  begin
    Result.Items[9+k, 0] := 'tauoz';
    Result.Items[9+k, 1] := '=';
    Result.Items[9+k, 2] := TryFormatFloat('%s ' +
      GetSymbol(SOLVER1.AdmDynamicTorsionalStressTauoz), '---',
      GetValue (SOLVER1.AdmDynamicTorsionalStressTauoz));

    Result.Items[10+k, 0] := 'tauhz';
    Result.Items[10+k, 1] := '=';
    Result.Items[10+k, 2] := TryFormatFloat('%s ' +
      GetSymbol(SOLVER1.AdmDynamicTorsionalStressRangeTauhz), '---',
      GetValue (SOLVER1.AdmDynamicTorsionalStressRangeTauhz));

    Result.Items[11+k, 0] := 'nf';
    Result.Items[11+k, 1] := '=';
    Result.Items[11+k, 2] := TryFormatFloat('%s', '---', SOLVER1.DynamicSafetyFactor);

    if SOLVER1.NumOfCycles > 0 then
    begin
      Result.Items[12+k, 0] := 'N';
      Result.Items[12+k, 1] := '=';
      Result.Items[12+k, 2] := TryFormatText('%s cycles', '---',
        TryFloatToText(SOLVER1.NumOfCycles, 2, 0));

      Result.Items[13+k, 0] := 'Nh';
      Result.Items[13+k+k, 1] := '=';
      Result.Items[13, 2] := TryFormatFloatDiv('%s hours', '---',
        SOLVER1.NumOfCycles, 3600 * SOLVER1.CycleFrequency.Value);
    end
    else
    begin
      Result.Items[14+k, 0] := 'N';
      Result.Items[14+k, 1] := '=';
      Result.Items[14+k, 2] := '---';

      Result.Items[15+k, 0] := 'Nh';
      Result.Items[15+k, 1] := '=';
      Result.Items[15+k, 2] := '---';
    end;
  end;
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

  Result.Items[0, 0] := TryFormatBool('Messages:', '',
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

  for i := 0 to Result.RowCount - 1 do
    for j := 0 to Result.ColumnCount - 1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  Result.Items[0, 0] := 'd';
  Result.Items[0, 1] := '=';
  Result.Items[0, 2] :=
    TryFormatFloat('%s', '---', GetValue(SOLVER1.WireDiameter)) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(SOLVER1.WireDiameter), '',
      GetValue (SOLVER1.WireDiameterMax - SOLVER1.WireDiameter));

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.Di), '---',
    GetValue (SOLVER1.Di));

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.Dm), '---',
    GetValue (SOLVER1.Dm));

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] :=
    TryFormatFloat('%s', '---', GetValue(SOLVER1.De)) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(TOL.CoilDiameterTolerance), '',
      GetValue (TOL.CoilDiameterTolerance));

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := TryFormatFloat('%s coils', '---', SOLVER1.ActiveColis);

  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := TryFormatFloat('%s coils', '---', SOLVER1.TotalCoils);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.SpringRateR), '---',
    GetValue (SOLVER1.SpringRateR));

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.De), '---',
    GetValue (SOLVER1.De + SOLVER1.DeltaDe));

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.DiMin), '---',
    GetValue (SOLVER1.DiMin));

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.DeMax), '---',
    GetValue (SOLVER1.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.DeflectionSk), '---',
    GetValue (SOLVER1.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.WireLength), '---',
    GetValue (SOLVER1.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.Mass), '---',
    GetValue (SOLVER1.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.SpringWorkW12), '---',
    GetValue (SOLVER1.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.SpringWorkW0n), '---',
    GetValue (SOLVER1.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.NaturalFrequency), '---',
    GetValue (SOLVER1.NaturalFrequency));

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.Pitch), '---', GetValue(SOLVER1.Pitch));

  Result.Items[17, 0] := 'PitchRatio';
  Result.Items[17, 1] := '=';
  Result.Items[17, 2] := TryFormatFloat('%s', '---', SOLVER1.PitchRatio);

  Result.Items[18, 0] := 'nu';
  Result.Items[18, 1] := '=';
  Result.Items[18, 2] := TryFormatFloat('%s', '---', SOLVER1.SeatingCoefficent);

  Result.Items[19, 0] := 'load';
  Result.Items[19, 1] := '=';

  if SOLVER1.DynamicLoad then
    Result.Items[19, 2] := ('dynamic')
  else
    Result.Items[19, 2] := ('static');

  Result.Items[22, 0] := 'tauk1';
  Result.Items[22, 1] := '=';
  Result.Items[22, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk1), '---',
    GetValue (SOLVER1.TorsionalStressTauk1));

  Result.Items[23, 0] := 'tauk2';
  Result.Items[23, 1] := '=';
  Result.Items[23, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk2), '---',
    GetValue (SOLVER1.TorsionalStressTauk2));

  Result.Items[24, 0] := 'taukh';
  Result.Items[24, 1] := '=';
  Result.Items[24, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTaukh), '---',
    GetValue (SOLVER1.TorsionalStressTaukh));

  Result.Items[26, 0] := 'E';
  Result.Items[26, 1] := '=';
  Result.Items[26, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.YoungModulus), '---',
    GetValue (SOLVER1.YoungModulus));

  Result.Items[27, 0] := 'G';
  Result.Items[27, 1] := '=';
  Result.Items[27, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.ShearModulus), '---',
    GetValue (SOLVER1.ShearModulus));

  Result.Items[28, 0] := 'rho';
  Result.Items[28, 1] := '=';
  Result.Items[28, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.MaterialDensity), '---',
    GetValue (SOLVER1.MaterialDensity));

  Result.Items[29, 0] := 'Rm';
  Result.Items[29, 1] := '=';
  Result.Items[29, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TensileStrengthRm), '---',
    GetValue (SOLVER1.TensileStrengthRm));

  Result.Items[30, 0] := 'tauz';
  Result.Items[30, 1] := '=';
  Result.Items[30, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.AdmStaticTorsionalStressTauz), '---',
    GetValue (SOLVER1.AdmStaticTorsionalStressTauz));

  Result.Items[31, 0] := 'ns';
  Result.Items[31, 1] := '=';
  Result.Items[31, 2] := TryFormatFloat('%s', '---', SOLVER1.StaticSafetyFactor);

  if SOLVER1.DynamicLoad then
  begin
    Result.Items[33, 0] := 'tauoz';
    Result.Items[33, 1] := '=';
    Result.Items[33, 2] := TryFormatFloat('%s ' +
      GetSymbol(SOLVER1.AdmDynamicTorsionalStressTauoz), '---',
      GetValue (SOLVER1.AdmDynamicTorsionalStressTauoz));

    Result.Items[34, 0] := 'tauhz';
    Result.Items[34, 1] := '=';
    Result.Items[34, 2] := TryFormatFloat('%s ' +
      GetSymbol(SOLVER1.AdmDynamicTorsionalStressRangeTauhz), '---',
      GetValue (SOLVER1.AdmDynamicTorsionalStressRangeTauhz));

    Result.Items[35, 0] := 'nf';
    Result.Items[35, 1] := '=';
    Result.Items[35, 2] := TryFormatFloat('%s', '---', SOLVER1.DynamicSafetyFactor);

    if SOLVER1.NumOfCycles > 0 then
    begin
      Result.Items[36, 0] := 'N';
      Result.Items[36, 1] := '=';
      Result.Items[36, 2] := TryFormatText('%s cycles', '---',
        TryFloatToText(SOLVER1.NumOfCycles, 2, 0));

      Result.Items[37, 0] := 'Nh';
      Result.Items[37, 1] := '=';
      Result.Items[37, 2] := TryFormatFloatDiv('%s hours', '---',
        SOLVER1.NumOfCycles, 3600 * SOLVER1.CycleFrequency.Value);
    end
    else
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
  SpringDrawing: TSectionSpringDrawing;
begin
  SetLength(Bit, 9);
  for i := Low(Bit) to High(Bit) do
    Bit[i] := TBGRABitmap.Create;

  // 0-Quick-1 List
  Quick1List := CreateQuick1List1(aScreenScale);
  Bit[0].SetSize(Quick1List.Width, Quick1List.Height);
  Quick1List.Draw(Bit[0].Canvas);
  Quick1List.Destroy;

  // 1-Quick-2 List
  Quick1List := CreateQuick1List2(aScreenScale);
  Bit[1].SetSize(Quick1List.Width, aScreen.Height - Bit[0].Height);
  Quick1List.Draw(Bit[1].Canvas);
  Bit[1].Draw(aScreen.Canvas, aScreen.Width - Bit[1].Width, Bit[0].Height, True);
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
  Bit[4].SetSize(Bit[1].Width + Bit[3].Width - Bit[0].Width, Quick1Table.Height);
  Quick1Table.Draw(Bit[4].Canvas);
  Bit[4].Draw(aScreen.Canvas, aScreen.Width - Bit[4].Width, 0, True);
  Quick1Table.Destroy;

  // 5-Quality Table
  QualityTable := CreateQualityTable(aScreenScale);
  Bit[5].SetSize(QualityTable.Width, Bit[0].Height - Bit[4].Height);
  QualityTable.Draw(Bit[5].Canvas);
  Bit[5].Draw(aScreen.Canvas, aScreen.Width - Bit[4].Width, Bit[4].Height, True);
  QualityTable.Destroy;

  // 6-Message List
  MessageList := CreateMessageList(aScreenScale);
  Bit[6].SetSize(Bit[4].Width - Bit[5].Width, Bit[5].Height);
  MessageList.Draw(Bit[6].Canvas);
  Bit[6].Draw(aScreen.Canvas, aScreen.Width - Bit[6].Width, Bit[4].Height, True);
  MessageList.Destroy;

  // 6-Spring Drawings
  SpringDrawing := CreateSectionSpringDrawing(AScreenScale);
  Bit[6].SetSize(Bit[2].Width div 3, Bit[0].Height);
  Bit[7].SetSize(Bit[2].Width div 3, Bit[0].Height);
  Bit[8].SetSize(Bit[2].Width - Bit[6].Width - Bit[7].Width, Bit[0].Height);

  SpringDrawing.AutoFit := True;
  SpringDrawing.Lx := GetValue(SOLVER1.LengthL0);
  SpringDrawing.Caption := TryFormatFloat('L0 = %s', 'L0 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := GetValue(SOLVER1.LengthL1);
  SpringDrawing.Caption := TryFormatFloat('L1 = %s', 'L1 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[7].Canvas, Bit[7].Width, Bit[7].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := GetValue(SOLVER1.LengthL2);
  SpringDrawing.Caption := TryFormatFloat('L2 = %s', 'L2 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[8].Canvas, Bit[8].Width, Bit[8].Height);

  Bit[6].Draw(aScreen.Canvas, 0, 0, True);
  Bit[7].Draw(aScreen.Canvas, Bit[6].Width, 0, True);
  Bit[8].Draw(aScreen.Canvas, Bit[6].Width + Bit[7].Width, 0, True);
  SpringDrawing.Destroy;

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
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' , '---',
    GetValue (SOLVER1.WireDiameter)) + TryFormatFloat(' ± %s mm', '',
    GetValue (SOLVER1.WireDiameterMax - SOLVER1.WireDiameter));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.Di), '---', GetValue(SOLVER1.Di));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Dm';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' , '---',
    GetValue (SOLVER1.Dm)) + TryFormatFloat(' ± %s ' +
    GetSymbol(TOL.CoilDiameterTolerance), '',
    GetValue (TOL.CoilDiameterTolerance));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'De';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] :=
    TryFormatFloat('%s ' + GetSymbol(SOLVER1.De) , '---', GetValue(SOLVER1.De));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'n';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s coils', '---', SOLVER1.ActiveColis);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'nt';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s colis', '---', SOLVER1.TotalCoils);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'nu';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s', '---', SOLVER1.SeatingCoefficent);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ', '---',
    GetValue (SOLVER1.LengthL0)) + TryFormatFloat(' ± %s ' +
    GetSymbol(TOL.LengthL0Tolerance), '',
    GetValue (TOL.LengthL0Tolerance));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L1';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LengthL1), '---',
    GetValue (SOLVER1.LengthL1));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L2';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LengthL2), '---',
    GetValue (SOLVER1.LengthL2));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Ln';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LengthLn), '---',
    GetValue (SOLVER1.LengthLn));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Lc';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LengthLc), '---',
    GetValue (SOLVER1.LengthLc));

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'Quality specs.';
  QuickXList.Items[Row, 2] := 'EN15800';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di, Dm, De';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.DmQualityGrade);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.L0QualityGrade);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.F1QualityGrade);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.F2QualityGrade);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.E1QualityGrade);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', TOL.E2QualityGrade);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'Spring ends';
  QuickXList.Items[Row, 1] := '=';
  if SOLVER1.ClosedEnds then
  begin
    if SOLVER1.GroundEnds then
      QuickXList.Items[Row, 2] := 'Closed and ground'
    else
      QuickXList.Items[Row, 2] := 'Closed';
  end
  else
    QuickXList.Items[Row, 2] := 'Open';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Coiling type';
  QuickXList.Items[Row, 1] := '=';
  if SOLVER1.ColdCoiled then
    QuickXList.Items[Row, 2] := 'Cold coiled'
  else
    QuickXList.Items[Row, 2] := 'Hot coiled';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Load type';
  QuickXList.Items[Row, 1] := '=';
  if SOLVER1.DynamicLoad then
    QuickXList.Items[Row, 2] := 'Dynamic'
  else
    QuickXList.Items[Row, 2] := 'Static';

  // 456
  Row := 2;
  QuickXList.Items[Row, 4] := 'Material';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatText('%s', '---', MAT.Items[MAT.ItemIndex]);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Rm';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TensileStrengthRm), '---',
    GetValue (SOLVER1.TensileStrengthRm));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'G';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.ShearModulus), '---',
    GetValue (SOLVER1.ShearModulus));

  Inc(Row);
  QuickXList.Items[Row, 4] := Format('G(%s°)', [TryFloatToText(MAT.Tempetature)]);
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(MAT.GetG(MAT.Tempetature)), '---',
    GetValue (MAT.GetG(MAT.Tempetature)));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'rho';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.MaterialDensity), '---',
    GetValue (SOLVER1.MaterialDensity));

  Inc(Row, 3);
  QuickXList.Items[Row, 4] := 'F1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] :=
    TryFormatFloat('%s ' , '---', SOLVER1.LoadF1.Value) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(TOL.LoadF1Tolerance), '',
      GetValue (TOL.LoadF1Tolerance));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'F2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] :=
    TryFormatFloat('%s ', '---', SOLVER1.LoadF2.Value) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(TOL.LoadF2Tolerance), '',
      GetValue (TOL.LoadF2Tolerance));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fn';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LoadFn), '---',
    GetValue (SOLVER1.LoadFn));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fc';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.LoadFc), '---',
    GetValue (SOLVER1.LoadFc));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauk1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk1), '---',
    GetValue (SOLVER1.TorsionalStressTauk1));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauk2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTauk2), '---',
    GetValue (SOLVER1.TorsionalStressTauk2));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'taukh';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.TorsionalStressTaukh), '---',
    GetValue (SOLVER1.TorsionalStressTaukh));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauhz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.AdmDynamicTorsionalStressRangeTauhz), '---',
    GetValue (SOLVER1.AdmDynamicTorsionalStressRangeTauhz));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauoz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.AdmDynamicTorsionalStressTauoz), '---',
    GetValue (SOLVER1.AdmDynamicTorsionalStressTauoz));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SOLVER1.AdmStaticTorsionalStressTauz), '---',
    GetValue (SOLVER1.AdmStaticTorsionalStressTauz));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'Static safety factor';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SOLVER1.StaticSafetyFactor);

  Inc(Row);
  if SOLVER1.DynamicLoad then
  begin
    QuickXList.Items[Row, 4] := 'Dynamic safety factor';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SOLVER1.DynamicSafetyFactor);
    if SOLVER1.NumOfCycles > 0 then
    begin
      Inc(Row);
      QuickXList.Items[Row, 4] := 'N';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] :=
        TryFormatText('%s cycles', '---', TryFloatToText(SOLVER1.NumOfCycles, 2, 0));
      Inc(Row);
      QuickXList.Items[Row, 4] := 'Nh';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] :=
        TryFormatFloatDiv('%s hours', '---', SOLVER1.NumOfCycles,
        3600 * SOLVER1.CycleFrequency.Value);
    end
    else
    begin
      Inc(Row);
      QuickXList.Items[Row, 4] := 'N';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := '---';
      Inc(Row);
      QuickXList.Items[Row, 4] := 'Nh';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := '---';
    end;
  end;

  // 0-List
  Bit[0].SetSize(QuickXList.Width, QuickXList.Height);
  QuickXList.Draw(Bit[0].Canvas);
  Bit[0].Draw(aScreen.Canvas, aScreen.Width - Bit[0].Width, 0, True);
  QuickXList.Destroy;

  // 1-Messages
  MessageList := CreateMessageList(aScreenScale);
  Bit[1].SetSize(Bit[0].Width, aScreen.Height - Bit[0].Height);
  MessageList.Draw(Bit[1].Canvas);
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

end.
