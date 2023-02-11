unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap,
  BGRABitmapTypes,
  Classes,
  GraphBase,
  Graphics,
  IniFiles,
  Math,
  SysUtils,
  UnitOfMeasurement;

type
  TCompozer = class
  private
    FSetting: TIniFile;
    FDensityUnit: TDensityUnit;
    FForceUnit: TForceUnit;
    FFrequencyUnit: TFrequencyUnit;
    FLengthUnit: TLengthUnit;
    FAreaUnit: TAreaUnit;
    FMassUnit: TMassUnit;
    FPressureUnit: TPressureUnit;
    FStiffnessUnit: TStiffnessUnit;
    FTimeUnit: TTimeUnit;
    FWorkUnit: TWorkUnit;
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
    function CreateGoodmanChart          (const aScreenScale: double): TChart;
    function CreateBucklingChart         (const AScreenScale: double): TChart;
    function CreateLoadF1Chart           (const AScreenScale: double): TChart;
    function CreateLoadF2Chart           (const AScreenScale: double): TChart;
    function CreateShearModulusChart     (const AScreenScale: double): TChart;
    function CreateYoungModulusChart     (const AScreenScale: double): TChart;

    function CreateSectionSpringDrawing  (const AScreenScale: double): TSectionSpringDrawing;
    function CreateQualityTable          (const AScreenScale: double): TReportTable;
    function CreateQuick1Table           (const AScreenScale: double): TReportTable;
    function CreateQuick1List            (const AScreenScale: double): TReportTable;
    function CreateMessageList           (const AScreenScale: double): TReportTable;
    function CreateQuick1AList           (const AScreenScale: double): TReportTable;

  public
    property AreaUnit: TAreaUnit read FAreaUnit write FAreaUnit;
    property DensityUnit: TDensityUnit read FDensityUnit write FDensityUnit;
    property ForceUnit: TForceUnit read FForceUnit write FForceUnit;
    property FrequencyUnit: TFrequencyUnit read FFrequencyUnit write FFrequencyUnit;
    property LengthUnit: TLengthUnit read FLengthUnit write FLengthUnit;
    property MassUnit: TMassUnit read FMassUnit write FMassUnit;
    property PressureUnit: TPressureUnit read FPressureUnit write FPressureUnit;
    property StiffnessUnit: TStiffnessUnit read FStiffnessUnit write FStiffnessUnit;
    property TimeUnit: TTimeUnit read FTimeUnit write FTimeUnit;
    property WorkUnit: TWorkUnit read FWorkUnit write FWorkUnit;
  end;


implementation

uses
  EN10270, EN13906, EN15800, UtilsBase, ApplicationFrm;

// TCompozer //

constructor TCompozer.Create(aSetting: TIniFile);
begin
  inherited Create;
  FSetting       := aSetting;
  FAreaUnit      := mm2;
  FDensityUnit   := g_dm3;
  FForceUnit     := N;
  FFrequencyUnit := Hz;
  FLengthUnit    := mm;
  FMassUnit      := g;
  FPressureUnit  := MPa;
  FStiffnessUnit := N_mm;
  FTimeUnit      := hr;
  FWorkUnit      := Nm;
end;

destructor TCompozer.Destroy;
begin
  inherited Destroy;
end;

function TCompozer.LoadFontStyle(const ASection, AIdent: string): TFontStyles;
var
  FontStyle: string;
begin
  Result    := [];
  FontStyle := FSetting.ReadString(ASection, AIdent , 'Bold');
  if Pos('Bold',      FontStyle) > 0 then Include(Result, fsBold);
  if Pos('Italic',    FontStyle) > 0 then Include(Result, fsItalic);
  if Pos('Underline', FontStyle) > 0 then Include(Result, fsUnderline);
  if Pos('StrikeOut', FontStyle) > 0 then Include(Result, fsStrikeOut);
end;

function TCompozer.LoadPenStyle(const ASection, AIdent: string): TPenStyle;
var
  PenStyle: string;
begin
  Result   := psSolid;
  PenStyle := FSetting.ReadString(ASection, AIdent , 'Solid');
  if PenStyle = 'Solid'       then Result := psSolid;
  if PenStyle = 'Dash'        then Result := psDash;
  if PenStyle = 'Dot'         then Result := psDot;
  if PenStyle = 'DashDot'     then Result := psDashDot;
  if PenStyle = 'DashDotDot'  then Result := psDashDotDot;
  if PenStyle = 'InsideFrame' then Result := psInsideFrame;
  if PenStyle = 'Pattern'     then Result := psPattern;
  if PenStyle = 'Clear'       then Result := psClear;
end;

procedure TCompozer.LoadChart1(Chart: TChart; const ASection: string);
begin
  // Title
  Chart.TitleFontName   := FSetting.ReadString(ASection, 'TitleFontName', 'default');
  Chart.TitleFontHeight := FSetting.ReadFloat(ASection, 'TitleFontHeight', 14);
  Chart.TitleFontStyle  := LoadFontStyle(ASection, 'TitleFontStyle');
  Chart.TitleFontColor.FromString(FSetting.ReadString(ASection, 'TitleFontColor', 'Black'));
  // X Axis
  Chart.XAxisFontName   := FSetting.ReadString(ASection, 'XAxisFontName', 'default');
  Chart.XAxisFontHeight := FSetting.ReadFloat (ASection, 'XAxisFontHeight', 13);
  Chart.XAxisFontStyle  := LoadFontStyle(ASection, 'XAxisFontStyle');
  Chart.XAxisFontColor.FromString(FSetting.ReadString(ASection, 'XAxisFontColor', 'Black' ));
  Chart.XAxisLineColor.FromString(FSetting.ReadString(ASection, 'XAxisLineColor', 'Black' ));
  Chart.XGridLineColor.FromString(FSetting.ReadString(ASection, 'XGridLineColor', 'Silver'));
  Chart.XAxisLineWidth  := FSetting.ReadFloat(ASection, 'XAxisLineWidth' , 1.0);
  Chart.XGridLineWidth  := FSetting.ReadFloat(ASection, 'XGridLineWidth' , 1.0);
  Chart.XAxisLineStyle  := LoadPenStyle(ASection, 'XAxisLineStyle');
  Chart.XGridLineStyle  := LoadPenStyle(ASection, 'XGridLineStyle');
  // Y Axis
  Chart.YAxisFontName   := FSetting.ReadString(ASection, 'YAxisFontName', 'default');
  Chart.YAxisFontHeight := FSetting.ReadFloat (ASection, 'YAxisFontHeight', 13);
  Chart.YAxisFontStyle  := LoadFontStyle(ASection, 'YAxisFontStyle');
  Chart.YAxisFontColor.FromString(FSetting.ReadString(ASection, 'YAxisFontColor', 'Black' ));
  Chart.YAxisLineColor.FromString(FSetting.ReadString(ASection, 'YAxisLineColor', 'Black' ));
  Chart.YGridLineColor.FromString(FSetting.ReadString(ASection, 'YGridLineColor', 'Silver'));
  Chart.YAxisLineWidth  := FSetting.ReadFloat(ASection, 'YAxisLineWidth' , 1.0);
  Chart.YGridLineWidth  := FSetting.ReadFloat(ASection, 'YGridLineWidth' , 1.0);
  Chart.YAxisLineStyle  := LoadPenStyle(ASection, 'YAxisLineStyle');
  Chart.YGridLineStyle  := LoadPenStyle(ASection, 'YGridLineStyle');
  // General
  Chart.Color          .FromString(FSetting.ReadString(ASection, 'Color', 'White'));
  Chart.BackgroundColor.FromString(FSetting.ReadString(ASection, 'BackgroundColor', 'White'));
end;

procedure TCompozer.LoadChart2(Chart: TChart; const ASection, AIdent: string);
begin
  Chart.FontName   := FSetting.ReadString(ASection,  AIdent + '.FontName', 'default');
  Chart.FontHeight := FSetting.ReadFloat (ASection,  AIdent + '.FontHeight', 12);
  Chart.FontStyle  := LoadFontStyle(ASection, AIdent + '.FontStyle');
  Chart.FontColor.FromString(FSetting.ReadString(ASection, AIdent + '.FontColor', 'Black' ));

  Chart.PenWidth   := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Chart.PenStyle   := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Chart.PenColor.FromString(FSetting.ReadString (ASection, AIdent + '.PenColor',  'Black'));

  Chart.TextureBackgroundColor.FromString(
    FSetting.ReadString(ASection, AIdent + '.TextureBackgroundColor', 'White' ));

  Chart.TextureColor.FromString(
    FSetting.ReadString(ASection, AIdent + '.TextureColor', 'Black' ));

  Chart.TextureWidth    := FSetting.ReadInteger(ASection, AIdent + '.TextureWidth',      8);
  Chart.TextureHeight   := FSetting.ReadInteger(ASection, AIdent + '.TextureHeight',     8);
  Chart.TexturePenWidth := FSetting.ReadFloat  (ASection, AIdent + '.TexturePenWidth', 1.0);
end;

procedure TCompozer.LoadTable(Table: TReportTable; const ASection, AIdent: string);
begin
  Table.BackgroundColor.FromString(FSetting.ReadString(ASection, AIdent + '.BackgroundColor', 'White'));
  Table.BorderWidth:= FSetting.ReadInteger(ASection, AIdent + '.BorderWidth', 0);

  Table.FontName   := FSetting.ReadString(ASection,  AIdent + '.FontName', 'default');
  Table.FontHeight := FSetting.ReadFloat (ASection,  AIdent + '.FontHeight', 12);
  Table.FontStyle  := LoadFontStyle(ASection, AIdent + '.FontStyle');
  Table.FontColor.FromString(FSetting.ReadString(ASection, AIdent + '.FontColor', 'Black' ));
  Table.PenWidth   := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);
  Table.PenStyle   := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Table.PenColor.FromString(FSetting.ReadString (ASection, AIdent + '.PenColor',  'Black'));

  Table.RowSpacer := FSetting.ReadInteger(ASection, AIdent + '.RowSpacer', 0);
  Table.ColumnSpacer := FSetting.ReadInteger(ASection, AIdent + '.ColumnSpacer', 0);
end;

procedure TCompozer.LoadSpring(Spring: TSectionSpringDrawing; const ASection, AIdent: string);
begin
  Spring.BackgroundColor.FromString(FSetting.ReadString(ASection, AIdent + '.BackgroundColor', 'White'));
  Spring.CenterLineColor.FromString(FSetting.ReadString(ASection, AIdent + '.CenterLineColor', 'Black' ));
  Spring.CenterLineStyle := LoadPenStyle(ASection, AIdent + '.CenterLineStyle');
  Spring.CenterLineWidth := FSetting.ReadFloat(ASection, AIdent + '.CenterLineWidth', 1.0);

  Spring.FontColor.FromString(FSetting.ReadString(ASection, AIdent + '.FontColor', 'Black'));
  Spring.FontName   := FSetting.ReadString(ASection,  AIdent + '.FontName', 'default');
  Spring.FontHeight := FSetting.ReadFloat(ASection, AIdent + '.FontHeight', 13);
  Spring.FontStyle  := LoadFontStyle(ASection, AIdent);

  Spring.PenColor.FromString(FSetting.ReadString(ASection, AIdent + '.PenColor', 'Black'));
  Spring.PenStyle := LoadPenStyle(ASection, AIdent + '.PenStyle');
  Spring.PenWidth := FSetting.ReadFloat(ASection, AIdent + '.PenWidth', 1.0);

  Spring.TextureBackgroundColor.FromString(FSetting.ReadString(ASection, AIdent + '.TextureBackgroundColor', 'White'));
  Spring.TextureColor.FromString(FSetting.ReadString(ASection, AIdent + '.TextureColor', 'Black'));
  Spring.TextureHeight   := FSetting.ReadInteger(ASection, AIdent + '.TextureHeight', 8);
  Spring.TextureWidth    := FSetting.ReadInteger(ASection, AIdent + '.TextureWidth',  8);
  Spring.TexturePenWidth := FSetting.ReadFloat(ASection, AIdent + '.TexturePenWidth', 1.0);
end;

// ---

function TCompozer.CreateForceDisplacementChart(const AScreenScale: double): TChart;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Force & Displacement Chart';
  Result.XAxisLabel := 's[' + FLengthUnit.Symbol   + ']';
  Result.YAxisLabel := 'F[' + FForceUnit.Symbol + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Draw bisector line
  if SOLVER.LoadFc > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := LengthUnit.Value(SOLVER.StrokeSc);
    Points[1].Y := ForceUnit.Value(SOLVER.LoadFc);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  // Draw tolerance lines
  if (TOL.LoadF1 > (0*N)) and (TOL.LoadF2 > (0*N)) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'ToleranceLine');
    SetLength(Points, 2);
    Points[0].X := LengthUnit.Value(SOLVER.StrokeS1);
    Points[0].Y := ForceUnit.Value(TOL.LoadF1 + TOL.LoadF1Tolerance);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeS2);
    Points[1].Y := ForceUnit.Value(TOL.LoadF2 + TOL.LoadF2Tolerance);
    Result.AddPolyLine(Points, True, 'Tolerance +');
    Points[0].X := LengthUnit.Value(SOLVER.StrokeS1);
    Points[0].Y := ForceUnit.Value(TOL.LoadF1 - TOL.LoadF1Tolerance);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeS2);
    Points[1].Y := ForceUnit.Value(TOL.LoadF2 - TOL.LoadF2Tolerance);
    Result.AddPolyLine(Points, True, 'Tolerance -');
    Points := nil;
  end;
  // Draw Load-F1
  if SOLVER.LoadF1 > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := ForceUnit.Value(SOLVER.LoadF1);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeS1);
    Points[1].Y := ForceUnit.Value(SOLVER.LoadF1);
    Points[2].X := LengthUnit.Value(SOLVER.StrokeS1);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F1');
    Points := nil;
  end;
  // Draw  Load F2
  if SOLVER.LoadF2 > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := ForceUnit.Value(SOLVER.LoadF2);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeS2);
    Points[1].Y := ForceUnit.Value(SOLVER.LoadF2);
    Points[2].X := LengthUnit.Value(SOLVER.StrokeS2);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F2');
    Points := nil;
  end;
  // Draw Load Fn
  if SOLVER.LoadFn > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := ForceUnit.Value(SOLVER.LoadFn);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeSn);
    Points[1].Y := ForceUnit.Value(SOLVER.LoadFn);
    Points[2].X := LengthUnit.Value(SOLVER.StrokeSn);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fn');
    Points := nil;
  end;
  // Draw Load Fc
  if SOLVER.LoadFc > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := ForceUnit.Value(SOLVER.LoadFc);
    Points[1].X := LengthUnit.Value(SOLVER.StrokeSc);
    Points[1].Y := ForceUnit.Value(SOLVER.LoadFc);
    Points[2].X := LengthUnit.Value(SOLVER.StrokeSc);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fc');
    Points := nil;
  end;
  // Draw label Load-F1
  if SOLVER.LoadF1 > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    Result.AddLabel(0, ForceUnit.Value(SOLVER.LoadF1),
      Trunc(32*AScreenScale), 0, taLeftJustify, taAlignBottom, 'F1');
  end;
  // Draw label Load-F2
  if SOLVER.LoadF2 > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    Result.AddLabel(0, ForceUnit.Value(SOLVER.LoadF2),
      Trunc(64*AScreenScale), 0, taLeftJustify, taAlignBottom, 'F2');
  end;
  // Draw label Load-Fn
  if SOLVER.LoadFn > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    Result.AddLabel(0, ForceUnit.Value(SOLVER.LoadFn),
      Trunc(96*AScreenScale), 0, taLeftJustify, taAlignBottom, 'Fn');
  end;
  // Draw label Load-Fc
  if SOLVER.LoadFc > (0*N) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    Result.AddLabel(0, ForceUnit.Value(SOLVER.LoadFc),
      Trunc(128*AScreenScale), 0, taLeftJustify, taAlignBottom, 'Fc');
  end;
end;

function TCompozer.CreateGoodmanChart(const AScreenScale: double): TChart;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  if MAT.ItemIndex <> -1 then
    Result.Title := Format('Goodman Chart: %s', [MAT.Items[MAT.ItemIndex]])
  else
    Result.Title := Format('Goodman Chart: %s', ['Custom material']);
  Result.XAxisLabel := 'tau U';
  Result.YAxisLabel := 'tau O';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Draw bisector line
  if (SOLVER.AdmStaticTorsionalStressTauz > (0*MPa)) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := PressureUnit.Value(SOLVER.AdmStaticTorsionalStressTauz);
    Points[1].Y := PressureUnit.Value(SOLVER.AdmStaticTorsionalStressTauz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  // Draw tauk-tolerances
  if (SOLVER.GetTauk(TOL.LoadF1Tolerance) > (0*MPa)) and
     (SOLVER.GetTauk(TOL.LoadF2Tolerance) > (0*MPa)) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := PressureUnit.Value(Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[0].Y := PressureUnit.Value(Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[1].X := PressureUnit.Value(Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[1].Y := PressureUnit.Value(Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[2].X := PressureUnit.Value(Solver.TorsionalStressTauk1 + SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[2].Y := PressureUnit.Value(Solver.TorsionalStressTauk2 + SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[3].X := PressureUnit.Value(Solver.TorsionalStressTauk1 - SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Points[3].Y := PressureUnit.Value(Solver.TorsionalStressTauk2 - SOLVER.GetTauk(TOL.LoadF1Tolerance));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk2');
    Points := nil;
  end;
  // Draw tauk1-tauk2
  if (Solver.TorsionalStressTauk1 > (0*Pa)) and
     (Solver.TorsionalStressTauk2 > (0*Pa)) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := PressureUnit.Value(Solver.TorsionalStressTauk1);
    Points[0].Y := PressureUnit.Value(Solver.TorsionalStressTauk1);
    Points[1].X := PressureUnit.Value(Solver.TorsionalStressTauk1);
    Points[1].Y := PressureUnit.Value(Solver.TorsionalStressTauk2);
    Result.AddPolyLine(Points, False, 'Tauk1-Tauk2');
    Points := nil;
  end;
  // Draw Goodmand curve
  if MAT.ItemIndex <> -1 then
  begin
    if (MAT.TorsionalStressTauOE5   > (0*Pa)) and
       (MAT.TorsionalStressTauUE5   > (0*Pa)) and
       (MAT.TorsionalStressTauYield > (0*Pa)) and
       (MAT.TorsionalStressTauUE6   > (0*Pa)) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E5');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := PressureUnit.Value(MAT.TorsionalStressTauOE5);
      Points[1].X := PressureUnit.Value(MAT.TorsionalStressTauUE5);
      Points[1].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Points[2].X := PressureUnit.Value(MAT.TorsionalStressTauUE6);
      Points[2].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E5 Cycles');
      Result.AddLabel(
        PressureUnit.Value(MAT.TorsionalStressTauUE5),
        PressureUnit.Value(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E5');
      Points := nil;
    end;

    if (MAT.TorsionalStressTauOE6   > (0*Pa)) and
       (MAT.TorsionalStressTauUE6   > (0*Pa)) and
       (MAT.TorsionalStressTauYield > (0*Pa)) and
       (MAT.TorsionalStressTauUE7   > (0*Pa)) then
     begin
      LoadChart2(Result, 'GoodmanChart', '1E6');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := PressureUnit.Value(MAT.TorsionalStressTauOE6);
      Points[1].X := PressureUnit.Value(MAT.TorsionalStressTauUE6);
      Points[1].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Points[2].X := PressureUnit.Value(MAT.TorsionalStressTauUE7);
      Points[2].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E6 Cycles');
      Result.AddLabel(
        PressureUnit.Value(MAT.TorsionalStressTauUE6),
        PressureUnit.Value(MAT.TorsionalStressTauYield),
        0, 0, taLeftJustify, taAlignBottom, '1E6');
      Points := nil;
     end;

    if (MAT.TorsionalStressTauOE7   > (0*Pa)) and
       (MAT.TorsionalStressTauUE7   > (0*Pa)) and
       (MAT.TorsionalStressTauYield > (0*Pa)) then
    begin
      LoadChart2(Result, 'GoodmanChart', '1E7');
      SetLength(Points, 3);
      Points[0].X := 0;
      Points[0].Y := PressureUnit.Value(MAT.TorsionalStressTauOE7);
      Points[1].X := PressureUnit.Value(MAT.TorsionalStressTauUE7);
      Points[1].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Points[2].X := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Points[2].Y := PressureUnit.Value(MAT.TorsionalStressTauYield);
      Result.AddPolyLine(Points, False, '1E7 Cycles');
      Result.AddLabel(
        PressureUnit.Value(MAT.TorsionalStressTauUE7),
        PressureUnit.Value(MAT.TorsionalStressTauYield),
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
  SOLVER.GetBucklingCurve(Points);
  if (Length(Points) > 0) then
  begin
    LoadChart2(Result, 'BucklingChart', 'BucklingCurve');
    Result.XMinF   := 0.0;
    Result.YMinF   := 0.0;
    Result.YMaxF   := 1.0;
    Result.YDeltaF := 0.1;
    Result.YCount  := 10;
    Result.AddPolyLine(Points, False, 'Buckling-Curve');
    Points := nil;
  end;

  if (SOLVER.Dm > (0*m)) and (SOLVER.LengthL0 > (0*m)) then
  begin
    LoadChart2(Result, 'BucklingChart', 'Sc');
    X := SOLVER.SeatingCoefficent*SOLVER.LengthL0/SOLVER.Dm;
    if SOLVER.StrokeSc > SOLVER.DeflectionSk then
      Y := SOLVER.StrokeSc / SOLVER.LengthL0
    else
      Y := SOLVER.DeflectionSk / SOLVER.LengthL0;

    SetLength(Points, 2);
    Points[0].x := X;
    Points[0].y := 0;
    Points[1].x := X;
    Points[1].y := Y;
    Result.AddPolyLine(Points, False, 'Sc');
    Result.AddDotLabel(X, SOLVER.StrokeSc/SOLVER.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'Sc');
    Points := nil;
    LoadChart2(Result, 'BucklingChart', 'Sx');
    Result.AddDotLabel(X, SOLVER.StrokeS1/SOLVER.LengthL0, 5, 10, 0, taLeftJustify, taVerticalCenter, 'S1');
    Result.AddDotLabel(X, SOLVER.StrokeS2/SOLVER.LengthL0, 5, 10, 0, taLeftJustify, taVerticalCenter, 'S2');
  end;
end;

function TCompozer.CreateLoadF1Chart(const AScreenScale: double): TChart;
const
  DT = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Load F1-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F1[' + ForceUnit.Symbol + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SOLVER.GetF1(MAT.Tempetature - DT) > (0*N)) and
     (SOLVER.GetF1(MAT.Tempetature + DT) > (0*N)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x:= MAT.Tempetature - DT;
    Points[0].y:= ForceUnit.Value(SOLVER.GetF1(Points[0].x));
    Points[1].x:= MAT.Tempetature + DT;
    Points[1].y:= ForceUnit.Value(SOLVER.GetF1(Points[1].x));
    Result.AddPolyLine(Points, True, 'F1(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature,
      ForceUnit.Value(SOLVER.GetF1(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateLoadF2Chart(const AScreenScale: double): TChart;
const
  DT = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Load F2-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F2[' + ForceUnit.Symbol + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SOLVER.GetF2(MAT.Tempetature - DT) > (0*N)) and
     (SOLVER.GetF2(MAT.Tempetature + DT) > (0*N)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x:= MAT.Tempetature - DT;
    Points[0].y:= ForceUnit.Value(SOLVER.GetF2(Points[0].x));
    Points[1].x:= MAT.Tempetature + DT;
    Points[1].y:= ForceUnit.Value(SOLVER.GetF2(Points[1].x));
    Result.AddPolyLine(Points, True, 'F2(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature,
      ForceUnit.Value(SOLVER.GetF2(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateShearModulusChart(const AScreenScale: double): TChart;
const
  DT = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Shear Modulus G-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'G[' + PressureUnit.Symbol + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (MAT.GetG(MAT.Tempetature - DT) > (0*Pa)) and
     (MAT.GetG(MAT.Tempetature + DT) > (0*Pa)) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x:= MAT.Tempetature - DT;
    Points[0].y:= PressureUnit.Value(MAT.GetG(Points[0].x));
    Points[1].x:= MAT.Tempetature + DT;
    Points[1].y:= PressureUnit.Value(MAT.GetG(Points[1].x));
    Result.AddPolyLine(Points, True, 'G(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature,
      PressureUnit.Value(MAT.GetG(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateYoungModulusChart(const AScreenScale: double): TChart;
const
  DT = 50;
  Section1 = 'Custom';
  Section2 = 'LinearChart';
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;
  Result.Title := 'Young Modulus G-Temperature Chart';
  Result.XAxisLabel := 'T [C°]';
  Result.YAxisLabel := 'E [' + PressureUnit.Symbol + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, Section1);

  if (MAT.GetE(MAT.Tempetature - DT) > (0*Pa)) and
     (MAT.GetE(MAT.Tempetature + DT) > (0*Pa)) then
  begin
    LoadChart2(Result, Section2, 'Line');
    SetLength(Points, 2);
    Points[0].x:= MAT.Tempetature - DT;
    Points[0].y:= PressureUnit.Value(MAT.GetE(Points[0].x));
    Points[1].x:= MAT.Tempetature + DT;
    Points[1].y:= PressureUnit.Value(MAT.GetE(Points[1].x));
    Result.AddPolyLine(Points, True, 'E(T°)');
    Points := nil;

    Result.AddDotLabel(MAT.Tempetature,
      PressureUnit.Value(MAT.GetE(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
end;

function TCompozer.CreateSectionSpringDrawing(const AScreenScale: double): TSectionSpringDrawing;
begin
  Result := TSectionSpringDrawing.Create;
  LoadSpring(Result, 'SpringDrawing', 'Spring');
  Result.d := FLengthUnit.Value(SOLVER.WireDiameter);
  Result.Dm := FLengthUnit.Value(SOLVER.Dm);
  Result.Lc := FLengthUnit.Value(SOLVER.LengthLc);
  Result.n := SOLVER.ActiveColis;
  Result.nt1 := (SOLVER.TotalCoils - SOLVER.ActiveColis) / 2;
  Result.nt2 := (SOLVER.TotalCoils - SOLVER.ActiveColis) / 2;
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
  Result[1, 4] := TryFormatFloat('%s ' + LengthUnit.Symbol, ' --- ', LengthUnit.Value(TOL.CoilDiameterTolerance));
  Result[2, 4] := TryFormatFloat('%s ' + LengthUnit.Symbol, ' --- ', LengthUnit.Value(TOL.LengthL0Tolerance));
  Result[3, 4] := TryFormatFloat('%s ' + ForceUnit .Symbol, ' --- ', ForceUnit .Value(TOL.LoadF1Tolerance));
  Result[4, 4] := TryFormatFloat('%s ' + ForceUnit .Symbol, ' --- ', ForceUnit .Value(TOL.LoadF2Tolerance));
  Result[5, 4] := TryFormatFloat('%s ' + LengthUnit.Symbol, ' --- ', LengthUnit.Value(TOL.EccentricityE1));
  Result[6, 4] := TryFormatFloat('%s ' + LengthUnit.Symbol, ' --- ', LengthUnit.Value(TOL.EccentricityE2));
end;

function TCompozer.CreateQuick1Table(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 7;
  Result.RowCount := 6;
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');

  Result[0, 0] := 'L [' + LengthUnit.Symbol + ']';
  Result[1, 0] := TryFormatFloat('L0: %s', 'L0: ---', LengthUnit.Value(SOLVER.LengthL0));
  Result[2, 0] := TryFormatFloat('L1: %s', 'L1: ---', LengthUnit.Value(SOLVER.LengthL1));
  Result[3, 0] := TryFormatFloat('L2: %s', 'L2: ---', LengthUnit.Value(SOLVER.LengthL2));
  Result[4, 0] := TryFormatFloat('Ln: %s', 'Ln: ---', LengthUnit.Value(SOLVER.LengthLn));
  Result[5, 0] := TryFormatFloat('Lc: %s', 'Lc: ---', LengthUnit.Value(SOLVER.LengthLc));

  Result[0, 1] := 'F [' + ForceUnit.Symbol + ']';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('F1: %s', 'F1: ---', ForceUnit.Value(SOLVER.LoadF1));
  Result[3, 1] := TryFormatFloat('F2: %s', 'F2: ---', ForceUnit.Value(SOLVER.LoadF2));
  Result[4, 1] := TryFormatFloat('Fn: %s', 'Fn: ---', ForceUnit.Value(SOLVER.LoadFn));
  Result[5, 1] := TryFormatFloat('Fc: %s', 'Fc: ---', ForceUnit.Value(SOLVER.LoadFc));

  Result[0, 2] := 'tau [' + PressureUnit.Symbol + ']';
  Result[1, 2] := '';
  Result[2, 2] := TryFormatFloat('tauk1: %s', 'tauk1: ---', PressureUnit.Value(SOLVER.TorsionalStressTauk1));
  Result[3, 2] := TryFormatFloat('tauk2: %s', 'tauk2: ---', PressureUnit.Value(SOLVER.TorsionalStressTauk2));
  Result[4, 2] := TryFormatFloat('tau n: %s', 'tau n: ---', PressureUnit.Value(SOLVER.TorsionalStressTaun));
  Result[5, 2] := TryFormatFloat('tau c: %s', 'tau c: ---', PressureUnit.Value(SOLVER.TorsionalStressTauc));

  Result[0, 3] := 's [' + LengthUnit.Symbol + ']';
  Result[1, 3] := '';
  Result[2, 3] := TryFormatFloat('s1: %s', 's1: ---', LengthUnit.Value(SOLVER.StrokeS1));
  Result[3, 3] := TryFormatFloat('s2: %s', 's2: ---', LengthUnit.Value(SOLVER.StrokeS2));
  Result[4, 3] := TryFormatFloat('sn: %s', 'sn: ---', LengthUnit.Value(SOLVER.StrokeSn));
  Result[5, 3] := TryFormatFloat('sc: %s', 'sc: ---', LengthUnit.Value(SOLVER.StrokeSc));

  Result[0, 4] := 'tau/tauz';
  Result[1, 4] := '';
  Result[2, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1.Value, SOLVER.AdmStaticTorsionalStressTauz.Value);
  Result[3, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2.Value, SOLVER.AdmStaticTorsionalStressTauz.Value);
  Result[4, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun.Value, SOLVER.AdmStaticTorsionalStressTauz.Value);
  Result[5, 4] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc.Value, SOLVER.AdmStaticTorsionalStressTauz.Value);

  Result[0, 5] := 'tau/Rm';
  Result[1, 5] := '';
  Result[2, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau1.Value, SOLVER.TensileStrengthRm.Value);
  Result[3, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTau2.Value, SOLVER.TensileStrengthRm.Value);
  Result[4, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTaun.Value, SOLVER.TensileStrengthRm.Value);
  Result[5, 5] := TryFormatFloatDiv('%s', '---', SOLVER.TorsionalStressTauc.Value, SOLVER.TensileStrengthRm.Value);

  Result[0, 6] := 'De [' + LengthUnit.Symbol + ']';
  Result[1, 6] := TryFormatFloat      ('%s', '---', LengthUnit.Value(SOLVER.De));
  Result[2, 6] := TryFormatFloatSumDiv('%s', '---', LengthUnit.Value(SOLVER.De), FAreaUnit.Value(SOLVER.DeltaDe*SOLVER.StrokeS1), LengthUnit.Value(SOLVER.StrokeSc));
  Result[3, 6] := TryFormatFloatSumDiv('%s', '---', LengthUnit.Value(SOLVER.De), FAreaUnit.Value(SOLVER.DeltaDe*SOLVER.StrokeS2), LengthUnit.Value(SOLVER.StrokeSc));
  Result[4, 6] := TryFormatFloatSumDiv('%s', '---', LengthUnit.Value(SOLVER.De), FAreaUnit.Value(SOLVER.DeltaDe*SOLVER.StrokeSn), LengthUnit.Value(SOLVER.StrokeSc));
  Result[5, 6] := TryFormatFloatSumDiv('%s', '---', LengthUnit.Value(SOLVER.De), FAreaUnit.Value(SOLVER.DeltaDe*SOLVER.StrokeSc), LengthUnit.Value(SOLVER.StrokeSc));
end;

function TCompozer.CreateQuick1List(const AScreenScale: double): TReportTable;
begin
  Result             := TReportTable.Create;
  Result.ColumnCount := 3;
  Result.RowCount    := 20;
  Result.Zoom        := AScreenScale;

  LoadTable(Result, 'CommonTable', 'Table');
  Result.ColumnAlignments[0] := taRightJustify;
  Result.ColumnAlignments[1] := taCenter;
  Result.ColumnAlignments[2] := taLeftJustify;

  Result.Items[ 0, 0] := 'd';
  Result.Items[ 0, 1] := '=';
  Result.Items[ 0, 2] := TryFormatFloat('%s', '---', LengthUnit.Value(SOLVER.WireDiameter)) +
    TryFormatFloat(' ± %s ' + LengthUnit.Symbol, '', LengthUnit.Value(SOLVER.WireDiameterMax - SOLVER.WireDiameter));

  Result.Items[ 1, 0] := 'Di';
  Result.Items[ 1, 1] := '=';
  Result.Items[ 1, 2] := TryFormatFloat('%s ' + LengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.Di));

  Result.Items[ 2, 0] := 'Dm';
  Result.Items[ 2, 1] := '=';
  Result.Items[ 2, 2] := TryFormatFloat('%s ' + LengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.Dm));

  Result.Items[ 3, 0] := 'De';
  Result.Items[ 3, 1] := '=';
  Result.Items[ 3, 2] := TryFormatFloat('%s', '---', LengthUnit.Value(SOLVER.De)) +
    TryFormatFloat(' ± %s ' + LengthUnit.Symbol, '', LengthUnit.Value(TOL.CoilDiameterTolerance));

  Result.Items[ 4, 0] := 'n';
  Result.Items[ 4, 1] := '=';
  Result.Items[ 4, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);

  Result.Items[ 5, 0] := 'nt';
  Result.Items[ 5, 1] := '=';
  Result.Items[ 5, 2] := TryFormatFloat('%s coils', '---', SOLVER.TotalCoils);

  Result.Items[ 6, 0] := 'R';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := TryFormatFloat('%s ' + FStiffnessUnit.Symbol,  '---', FStiffnessUnit.Value(SOLVER.SpringRateR));

  Result.Items[ 7, 0] := 'Dec';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.De + SOLVER.DeltaDe));

  Result.Items[ 8, 0] := 'Di.min';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.DiMin));

  Result.Items[ 9, 0] := 'De.max';
  Result.Items[ 9, 1] := '=';
  Result.Items[ 9, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', LengthUnit.Value(SOLVER.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s g', '---', FMassUnit.Value(SOLVER.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s Nmm', '---', FWorkUnit.Value(SOLVER.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s Nmm', '---', FWorkUnit.Value(SOLVER.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s Hz', '---' , FFrequencyUnit.Value(SOLVER.NaturalFrequency));

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s mm', '---', FLengthUnit.Value(SOLVER.Pitch));

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

function TCompozer.CreateMessageList(const AScreenScale: double): TReportTable;
var
  i, j: longint;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 1;
  Result.RowCount := 1 + ErrorMessage.Count + WarningMessage.Count;
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'MessageList', 'Table');

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

  for i := 0 to Result.RowCount -1 do
    for j := 0 to Result.ColumnCount -1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  Result.Items[ 0, 0] := 'd';
  Result.Items[ 0, 1] := '=';
  Result.Items[ 0, 2] := TryFormatFloat('%s', '---', FLengthUnit.Value(SOLVER.WireDiameter)) +
    TryFormatFloat(' ± %s ' + FLengthUnit.Symbol, '', FLengthUnit.Value(SOLVER.WireDiameterMax - SOLVER.WireDiameter));

  Result.Items[ 1, 0] := 'Di';
  Result.Items[ 1, 1] := '=';
  Result.Items[ 1, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.Di));

  Result.Items[ 2, 0] := 'Dm';
  Result.Items[ 2, 1] := '=';
  Result.Items[ 2, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.Dm));

  Result.Items[ 3, 0] := 'De';
  Result.Items[ 3, 1] := '=';
  Result.Items[ 3, 2] := TryFormatFloat('%s', '---', FLengthUnit.Value(SOLVER.De)) +
    TryFormatFloat(' ± %s ' + FLengthUnit.Symbol, '', FLengthUnit.Value(TOL.CoilDiameterTolerance));

  Result.Items[ 4, 0] := 'n';
  Result.Items[ 4, 1] := '=';
  Result.Items[ 4, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);

  Result.Items[ 5, 0] := 'nt';
  Result.Items[ 5, 1] := '=';
  Result.Items[ 5, 2] := TryFormatFloat('%s coils', '---', SOLVER.TotalCoils);

  Result.Items[ 6, 0] := 'nt';
  Result.Items[ 6, 1] := '=';
  Result.Items[ 6, 2] := TryFormatFloat('%s ',  '---', FStiffnessUnit.Value(SOLVER.SpringRateR));

  Result.Items[ 7, 0] := 'Dec';
  Result.Items[ 7, 1] := '=';
  Result.Items[ 7, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.De + SOLVER.DeltaDe));

  Result.Items[ 8, 0] := 'Di.min';
  Result.Items[ 8, 1] := '=';
  Result.Items[ 8, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.DiMin));

  Result.Items[ 9, 0] := 'De.max';
  Result.Items[ 9, 1] := '=';
  Result.Items[ 9, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s ' + FMassUnit.Symbol, '---', FMassUnit.Value(SOLVER.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s ' + FWorkUnit.Symbol, '---', FWorkUnit.Value(SOLVER.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s ' + FWorkUnit.Symbol, '---', FWorkUnit.Value(SOLVER.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s ' + FFrequencyUnit.Symbol, '---' , FFrequencyUnit.Value(SOLVER.NaturalFrequency));

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', FLengthUnit.Value(SOLVER.Pitch));

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
  Result.Items[22, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.TorsionalStressTauk1));

  Result.Items[23, 0] := 'tauk2';
  Result.Items[23, 1] := '=';
  Result.Items[23, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.TorsionalStressTauk2));

  Result.Items[24, 0] := 'taukh';
  Result.Items[24, 1] := '=';
  Result.Items[24, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.TorsionalStressTaukh));

  Result.Items[26, 0] := 'E';
  Result.Items[26, 1] := '=';
  Result.Items[26, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.YoungModulus));

  Result.Items[27, 0] := 'G';
  Result.Items[27, 1] := '=';
  Result.Items[27, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.ShearModulus));

  Result.Items[28, 0] := 'rho';
  Result.Items[28, 1] := '=';
  Result.Items[28, 2] := TryFormatFloat('%s ' + FDensityUnit.Symbol, '---', FDensityUnit.Value(SOLVER.MaterialDensity));

  Result.Items[29, 0] := 'Rm';
  Result.Items[29, 1] := '=';
  Result.Items[29, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.TensileStrengthRm));

  Result.Items[30, 0] := 'tauz';
  Result.Items[30, 1] := '=';
  Result.Items[30, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.AdmStaticTorsionalStressTauz));

  Result.Items[31, 0] := 'ns';
  Result.Items[31, 1] := '=';
  Result.Items[31, 2] := TryFormatFloat('%s', '---', SOLVER.StaticSafetyFactor);

  if SOLVER.DynamicLoad then
  begin
    Result.Items[33, 0] := 'tauoz';
    Result.Items[33, 1] := '=';
    Result.Items[33, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.AdmDynamicTorsionalStressTauoz));

    Result.Items[34, 0] := 'tauhz';
    Result.Items[34, 1] := '=';
    Result.Items[34, 2] := TryFormatFloat('%s ' + FPressureUnit.Symbol, '---', FPressureUnit.Value(SOLVER.AdmDynamicTorsionalStressRangeTauhz));

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

  // 0-Force & Displacement Chart
  ForceDiagram := CreateForceDisplacementChart(aScreenScale);
  Bit[0].SetSize(Trunc(aScreen.Width * 0.35), aScreen.Height div 2);
  ForceDiagram.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
  Bit[0].Draw(aScreen.Canvas, 0, 0, True);
  ForceDiagram.Destroy;

  // 1-Goodman Chart
  GoodmanDiagram := CreateGoodmanChart(aScreenScale);
  Bit[1].SetSize(Bit[0].Width, Bit[0].Height);
  GoodmanDiagram.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
  Bit[1].Draw(aScreen.Canvas, 0, Bit[0].Height, True);
  GoodmanDiagram.Destroy;

  // 2-Quick-1 List
  Quick1List := CreateQuick1AList(aScreenScale);
  while Quick1List.Height < (aScreen.Height) do
  begin
    Quick1List.RowSpacer := Quick1List.RowSpacer + 1;
  end;
  Quick1List.RowSpacer := Quick1List.RowSpacer - 1;
  Bit[2].SetSize(Quick1List.Width, Quick1List.Height);
  Quick1List.Draw(Bit[2].Canvas);
  Bit[2].Draw(aScreen.Canvas, Bit[0].Width, 0, True);
  Quick1List.Destroy;

  // 3-Quick-1 Table
  Quick1Table := CreateQuick1Table(aScreenScale);
  while Quick1Table.Width < (aScreen.Width - Bit[0].Width - Bit[2].Width) do
  begin
    Quick1Table.ColumnSpacer := Quick1Table.ColumnSpacer + 1;
  end;
  Quick1Table.ColumnSpacer := Quick1Table.ColumnSpacer - 1;
  Bit[3].SetSize(Quick1Table.Width, Quick1Table.Height);
  Quick1Table.Draw(Bit[3].Canvas);
  Bit[3].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width, 0, True);
  Quick1Table.Destroy;

  // 4-Quality Table
  QualityTable := CreateQualityTable(aScreenScale);
  Bit[4].SetSize(QualityTable.Width, QualityTable.Height);
  QualityTable.Draw(Bit[4].Canvas);
  Bit[4].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width, Bit[3].Height, True);
  QualityTable.Destroy;

  // 5-Message List
  MessageList := CreateMessageList(aScreenScale);
  Bit[5].SetSize(MessageList.Width, MessageList.Height);
  MessageList.Draw(Bit[5].Canvas);
  Bit[5].Draw(aScreen.Canvas, Bit[0].Width + Bit[2].Width + Bit[4].Width, Bit[3].Height, True);
  MessageList.Destroy;

  // 6-Spring Drawings
  SpringDrawing := CreateSectionSpringDrawing(AScreenScale);
  Bit[6].SetSize((aScreen.Width  - Bit[1].Width  - Bit[2].Width) div 3,
                  aScreen.Height - Bit[3].Height - Bit[4].Height);

  Bit[7].SetSize((aScreen.Width  - Bit[1].Width  - Bit[2].Width) div 3,
                  aScreen.Height - Bit[3].Height - Bit[4].Height);

  Bit[8].SetSize((aScreen.Width  - Bit[1].Width  - Bit[2].Width) div 3,
                  aScreen.Height - Bit[3].Height - Bit[4].Height);

  SpringDrawing.AutoFit := True;
  SpringDrawing.Lx      := mm.Value(SOLVER.LengthL0);
  SpringDrawing.Caption := TryFormatFloat('L0 = %s', 'L0 = ---',SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx      := mm.Value(SOLVER.LengthL1);
  SpringDrawing.Caption := TryFormatFloat('L1 = %s', 'L1 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[7].Canvas, Bit[7].Width, Bit[7].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx      := mm.Value(SOLVER.LengthL2);
  SpringDrawing.Caption := TryFormatFloat('L2 = %s', 'L2 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[8].Canvas, Bit[8].Width, Bit[8].Height);

  Bit[6].Draw(aScreen.Canvas, Bit[1].Width + Bit[2].Width + Bit[6].Width*0, Bit[3].Height + Bit[4].Height, True);
  Bit[7].Draw(aScreen.Canvas, Bit[1].Width + Bit[2].Width + Bit[6].Width*1, Bit[3].Height + Bit[4].Height, True);
  Bit[8].Draw(aScreen.Canvas, Bit[1].Width + Bit[2].Width + Bit[6].Width*2, Bit[3].Height + Bit[4].Height, True);
  SpringDrawing.Destroy;

  for i := Low(Bit) to High(Bit) do
    Bit[i].Destroy;
  Bit := nil
end;

procedure TCompozer.DrawQuickX(var aScreen: TBGRABitmap; const aScreenScale: double; X: longint);
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
  QuickXList.RowCount    := 26;
  QuickXList.Zoom        := aScreenScale;
  LoadTable(QuickXList, 'QuickXList', 'Table');
  QuickXList.ColumnAlignments[0] := taLeftJustify;
  QuickXList.ColumnAlignments[1] := taCenter;
  QuickXList.ColumnAlignments[2] := taLeftJustify;
  QuickXList.ColumnAlignments[3] := taCenter;
  QuickXList.ColumnAlignments[4] := taLeftJustify;
  QuickXList.ColumnAlignments[5] := taCenter;
  QuickXList.ColumnAlignments[6] := taLeftJustify;

  for i := 0 to QuickXList.RowCount -1 do
    for j := 0 to QuickXList.ColumnCount -1 do
      QuickXList.Items[i, j] := '  ';

  //012
  Row := 0;
  QuickXList.Items[Row, 0] := 'd';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' + FLengthUnit.Symbol, '---', mm.Value(SOLVER.WireDiameter)) +
    TryFormatFloat(' ± %s mm', '', mm.Value(SOLVER.WireDiameterMax - SOLVER.WireDiameter));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.Di));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Dm';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.Dm)) +
    TryFormatFloat(' ± %s mm', '', mm.Value(TOL.CoilDiameterTolerance));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'De';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.De));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'n';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s coils', '---', SOLVER.ActiveColis);
  Inc(Row);
  QuickXList.Items[Row, 0] := 'nt';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s colis', '---', SOLVER.TotalCoils);
  Inc(Row);
  QuickXList.Items[Row, 0] := 'nu';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s', '---', SOLVER.SeatingCoefficent);
  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.LengthL0)) +
    TryFormatFloat(' ± %s mm', '', mm.Value(TOL.LengthL0Tolerance));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'L1';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.LengthL1));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'L2';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.LengthL2));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Ln';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.LengthLn));
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Lc';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s mm', '---', mm.Value(SOLVER.LengthLc));
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
  if SOLVER.ClosedEnds then
  begin
    if SOLVER.GroundEnds then
      QuickXList.Items[Row, 2] := 'Closed and ground'
    else
      QuickXList.Items[Row, 2] := 'Closed';
  end else
    QuickXList.Items[Row, 2] := 'Open';
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Coiling type';
  QuickXList.Items[Row, 1] := '=';
  if SOLVER.ColdCoiled then
    QuickXList.Items[Row, 2] := 'Cold coiled'
  else
    QuickXList.Items[Row, 2] := 'Hot coiled';
  Inc(Row);
  QuickXList.Items[Row, 0] := 'Load type';
  QuickXList.Items[Row, 1] := '=';
  if SOLVER.DynamicLoad then
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
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.TensileStrengthRm));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'G';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.ShearModulus));
  Inc(Row);
  QuickXList.Items[Row, 4] := Format('G(%s°)', [TryFloatToText(MAT.Tempetature)]);
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(MAT.GetG(MAT.Tempetature)));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'rho';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s kg/dm3', '---', kg_dm3.Value(SOLVER.MaterialDensity));
  Inc(Row, 3);
  QuickXList.Items[Row, 4] := 'F1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s N',  '---', N.Value(SOLVER.LoadF1)) +
    TryFormatFloat(' ± %s N', '', N.Value(TOL.LoadF1Tolerance));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'F2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s N', '---', N.Value(SOLVER.LoadF2)) +
    TryFormatFloat(' ± %s N', '', N.Value(TOL.LoadF2Tolerance));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fn';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s N', '---', N.Value(SOLVER.LoadFn));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fc';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s N', '---', N.Value(SOLVER.LoadFc));
  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauk1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.TorsionalStressTauk1));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauk2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.TorsionalStressTauk2));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'taukh';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.TorsionalStressTaukh));
  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauhz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.AdmDynamicTorsionalStressRangeTauhz));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauoz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.AdmDynamicTorsionalStressTauoz));
  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s MPa', '---', MPa.Value(SOLVER.AdmStaticTorsionalStressTauz));
  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'Static safety factor';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SOLVER.StaticSafetyFactor);
  Inc(Row);
  if SOLVER.DynamicLoad then
  begin
    QuickXList.Items[Row, 4] := 'Dynamic safety factor';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SOLVER.DynamicSafetyFactor);
    if SOLVER.NumOfCycles > 0 then
    begin
      Inc(Row);
      QuickXList.Items[Row, 4] := 'N';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := TryFormatText('%s cycles', '---', TryFloatToText(SOLVER.NumOfCycles, 2, 0));
      Inc(Row);
      QuickXList.Items[Row, 4] := 'Nh';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] := TryFormatFloatDiv('%s hours', '---', SOLVER.NumOfCycles, 3600*ApplicationForm.CycleFrequency.Value);
    end else
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
  Bit[1].SetSize(Bit[0].Width, aScreen.Width - Bit[0].Height);
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

