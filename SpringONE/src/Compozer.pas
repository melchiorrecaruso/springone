unit Compozer;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, Classes, ADim,
  GraphBase, Graphics, IniFiles, LibLink, Math, SysUtils;

type
  TCompozer = class
  private
    FSetting: TIniFile;
    function  LoadFontStyle(const ASection, AIdent: string): TFontStyles;
    function  LoadPenStyle(const ASection, AIdent: string): TPenStyle;
    procedure LoadChart1(Chart: TChart; const ASection: string);
    procedure LoadChart2(Chart: TChart; const ASection, AIdent: string);
    procedure LoadTable(Table: TReportTable; const ASection, AIdent: string);
    procedure LoadSpring(Spring: TSectionSpringDrawing; const ASection, AIdent: string);

    procedure DrawQuickX(var aScreen: TBGRABitmap; const aScreenScale: double; X: longint);
  public
    constructor Create(ASetting: TIniFile);
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
  EN10270, EN15800, UtilsBase;

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

  {$IFDEF MODULE1}
  Result.Title := 'Force & Displacement Chart';
  Result.XAxisLabel := 's[' + GetSymbol(SpringSolver.LengthLc) + ']';
  Result.YAxisLabel := 'F[' + GetSymbol(SpringSolver.LoadFc) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');
  // Drawing bisector line
  if SpringSolver.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetValue(SpringSolver.StrokeSc);
    Points[1].Y := GetValue(SpringSolver.LoadFc);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing tolerance lines
  if (SpringTolerance.Load1.Value > 0) and (SpringTolerance.Load2.Value > 0) then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'ToleranceLine');
    SetLength(Points, 2);
    Points[0].X := GetValue(SpringSolver.StrokeS1);
    Points[0].Y := GetValue(SpringTolerance.Load1 + SpringTolerance.ToleranceOnLoad1);
    Points[1].X := GetValue(SpringSolver.StrokeS2);
    Points[1].Y := GetValue(SpringTolerance.Load2 + SpringTolerance.ToleranceOnLoad2);
    Result.AddPolyLine(Points, True, 'Tolerance +');
    Points[0].X := GetValue(SpringSolver.StrokeS1);
    Points[0].Y := GetValue(SpringTolerance.Load1 - SpringTolerance.ToleranceOnLoad1);
    Points[1].X := GetValue(SpringSolver.StrokeS2);
    Points[1].Y := GetValue(SpringTolerance.Load2 - SpringTolerance.ToleranceOnLoad2);
    Result.AddPolyLine(Points, True, 'Tolerance -');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}

  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load-F1
  if SpringSolver.LoadF1.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SpringSolver.LoadF1);
    Points[1].X := GetValue(SpringSolver.StrokeS1);
    Points[1].Y := GetValue(SpringSolver.LoadF1);
    Points[2].X := GetValue(SpringSolver.StrokeS1);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F1');
    Points := nil;
  end;
  {$ENDIF}


  {$IFDEF MODULE1}
  // Drawing Load F2
  if SpringSolver.LoadF2.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SpringSolver.LoadF2);
    Points[1].X := GetValue(SpringSolver.StrokeS2);
    Points[1].Y := GetValue(SpringSolver.LoadF2);
    Points[2].X := GetValue(SpringSolver.StrokeS2);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'F2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load Fn
  if SpringSolver.LoadFn.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SpringSolver.LoadFn);
    Points[1].X := GetValue(SpringSolver.StrokeSn);
    Points[1].Y := GetValue(SpringSolver.LoadFn);
    Points[2].X := GetValue(SpringSolver.StrokeSn);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fn');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Load Fc
  if SpringSolver.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := GetValue(SpringSolver.LoadFc);
    Points[1].X := GetValue(SpringSolver.StrokeSc);
    Points[1].Y := GetValue(SpringSolver.LoadFc);
    Points[2].X := GetValue(SpringSolver.StrokeSc);
    Points[2].Y := 0;
    Result.AddPolyLine(Points, False, 'Fc');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing label Load-F1
  if SpringSolver.LoadF1.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F1');
    Result.AddLabel(0, GetValue(SpringSolver.LoadF1),
      32, 0, taLeftJustify, taAlignBottom, 'F1');
  end;
  // Drawing label Load-F2
  if SpringSolver.LoadF2.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-F2');
    Result.AddLabel(0, GetValue(SpringSolver.LoadF2),
      64, 0, taLeftJustify, taAlignBottom, 'F2');
  end;
  // Drawing label Load-Fn
  if SpringSolver.LoadFn.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fn');
    Result.AddLabel(0, GetValue(SpringSolver.LoadFn),
      96, 0, taLeftJustify, taAlignBottom, 'Fn');
  end;
  // Drawing label Load-Fc
  if SpringSolver.LoadFc.Value > 0 then
  begin
    LoadChart2(Result, 'ForceDisplacementChart', 'Load-Fc');
    Result.AddLabel(0, GetValue(SpringSolver.LoadFc),
      128, 0, taLeftJustify, taAlignBottom, 'Fc');
  end;
  {$ENDIF}

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

  {$IFDEF MODULE1}
  // Drawing bisector line
  if SpringSolver.AdmStaticTorsionalStressTauz.Value > 0 then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetValue(SpringSolver.AdmStaticTorsionalStressTauz);
    Points[1].Y := GetValue(SpringSolver.AdmStaticTorsionalStressTauz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing bisector line
  if SpringSolver.AdmStaticBendingStressSigmaz.Value > 0 then
  begin
    LoadChart2(Result, 'GoodmanChart', 'BisectorLine');
    SetLength(Points, 2);
    Points[0].X := 0;
    Points[0].Y := 0;
    Points[1].X := GetValue(SpringSolver.AdmStaticBendingStressSigmaz);
    Points[1].Y := GetValue(SpringSolver.AdmStaticBendingStressSigmaz);
    Result.AddPolyLine(Points, True, 'Bisector');
    Points := nil;
  end;
  {$ENDIF}


  {$IFDEF MODULE1}
  // Drawing Tauk-Tolerances
  if (SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1).Value > 0) and
     (SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad2).Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := GetValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[0].Y := GetValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[1].X := GetValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[1].Y := GetValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[2].X := GetValue(SpringSolver.TorsionalStressTauk1 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[2].Y := GetValue(SpringSolver.TorsionalStressTauk2 + SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[3].X := GetValue(SpringSolver.TorsionalStressTauk1 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Points[3].Y := GetValue(SpringSolver.TorsionalStressTauk2 - SpringSolver.GetTauk(SpringTolerance.ToleranceOnLoad1));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'tauk2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Sigmaq-Tolerances
  if (SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1).Value > 0) and
     (SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque2).Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'TaukTol');
    SetLength(Points, 4);
    Points[0].X := GetValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[0].Y := GetValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[1].X := GetValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[1].Y := GetValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[2].X := GetValue(SpringSolver.BendingStressSigmaq1 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[2].Y := GetValue(SpringSolver.BendingStressSigmaq2 + SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[3].X := GetValue(SpringSolver.BendingStressSigmaq1 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Points[3].Y := GetValue(SpringSolver.BendingStressSigmaq2 - SpringSolver.GetSigmaq(SpringTolerance.ToleranceOnTorque1));
    Result.AddPolygon(Points, 'TaukTol');
    Result.AddLabel(Points[1].X, Points[1].Y, 6, 3, taLeftJustify, taAlignTop, 'sigmaq1');
    Result.AddLabel(Points[2].X, Points[2].Y, 6, 3, taLeftJustify, taAlignTop, 'sigmaq2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE1}
  // Drawing Tauk1-Tauk2
  if (SpringSolver.TorsionalStressTauk1.Value > 0) and
     (SpringSolver.TorsionalStressTauk2.Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := GetValue(SpringSolver.TorsionalStressTauk1);
    Points[0].Y := GetValue(SpringSolver.TorsionalStressTauk1);
    Points[1].X := GetValue(SpringSolver.TorsionalStressTauk1);
    Points[1].Y := GetValue(SpringSolver.TorsionalStressTauk2);
    Result.AddPolyLine(Points, False, 'Tauk1-Tauk2');
    Points := nil;
  end;
  {$ENDIF}

  {$IFDEF MODULE3}
  // Drawing Sigmaq1-Sigmaq2
  if (SpringSolver.BendingStressSigmaq1.Value > 0) and
     (SpringSolver.BendingStressSigmaq2.Value > 0) then
  begin
    LoadChart2(Result, 'GoodmanChart', 'Tauk');
    SetLength(Points, 2);
    Points[0].X := GetValue(SpringSolver.BendingStressSigmaq1);
    Points[0].Y := GetValue(SpringSolver.BendingStressSigmaq1);
    Points[1].X := GetValue(SpringSolver.BendingStressSigmaq1);
    Points[1].Y := GetValue(SpringSolver.BendingStressSigmaq2);
    Result.AddPolyLine(Points, False, 'Sigmaq1-Sigmaq2');
    Points := nil;
  end;
  {$ENDIF}

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

  {$IFDEF MODULE1}
  // Draw buckling curve
  SpringSolver.GetBucklingCurve(Points);
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

  if (SpringSolver.Dm.Value > 0) and (SpringSolver.LengthL0.Value > 0) then
  begin
    LoadChart2(Result, 'BucklingChart', 'Sc');
    X := SpringSolver.SeatingCoefficent * SpringSolver.LengthL0 / SpringSolver.Dm;

    if SpringSolver.StrokeSc > SpringSolver.DeflectionSk then
      Y := SpringSolver.StrokeSc / SpringSolver.LengthL0
    else
      Y := SpringSolver.DeflectionSk / SpringSolver.LengthL0;

    SetLength(Points, 2);
    Points[0].x := X;
    Points[0].y := 0;
    Points[1].x := X;
    Points[1].y := Y;
    Result.AddPolyLine(Points, False, 'Sc');
    Result.AddDotLabel(X, SpringSolver.StrokeSc / SpringSolver.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'Sc');
    Points := nil;
    LoadChart2(Result, 'BucklingChart', 'Sx');
    Result.AddDotLabel(X, SpringSolver.StrokeS1 / SpringSolver.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S1');
    Result.AddDotLabel(X, SpringSolver.StrokeS2 / SpringSolver.LengthL0,
      5, 10, 0, taLeftJustify, taVerticalCenter, 'S2');
  end;
  {$ENDIF}
end;

function TCompozer.CreateLoadF1Chart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;

  {$IFDEF MODULE1}
  Result.Title := 'Load F1-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F1[' + GetSymbol(SpringSolver.LoadF1) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SpringSolver.GetF1(MAT.Tempetature - DeltaTemp).Value > 0) and
     (SpringSolver.GetF1(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(SpringSolver.GetF1(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(SpringSolver.GetF1(Points[1].x));
    Result.AddPolyLine(Points, True, 'F1(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature, GetValue(SpringSolver.GetF1(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
  {$ENDIF}


end;

function TCompozer.CreateLoadF2Chart(const AScreenScale: double): TChart;
const
  DeltaTemp = 50;
var
  Points: ArrayOfTPointF = nil;
begin
  Result := TChart.Create;
  Result.LegendEnabled := False;

  {$IFDEF MODULE1}
  Result.Title := 'Load F2-Temperature Chart';
  Result.XAxisLabel := 'T[C°]';
  Result.YAxisLabel := 'F2[' + GetSymbol(SpringSolver.LoadF2) + ']';
  Result.Scale := AScreenScale;
  LoadChart1(Result, 'Custom');

  if (SpringSolver.GetF2(MAT.Tempetature - DeltaTemp).Value > 0) and
     (SpringSolver.GetF2(MAT.Tempetature + DeltaTemp).Value > 0) then
  begin
    LoadChart2(Result, 'LinearChart', 'Line');
    SetLength(Points, 2);
    Points[0].x := MAT.Tempetature - DeltaTemp;
    Points[0].y := GetValue(SpringSolver.GetF2(Points[0].x));
    Points[1].x := MAT.Tempetature + DeltaTemp;
    Points[1].y := GetValue(SpringSolver.GetF2(Points[1].x));
    Result.AddPolyLine(Points, True, 'F2(T°)');
    Points := nil;
    Result.AddDotLabel(MAT.Tempetature, GetValue(SpringSolver.GetF2(MAT.Tempetature)),
      5, 0, 10, taLeftJustify, taAlignBottom, FloatToStr(MAT.Tempetature) + ' C°');
  end;
  {$ENDIF}

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
  Result.YAxisLabel := 'G[' + GetSymbol(SpringSolver.ShearModulus) + ']';
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
  Result.YAxisLabel := 'E [' + GetSymbol(SpringSolver.YoungModulus) + ']';
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

  {$IFDEF MODULE1}
  Result.d  := GetValue(SpringSolver.WireDiameter);
  Result.Dm := GetValue(SpringSolver.Dm);
  Result.Lc := GetValue(SpringSolver.LengthLc);
  Result.n  := SpringSolver.ActiveColis;
  Result.nt1 := (SpringSolver.TotalCoils - SpringSolver.ActiveColis) / 2;
  Result.nt2 := (SpringSolver.TotalCoils - SpringSolver.ActiveColis) / 2;
  Result.ClosedEnds := False;
  Result.GroundEnds := False;
  Result.Spacer := DefaultSpacer;
  Result.Scale  := AScreenScale;
  {$ENDIF}

end;

function TCompozer.CreateQualityTable(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  Result.ColumnCount := 5;
  Result.RowCount := 7;
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
  Result[1, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade1);
  Result[2, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade1);
  Result[3, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade1);
  Result[4, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade1);
  Result[5, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade1);
  Result[6, 1] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade1);

  Result[0, 2] := '2';
  Result[1, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade2);
  Result[2, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade2);
  Result[3, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade2);
  Result[4, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade2);
  Result[5, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade2);
  Result[6, 2] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade2);

  Result[0, 3] := '3';
  Result[1, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnCoilDiameter = QualityGrade3);
  Result[2, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnFreeBodyLength = QualityGrade3);
  Result[3, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad1 = QualityGrade3);
  Result[4, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnLoad2 = QualityGrade3);
  Result[5, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnPerpendicularity = QualityGrade3);
  Result[6, 3] := TryFormatBool('x', ' ', SpringTolerance.QualityGradeOnParallelism = QualityGrade3);

  Result[0, 4] := 'Tol.';
  Result[1, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceOnCoilDiameter));
  Result[2, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceFreeBodyLength));
  Result[3, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceOnLoad1));
  Result[4, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceOnLoad2));
  Result[5, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceOnPerpendicularity));
  Result[6, 4] := '± ' + TryFloatToText(GetValue(SpringTolerance.ToleranceOnParallelism));

  {$ENDIF}
end;

function TCompozer.CreateQuick1Table(const AScreenScale: double): TReportTable;
begin
  Result := TReportTable.Create;
  {$IFDEF MODULE1} Result.ColumnCount := 7; {$ENDIF}
  {$IFDEF MODULE3} Result.ColumnCount := 7; {$ENDIF}
  {$IFDEF MODULE1} Result.RowCount := 6;    {$ENDIF}
  {$IFDEF MODULE3} Result.RowCount := 5;    {$ENDIF}
  Result.Zoom := AScreenScale;
  LoadTable(Result, 'CommonTable', 'Table');
  {$IFDEF MODULE1}
  Result[0, 0] := 'L [' + GetSymbol(SpringSolver.LengthL0) + ']';
  Result[1, 0] := TryFormatFloat('L0: %s', 'L0: ---', GetValue(SpringSolver.LengthL0));
  Result[2, 0] := TryFormatFloat('L1: %s', 'L1: ---', GetValue(SpringSolver.LengthL1));
  Result[3, 0] := TryFormatFloat('L2: %s', 'L2: ---', GetValue(SpringSolver.LengthL2));
  Result[4, 0] := TryFormatFloat('Ln: %s', 'Ln: ---', GetValue(SpringSolver.LengthLn));
  Result[5, 0] := TryFormatFloat('Lc: %s', 'Lc: ---', GetValue(SpringSolver.LengthLc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 0] := 'angle [' + GetSymbol(0*deg) + ']';
  Result[1, 0] := TryFormatFloat('α0: %s', 'α0: 0  ', GetValue(0*deg));
  Result[2, 0] := TryFormatFloat('α1: %s', 'α1: ---', GetValue(SpringSolver.Alpha1));
  Result[3, 0] := TryFormatFloat('α2: %s', 'α2: ---', GetValue(SpringSolver.Alpha2));
  Result[4, 0] := TryFormatFloat('αn: %s', 'αn: ---', GetValue(0*deg));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 1] := 'F [' + GetSymbol(SpringSolver.LoadF1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('F1: %s', 'F1: ---', GetValue(SpringSolver.LoadF1));
  Result[3, 1] := TryFormatFloat('F2: %s', 'F2: ---', GetValue(SpringSolver.LoadF2));
  Result[4, 1] := TryFormatFloat('Fn: %s', 'Fn: ---', GetValue(SpringSolver.LoadFn));
  Result[5, 1] := TryFormatFloat('Fc: %s', 'Fc: ---', GetValue(SpringSolver.LoadFc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 1] := 'torque [' + GetSymbol(SpringSolver.TorqueT1) + ']';
  Result[1, 1] := '';
  Result[2, 1] := TryFormatFloat('T1: %s', 'T1: ---', GetValue(SpringSolver.TorqueT1));
  Result[3, 1] := TryFormatFloat('T2: %s', 'T2: ---', GetValue(SpringSolver.TorqueT2));
  Result[4, 1] := TryFormatFloat('Tn: %s', 'Tn: ---', GetValue(SpringSolver.TorqueTn));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 2] := 'tau [' + GetSymbol(SpringSolver.TorsionalStressTauk1) + ']';
  Result[1, 2] := '';
  Result[2, 2] := TryFormatFloat('tauk1: %s', 'tauk1: ---', GetValue(SpringSolver.TorsionalStressTauk1));
  Result[3, 2] := TryFormatFloat('tauk2: %s', 'tauk2: ---', GetValue(SpringSolver.TorsionalStressTauk2));
  Result[4, 2] := TryFormatFloat('tau n: %s', 'tau n: ---', GetValue(SpringSolver.TorsionalStressTaun));
  Result[5, 2] := TryFormatFloat('tau c: %s', 'tau c: ---', GetValue(SpringSolver.TorsionalStressTauc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 2] := 'σ [' + GetSymbol(SpringSolver.BendingStressSigmaq1) + ']';
  Result[1, 2] := '';
  Result[2, 2] := TryFormatFloat('σ1: %s', 'σ1: ---', GetValue(SpringSolver.BendingStressSigmaq1));
  Result[3, 2] := TryFormatFloat('σ2: %s', 'σ2: ---', GetValue(SpringSolver.BendingStressSigmaq2));
  Result[4, 2] := TryFormatFloat('σn: %s', 'σn: ---', GetValue(SpringSolver.BendingStressSigman));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 3] := 's [' + GetSymbol(SpringSolver.LengthL1) + ']';
  Result[1, 3] := '';
  Result[2, 3] := TryFormatFloat('s1: %s', 's1: ---', GetValue(SpringSolver.StrokeS1));
  Result[3, 3] := TryFormatFloat('s2: %s', 's2: ---', GetValue(SpringSolver.StrokeS2));
  Result[4, 3] := TryFormatFloat('sn: %s', 'sn: ---', GetValue(SpringSolver.StrokeSn));
  Result[5, 3] := TryFormatFloat('sc: %s', 'sc: ---', GetValue(SpringSolver.StrokeSc));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 4] := 'tau/tauz';
  Result[1, 4] := '';
  Result[2, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTau1), GetValue(SpringSolver.AdmStaticTorsionalStressTauz));
  Result[3, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTau2), GetValue(SpringSolver.AdmStaticTorsionalStressTauz));
  Result[4, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTaun), GetValue(SpringSolver.AdmStaticTorsionalStressTauz));
  Result[5, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTauc), GetValue(SpringSolver.AdmStaticTorsionalStressTauz));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 3] := 'σ/σz';
  Result[1, 3] := '';
  Result[2, 3] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigmaq1), GetValue(SpringSolver.AdmStaticBendingStressSigmaz));
  Result[3, 3] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigmaq2), GetValue(SpringSolver.AdmStaticBendingStressSigmaz));
  Result[4, 3] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigman ), GetValue(SpringSolver.AdmStaticBendingStressSigmaz));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 5] := 'tau/Rm';
  Result[1, 5] := '';
  Result[2, 5] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTau1), GetValue(SpringSolver.TensileStrengthRm));
  Result[3, 5] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTau2), GetValue(SpringSolver.TensileStrengthRm));
  Result[4, 5] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTaun), GetValue(SpringSolver.TensileStrengthRm));
  Result[5, 5] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.TorsionalStressTauc), GetValue(SpringSolver.TensileStrengthRm));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 4] := 'σ/Rm';
  Result[1, 4] := '';
  Result[2, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigmaq1), GetValue(SpringSolver.TensileStrengthRm));
  Result[3, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigmaq1), GetValue(SpringSolver.TensileStrengthRm));
  Result[4, 4] := TryFormatFloatDiv('%s', '---', GetValue(SpringSolver.BendingStressSigman ), GetValue(SpringSolver.TensileStrengthRm));
  {$ENDIF}

  {$IFDEF MODULE1}
  Result[0, 6] := 'De [' + GetSymbol(SpringSolver.De) + ']';
  Result[1, 6] := TryFormatFloat('%s', '---', GetValue(SpringSolver.De));
  Result[2, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SpringSolver.De), GetValue(SpringSolver.DeltaDe) * GetValue(SpringSolver.StrokeS1), GetValue(SpringSolver.StrokeSc));
  Result[3, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SpringSolver.De), GetValue(SpringSolver.DeltaDe) * GetValue(SpringSolver.StrokeS2), GetValue(SpringSolver.StrokeSc));
  Result[4, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SpringSolver.De), GetValue(SpringSolver.DeltaDe) * GetValue(SpringSolver.StrokeSn), GetValue(SpringSolver.StrokeSc));
  Result[5, 6] := TryFormatFloatSumDiv('%s', '---', GetValue(SpringSolver.De), GetValue(SpringSolver.DeltaDe) * GetValue(SpringSolver.StrokeSc), GetValue(SpringSolver.StrokeSc));
  {$ENDIF}
  {$IFDEF MODULE3}
  Result[0, 5] := 'Lk [' + GetSymbol(SpringSolver.Lk(0*rad)) + ']';
  Result[1, 5] := TryFormatFloat('%s', '---', GetValue(SpringSolver.Lk(0*rad)));
  Result[2, 5] := TryFormatFloat('%s', '---', GetValue(SpringSolver.Lk(SpringSolver.Alpha1)));
  Result[3, 5] := TryFormatFloat('%s', '---', GetValue(SpringSolver.Lk(SpringSolver.Alpha2)));
  Result[4, 5] := TryFormatFloat('%s', '---', GetValue(0*rad));
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
  Result.Items[0, 2] := TryFormatFloat('%s', '---',
    GetValue(SpringSolver.WireDiameter))  + TryFormatFloat(' ± %s ' + GetSymbol(SpringSolver.WireDiameter), '',
    GetValue(SpringSolver.WireDiameterMax - SpringSolver.WireDiameter));

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.Di), '---', GetValue(SpringSolver.Di));

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.Dm), '---', GetValue(SpringSolver.Dm));

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] := TryFormatFloat('%s', '---', GetValue(SpringSolver.De)) +
    TryFormatFloat(' ± %s ' + GetSymbol(SpringTolerance.ToleranceOnCoilDiameter), '',
    GetValue(SpringTolerance.ToleranceOnCoilDiameter));

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := TryFormatFloat('%s coils', '---', SpringSolver.ActiveColis);


  {$IFDEF MODULE1}
  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := TryFormatFloat('%s coils', '---', SpringSolver.TotalCoils);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.SpringRateR), '---', GetValue(SpringSolver.SpringRateR));

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.De),
    '---', GetValue(SpringSolver.De + SpringSolver.DeltaDe));

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.DiMin),
    '---', GetValue(SpringSolver.DiMin));

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.DeMax),
    '---', GetValue(SpringSolver.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.DeflectionSk),
    '---', GetValue(SpringSolver.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.WireLength),
    '---', GetValue(SpringSolver.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.Mass),
    '---', GetValue(SpringSolver.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.SpringWorkW12),
    '---', GetValue(SpringSolver.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.SpringWorkW0n),
    '---', GetValue(SpringSolver.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s ' + GetSymbol(SpringSolver.NaturalFrequency),
    '---', GetValue(SpringSolver.NaturalFrequency));

  Result.Items[16, 0] := 'nu';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s', '---', SpringSolver.SeatingCoefficent);

  Result.Items[17, 0] := 'load';
  Result.Items[17, 1] := '=';

  if SpringSolver.DynamicLoad then
    Result.Items[17, 2] := ('dynamic')
  else
    Result.Items[17, 2] := ('static');
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
  Result.Items[0+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk1), '---',
    GetValue (SpringSolver.TorsionalStressTauk1));

  Result.Items[1+k, 0] := 'tauk2';
  Result.Items[1+k, 1] := '=';
  Result.Items[1+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk2), '---',
    GetValue (SpringSolver.TorsionalStressTauk2));

  Result.Items[2+k, 0] := 'taukh';
  Result.Items[2+k, 1] := '=';
  Result.Items[2+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTaukh), '---',
    GetValue (SpringSolver.TorsionalStressTaukh));

  Result.Items[3+k, 0] := 'E';
  Result.Items[3+k, 1] := '=';
  Result.Items[3+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.YoungModulus), '---',
    GetValue (SpringSolver.YoungModulus));

  Result.Items[4+k, 0] := 'G';
  Result.Items[4+k, 1] := '=';
  Result.Items[4+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.ShearModulus), '---',
    GetValue (SpringSolver.ShearModulus));

  Result.Items[5+k, 0] := 'rho';
  Result.Items[5+k, 1] := '=';
  Result.Items[5+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.MaterialDensity), '---',
    GetValue (SpringSolver.MaterialDensity));

  Result.Items[6+k, 0] := 'Rm';
  Result.Items[6+k, 1] := '=';
  Result.Items[6+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TensileStrengthRm), '---',
    GetValue (SpringSolver.TensileStrengthRm));

  Result.Items[7+k, 0] := 'tauz';
  Result.Items[7+k, 1] := '=';
  Result.Items[7+k, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.AdmStaticTorsionalStressTauz), '---',
    GetValue (SpringSolver.AdmStaticTorsionalStressTauz));

  Result.Items[8+k, 0] := 'ns';
  Result.Items[8+k, 1] := '=';
  Result.Items[8+k, 2] := TryFormatFloat('%s', '---', SpringSolver.StaticSafetyFactor);

  if SpringSolver.DynamicLoad then
  begin
    Result.Items[9+k, 0] := 'tauoz';
    Result.Items[9+k, 1] := '=';
    Result.Items[9+k, 2] := TryFormatFloat('%s ' +
      GetSymbol(SpringSolver.AdmDynamicTorsionalStressTauoz), '---',
      GetValue (SpringSolver.AdmDynamicTorsionalStressTauoz));

    Result.Items[10+k, 0] := 'tauhz';
    Result.Items[10+k, 1] := '=';
    Result.Items[10+k, 2] := TryFormatFloat('%s ' +
      GetSymbol(SpringSolver.AdmDynamicTorsionalStressRangeTauhz), '---',
      GetValue (SpringSolver.AdmDynamicTorsionalStressRangeTauhz));

    Result.Items[11+k, 0] := 'nf';
    Result.Items[11+k, 1] := '=';
    Result.Items[11+k, 2] := TryFormatFloat('%s', '---', SpringSolver.DynamicSafetyFactor);

    if SpringSolver.NumOfCycles > 0 then
    begin
      Result.Items[12+k, 0] := 'N';
      Result.Items[12+k, 1] := '=';
      Result.Items[12+k, 2] := TryFormatText('%s cycles', '---',
        TryFloatToText(SpringSolver.NumOfCycles, 2, 0));

      Result.Items[13+k, 0] := 'Nh';
      Result.Items[13+k+k, 1] := '=';
      Result.Items[13, 2] := TryFormatFloatDiv('%s hours', '---',
        SpringSolver.NumOfCycles, 3600 * SpringSolver.CycleFrequency.Value);
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

  {$IFDEF MODULE1}
  for i := 0 to Result.RowCount - 1 do
    for j := 0 to Result.ColumnCount - 1 do
    begin
      Result.Items[i, j] := ' ';
    end;

  Result.Items[0, 0] := 'd';
  Result.Items[0, 1] := '=';
  Result.Items[0, 2] :=
    TryFormatFloat('%s', '---', GetValue(SpringSolver.WireDiameter)) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(SpringSolver.WireDiameter), '',
      GetValue (SpringSolver.WireDiameterMax - SpringSolver.WireDiameter));

  Result.Items[1, 0] := 'Di';
  Result.Items[1, 1] := '=';
  Result.Items[1, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.Di), '---',
    GetValue (SpringSolver.Di));

  Result.Items[2, 0] := 'Dm';
  Result.Items[2, 1] := '=';
  Result.Items[2, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.Dm), '---',
    GetValue (SpringSolver.Dm));

  Result.Items[3, 0] := 'De';
  Result.Items[3, 1] := '=';
  Result.Items[3, 2] :=
    TryFormatFloat('%s', '---', GetValue(SpringSolver.De)) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(SpringTolerance.ToleranceOnCoilDiameter), '',
      GetValue (SpringTolerance.ToleranceOnCoilDiameter));

  Result.Items[4, 0] := 'n';
  Result.Items[4, 1] := '=';
  Result.Items[4, 2] := TryFormatFloat('%s coils', '---', SpringSolver.ActiveColis);

  Result.Items[5, 0] := 'nt';
  Result.Items[5, 1] := '=';
  Result.Items[5, 2] := TryFormatFloat('%s coils', '---', SpringSolver.TotalCoils);

  Result.Items[6, 0] := 'R';
  Result.Items[6, 1] := '=';
  Result.Items[6, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.SpringRateR), '---',
    GetValue (SpringSolver.SpringRateR));

  Result.Items[7, 0] := 'Dec';
  Result.Items[7, 1] := '=';
  Result.Items[7, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.De), '---',
    GetValue (SpringSolver.De + SpringSolver.DeltaDe));

  Result.Items[8, 0] := 'Di.min';
  Result.Items[8, 1] := '=';
  Result.Items[8, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.DiMin), '---',
    GetValue (SpringSolver.DiMin));

  Result.Items[9, 0] := 'De.max';
  Result.Items[9, 1] := '=';
  Result.Items[9, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.DeMax), '---',
    GetValue (SpringSolver.DeMax));

  Result.Items[10, 0] := 'sk';
  Result.Items[10, 1] := '=';
  Result.Items[10, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.DeflectionSk), '---',
    GetValue (SpringSolver.DeflectionSk));

  Result.Items[11, 0] := 'L';
  Result.Items[11, 1] := '=';
  Result.Items[11, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.WireLength), '---',
    GetValue (SpringSolver.WireLength));

  Result.Items[12, 0] := 'm';
  Result.Items[12, 1] := '=';
  Result.Items[12, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.Mass), '---',
    GetValue (SpringSolver.Mass));

  Result.Items[13, 0] := 'W12';
  Result.Items[13, 1] := '=';
  Result.Items[13, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.SpringWorkW12), '---',
    GetValue (SpringSolver.SpringWorkW12));

  Result.Items[14, 0] := 'W0n';
  Result.Items[14, 1] := '=';
  Result.Items[14, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.SpringWorkW0n), '---',
    GetValue (SpringSolver.SpringWorkW0n));

  Result.Items[15, 0] := 'fe';
  Result.Items[15, 1] := '=';
  Result.Items[15, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.NaturalFrequency), '---',
    GetValue (SpringSolver.NaturalFrequency));

  Result.Items[16, 0] := 'Pitch';
  Result.Items[16, 1] := '=';
  Result.Items[16, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.Pitch), '---', GetValue(SpringSolver.Pitch));

  Result.Items[17, 0] := 'PitchRatio';
  Result.Items[17, 1] := '=';
  Result.Items[17, 2] := TryFormatFloat('%s', '---', SpringSolver.PitchRatio);

  Result.Items[18, 0] := 'nu';
  Result.Items[18, 1] := '=';
  Result.Items[18, 2] := TryFormatFloat('%s', '---', SpringSolver.SeatingCoefficent);

  Result.Items[19, 0] := 'load';
  Result.Items[19, 1] := '=';

  if SpringSolver.DynamicLoad then
    Result.Items[19, 2] := ('dynamic')
  else
    Result.Items[19, 2] := ('static');

  Result.Items[22, 0] := 'tauk1';
  Result.Items[22, 1] := '=';
  Result.Items[22, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk1), '---',
    GetValue (SpringSolver.TorsionalStressTauk1));

  Result.Items[23, 0] := 'tauk2';
  Result.Items[23, 1] := '=';
  Result.Items[23, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk2), '---',
    GetValue (SpringSolver.TorsionalStressTauk2));

  Result.Items[24, 0] := 'taukh';
  Result.Items[24, 1] := '=';
  Result.Items[24, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTaukh), '---',
    GetValue (SpringSolver.TorsionalStressTaukh));

  Result.Items[26, 0] := 'E';
  Result.Items[26, 1] := '=';
  Result.Items[26, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.YoungModulus), '---',
    GetValue (SpringSolver.YoungModulus));

  Result.Items[27, 0] := 'G';
  Result.Items[27, 1] := '=';
  Result.Items[27, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.ShearModulus), '---',
    GetValue (SpringSolver.ShearModulus));

  Result.Items[28, 0] := 'rho';
  Result.Items[28, 1] := '=';
  Result.Items[28, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.MaterialDensity), '---',
    GetValue (SpringSolver.MaterialDensity));

  Result.Items[29, 0] := 'Rm';
  Result.Items[29, 1] := '=';
  Result.Items[29, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TensileStrengthRm), '---',
    GetValue (SpringSolver.TensileStrengthRm));

  Result.Items[30, 0] := 'tauz';
  Result.Items[30, 1] := '=';
  Result.Items[30, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.AdmStaticTorsionalStressTauz), '---',
    GetValue (SpringSolver.AdmStaticTorsionalStressTauz));

  Result.Items[31, 0] := 'ns';
  Result.Items[31, 1] := '=';
  Result.Items[31, 2] := TryFormatFloat('%s', '---', SpringSolver.StaticSafetyFactor);

  if SpringSolver.DynamicLoad then
  begin
    Result.Items[33, 0] := 'tauoz';
    Result.Items[33, 1] := '=';
    Result.Items[33, 2] := TryFormatFloat('%s ' +
      GetSymbol(SpringSolver.AdmDynamicTorsionalStressTauoz), '---',
      GetValue (SpringSolver.AdmDynamicTorsionalStressTauoz));

    Result.Items[34, 0] := 'tauhz';
    Result.Items[34, 1] := '=';
    Result.Items[34, 2] := TryFormatFloat('%s ' +
      GetSymbol(SpringSolver.AdmDynamicTorsionalStressRangeTauhz), '---',
      GetValue (SpringSolver.AdmDynamicTorsionalStressRangeTauhz));

    Result.Items[35, 0] := 'nf';
    Result.Items[35, 1] := '=';
    Result.Items[35, 2] := TryFormatFloat('%s', '---', SpringSolver.DynamicSafetyFactor);

    if SpringSolver.NumOfCycles > 0 then
    begin
      Result.Items[36, 0] := 'N';
      Result.Items[36, 1] := '=';
      Result.Items[36, 2] := TryFormatText('%s cycles', '---',
        TryFloatToText(SpringSolver.NumOfCycles, 2, 0));

      Result.Items[37, 0] := 'Nh';
      Result.Items[37, 1] := '=';
      Result.Items[37, 2] := TryFormatFloatDiv('%s hours', '---',
        SpringSolver.NumOfCycles, 3600 * SpringSolver.CycleFrequency.Value);
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

  {$IFDEF MODULE1}
  SpringDrawing.AutoFit := True;
  SpringDrawing.Lx := GetValue(SpringSolver.LengthL0);
  SpringDrawing.Caption := TryFormatFloat('L0 = %s', 'L0 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := GetValue(SpringSolver.LengthL1);
  SpringDrawing.Caption := TryFormatFloat('L1 = %s', 'L1 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[7].Canvas, Bit[7].Width, Bit[7].Height);

  SpringDrawing.AutoFit := False;
  SpringDrawing.Lx := GetValue(SpringSolver.LengthL2);
  SpringDrawing.Caption := TryFormatFloat('L2 = %s', 'L2 = ---', SpringDrawing.Lx);
  SpringDrawing.Draw(Bit[8].Canvas, Bit[8].Width, Bit[8].Height);

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
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' , '---',
    GetValue (SpringSolver.WireDiameter)) + TryFormatFloat(' ± %s mm', '',
    GetValue (SpringSolver.WireDiameterMax - SpringSolver.WireDiameter));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.Di), '---', GetValue(SpringSolver.Di));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Dm';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' , '---',
    GetValue (SpringSolver.Dm)) + TryFormatFloat(' ± %s ' +
    GetSymbol(SpringTolerance.ToleranceOnCoilDiameter), '',
    GetValue (SpringTolerance.ToleranceOnCoilDiameter));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'De';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] :=
    TryFormatFloat('%s ' + GetSymbol(SpringSolver.De) , '---', GetValue(SpringSolver.De));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'n';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s coils', '---', SpringSolver.ActiveColis);

  {$IFDEF MODULE1}
  Inc(Row);
  QuickXList.Items[Row, 0] := 'nt';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s colis', '---', SpringSolver.TotalCoils);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'nu';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s', '---', SpringSolver.SeatingCoefficent);

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ', '---',
    GetValue (SpringSolver.LengthL0)) + TryFormatFloat(' ± %s ' +
    GetSymbol(SpringTolerance.ToleranceFreeBodyLength), '',
    GetValue (SpringTolerance.ToleranceFreeBodyLength));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L1';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LengthL1), '---',
    GetValue (SpringSolver.LengthL1));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L2';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LengthL2), '---',
    GetValue (SpringSolver.LengthL2));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Ln';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LengthLn), '---',
    GetValue (SpringSolver.LengthLn));

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Lc';
  QuickXList.Items[Row, 1] := '=';
  QuickXList.Items[Row, 2] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LengthLc), '---',
    GetValue (SpringSolver.LengthLc));

  Inc(Row, 2);
  QuickXList.Items[Row, 0] := 'Quality specs.';
  QuickXList.Items[Row, 2] := 'EN15800';

  Inc(Row);
  QuickXList.Items[Row, 0] := 'Di, Dm, De';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnCoilDiameter) + 1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'L0';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnFreeBodyLength) + 1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnLoad1) + 1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'F2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnLoad2) + 1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e1';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnPerpendicularity) + 1);

  Inc(Row);
  QuickXList.Items[Row, 0] := 'e2';
  QuickXList.Items[Row, 1] := ' ';
  QuickXList.Items[Row, 2] := TryFormatInt('grade %s', '---', Ord(SpringTolerance.QualityGradeOnParallelism) + 1);

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
  QuickXList.Items[Row, 6] := TryFormatText('%s', '---', MAT.Items[MAT.ItemIndex]);

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Rm';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TensileStrengthRm), '---',
    GetValue (SpringSolver.TensileStrengthRm));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'G';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.ShearModulus), '---',
    GetValue (SpringSolver.ShearModulus));

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
    GetSymbol(SpringSolver.MaterialDensity), '---',
    GetValue (SpringSolver.MaterialDensity));

  Inc(Row, 3);
  QuickXList.Items[Row, 4] := 'F1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] :=
    TryFormatFloat('%s ' , '---', SpringSolver.LoadF1.Value) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(SpringTolerance.ToleranceOnLoad1), '',
      GetValue (SpringTolerance.ToleranceOnLoad1));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'F2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] :=
    TryFormatFloat('%s ', '---', SpringSolver.LoadF2.Value) +
    TryFormatFloat(' ± %s ' +
      GetSymbol(SpringTolerance.ToleranceOnLoad2), '',
      GetValue (SpringTolerance.ToleranceOnLoad2));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fn';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LoadFn), '---',
    GetValue (SpringSolver.LoadFn));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'Fc';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.LoadFc), '---',
    GetValue (SpringSolver.LoadFc));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauk1';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk1), '---',
    GetValue (SpringSolver.TorsionalStressTauk1));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauk2';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTauk2), '---',
    GetValue (SpringSolver.TorsionalStressTauk2));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'taukh';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.TorsionalStressTaukh), '---',
    GetValue (SpringSolver.TorsionalStressTaukh));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'tauhz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.AdmDynamicTorsionalStressRangeTauhz), '---',
    GetValue (SpringSolver.AdmDynamicTorsionalStressRangeTauhz));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauoz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.AdmDynamicTorsionalStressTauoz), '---',
    GetValue (SpringSolver.AdmDynamicTorsionalStressTauoz));

  Inc(Row);
  QuickXList.Items[Row, 4] := 'tauz';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s ' +
    GetSymbol(SpringSolver.AdmStaticTorsionalStressTauz), '---',
    GetValue (SpringSolver.AdmStaticTorsionalStressTauz));

  Inc(Row, 2);
  QuickXList.Items[Row, 4] := 'Static safety factor';
  QuickXList.Items[Row, 5] := '=';
  QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SpringSolver.StaticSafetyFactor);

  Inc(Row);
  if SpringSolver.DynamicLoad then
  begin
    QuickXList.Items[Row, 4] := 'Dynamic safety factor';
    QuickXList.Items[Row, 5] := '=';
    QuickXList.Items[Row, 6] := TryFormatFloat('%s', '---', SpringSolver.DynamicSafetyFactor);
    if SpringSolver.NumOfCycles > 0 then
    begin
      Inc(Row);
      QuickXList.Items[Row, 4] := 'N';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] :=
        TryFormatText('%s cycles', '---', TryFloatToText(SpringSolver.NumOfCycles, 2, 0));
      Inc(Row);
      QuickXList.Items[Row, 4] := 'Nh';
      QuickXList.Items[Row, 5] := '=';
      QuickXList.Items[Row, 6] :=
        TryFormatFloatDiv('%s hours', '---', SpringSolver.NumOfCycles,
        3600 * SpringSolver.CycleFrequency.Value);
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

end.
