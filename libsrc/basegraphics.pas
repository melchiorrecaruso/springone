{
  Helical Compression Spring Designer

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

unit BaseGraphics;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, BGRATextFX, BGRACanvas2D,
  Classes, DateUtils, Graphics, SysUtils, BaseUtils;

type
  TChart = class
  private
    FBit: TBGRABitmap;
    FBackgroundColor: TBGRAPixel;
    FColor: TBGRAPixel;

    FLegendLineLength: longint;
    FLegendEnabled: boolean;

    FTitle: string;
    FTitleFontName: string;
    FTitleFontColor: TBGRAPixel;
    FTitleFontHeight: single;
    FTitleFontStyle: TFontStyles;

    FXAxisLabel: string;
    FXAxisFontName: string;
    FXAxisFontColor: TBGRAPixel;
    FXAxisFontHeight: single;
    FXAxisFontStyle: TFontStyles;
    FXAxisLineColor: TBGRAPixel;
    FXAxisLineStyle: TPenStyle;
    FXAxisLineWidth: single;
    FXGridLineColor: TBGRAPixel;
    FXGridLineStyle: TPenStyle;
    FXGridLineWidth: single;

    FYAxisLabel: string;
    FYAxisFontName: string;
    FYAxisFontColor: TBGRAPixel;
    FYAxisFontHeight: single;
    FYAxisFontStyle: TFontStyles;
    FYAxisLineColor: TBGRAPixel;
    FYAxisLineStyle: TPenStyle;
    FYAxisLineWidth: single;
    FYGridLineColor: TBGRAPixel;
    FYGridLineStyle: TPenStyle;
    FYGridLineWidth: single;

    FCurrentFontName: string;
    FCurrentFontHeight: single;
    FCurrentFontColor: TBGRAPixel;
    FCurrentFontStyle: TFontStyles;
    FCurrentFontQuality: TBGRAFontQuality;
    FCurrentPenColor: TBGRAPixel;
    FCurrentPenStyle: TPenStyle;
    FCurrentPenWidth: single;
    FCurrentTextureColor: TBGRAPixel;
    FCurrentTextureBackgroundColor: TBGRAPixel;
    FCurrentTextureWidth: longint;
    FCurrentTextureHeight: longint;
    FCurrentTexturePenWidth: single;

    FXMin, FYMin: longint;
    FXMax, FYMax: longint;
    FXCount, FYCount: longint;

    FXMinF, FYMinF: single;
    FXMaxF, FYMaxF: single;
    FXDeltaF, FYDeltaF: single;
    FXScaleF, FYScaleF: single;
    FItems: TList;

    FWidth, FHeight: longint;
    FShowOrigin: boolean;
    FSpacer: longint;
    FScale: single;

    FIsNeededUpdateSize: boolean;
    FIsNeededCalcXDeltaF: boolean;
    FIsNeededCalcYDeltaF: boolean;
    FIsNeededCalcXCount: boolean;
    FIsNeededCalcYCount: boolean;

    procedure UpdateSize;
    procedure SetXMaxF(Value: single);
    procedure SetXMinF(Value: single);
    procedure SetYMaxF(Value: single);
    procedure SetYMinF(Value: single);
    procedure SetXDeltaF(Value: single);
    procedure SetYDeltaF(Value: single);
    procedure SetXCount(Value: longint);
    procedure SetYCount(Value: longint);

    function GetLegendSize: TSize;
    function XToCanvas(X: single): single;
    function YToCanvas(Y: single): single;
    procedure DrawLine(x0, y0, x1, y1: single; aPenColor: TBGRAPixel; aPenWidth: single);
    procedure DrawText(X, Y: single; const aText: string; aTextColor: TBGRAPixel;
      aAlign: TAlignment; aVertAlign: TVerticalAlignment);

    procedure DrawGrid;
    procedure DrawLegend;
    procedure DrawItems;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPolyLine(const aPoints: ArrayOfTPointF; aExtend: boolean; const aCaption: string);
    procedure AddPolygon(const aPoints: ArrayOfTPointF; const aCaption: string);

    procedure AddLabel(aX, aY: single; aShiftX, aShiftY: longint; aAlign: TAlignment;
      aVertAlign: TVerticalAlignment; const aCaption: string);

    procedure AddDotLabel(aX, aY, aRadius: single; aShiftX, aShiftY: longint;
      aAlign: TAlignment; aVertAlign: TVerticalAlignment; const aCaption: string);

    procedure Draw(ACanvas: TCanvas; AWidth, AHeight: longint; AOpaque: boolean = True);
    procedure Clear;
  public
    property Title: string read FTitle write FTitle;
    property TitleFontName: string read FTitleFontName write FTitleFontName;
    property TitleFontHeight: single read FTitleFontHeight write FTitleFontHeight;
    property TitleFontColor: TBGRAPixel read FTitleFontColor write FTitleFontColor;
    property TitleFontStyle: TFontStyles read FTitleFontStyle write FTitleFontStyle;

    property XAxisLabel: string read FXAxisLabel write FXAxisLabel;
    property XAxisFontName: string read FXAxisFontName write FXAxisFontName;
    property XAxisFontHeight: single read FXAxisFontHeight write FXAxisFontHeight;
    property XAxisFontColor: TBGRAPixel read FXAxisFontColor write FXAxisFontColor;
    property XAxisFontStyle: TFontStyles read FXAxisFontStyle write FXAxisFontStyle;
    property XAxisLineColor: TBGRAPixel read FXAxisLineColor write FXAxisLineColor;
    property XAxisLineStyle: TPenStyle read FXAxisLineStyle write FXAxisLineStyle;
    property XAxisLineWidth: single read FXAxisLineWidth write FXAxisLineWidth;
    property XGridLineColor: TBGRAPixel read FXGridLineColor write FXGridLineColor;
    property XGridLineStyle: TPenStyle read FXGridLineStyle write FXGridLineStyle;
    property XGridLineWidth: single read FXGridLineWidth write FXGridLineWidth;

    property YAxisLabel: string read FYAxisLabel write FYAxisLabel;
    property YAxisFontName: string read FYAxisFontName write FYAxisFontName;
    property YAxisFontHeight: single read FYAxisFontHeight write FYAxisFontHeight;
    property YAxisFontColor: TBGRAPixel read FYAxisFontColor write FYAxisFontColor;
    property YAxisFontStyle: TFontStyles read FYAxisFontStyle write FYAxisFontStyle;
    property YAxisLineColor: TBGRAPixel read FYAxisLineColor write FYAxisLineColor;
    property YAxisLineStyle: TPenStyle read FYAxisLineStyle write FYAxisLineStyle;
    property YAxisLineWidth: single read FYAxisLineWidth write FYAxisLineWidth;
    property YGridLineColor: TBGRAPixel read FYGridLineColor write FYGridLineColor;
    property YGridLineStyle: TPenStyle read FYGridLineStyle write FYGridLineStyle;
    property YGridLineWidth: single read FYGridLineWidth write FYGridLineWidth;

    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;
    property Color: TBGRAPixel read FColor write FColor;

    property LegendLineLength: longint read FLegendLineLength write FLegendLineLength;
    property LegendEnabled: boolean read FLegendEnabled write FLegendEnabled;

    property FontName: string read FCurrentFontName write FCurrentFontName;
    property FontHeight: single read FCurrentFontHeight write FCurrentFontHeight;
    property FontColor: TBGRAPixel read FCurrentFontColor write FCurrentFontColor;
    property FontStyle: TFontStyles read FCurrentFontStyle write FCurrentFontStyle;

    property PenColor: TBGRAPixel read FCurrentPenColor write FCurrentPenColor;
    property PenStyle: TPenStyle read FCurrentPenStyle write FCurrentPenStyle;
    property PenWidth: single read FCurrentPenWidth write FCurrentPenWidth;

    property TextureColor: TBGRAPixel read FCurrentTextureColor write FCurrentTextureColor;
    property TextureBackgroundColor: TBGRAPixel read FCurrentTextureBackgroundColor write FCurrentTextureBackgroundColor;
    property TextureWidth: longint read FCurrentTextureWidth write FCurrentTextureWidth;
    property TextureHeight: longint read FCurrentTextureHeight write FCurrentTextureHeight;
    property TexturePenWidth: single read FCurrentTexturePenWidth write FCurrentTexturePenWidth;

    property XMaxF: single read FXMaxF write SetXMaxF;
    property XMinF: single read FXMinF write SetXMinF;
    property YMaxF: single read FYMaxF write SetYMaxF;
    property YMinF: single read FYMinF write SetYMinF;
    property XDeltaF: single read FXDeltaF write SetXDeltaF;
    property YDeltaF: single read FYDeltaF write SetYDeltaF;
    property XCount: longint read FXCount write SetXCount;
    property YCount: longint read FYCount write SetYCount;

    property ShowOrigin: boolean read FShowOrigin write FShowOrigin;
    property Spacer: longint read FSpacer write FSpacer;
    property Scale: single read FScale write FScale;
  end;

  TReportTable = class
  private
    FBit: TBGRABitmap;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;
    FFontQuality: TBGRAFontQuality;

    FBorderWidth: longint;
    FBackgroundColor: TBGRAPixel;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;

    FLeft: longint;
    FTop: longint;
    FScale: single;
    FAutosize: boolean;
    FRowSpacer: longint;
    FRowCount: longint;
    FRowAlignments: array of TVerticalAlignment;
    FColumnSpacer: longint;
    FColumnCount: longint;
    FColumnAlignments: array of TAlignment;

    FTable: array of array of string;
    FIsNeededUpdateSize: boolean;
    FWidth: longint;
    FHeight: longint;

    function GetWidth: longint;
    function GetHeight: longint;

    function GetItem(aRow, aColumn: longint): string;
    procedure SetItem(aRow, aColumn: longint; const S: string);
    procedure SetColumnCount(Value: longint);
    procedure SetRowCount(Value: longint);
    function GetRowAlignment(Index: longint): TVerticalAlignment;
    function GetColumnAlignment(Index: longint): TAlignment;
    procedure SetRowAlignment(Index: longint; Value: TVerticalAlignment);
    procedure SetColumnAlignment(Index: longint; Value: TAlignment);
    procedure SetSize(aRowCount, aColumnCount: longint);

    procedure SetFontName(const Value: string);
    procedure SetFontHeight(const Value: single);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure UpdateSize;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(ACanvas: TCanvas; AWidth, AHeight: longint; AOpaque: boolean = True);
  public
    property Autosize: boolean read FAutosize write FAutosize;
    property RowCount: longint read FRowCount write SetRowCount;
    property ColumnCount: longint read FColumnCount write SetColumnCount;
    property Items[Row, Column: longint]: string read GetItem write SetItem; default;

    property BorderWidth: longint read FBorderWidth write FBorderWidth;
    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;

    property FontName: string read FFontName write SetFontName;
    property FontHeight: single read FFontHeight write SetFontHeight;
    property FontColor: TBGRAPixel read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property PenColor: TBGRAPixel read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenWidth: single read FPenWidth write FPenWidth;

    property RowSpacer: longint read FRowSpacer write FRowSpacer;
    property RowAlignments[Row: longint]: TVerticalAlignment read GetRowAlignment write SetRowAlignment;
    property ColumnSpacer: longint read FColumnSpacer write FColumnSpacer;
    property ColumnAlignments[Column: longint]: TAlignment read GetColumnAlignment write SetColumnAlignment;

    property Zoom: single read FScale write FScale;

    property Left: longint read FLeft write FLeft;
    property Top: longint read FTop write FTop;
    property Width: longint read GetWidth;
    property Height: longint read GetHeight;
  end;

  TSpringDrawing = class
  private
    Fd: double;
    FDm: double;
    fClockWise: boolean;
    FLc: double;
    FLx: double;
    Fn: double;
    Fnt1: double;
    Fnt2: double;
    FPitchF: double;
    fClosedEnds: boolean;
    FGroundEnds: boolean;

    FCaption: string;
    FAutoFit: boolean;
    FAutoScale: double;
    FScale: single;
    FSpacer: longint;

    FBit: TBGRABitmap;
    FBitCharSize: TSize;
    FBackgroundColor: TBGRAPixel;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;

    FCenterLineColor: TBGRAPixel;
    FCenterLineStyle: TPenStyle;
    FCenterLineWidth: single;

    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
    FWidth, FHeight: longint;

    function xt(const t: double): double;
    function yt(const t: double): double;
    function zt(const t: double): double;
    function PreCheck: boolean;

    function XToCanvas(X: single): single;
    function YToCanvas(Y: single): single;

  public
    constructor Create;
    destructor Destroy; override;
    procedure DrawInSection(aCanvas: TCanvas);
    procedure DrawInSection(aCanvas: TCanvas; aWidth, aHeight: longint);

    procedure DrawInProfile(aCanvas: TCanvas);
    procedure DrawInProfile(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property d: double read Fd write Fd;
    property Dm: double read FDm write FDm;
    property Lc: double read FLc write FLc;
    property Lx: double read FLx write FLx;
    property n: double read Fn write Fn;
    property nt1: double read Fnt1 write Fnt1;
    property nt2: double read Fnt2 write Fnt2;
    property ClockWise: boolean read fClockWise write fClockWise;
    property GroundEnds: boolean read FGroundEnds write FGroundEnds;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;

    property Caption: string read FCaption write FCaption;
    property AutoFit: boolean read fAutoFit write fAutoFit;

    property FontName: string read FFontName write FFontName;
    property FontHeight: single read FFontHeight write FFontHeight;
    property FontColor: TBGRAPixel read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;

    property CenterLineColor: TBGRAPixel read FCenterLineColor write FCenterLineColor;
    property CenterLineStyle: TPenStyle read FCenterLineStyle write FCenterLineStyle;
    property CenterLineWidth: single read FCenterLineWidth write FCenterLineWidth;

    property PenColor: TBGRAPixel read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenWidth: single read FPenWidth write FPenWidth;

    property TextureColor: TBGRAPixel read FTextureColor write FTextureColor;
    property TextureBackgroundColor: TBGRAPixel read FTextureBackgroundColor write FTextureBackgroundColor;
    property TextureWidth: longint read FTextureWidth write FTextureWidth;
    property TextureHeight: longint read FTextureHeight write FTextureHeight;
    property TexturePenWidth: single read FTexturePenWidth write FTexturePenWidth;

    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;

    property Spacer: longint read FSpacer write FSpacer;
    property Height: longint read FHeight write FHeight;
    property Width: longint read FWidth write FWidth;

    property Scale: single read FScale write FScale;
  end;


procedure DrawLogo(aCanvas: TCanvas; aWidth, aHeight: longint);

const
  DefaultSpacer = 16;

implementation

uses ADim, Math;

type
  TChartItem = class
  private
    FCaption: string;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartLabelItem = class(TChartItem)
  private
    FX: single;
    FY: single;
    FShiftX: longint;
    FShiftY: longint;
    FAlign: TAlignment;
    FVertAlign: TVerticalAlignment;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartPolyLineItem = class(TChartItem)
  private
    FExtend: boolean;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FPoints: ArrayOfTPointF;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartPolygonItem = class(TChartPolyLineItem)
  private
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartDotLabelItem = class(TChartLabelItem)
  private
    FRadius: single;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
  public
    constructor Create;
    destructor Destroy; override;
  end;

// Common routines

function GetDelta(Count: longint; const Range: single): single;
var
  k: double = 0.001;
begin
  Result := 0;
  while True do
  begin
    Result := 0.10 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.20 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.25 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.50 * k; if (Result * Count) >= (Range) then Break;
    k := k * 10;
  end;
end;

function GetMin(const MinValue: single): single;
var
  k: longint;
begin
  Result := 0;
  if MinValue > 0 then
  begin
    k := Trunc(Log10(MinValue));
    if k > 1 then
      Result := Trunc(MinValue / Power(10, k - 1)) * Power(10, k - 1)
    else
      Result := MinValue;
  end else
    if MinValue < 0 then
    begin
      k := Trunc(Log10(Abs(MinValue)));
      if k > 1 then
        Result := -Trunc(Abs(MinValue) / Power(10, k - 1) + 1) * Power(10, k - 1)
      else
        Result := MinValue;
    end;
end;

// TChartItem

constructor TChartItem.Create;
begin
  inherited Create;
end;

destructor TChartItem.Destroy;
begin
  inherited Destroy;
end;

// TChartLabelItem

constructor TChartLabelItem.Create;
begin
  inherited Create;
end;

destructor TChartLabelItem.Destroy;
begin
  inherited Destroy;
end;

// TChartPolyLineItem

constructor TChartPolyLineItem.Create;
begin
  inherited Create;
  FPoints := nil;
end;

destructor TChartPolyLineItem.Destroy;
begin
  FPoints := nil;
  inherited Destroy;
end;

// TChartPoligonItem

constructor TChartPolygonItem.Create;
begin
  inherited Create;
  FPoints := nil;
end;

destructor TChartPolygonItem.Destroy;
begin
  FPoints := nil;
  inherited Destroy;
end;

// TChartDotLabelItem

constructor TChartDotLabelItem.Create;
begin
  inherited Create;
end;

destructor TChartDotLabelItem.Destroy;
begin
  inherited Destroy;
end;

// TChart

constructor TChart.Create;
begin
  inherited Create;
  FBit := TBGRABitmap.Create;
  FItems := TList.Create;
  Clear;
end;

destructor TChart.Destroy;
begin
  Clear;
  FItems.Destroy;
  FBit.Destroy;
end;

procedure TChart.Clear;
var
  I: longint;
begin
  FColor.FromColor(clWindow);
  FBackgroundColor.FromColor(clBtnFace);

  FTitle := 'Chart';
  FTitleFontName := 'default';
  FTitleFontColor.FromColor(clBlack);
  FTitleFontHeight := 20;
  FTitleFontStyle := [fsBold];

  FXAxisLabel := 'X Axis';
  FXAxisFontName := 'default';
  FXAxisFontColor.FromColor(clBlack);
  FXAxisFontHeight := 14;
  FXAxisFontStyle := [fsBold];
  FXAxisLineColor.FromColor(clBlack);
  FXAxisLineStyle := psSolid;
  FXAxisLineWidth := 1.0;
  FXGridLineColor.FromColor(clSilver);
  FXGridLineStyle := psSolid;
  FXGridLineWidth := 0.5;

  FYAxisLabel := 'Y';
  FYAxisFontName := 'default';
  FYAxisFontColor.FromColor(clBlack);
  FYAxisFontHeight := 14;
  FYAxisFontStyle := [fsBold];
  FYAxisLineColor.FromColor(clBlack);
  FYAxisLineStyle := psSolid;
  FYAxisLineWidth := 1.0;
  FYGridLineColor.FromColor(clSilver);
  FYGridLineStyle := psSolid;
  FYGridLineWidth := 0.5;

  FLegendLineLength := 16;
  FLegendEnabled := True;

  FCurrentFontName := 'default';
  FCurrentFontHeight := 14;
  FCurrentFontColor.FromColor(clRed);
  FCurrentFontStyle := [fsBold];
  FCurrentPenColor.FromColor(clRed);
  FCurrentPenStyle := psSolid;
  FCurrentFontQuality := fqSystem;

  FCurrentPenWidth := 1.0;
  FCurrentTextureColor.FromColor(clRed);
  FCurrentTextureBackgroundColor := BGRA(255, 255, 255, 0);
  FCurrentTextureWidth    := 8;
  FCurrentTextureHeight   := 8;
  FCurrentTexturePenWidth := 1.0;

  FXMin    := 0;
  FXMax    := 0;
  FYMin    := 0;
  FYMax    := 0;

  FXMinF   := 0;
  FXMaxF   := 0;
  FYMinF   := 0;
  FYMaxF   := 0;
  FXDeltaF := 0;
  FYDeltaF := 0;
  FXScaleF := 0;
  FYScaleF := 0;
  FXCount  := 0;
  FYCount  := 0;

  FWidth   := 0;
  FHeight  := 0;

  FIsNeededUpdateSize  := True;
  FIsNeededCalcXDeltaF := True;
  FIsNeededCalcYDeltaF := True;
  FIsNeededCalcXCount  := True;
  FIsNeededCalcYCount  := True;

  FShowOrigin := False;
  FSpacer := DefaultSpacer;
  FScale  := 1.0;

  for I := 0 to FItems.Count -1 do
    TChartItem(FItems[I]).Destroy;
  FItems.Clear;
end;

procedure TChart.AddPolyLine(const APoints: ArrayOfTPointF;
  aExtend: boolean; const ACaption: string);
var
  I: longint;
  Item: TChartPolyLineItem;
begin
  Item := TChartPolyLineItem.Create;
  SetLength(Item.FPoints, Length(APoints));
  for I := Low(APoints) to High(APoints) do
    Item.FPoints[I] := APoints[I];

  Item.FCaption    := ACaption;
  Item.FFontName   := FCurrentFontName;
  Item.FFontHeight := FCurrentFontHeight;
  Item.FFontColor  := FCurrentFontColor;
  Item.FFontStyle  := FCurrentFontStyle;
  Item.FPenColor   := FCurrentPenColor;
  Item.FPenStyle   := FCurrentPenStyle;
  Item.FPenWidth   := FCurrentPenWidth;
  Item.FExtend     := aExtend;
  FItems.Add(Item);
end;

procedure TChart.AddPolygon(const aPoints: ArrayOfTPointF; const ACaption: string);
var
  I: longint;
  Item: TChartPolygonItem;
begin
  Item := TChartPolygonItem.Create;
  SetLength(Item.FPoints, Length(APoints));
  for I := Low(APoints) to High(APoints) do
    Item.FPoints[I] := APoints[I];

  Item.FCaption                := ACaption;
  Item.FFontName               := FCurrentFontName;
  Item.FFontHeight             := FCurrentFontHeight;
  Item.FFontColor              := FCurrentFontColor;
  Item.FFontStyle              := FCurrentFontStyle;
  Item.FPenColor               := FCurrentPenColor;
  Item.FPenStyle               := FCurrentPenStyle;
  Item.FPenWidth               := FCurrentPenWidth;
  Item.FTextureColor           := FCurrentTextureColor;
  Item.FTextureBackgroundColor := FCurrentTextureBackgroundColor;
  Item.FTextureWidth           := FCurrentTextureWidth;
  Item.FTextureHeight          := FCurrentTextureHeight;
  Item.FTexturePenWidth        := FCurrentTexturePenWidth;
  FItems.Add(Item);
end;

procedure TChart.AddLabel(AX, AY: single; AShiftX, AShiftY: longint;
  AAlign: TAlignment; AVertAlign: TVerticalAlignment; const ACaption: string);
var
  Item: TChartLabelItem;
begin
  Item             := TChartLabelItem.Create;
  Item.FCaption    := ACaption;
  Item.FFontName   := FCurrentFontName;
  Item.FFontHeight := FCurrentFontHeight;
  Item.FFontColor  := FCurrentFontColor;
  Item.FFontStyle  := FCurrentFontStyle;
  Item.FX          := AX;
  Item.FY          := AY;
  Item.FShiftX     := AShiftX;
  Item.FShiftY     := AShiftY;
  Item.FAlign      := AAlign;
  Item.FVertAlign  := AVertAlign;
  FItems.Add(Item);
end;

procedure TChart.AddDotLabel(aX, aY, aRadius: single; aShiftX, aShiftY: longint;
  aAlign: TAlignment; aVertAlign: TVerticalAlignment; const aCaption: string);
var
  Item: TChartDotLabelItem;
begin
  Item                         := TChartDotLabelItem.Create;
  Item.FCaption                := ACaption;
  Item.FFontName               := FCurrentFontName;
  Item.FFontHeight             := FCurrentFontHeight;
  Item.FFontColor              := FCurrentFontColor;
  Item.FFontStyle              := FCurrentFontStyle;
  Item.FPenColor               := FCurrentPenColor;
  Item.FPenStyle               := FCurrentPenStyle;
  Item.FPenWidth               := FCurrentPenWidth;
  Item.FTextureColor           := FCurrentTextureColor;
  Item.FTextureBackgroundColor := FCurrentTextureBackgroundColor;
  Item.FTextureWidth           := FCurrentTextureWidth;
  Item.FTextureHeight          := FCurrentTextureHeight;
  Item.FTexturePenWidth        := FCurrentTexturePenWidth;
  Item.FX                      := AX;
  Item.FY                      := AY;
  Item.FRadius                 := aRadius;
  Item.FShiftX                 := AShiftX;
  Item.FShiftY                 := AShiftY;
  Item.FAlign                  := AAlign;
  Item.FVertAlign              := AVertAlign;
  FItems.Add(Item);
end;

procedure TChart.UpdateSize;
var
  i, j: longint;
  Item: TChartItem;
begin
  if FItems.Count = 0 then
  begin
    FXMinF := 0;
    FXMaxF := 0;
    FYMinF := 0;
    FYMaxF := 0;
  end else
  begin
    if FIsNeededUpdateSize then
    begin
      FXMinF := + MaxSingle;
      FXMaxF := - MaxSingle;
      FYMinF := + MaxSingle;
      FYMaxF := - MaxSingle;

      for i := 0 to FItems.Count -1 do
      begin
        Item := TChartItem(FItems[i]);
        if (Item is TChartPolyLineItem) then
        begin
          for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
          begin
            FXMinF := Min(FXMinF, TChartPolyLineItem(Item).FPoints[j].x);
            FXMaxF := Max(FXMaxF, TChartPolyLineItem(Item).FPoints[j].x);
            FYMinF := Min(FYMinF, TChartPolyLineItem(Item).FPoints[j].y);
            FYMaxF := Max(FYMaxF, TChartPolyLineItem(Item).FPoints[j].y);
          end;
        end;
      end;
      FXMinF := GetMin(FXMinF);
      FYMinF := GetMin(FYMinF);
    end;
  end;

  if FShowOrigin then
  begin
    FXMinF := Min(FXMinF, 0);
    FYMinF := Min(FYMinF, 0);
  end;
end;

procedure TChart.DrawGrid;
var
  I: longint;
  X, XShift, XSpacing: double;
  Y, YShift, YSpacing: double;
begin
  XShift   := -(FSpacer * FScale) * 0.5;
  YShift   := -(FSpacer * FScale) * 0.5;
  XSpacing := +(FXMax - FXMin) / FXCount;
  YSpacing := +(FYMax - FYMin) / FYCount;
  // Draw Y secondary axis and X labels
  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FXAxisFontName;
  FBit.FontStyle     := FXAxisFontStyle;
  FBit.FontHeight    := Trunc(FXAxisFontHeight * FScale);
  FBit.JoinStyle     := pjsRound;
  FBit.LineCap       := pecRound;
  FBit.PenStyle      := FYGridLineStyle;

  for I := 0 to FXCount - 1 do
  begin
    X := FXMin + XSpacing * I;
    DrawLine(X, FYMin, X, FYMax, FYGridLineColor, FYGridLineWidth * FScale);
    DrawText(X, FYMin + YShift, GetString(FXMinF + FXDeltaF * I), FXAxisFontColor, taCenter, taAlignTop);
  end;
  DrawLine(FXMax, FYMin, FXMax, FYMax, FYGridLineColor, FYGridLineWidth * FScale);
  DrawText(FXMax, FYMin + YShift, FXAxisLabel, FXAxisFontColor, taCenter, taAlignTop);
  // Draw X secondary axis and Y labels
  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FYAxisFontName;
  FBit.FontStyle     := FYAxisFontStyle;
  FBit.FontHeight    := Trunc(FYAxisFontHeight * FScale);
  FBit.JoinStyle     := pjsRound;
  FBit.LineCap       := pecRound;
  FBit.PenStyle      := FXGridLineStyle;

  for I := 0 to FYCount - 1 do
  begin
    Y := FYMin + YSpacing * I;
    DrawLine(FXMin, Y, FXMax, Y, FXGridLineColor, FXGridLineWidth * FScale);
    DrawText(FXMin + XShift, Y, GetString(FYMinF + FYDeltaF * I), FYAxisFontColor, taRightJustify, taVerticalCenter);
  end;
  DrawLine(FXMin, FYMax, FXMax, FYMax, FXGridLineColor, FXGridLineWidth * FScale);
  DrawText(FXMin + XShift, FYMax, FYAxisLabel, FYAxisFontColor, taRightJustify, taVerticalCenter);
  // Draw Chart Title
  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FTitleFontName;
  FBit.FontStyle     := FTitleFontStyle;
  FBit.FontHeight    := Trunc(FTitleFontHeight * FScale);

  YShift := (FSpacer * FScale) * 0.5;
  DrawText((FXMin + FXMax) * 0.5, FYMax + YShift, FTitle, FTitleFontColor, taCenter, taAlignBottom);
end;

procedure TChart.DrawLine(x0, y0, x1, y1: single; aPenColor: TBGRAPixel; aPenWidth: single);
begin
  FBit.DrawLineAntialias(
    XToCanvas(x0),
    YToCanvas(y0),
    XToCanvas(x1),
    YToCanvas(y1),
    aPenColor, aPenWidth);
end;

procedure TChart.DrawText(X, Y: single; const AText: string; ATextColor: TBGRAPixel;
  AAlign: TAlignment; AVertAlign: TVerticalAlignment);
var
  ShiftX, ShiftY: double;
  TxtSize: TSize;
begin
  TxtSize := FBit.TextSize(AText);
  case AAlign of
    taLeftJustify:    ShiftX := 0;
    taRightJustify:   ShiftX := - TxtSize.Width;
    taCenter:         ShiftX := - TxtSize.Width / 2;
  end;

  case AVertAlign of
    taAlignTop:       ShiftY := 0;
    taAlignBottom:    ShiftY := + TxtSize.Height;
    taVerticalCenter: ShiftY := + TxtSize.Height / 2;
  end;
  FBit.TextOut(XToCanvas(X + ShiftX), YToCanvas(Y + ShiftY), AText, ATextColor, taLeftJustify);
end;

function TChart.GetLegendSize: TSize;
var
  I: longint;
  Item: TChartItem;
  TxtSize: TSize;
begin
  Result.Width  := 0;
  Result.Height := 0;
  if not FLegendEnabled then Exit;

  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);

    if (Item is TChartPolygonItem) or
       (Item is TChartPolyLineItem) then
    begin
      if Item.FCaption <> '' then
      begin
        FBit.FontAntialias := True;
        FBit.FontQuality   := FCurrentFontQuality;
        FBit.FontName      := Item.FFontName;
        FBit.FontStyle     := Item.FFontStyle;
        FBit.FontHeight    := Trunc(Item.FFontHeight*FScale);
        TxtSize            := FBit.TextSize(Item.FCaption);

        Result.Width  := Max(Result.Width,  TxtSize.Width);
        Result.Height := Max(Result.Height, TxtSize.Height);
      end;
    end;
  end;

  if Result.Width > 0 then
    Result.Width := Result.Width + Trunc(FSpacer*FScale*2.5 + FLegendLineLength*FScale);

  if Result.Height > 0 then
    Result.Height := Result.Height + Trunc(FSpacer*FScale);
end;

procedure TChart.DrawLegend;
var
  I: longint;
  Item: TChartItem;
  X, Y: single;
  TxtSize: TSize;
begin
  if not FLegendEnabled then Exit;

  X := FXMax;
  Y := FYMax;
  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);

    if (Item is TChartPolygonItem) or
       (Item is TChartPolyLineItem) then
    begin
      if Item.FCaption <> '' then
      begin
        FBit.JoinStyle := pjsRound;
        FBit.LineCap   := pecRound;
        FBit.PenStyle := TChartPolyLineItem(Item).FPenStyle;

        FBit.DrawLineAntialias(
          XToCanvas(X + FSpacer * FScale),
          YToCanvas(Y),
          XToCanvas(X + FSpacer * FScale + FLegendLineLength * FScale),
          YToCanvas(Y),
          TChartPolyLineItem(Item).FPenColor,
          TChartPolyLineItem(Item).FPenWidth * FScale);

        FBit.FontAntialias := True;
        FBit.FontQuality   := FCurrentFontQuality;
        FBit.FontName      := Item.FFontName;
        FBit.FontHeight    := Trunc(Item.FFontHeight * FScale);
        FBit.FontStyle     := Item.FFontStyle;
        TxtSize            := FBit.TextSize(Item.FCaption);

        DrawText(X + (FSpacer * FScale * 1.5) + FLegendLineLength * FScale, Y,
          Item.FCaption, Item.FFontColor, taLeftJustify, taVerticalCenter);

        Y := Y - TxtSize.Height - (FSpacer * FScale) * 0.25;
      end;
    end;
  end;
end;

function TChart.XToCanvas(X: single): single;
begin
  Result := X;
end;

function TChart.YToCanvas(Y: single): single;
begin
  Result := FHeight - Y;
end;

function GetCoefficent(const P1, P2: TPointF; var m, q: single): boolean;
begin
  Result := P2.x <> P1.x;
  if Result then
  begin
    m := (P2.y - P1.y) / (P2.x - P1.x);
    q := (P2.y - P2.x * m);
  end;
end;

procedure TChart.DrawItems;
var
  I, J: longint;
  Item: TChartItem;
  T: ArrayOfTPointF;
  Tex: TBGRABitmap;
  m, q: single;
begin
  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);

    if Item.ClassType = TChartPolygonItem then
    begin
      with Item as TChartPolygonItem do
      begin
        FBit.JoinStyle := pjsRound;
        FBit.LineCap   := pecRound;
        FBit.PenStyle  := FPenStyle;

        T := nil;
        SetLength(T, Length(FPoints));
        for j := Low(FPoints) to High(FPoints) do
        begin
          T[j].x := XToCanvas(FXMin + (FPoints[j].x - FXMinF) * FXScaleF);
          T[j].y := YToCanvas(FYMin + (FPoints[j].y - FYMinF) * FYScaleF);
        end;

        Tex := FBit.CreateBrushTexture(bsFDiagonal, FTextureColor, FTextureBackgroundColor,
          Trunc(FTextureWidth*FScale), Trunc(FTextureHeight*FScale), FTexturePenWidth*FScale) as TBGRABitmap;
        FBit.FillPolyAntialias(T, Tex);
        FBit.DrawPolygonAntialias(T, FPenColor, FPenWidth * FScale, BGRA(255, 255, 255, 0));
        Tex.Destroy;
        T := nil;
      end;
    end else
    if Item is TChartPolyLineItem then
    begin
      with Item as TChartPolyLineItem do
      begin
        FBit.JoinStyle := pjsRound;
        FBit.LineCap   := pecRound;
        FBit.PenStyle  := FPenStyle;

        T := nil;
        SetLength(T, Length(FPoints));
        for j := Low(FPoints) to High(FPoints) do
        begin
          T[j].x := FPoints[j].x;
          T[j].y := FPoints[j].y;
        end;

        if FExtend then
        begin
          j := Low(T);
          if (T[j + 1].Distance(T[j]) <> 0) then
          begin
            if GetCoefficent(T[j + 1], T[j], m, q) then
            begin
              T[j].x := FXMinF;
              T[j].y := Max(Min(m * FXMinF + q, FYMaxF), FYMinF);

              if m = 0 then
                T[j].x := FXMinF
              else
                T[j].x := (T[j].y - q) / m;
            end else
            begin
              T[j].y := FYMinF;
            end;
          end;

          j := High(T);
          if GetCoefficent(T[j - 1], T[j], m, q) then
          begin
            T[j].x := FXMaxF;
            T[j].y := Max(Min(m * FXMaxF + q, FYMaxF), FYMinF);

            if m = 0 then
              T[j].x := FXMaxF
            else
              T[j].x := (T[j].y - q) / m;
          end else
          begin
            T[j].y := FYMaxF;
          end;
        end;

        for j := Low(T) to High(T) do
        begin
          T[j].x := XToCanvas(FXMin + (T[j].x - FXMinF) * FXScaleF);
          T[j].y := YToCanvas(FYMin + (T[j].y - FYMinF) * FYScaleF);
        end;
        FBit.DrawPolyLineAntialias(T, FPenColor, FPenWidth * FScale, BGRA(255, 255, 255, 0));

        T := nil;
      end;
    end else
    if Item is TChartDotLabelItem then
    begin
      with Item as TChartDotLabelItem do
      begin
        FBit.JoinStyle := pjsRound;
        FBit.LineCap   := pecRound;
        FBit.PenStyle  := FPenStyle;

        FBit.EllipseAntialias(
          XToCanvas(FXMin + (FX - FXMinF) * FXScaleF),
          YToCanvas(FYMin + (FY - FYMinF) * FYScaleF),
          FRadius*FScale,
          FRadius*FScale,
          FPenColor,
          FPenWidth,
          FPenColor);

        FBit.FontAntialias := True;
        FBit.FontQuality   := FCurrentFontQuality;
        FBit.FontName      := FFontName;
        FBit.FontHeight    := Trunc(FFontHeight * FScale);
        FBit.FontStyle     := FFontStyle;

        DrawText(
          FXMin + (FX - FXMinF) * FXScaleF + FShiftX*FScale,
          FYMin + (FY - FYMinF) * FYScaleF + FShiftY*FScale,
          FCaption, FFontColor, FAlign, FVertAlign);
      end;
    end else
    if Item is TChartLabelItem then
    begin
      with Item as TChartLabelItem do
      begin
        FBit.FontAntialias := True;
        FBit.FontQuality   := FCurrentFontQuality;
        FBit.FontName      := FFontName;
        FBit.FontHeight    := Trunc(FFontHeight * FScale);
        FBit.FontStyle     := FFontStyle;

        DrawText(
          FXMin + (FX - FXMinF) * FXScaleF + FShiftX*FScale,
          FYMin + (FY - FYMinF) * FYScaleF + FShiftY*FScale,
          FCaption, FFontColor, FAlign, FVertAlign);
      end;
    end;
  end;
end;

procedure TChart.Draw(ACanvas: TCanvas; AWidth, AHeight: longint; AOpaque: boolean = True);
var
  i, j: longint;
  Item: TChartItem;
  maxXLabelWidth:  longint = 0;
  maxYLabelWidth:  longint = 0;
  maxXLabelHeight: longint = 0;
  maxYLabelHeight: longint = 0;
  maxTitleWidth:   longint = 0;
  maxTitleHeight:  longint = 0;
  Start: TDateTime;
  TxtSize1: TSize;
  TxtSize2: TSize;
begin
  Start   := Now;
  FWidth  := AWidth;
  FHeight := AHeight;
  FBit.SetSize(AWidth, AHeight);
  FBit.Fill(FBackgroundColor);

  UpdateSize;

  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FXAxisFontName;
  FBit.FontStyle     := FXAxisFontStyle;
  FBit.FontHeight    := Trunc(FXAxisFontHeight * FScale);

  TxtSize1        := FBit.TextSize(FXAxisLabel);
  TxtSize2        := FBit.TextSize(GetString(FXMaxF));
  maxXLabelWidth  := Max(TxtSize1.Width,  TxtSize2.Width ) + Trunc(FSpacer * FScale);
  maxXLabelHeight := Max(TxtSize1.Height, TxtSize2.Height) + Trunc(FSpacer * FScale);

  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FYAxisFontName;
  FBit.FontStyle     := FYAxisFontStyle;
  FBit.FontHeight    := Trunc(FYAxisFontHeight * FScale);

  TxtSize1        := FBit.TextSize(FYAxisLabel);
  TxtSize2        := FBit.TextSize(GetString(FYMaxF));
  maxYLabelWidth  := Max(TxtSize1.Width,  TxtSize2.Width ) + Trunc(FSpacer * FScale);
  maxYLabelHeight := Max(TxtSize1.Height, TxtSize2.Height) + Trunc(FSpacer * FScale);

  FBit.FontAntialias := True;
  FBit.FontQuality   := FCurrentFontQuality;
  FBit.FontName      := FTitleFontName;
  FBit.FontStyle     := FTitleFontStyle;
  FBit.FontHeight    := Trunc(FTitleFontHeight * FScale);

  TxtSize1       := FBit.TextSize(FTitle);
  maxTitleWidth  := TxtSize1.Width  + Trunc(FSpacer * FScale);
  maxTitleHeight := TxtSize1.Height + Trunc(FSpacer * FScale);

  FXMin := maxYLabelWidth;
  FYMin := maxXLabelHeight;
  FXMax := FWidth  - Max(maxXLabelWidth div 2, GetLegendSize.Width);
  FYMax := FHeight - maxTitleHeight;

  if (FXMax > FXMin) and (FYMax > FYMin) then
  begin
    if FIsNeededCalcXCount then FXCount := (FXMax - FXMin) div (maxXLabelWidth);
    if FIsNeededCalcYCount then FYCount := (FYMax - FYMin) div (maxYLabelHeight);

    if (FXCount > 0) and (FYCount > 0) then
    begin
      if FIsNeededCalcXDeltaF then FXDeltaF := GetDelta(FXCount, FXMaxF - FXMinF);
      if FIsNeededCalcYDeltaF then FYDeltaF := GetDelta(FYCount, FYMaxF - FYMinF);

      if (FXDeltaF > 0) and (FYDeltaF > 0) then
      begin
        while (FXMinF + ((FXCount -1) * FXDeltaF) > FXMaxF) do Dec(FXCount);
        while (FYMinF + ((FYCount -1) * FYDeltaF) > FYMaxF) do Dec(FYCount);

        FXMaxF   :=  FXMinF + FXDeltaF * FXCount;
        FYMaxF   :=  FYMinF + FYDeltaF * FYCount;
        FXScaleF := (FXMax - FXMin) / (FXMaxF - FXMinF);
        FYScaleF := (FYMax - FYMin) / (FYMaxF - FYMinF);

        DrawGrid;
        DrawItems;
        DrawLegend;
      end;
    end;
  end;

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FXAxisLineStyle;
  DrawLine(FXMin, FYMin, FXMax, FYmin, FXAxisLineColor, FXAxisLineWidth * FScale);

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FYAxisLineStyle;
  DrawLine(FXMin, FYMin, FXMin, FYMax, FYAxisLineColor, FYAxisLineWidth * FScale);

  FBit.InvalidateBitmap;
  FBit.Draw(aCanvas, 0, 0, AOpaque);
end;

procedure TChart.SetXMaxF(Value: single);
begin
  FIsNeededUpdateSize := True;
  FXMaxF := Value;
end;

procedure TChart.SetXMinF(Value: single);
begin
  FIsNeededUpdateSize := True;
  FXMinF := Value;
end;

procedure TChart.SetYMaxF(Value: single);
begin
  FIsNeededUpdateSize := True;
  FYMaxF := Value;
end;

procedure TChart.SetYMinF(Value: single);
begin
  FIsNeededUpdateSize := True;
  FYMinF := Value;
end;

procedure TChart.SetXDeltaF(Value: single);
begin
  FXDeltaF := Value;
  FIsNeededCalcXDeltaF := False;
end;

procedure TChart.SetYDeltaF(Value: single);
begin
  FYDeltaF := Value;
  FIsNeededCalcYDeltaF := False;
end;

procedure TChart.SetXCount(Value: longint);
begin
  FXCount := Value;
  FIsNeededCalcXCount := False;
end;

procedure TChart.SetYCount(Value: longint);
begin
  FYCount := Value;
  FIsNeededCalcYCount := False;
end;

// TReportTable

constructor TReportTable.Create;
begin
  inherited Create;
  FBit := TBGRABitmap.Create;
  FBit.FontRenderer := TBGRATextEffectFontRenderer.Create;

  FFontName    := 'default';
  FFontHeight  := 13;
  FFontColor   := BGRA(0, 0, 0, 255);
  FFontStyle   := [fsBold];
  FFontQuality := fqSystem; //fqSystemClearType,

  FIsNeededUpdateSize := True;
  FWidth  := 0;
  FHeight := 0;

  FPenColor   := BGRA(0, 0, 0, 255);
  FPenStyle   := psSolid;
  FPenWidth   := 1.0;

  FAutosize        := True;
  FBorderWidth     := DefaultSpacer;
  FBackgroundColor := BGRA(255, 255, 255, 255);

  FLeft  := 0;
  FTop   := 0;
  FScale := 1.0;
  FColumnSpacer := 0;
  FRowSpacer := 0;

  SetSize(0, 0);
end;

destructor TReportTable.Destroy;
begin
  SetSize(0, 0);
  FBit.Destroy;
  inherited Destroy;
end;

function TReportTable.GetItem(aRow, aColumn: longint): string;
begin
  Result := FTable[aRow][aColumn];
end;

procedure TReportTable.SetItem(aRow, aColumn: longint; const S: string);
begin
  FTable[aRow][aColumn] := S;
end;

procedure TReportTable.SetColumnCount(Value: longint);
begin
  SetSize(FRowCount, Value);
end;

procedure TReportTable.SetRowCount(Value: longint);
begin
  SetSize(Value, FColumnCount);
end;

function TReportTable.GetRowAlignment(Index: longint): TVerticalAlignment;
begin
  Result := FRowAlignments[Index];
end;

function TReportTable.GetColumnAlignment(Index: longint): TAlignment;
begin
  Result := FColumnAlignments[Index];
end;

procedure TReportTable.SetRowAlignment(Index: longint; Value: TVerticalAlignment);
begin
  FRowAlignments[Index] := Value;
end;

procedure TReportTable.SetColumnAlignment(Index: longint; Value: TAlignment);
begin
  FColumnAlignments[Index] := Value;
end;

procedure TReportTable.SetSize(aRowCount, aColumnCount: longint);
var
  i: longint;
begin
  SetLength(FRowAlignments, 0);
  SetLength(FColumnAlignments, 0);
  for i := Low(FTable) to High(FTable) do
    SetLength(FTable[i], 0);
  SetLength(FTable, 0);

  FRowCount := aRowCount;
  FColumnCount := aColumnCount;
  if (FRowCount > 0) and (FColumnCount > 0) then
  begin
    SetLength(FTable, FRowCount);
    for i := Low(FTable) to High(FTable) do
    begin
      SetLength(FTable[i], FColumnCount);
    end;

    SetLength(FColumnAlignments, FColumnCount);
    for I := Low(FColumnAlignments) to High(FColumnAlignments) do
    begin
      FColumnAlignments[I] := taLeftJustify;
    end;

    SetLength(FRowAlignments, FRowCount);
    for I := Low(FRowAlignments) to High(FRowAlignments) do
    begin
      FRowAlignments[I] := taVerticalCenter;
    end;
  end;
end;

procedure TReportTable.SetFontName(const Value: string);
begin
  FIsNeededUpdateSize := FFontName <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontName := Value;
  end;
end;

procedure TReportTable.SetFontHeight(const Value: single);
begin
  FIsNeededUpdateSize := FFontHeight <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontHeight := Value;
  end;
end;

procedure TReportTable.SetFontStyle(const Value: TFontStyles);
begin
  FIsNeededUpdateSize := FFontStyle <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontStyle := Value;
  end;
end;

procedure TReportTable.UpdateSize;
var
  i: longint;
  j: longint;
  CellWidthList: array of longint = nil;
  CellHeightList: array of longint = nil;
  CellSize: TSize;
begin
  FBit.FontAntialias  := True;
  FBit.FontQuality    := FFontQuality;
  FBit.FontName       := FFontName;
  FBit.FontStyle      := FFontStyle;
  FBit.FontHeight     := Trunc(FFontHeight * FScale);
  FIsNeededUpdateSize := False;

  FWidth  := Ceil(2*FBorderWidth*FScale);
  FHeight := Ceil(2*FBorderWidth*FScale);
  if (FRowCount > 0) and (FColumnCount > 0) then
  begin
    SetLength(CellHeightList, FRowCount);
    for i := Low(CellHeightList) to High(CellHeightList) do CellHeightList[i] := 0;

    SetLength(CellWidthList, FColumnCount);
    for i := Low(CellWidthList) to High(CellWidthList) do CellWidthList[i] := 0;

    for i := Low(FTable) to High(FTable) do
      for j := Low(FTable[i]) to High(FTable[i]) do
      begin
        CellSize          := FBit.TextSize(GetItem(i, j));
        CellHeightList[i] := Max(CellHeightList[i], CellSize.Height);
        CellWidthList [j] := Max(CellWidthList [j], CellSize.Width );
      end;

    for i := Low(CellHeightList) to High(CellHeightList) do
      Inc(FHeight, CellHeightList[i] + Ceil(FRowSpacer*FScale));

    for i := Low(CellWidthList ) to High(CellWidthList ) do
      Inc(FWidth, CellWidthList[i] + Ceil(FColumnSpacer*FScale));

    CellHeightList := nil;
    CellWidthList := nil;
  end;
end;

function TReportTable.GetWidth: longint;
begin
  if FIsNeededUpdateSize then
  begin
    UpdateSize;
  end;
  result := FWidth;
end;

function TReportTable.GetHeight: longint;
begin
  if FIsNeededUpdateSize then
  begin
    UpdateSize;
  end;
  result := FHeight;
end;

procedure TReportTable.Draw(ACanvas: TCanvas; AWidth, AHeight: longint; AOpaque: boolean);
var
  i: longint;
  j: longint;
  x: array of longint = nil;
  y: array of longint = nil;
  xsum, xoffset: single;
  ysum, yoffset: single;
begin
  if FIsNeededUpdateSize then UpdateSize;
  if FAutosize then
    FBit.SetSize(GetWidth, GetHeight)
  else
    FBit.SetSize(AWidth, AHeight);
  FBit.Fill(FBackgroundColor);

  SetLength(y, fRowCount    + 1);
  SetLength(x, fColumnCount + 1);
  for i := Low(y) to High(y) do y[i] := Trunc(FBorderWidth*FScale);
  for i := Low(x) to High(x) do x[i] := Trunc(FBorderWidth*FScale);

  for i := Low(fTable) to High(fTable) do
  begin
    for j := Low(fTable[i]) to High(fTable[i]) do
    begin
      x[j + 1] := Max(x[j + 1], FBit.TextSize(GetItem(i, j)).Width  + Trunc(FColumnSpacer*FScale));
      y[i + 1] := Max(y[i + 1], FBit.TextSize(GetItem(i, j)).Height + Trunc(FRowSpacer   *FScale));
    end;
  end;

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FPenStyle;

  // Draw horizontal lines
  xsum := 0;
  ysum := 0;
  for i := Low(x) to High(x) do xsum := xsum + x[i];
  for i := Low(y) to High(y) do
  begin
    ysum := ysum + y[i];

    FBit.DrawLineAntialias(
      Trunc(FBorderWidth * FScale),
      ysum,
      xsum,
      ysum,
      FPenColor, FPenWidth * FScale);
  end;

  // Draw vertical lines
  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) do ysum := ysum + y[i];
  for i := Low(x) to High(x) do
  begin
    xsum := xsum + x[i];

    FBit.DrawLineAntialias(
      xsum,
      Trunc(FBorderWidth * FScale),
      xsum,
      ysum,
      FPenColor, FPenWidth * FScale);
  end;

  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) -1 do
  begin
    xsum := 0;
    ysum := ysum + y[i];
    for j := Low(x) to High(x) -1 do
    begin
      xsum := xsum + x[j];

      case FRowAlignments[i] of
        taAlignTop      : yoffset := (FRowSpacer*FScale)*0.5;
        taAlignBottom   : yoffset := (FRowSpacer*FScale)*0.5;
        taVerticalCenter: yoffset := (FRowSpacer*FScale)*0.5;
      end;

      case FColumnAlignments[j] of
        taLeftJustify   : xoffset :=            (FColumnSpacer*FScale)*0.5;
        taRightJustify  : xoffset := (x[j+1]) - (FColumnSpacer*FScale)*0.5;
        taCenter        : xoffset := (x[j+1])/2;
      end;

      FBit.TextOut(
        xsum + xoffset,
        ysum + yoffset,
        FTable[i, j], FFontColor, FColumnAlignments[j]);
    end;
  end;
  x := nil;
  y := nil;

  // Draw
  fBit.InvalidateBitmap;
  fBit.Draw(ACanvas, 0, 0, AOpaque);
end;

// TSpringDrawing

constructor TSpringDrawing.Create;
begin
  inherited Create;
  Fd := 0;
  FDm := 0;
  FLc := 0;
  FLx := 0;
  Fn := 0;
  Fnt1 := 0;
  Fnt2 := 0;
  FPitchF := 0;

  FFontHeight := 12;

  fClockWise := True;
  FGroundEnds := True;
  fClosedEnds := True;
  FCaption := '';
  FScale := 1.0;
  FAutoFit := True;
  FAutoScale := 1.0;
end;

destructor TSpringDrawing.Destroy;
begin
  inherited Destroy;
end;

function TSpringDrawing.xt(const t: double): double;
begin
  if fClockWise then
    Result := (FDm / 2) * System.Cos(2*pi*t + pi)
  else
    Result := (FDm / 2) * System.Cos(2*pi*t);
end;

function TSpringDrawing.yt(const t: double): double;
begin
  Result := (FDm / 2) * System.Sin(2*pi*t);
end;

function TSpringDrawing.zt(const t: double): double;
begin
  Result := 0;
  if (t > 0) and (t <= Fnt2) then
  begin
    Result := t * Fd;
  end;

  if (t > Fnt2) and (t <= (Fnt2 + Fn)) then
  begin
    Result := Fnt2 * Fd + (t - Fnt2) * FPitchF;
  end;

  if (t > (Fnt2 + Fn)) then
  begin
    Result := Fn * FPitchF + (t - Fn) * Fd;
  end;
end;

function TSpringDrawing.PreCheck: boolean;
begin
  Result := True;
  if Fd     <= 0  then Result := False;
  if FDm    <= 0  then Result := False;
  if FLc    <= 0  then Result := False;
  if FLx    < FLc then Result := False;
  if Fn     <= 0  then Result := False;
  if Fnt1   <  0  then Result := False;
  if Fnt2   <  0  then Result := False;
  if FScale <= 0  then Result := False;
end;

procedure TSpringDrawing.DrawInSection(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  DrawInSection(aCanvas);
end;

procedure TSpringDrawing.DrawInProfile(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  DrawInProfile(aCanvas);
end;

function TSpringDrawing.XToCanvas(X: single): single;
begin
  Result := X;
end;

function TSpringDrawing.YToCanvas(Y: single): single;
begin
  Result := FHeight - Y;
end;

procedure TSpringDrawing.DrawInSection(aCanvas: TCanvas);
var
  CenterPosition: double;
  x0, x1: double;
  y0, y1: double;
  alpha, t: double;
  Tex: TBGRABitmap;

  FXMin: longint;
  FYMin: longint;
  FXMax: longint;
  FYMax: longint;

  FXMinF: double;
  FYMinF: double;
  FXMaxF: double;
  FYMaxF: double;
begin
  CenterPosition := FWidth div 2;
  FBit := TBGRABitmap.Create;
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FBackgroundColor);

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FFontName;
  FBit.FontStyle     := FFontStyle;
  FBit.FontHeight    := Trunc(FFontHeight * FScale);
  FBitCharSize       := FBit.TextSize('M');

  FXMin := FSpacer;
  FXMax := FWidth - Trunc(FSpacer*FScale * 0.5);

  FYMin := FBitCharSize.Height + Trunc(FSpacer*FScale * 1.5);
  FYMax := FHeight - Trunc(FSpacer*FScale * 0.5);

  if PreCheck then
  begin
    FPitchF := (Lx - Lc) / Fn + Fd;
    FXMinF  := 0;
    FXMaxF  := FDm + Fd;
    FYMinF  := 0;
    FYMaxF  := zt(Fnt2 + Fn + Fnt1) + Fd;

    if FAutoFit then
    begin
      FAutoScale := Min((FXMax - FXMin)/(FXMaxF - FXMinF), (FYMax - FYMin)/(FYMaxF - FYMinF));
    end;

    x0 := 0;
    y0 := 0;
    t  := -0.5;
    while t < (Fnt2 + Fn + Fnt1) do
    begin
      t := t + 0.5;
      if fClockWise then
        x1 := CenterPosition + xt(t + 0.5) * FAutoScale
      else
        x1 := CenterPosition + xt(t) * FAutoScale;
      y1 := FYMin + (zt(t) + Fd / 2) * FAutoScale;

      Tex := FBit.CreateBrushTexture(bsFDiagonal, FTextureColor, FTextureBackgroundColor,
        Trunc(FTextureWidth*FScale), Trunc(FTextureHeight*FScale), FTexturePenWidth * Min(2, FScale)) as TBGRABitmap;
      FBit.FillEllipseAntialias(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd / 2 * FAutoScale, Fd / 2 * FAutoScale, Tex);
      Tex.Destroy;

      FBit.EllipseAntialias(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd / 2 * FAutoScale,
        Fd / 2 * FAutoScale,
        FPenColor,
        FPenWidth * Min(2, FScale), BGRA(255, 255, 255, 0));

      if (t > 0) and ((t mod 1) > 0) then
      begin
        alpha := arctan2((y1 - y0), -(x1 - x0));

        FBit.DrawLineAntialias(
          XToCanvas(x0 + (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 + (Fd/2) * System.Cos(alpha) * FAutoScale),
          XToCanvas(x1 + (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 + (Fd/2) * System.Cos(alpha) * FAutoScale),
          FPenColor, FPenWidth * Min(2.0, FScale), False);

        FBit.DrawLineAntialias(
          XToCanvas(x0 - (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 - (Fd/2) * System.Cos(alpha) * FAutoScale),
          XToCanvas(x1 - (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 - (Fd/2) * System.Cos(alpha) * FAutoScale),
          FPenColor, FPenWidth * Min(2.0, FScale), False);
      end;
      x0 := x1;
      y0 := y1;
    end;

    // Draw Ends
    x0 := CenterPosition - (FDm + Fd) / 2 * FAutoScale;
    x1 := CenterPosition + (FDm + Fd) / 2 * FAutoScale;
    if FGroundEnds then
      y0 := FYMin + Fd / 2 * FAutoScale
    else
      y0 := FYMin;
    y1 := y0;

    if FGroundEnds then
    begin
      FBit.FillRect(
        Trunc(XToCanvas(x0 - 2)),
        Trunc(YToCanvas(0)),
        Trunc(XToCanvas(x1 + 2)),
        Trunc(YToCanvas(y1)),
        FBackgroundColor);

      FBit.DrawLineAntialias(
        Trunc(XToCanvas(x0)),
        Trunc(YToCanvas(y0)),
        Trunc(XToCanvas(x1)),
        Trunc(YToCanvas(y1)),
        FPenColor, FPenWidth * Min(2, FScale), False);
    end;

    x0 := CenterPosition - (FDm + Fd) / 2 * FAutoScale;
    x1 := CenterPosition + (FDm + Fd) / 2 * FAutoScale;
    if FGroundEnds then
      y0 := FYMin + (zt(Fnt2 + Fn + Fnt1)) * FAutoScale
    else
      y0 := FYMin + (zt(Fnt2 + Fn + Fnt1) + Fd / 2) * FAutoScale;
    y1 := y0;

    if FGroundEnds then
    begin
      FBit.FillRect(
        Trunc(XToCanvas(x0 - 2)),
        Trunc(YToCanvas(fHeight)),
        Trunc(XToCanvas(x1 + 2)),
        Trunc(YToCanvas(y1)),
        FBackgroundColor);

      FBit.DrawLineAntialias(
        Trunc(XToCanvas(x0)),
        Trunc(YToCanvas(y0)),
        Trunc(XToCanvas(x1)),
        Trunc(YToCanvas(y1)),
        FPenColor, FPenWidth * Min(2, FScale), False);
    end;
    // Draw center line
    FBit.PenStyle := psDashDot;
    FBit.DrawLineAntialias(
      XToCanvas(CenterPosition),
      YToCanvas(FYMin),
      XToCanvas(CenterPosition),
      YToCanvas(FYMin + (FYMaxF - FYMinF) * FAutoScale),
      FCenterLineColor, FCenterLineWidth * Min(2, FScale), False);
    // Draw caption
    x0 := CenterPosition;
    y0 := FSpacer*FScale + FBitCharSize.Height;
    FBit.TextOut(
      XToCanvas(x0),
      YToCanvas(y0),
      FCaption, FFontColor, taCenter);
    // Draw
  end;

  FBit.InvalidateBitmap;
  FBit.Draw(aCanvas, 0, 0, True);
  FBit.Destroy;
end;

procedure TSpringDrawing.DrawInProfile(aCanvas: TCanvas);
var
  CenterPosition: double;
  x0, x1: double;
  y0, y1: double;
  nx: double;
  alpha, beta: double;
  ctx: TBGRACanvas2D;

  FXMin: longint;
  FYMin: longint;
  FXMax: longint;
  FYMax: longint;

  FXMinF: double;
  FYMinF: double;
  FXMaxF: double;
  FYMaxF: double;
begin
  CenterPosition := FWidth div 2;
  FBit := TBGRABitmap.Create;
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FBackgroundColor);

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FFontName;
  FBit.FontStyle     := FFontStyle;
  FBit.FontHeight    := Trunc(FFontHeight * FScale);
  FBitCharSize       := FBit.TextSize('M');

  FXMin := FSpacer;
  FXMax := FWidth - Trunc(FSpacer*FScale * 0.5);

  FYMin := FBitCharSize.Height + Trunc(FSpacer*FScale * 1.5);
  FYMax := FHeight - Trunc(FSpacer*FScale * 0.5);

  if PreCheck then
  begin
    FPitchF := (Lx - Lc) / Fn + Fd;
    FXMinF  := 0;
    FXMaxF  := FDm + Fd;
    FYMinF  := 0;
    FYMaxF  := zt(Fnt2 + Fn + Fnt1) + Fd;

    if FAutoFit then
    begin
      FAutoScale :=
        Min((FXMax - FXMin)/(FXMaxF - FXMinF),
            (FYMax - FYMin)/(FYMaxF - FYMinF));
    end;

    nx := 0.0;
    while nx < (Fnt2 + Fn + Fnt1) do
    begin
      x0 := CenterPosition + xt(nx      ) * FAutoScale;
      x1 := CenterPosition + xt(nx + 0.5) * FAutoScale;

      y0 := FYMin + (zt(nx      ) + Fd/2) * FAutoScale;
      y1 := FYMin + (zt(nx + 0.5) + Fd/2) * FAutoScale;

      alpha := arctan2((y1 - y0), -(x1 - x0));

      ctx := FBit.Canvas2D;
      ctx.fillStyle(FTextureBackgroundColor);
      ctx.strokeStyle(FPenColor);
      ctx.lineWidth := FPenWidth * Min(2.0, FScale);
      ctx.beginPath();

      ctx.moveTo(
        XToCanvas(x0 + Fd/2 * System.Sin(alpha) * FAutoScale),
        YToCanvas(y0 + Fd/2 * System.Cos(alpha) * FAutoScale));

      ctx.arc(
        XToCanvas(x0),
        YToCanvas(y0),
        Fd/2 * FAutoScale,
        1.5*pi +alpha,
        0.5*pi +alpha);

      ctx.lineTo(
        XToCanvas(x1 - Fd/2 * System.Sin(alpha) * FAutoScale),
        YToCanvas(y1 - Fd/2 * System.Cos(alpha) * FAutoScale));

      ctx.arc(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd/2 * FAutoScale,
        0.5*pi +alpha,
        1.5*pi +alpha);

      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      if nx = 0.0 then
      begin
        x0 := CenterPosition + xt(0) * FAutoScale;
        y0 := FYMin + (zt(0) + Fd/2) * FAutoScale;

        ctx.beginPath();
        ctx.moveTo(
          XToCanvas(x0),
          YToCanvas(y0 + Fd/2 * FAutoScale));

        ctx.arc(
          XToCanvas(x0),
          YToCanvas(y0),
          Fd/2 * FAutoScale,
          1.5*pi,
          0.5*pi, FClockWise);

        ctx.lineTo(
          XToCanvas(CenterPosition),
          YToCanvas(FYMin));

        ctx.lineTo(
          XToCanvas(CenterPosition),
          YToCanvas(FYMin + Fd*FAutoScale));

        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      end else
      begin
        x0 := CenterPosition + xt(nx - 0.5) * FAutoScale;
        x1 := CenterPosition + xt(nx      ) * FAutoScale;

        y0 := FYMin + (zt(nx - 0.5) + Fd/2) * FAutoScale;
        y1 := FYMin + (zt(nx      ) + Fd/2) * FAutoScale;

        alpha := arctan2((y1 - y0), +(x1 - x0));

        ctx.beginPath();
        ctx.moveTo(
          XToCanvas(x0 + Fd/2 * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 - Fd/2 * System.Cos(alpha) * FAutoScale));

        ctx.arc(
          XToCanvas(x0),
          YToCanvas(y0),
          Fd/2 * FAutoScale,
          0.5*pi -alpha,
          1.5*pi -alpha);

        ctx.lineTo(
          XToCanvas(x1 - Fd/2 * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 + Fd/2 * System.Cos(alpha) * FAutoScale));

        ctx.arc(
          XToCanvas(x1),
          YToCanvas(y1),
          Fd/2 * FAutoScale,
          1.5*pi -alpha,
          0.5*pi -alpha);

        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      end;
      nx := nx + 1.0;
    end;

    if nx > 0 then
    begin
      x0 := CenterPosition + xt(nx - 0.5) * FAutoScale;
      y0 := FYMin + (zt(nx - 0.5) + Fd/2) * FAutoScale;

      ctx.beginPath();
      ctx.moveTo(
        XToCanvas(x0),
        YToCanvas(y0 - Fd/2 * FAutoScale));

      ctx.arc(
        XToCanvas(x0),
        YToCanvas(y0),
        Fd/2 * FAutoScale,
        0.5*pi,
        1.5*pi, FClockWise);

      ctx.lineTo(
        XToCanvas(CenterPosition),
        YToCanvas(y0 + Fd/2 * FAutoScale));

      ctx.lineTo(
        XToCanvas(CenterPosition),
        YToCanvas(y0 - Fd/2 * FAutoScale));

      ctx.closePath();
      ctx.fill();
      ctx.stroke();
    end;
    if FGroundEnds then
    begin




    end;
    // Draw center line
    FBit.PenStyle := psDashDot;
    FBit.DrawLineAntialias(
      XToCanvas(CenterPosition),
      YToCanvas(FYMin),
      XToCanvas(CenterPosition),
      YToCanvas(FYMin + (FYMaxF - FYMinF) * FAutoScale),
      FCenterLineColor, FCenterLineWidth * Min(2, FScale), False);
    // Draw caption
    x0 := CenterPosition;
    y0 := FSpacer*FScale + FBitCharSize.Height;
    FBit.TextOut(
      XToCanvas(x0),
      YToCanvas(y0),
      FCaption, FFontColor, taCenter);
  end;

  FBit.InvalidateBitmap;
  FBit.Draw(aCanvas, 0, 0, True);
  FBit.Destroy;
end;

// DrawLogo

procedure DrawLogo(aCanvas: TCanvas; aWidth, aHeight: longint);
var
  Bit: TBGRABitmap;
  x, y: longint;
begin
  Bit := TBGRABitmap.Create;
  Bit.SetSize(aWidth, aHeight);
  Bit.Fill(BGRA(127, 127, 127, 255));

  Bit.FontHeight := 30;
  Bit.FontAntialias := True;
  Bit.FontStyle := [fsBold];

  x := -50;
  while x < (aWidth + 50) do
  begin
    y := -50;
    while y < (aHeight + 50) do
    begin
      Bit.TextOutAngle(x, y, 250, ApplicationName, BGRA(150, 150, 150, 255), taLeftJustify);
      Inc(y, Bit.TextSize(ApplicationName).Height + DefaultSpacer div 4);
    end;
    Inc(x, Bit.TextSize(ApplicationName).Width + DefaultSpacer div 4);
  end;

  Bit.InvalidateBitmap;
  Bit.Draw(aCanvas, 0, 0);
  Bit.Destroy;
end;

end.
