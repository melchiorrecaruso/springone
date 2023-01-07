{
  EN13906 Helical Compression Spring Designer

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

unit GraphBase;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, BGRATextFX, Classes,
  Graphics, IniFiles, Math, SysUtils, UtilsBase;

type
  TChart = class
  private
    FBit: TBGRABitmap;
    FColor: TBGRAPixel;
    FBackgroundColor: TBGRAPixel;

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
    FSpacer: longint;
    FZoom: single;

    FIsNeededCalcXMaxF: boolean;
    FIsNeededCalcXMinF: boolean;
    FIsNeededCalcYMaxF: boolean;
    FIsNeededCalcYMinF: boolean;
    FIsNeededCalcXDeltaF: boolean;
    FIsNeededCalcYDeltaF: boolean;
    FIsNeededCalcXCount: boolean;
    FIsNeededCalcYCount: boolean;

    procedure CalculareChartArea;
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
    constructor Create(aWidth, aHeight: longint);
    destructor Destroy; override;

    procedure AddPolyLine(const aPoints: ArrayOfTPointF; aExtend: boolean; const aCaption: string);
    procedure AddPolygon(const aPoints: ArrayOfTPointF; const aCaption: string);

    procedure AddLabel(aX, aY: single; aShiftX, aShiftY: longint; aAlign: TAlignment;
      aVertAlign: TVerticalAlignment; const aCaption: string);

    procedure AddDotLabel(aX, aY, aRadius: single; aShiftX, aShiftY: longint;
      aAlign: TAlignment; aVertAlign: TVerticalAlignment; const aCaption: string);

    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
    procedure Draw(aCanvas: TCanvas);
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

    property Color: TBGRAPixel read FColor write FColor;
    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;

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

    property Spacer: longint read FSpacer write FSpacer;
    property Width: longint read FWidth write FWidth;
    property Height: longint read FHeight write FHeight;
    property Zoom: single read FZoom write FZoom;
  end;


  TCustomGraph = class
  private
    fBitmap: TBGRABitmap;
    fBitmapColor: TBGRAPixel;
    fSection: string;
    fSetting: TIniFile;
    fSpacer: longint;
    fHeight: longint;
    fWidth: longint;
    fZoom: double;
    function CartesianToCanvasX(const x: double): double;
    function CartesiantoCanvasY(const y: double): double;
    procedure LoadFont(Index: string; var FontColor: TBGRAPixel);
    procedure LoadLine(Index: string; var PenColor: TBGRAPixel; var PenWidth: double);
    //procedure SaveFont(Index: string; const FontColor: TBGRAPixel);
    //procedure SaveLine(Index: string; const PenColor: TBGRAPixel; const PenWidth: double);
    procedure SetSpacer(const Value: longint); virtual;
    procedure SetZoom(const Value: double);
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    destructor Destroy; override;
  public
    property Spacer: longint read fSpacer write SetSpacer;
    property Zoom: double read fZoom write SetZoom;
  end;




  TReportTable = class(TCustomGraph)
  private
    fRowCount: longint;
    fColumnCount: longint;
    fHorizontalSpacer: longint;
    fTable: array of array of string;
    fVerticalSpacer: longint;
    fHorizontalAlignment: longint;
    fVerticalAlignment: longint;
    function GetItem(Row, Column: longint): string;
    procedure SetItem(Row, Column: longint; const S: string);
    procedure SetColumnCount(Value: longint);
    procedure SetRowCount(Value: longint);
    procedure SetSpacer(const Value: longint); override;
    procedure SetSize(aRowCount, aColumnCount: longint);
    procedure SetHorizontalAlignment(Value: longint);
    procedure SetVerticalAlignment(Value: longint);
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    destructor Destroy; override;
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
    function Height: longint;
    function Width: longint;
  public
    property RowCount: longint read fRowCount write SetRowCount;
    property ColumnCount: longint read fColumnCount write SetColumnCount;
    property Items[Row, Column: longint]: string read GetItem write SetItem; default;
    property HorizontalAlignment: longint read fHorizontalAlignment
      write SetHorizontalAlignment;
    property VerticalAlignment: longint read fVerticalAlignment
      write SetVerticalAlignment;
  end;

  TSectionSpringDrawing = class(TCustomGraph)
  private
    fd: double;
    fDm: double;
    fClockWise: boolean;
    fLc: double;
    fLx: double;
    fn: double;
    fnt1: double;
    fnt2: double;
    fpitch: double;
    fClosedEnds: boolean;
    fGroundEnds: boolean;
    fScale: double;
    fText: string;
    fFit: boolean;
    function xt(const t: double): double;
    function yt(const t: double): double;
    function zt(const t: double): double;
    function PreCheck: boolean;
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    destructor Destroy; override;
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property d: double read fd write fd;
    property Dm: double read fDm write fDm;
    property Lc: double read fLc write fLc;
    property Lx: double read fLx write fLx;
    property n: double read fn write fn;
    property nt1: double read fnt1 write fnt1;
    property nt2: double read fnt2 write fnt2;
    property ClockWise: boolean read fClockWise write fClockWise;
    property GroundEnds: boolean read fGroundEnds write fGroundEnds;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;

    property Text: string read fText write fText;
    property Fit: boolean read fFit write fFit;
  end;


procedure DrawLogo(aCanvas: TCanvas; aWidth, aHeight: longint);


const
  DefaultSpacer = 16;


implementation

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

function GetDelta(Count: longint; var Range: double): double;
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
  Range := Result * Count;
end;

function GetDelta2(Count: longint; const Range: single): single;
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
  begin
    Clear;
  end;
end;

constructor TChart.Create(aWidth, aHeight: longint);
begin
  inherited Create;
  FWidth  := aWidth;
  FHeight := aHeight;
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
  FWidth   := 0;
  FHeight  := 0;

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

  FIsNeededCalcXMaxF   := True;
  FIsNeededCalcXMinF   := True;
  FIsNeededCalcYMaxF   := True;
  FIsNeededCalcYMinF   := True;
  FIsNeededCalcXDeltaF := True;
  FIsNeededCalcYDeltaF := True;
  FIsNeededCalcXCount  := True;
  FIsNeededCalcYCount  := True;

  FItems   := TList.Create;
  FSpacer  := DefaultSpacer;
  FZoom    := 1.0;

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

  Item.FCaption     := ACaption;
  Item.FFontName    := FCurrentFontName;
  Item.FFontHeight  := FCurrentFontHeight;
  Item.FFontColor   := FCurrentFontColor;
  Item.FFontStyle   := FCurrentFontStyle;
  Item.FPenColor    := FCurrentPenColor;
  Item.FPenStyle    := FCurrentPenStyle;
  Item.FPenWidth    := FCurrentPenWidth;
  Item.FExtend      := aExtend;
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

procedure TChart.CalculareChartArea;
var
  i, j: longint;
  Item: TChartItem;
begin
  if FIsNeededCalcXMinF then
  begin
    FXMinF := + MaxSingle;
    for i := 0 to FItems.Count -1 do
    begin
      Item := TChartItem(FItems[i]);
      if (Item is TChartPolyLineItem) then
      begin
        for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
        begin
          FXMinF := Min(FXMinF, TChartPolyLineItem(Item).FPoints[j].x);
        end;
      end;
    end;
    FXMinF := GetMin(FXMinF);
  end;

  if FIsNeededCalcXMaxF then
  begin
    FXMaxF := - MaxSingle;
    for i := 0 to FItems.Count -1 do
    begin
      Item := TChartItem(FItems[I]);
      if (Item is TChartPolyLineItem) then
      begin
        for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
        begin
          FXMaxF := Max(FXMaxF, TChartPolyLineItem(Item).FPoints[j].x);
        end;
      end;
    end;
  end;

  if FIsNeededCalcYMinF then
  begin
    FYMinF := + MaxSingle;
    for i := 0 to FItems.Count -1 do
    begin
      Item := TChartItem(FItems[I]);
      if (Item is TChartPolyLineItem) then
      begin
        for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
        begin
          FYMinF := Min(FYMinF, TChartPolyLineItem(Item).FPoints[j].y);
        end;
      end;
    end;
    FYMinF := GetMin(FYMinF);
  end;

  if FIsNeededCalcYMaxF then
  begin
    FYMaxF := - MaxSingle;
    for i := 0 to FItems.Count -1 do
    begin
      Item := TChartItem(FItems[I]);
      if (Item is TChartPolyLineItem) then
      begin
        for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
        begin
          FYMaxF := Max(FYMaxF, TChartPolyLineItem(Item).FPoints[j].y);
        end;
      end;
    end;
  end;
end;

procedure TChart.DrawGrid;
var
  I: longint;
  X, XShift, XSpacing: double;
  Y, YShift, YSpacing: double;
begin
  XShift   := -(FSpacer * FZoom) * 0.5;
  YShift   := -(FSpacer * FZoom) * 0.5;
  XSpacing := +(FXMax - FXMin) / FXCount;
  YSpacing := +(FYMax - FYMin) / FYCount;
  // Draw background color
  FBit.FillRectAntialias(
    XToCanvas(FXMin),
    YToCanvas(FYMax),
    XToCanvas(FXMax),
    YToCanvas(FYMin),
    FBackgroundColor);
  // Draw Y secondary axis and X labels
  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FXAxisFontName;
  FBit.FontStyle     := FXAxisFontStyle;
  FBit.FontHeight    := Trunc(FXAxisFontHeight * FZoom);

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FYGridLineStyle;

  for I := 0 to FXCount - 1 do
  begin
    X := FXMin + XSpacing * I;
    DrawLine(X, FYMin, X, FYMax, FYGridLineColor, FYGridLineWidth * FZoom);
    DrawText(X, FYMin + YShift, TryFloatToText(FXMinF + FXDeltaF * I), FXAxisFontColor, taCenter, taAlignTop);
  end;
  DrawLine(FXMax, FYMin, FXMax, FYMax, FYGridLineColor, FYGridLineWidth * FZoom);
  DrawText(FXMax, FYMin + YShift, FXAxisLabel, FXAxisFontColor, taCenter, taAlignTop);
  // Draw X secondary axis and Y labels
  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FYAxisFontName;
  FBit.FontStyle     := FYAxisFontStyle;
  FBit.FontHeight    := Trunc(FYAxisFontHeight * FZoom);

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FXGridLineStyle;

  for I := 0 to FYCount - 1 do
  begin
    Y := FYMin + YSpacing * I;
    DrawLine(FXMin, Y, FXMax, Y, FXGridLineColor, FXGridLineWidth * FZoom);
    DrawText(FXMin + XShift, Y, TryFloatToText(FYMinF + FYDeltaF * I), FYAxisFontColor, taRightJustify, taVerticalCenter);
  end;
  DrawLine(FXMin, FYMax, FXMax, FYMax, FXGridLineColor, FXGridLineWidth * FZoom);
  DrawText(FXMin + XShift, FYMax, FYAxisLabel, FYAxisFontColor, taRightJustify, taVerticalCenter);
  // Draw Chart Title
  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FTitleFontName;
  FBit.FontStyle     := FTitleFontStyle;
  FBit.FontHeight    := Trunc(FTitleFontHeight * fZoom);

  YShift := (FSpacer * FZoom) * 0.5;
  DrawText((FXMin + FXMax) * 0.5, FYMax + YShift, FTitle, FTitleFontColor, taCenter, taAlignBottom);
end;

procedure TChart.DrawLine(x0, y0, x1, y1: single; aPenColor: TBGRAPixel; aPenWidth: single);
begin
  fBit.DrawLineAntialias(
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
begin
  case AAlign of
    taLeftJustify:    ShiftX := 0;
    taRightJustify:   ShiftX := - FBit.TextSize(AText).Width;
    taCenter:         ShiftX := - FBit.TextSize(AText).Width / 2;
  end;

  case AVertAlign of
    taAlignTop:       ShiftY := 0;
    taAlignBottom:    ShiftY := + FBit.TextSize(AText).Height;
    taVerticalCenter: ShiftY := + FBit.TextSize(AText).Height / 2;
  end;

  FBit.TextOut(XToCanvas(X + ShiftX), YToCanvas(Y + ShiftY), AText, ATextColor, taLeftJustify);
end;

function TChart.GetLegendSize: TSize;
var
  I: longint;
  Item: TChartItem;
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
        FBit.FontQuality   := fqSystemClearType;
        FBit.FontName      := Item.FFontName;
        FBit.FontHeight    := Trunc(Item.FFontHeight * FZoom);
        FBit.FontStyle     := Item.FFontStyle;

        Result.Width  := Max(Result.Width,  FBit.TextSize(Item.FCaption).Width);
        Result.Height := Max(Result.Height, FBit.TextSize(Item.FCaption).Height);
      end;
    end;
  end;

  if Result.Width > 0 then
  begin
    Result.Width := Result.Width + Trunc(FSpacer * FZoom * 2.5 + FLegendLineLength * FZoom);
  end;

  if Result.Height > 0 then
  begin
    Result.Height := Result.Height + Trunc(FSpacer * FZoom);
  end;
end;

procedure TChart.DrawLegend;
var
  I: longint;
  Item: TChartItem;
  X, Y: single;
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
          XToCanvas(X + FSpacer * FZoom),
          YToCanvas(Y),
          XToCanvas(X + FSpacer * FZoom + FLegendLineLength * FZoom),
          YToCanvas(Y),
          TChartPolyLineItem(Item).FPenColor,
          TChartPolyLineItem(Item).FPenWidth * FZoom);

        FBit.FontAntialias := True;
        FBit.FontQuality   := fqSystemClearType;
        FBit.FontName      := Item.FFontName;
        FBit.FontHeight    := Trunc(Item.FFontHeight * FZoom);
        FBit.FontStyle     := Item.FFontStyle;
        DrawText(X + (FSpacer * FZoom * 1.5) + FLegendLineLength * FZoom, Y,
          Item.FCaption, Item.FFontColor, taLeftJustify, taVerticalCenter);

        Y := Y - FBit.TextSize(Item.FCaption).Height - (FSpacer * FZoom) * 0.25;
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
          Trunc(FTextureWidth*FZoom), Trunc(FTextureHeight*FZoom), FTexturePenWidth * FZoom) as TBGRABitmap;
        FBit.FillPolyAntialias(T, Tex);
        FBit.DrawPolygonAntialias(T, FPenColor, FPenWidth * FZoom, BGRA(255, 255, 255, 0));
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
        FBit.DrawPolyLineAntialias(T, FPenColor, FPenWidth * FZoom, BGRA(255, 255, 255, 0));
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
          FRadius*FZoom,
          FRadius*FZoom,
          FPenColor,
          FPenWidth,
          FPenColor);

        FBit.FontAntialias := True;
        FBit.FontQuality   := fqSystemClearType;
        FBit.FontName      := FFontName;
        FBit.FontHeight    := Trunc(FFontHeight * FZoom);
        FBit.FontStyle     := FFontStyle;

        DrawText(
          FXMin + (FX - FXMinF) * FXScaleF + FShiftX*FZoom,
          FYMin + (FY - FYMinF) * FYScaleF + FShiftY*FZoom,
          FCaption, FFontColor, FAlign, FVertAlign);
      end;
    end else
    if Item is TChartLabelItem then
    begin
      with Item as TChartLabelItem do
      begin
        FBit.FontAntialias := True;
        FBit.FontQuality   := fqSystemClearType;
        FBit.FontName      := FFontName;
        FBit.FontHeight    := Trunc(FFontHeight * FZoom);
        FBit.FontStyle     := FFontStyle;

        DrawText(
          FXMin + (FX - FXMinF) * FXScaleF + FShiftX*FZoom,
          FYMin + (FY - FYMinF) * FYScaleF + FShiftY*FZoom,
          FCaption, FFontColor, FAlign, FVertAlign);
      end;
    end;
  end;
end;

procedure TChart.Draw(aCanvas: TCanvas);
var
  i, j: longint;
  Item: TChartItem;
  maxXLabelWidth:  longint = 0;
  maxYLabelWidth:  longint = 0;
  maxXLabelHeight: longint = 0;
  maxYLabelHeight: longint = 0;
  maxTitleWidth:   longint = 0;
  maxTitleHeight:  longint = 0;
begin
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FColor);

  CalculareChartArea;

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FXAxisFontName;
  FBit.FontStyle     := FXAxisFontStyle;
  FBit.FontHeight    := Trunc(FXAxisFontHeight * FZoom);

  maxXLabelWidth  := Max(FBit.TextSize(FXAxisLabel).Width,  FBit.TextSize(TryFloatToText(FXMaxF)).Width ) + Trunc(FSpacer * FZoom);
  maxXLabelHeight := Max(FBit.TextSize(FXAxisLabel).Height, FBit.TextSize(TryFloatToText(FXMaxF)).Height) + Trunc(FSpacer * FZoom);
  maxYLabelWidth  := Max(FBit.TextSize(FYAxisLabel).Width,  FBit.TextSize(TryFloatToText(FYMaxF)).Width ) + Trunc(FSpacer * FZoom);
  maxYLabelHeight := Max(FBit.TextSize(FYAxisLabel).Height, FBit.TextSize(TryFloatToText(FYMaxF)).Height) + Trunc(FSpacer * FZoom);

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FTitleFontName;
  FBit.FontStyle     := FTitleFontStyle;
  FBit.FontHeight    := Trunc(FTitleFontHeight * FZoom);

  maxTitleWidth  := FBit.TextSize(FTitle).Width  + Trunc(FSpacer * FZoom);
  maxTitleHeight := FBit.TextSize(FTitle).Height + Trunc(FSpacer * FZoom);

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
      if FIsNeededCalcXDeltaF then FXDeltaF := GetDelta2(FXCount, FXMaxF - FXMinF);
      if FIsNeededCalcYDeltaF then FYDeltaF := GetDelta2(FYCount, FYMaxF - FYMinF);

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
  DrawLine(FXMin, FYMin, FXMax, FYmin, FXAxisLineColor, FXAxisLineWidth * FZoom);

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FYAxisLineStyle;
  DrawLine(FXMin, FYMin, FXMin, FYMax, FYAxisLineColor, FYAxisLineWidth * FZoom);

  FBit.InvalidateBitmap;
  FBit.Draw(aCanvas, 0, 0, False);
end;

procedure TChart.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  Draw(aCanvas);
end;

procedure TChart.SetXMaxF(Value: single);
begin
  FXMaxF := Value;
  FIsNeededCalcXMaxF := False;
end;

procedure TChart.SetXMinF(Value: single);
begin
  FXMinF := Value;
  FIsNeededCalcXMinF := False;
end;

procedure TChart.SetYMaxF(Value: single);
begin
  FYMaxF := Value;
  FIsNeededCalcYMaxF := False;
end;

procedure TChart.SetYMinF(Value: single);
begin
  FYMinF := Value;
  FIsNeededCalcYMinF := False;
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


// TCustomGraph

constructor TCustomGraph.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create;
  fBitmap := TBGRABitmap.Create;
  fBitmap.FontRenderer := TBGRATextEffectFontRenderer.Create;

  fHeight  := 0;
  fWidth   := 0;
  fZoom    := 1.0;
  fSection := aSection;
  fSetting := aSetting;
  fSpacer  := DefaultSpacer;

  fBitmapColor.FromString(fSetting.ReadString('Custom', 'BackgroundColor', 'White'));
end;

destructor TCustomGraph.Destroy;
begin
  fBitmap.Destroy;
  inherited Destroy;
end;

function TCustomGraph.CartesianToCanvasX(const X: double): double;
begin
  Result := X;
end;

function TCustomGraph.CartesianToCanvasY(const Y: double): double;
begin
  Result := fHeight - Y;
end;

procedure TCustomGraph.LoadFont(Index: string; var FontColor: TBGRAPixel);
var
  FontStyle: string;
begin
  fBitmap.FontStyle := [];
  fBitmap.FontAntialias := True;
  fBitmap.FontQuality := fqSystemClearType;
  fBitmap.FontName := fSetting.ReadString(fSection,
    Format('Font %s Name', [Index]), 'Courier New');
  fBitmap.FontHeight := Trunc(fSetting.ReadInteger(fSection,
    Format('Font %s Height', [Index]), 16) * fZoom);

  FontStyle := fSetting.ReadString(fSection, Format('Font %s Style', [Index]), '');
  if Pos('Bold', FontStyle) > 0 then Include(fBitmap.FontStyle, fsBold);
  if Pos('Italic', FontStyle) > 0 then Include(fBitmap.FontStyle, fsItalic);
  if Pos('Underline', FontStyle) > 0 then Include(fBitmap.FontStyle, fsUnderline);
  if Pos('StrikeOut', FontStyle) > 0 then Include(fBitmap.FontStyle, fsStrikeOut);

  FontColor.FromString(fSetting.ReadString(fSection,
    Format('Font %s Color', [index]), 'Black'));
end;

(*
procedure TCustomGraph.SaveFont(Index: string; const FontColor: TBGRAPixel);
var
  FontStyle: string;
begin
  fSetting.WriteString (fSection, Format('Font %s Name',   [Index]), fBitmap.FontName);
  fSetting.WriteInteger(fSection, Format('Font %s Height', [Index]), fBitmap.FontHeight);

  FontStyle := '';
  if fsBold      in fBitmap.FontStyle then FontStyle := FontStyle + 'Bold ';
  if fsItalic    in fBitmap.FontStyle then FontStyle := FontStyle + 'Italic ';
  if fsUnderline in fBitmap.FontStyle then FontStyle := FontStyle + 'Underline ';
  if fsStrikeOut in fBitmap.FontStyle then FontStyle := FontStyle + 'StrikeOut ';

  fSetting.WriteString(fSection, Format('Font %s Style', [index]), FontStyle         );
  fSetting.WriteString(fSection, Format('Font %s Color', [index]), FontColor.ToString);
end;
*)

procedure TCustomGraph.LoadLine(Index: string; var PenColor: TBGRAPixel;
  var PenWidth: double);
var
  PenStyle: string;
begin
  fBitmap.JoinStyle := pjsRound;
  fBitmap.LineCap   := pecRound;

  PenStyle := fSetting.ReadString(fSection,
    Format('Line %s Style', [index]), 'Solid');
  PenWidth := TryTextToFloat(fSetting.ReadString(fSection,
    Format('Line %s Width', [index]), '1.0')) * fZoom;

  if PenStyle = 'Solid' then fBitmap.PenStyle := psSolid;
  if PenStyle = 'Dash' then fBitmap.PenStyle := psDash;
  if PenStyle = 'Dot' then fBitmap.PenStyle := psDot;
  if PenStyle = 'DashDot' then fBitmap.PenStyle := psDashDot;
  if PenStyle = 'DashDotDot' then fBitmap.PenStyle := psDashDotDot;
  if PenStyle = 'InsideFrame' then fBitmap.PenStyle := psInsideFrame;
  if PenStyle = 'Pattern' then fBitmap.PenStyle := psPattern;
  if PenStyle = 'Clear' then fBitmap.PenStyle := psClear;

  PenColor.FromString(fSetting.ReadString(fSection,
    Format('Line %s Color', [index]), 'Black'));
end;

(*
procedure TCustomGraph.SaveLine(Index: string; const PenColor: TBGRAPixel; const PenWidth: double);
begin
  if fBitmap.PenStyle = psSolid       then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'Solid');
  if fBitmap.PenStyle = psDash        then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'Dash');
  if fBitmap.PenStyle = psDot         then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'Dot');
  if fBitmap.PenStyle = psDashDot     then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'DashDot');
  if fBitmap.PenStyle = psDashDotDot  then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'DashDotDot');
  if fBitmap.PenStyle = psInsideFrame then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'InsideFrame');
  if fBitmap.PenStyle = psPattern     then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'Pattern');
  if fBitmap.PenStyle = psClear       then fSetting.WriteString(fSection, Format('Line %s Style', [index]), 'Clear');

  fSetting.WriteString (fSection, Format('Line %s Color', [index]), PenColor.ToString);
  fSetting.WriteFloat  (fSection, Format('Line %s Width', [index]), PenWidth);
end;
*)

procedure TCustomGraph.SetZoom(const Value: double);
begin
  fZoom := Value;
end;

procedure TCustomGraph.SetSpacer(const Value: longint);
begin
  fSpacer := Value;
end;



// TReportTable

constructor TReportTable.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fHorizontalAlignment := 0;
  fHorizontalSpacer := fSpacer;
  fVerticalAlignment := 1;
  fVerticalSpacer := 0;
  SetSize(0, 0);
end;

destructor TReportTable.Destroy;
begin
  SetSize(0, 0);
  inherited Destroy;
end;

function TReportTable.GetItem(Row, Column: longint): string;
begin
  Result := fTable[Row][Column];
end;

procedure TReportTable.SetItem(Row, Column: longint; const S: string);
begin
  fTable[Row][Column] := S;
end;

procedure TReportTable.SetColumnCount(Value: longint);
begin
  SetSize(fRowCount, Value);
end;

procedure TReportTable.SetRowCount(Value: longint);
begin
  SetSize(Value, fColumnCount);
end;

procedure TReportTable.SetSpacer(const Value: longint);
begin
  inherited SetSpacer(Value);
  fHorizontalSpacer := fSpacer;
  fVerticalSpacer := 0;
end;

procedure TReportTable.SetSize(aRowCount, aColumnCount: longint);
var
  i: longint;
begin
  for i := Low(fTable) to High(fTable) do
    SetLength(fTable[i], 0);
  SetLength(fTable, 0);

  fRowCount := aRowCount;
  fColumnCount := aColumnCount;
  if (fRowCount > 0) and (fColumnCount > 0) then
  begin
    SetLength(fTable, fRowCount);
    for i := Low(fTable) to High(fTable) do
    begin
      SetLength(fTable[i], fColumnCount);
    end;
  end;
end;

function TReportTable.Height: longint;
var
  i: longint;
  FontColor: TBGRAPixel;
begin
  Result := 1;

  LoadFont('2nd', FontColor);
  for i := Low(fTable) to High(fTable) do
  begin
    Result := Result + fBitmap.TextSize('FF').Height + fVerticalSpacer;
  end;
end;

function TReportTable.Width: longint;
var
  i: longint;
  j: longint;
  x: array of longint = nil;
  FontColor: TBGRAPixel;
begin
  LoadFont('2nd', FontColor);

  SetLength(x, fColumnCount);
  for i := Low(x) to High(x) do x[i] := 0;

  for i := Low(fTable) to High(fTable) do
  begin
    for j := Low(fTable[i]) to High(fTable[i]) do
    begin
      x[j] := Max(x[j], fBitmap.TextSize(GetItem(i, j)).Width + fHorizontalSpacer);
    end;
  end;

  Result := 1;
  for i := Low(x) to High(x) do
  begin
    Result := Result + x[i];
  end;
  x := nil;
end;

procedure TReportTable.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
var
  i: longint;
  j: longint;
  xoffset: longint;
  yoffset: longint;
  x: array of longint = nil;
  y: array of longint = nil;
  xsum: longint;
  ysum: longint;
  TextColor: TBGRAPixel;
  PenColor: TBGRAPixel;
  PenWidth: double;
begin
  fBitmap.SetSize(aWidth, aHeight);
  fBitmap.Fill(fBitmapColor);

  case fHorizontalAlignment of
    0: xoffset := 0;
    1: xoffset := (aWidth - Width) div 2;
    2: xoffset := (aWidth - Width);
    else
      xoffset := 0;
  end;

  case fVerticalAlignment of
    0: yoffset := 0;
    1: yoffset := (aHeight - Height) div 2;
    2: yoffset := (aHeight - Height);
    else
      yoffset := 0;
  end;

  SetLength(y, fRowCount + 1);
  SetLength(x, fColumnCount + 1);
  for i := Low(x) to High(x) do x[i] := xoffset;
  for i := Low(y) to High(y) do y[i] := yoffset;

  LoadFont('2nd', TextColor);
  for i := Low(fTable) to High(fTable) do
  begin
    for j := Low(fTable[i]) to High(fTable[i]) do
    begin
      x[j + 1] := Max(x[j + 1], fBitmap.TextSize(GetItem(i, j)).Width +
        fHorizontalSpacer);
    end;
    y[i + 1] := fBitmap.TextSize('FF').Height + fVerticalSpacer;
  end;

  LoadLine('1st', PenColor, PenWidth);
  xsum := 0;
  ysum := 0;
  for i := Low(x) to High(x) do xsum := xsum + x[i];
  for i := Low(y) to High(y) do
  begin
    ysum := ysum + y[i];
    fBitmap.DrawLineAntialias(
      xoffset,
      ysum,
      xsum,
      ysum,
      PenColor, PenWidth);
  end;

  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) do ysum := ysum + y[i];
  for i := Low(x) to High(x) do
  begin
    xsum := xsum + x[i];
    fBitmap.DrawLineAntialias(
      xsum,
      yoffset,
      xsum,
      ysum,
      PenColor, PenWidth);
  end;

  LoadFont('2nd', TextColor);
  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) do
  begin
    xsum := 0;
    ysum := ysum + y[i];
    for j := Low(x) to High(x) do
    begin
      xsum := xsum + x[j];
      if (i < fRowCount) and (j < fColumnCount) then
      begin
        fBitmap.TextOut(
          xsum + fHorizontalSpacer div 2,
          ysum + fVerticalSpacer div 2,
          fTable[i, j], TextColor, taLeftJustify);
      end;
    end;
  end;
  x := nil;
  y := nil;
  // Draw
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
end;

procedure TReportTable.SetHorizontalAlignment(Value: longint);
begin
  if (Value >= 0) and (Value <= 2) then
  begin
    fHorizontalAlignment := Value;
  end;
end;

procedure TReportTable.SetVerticalAlignment(Value: longint);
begin
  if (Value >= 0) and (Value <= 2) then
  begin
    fVerticalAlignment := Value;
  end;
end;

// TSectionSpringDrawing

constructor TSectionSpringDrawing.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fd := 0;
  fDm := 0;
  fLc := 0;
  fLx := 0;
  fn := 0;
  fnt1 := 0;
  fnt2 := 0;
  fpitch := 0;
  fScale := 1;
  fClockWise := True;
  fGroundEnds := True;
  fClosedEnds := True;
  fText := '';
  fScale := 1.0;
  fFit := True;
end;

destructor TSectionSpringDrawing.Destroy;
begin
  inherited Destroy;
end;

function TSectionSpringDrawing.xt(const t: double): double;
begin
  Result := (fDm / 2) * cos(2 * pi * t);
end;

function TSectionSpringDrawing.yt(const t: double): double;
begin
  Result := (fDm / 2) * sin(2 * pi * t);
end;

function TSectionSpringDrawing.zt(const t: double): double;
begin
  Result := 0;
  if (t > 0) and (t <= fnt2) then
  begin
    Result := t * fd;
  end;

  if (t > fnt2) and (t <= (fnt2 + fn)) then
  begin
    Result := fnt2 * fd + (t - fnt2) * fpitch;
  end;

  if (t > (fnt2 + fn)) then
  begin
    Result := fnt2 * fd + fn * fpitch + (t - (fnt2 + fn)) * fd;
  end;
end;

function TSectionSpringDrawing.PreCheck: boolean;
begin
  Result := True;
  if fd <= 0 then Result := False;
  if fDm <= 0 then Result := False;
  if fLc <= 0 then Result := False;
  if fLx < fLc then Result := False;
  if fn <= 0 then Result := False;
  if fnt1 <= 0 then Result := False;
  if fnt2 <= 0 then Result := False;
  //if fpitch <= 0   then Result := False;
  if fScale <= 0 then Result := False;
end;

procedure TSectionSpringDrawing.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
var
  x0, x1: double;
  y0, y1: double;
  OffSet: double;
  TexColor: TBGRAPixel;
  TextColor: TBGRAPixel;
  TexWidth: double;
  PenColor: TBGRAPixel;
  PenWidth: double;
  Tex: TBGRABitmap;
  t: double;

  fCANVAS_X_MIN: longint;
  fCANVAS_Y_MIN: longint;
  fCANVAS_X_RANGE: longint;
  fCANVAS_Y_RANGE: longint;
  fX_MIN: double;
  fX_MAX: double;
  fYMin: double;
  fY_MAX: double;
begin
  fWidth := aWidth;
  fHeight := aHeight;
  OffSet := fWidth div 2;

  fBitmap.SetSize(fWidth, fHeight);
  fBitmap.Fill(fBitmapColor);

  LoadFont('2nd', TextColor);
  fCANVAS_X_MIN := fSpacer;
  fCANVAS_Y_MIN := fSpacer div 2 + fBitmap.TextSize('FF').Height + fSpacer;
  fCANVAS_X_RANGE := (aWidth - fSpacer) - fCANVAS_X_MIN;
  fCANVAS_Y_RANGE := (aHeight - fSpacer) - fCANVAS_Y_MIN;

  if PreCheck then
  begin
    fpitch := fd + (Lx - Lc) / fn;
    fX_MIN := 0;
    fX_MAX := fDm + fd;

    fYMin := 0;
    fY_MAX := zt(fnt2 + fn + fnt1) + fd;

    if fFit then
      fScale := Min(fCANVAS_X_RANGE / fX_MAX, fCANVAS_Y_RANGE / fY_MAX);

    LoadLine('Tex', TexColor, TexWidth);
    x0 := 0;
    y0 := 0;

    t := -0.5;
    while t < (fnt2 + fn + fnt1) do
    begin
      t := t + 0.5;
      if fClockWise then
        x1 := OffSet + xt(t) * fScale
      else
        x1 := OffSet + xt(t + 0.5) * fScale;

      y1 := fCANVAS_Y_MIN + (zt(t) + fd / 2) * fScale;

      Tex := fBitmap.CreateBrushTexture(bsFDiagonal, TexColor, BGRA(255, 255, 255, 0)) as
        TBGRABitmap;
      fBitmap.FillEllipseAntialias(
        CartesianToCanvasX(x1),
        CartesianToCanvasY(y1),
        fd / 2 * fScale, fd / 2 * fScale, Tex);
      Tex.Destroy;

      LoadLine('2nd', PenColor, PenWidth);
      fBitmap.EllipseAntialias(
        CartesianToCanvasX(x1),
        CartesianToCanvasY(y1),
        fd / 2 * fScale, fd / 2 * fScale, PenColor, PenWidth, BGRA(255, 255, 255, 0));

      if (t > 0) and ((t mod 1) > 0) then
      begin
        fBitmap.DrawLineAntialias(
          CartesianToCanvasX(x0),
          CartesianToCanvasY(y0 + fd / 2 * fScale),
          CartesianToCanvasX(x1),
          CartesianToCanvasY(y1 + fd / 2 * fScale),
          PenColor, PenWidth, False);

        fBitmap.DrawLineAntialias(
          CartesianToCanvasX(x0),
          CartesianToCanvasY(y0 - fd / 2 * fScale),
          CartesianToCanvasX(x1),
          CartesianToCanvasY(y1 - fd / 2 * fScale),
          PenColor, PenWidth, False);
      end;
      x0 := x1;
      y0 := y1;
    end;

    // Draw Ends
    x0 := OffSet - (fDm + fd) / 2 * fScale;
    x1 := OffSet + (fDm + fd) / 2 * fScale;
    if fGroundEnds then
      y0 := fCANVAS_Y_MIN + fd / 2 * fScale
    else
      y0 := fCANVAS_Y_MIN;
    y1 := y0;

    if fGroundEnds then
    begin
      fBitmap.FillRect(
        Trunc(CartesianToCanvasX(x0 - 2)),
        Trunc(CartesianToCanvasY(0)),
        Trunc(CartesianToCanvasX(x1 + 2)),
        Trunc(CartesianToCanvasY(y1)),
        fBitmapColor);

      fBitmap.DrawLineAntialias(
        Trunc(CartesianToCanvasX(x0)),
        Trunc(CartesianToCanvasY(y0)),
        Trunc(CartesianToCanvasX(x1)),
        Trunc(CartesianToCanvasY(y1)),
        PenColor, PenWidth, False);
    end;

    x0 := OffSet - (fDm + fd) / 2 * fScale;
    x1 := OffSet + (fDm + fd) / 2 * fScale;
    if fGroundEnds then
      y0 := fCANVAS_Y_MIN + (zt(fnt2 + fn + fnt1)) * fScale
    else
      y0 := fCANVAS_Y_MIN + (zt(fnt2 + fn + fnt1) + fd / 2) * fScale;
    y1 := y0;

    if fGroundEnds then
    begin
      fBitmap.FillRect(
        Trunc(CartesianToCanvasX(x0 - 2)),
        Trunc(CartesianToCanvasY(fHeight)),
        Trunc(CartesianToCanvasX(x1 + 2)),
        Trunc(CartesianToCanvasY(y1)),
        fBitmapColor);

      fBitmap.DrawLineAntialias(
        Trunc(CartesianToCanvasX(x0)),
        Trunc(CartesianToCanvasY(y0)),
        Trunc(CartesianToCanvasX(x1)),
        Trunc(CartesianToCanvasY(y1)),
        PenColor, PenWidth, False);
    end;
    // Draw center line
    LoadLine('CenterLine', PenColor, PenWidth);
    fBitmap.DrawLineAntialias(
      CartesianToCanvasX(OffSet),
      CartesianToCanvasY(fCANVAS_Y_MIN - fSpacer / 2),
      CartesianToCanvasX(OffSet),
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE + fSpacer / 2),
      PenColor, PenWidth, False);
    // Text
    LoadFont('2nd', TextColor);
    x0 := OffSet;
    Y0 := fSpacer div 2 + fBitmap.TextSize(fText).Height;
    fBitmap.TextOut(
      CartesianToCanvasX(x0),
      CartesianToCanvasY(y0),
      fText, TextColor, taCenter);
    // Draw
  end;
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
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
      Bit.TextOutAngle(x, y, 250, ApplicationName, BGRA(150, 150, 150, 255),
        taLeftJustify);

      Inc(y, Bit.TextSize(ApplicationName).Height + DefaultSpacer div 4);
    end;
    Inc(x, Bit.TextSize(ApplicationName).Width + DefaultSpacer div 4);
  end;

  Bit.InvalidateBitmap;
  Bit.Draw(aCanvas, 0, 0);
  Bit.Destroy;
end;

end.
