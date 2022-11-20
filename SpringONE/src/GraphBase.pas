{
  EN13906-1 Helical Compression Spring Designer

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
  BGRABitmap, BGRABitmapTypes, BGRATextFX, Classes, Graphics, IniFiles, Math, SysUtils, UtilsBase;

type
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

  TCustomDiagram = class(TCustomGraph)
  private
    fCANVAS_X_MIN: longint;
    fCANVAS_X_RANGE: longint;
    fCANVAS_X_COUNT: longint;
    fCANVAS_Y_MIN: longint;
    fCANVAS_Y_RANGE: longint;
    fCANVAS_Y_COUNT: longint;
    fX_MAX: double;
    fX_MIN: double;
    fX_RANGE: double;
    fX_DELTA: double;
    fY_MAX: double;
    fY_MIN: double;
    fY_RANGE: double;
    fY_DELTA: double;
    fCaption: string;
    fHorizontalLabel: string;
    fHorizontalLabels: array of string;
    fVerticalLabel: string;
    fVerticalLabels: array of string;
    procedure Initialize;
    procedure DrawTitle;
    procedure DrawAxis;
    procedure DrawLine(const x0, y0, x1, y1: double; aPenColor: TBGRAPixel; const aPenWidth: double);
    procedure DrawPolyLine(var aPoints: array of TPointF; aPenColor, aTexColor: TBGRAPixel; const aPenWidth: double);
    procedure DrawText(const x, y: double; const aText: string; aTextColor: TBGRAPixel; aAllign: TAlignment; aShiftX, aShiftY: longint);
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    destructor Destroy; override;
  public
    property Caption: string read fCaption write fCaption;
    property HorizontalLabel: string read fHorizontalLabel write fHorizontalLabel;
    property VerticalLabel: string read fVerticalLabel write fVerticalLabel;
  end;

  TForceDisplacementDiagram = class(TCustomDiagram)
  private
    fF1: double;
    fF2: double;
    fFn: double;
    fFc: double;
    fs1: double;
    fs2: double;
    fsn: double;
    fsc: double;
    fToleranceF1: double;
    fToleranceF2: double;
    procedure DrawFx(const x, y: double; const aText: string; const aMulX: double);
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property F1: double read fF1 write fF1;
    property F2: double read fF2 write fF2;
    property Fn: double read fFn write fFn;
    property Fc: double read fFc write fFc;
    property s1: double read fs1 write fs1;
    property s2: double read fs2 write fs2;
    property sn: double read fsn write fsn;
    property sc: double read fsc write fsc;
    property ToleranceF1: double read fToleranceF1 write fToleranceF1;
    property ToleranceF2: double read fToleranceF2 write fToleranceF2;
  end;

  TBucklingDiagram = class(TCustomDiagram)
  private
    fDm: double;
    fE:  double;
    fG:  double;
    fL0: double;
    fnu: double;
    fs1: double;
    fs2: double;
    fsc: double;
    fsk: double;
    function PreCheck: boolean;
    function Curve(const y: double): double;
    procedure DrawSx(const x, y: double; const aText: string; const aMulX, aMulY: double);
    procedure DrawCurve;
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property Dm: double read fDm write fDm;
    property E:  double read fE  write fE;
    property G:  double read fG  write fG;
    property L0: double read fL0 write fL0;
    property nu: double read fnu write fnu;
    property s1: double read fs1 write fs1;
    property s2: double read fs2 write fs2;
    property sc: double read fsc write fsc;
    property sk: double read fsk write fsk;
  end;

  TGoodmanDiagram = class(TCustomDiagram)
  private
    fTauk1: double;
    fTauk2: double;
    fTauz: double;
    fTauYield: double;
    fTauOE7: double;
    fTauOE6: double;
    fTauOE5: double;
    fTauUE7: double;
    fTauUE6: double;
    fTauUE5: double;
    fNumOfCyclesE7: string;
    fNumOfCyclesE6: string;
    fNumOfCyclesE5: string;
    fTauk1Tolerance: double;
    fTauk2Tolerance: double;
    procedure DrawCurve;
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property Tauz:  double read fTauz  write fTauz;
    property Tauk1: double read fTauk1 write fTauk1;
    property Tauk2: double read fTauk2 write fTauk2;
    property Tauk1Tolerance: double read fTauk1Tolerance write fTauk1Tolerance;
    property Tauk2Tolerance: double read fTauk2Tolerance write fTauk2Tolerance;
    property TorsionalStressTauYield: double read fTauYield write fTauYield;
    property TorsionalStressTauOE7: double read fTauOE7 write fTauOE7;
    property TorsionalStressTauOE6: double read fTauOE6 write fTauOE6;
    property TorsionalStressTauOE5: double read fTauOE5 write fTauOE5;
    property TorsionalStressTauUE7: double read fTauUE7 write fTauUE7;
    property TorsionalStressTauUE6: double read fTauUE7 write fTauUE6;
    property TorsionalStressTauUE5: double read fTauUE7 write fTauUE5;
    property NumOfCyclesE7: string read fNumOfCyclesE7 write fNumOfCyclesE7;
    property NumOfCyclesE6: string read fNumOfCyclesE7 write fNumOfCyclesE6;
    property NumOfCyclesE5: string read fNumOfCyclesE7 write fNumOfCyclesE5;
  end;

  TLinearTemperatureDiagram = class(TCustomDiagram)
  private
    fm: double;
    fq: double;
    fTemperature: double;
    fTemperature0: double;
    fTemperature1: double;
    fValue: double;
    fValue0: double;
    fValue1: double;
    procedure DrawCurve;
  public
    constructor Create(const aSection: string; aSetting: TIniFile);
    procedure Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
  public
    property Temperature:  double read fTemperature  write fTemperature;
    property Temperature0: double read fTemperature0 write fTemperature0;
    property Temperature1: double read fTemperature1 write fTemperature1;
    property Value:  double read fValue  write fValue;
    property Value0: double read fValue0 write fValue0;
    property Value1: double read fValue1 write fValue1;
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
    function  GetItem(Row, Column: longint): string;
    procedure SetItem(Row, Column: longint; const S: String);
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
    property HorizontalAlignment: longint read fHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: longint read fVerticalAlignment write SetVerticalAlignment;
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
    fScale:  double;
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

// Common routines

function GetDelta(Count: longint; var Range: double): double;
var
  k: double = 0.001;
begin
  Result := 0;
  while True do
  begin
    Result := 0.10 * k; if (Result*Count) >= (Range) then Break;
    Result := 0.20 * k; if (Result*Count) >= (Range) then Break;
    Result := 0.25 * k; if (Result*Count) >= (Range) then Break;
    Result := 0.50 * k; if (Result*Count) >= (Range) then Break;
  //Result := 0.75 * k; if (Result*Count) >= (Range) then Break;
    k := k * 10;
  end;
  Range := Result*Count;
end;

function GetMin(const MinValue: double): double;
var
  k: longint;
begin
  Result := 0;
  if MinValue > 0 then
  begin
    k := Trunc(Log10(Abs(MinValue)));
    if k > 1 then
      Result := Trunc(MinValue/power(10, k - 1)) * power(10, k - 1)
    else
      Result := MinValue;
  end;
end;

// TCustomGraph

constructor TCustomGraph.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create;
  fBitmap              := TBGRABitmap.Create;
  fBitmap.FontRenderer := TBGRATextEffectFontRenderer.Create;

  fHeight   := 0;
  fWidth    := 0;
  fZoom     := 1.0;
  fSection  := aSection;
  fSetting  := aSetting;
  fSpacer   := DefaultSpacer;

  fBitmapColor.FromString(fSetting.ReadString('Custom', 'Background Color', 'White'));
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
  fBitmap.FontStyle      := [];
  fBitmap.FontAntialias  := True;
  fBitmap.FontQuality    := fqSystemClearType;
  fBitmap.FontName       := fSetting.ReadString (fSection, Format('Font %s Name', [Index]), 'Courier New');
  fBitmap.FontHeight     := Trunc(fSetting.ReadInteger(fSection, Format('Font %s Height', [Index]), 16) * fZoom);

  FontStyle := fSetting.ReadString(fSection, Format('Font %s Style', [Index]), '');
  if Pos('Bold',      FontStyle) > 0 then Include(fBitmap.FontStyle, fsBold      );
  if Pos('Italic',    FontStyle) > 0 then Include(fBitmap.FontStyle, fsItalic    );
  if Pos('Underline', FontStyle) > 0 then Include(fBitmap.FontStyle, fsUnderline );
  if Pos('StrikeOut', FontStyle) > 0 then Include(fBitmap.FontStyle, fsStrikeOut );

  FontColor.FromString(fSetting.ReadString(fSection, Format('Font %s Color', [index]), 'Black'));
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

procedure TCustomGraph.LoadLine(Index: string; var PenColor: TBGRAPixel; var PenWidth: double);
var
  PenStyle: string;
begin
  fBitmap.JoinStyle := pjsRound;
  fBitmap.LineCap   := pecSquare;

  PenStyle :=                fSetting.ReadString(fSection, Format('Line %s Style', [index]), 'Solid');
  PenWidth := TryTextToFloat(fSetting.ReadString(fSection, Format('Line %s Width', [index]), '1.0')) * fZoom;

  if PenStyle = 'Solid'       then fBitmap.PenStyle := psSolid;
  if PenStyle = 'Dash'        then fBitmap.PenStyle := psDash;
  if PenStyle = 'Dot'         then fBitmap.PenStyle := psDot;
  if PenStyle = 'DashDot'     then fBitmap.PenStyle := psDashDot;
  if PenStyle = 'DashDotDot'  then fBitmap.PenStyle := psDashDotDot;
  if PenStyle = 'InsideFrame' then fBitmap.PenStyle := psInsideFrame;
  if PenStyle = 'Pattern'     then fBitmap.PenStyle := psPattern;
  if PenStyle = 'Clear'       then fBitmap.PenStyle := psClear;

  PenColor.FromString(fSetting.ReadString(fSection, Format('Line %s Color', [index]), 'Black'));
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

// TCustomDiagram

constructor TCustomDiagram.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fCANVAS_X_MIN     := 0;
  fCANVAS_X_RANGE   := 0;
  fCANVAS_X_COUNT   := 0;
  fCANVAS_Y_MIN     := 0;
  fCANVAS_Y_RANGE   := 0;
  fCANVAS_Y_COUNT   := 0;
  fX_MAX            := 0;
  fX_MIN            := 0;
  fX_RANGE          := 0;
  fX_DELTA          := 0;
  fY_MAX            := 0;
  fY_MIN            := 0;
  fY_RANGE          := 0;
  fY_DELTA          := 0;
  fCaption          := '';
  fHorizontalLabel  := '';
  fHorizontalLabels := nil;
  fVerticalLabel    := '';
  fVerticalLabels   := nil;
end;

destructor TCustomDiagram.Destroy;
begin
  SetLength(fHorizontalLabels, 0);
  SetLength(fVerticalLabels,   0);
  inherited Destroy;
end;

procedure TCustomDiagram.DrawLine(const x0, y0, x1, y1: double; aPenColor: TBGRAPixel; const aPenWidth: double);
begin
  if (fX_RANGE > 0) and (fY_RANGE > 0) then
  begin
    fBitmap.DrawLineAntialias(
      CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (x0 - fX_MIN) / fX_RANGE),
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (y0 - fY_MIN) / fY_RANGE),
      CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (x1 - fX_MIN) / fX_RANGE),
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (y1 - fY_MIN) / fY_RANGE),
      aPenColor, aPenWidth, False);
  end;
end;

procedure TCustomDiagram.DrawPolyLine(var aPoints: array of TPointF; aPenColor, aTexColor: TBGRAPixel; const aPenWidth: double);
var
  i: longint;
  Tex: TBGRABitmap;
begin
  for i := Low(aPoints) to High(aPoints) do
  begin
    aPoints[i].x := CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (aPoints[i].x - fX_MIN) / fX_RANGE);
    aPoints[i].y := CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (aPoints[i].y - fY_MIN) / fY_RANGE);
  end;

  Tex := fBitmap.CreateBrushTexture(bsFDiagonal, aTexColor, BGRA(255,255,255,0)) as TBGRABitmap;
  fBitmap.FillPolyAntialias(aPoints, Tex);
  fBitmap.DrawPolygonAntialias(aPoints, aPenColor, aPenWidth, BGRA(255,255,255,0));
  Tex.Destroy;
end;

procedure TCustomDiagram.DrawText(const x, y: double; const aText: string;
  aTextColor: TBGRAPixel; aAllign: TAlignment; aShiftX, aShiftY: longint);
begin
  if (fX_RANGE > 0) and (fY_RANGE > 0) then
  begin
    fBitmap.TextOut(
      CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (x - fX_MIN) / fX_RANGE) + aShiftX,
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (y - fY_MIN) / fY_RANGE) + aShiftY,
      aText, aTextColor, aAllign);
  end;
end;

procedure TCustomDiagram.Initialize;
var
  i: longint;
  TextColor: TBGRAPixel;
begin
  fBitmap.SetSize(fWidth, fHeight);
  fBitmap.Fill(fBitmapColor);

  LoadFont('2nd', TextColor);
  fCANVAS_X_MIN   := Max(fBitmap.TextSize(fVerticalLabel).Width, fBitmap.TextSize(TryFloatToText(fY_MAX)).Width) + fSpacer;
  fCANVAS_X_RANGE := fWidth - fCANVAS_X_MIN - (fBitmap.TextSize(fHorizontalLabel).Width + fSpacer) div 2;
  fCANVAS_X_COUNT := fCANVAS_X_RANGE div (Max(fBitmap.TextSize(fHorizontalLabel).Width, fBitmap.TextSize(TryFloatToText(fX_MAX)).Width) + fSpacer);

  fX_DELTA := GetDelta(fCANVAS_X_COUNT, fX_RANGE);
  while (fX_MIN + (fCANVAS_X_COUNT -1) * fX_DELTA) > fX_MAX do Dec(fCANVAS_X_COUNT);
  fX_MAX   := fX_MIN + fCANVAS_X_COUNT * fX_DELTA;
  fX_RANGE := fX_MAX - fX_MIN;

  fCANVAS_Y_MIN := Max(fBitmap.TextSize(fHorizontalLABEL).Height, fBitmap.TextSize(TryFloatToText(fX_MAX)).Height) + fSpacer;

  LoadFont('1st', TextColor);
  fCANVAS_Y_RANGE := fHeight - fCANVAS_Y_MIN - (fBitmap.TextSize(fCaption).Height + fSpacer);

  LoadFont('2nd', TextColor);
  fCANVAS_Y_COUNT := fCANVAS_Y_RANGE div (fBitmap.TextSize(fVerticalLabel).Height + fSpacer);

  fY_DELTA := GetDelta(fCANVAS_Y_COUNT, fY_RANGE);
  while (fY_MIN + (fCANVAS_Y_COUNT -1) * fY_DELTA) > fY_MAX do Dec(fCANVAS_Y_COUNT);
  fY_MAX   := fY_MIN + fCANVAS_Y_COUNT * fY_DELTA;
  fY_RANGE := fY_MAX - fY_MIN;
  // OVERVRITE FOR TBucklingDiagram
  if ClassName = 'TBucklingDiagram' then
  begin
    fY_MAX          := 1.0;
    fY_MIN          := 0.0;
    fY_RANGE        := fY_MAX - fY_MIN;
    fY_DELTA        := 0.1;
    fCANVAS_Y_COUNT := 10;
  end;
  // ***
  SetLength(fHorizontalLabels, fCANVAS_X_COUNT +1);
  for i := Low(fHorizontalLabels) to High(fHorizontalLabels) do fHorizontalLabels[i] := TryFloatToText(fX_MIN + fX_DELTA * i);
  fHorizontalLabels[fCANVAS_X_COUNT] := fHorizontalLabel;

  SetLength(fVerticalLabels, fCANVAS_Y_COUNT +1);
  for i := Low(fVerticalLabels) to High(fVerticalLabels) do fVerticalLabels[i] := TryFloatToText(fY_MIN + fY_DELTA * i);
  fVerticalLabels[fCANVAS_Y_COUNT] := fVerticalLabel;
end;

procedure TCustomDiagram.DrawAxis;
var
  i: longint;
  ShiftX: longint;
  ShiftY: longint;
  FontColor: TBGRAPixel;
  PenColor: TBGRAPixel;
  PenWidth: double;
begin
  Initialize;
  for i := 0 to fCANVAS_Y_COUNT do
  begin
    // Y-Axis
    if i = 0 then
    begin
      LoadLine('1st', PenColor, PenWidth);
      DrawLine(
        fX_MIN,
        fY_MIN,
        fX_MIN + fX_RANGE,
        fY_MIN,
        PenColor, PenWidth);
    end else
    begin
      // Secondary Y-Axis
      LoadLine('2nd', PenColor, PenWidth);
      DrawLine(
        fX_MIN,
        fY_MIN + fY_DELTA * i,
        fX_MIN + fX_RANGE,
        fY_MIN + fY_DELTA * i,
        PenColor, PenWidth);
    end;
    // X Scale Text
    LoadFont('2nd', FontColor);
    ShiftX := - fSpacer div 2;
    ShiftY := - fBitmap.FontHeight div 2;
    DrawText(
      fX_MIN,
      fY_MIN + fY_DELTA * i,
      fVerticalLabels[i], FontColor, taRightJustify, ShiftX, ShiftY);
  end;

  for i := 0 to fCANVAS_X_COUNT do
  begin
    // X-Axis
    if i = 0 then
    begin
      LoadLine('1st', PenColor, PenWidth);
      DrawLine(
        fX_MIN,
        fY_MIN,
        fX_MIN,
        fY_MIN + fY_RANGE,
        PenColor, PenWidth);
    end else
    begin
      // Secondary X-Axis
      LoadLine('2nd', PenColor, PenWidth);
      DrawLine(
        fX_MIN + fX_DELTA * i,
        fY_MIN,
        fX_MIN + fX_DELTA * i,
        fY_MIN + fY_RANGE,
        PenColor, PenWidth);
    end;
    // X Scale Text
    LoadFont('2nd', FontColor);
    ShiftX := + 0;
    ShiftY := + fSpacer div 2;
    DrawText(
      fX_MIN + fX_DELTA * i,
      fY_MIN,
      fHorizontalLabels[i], FontColor, taCenter, ShiftX, ShiftY);
  end;
end;

procedure TCustomDiagram.DrawTitle;
var
  FontColor: TBGRAPixel;
  ShiftX, ShiftY: longint;
begin
  LoadFont('1st', FontColor);
  ShiftX := + 0;
  ShiftY := - fBitmap.TextSize(fCaption).Height - fSpacer div 2;
  DrawText(
    fX_MIN + fX_RANGE / 2,
    fY_MIN + fY_RANGE,
    fCaption, FontColor, taCenter, ShiftX, ShiftY);
end;

// TForceDisplacementDiagram

constructor TForceDisplacementDiagram.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fCaption         := 'Force & Displacement diagram';
  fVerticalLabel   := 'F[N]';
  fHorizontalLabel := 's[mm]';
end;

procedure TForceDisplacementDiagram.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
var
  PenColor: TBGRAPixel;
  PenWidth: double;
begin
  fWidth   := aWidth;
  fHeight  := aHeight;
  // ***
  fX_MIN   := 0;
  fX_MAX   := Max(Max(Max(s1, s2), sn), sc);
  fX_RANGE := fX_MAX - fX_MIN;
  fX_DELTA := 0;

  fY_MIN   := 0;
  fY_MAX   := Max(Max(Max(F1, F2), Fn), Fc);
  fY_RANGE := fY_MAX - fY_MIN;
  fY_DELTA := 0;
  // Draw ...
  DrawAxis();
  DrawTitle();

  if fF1 > 0 then DrawFx(fs1, fF1, 'F1', 6.0);
  if fF2 > 0 then DrawFx(fs2, fF2, 'F2', 4.0);
  if fFn > 0 then DrawFx(fsn, fFn, 'Fn', 2.0);
  if fFc > 0 then
  begin
    DrawFx(fsc, fFc, 'Fc', 0.0);
    if (fsc > 0) then
    begin
      LoadLine('Fx', PenColor, PenWidth);
      DrawLine(0, 0, fsc, fFc, PenColor, PenWidth);
    end;
  end;
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
end;

procedure TForceDisplacementDiagram.DrawFx(const x, y: double; const aText: string; const aMulX: double);
var
  PenWidth: double;
  PenColor: TBGRAPixel;
  TextColor: TBGRAPixel;
  ShiftX: longint;
  ShiftY: longint;
begin
  LoadLine(aText, PenColor, PenWidth);
  DrawLine(0, y, x, y, PenColor, PenWidth);
  DrawLine(x, 0, x, y, PenColor, PenWidth);

  LoadFont(aText, TextColor);
  ShiftX := -Trunc(fBitmap.TextSize('FF').Width * aMulX);
  ShiftY := -Trunc(fBitmap.TextSize('FF').Height);
  DrawText(x, y, aText, TextColor, taRightJustify, ShiftX, ShiftY);
end;

// TBucklingDiagram

constructor TBucklingDiagram.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fCaption         := 'Buckling diagram';
  fVerticalLabel   := 's/L0';
  fHorizontalLabel := 'nu*L0/D';
  fDm := 0;
  fE  := 0;
  fG  := 0;
  fL0 := 0;
  fnu := 0;
  fs1 := 0;
  fs2 := 0;
  fsc := 0;
  fsk := 0;
end;

function TBucklingDiagram.PreCheck: boolean;
begin
  Result := True;
  if fDm <= 0 then Result := False;
  if fE  <= 0 then Result := False;
  if fG  <= 0 then Result := False;
  if fL0 <= 0 then Result := False;
  if fnu <= 0 then Result := False;
  if fs1 <= 0 then Result := False;
  if fs2 <= 0 then Result := False;
  if fsc <= 0 then Result := False;
  if fsk <= 0 then Result := False;
end;

function TBucklingDiagram.Curve(const y: double): double;
begin
  Result := pi/(sqrt((1-sqr(1-y*2*(1-fG/fE)))*(0.5+fG/fE)/(1-fG/fE)));
end;

procedure TBucklingDiagram.DrawCurve;
var
  PenColor: TBGRAPixel;
  PenWidth: double;
  y: double;
begin
  LoadLine('Curve', PenColor, PenWidth);

  y := 0.9999;
  while Curve(y -0.0004) < fX_MAX do
  begin
    DrawLine(Curve(y), y, Curve(y -0.0004), y -0.0004, PenColor, PenWidth);

    y := y - 0.0004;
  end;
end;

procedure TBucklingDiagram.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  fWidth   := aWidth;
  fHeight  := aHeight;
  if PreCheck then
  begin
    fX_MAX   := RoundTo(fnu*fL0/fDm, -2);
    fX_MIN   := 0.0;
    fX_RANGE := fX_MAX - fX_MIN;
    fX_DELTA := 0.0;
    fY_MAX   := 1.0;
    fY_MIN   := 0.0;
    fY_RANGE := fY_MAX - fY_MIN;
    fY_DELTA := 0.1;
  end;
  // Draw ...

  DrawAxis();
  DrawTitle();
  if PreCheck then
  begin
    DrawCurve();
    DrawSx(fnu*fL0/fDm, fsc/fL0, 'sc', +1.00, -0.65);
    DrawSx(fnu*fL0/fDm, fs2/fL0, 's2', -1.75, -0.65);
    DrawSx(fnu*fL0/fDm, fs1/fL0, 's1', +1.00, -0.65);
  end;
  // Results
  //DrawValue(Format('s1 = %0:7.1f mm', [Solver.s1, Solver.tolerance_F1]), 1.50,  0.00);
  //DrawValue(Format('s2 = %0:7.1f mm', [Solver.s2, Solver.tolerance_F1]), 1.50,  1.25);
  //DrawValue(Format('sc = %0:7.1f mm', [Solver.sc, Solver.tolerance_F1]), 1.50,  2.50);
  //DrawValue(Format('sk = %0:7.1f mm', [Solver.sk, Solver.tolerance_F1]), 1.50,  3.75);
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
end;

procedure TBucklingDiagram.DrawSx(const x, y: double; const aText: string; const aMulX, aMulY: double);
var
  PenWidth: double;
  PenColor: TBGRAPixel;
  TextColor: TBGRAPixel;
  ShiftX: longint;
  ShiftY: longint;
begin
  if y < fY_MAX then
  begin
    LoadLine(aText, PenColor, PenWidth);
    DrawLine(x, 0, x, y, PenColor, PenWidth);

    fBitmap.EllipseAntialias(
      CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (x - fX_MIN) / fX_RANGE),
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (y - fY_MIN) / fY_RANGE),
      2.5, 2.5, PenColor, 5.0);

    LoadFont(aText, TextColor);
    ShiftX := Trunc(fBitmap.TextSize('s1').Width  * aMulX);
    ShiftY := Trunc(fBitmap.TextSize('s1').Height * aMulY);
    DrawText(x, y, aText, TextColor, taLeftJustify, ShiftX, ShiftY);
  end;
end;

// TGoodmanDiagram

constructor TGoodmanDiagram.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fCaption         := 'Goodman Diagram';
  fVerticalLabel   := 'tau O';
  fHorizontalLabel := 'tau U';
end;

procedure TGoodmanDiagram.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  fWidth   := aWidth;
  fHeight  := aHeight;
  // ***
  fX_MAX   := RoundTo(Max(Max(fTauk1 + Abs(fTauk1Tolerance), fTauk2 + Abs(fTauk2Tolerance)), fTauz), 0);
  fX_MIN   := 0;
  fX_RANGE := fX_MAX - fX_MIN;
  fX_DELTA := 0;

  fY_MAX   := RoundTo(Max(Max(fTauk1 + Abs(fTauk1Tolerance), fTauk2 + Abs(fTauk2Tolerance)), fTauz), 0);
  fY_MIN   := 0;
  fY_RANGE := fY_MAX - fY_MIN;
  fY_DELTA := 0;
  // Draw ...
  DrawAxis();
  DrawTitle();
  DrawCurve();
  //
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
end;

procedure TGoodmanDiagram.DrawCurve;
var
  Points: array of TPointF;
  PenWidth: double;
  PenColor: TBGRAPixel;
  TextColor: TBGRAPixel;
  TexColor: TBGRAPixel;
  TexWidth: double;
begin
  //10E7
  if (fTauYield > 0) and (fTauOE7 > 0) and (fTauUE7 > 0) then
  begin
    LoadLine('10E7', PenColor, PenWidth);
    DrawLine(
      0,
      fTauOE7,
      fTauUE7,
      fTauYield,
      PenColor, PenWidth);
    DrawLine(
      fTauUE7,
      fTauYield,
      fTauYield,
      fTauYield,
      PenColor, PenWidth);

    LoadFont('10E7', TextColor);
    DrawText(
      fTauUE7,
      fTauYield,
      fNumOfCyclesE7, TextColor, taLeftJustify, 0, -fBitmap.TextSize('FF').Height);
  end;

  //10E6
  if (fTauYield > 0) and (fTauOE6 > 0) and  (fTauUE6 > 0) and (fTauOE6 < fTauYield) then
  begin
    LoadLine('10E6', PenColor, PenWidth);
    DrawLine(
      0,
      fTauOE6,
      fTauUE6,
      fTauYield,
      PenColor, PenWidth);
    DrawLine(
      fTauUE6,
      fTauYield,
      fTauUE7,
      fTauYield,
      PenColor, PenWidth);

    LoadFont('10E6', TextColor);
    DrawText(
      fTauUE6,
      fTauYield,
      fNumOfCyclesE6, TextColor, taLeftJustify, 0, -fBitmap.TextSize('FF').Height);
  end;

  //10E5
  if (fTauYield > 0) and (fTauOE5 > 0) and (fTauUE5 > 0) and (fTauOE5 < fTauYield) then
  begin
    LoadLine('10E5', PenColor, PenWidth);
    DrawLine(
      0,
      fTauOE5,
      fTauUE5,
      fTauYield,
      PenColor, PenWidth);
    DrawLine(
      fTauUE5,
      fTauYield,
      fTauUE6,
      fTauYield,
      PenColor, PenWidth);

    LoadFont('10E5', TextColor);
    DrawText(
      fTauUE5,
      fTauYield,
      fNumOfCyclesE5, TextColor, taLeftJustify, 0, -fBitmap.TextSize('FF').Height);
  end;

  if (fTauk1 > 0) and (fTauk2 > fTauk1) then
  begin
    Points := nil;
    SetLength(Points, 4);
    Points[0].x := fTauk1 + Abs(fTauk1Tolerance);
    Points[0].y := fTauk1 + Abs(fTauk1Tolerance);
    Points[1].x := fTauk1 + Abs(fTauk1Tolerance);
    Points[1].y := fTauk2 + Abs(fTauk2Tolerance);
    Points[2].x := fTauk1 - Abs(fTauk1Tolerance);
    Points[2].y := fTauk2 - Abs(fTauk2Tolerance);
    Points[3].x := fTauk1 - Abs(fTauk1Tolerance);
    Points[3].y := fTauk1 - Abs(fTauk1Tolerance);

    LoadLine('Tex',      TexColor, TexWidth);
    LoadLine('Tauk+Tol', PenColor, PenWidth);
    DrawPolyLine(Points, PenColor, TexColor, PenWidth);
    Points := nil;

    LoadLine('45DEG', PenColor, PenWidth);
    DrawLine(0, 0,
      Min(fX_MAX, fY_MAX),
      Min(fX_MAX, fY_MAX),
      PenColor, PenWidth);
    LoadLine('Tauk', PenColor, PenWidth);
    DrawLine(
      fTauk1,
      fTauk1,
      fTauk1,
      fTauk2,
      PenColor, PenWidth);

    LoadFont('Tauk', TextColor);
    DrawText(
      fTauk1 + Abs(fTauk1Tolerance),
      fTauk1 + Abs(fTauk1Tolerance),
      'tauk1', TextColor, taLeftJustify, 0, 0);
    DrawText(
      fTauk1 + Abs(fTauk1Tolerance),
      fTauk2 + Abs(fTauk2Tolerance),
      'tauk2', TextColor, taLeftJustify, 0, -fBitmap.TextSize('tauk2').Height);
  end;
end;

// TLinearTemperatureDiagram

constructor TLinearTemperatureDiagram.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fCaption         := '';
  fVerticalLabel   := '';
  fHorizontalLabel := '';
end;

procedure TLinearTemperatureDiagram.Draw(aCanvas: TCanvas; aWidth, aHeight: longint);
begin
  fWidth   := aWidth;
  fHeight  := aHeight;
  // ***
  fm := (fValue1 - fValue0)/(fTemperature1 - fTemperature0);
  fq :=  fValue1 - fm * fTemperature1;

  fX_MAX   :=        RoundTo(fTemperature + 50, 0);
  fX_MIN   :=       (RoundTo(fTemperature - 50, 0));
  fX_RANGE := fX_MAX - fX_MIN;
  fX_DELTA := 0;

  fY_MAX   :=        RoundTo(fX_MIN * fm + fq, 0);
  fY_MIN   := GetMin(RoundTo(fX_MAX * fm + fq, 0));
  fY_RANGE := fY_MAX - fY_MIN;
  fY_DELTA := 0;

  // Draw ...
  DrawAxis();
  DrawTitle();
  DrawCurve();
  //
  fBitmap.InvalidateBitmap;
  fBitmap.Draw(aCanvas, 0, 0, False);
end;

procedure TLinearTemperatureDiagram.DrawCurve;
var
  PenColor: TBGRAPixel;
  PenWidth: double;
  x0, x1: double;
begin
  x0 := fX_MIN;
  while x0 * fm + fq > fY_MAX do
  begin
    x0 := x0 + 1;
  end;

  x1 := fX_MAX;
  while x1 * fm + fq < fY_MIN do
  begin
    x1 := x1 - 1;
  end;

  LoadLine('Curve', PenColor, PenWidth);
  DrawLine(
    x0,
    x0 * fm + fq,
    x1,
    x1 * fm + fq,
    PenColor, PenWidth);

  LoadLine('Dot', PenColor, PenWidth);
  fBitmap.EllipseAntialias(
    CartesianToCanvasX(fCANVAS_X_MIN + fCANVAS_X_RANGE * (fTemperature - fX_MIN) / fX_RANGE),
    CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE * (fValue       - fY_MIN) / fY_RANGE),
    PenWidth/2, PenWidth/2, PenColor, PenWidth);
end;

// TReportTable

constructor TReportTable.Create(const aSection: string; aSetting: TIniFile);
begin
  inherited Create(aSection, aSetting);
  fHorizontalAlignment := 0;
  fHorizontalSpacer    := fSpacer;
  fVerticalAlignment   := 1;
  fVerticalSpacer      := 0;
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

procedure TReportTable.SetItem(Row, Column: longint; const S: String);
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
  fVerticalSpacer   := 0;
end;

procedure TReportTable.SetSize(aRowCount, aColumnCount: longint);
var
  i: longint;
begin
  for i := Low(fTable) to High(fTable) do
    SetLength(fTable[i], 0);
  SetLength(fTable, 0);

  fRowCount    := aRowCount;
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
  //
  case fHorizontalAlignment of
    0: xoffset := 0;
    1: xoffset := (aWidth - Width) div 2;
    2: xoffset := (aWidth - Width);
  else xoffset := 0;
  end;

  case fVerticalAlignment of
    0: yoffset := 0;
    1: yoffset := (aHeight - Height) div 2;
    2: yoffset := (aHeight - Height);
  else yoffset := 0;
  end;
  //
  SetLength(y, fRowCount + 1);
  SetLength(x, fColumnCount + 1);
  for i := Low(x) to High(x) do x[i] := xoffset;
  for i := Low(y) to High(y) do y[i] := yoffset;

  LoadFont('2nd', TextColor);
  for i := Low(fTable) to High(fTable) do
  begin
    for j := Low(fTable[i]) to High(fTable[i]) do
    begin
      x[j + 1] := Max(x[j + 1], fBitmap.TextSize(GetItem(i, j)).Width + fHorizontalSpacer);
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
  //
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
       if (i < fRowCount)    and
          (j < fColumnCount) then
       begin
         fBitmap.TextOut(
           xsum + fHorizontalSpacer div 2,
           ysum + fVerticalSpacer   div 2,
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
  fd          := 0;
  fDm         := 0;
  fLc         := 0;
  fLx         := 0;
  fn          := 0;
  fnt1        := 0;
  fnt2        := 0;
  fpitch      := 0;
  fScale      := 1;
  fClockWise  := True;
  fGroundEnds := True;
  fClosedEnds := True;
  fText       := '';
  fScale      := 1.0;
  fFit        := True;
end;

destructor TSectionSpringDrawing.Destroy;
begin
  inherited Destroy;
end;

function TSectionSpringDrawing.xt(const t: double): double;
begin
  Result := (fDm/2)*cos(2*pi*t);
end;

function TSectionSpringDrawing.yt(const t: double): double;
begin
  Result := (fDm/2)*sin(2*pi*t);
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
    Result := fnt2*fd + (t - fnt2)*fpitch;
  end;

  if (t > (fnt2 + fn)) then
  begin
    Result := fnt2*fd + fn*fpitch + (t - (fnt2 + fn))*fd;
  end;
end;

function TSectionSpringDrawing.PreCheck: boolean;
begin
  Result := True;
  if fd     <= 0   then Result := False;
  if fDm    <= 0   then Result := False;
  if fLc    <= 0   then Result := False;
  if fLx    <  fLc then Result := False;
  if fn     <= 0   then Result := False;
  if fnt1   <= 0   then Result := False;
  if fnt2   <= 0   then Result := False;
//if fpitch <= 0   then Result := False;
  if fScale <= 0   then Result := False;
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
  fY_MIN: double;
  fY_MAX: double;
begin
  fWidth  := aWidth;
  fHeight := aHeight;
  OffSet  := fWidth div 2;
  //
  fBitmap.SetSize(fWidth, fHeight);
  fBitmap.Fill(fBitmapColor);
  //
  LoadFont('2nd', TextColor);
  fCANVAS_X_MIN   := fSpacer;
  fCANVAS_Y_MIN   := fSpacer div 2 + fBitmap.TextSize('FF').Height + fSpacer;
  fCANVAS_X_RANGE := (aWidth  - fSpacer) - fCANVAS_X_MIN;
  fCANVAS_Y_RANGE := (aHeight - fSpacer) - fCANVAS_Y_MIN;

  if PreCheck then
  begin
    fpitch := fd + (Lx - Lc)/fn;
    fX_MIN := 0;
    fX_MAX := fDm + fd;

    fY_MIN := 0;
    fY_MAX := zt(fnt2 + fn + fnt1) + fd;

    if fFit then
      fScale := Min(fCANVAS_X_RANGE/fX_MAX, fCANVAS_Y_RANGE/fY_MAX);

    LoadLine('Tex', TexColor, TexWidth);
    x0 := 0;
    y0 := 0;

    t  := -0.5;
    while t < (fnt2 + fn + fnt1) do
    begin
      t := t + 0.5;
      if fClockWise then
        x1 := OffSet + xt(t)*fScale
      else
        x1 := OffSet + xt(t + 0.5)*fScale;

      y1 := fCANVAS_Y_MIN + (zt(t) + fd/2)*fScale;

      Tex := fBitmap.CreateBrushTexture(bsFDiagonal, TexColor, BGRA(255,255,255,0)) as TBGRABitmap;
      fBitmap.FillEllipseAntialias(
        CartesianToCanvasX(x1),
        CartesianToCanvasY(y1),
        fd/2*fScale, fd/2*fScale, Tex);
      Tex.Destroy;

      LoadLine('2nd', PenColor, PenWidth);
      fBitmap.EllipseAntialias(
        CartesianToCanvasX(x1),
        CartesianToCanvasY(y1),
        fd/2*fScale, fd/2*fScale, PenColor, PenWidth, BGRA(255,255,255,0));

      if (t > 0) and ((t mod 1) > 0) then
      begin
        fBitmap.DrawLineAntialias(
          CartesianToCanvasX(x0),
          CartesianToCanvasY(y0 + fd/2*fScale),
          CartesianToCanvasX(x1),
          CartesianToCanvasY(y1 + fd/2*fScale),
          PenColor, PenWidth, False);

        fBitmap.DrawLineAntialias(
          CartesianToCanvasX(x0),
          CartesianToCanvasY(y0 - fd/2*fScale),
          CartesianToCanvasX(x1),
          CartesianToCanvasY(y1 - fd/2*fScale),
          PenColor, PenWidth, False);
      end;
      x0 := x1;
      y0 := y1;
    end;

    // Draw Ends
    x0 := OffSet - (fDm + fd)/2 *fScale;
    x1 := OffSet + (fDm + fd)/2 *fScale;
    if fGroundEnds then
      y0 := fCANVAS_Y_MIN + fd/2*fScale
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

    x0 := OffSet - (fDm + fd)/2 *fScale;
    x1 := OffSet + (fDm + fd)/2 *fScale;
    if fGroundEnds then
      y0 := fCANVAS_Y_MIN + (zt(fnt2+fn+fnt1))*fScale
    else
      y0 := fCANVAS_Y_MIN + (zt(fnt2+fn+fnt1) + fd/2)*fScale;
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
      CartesianToCanvasY(fCANVAS_Y_MIN - fSpacer/2),
       CartesianToCanvasX(OffSet),
      CartesianToCanvasY(fCANVAS_Y_MIN + fCANVAS_Y_RANGE + fSpacer/2),
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
  Bit.FontAntialias := true;
  Bit.FontStyle := [fsBold];

  x := -50;
  while x < (aWidth + 50) do
  begin
    y := -50;
    while y < (aHeight + 50) do
    begin
      Bit.TextOutAngle(x, y , 250, 'SpringONE', BGRA(150, 150, 150, 255), taLeftJustify);

      Inc(y, Bit.TextSize('SpringONE').Height + DefaultSpacer div 4);
    end;
    Inc(x, Bit.TextSize('SpringONE').Width + DefaultSpacer div 4);
  end;

  Bit.InvalidateBitmap;
  Bit.Draw(aCanvas, 0, 0);
  Bit.Destroy;
end;

end.

