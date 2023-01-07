unit TestFrm;

{$mode objfpc}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, BGRAVirtualScreen, Classes, SysUtils, Forms,
  Controls, GraphBase, Graphics, Dialogs, ExtCtrls, StdCtrls, TAStyles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Screen: TBGRAVirtualScreen;
    Button1: TButton;
    procedure Image1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math;

{ TForm1 }

procedure TForm1.Image1Click(Sender: TObject);
var
  i: single;
  Chart: TChart;
  Points: ArrayOfTPointF;
begin
  Chart := TChart.Create;

  Chart.PenColor := BGRA(0, 0, 0, 225);
  Chart.PenWidth := 1;
  SetLength(Points, 2);

  i := 0;
  while i < 360 do
  begin
    Points[0].x := 0;
    Points[0].y := 0;
    Points[1].x := 500 * cos(DegToRad(i));
    Points[1].y := 500 * sin(DegToRad(i));
    Chart.AddPolyLine(Points, True, '');
    i := i + 22.5;
  end;

  (*
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 500 * cos(DegToRad(22.5));
  Points[1].y := 500 * sin(DegToRad(22.5));
  Chart.AddPolyLine(Points, true, true, 'Start');

  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 500 * cos(DegToRad(45));
  Points[1].y := 500 * sin(DegToRad(45));
  Chart.AddPolyLine(Points, true, true, 'Start');

  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 500 * cos(DegToRad(70));
  Points[1].y := 500 * sin(DegToRad(70));
  Chart.AddPolyLine(Points, true, true, 'Start');

  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 500 * cos(DegToRad(110));
  Points[1].y := 500 * sin(DegToRad(110));
  Chart.AddPolyLine(Points, true, true, 'Start');

  *)



  SetLength(Points, 3);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 1000;
  Points[1].y := 1000;
  Points[2].x := 1505;
  Points[2].y := 1250;

  Chart.PenColor := BGRA(255, 0, 255, 255);
  Chart.AddPolyLine(Points, False, 'F1');

  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 1505;
  Points[1].y := 1000;
  Points[2].x := 1505;
  Points[2].y := 1250;


  Chart.PenColor := BGRA(0, 255, 255, 255);
  Chart.AddPolyLine(Points, False, 'F2');


  SetLength(Points, 5);
  Points[0].x := 100;
  Points[0].y := 100;
  Points[1].x := 200;
  Points[1].y := 800;
  Points[2].x := 1200;
  Points[2].y := 800;
  Points[3].x := 1200;
  Points[3].y := 150;
  Points[4].x := 100;
  Points[4].y := 100;

  Chart.PenColor     := BGRA(255, 255, 0, 255);
  Chart.TextureColor := BGRA(255, 255, 0, 127);

  Chart.AddPolygon(Points, 'FP');
  Points := nil;

  Chart.AddLabel(200, 800, 0, 0, taLeftJustify, taAlignBottom, 'SuperPippo');


  Chart.Width  := Screen.Width;
  Chart.Height := Screen.Height;
  Chart.Zoom := 2.0;
  Chart.Draw(Screen.Canvas);
  Chart.Destroy;
end;

end.

