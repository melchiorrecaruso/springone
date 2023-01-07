unit OptMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Spin, Buttons;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    SolutionLB: TLabel;
    OptimizeBtn: TBitBtn;
    Label1: TLabel;
    SpinEdit: TSpinEdit;
    StringGrid: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure OptimizeBtnClick(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  private
    Busy: boolean;
    function Fitness(const Rm0, DRm: longint): double;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  Math;

{ TMainForm }

procedure TMainForm.SpinEditChange(Sender: TObject);
begin
  if Busy = False then
  begin
    StringGrid.RowCount := SpinEdit.Value + 1;
    SolutionLB.Caption := 'Best solution: none';
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Busy := False;

  StringGrid.Cells[0, 0] := 'Wire diameter [mm]';
  StringGrid.Cells[1, 0] := 'Tensile stress [MPa]';
  SpinEditChange(Sender);
end;

// Math routines

function TMainForm.Fitness(const Rm0, DRm: longint): double;
var
  i: longint;
begin
  Result := 0;
  for i := 1 to StringGrid.RowCount -1 do
  begin
    Result := Result + Sqr((Rm0 - DRm*Log10(StrToFloat(StringGrid.Cells[0, i]) / 1.0)) - StrToFloat(StringGrid.Cells[1, i]));
  end;
  Result := Sqrt(Result);
end;

procedure TMainForm.OptimizeBtnClick(Sender: TObject);
var
  Stress0Min: longint;
  Stress0Max: longint;

  Stress0, Delta0: longint;
  Stress1, Delta1: longint;
  Fitness0: double;
  Fitness1: double;

begin
  try
    Fitness(100, 10);
  except
    ShowMessage('Error');
    Exit;
  end;

  if Busy = False then
  begin
    Busy        := True;
    Stress0Min  := Trunc(0.75 * StrToFloat(StringGrid.Cells[1, SpinEdit.Value]));
    Stress0Max  := Trunc(1.25 * StrToFloat(StringGrid.Cells[1,              1]));

    Fitness1    := MaxFloat;
    Stress1     := MaxInt;
    Delta1      := MaxInt;

    for Stress0 := Stress0Min to Stress0Max do
    begin
      for Delta0 := 1 to Stress0Max do
      begin
        Fitness0 := Fitness(Stress0, Delta0);
        if Fitness0 < Fitness1 then
        begin
          Fitness1 := Fitness0;
          Stress1  := Stress0;
          Delta1   := Delta0;

          SolutionLB.Caption := Format('Current solution: Const=%d  Delta=%d  Error=%0.1f', [Stress1, Delta1, Fitness1]);
        end;
        Application.ProcessMessages;
      end;
    end;
    SolutionLB.Caption := Format('Best solution: Const=%d  Delta=%d  Error=%0.1f', [Stress1, Delta1, Fitness1]);
    Busy := False;
  end;
end;

end.

