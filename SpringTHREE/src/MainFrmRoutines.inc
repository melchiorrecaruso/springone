// FormPaint

procedure TMainForm.PaintTo(var aScreen: TBGRABitmap; aScreenColor: TBGRAPixel; aScreenScale: double; aSetting: TIniFile);
var
  i: longint;
  Bit: array of TBGRABitmap = nil;
  BucklingDiagram: TBucklingDiagram;
  ForceDiagram: TForceDisplacementDiagram;
  GoodmanDiagram: TGoodmanDiagram;
  Load1Diagram: TLinearTemperatureDiagram;
  Load2Diagram: TLinearTemperatureDiagram;
  MessageList: TReportTable;
  QualityTable: TReportTable;
  ReportList0: TReportTable;
  ReportList1: TReportTable;
  ReportTable: TReportTable;
  ShearModulusDiagram: TLinearTemperatureDiagram;
  SpringDrawing: TSectionSpringDrawing;
  YoungModulusDiagram: TLinearTemperatureDiagram;
  SVG: TBGRASvg;
begin
  ErrorMessage.Clear;
  WarningMessage.Clear;
  aScreen.Fill(aScreenColor);

  SOLVER.Solve;
  ForceDiagram        := CreateForceDisplacementDiagram(aScreenScale, aSetting);
  GoodmanDiagram      := CreateGoodmanDiagram          (aScreenScale, aSetting);
  BucklingDiagram     := CreateBucklingDiagram         (aScreenScale, aSetting);
  ShearModulusDiagram := CreateShearModulusDiagram     (aScreenScale, aSetting);
  YoungModulusDiagram := CreateYoungModulusDiagram     (aScreenScale, aSetting);
  Load1Diagram        := CreateLoad1Diagram            (aScreenScale, aSetting);
  Load2Diagram        := CreateLoad2Diagram            (aScreenScale, aSetting);
  MessageList         := CreateMessageList             (aScreenScale, aSetting);
  ReportTable         := CreateReportTable             (aScreenScale, aSetting);
  QualityTable        := CreateQualityTable            (aScreenScale, aSetting);
  ReportList0         := CreateReportList0             (aScreenScale, aSetting);
  SpringDrawing       := CreateSpringDrawing           (aScreenScale, aSetting);

  if ForceMenuItem        .Checked then ForceDiagram       .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if GoodmanMenuItem      .Checked then GoodmanDiagram     .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if BucklingMenuItem     .Checked then BucklingDiagram    .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if ShearModulusMenuItem .Checked then ShearModulusDiagram.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if YoungModulusMenuItem .Checked then YoungModulusDiagram.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if F1MenuItem           .Checked then Load1Diagram       .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if F2MenuItem           .Checked then Load2Diagram       .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
  if CustomSectionMenuItem.Checked then SpringDrawing      .Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);

  if SectionMenuItem.Checked then
  begin
    SetLength(Bit, 3);
    for i := Low(Bit) to High(Bit) do
      Bit[i] := TBGRABitmap.Create;
    SpringDrawing.ClosedEnds  := False;
    SpringDrawing.GroundEnds  := False;
    Bit[0].SetSize(aScreen.Width div 3, aScreen.Height);
    SpringDrawing.Fit  := True;
    SpringDrawing.Lx   := SOLVER.LengthL0;
    SpringDrawing.Text := TryFormatFloat('L0 = %s', 'L0 = ---',SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
    Bit[1].SetSize(Bit[0].Width, Bit[0].Height);
    SpringDrawing.Fit  := False;
    SpringDrawing.Lx   := SOLVER.LengthL1;
    SpringDrawing.Text := TryFormatFloat('L1 = %s', 'L1 = ---', SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
    Bit[2].SetSize(Bit[1].Width, Bit[1].Height);
    SpringDrawing.Fit  := False;
    SpringDrawing.Lx   := SOLVER.LengthL2;
    SpringDrawing.Text := TryFormatFloat('L2 = %s', 'L2 = ---', SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);
    Bit[0].Draw(aScreen.Canvas, Bit[0].Width * 0, 0, False);
    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 1, 0, False);
    Bit[2].Draw(aScreen.Canvas, Bit[2].Width * 2, 0, False);
    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil;
  end;

  if Quick1MenuItem.Checked then
  begin
    SetLength(Bit, 10);
    for i := Low(Bit) to High(Bit) do
      Bit[i] := TBGRABitmap.Create;

    Bit[0].SetSize(ReportList0.Width + ReportList0.Spacer, ReportList0.Height + ReportList0.Spacer);
    ReportList0.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    ReportList1 := TReportTable.Create('ReportList', aSetting);
    ReportList1.Spacer       := Trunc(DefaultSpacer*aScreenScale);
    ReportList1.Zoom         := aScreenScale;

    ReportList1.ColumnCount  := 1;
    ReportList1.RowCount     := 17;
    ReportList1.Items[ 0, 0] := TryFormatFloat   ('d     = %s mm',     'd     = ---', SOLVER.WireDiameter);
    ReportList1.Items[ 1, 0] := TryFormatFloat   ('tauk1 = %s MPa',    'tauk1 = ---', SOLVER.TorsionalStressTauk1);
    ReportList1.Items[ 2, 0] := TryFormatFloat   ('tauk2 = %s MPa',    'tauk2 = ---', SOLVER.TorsionalStressTauk2);
    ReportList1.Items[ 3, 0] := TryFormatFloat   ('taukh = %s MPa',    'taukh = ---', SOLVER.TorsionalStressTaukh);
    ReportList1.Items[ 4, 0] := ' ';
    ReportList1.Items[ 5, 0] := TryFormatFloat   ('E     = %s MPa',    'E     = ---', SOLVER.YoungModulus);
    ReportList1.Items[ 6, 0] := TryFormatFloat   ('G     = %s MPa',    'G     = ---', SOLVER.ShearModulus);
    ReportList1.Items[ 7, 0] := TryFormatFloat   ('rho   = %s kg/dm3', 'rho   = ---', SOLVER.MaterialDensity);
    ReportList1.Items[ 8, 0] := TryFormatFloat   ('Rm    = %s MPa',    'Rm    = ---', SOLVER.TensileStrengthRm);
    ReportList1.Items[ 9, 0] := TryFormatFloat   ('tauz  = %s MPa',    'tauz  = ---', SOLVER.AdmStaticTorsionalStressTauz);
    ReportList1.Items[10, 0] := ' ';
    ReportList1.Items[11, 0] := TryFormatFloat   ('ns    = %s',        'ns    = ---', SOLVER.StaticSafetyFactor);
    if ApplicationForm.LoadType.ItemIndex = 0 then
    begin
      ReportList1.Items[12, 0] := TryFormatFloat   ('tauoz = %s MPa',    'tauoz = ---', SOLVER.AdmDynamicTorsionalStressTauoz);
      ReportList1.Items[13, 0] := TryFormatFloat   ('tauhz = %s MPa',    'tauhz = ---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);
      ReportList1.Items[14, 0] := TryFormatFloat   ('nf    = %s',        'nf    = ---', SOLVER.DynamicSafetyFactor);

      ReportList1.Items[15, 0] := 'N     = ---';
      ReportList1.Items[16, 0] := 'Nh    = ---';
      if SOLVER.NumOfCycles > 0 then
      begin
        ReportList1.Items[15, 0] := TryFormatText    ('N     = %s cycles', 'N     = ---', TryFloatToText(SOLVER.NumOfCycles, 2, 0));
        ReportList1.Items[16, 0] := TryFormatFloatDiv('Nh    = %s hours',  'Nh    = ---', SOLVER.NumOfCycles, 3600*ApplicationForm.CycleFrequency.Value);
      end;
    end;

    Bit[1].SetSize(ReportList1.Width + ReportList1.Spacer, aScreen.Height - Bit[0].Height);
    ReportList1.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    Bit[2].SetSize((aScreen.Width - Bit[1].Width) div 2, Bit[1].Height);
    ForceDiagram.Draw(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);

    Bit[3].SetSize((aScreen.Width - Bit[1].Width) div 2, Bit[1].Height);
    GoodmanDiagram.Draw(Bit[3].Canvas, Bit[3].Width, Bit[3].Height);

    Bit[4].SetSize(ReportTable.Width + ReportTable.Spacer, ReportTable.Height + ReportTable.Spacer);
    ReportTable.Draw(Bit[4].Canvas, Bit[4].Width, Bit[4].Height);

    Bit[5].SetSize((aScreen.Width - Bit[0].Width - Bit[4].Width) div 3, Bit[0].Height);
    SpringDrawing.Text := Format('L0 = %s', [TryFloatToText(SOLVER.LengthL0)]);
    SpringDrawing.Lx   := SOLVER.LengthL0;
    SpringDrawing.Fit  := True;
    SpringDrawing.Draw(Bit[5].Canvas, Bit[5].Width, Bit[5].Height);

    if (3*Bit[5].Width) <= (aScreen.Width - Bit[0].Width - Bit[4].Width) then
    begin
      Bit[6].SetSize(Bit[5].Width, Bit[5].Height);
      SpringDrawing.Text := Format('L1 = %s', [TryFloatToText(SOLVER.LengthL1)]);
      SpringDrawing.Lx   := SOLVER.LengthL1;
      SpringDrawing.Fit  := False;
      SpringDrawing.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

      Bit[7].SetSize(Bit[5].Width, Bit[5].Height);
      SpringDrawing.Text := Format('L2 = %s', [TryFloatToText(SOLVER.LengthL2)]);
      SpringDrawing.Lx   := SOLVER.LengthL2;
      SpringDrawing.Fit  := False;
      SpringDrawing.Draw(Bit[7].Canvas, Bit[7].Width, Bit[7].Height);
    end;

    Bit[8].SetSize(MessageList.Width, MessageList.Height);
    MessageList.Draw(Bit[8].Canvas, Bit[8].Width, Bit[8].Height);

    Bit[9].SetSize(QualityTable.Width + QualityTable.Spacer, Bit[0].Height - Bit[4].Height);
    QualityTable.Draw(Bit[9].Canvas, Bit[9].Width, Bit[9].Height);

    Bit[0].Draw(aScreen.Canvas, Bit[5].Width + Bit[6].Width + Bit[7].Width, 0, False);
    Bit[1].Draw(aScreen.Canvas, Bit[2].Width + Bit[3].Width, Bit[0].Height, False);
    Bit[2].Draw(aScreen.Canvas, 0, Bit[0].Height, False);
    Bit[3].Draw(aScreen.Canvas, Bit[2].Width, Bit[0].Height, False);
    Bit[4].Draw(aScreen.Canvas, Bit[5].Width + Bit[6].Width + Bit[7].Width + Bit[0].Width, 0, False);
    Bit[5].Draw(aScreen.Canvas, 0 , 0, False);
    Bit[6].Draw(aScreen.Canvas, Bit[5].Width, 0, False);
    Bit[7].Draw(aScreen.Canvas, Bit[5].Width + Bit[6].Width, 0, False);
    Bit[9].Draw(aScreen.Canvas, Bit[5].Width + Bit[6].Width + Bit[7].Width + Bit[0].Width, Bit[4].Height, False);
    Bit[8].Draw(aScreen.Canvas, Bit[5].Width + Bit[6].Width + Bit[7].Width + Bit[0].Width + Bit[9].Width, Bit[4].Height, False);

    ReportList1.Destroy;
    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil
  end;

  if (Quick2MenuItem.Checked) or (Quick3MenuItem.Checked) then
  begin
    SetLength(Bit, 2);
    for i := Low(Bit) to High(Bit) do
      Bit[i] := TBGRABitmap.Create;

    ReportList1                   := TReportTable.Create('ReportList', aSetting);
    ReportList1.Spacer            := Trunc(DefaultSpacer*aScreenScale);
    ReportList1.VerticalAlignment := 1;
    ReportList1.Zoom              := aScreenScale;

    ReportList1.ColumnCount  := 3;
    ReportList1.RowCount     := 33;
    ReportList1.Items[ 0, 0] := TryFormatFloat   ('d        = %s mm',     'd     = ---', SOLVER.WireDiameter) + TryFormatFloat(' ± %s mm', '', SOLVER.WireDiameterMax - SOLVER.WireDiameter);
    ReportList1.Items[ 1, 0] := TryFormatFloat   ('Di       = %s mm',     'Di    = ---', SOLVER.Di);
    ReportList1.Items[ 2, 0] := TryFormatFloat   ('Dm       = %s mm',     'Dm    = ---', SOLVER.Dm) + TryFormatFloat(' ± %s mm', '', TOL.CoilDiameterTolerance);
    ReportList1.Items[ 3, 0] := TryFormatFloat   ('De       = %s mm',     'De    = ---', SOLVER.De);
    ReportList1.Items[ 4, 0] := TryFormatFloat   ('n        = %s coils',  'n     = ---', SOLVER.ActiveColis);
    ReportList1.Items[ 5, 0] := TryFormatFloat   ('nt       = %s colis',  'nt    = ---', SOLVER.TotalCoils);
    ReportList1.Items[ 6, 0] := TryFormatFloatDiv('Dm/d     = %s',        'Dm/d  = ---', SOLVER.Dm, SOLVER.WireDiameter);
    ReportList1.Items[ 7, 0] := TryFormatFloat   ('nu       = %s',        'nu    = ---', SOLVER.SeatingCoefficent);
    ReportList1.Items[ 8, 0] := TryFormatFloat   ('k        = %s',        'k     = ---', SOLVER.CorrectionFactorK);
    ReportList1.Items[ 9, 0] := '';
    ReportList1.Items[10, 0] := TryFormatFloat   ('L        = %s mm',     'L     = ---', SOLVER.WireLength);
    ReportList1.Items[11, 0] := TryFormatFloat   ('rho      = %s kg/dm3', 'rho   = ---', SOLVER.MaterialDensity);
    ReportList1.Items[12, 0] := TryFormatFloat   ('mass     = %s g',      'mass  = ---', SOLVER.Mass);
    ReportList1.Items[13, 0] := TryFormatFloat   ('fe       = %s Hz',     'fe    = ---', SOLVER.NaturalFrequency);
    ReportList1.Items[14, 0] := '';
    ReportList1.Items[15, 0] := TryFormatText    ('Material = %s',        'Material = ---', MAT.Items[MAT.ItemIndex]);
    ReportList1.Items[16, 0] := TryFormatFloat   ('G        = %s MPa',    'G        = ---', SOLVER.ShearModulus);
    ReportList1.Items[17, 0] := TryFormatFloat   ('Rm       = %s MPa',    'Rm       = ---', SOLVER.TensileStrengthRm);
    ReportList1.Items[18, 0] := TryFormatFloat   ('tauz     = %s MPa',    'tauz     = ---', SOLVER.AdmStaticTorsionalStressTauz);
    ReportList1.Items[19, 0] := TryFormatFloat   ('T        = %s C°',     'T        = ---', MAT.Tempetature);
    ReportList1.Items[20, 0] := TryFormatFloat   ('G(T)     = %s MPa',    'G(T)     = ---', MAT.GetG(MAT.Tempetature));
    ReportList1.Items[21, 0] := '';
    ReportList1.Items[22, 0] := TryFormatBool    ('Closed ends    = True', 'Closed ends    = False', SOLVER.ClosedEnds);
    ReportList1.Items[23, 0] := TryFormatBool    ('Ground ends    = True', 'Ground ends    = False', SOLVER.GroundEnds);
    ReportList1.Items[24, 0] := TryFormatBool    ('Cold coiled    = True', 'Cold coiled    = False', SOLVER.ColdCoiled);
    ReportList1.Items[25, 0] := TryFormatBool    ('Dynamic strain = True', 'Dynamic strain = False', SOLVER.DynamicLoad);
    ReportList1.Items[26, 0] := '';
    ReportList1.Items[27, 0] := TryFormatInt     ('EN15800 Quality Grade Dm  = %s', 'EN15800 Quality Grade Dm  = ---', TOL.DmQualityGrade);
    ReportList1.Items[28, 0] := TryFormatInt     ('EN15800 Quality Grade L0  = %s', 'EN15800 Quality Grade L0  = ---', TOL.L0QualityGrade);
    ReportList1.Items[29, 0] := TryFormatInt     ('EN15800 Quality Grade F1  = %s', 'EN15800 Quality Grade F1  = ---', TOL.F1QualityGrade);
    ReportList1.Items[30, 0] := TryFormatInt     ('EN15800 Quality Grade F2  = %s', 'EN15800 Quality Grade F2  = ---', TOL.F2QualityGrade);
    ReportList1.Items[31, 0] := TryFormatInt     ('EN15800 Quality Grade e1  = %s', 'EN15800 Quality Grade e1  = ---', TOL.E1QualityGrade);
    ReportList1.Items[32, 0] := TryFormatInt     ('EN15800 Quality Grade e2  = %s', 'EN15800 Quality Grade e2  = ---', TOL.E2QualityGrade);

    ReportList1.Items[ 0, 1] := '   ';

    ReportList1.Items[ 0, 2] := TryFormatFloat('L0    = %s mm',  'L0    = ---', SOLVER.LengthL0) + TryFormatFloat(' ± %s mm', '', TOL.LengthL0Tolerance);
    ReportList1.Items[ 1, 2] := TryFormatFloat('L1    = %s mm',  'L1    = ---', SOLVER.LengthL1);
    ReportList1.Items[ 2, 2] := TryFormatFloat('L2    = %s mm',  'L2    = ---', SOLVER.LengthL2);
    ReportList1.Items[ 3, 2] := TryFormatFloat('Ln    = %s mm',  'Ln    = ---', SOLVER.LengthLn);
    ReportList1.Items[ 4, 2] := TryFormatFloat('Lc    = %s mm',  'Lc    = ---', SOLVER.LengthLc);
    ReportList1.Items[ 5, 2] := '';
    ReportList1.Items[ 6, 2] := TryFormatFloat('s1    = %s mm',  's1    = ---', SOLVER.DeflectionS1);
    ReportList1.Items[ 7, 2] := TryFormatFloat('s2    = %s mm',  's2    = ---', SOLVER.DeflectionS2);
    ReportList1.Items[ 8, 2] := TryFormatFloat('sh    = %s mm',  'sh    = ---', SOLVER.DeflectionSh);
    ReportList1.Items[ 9, 2] := TryFormatFloat('sn    = %s mm',  'sn    = ---', SOLVER.DeflectionSn);
    ReportList1.Items[10, 2] := TryFormatFloat('sc    = %s mm',  'sc    = ---', SOLVER.DeflectionSc);
    ReportList1.Items[11, 2] := '';
    ReportList1.Items[12, 2] := TryFormatFloat('F1    = %s N',   'F1    = ---', SOLVER.LoadF1) + TryFormatFloat(' ± %s N', '', TOL.LoadF1Tolerance);
    ReportList1.Items[13, 2] := TryFormatFloat('F2    = %s N',   'F2    = ---', SOLVER.LoadF2) + TryFormatFloat(' ± %s N', '', TOL.LoadF2Tolerance);
    ReportList1.Items[14, 2] := TryFormatFloat('Fn    = %s N',   'Fn    = ---', SOLVER.LoadFn);
    ReportList1.Items[15, 2] := TryFormatFloat('Fc    = %s N',   'Fc    = ---', SOLVER.LoadFc);
    ReportList1.Items[16, 2] := '';
    ReportList1.Items[17, 2] := TryFormatFloat('tauk1 = %s MPa', 'tauk1 = ---', SOLVER.TorsionalStressTauk1);
    ReportList1.Items[18, 2] := TryFormatFloat('tauk2 = %s MPa', 'tauk2 = ---', SOLVER.TorsionalStressTauk2);
    ReportList1.Items[19, 2] := TryFormatFloat('taukh = %s MPa', 'taukh = ---', SOLVER.TorsionalStressTaukh);
    ReportList1.Items[20, 2] := '';
    ReportList1.Items[21, 2] := TryFormatFloat('tauoz = %s MPa', 'tauoz = ---', SOLVER.AdmDynamicTorsionalStressTauoz);
    ReportList1.Items[22, 2] := TryFormatFloat('tauhz = %s MPa', 'tauhz = ---', SOLVER.AdmDynamicTorsionalStressRangeTauhz);
    ReportList1.Items[23, 2] := '';

    Bit[0].SetSize(ReportList1.Width + ReportList1.Spacer, aScreen.Height);
    ReportList1.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);
    Bit[1].SetSize(aScreen.Width - Bit[0].Width, aScreen.Height);
    if Quick2MenuItem.Checked then ForceDiagram  .Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
    if Quick3MenuItem.Checked then GoodmanDiagram.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);
    Bit[0].Draw(aScreen.Canvas, Bit[1].Width, 0, False);
    Bit[1].Draw(aScreen.Canvas, 0, 0, False);
    ReportList1.Destroy;

    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil;
  end;

  if Production2MenuItem.Checked then
  begin
    SetLength(Bit, 1);
    Bit[0] := TBGRABitmap.Create;
    Bit[0].SetSize(aScreen.Width, aScreen.Height);
    begin
      SVG := TBGRASVG.Create;
      SVG.LoadFromResource('TEMPLATE');
      SVG.AsUTF8String := CreateProductionDrawing(SVG.AsUTF8String, aSetting);
      SVG.StretchDraw(Bit[0].Canvas2D, taLeftJustify, tlCenter, 0, 0, Bit[0].Width, Bit[0].Height, False);
      SVG.Destroy;
    end;
    Bit[0].InvalidateBitmap;
    Bit[0].Draw(aScreen.Canvas, 0, 0, False);
    Bit[0].Destroy;
    Bit := nil;
  end;

  ForceDiagram.Destroy;
  GoodmanDiagram.Destroy;
  BucklingDiagram.Destroy;
  ShearModulusDiagram.Destroy;
  YoungModulusDiagram.Destroy;
  Load1Diagram.Destroy;
  Load2Diagram.Destroy;
  MessageList.Destroy;
  QualityTable.Destroy;
  ReportTable.Destroy;
  ReportList0.Destroy;
  SpringDrawing.Destroy;

  VirtualScreenResize(nil);
  VirtualScreen.RedrawBitmap;
end;

function TMainForm.CreateForceDisplacementDiagram(const aScreenScale: double; aSetting: TIniFile): TForceDisplacementDiagram;
begin
  Result             := TForceDisplacementDiagram.Create('ForceDisplacementDiagram', aSetting);
  Result.F1          := SOLVER.LoadF1;
  Result.F2          := SOLVER.LoadF2;
  Result.Fn          := SOLVER.LoadFn;
  Result.Fc          := SOLVER.LoadFc;
  Result.s1          := SOLVER.DeflectionS1;
  Result.s2          := SOLVER.DeflectionS2;
  Result.sn          := SOLVER.DeflectionSn;
  Result.sc          := SOLVER.DeflectionSc;
  Result.ToleranceF1 := TOL.LoadF1Tolerance;
  Result.ToleranceF1 := TOL.LoadF2Tolerance;
  Result.Spacer      := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom        := aScreenScale;
end;

function TMainForm.CreateGoodmanDiagram(const aScreenScale: double; aSetting: TIniFile): TGoodmanDiagram;
begin
  Result                         := TGoodmanDiagram.Create('GoodmanDiagram', aSetting);
  Result.Caption                 := Format('Goodman Diagram: %s', [MAT.Items[MAT.ItemIndex]]);
  Result.Tauz                    := SOLVER.AdmStaticTorsionalStressTauz;
  Result.Tauk1                   := SOLVER.TorsionalStressTauk1;
  Result.Tauk2                   := SOLVER.TorsionalStressTauk2;
  Result.TorsionalStressTauYield := MAT.TorsionalStressTauYield;
  Result.TorsionalStressTauOE7   := MAT.TorsionalStressTauOE7;
  Result.TorsionalStressTauOE6   := MAT.TorsionalStressTauOE6;
  Result.TorsionalStressTauOE5   := MAT.TorsionalStressTauOE5;
  Result.TorsionalStressTauUE7   := MAT.TorsionalStressTauUE7;
  Result.TorsionalStressTauUE6   := MAT.TorsionalStressTauUE6;
  Result.TorsionalStressTauUE5   := MAT.TorsionalStressTauUE5;
  Result.NumOfCyclesE7           := FloatToStrF(MAT.NumOfCyclesE7, ffGeneral, 4, 0) + ' Cycles';
  Result.NumOfCyclesE6           := FloatToStrF(MAT.NumOfCyclesE6, ffGeneral, 4, 0);
  Result.NumOfCyclesE5           := FloatToStrF(MAT.NumOfCyclesE5, ffGeneral, 4, 0);
  Result.Tauk1Tolerance          := SOLVER.GetTauk(TOL.LoadF1Tolerance);
  Result.Tauk2Tolerance          := SOLVER.GetTauk(TOL.LoadF2Tolerance);
  Result.Spacer                  := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom                    := aScreenScale;
end;

function TMainForm.CreateBucklingDiagram(const aScreenScale: double; aSetting: TIniFile): TBucklingDiagram;
begin
  Result        := TBucklingDiagram.Create('BucklingDiagram', aSetting);
  Result.Dm     := SOLVER.Dm;
  Result.E      := SOLVER.YoungModulus;
  Result.G      := SOLVER.ShearModulus;
  Result.L0     := SOLVER.LengthL0;
  Result.nu     := SOLVER.SeatingCoefficent;
  Result.s1     := SOLVER.DeflectionS1;
  Result.s2     := SOLVER.DeflectionS2;
  Result.sc     := SOLVER.DeflectionSc;
  Result.sk     := SOLVER.DeflectionSk;
  Result.Spacer := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom   := aScreenScale;
end;

function TMainForm.CreateLoad1Diagram(const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
begin
  Result                 := TLinearTemperatureDiagram.Create('LinearTemperatureDiagram', aSetting);
  Result.Caption         := 'F1-T Diagram';
  Result.HorizontalLabel := 'T [C°]';
  Result.VerticalLabel   := 'F1 [N]';
  Result.Temperature     := MAT.Tempetature;
  Result.Temperature0    := 0;
  Result.Temperature1    := 500;
  Result.Value           := SOLVER.LoadF1;
  Result.Value0          := SOLVER.GetF1(Result.Temperature0);
  Result.Value1          := SOLVER.GetF1(Result.Temperature1);
  Result.Spacer          := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom            := aScreenScale;
end;

function TMainForm.CreateLoad2Diagram(const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
begin
  Result                 := TLinearTemperatureDiagram.Create('LinearTemperatureDiagram', aSetting);
  Result.Caption         := 'F2-T Diagram';
  Result.HorizontalLabel := 'T [C°]';
  Result.VerticalLabel   := 'F2 [N]';
  Result.Temperature     := MAT.Tempetature;
  Result.Temperature0    := 0;
  Result.Temperature1    := 500;
  Result.Value           := SOLVER.LoadF2;
  Result.Value0          := SOLVER.GetF2(Result.Temperature0);
  Result.Value1          := SOLVER.GetF2(Result.Temperature1);
  Result.Spacer          := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom            := aScreenScale;
end;

function TMainForm.CreateReportTable(const aScreenScale: double; aSetting: TIniFile): TReportTable;
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

function TMainForm.CreateReportList0(const aScreenScale: double; aSetting: TIniFile): TReportTable;
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

function TMainForm.CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSectionSpringDrawing;
begin
  Result            := TSectionSpringDrawing.Create('SpringDrawing', aSetting);
  Result.d          := SOLVER.WireDiameter;
  Result.Dm         := SOLVER.Dm;
  Result.Lc         := SOLVER.LengthLc;
  Result.Lx         := DrawingForm.SpringLength.Value;
  Result.Text       := TryFormatFloat('L = %s', 'L = ---', Result.Lx);
  Result.n          := SOLVER.ActiveColis;
  Result.nt1        := GeometryForm.InactiveCoil1.Value;
  Result.nt2        := GeometryForm.InactiveCoil2.Value;
  Result.ClockWise  := True;
  Result.Fit        := True;
  Result.GroundEnds := SOLVER.GroundEnds;
  Result.Spacer     := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom       := aScreenScale;
end;

function TMainForm.CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
begin
//Solve();
  Result := Tx;
  Result := StringReplace(Result, '@0.00', Format('e1=%s mm', [TryFloatToText(SOLVER.EccentricityE1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.01', Format('e2=%s mm', [TryFloatToText(SOLVER.EccentricityE2)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.02', Format('d=%s mm',  [TryFloatToText(SOLVER.WireDiameter  )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.03', Format('Di=%s mm', [TryFloatToText(SOLVER.Di            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.04', Format('Dm=%s mm', [TryFloatToText(SOLVER.Dm            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.05', Format('De=%s mm', [TryFloatToText(SOLVER.De            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.06', Format('L0=%s mm', [TryFloatToText(SOLVER.LengthL0      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.07', Format('L1=%s mm', [TryFloatToText(SOLVER.LengthL1      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.08', Format('L2=%s mm', [TryFloatToText(SOLVER.LengthL2      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.09', Format('Ln=%s mm', [TryFloatToText(SOLVER.LengthLn      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.10', Format('Lc=%s mm', [TryFloatToText(SOLVER.LengthLc      )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.11', Format('F1=%s',    [TryFloatToText(SOLVER.LoadF1        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.13', Format('F2=%s',    [TryFloatToText(SOLVER.LoadF2        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.15', Format('Fn=%s',    [TryFloatToText(SOLVER.LoadFn        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.17', Format('Fc=%s',    [TryFloatToText(SOLVER.LoadFc        )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.12', Format('Tauk1=%s', [TryFloatToText(SOLVER.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.14', Format('Tauk2=%s', [TryFloatToText(SOLVER.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.16', Format('Taukn=%s', [TryFloatToText(SOLVER.TorsionalStressTaukn)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.18', Format('Tauc=%s',  [TryFloatToText(SOLVER.TorsionalStressTauc )]), [rfReplaceAll, rfIgnoreCase]);

  if SOLVER.ClosedEnds and (SOLVER.GroundEnds = True) then
  begin
    Result := StringReplace(Result, '@0.20', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.21', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.22', ' ', [rfReplaceAll, rfIgnoreCase]);
  end;

  if SOLVER.ClosedEnds and (SOLVER.GroundEnds = False) then
  begin
    Result := StringReplace(Result, '@0.20', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.21', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.22', ' ', [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := StringReplace(Result, '@1.0', Format('n=%s',      [TryFloatToText(SOLVER.ActiveColis)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@1.1', Format('nt=%s',     [TryFloatToText(SOLVER.TotalCoils )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@2.0', Format('R=%s N/mm', [TryFloatToText(SOLVER.SpringRateR)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.DirectionCoils.ItemIndex of
    0: ;
    1: Result := StringReplace(Result, '@3.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@3.1', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@3.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@3.1', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@4.0', Format('Dd=%s mm', [TryFloatToText(SOLVER.DiMin)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@4.1', Format('Dh=%s mm', [TryFloatToText(SOLVER.DeMax)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.BurringEnds.ItemIndex of
    0: Result := StringReplace(Result, '@5.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    1: Result := StringReplace(Result, '@5.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@5.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@5.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.2', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@6.0', Format('fe=%s Hz', [TryFloatToText(SOLVER.DeMax)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@7.0', Format('%s C° / %s C°',
    [TryFloatToText(MAT.TempetatureMin),
     TryFloatToText(MAT.TempetatureMax), MAT.TempetatureMax
    ]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.WireSurface.ItemIndex of
    0: Result := StringReplace(Result, '@8.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    1: Result := StringReplace(Result, '@8.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@8.2', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@8.3', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@8.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@8.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@8.2', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@8.3', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@9.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@10.0', MAT.Items[MAT.ItemIndex], [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@10.1', Format('tauz=%s Mpa', [TryFloatToText(SOLVER.AdmStaticTorsionalStressTauz)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@10.2', Format('G=%s Mpa', [TryFloatToText(MAT.ShearModulusG20)]), [rfReplaceAll, rfIgnoreCase]);

  case TOL.DmQualityGrade of
    1: Result := StringReplace(Result, '@11.00', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.01', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.02', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.00', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.01', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.02', ' ', [rfReplaceAll, rfIgnoreCase]);

  case TOL.L0QualityGrade of
    1: Result := StringReplace(Result, '@11.10', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.11', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.12', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.10', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.11', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.12', ' ', [rfReplaceAll, rfIgnoreCase]);

  case TOL.F1QualityGrade of
    1: Result := StringReplace(Result, '@11.20', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.21', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.22', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.20', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.21', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.22', ' ', [rfReplaceAll, rfIgnoreCase]);

  case TOL.F2QualityGrade of
    1: Result := StringReplace(Result, '@11.30', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.31', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.32', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.30', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.31', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.32', ' ', [rfReplaceAll, rfIgnoreCase]);

  case TOL.E1QualityGrade of
    1: Result := StringReplace(Result, '@11.40', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.41', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.42', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.40', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.41', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.42', ' ', [rfReplaceAll, rfIgnoreCase]);

  case TOL.E2QualityGrade of
    1: Result := StringReplace(Result, '@11.50', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.51', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.52', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.50', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.51', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.52', ' ', [rfReplaceAll, rfIgnoreCase]);

  if ProductionForm.L0        .Checked then Result := StringReplace(Result, '@12.0', 'X', [rfReplaceAll, rfIgnoreCase]);
  if ProductionForm.nAndd     .Checked then Result := StringReplace(Result, '@12.1', 'X', [rfReplaceAll, rfIgnoreCase]);
  if ProductionForm.nAndDeDi  .Checked then Result := StringReplace(Result, '@12.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  if ProductionForm.L0nAndd   .Checked then Result := StringReplace(Result, '@12.3', 'X', [rfReplaceAll, rfIgnoreCase]);
  if ProductionForm.L0nAndDeDi.Checked then Result := StringReplace(Result, '@12.4', 'X', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@12.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@12.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@12.2', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@12.3', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@12.4', ' ', [rfReplaceAll, rfIgnoreCase]);

  if ProductionForm.LengthLs.Value > 0 then
  begin
    Result := StringReplace(Result, '@13.0', Format('Ls=%s mm', [TryFloatToText(ProductionForm.LengthLs.Value)]), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.2', ' ', [rfReplaceAll, rfIgnoreCase]);
  end else
  begin
    Result := StringReplace(Result, '@13.0', 'Ls= --- mm', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.1', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := StringReplace(Result, '@14.0', TextForm.Note1        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@14.1', TextForm.Note2        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.0', TextForm.DrawingName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.1', TextForm.DrawingNumber.Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.2', TextForm.CompanyName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.3', ApplicationVer,              [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '#ff0',    aSetting.ReadString('Printer', 'Page.Color4', '#ff0'),     [rfReplaceAll, rfIgnoreCase]); // Yellow line
  Result := StringReplace(Result, '#f00',    aSetting.ReadString('Printer', 'Page.Color5', '#f00'),     [rfReplaceAll, rfIgnoreCase]); // Red    line
  Result := StringReplace(Result, '#0f0',    aSetting.ReadString('Printer', 'Page.Color6', '#0f0'),     [rfReplaceAll, rfIgnoreCase]); // Green  line

  Result := StringReplace(Result, '#ffff00', aSetting.ReadString('Printer', 'Page.Color1', '#ffff00'),  [rfReplaceAll, rfIgnoreCase]); // Yellow
  Result := StringReplace(Result, '#ff0000', aSetting.ReadString('Printer', 'Page.Color2', '#ff0000'),  [rfReplaceAll, rfIgnoreCase]); // Red
  Result := StringReplace(Result, '#00ff00', aSetting.ReadString('Printer', 'Page.Color3', '#00ff00'),  [rfReplaceAll, rfIgnoreCase]); // Green
end;

