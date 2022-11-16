{ EN13906-1 Helical Compression Spring Designer

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

unit MainFrm;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRABitmapTypes, BGRAVirtualScreen, BGRASVG, BGRATextFX, BGRAUnits,
  Classes, Controls, Dialogs, IniFiles, EN13906_1, EN10270, EN15800, ExtCtrls, Forms,
  GraphBase, Graphics, Math, Menus, StdCtrls, Spin, ExtDlgs, PrintersDlgs,
  SysUtils, LResources;

type

  { TMainForm }

  TMainForm = class(TForm)
    DrawingTextMenuItem: TMenuItem;
    GeometryMenuItem: TMenuItem;
    MaterialMenuItem: TMenuItem;
    DrawMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    IncreaseFontMenuItem: TMenuItem;
    DecreaseFontMenuItem: TMenuItem;
    ExportMenuItem: TMenuItem;
    ExportReportMenuItem: TMenuItem;
    ExportProductionMenuItem: TMenuItem;
    PageSetupMenuItem: TMenuItem;
    PageSetupDialog: TPageSetupDialog;
    ProductionMenuItem: TMenuItem;
    Production2MenuItem: TMenuItem;
    SectionMenuItem: TMenuItem;
    CustomSectionMenuItem: TMenuItem;
    ProfileMenuItem: TMenuItem;
    CustomProfileMenuItem: TMenuItem;
    Separator10: TMenuItem;
    Separator11: TMenuItem;
    ShearModulusMenuItem: TMenuItem;
    YoungModulusMenuItem: TMenuItem;
    F1MenuItem: TMenuItem;
    F2MenuItem: TMenuItem;
    ResetFontMenuItem: TMenuItem;
    SavePictureDialog: TSavePictureDialog;
    Separator3: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    Zoom250MenuItem: TMenuItem;
    Zoom225MenuItem: TMenuItem;
    ZoomMenuItem: TMenuItem;
    Zoom100MenuItem: TMenuItem;
    Zoom125MenuItem: TMenuItem;
    Zoom150MenuItem: TMenuItem;
    Zoom175MenuItem: TMenuItem;
    Zoom200MenuItem: TMenuItem;
    QualityMenuItem: TMenuItem;
    VirtualScreen: TBGRAVirtualScreen;
    MainMenu: TMainMenu;
    FileMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    TempMenuItem: TMenuItem;
    PrintDialog: TPrintDialog;
    SaveAsMenuItem: TMenuItem;
    Quick1MenuItem: TMenuItem;
    Quick2MenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    ForceMenuItem: TMenuItem;
    GoodmanMenuItem: TMenuItem;
    BucklingMenuItem: TMenuItem;
    ReportMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    PrintMenuItem: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    Quick3MenuItem: TMenuItem;
    Separator4: TMenuItem;
    Separator2: TMenuItem;
    SaveMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    ApplicationMenuItem: TMenuItem;
    ViewMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    Design: TMenuItem;
    DocsMenuItem: TMenuItem;
    MenuItem9: TMenuItem;
    Separator1: TMenuItem;
    procedure CloseMenuItemClick(Sender: TObject);
    procedure ExportMenuItemClick(Sender: TObject);
    procedure ExportProductionMenuItemClick(Sender: TObject);
    procedure ExportReportMenuItemClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure GeometryMenuItemClick(Sender: TObject);

    procedure MaterialMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure ModifyFontMenuItemClick(Sender: TObject);

    procedure PrintMenuItemClick(Sender: TObject);
    procedure ProductionMenuItemClick(Sender: TObject);
    procedure QualityMenuItemClick(Sender: TObject);

    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ApplicationMenuItemClick(Sender: TObject);
    procedure CustomSectionMenuItemClick(Sender: TObject);
    procedure VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure DrawingTextMenuItemClick(Sender: TObject);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure ReportMenuItemClick(Sender: TObject);
    procedure VirtualScreenResize(Sender: TObject);
    procedure ZoomMenuItemClick(Sender: TObject);
  private
    FileName: string;
    ClientFile: TIniFile;
    PrinterFile: TIniFile;
    MoveX, MoveY: longint;
    MouseIsDown: boolean;
    Px, Py: longint;
    ScreenImage: TBGRABitmap;
    ScreenImageWidth: longint;
    ScreenImageHeight: longint;
    ScreenColor: TBGRAPixel;
    ScreenScale: double;

    function CreateForceDisplacementDiagram(const aScreenScale: double; aSetting: TIniFile): TForceDisplacementDiagram;
    function CreateGoodmanDiagram          (const aScreenScale: double; aSetting: TIniFile): TGoodmanDiagram;
    function CreateBucklingDiagram         (const aScreenScale: double; aSetting: TIniFile): TBucklingDiagram;
    function CreateShearModulusDiagram     (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateYoungModulusDiagram     (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateLoad1Diagram            (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateLoad2Diagram            (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateMessageList             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateReportTable             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateQualityTable            (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateReportList0             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateSpringDrawing           (const aScreenScale: double; aSetting: TIniFile): TSectionSpringDrawing;
  public
    //procedure LoadClient(IniFile: TIniFile);
    //procedure SaveClient(IniFile: TIniFile);
    function CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;
    function ProductionDrawing(const Tx: string; aSetting: TIniFile): string;
    procedure PaintTo(var aScreen: TBGRABitmap; aScreenColor: TBGRAPixel; aScreenScale: double; aSetting: TIniFile);
    procedure Clear;
    procedure Solve;
  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  AboutFrm, ApplicationFrm, DrawingFrm, DrawingTextFrm, GeometryFrm, LCLIntf, LCLType,
  MaterialFrm, Printers, ProductionFrm, QualityFrm, ReportFrm, UtilsBase;

// Solve routine

procedure TMainForm.Solve;
begin
  SpringSolver.Clear;
  // Geometry
  SpringSolver.WireDiameter := GetMillimeters(GeometryForm.WireDiameter.Value, GeometryForm.WireDiameterUnit.ItemIndex);
  case GeometryForm.CoilDiameterIndex.ItemIndex of
     0: SpringSolver.Dm := GetMillimeters(GeometryForm.CoilDiameter.Value, GeometryForm.CoilDiameterUnit.ItemIndex) + SpringSolver.WireDiameter; // Input Di
     1: SpringSolver.Dm := GetMillimeters(GeometryForm.CoilDiameter.Value, GeometryForm.CoilDiameterUnit.ItemIndex);                             // Input Dm
     2: SpringSolver.Dm := GetMillimeters(GeometryForm.CoilDiameter.Value, GeometryForm.CoilDiameterUnit.ItemIndex) - SpringSolver.WireDiameter; // Input De
  end;
  SpringSolver.ActiveColis := GeometryForm.ActiveCoil.Value;
  SpringSolver.TotalCoils  := GeometryForm.ActiveCoil.Value + GeometryForm.InactiveCoil1.Value + GeometryForm.InactiveCoil2.Value;

  SpringSolver.ClosedEnds := GeometryForm.EndCoilType.ItemIndex in [0, 1];
  SpringSolver.GroundEnds := GeometryForm.EndCoilType.ItemIndex in [   1];

  SpringSolver.LengthL0 := GetMillimeters(GeometryForm.LengthL0.Value, GeometryForm.LengthL0Unit.ItemIndex);
  SpringSolver.LengthL1 := GetMillimeters(GeometryForm.LengthL1.Value, GeometryForm.LengthL1Unit.ItemIndex);
  SpringSolver.LengthL2 := GetMillimeters(GeometryForm.LengthL2.Value, GeometryForm.LengthL2Unit.ItemIndex);
  // Material Grade & Manufactoring
  MaterialForm.Change(nil);
  if MaterialForm.Material.Text = '' then
  begin
    SpringSolver.YoungModulus := GetMegaPascal(MaterialForm.YoungModulus.Value, MaterialForm.YoungModulusUnit.ItemIndex);
    SpringSolver.ShearModulus := GetMegaPascal(MaterialForm.ShearModulus.Value, MaterialForm.ShearModulusUnit.ItemIndex);
  end else
  begin
    SpringSolver.YoungModulus := MAT.YoungModulusE;
    SpringSolver.ShearModulus := MAT.ShearModulusG;
  end;
  SpringSolver.TensileStrengthRm  := GetMegaPascal(MaterialForm.TensileStrength.Value, MaterialForm.TensileStrengthUnit.ItemIndex);
  SpringSolver.DensityRho         := GetDensity   (MaterialForm.MaterialDensity.Value, MaterialForm.MaterialDensityUnit.ItemIndex);
  case MaterialForm.CoilingType.ItemIndex of
    0: SpringSolver.ColdCoiled := True;
    1: SpringSolver.ColdCoiled := False;
  end;
  // Quality Grade
  SpringSolver.ToleranceWireDiameter := GetMillimeters(QualityForm.ToleranceWireDiameter.Value, QualityForm.ToleranceWireDiameterUnit.ItemIndex);
  case QualityForm.ToleranceCoilDiameter.ItemIndex of
    0: SpringSolver.QualityGradeDm := 1;
    1: SpringSolver.QualityGradeDm := 2;
    2: SpringSolver.QualityGradeDm := 3;
  end;
  case QualityForm.ToleranceLengthL0.ItemIndex of
    0: SpringSolver.QualityGradeL0 := 1;
    1: SpringSolver.QualityGradeL0 := 2;
    2: SpringSolver.QualityGradeL0 := 3;
  end;
  case QualityForm.ToleranceLoadF1.ItemIndex of
    0: SpringSolver.QualityGradeF1 := 1;
    1: SpringSolver.QualityGradeF1 := 2;
    2: SpringSolver.QualityGradeF1 := 3;
  end;
  case QualityForm.ToleranceLoadF2.ItemIndex of
    0: SpringSolver.QualityGradeF2 := 1;
    1: SpringSolver.QualityGradeF2 := 2;
    2: SpringSolver.QualityGradeF2 := 3;
  end;
  case QualityForm.ToleranceEccentricitye1.ItemIndex of
    0: SpringSolver.QualityGradee1 := 1;
    1: SpringSolver.QualityGradee1 := 2;
    2: SpringSolver.QualityGradee1 := 3;
  end;
  case QualityForm.ToleranceEccentricitye2.ItemIndex of
    0: SpringSolver.QualityGradee2 := 1;
    1: SpringSolver.QualityGradee2 := 2;
    2: SpringSolver.QualityGradee2 := 3;
  end;
  // Analisys
  case ApplicationForm.LoadType.ItemIndex of
    0: SpringSolver.DynamicLoad := True;
    1: SpringSolver.DynamicLoad := False;
  end;
  SpringSolver.SeatingCoefficent := TryTextToFloat(ApplicationForm.SeatingCoefficent.Text);
  // Solve
  MainForm.FormPaint(nil);
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  PaperName: string;
  {$ifopt D+}
  Logo: TBGRABitmap;
  {$endif}
begin
  Caption     := ApplicationVer;
  ClientFile  := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'client.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);
  PrinterFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'printer.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  Clear;
  ScreenImage       := TBGRABitmap.Create;
  ScreenImageWidth  := Screen.Width  - (Width  - VirtualScreen.Width);
  ScreenImageHeight := Screen.Height - (Height - VirtualScreen.Height + LCLIntf.GetSystemMetrics(SM_CYCAPTION));
  ScreenColor.FromString(ClientFile.ReadString('Custom', 'Background Color', 'White'));
  VirtualScreen.Color := ScreenColor;

  PaperName := ClientFile.ReadString ('Printer', 'Page.Name', '');
  if PaperName <> '' then
  begin
    Printer.PaperSize.PaperName  := PaperName;
    Printer.Orientation          := TPrinterOrientation(ClientFile.ReadInteger('Printer', 'Page.Orientation',  0 ));
    PageSetupDialog.MarginTop    := TryTextToInt       (ClientFile.ReadString('Printer', 'Page.MarginTop',    '0'));
    PageSetupDialog.MarginLeft   := TryTextToInt       (ClientFile.ReadString('Printer', 'Page.MarginLeft',   '0'));
    PageSetupDialog.MarginRight  := TryTextToInt       (ClientFile.ReadString('Printer', 'Page.MarginRight',  '0'));
    PageSetupDialog.MarginBottom := TryTextToInt       (ClientFile.ReadString('Printer', 'Page.MarginBottom', '0'));
  end;
  WindowState := wsMaximized;
  FormPaint(nil);

  {$ifopt D+}
  Logo := TBGRABitmap.Create;
  Logo.SetSize(2560, 2048);
  DrawLogo2(Logo.Canvas, Logo.Width, Logo.Height);
  Logo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'BACKGROUND.png');
  Logo.Destroy;
  {$endif}
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClientFile.Destroy;
  PrinterFile.Destroy;
  ScreenImage.Destroy;
end;

procedure TMainForm.Clear;
var
  i: longint;
begin
  MouseIsDown := False;
  MoveX := 0;
  MoveY := 0;
  ScreenScale   := 1.00;

  for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
  for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
  for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;
  for i := 0 to ZoomMenuitem.Count -1 do ZoomMenuItem.Items[i].Checked := False;
  for i := 0 to DocsMenuItem.Count -1 do DocsMenuItem.Items[i].Checked := False;
  FileName := '';
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    ScreenImageWidth  := VirtualScreen.Width;
    ScreenImageHeight := VirtualScreen.Height;
  end;
end;

(*
procedure TMainForm.Load(IniFile: TIniFile);
begin

end;

procedure TMainForm.Save(IniFile: TIniFile);
begin

end;

*)

// Mouse Events

procedure TMainForm.VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    Px := X - MoveX;
    Py := Y - MoveY;
  end;
end;

procedure TMainForm.VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if MouseIsDown then
  begin
    MoveX := X - Px;
    MoveY := Y - Py;
    VirtualScreenResize(Sender);
    VirtualScreen.RedrawBitmap;
  end;
end;

procedure TMainForm.VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseIsDown := False;
end;

// Virtual Screen Events

procedure TMainForm.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(MoveX, MoveY, ScreenImage, dmSet);
end;

procedure TMainForm.VirtualScreenResize(Sender: TObject);
begin
  if MoveX > 0 then MoveX := 0;
  if MoveY > 0 then MoveY := 0;

  if MoveX < VirtualScreen.Width  - ScreenImage.Width  then MoveX := VirtualScreen.Width  - ScreenImage.Width;
  if MoveY < VirtualScreen.Height - ScreenImage.Height then MoveY := VirtualScreen.Height - ScreenImage.Height;
end;

// Menu File //

procedure TMainForm.NewMenuItemClick(Sender: TObject);
begin
  CloseMenuItemClick(Sender);
  if (DrawingTextForm.ShowModal = mrOk) and
     (GeometryForm   .ShowModal = mrOk) and
     (MaterialForm   .ShowModal = mrOk) and
     (QualityForm    .ShowModal = mrOk) and
     (ProductionForm .ShowModal = mrOk) and
     (ApplicationForm.ShowModal = mrOk) then
  begin
    Quick1MenuItem .Checked := True;
    Zoom100MenuItem.Checked := True;
    Solve();
  end;
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  IniFile: TIniFile;
begin
  OpenDialog.Filter := 'Spring ONE file (*.springone)|*.springone|All files (*.*)|*.*|;';
  if OpenDialog.Execute then
  begin
    CloseMenuItemClick(Sender);
    FileName := OpenDialog.FileName;
    Caption  := Format(ApplicationVer + ' - %s', [FileName]);
    begin
      IniFile := TIniFile.Create(FileName);
      IniFile.FormatSettings.DecimalSeparator := '.' ;
      DrawingTextForm.Load(IniFile);
      GeometryForm   .Load(IniFile);
      MaterialForm   .Load(IniFile);
      QualityForm    .Load(IniFile);
      ProductionForm .Load(IniFile);
      ApplicationForm.Load(IniFile);
      IniFile.Destroy;
    end;
    Quick1MenuItem .Checked := True;
    Zoom100MenuItem.Checked := True;
    Solve();
  end;
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
var
  IniFile: TIniFile;
begin
  if FileName = '' then
  begin
    SaveAsMenuItemClick(Sender);
  end else
  begin
    IniFile := TIniFile.Create(FileName);
    DrawingTextForm.Save(IniFile);
    GeometryForm   .Save(IniFile);
    MaterialForm   .Save(IniFile);
    QualityForm    .Save(IniFile);
    ProductionForm .Save(IniFile);
    ApplicationForm.Save(IniFile);
    IniFile.Destroy;
  end;
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
begin
  SaveDialog.Filter := 'Spring ONE file (*.springone)|*.springone|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    Caption  := Format(ApplicationVer + ' - %s', [FileName]);
    SaveMenuItemClick(Sender);
  end;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  MainForm       .Clear;
  DrawingTextForm.Clear;
  GeometryForm   .Clear;
  MaterialForm   .Clear;
  QualityForm    .Clear;
  ProductionForm .Clear;
  ApplicationForm.Clear;

  Caption := ApplicationVer;
//Quick1MenuItem .Checked := True;
  Zoom100MenuItem.Checked := True;
  Solve();
end;

function TMainForm.CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;
var
  PageColor: TBGRAPixel;
begin
  Result := TBGRABitmap.Create;
  Result.SetSize(
    Trunc(aWidth  *aScale),
    Trunc(aHeight *aScale));
  PageColor.FromString(aSetting.ReadString('Custom', 'Background Color', 'White'));
  PaintTo(Result, PageColor, aScale, aSetting);
end;

procedure TMainForm.ExportMenuItemClick(Sender: TObject);
var
  Page: TBGRABitmap;
begin
  Solve();
  SavePictureDialog.Filter :=
    'Graphics (*.png;*.xpm;*.bmp;*.jpeg;*.jpg;)|*.png;*.xpm;*.bmp;*.jpeg;*.jpg|' +
    'PNG Files (*.png)|*.png|' + 'Pixmap Files (*.xpm)|*.xpm|' + 'Bitmap Files (*.bmp)|*.bmp)|' +
    'JPEG Files (*.jpeg;*.jpg;)|*.jpeg;*.jpg|' + 'Tutti i file (*.*)|*.*|;';
  if SavePictureDialog.Execute then
  begin
    if Production2MenuItem.Checked then
      Page := CreatePage(Trunc(ScreenImageHeight*0.707), ScreenImageHeight, 4.5, PrinterFile)
    else
      Page := CreatePage(ScreenImageWidth, ScreenImageHeight, 4.0, PrinterFile);
    Page.SaveToFile(SavePictureDialog.FileName);
    Page.Destroy;
  end;
end;

procedure TMainForm.PageSetupMenuItemClick(Sender: TObject);
begin
  if PageSetupDialog.Execute then
  begin
    ClientFile.WriteString ('Printer', 'Page.Name',         Printer.PaperSize.PaperName );
    ClientFile.WriteInteger('Printer', 'Page.Orientation',  longint(Printer.Orientation));
    ClientFile.WriteInteger('Printer', 'Page.MarginTop',    PageSetupDialog.MarginTop   );
    ClientFile.WriteInteger('Printer', 'Page.MarginLeft',   PageSetupDialog.MarginLeft  );
    ClientFile.WriteInteger('Printer', 'Page.MarginRight',  PageSetupDialog.MarginRight );
    ClientFile.WriteInteger('Printer', 'Page.MarginBottom', PageSetupDialog.MarginBottom);
  end;
end;

procedure TMainForm.PrintMenuItemClick(Sender: TObject);
var
  OffSetX: longint;
  OffSetY: longint;
  Page: TBGRABitmap;
  Scale: double;
begin
  Solve();
  if PrintDialog.Execute then
  begin
    if Production2MenuItem.Checked then
      Scale := Printer.PageHeight / Printer.PageWidth
    else
      Scale := ScreenImageHeight  / ScreenImageWidth;

    Page := CreatePage(ScreenImageWidth, Trunc(ScreenImageWidth*Scale), 4.0, PrinterFile);
    Printer.BeginDoc;
    Scale := Min(Printer.PageWidth /Page.Width,
                 Printer.PageHeight/Page.Height);

    OffSetX := (Printer.PageWidth  - Trunc(Page.Width *Scale)) div 2;
    OffSetY := (Printer.PageHeight - Trunc(Page.Height*Scale)) div 2;
    Printer.Canvas.StretchDraw(Rect(OffSetX, OffSetY,
      OffSetX + Trunc(Page.Width *Scale),
      OffSetY + Trunc(Page.Height*Scale)),
      Page.Bitmap);
    Printer.EndDoc;
    Page.Destroy;
  end;
end;

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

// Menu Edit //

procedure TMainForm.DrawingTextMenuItemClick(Sender: TObject);
var
  DrawingName: string;
  DrawingNumber: string;
  AuthorName: string;
  CompanyName: string;
  Note1: string;
  Note2: string;
begin
  DrawingName   := DrawingTextForm.DrawingName  .Text;
  DrawingNumber := DrawingTextForm.DrawingNumber.Text;
  AuthorName    := DrawingTextForm.AuthorName   .Text;
  CompanyName   := DrawingTextForm.CompanyName  .Text;
  Note1         := DrawingTextForm.Note1        .Text;
  Note2         := DrawingTextForm.Note2        .Text;
  if DrawingTextForm.ShowModal <> mrOk then
  begin
    DrawingTextForm.DrawingName  .Text := DrawingName;
    DrawingTextForm.DrawingNumber.Text := DrawingNumber;
    DrawingTextForm.AuthorName   .Text := AuthorName;
    DrawingTextForm.CompanyName  .Text := CompanyName;
    DrawingTextForm.Note1        .Text := Note1;
    DrawingTextForm.Note2        .Text := Note2;
  end;
  Solve();
end;

procedure TMainForm.GeometryMenuItemClick(Sender: TObject);
var
  WireDiameter: double;
  WireDiameterUnit: longint;
  CoilDiameterIndex: longint;
  CoilDiameter: double;
  CoilDiameterUnit: longint;
  AnctiveCoil: double;
  InactiveCoil1: double;
  InactiveCoil2: double;
  LengthL0: double;
  LengthL0Unit: longint;
  LengthL1: double;
  LengthL1Unit: longint;
  LengthL2: double;
  LengthL2Unit: longint;
  EndCoilType: longint;
begin
  WireDiameter      := GeometryForm.WireDiameter     .Value;
  WireDiameterUnit  := GeometryForm.WireDiameterUnit .ItemIndex;
  CoilDiameterIndex := GeometryForm.CoilDiameterIndex.ItemIndex;
  CoilDiameter      := GeometryForm.CoilDiameter     .Value;
  CoilDiameterUnit  := GeometryForm.CoilDiameterUnit .ItemIndex;
  AnctiveCoil       := GeometryForm.ActiveCoil       .Value;
  InactiveCoil1     := GeometryForm.InactiveCoil1    .Value;
  InactiveCoil2     := GeometryForm.InactiveCoil2    .Value;
  LengthL0          := GeometryForm.LengthL0         .Value;
  LengthL0Unit      := GeometryForm.LengthL0Unit     .ItemIndex;
  LengthL1          := GeometryForm.LengthL1         .Value;
  LengthL1Unit      := GeometryForm.LengthL1Unit     .ItemIndex;
  LengthL2          := GeometryForm.LengthL2         .Value;
  LengthL2Unit      := GeometryForm.LengthL2Unit     .ItemIndex;
  EndCoilType       := GeometryForm.EndCoilType      .ItemIndex;
  if GeometryForm.ShowModal <> mrOk then
  begin
    GeometryForm.WireDiameter     .Value     := WireDiameter;
    GeometryForm.WireDiameterUnit .ItemIndex := WireDiameterUnit;
    GeometryForm.CoilDiameterIndex.ItemIndex := CoilDiameterIndex;
    GeometryForm.CoilDiameter     .Value     := CoilDiameter;
    GeometryForm.CoilDiameterUnit .ItemIndex := CoilDiameterUnit;
    GeometryForm.ActiveCoil       .Value     := AnctiveCoil;
    GeometryForm.InactiveCoil1    .Value     := InactiveCoil1;
    GeometryForm.InactiveCoil2    .Value     := InactiveCoil2;
    GeometryForm.LengthL0         .Value     := LengthL0;
    GeometryForm.LengthL0Unit     .ItemIndex := LengthL0Unit;
    GeometryForm.LengthL1         .Value     := LengthL1;
    GeometryForm.LengthL1Unit     .ItemIndex := LengthL1Unit;
    GeometryForm.LengthL2         .Value     := LengthL2;
    GeometryForm.LengthL2Unit     .ItemIndex := LengthL2Unit;
    GeometryForm.EndCoilType      .ItemIndex := EndCoilType;
  end;
  Solve();
end;

procedure TMainForm.MaterialMenuItemClick(Sender: TObject);
var
  Material: longint;
  YoungModulus: double;
  ShearModulus: double;
  TensileStrength: double;
  MaterialDensity: double;
  CoilingType: longint;
begin
  Material        := MaterialForm.Material       .ItemIndex;
  YoungModulus    := MaterialForm.YoungModulus   .Value;
  ShearModulus    := MaterialForm.ShearModulus   .Value;
  TensileStrength := MaterialForm.TensileStrength.Value;
  MaterialDensity := MaterialForm.MaterialDensity.Value;
  CoilingType     := MaterialForm.CoilingType    .ItemIndex;
  if MaterialForm.ShowModal <> mrOk then
  begin
    MaterialForm.Material       .ItemIndex := Material;
    MaterialForm.YoungModulus   .Value     := YoungModulus;
    MaterialForm.ShearModulus   .Value     := ShearModulus;
    MaterialForm.TensileStrength.Value     := TensileStrength;
    MaterialForm.MaterialDensity.Value     := MaterialDensity;
    MaterialForm.CoilingType    .ItemIndex := CoilingType;
  end;
  Solve();
end;

procedure TMainForm.QualityMenuItemClick(Sender: TObject);
var
  ToleranceWireDiameter: double;
  ToleranceWireDiameterUnit: longint;
  ToleranceCoilDiameter: longint;
  ToleranceLengthL0: longint;
  ToleranceLoadF1: longint;
  ToleranceLoadF2: longint;
  ToleranceEccentricitye1: longint;
  ToleranceEccentricitye2: longint;
begin
  ToleranceWireDiameter     := QualityForm.ToleranceWireDiameter    .Value;
  ToleranceWireDiameterUnit := QualityForm.ToleranceWireDiameterUnit.ItemIndex;
  ToleranceCoilDiameter     := QualityForm.ToleranceCoilDiameter    .ItemIndex;
  ToleranceLengthL0         := QualityForm.ToleranceLengthL0        .ItemIndex;
  ToleranceLoadF1           := QualityForm.ToleranceLoadF1          .ItemIndex;
  ToleranceLoadF2           := QualityForm.ToleranceLoadF2          .ItemIndex;
  ToleranceEccentricitye1   := QualityForm.ToleranceEccentricitye1  .ItemIndex;
  ToleranceEccentricitye2   := QualityForm.ToleranceEccentricitye2  .ItemIndex;
  if QualityForm.ShowModal <> mrOk then
  begin
    QualityForm.ToleranceWireDiameter    .Value     := ToleranceWireDiameter;
    QualityForm.ToleranceWireDiameterUnit.ItemIndex := ToleranceWireDiameterUnit;
    QualityForm.ToleranceCoilDiameter    .ItemIndex := ToleranceCoilDiameter;
    QualityForm.ToleranceLengthL0        .ItemIndex := ToleranceLengthL0;
    QualityForm.ToleranceLoadF1          .ItemIndex := ToleranceLoadF1;
    QualityForm.ToleranceLoadF2          .ItemIndex := ToleranceLoadF2;
    QualityForm.ToleranceEccentricitye1  .ItemIndex := ToleranceEccentricitye1;
    QualityForm.ToleranceEccentricitye2  .ItemIndex := ToleranceEccentricitye2;
  end;
  Solve();
end;

procedure TMainForm.ProductionMenuItemClick(Sender: TObject);
var
  DirectionCoilsIndex: longint;
  BurringEndsIndex: longint;
  WireSurfaceIndex: longint;
  LengthLs: double;
  L0, nAndd, nAndDeDi, L0nAndd, L0nAndDeDi: boolean;
begin
  DirectionCoilsIndex := ProductionForm.DirectionCoils.ItemIndex;
  BurringEndsIndex    := ProductionForm.BurringEnds.ItemIndex;
  WireSurfaceIndex    := ProductionForm.WireSurface.ItemIndex;
  LengthLs            := ProductionForm.LengthLs.Value;
  L0                  := ProductionForm.L0.Checked;
  nAndd               := ProductionForm.nAndd.Checked;
  nAndDeDi            := ProductionForm.nAndDeDi.Checked;
  L0nAndd             := ProductionForm.L0nAndd.Checked;
  L0nAndDeDi          := ProductionForm.L0nAndDeDi.Checked;

  if ProductionForm.ShowModal <> mrOk then
  begin
    ProductionForm.DirectionCoils.ItemIndex := DirectionCoilsIndex;
    ProductionForm.BurringEnds   .ItemIndex := BurringEndsIndex;
    ProductionForm.WireSurface   .ItemIndex := WireSurfaceIndex;
    ProductionForm.LengthLs      .Value     := LengthLs;
    ProductionForm.L0            .Checked   := L0;
    ProductionForm.nAndd         .Checked   := nAndd;
    ProductionForm.nAndDeDi      .Checked   := nAndDeDi;
    ProductionForm.L0nAndd       .Checked   := L0nAndd;
    ProductionForm.L0nAndDeDi    .Checked   := L0nAndDeDi;
  end;
  Solve();
end;

procedure TMainForm.ApplicationMenuItemClick(Sender: TObject);
var
  LoadType: longint;
  Temperature: double;
  TemperatureUnit: longint;
  SeatingCoefficent: longint;
begin
  LoadType          := ApplicationForm.LoadType         .ItemIndex;
  Temperature       := ApplicationForm.Temperature      .Value;
  TemperatureUnit   := ApplicationForm.TemperatureUnit  .ItemIndex;
  SeatingCoefficent := ApplicationForm.SeatingCoefficent.ItemIndex;
  if ApplicationForm.ShowModal <> mrOk then
  begin
    ApplicationForm.LoadType         .ItemIndex := LoadType;
    ApplicationForm.Temperature      .Value     := Temperature;
    ApplicationForm.TemperatureUnit  .ItemIndex := TemperatureUnit;
    ApplicationForm.SeatingCoefficent.ItemIndex := SeatingCoefficent;
  end;
  Solve();
end;

// Memu View //

procedure TMainForm.ViewMenuItemClick(Sender: TObject);
var
  i: longint;
begin
  if (Sender <> TempMenuItem) and
     (Sender <> DrawMenuItem) then
  begin
    for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
    for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
    for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;
    for i := 0 to DocsMenuItem.Count -1 do DocsMenuItem.Items[i].Checked := False;

    if Assigned(Sender) then
      TMenuItem(Sender).Checked := True;
    FormPaint(Sender);
  end;
end;

procedure TMainForm.CustomSectionMenuItemClick(Sender: TObject);
var
  i: longint;
  SpringLength: double;
begin
  if (Sender <> TempMenuItem) and
     (Sender <> DrawMenuItem) then
  begin
    DrawingForm.SpringLength.MinValue := SpringSolver.LengthLc;
    DrawingForm.SpringLength.MaxValue := SpringSolver.LengthL0;

    SpringLength := DrawingForm.SpringLength.Value;
    if DrawingForm.ShowModal = mrOk then
    begin
      for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
      for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
      for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;

      if Assigned(Sender) then
        TMenuItem(Sender).Checked := True;
    end else
      DrawingForm.SpringLength.Value := SpringLength;

    FormPaint(Sender);
  end;
end;

// Menu Zoom //

procedure TMainForm.ZoomMenuItemClick(Sender: TObject);
var
  i: longint;
begin
  for i := 0 to ZoomMenuItem.Count -1 do
    ZoomMenuItem.Items[i].Checked := False;

  if Assigned(Sender) then
  begin
    ScreenScale := 1.00;
    if Sender = Zoom100MenuItem then ScreenScale := 1.00;
    if Sender = Zoom125MenuItem then ScreenScale := 1.25;
    if Sender = Zoom150MenuItem then ScreenScale := 1.50;
    if Sender = Zoom175MenuItem then ScreenScale := 1.75;
    if Sender = Zoom200MenuItem then ScreenScale := 2.00;
    if Sender = Zoom225MenuItem then ScreenScale := 2.25;
    if Sender = Zoom250MenuItem then ScreenScale := 2.50;
    TMenuItem(Sender).Checked := True;
  end;

  MoveX := 0;
  MoveY := 0;
  FormPaint(Sender);
end;

procedure TMainForm.ModifyFontMenuItemClick(Sender: TObject);
var
  FontHeightInc: longint;
begin
  FontHeightInc := ClientFile.ReadInteger('Custom', 'FontHeightInc', 0);

  if Sender = IncreaseFontMenuItem then
    Inc(FontHeightInc);

  if Sender = DecreaseFontMenuItem then
    Dec(FontHeightInc);

  if Sender = ResetFontMenuItem then
    FontHeightInc := 0;

  ClientFile.WriteInteger('Custom', 'FontHeightInc', FontHeightInc);
  try
    FormPaint(nil);
  except
    if Sender = DecreaseFontMenuItem then
      ModifyFontMenuItemClick(IncreaseFontMenuItem);

    if Sender = IncreaseFontMenuItem then
      ModifyFontMenuItemClick(DecreaseFontMenuItem);
  end;
end;

// Menu Documentation

procedure TMainForm.ReportMenuItemClick(Sender: TObject);
begin
  Solve();
  ReportForm.LoadReport;
  ReportForm.ShowModal;
end;

function TMainForm.ProductionDrawing(const Tx: string; aSetting: TIniFile): string;
begin
  Result := Tx;
  Result := StringReplace(Result, '@0.00', Format('e1=%s mm', [TryFloatToText(SpringSolver.EccentricityE1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.01', Format('e2=%s mm', [TryFloatToText(SpringSolver.EccentricityE2)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.02', Format('d=%s mm',  [TryFloatToText(SpringSolver.WireDiameter  )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.03', Format('Di=%s mm', [TryFloatToText(SpringSolver.Di            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.04', Format('Dm=%s mm', [TryFloatToText(SpringSolver.Dm            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.05', Format('De=%s mm', [TryFloatToText(SpringSolver.De            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.06', Format('L0=%s mm', [TryFloatToText(SpringSolver.LengthL0      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.07', Format('L1=%s mm', [TryFloatToText(SpringSolver.LengthL1      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.08', Format('L2=%s mm', [TryFloatToText(SpringSolver.LengthL2      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.09', Format('Ln=%s mm', [TryFloatToText(SpringSolver.LengthLn      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.10', Format('Lc=%s mm', [TryFloatToText(SpringSolver.LengthLc      )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.11', Format('F1=%s',    [TryFloatToText(SpringSolver.LoadF1        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.13', Format('F2=%s',    [TryFloatToText(SpringSolver.LoadF2        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.15', Format('Fn=%s',    [TryFloatToText(SpringSolver.LoadFn        )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.17', Format('Fc=%s',    [TryFloatToText(SpringSolver.LoadFc        )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.12', Format('Tauk1=%s', [TryFloatToText(SpringSolver.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.14', Format('Tauk2=%s', [TryFloatToText(SpringSolver.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.16', Format('Taukn=%s', [TryFloatToText(SpringSolver.TorsionalStressTaukn)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.18', Format('Tauc=%s',  [TryFloatToText(SpringSolver.TorsionalStressTauc )]), [rfReplaceAll, rfIgnoreCase]);

  if SpringSolver.ClosedEnds and (SpringSolver.GroundEnds = True) then
  begin
    Result := StringReplace(Result, '@0.20', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.21', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.22', ' ', [rfReplaceAll, rfIgnoreCase]);
  end;

  if SpringSolver.ClosedEnds and (SpringSolver.GroundEnds = False) then
  begin
    Result := StringReplace(Result, '@0.20', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.21', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@0.22', ' ', [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := StringReplace(Result, '@1.0', Format('n=%s',      [TryFloatToText(SpringSolver.ActiveColis)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@1.1', Format('nt=%s',     [TryFloatToText(SpringSolver.TotalCoils )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@2.0', Format('R=%s N/mm', [TryFloatToText(SpringSolver.SpringRateR)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.DirectionCoils.ItemIndex of
    0: ;
    1: Result := StringReplace(Result, '@3.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@3.1', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@3.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@3.1', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@4.0', Format('Dd=%s mm', [TryFloatToText(SpringSolver.Di_Min)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@4.1', Format('Dh=%s mm', [TryFloatToText(SpringSolver.De_Max)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.BurringEnds.ItemIndex of
    0: Result := StringReplace(Result, '@5.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    1: Result := StringReplace(Result, '@5.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@5.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@5.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.2', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@6.0', Format('fe=%s Hz', [TryFloatToText(SpringSolver.De_Max)]), [rfReplaceAll, rfIgnoreCase]);
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
  Result := StringReplace(Result, '@10.1', Format('tauz=%s Mpa', [TryFloatToText(SpringSolver.AdmStaticTorsionalStressTauz)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@10.2', Format('G=%s Mpa', [TryFloatToText(MAT.ShearModulusG20)]), [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradeDm of
    1: Result := StringReplace(Result, '@11.00', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.01', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.02', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.00', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.01', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.02', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradeL0 of
    1: Result := StringReplace(Result, '@11.10', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.11', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.12', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.10', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.11', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.12', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradeF1 of
    1: Result := StringReplace(Result, '@11.20', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.21', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.22', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.20', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.21', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.22', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradeF2 of
    1: Result := StringReplace(Result, '@11.30', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.31', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.32', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.30', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.31', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.32', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradee1 of
    1: Result := StringReplace(Result, '@11.40', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@11.41', 'X', [rfReplaceAll, rfIgnoreCase]);
    3: Result := StringReplace(Result, '@11.42', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.40', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.41', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.42', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringSolver.QualityGradee2 of
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

  Result := StringReplace(Result, '@14.0', DrawingTextForm.Note1        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@14.1', DrawingTextForm.Note2        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.0', DrawingTextForm.DrawingName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.1', DrawingTextForm.DrawingNumber.Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.2', DrawingTextForm.CompanyName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.3', ApplicationVer,                     [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '#ff0',    aSetting.ReadString('Printer', 'Page.Color4', '#ff0'),     [rfReplaceAll, rfIgnoreCase]); // Yellow line
  Result := StringReplace(Result, '#f00',    aSetting.ReadString('Printer', 'Page.Color5', '#f00'),     [rfReplaceAll, rfIgnoreCase]); // Red    line
  Result := StringReplace(Result, '#0f0',    aSetting.ReadString('Printer', 'Page.Color6', '#0f0'),     [rfReplaceAll, rfIgnoreCase]); // Green  line

  Result := StringReplace(Result, '#ffff00', aSetting.ReadString('Printer', 'Page.Color1', '#ffff00'),  [rfReplaceAll, rfIgnoreCase]); // Yellow
  Result := StringReplace(Result, '#ff0000', aSetting.ReadString('Printer', 'Page.Color2', '#ff0000'),  [rfReplaceAll, rfIgnoreCase]); // Red
  Result := StringReplace(Result, '#00ff00', aSetting.ReadString('Printer', 'Page.Color3', '#00ff00'),  [rfReplaceAll, rfIgnoreCase]); // Green
end;

procedure TMainForm.ExportReportMenuItemClick(Sender: TObject);
begin
  Solve();
  SaveDialog.Filter := 'Text file (*.txt)|*.txt|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    ReportForm.LoadReport;
    ReportForm.Memo.Lines.SaveToFile(SaveDialog.FileName);
  end;
  SaveDialog.FileName := FileName;
end;

procedure TMainForm.ExportProductionMenuItemClick(Sender: TObject);
var
  SVG: TBGRASVG;
begin
  Solve();
  SaveDialog.Filter := 'Svg file (*.svg)|*.svg|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    SVG := TBGRASVG.Create;
    SVG.LoadFromResource('TEMPLATE');
    SVG.AsUTF8String := ProductionDrawing(SVG.AsUTF8String, PrinterFile);
    SVG.SaveToFile(SaveDialog.FileName);
    SVG.Destroy;
  end;
end;

// Menu Help //

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.AboutNameLb.Caption := ApplicationVer;
  AboutForm.ShowModal;
end;

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

  SpringSolver.Solve;
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
    SpringDrawing.Lx   := SpringSolver.LengthL0;
    SpringDrawing.Text := TryFormatFloat('L0 = %s', 'L0 = ---',SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    Bit[1].SetSize(Bit[0].Width, Bit[0].Height);
    SpringDrawing.Fit  := False;
    SpringDrawing.Lx   := SpringSolver.LengthL1;
    SpringDrawing.Text := TryFormatFloat('L1 = %s', 'L1 = ---', SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    Bit[2].SetSize(Bit[1].Width, Bit[1].Height);
    SpringDrawing.Fit  := False;
    SpringDrawing.Lx   := SpringSolver.LengthL2;
    SpringDrawing.Text := TryFormatFloat('L2 = %s', 'L2 = ---', SpringDrawing.Lx);
    SpringDrawing.Draw(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);

    Bit[0].Draw(aScreen.Canvas, Bit[0].Width * 0, 0, False);
    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 1, 0, False);
    Bit[2].Draw(aScreen.Canvas, Bit[2].Width * 2, 0, False);

    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil
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
    ReportList1.RowCount     := 16;
    ReportList1.Items[ 0, 0] := TryFormatFloat   ('d     = %s mm',     'd     = ---', SpringSolver.WireDiameter);
    ReportList1.Items[ 1, 0] := TryFormatFloat   ('tauk1 = %s MPa',    'tauk1 = ---', SpringSolver.TorsionalStressTauk1);
    ReportList1.Items[ 2, 0] := TryFormatFloat   ('tauk2 = %s MPa',    'tauk2 = ---', SpringSolver.TorsionalStressTauk2);
    ReportList1.Items[ 3, 0] := TryFormatFloat   ('taukh = %s MPa',    'taukh = ---', SpringSolver.TorsionalStressTaukh);
    ReportList1.Items[ 4, 0] := ' ';
    ReportList1.Items[ 5, 0] := TryFormatFloat   ('E     = %s MPa',    'E     = ---', SpringSolver.YoungModulus);
    ReportList1.Items[ 6, 0] := TryFormatFloat   ('G     = %s MPa',    'G     = ---', SpringSolver.ShearModulus);
    ReportList1.Items[ 7, 0] := TryFormatFloat   ('rho   = %s kg/dm3', 'rho   = ---', SpringSolver.DensityRho);
    ReportList1.Items[ 8, 0] := TryFormatFloat   ('Rm    = %s MPa',    'Rm    = ---', SpringSolver.TensileStrengthRm);
    ReportList1.Items[ 9, 0] := TryFormatFloat   ('tauoz = %s MPa',    'tauoz = ---', SpringSolver.AdmDynamicTorsionalStressTauoz);
    ReportList1.Items[10, 0] := TryFormatFloat   ('tauhz = %s MPa',    'tauhz = ---', SpringSolver.AdmDynamicTorsionalStressRangeTauhz);
    ReportList1.Items[11, 0] := ' ';
    ReportList1.Items[12, 0] := TryFormatFloat   ('ns    = %s',        'ns    = ---', SpringSolver.StaticSafetyFactor);
    if ApplicationForm.LoadType.ItemIndex = 0 then
    begin
      ReportList1.Items[13, 0] := TryFormatFloat   ('nf    = %s',        'nf    = ---', SpringSolver.DynamicSafetyFactor);
      ReportList1.Items[14, 0] := TryFormatText    ('N     = %s cycles', 'N     = ---', TryFloatToText(SpringSolver.NumOfCycles, 2, 0));
      ReportList1.Items[15, 0] := TryFormatFloatDiv('Nh    = %s hours',  'Nh    = ---', SpringSolver.NumOfCycles, 3600*ApplicationForm.CycleFrequency.Value);
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
    SpringDrawing.Text := Format('L0 = %s', [TryFloatToText(SpringSolver.LengthL0)]);
    SpringDrawing.Lx   := SpringSolver.LengthL0;
    SpringDrawing.Fit  := True;
    SpringDrawing.Draw(Bit[5].Canvas, Bit[5].Width, Bit[5].Height);

    if (3*Bit[5].Width) <= (aScreen.Width - Bit[0].Width - Bit[4].Width) then
    begin
      Bit[6].SetSize(Bit[5].Width, Bit[5].Height);
      SpringDrawing.Text := Format('L1 = %s', [TryFloatToText(SpringSolver.LengthL1)]);
      SpringDrawing.Lx   := SpringSolver.LengthL1;
      SpringDrawing.Fit  := False;
      SpringDrawing.Draw(Bit[6].Canvas, Bit[6].Width, Bit[6].Height);

      Bit[7].SetSize(Bit[5].Width, Bit[5].Height);
      SpringDrawing.Text := Format('L2 = %s', [TryFloatToText(SpringSolver.LengthL2)]);
      SpringDrawing.Lx   := SpringSolver.LengthL2;
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
    ReportList1.Items[ 0, 0] := TryFormatFloat   ('d        = %s mm',     'd     = ---', SpringSolver.WireDiameter) + TryFormatFloat(' ± %s mm', '', SpringSolver.ToleranceWireDiameter);
    ReportList1.Items[ 1, 0] := TryFormatFloat   ('Di       = %s mm',     'Di    = ---', SpringSolver.Di);
    ReportList1.Items[ 2, 0] := TryFormatFloat   ('Dm       = %s mm',     'Dm    = ---', SpringSolver.Dm) + TryFormatFloat(' ± %s mm', '', SpringSolver.ToleranceDm);
    ReportList1.Items[ 3, 0] := TryFormatFloat   ('De       = %s mm',     'De    = ---', SpringSolver.De);
    ReportList1.Items[ 4, 0] := TryFormatFloat   ('n        = %s coils',  'n     = ---', SpringSolver.ActiveColis);
    ReportList1.Items[ 5, 0] := TryFormatFloat   ('nt       = %s colis',  'nt    = ---', SpringSolver.TotalCoils);
    ReportList1.Items[ 6, 0] := TryFormatFloatDiv('Dm/d     = %s',        'Dm/d  = ---', SpringSolver.Dm, SpringSolver.WireDiameter);
    ReportList1.Items[ 7, 0] := TryFormatFloat   ('nu       = %s',        'nu    = ---', SpringSolver.SeatingCoefficent);
    ReportList1.Items[ 8, 0] := TryFormatFloat   ('k        = %s',        'k     = ---', SpringSolver.CorrectionFactorK);
    ReportList1.Items[ 9, 0] := '';
    ReportList1.Items[10, 0] := TryFormatFloat   ('L        = %s mm',     'L     = ---', SpringSolver.WireLength);
    ReportList1.Items[11, 0] := TryFormatFloat   ('rho      = %s kg/dm3', 'rho   = ---', SpringSolver.DensityRho);
    ReportList1.Items[12, 0] := TryFormatFloat   ('mass     = %s g',      'mass  = ---', SpringSolver.Mass);
    ReportList1.Items[13, 0] := TryFormatFloat   ('fe       = %s Hz',     'fe    = ---', SpringSolver.NaturalFrequency);
    ReportList1.Items[14, 0] := '';
    ReportList1.Items[15, 0] := TryFormatText    ('Material = %s',        'Material = ---', MAT.Items[MAT.ItemIndex]);
    ReportList1.Items[16, 0] := TryFormatFloat   ('G        = %s MPa',    'G        = ---', SpringSolver.ShearModulus);
    ReportList1.Items[17, 0] := TryFormatFloat   ('Rm       = %s MPa',    'Rm       = ---', SpringSolver.TensileStrengthRm);
    ReportList1.Items[18, 0] := TryFormatFloat   ('tauz     = %s MPa',    'tauz     = ---', SpringSolver.AdmStaticTorsionalStressTauz);
    ReportList1.Items[19, 0] := TryFormatFloat   ('T        = %s C°',     'T        = ---', MAT.Tempetature);
    ReportList1.Items[20, 0] := TryFormatFloat   ('G(T)     = %s MPa',    'G(T)     = ---', MAT.GetG(MAT.Tempetature));
    ReportList1.Items[21, 0] := '';
    ReportList1.Items[22, 0] := TryFormatBool    ('Closed ends    = True', 'Closed ends    = False', SpringSolver.ClosedEnds);
    ReportList1.Items[23, 0] := TryFormatBool    ('Ground ends    = True', 'Ground ends    = False', SpringSolver.GroundEnds);
    ReportList1.Items[24, 0] := TryFormatBool    ('Cold coiled    = True', 'Cold coiled    = False', SpringSolver.ColdCoiled);
    ReportList1.Items[25, 0] := TryFormatBool    ('Dynamic strain = True', 'Dynamic strain = False', SpringSolver.DynamicLoad);
    ReportList1.Items[26, 0] := '';
    ReportList1.Items[27, 0] := TryFormatInt     ('EN15800 Quality Grade Dm  = %s', 'EN15800 Quality Grade Dm  = ---', SpringSolver.QualityGradeDm);
    ReportList1.Items[28, 0] := TryFormatInt     ('EN15800 Quality Grade L0  = %s', 'EN15800 Quality Grade L0  = ---', SpringSolver.QualityGradeL0);
    ReportList1.Items[29, 0] := TryFormatInt     ('EN15800 Quality Grade F1  = %s', 'EN15800 Quality Grade F1  = ---', SpringSolver.QualityGradeF1);
    ReportList1.Items[30, 0] := TryFormatInt     ('EN15800 Quality Grade F2  = %s', 'EN15800 Quality Grade F2  = ---', SpringSolver.QualityGradeF2);
    ReportList1.Items[31, 0] := TryFormatInt     ('EN15800 Quality Grade e1  = %s', 'EN15800 Quality Grade e1  = ---', SpringSolver.QualityGradee1);
    ReportList1.Items[32, 0] := TryFormatInt     ('EN15800 Quality Grade e2  = %s', 'EN15800 Quality Grade e2  = ---', SpringSolver.QualityGradee2);

    ReportList1.Items[ 0, 1] := '   ';

    ReportList1.Items[ 0, 2] := TryFormatFloat('L0    = %s mm',  'L0    = ---', SpringSolver.LengthL0) + TryFormatFloat(' ± %s mm', '', SpringSolver.ToleranceL0);
    ReportList1.Items[ 1, 2] := TryFormatFloat('L1    = %s mm',  'L1    = ---', SpringSolver.LengthL1);
    ReportList1.Items[ 2, 2] := TryFormatFloat('L2    = %s mm',  'L2    = ---', SpringSolver.LengthL2);
    ReportList1.Items[ 3, 2] := TryFormatFloat('Ln    = %s mm',  'Ln    = ---', SpringSolver.LengthLn);
    ReportList1.Items[ 4, 2] := TryFormatFloat('Lc    = %s mm',  'Lc    = ---', SpringSolver.LengthLc);
    ReportList1.Items[ 5, 2] := '';
    ReportList1.Items[ 6, 2] := TryFormatFloat('s1    = %s mm',  's1    = ---', SpringSolver.DeflectionS1);
    ReportList1.Items[ 7, 2] := TryFormatFloat('s2    = %s mm',  's2    = ---', SpringSolver.DeflectionS2);
    ReportList1.Items[ 8, 2] := TryFormatFloat('sh    = %s mm',  'sh    = ---', SpringSolver.DeflectionSh);
    ReportList1.Items[ 9, 2] := TryFormatFloat('sn    = %s mm',  'sn    = ---', SpringSolver.DeflectionSn);
    ReportList1.Items[10, 2] := TryFormatFloat('sc    = %s mm',  'sc    = ---', SpringSolver.DeflectionSc);
    ReportList1.Items[11, 2] := '';
    ReportList1.Items[12, 2] := TryFormatFloat('F1    = %s N',   'F1    = ---', SpringSolver.LoadF1) + TryFormatFloat(' ± %s N', '', SpringSolver.ToleranceLoadF1);
    ReportList1.Items[13, 2] := TryFormatFloat('F2    = %s N',   'F2    = ---', SpringSolver.LoadF2) + TryFormatFloat(' ± %s N', '', SpringSolver.ToleranceLoadF2);
    ReportList1.Items[14, 2] := TryFormatFloat('Fn    = %s N',   'Fn    = ---', SpringSolver.LoadFn);
    ReportList1.Items[15, 2] := TryFormatFloat('Fc    = %s N',   'Fc    = ---', SpringSolver.LoadFc);
    ReportList1.Items[16, 2] := '';
    ReportList1.Items[17, 2] := TryFormatFloat('tauk1 = %s MPa', 'tauk1 = ---', SpringSolver.TorsionalStressTauk1);
    ReportList1.Items[18, 2] := TryFormatFloat('tauk2 = %s MPa', 'tauk2 = ---', SpringSolver.TorsionalStressTauk2);
    ReportList1.Items[19, 2] := TryFormatFloat('taukh = %s MPa', 'taukh = ---', SpringSolver.TorsionalStressTaukh);
    ReportList1.Items[20, 2] := '';
    ReportList1.Items[21, 2] := TryFormatFloat('tauoz = %s MPa', 'tauoz = ---', SpringSolver.AdmDynamicTorsionalStressTauoz);
    ReportList1.Items[22, 2] := TryFormatFloat('tauhz = %s MPa', 'tauhz = ---', SpringSolver.AdmDynamicTorsionalStressRangeTauhz);
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
      SVG.AsUTF8String := ProductionDrawing(SVG.AsUTF8String, aSetting);
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
  VirtualScreen.RedrawBitmap;
end;

function TMainForm.CreateForceDisplacementDiagram(const aScreenScale: double; aSetting: TIniFile): TForceDisplacementDiagram;
begin
  Result             := TForceDisplacementDiagram.Create('ForceDisplacementDiagram', aSetting);
  Result.F1          := SpringSolver.LoadF1;
  Result.F2          := SpringSolver.LoadF2;
  Result.Fn          := SpringSolver.LoadFn;
  Result.Fc          := SpringSolver.LoadFc;
  Result.s1          := SpringSolver.DeflectionS1;
  Result.s2          := SpringSolver.DeflectionS2;
  Result.sn          := SpringSolver.DeflectionSn;
  Result.sc          := SpringSolver.DeflectionSc;
  Result.ToleranceF1 := SpringSolver.ToleranceLoadF1;
  Result.ToleranceF1 := SpringSolver.ToleranceLoadF2;
  Result.Spacer      := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom        := aScreenScale;
end;

function TMainForm.CreateGoodmanDiagram(const aScreenScale: double; aSetting: TIniFile): TGoodmanDiagram;
begin
  Result                         := TGoodmanDiagram.Create('GoodmanDiagram', aSetting);
  Result.Caption                 := Format('Goodman Diagram: %s', [MAT.Items[MAT.ItemIndex]]);
  Result.Tauz                    := SpringSolver.AdmStaticTorsionalStressTauz;
  Result.Tauk1                   := SpringSolver.TorsionalStressTauk1;
  Result.Tauk2                   := SpringSolver.TorsionalStressTauk2;
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
  Result.Tauk1Tolerance          := SpringSolver.Tolerancetauk1;
  Result.Tauk2Tolerance          := SpringSolver.Tolerancetauk2;
  Result.Spacer                  := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom                    := aScreenScale;
end;

function TMainForm.CreateBucklingDiagram(const aScreenScale: double; aSetting: TIniFile): TBucklingDiagram;
begin
  Result        := TBucklingDiagram.Create('BucklingDiagram', aSetting);
  Result.Dm     := SpringSolver.Dm;
  Result.E      := SpringSolver.YoungModulus;
  Result.G      := SpringSolver.ShearModulus;
  Result.L0     := SpringSolver.LengthL0;
  Result.nu     := SpringSolver.SeatingCoefficent;
  Result.s1     := SpringSolver.DeflectionS1;
  Result.s2     := SpringSolver.DeflectionS2;
  Result.sc     := SpringSolver.DeflectionSc;
  Result.sk     := SpringSolver.DeflectionSk;
  Result.Spacer := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom   := aScreenScale;
end;

function TMainForm.CreateShearModulusDiagram(const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
begin
  Result                 := TLinearTemperatureDiagram.Create('LinearTemperatureDiagram', aSetting);
  Result.Caption         := 'G-T Diagram';
  Result.HorizontalLabel := 'T [C°]';
  Result.VerticalLabel   := 'G [MPa]';
  Result.Temperature     := MAT.Tempetature;
  Result.Temperature0    := 0;
  Result.Temperature1    := 500;
  Result.Value           := MAT.ShearModulusG;
  Result.Value0          := MAT.GetG(Result.Temperature0);
  Result.Value1          := MAT.GetG(Result.Temperature1);
  Result.Spacer          := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom            := aScreenScale;
end;

function TMainForm.CreateYoungModulusDiagram(const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
begin
  Result                 := TLinearTemperatureDiagram.Create('LinearTemperatureDiagram', aSetting);
  Result.Caption         := 'E-T Diagram';
  Result.HorizontalLabel := 'T [C°]';
  Result.VerticalLabel   := 'E [MPa]';
  Result.Temperature     := MAT.Tempetature;
  Result.Temperature0    := 0;
  Result.Temperature1    := 500;
  Result.Value           := MAT.YoungModulusE;
  Result.Value0          := MAT.GetE(Result.Temperature0);
  Result.Value1          := MAT.GetE(Result.Temperature1);
  Result.Spacer          := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom            := aScreenScale;
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
  Result.Value           := SpringSolver.LoadF1;
  Result.Value0          := SpringSolver.GetF1(Result.Temperature0);
  Result.Value1          := SpringSolver.GetF1(Result.Temperature1);
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
  Result.Value           := SpringSolver.LoadF2;
  Result.Value0          := SpringSolver.GetF2(Result.Temperature0);
  Result.Value1          := SpringSolver.GetF2(Result.Temperature1);
  Result.Spacer          := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom            := aScreenScale;
end;

function TMainForm.CreateMessageList(const aScreenScale: double; aSetting: TIniFile): TReportTable;
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

function TMainForm.CreateReportTable(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result             := TReportTable.Create('ReportTable', aSetting);
  Result.Spacer      := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom        := aScreenScale;
  Result.RowCount    := 6;
  Result.ColumnCount := 7;
  Result[0, 0]       := 'L [mm]';
  Result[1, 0]       := TryFormatFloat('L0: %s', 'L0: ---', SpringSolver.LengthL0);
  Result[2, 0]       := TryFormatFloat('L1: %s', 'L1: ---', SpringSolver.LengthL1);
  Result[3, 0]       := TryFormatFloat('L2: %s', 'L2: ---', SpringSolver.LengthL2);
  Result[4, 0]       := TryFormatFloat('Ln: %s', 'Ln: ---', SpringSolver.LengthLn);
  Result[5, 0]       := TryFormatFloat('Lc: %s', 'Lc: ---', SpringSolver.LengthLc);

  Result[0, 1]       := 'F [N]';
  Result[1, 1]       := '';
  Result[2, 1]       := TryFormatFloat('F1: %s', 'F1: ---', SpringSolver.LoadF1);
  Result[3, 1]       := TryFormatFloat('F2: %s', 'F2: ---', SpringSolver.LoadF2);
  Result[4, 1]       := TryFormatFloat('Fn: %s', 'Fn: ---', SpringSolver.LoadFn);
  Result[5, 1]       := TryFormatFloat('Fc: %s', 'Fc: ---', SpringSolver.LoadFc);

  Result[0, 2]       := 'tau [MPa]';
  Result[1, 2]       := '';
  Result[2, 2]       := TryFormatFloat('tauk1: %s', 'tauk1: ---', SpringSolver.TorsionalStressTauk1);
  Result[3, 2]       := TryFormatFloat('tauk2: %s', 'tauk2: ---', SpringSolver.TorsionalStressTauk2);
  Result[4, 2]       := TryFormatFloat('tau n: %s', 'tau n: ---', SpringSolver.TorsionalStressTaun);
  Result[5, 2]       := TryFormatFloat('tau c: %s', 'tau c: ---', SpringSolver.TorsionalStressTauc);

  Result[0, 3]       := 's [mm]';
  Result[1, 3]       := '';
  Result[2, 3]       := TryFormatFloat('s1: %s', 's1: ---', SpringSolver.DeflectionS1);
  Result[3, 3]       := TryFormatFloat('s2: %s', 's2: ---', SpringSolver.DeflectionS2);
  Result[4, 3]       := TryFormatFloat('sn: %s', 'sn: ---', SpringSolver.DeflectionSn);
  Result[5, 3]       := TryFormatFloat('sc: %s', 'sc: ---', SpringSolver.DeflectionSc);

  Result[0, 4]       := 'tau/tauz';
  Result[1, 4]       := '';
  Result[2, 4]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauk1, SpringSolver.AdmStaticTorsionalStressTauz);
  Result[3, 4]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauk2, SpringSolver.AdmStaticTorsionalStressTauz);
  Result[4, 4]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTaun , SpringSolver.AdmStaticTorsionalStressTauz);
  Result[5, 4]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauc , SpringSolver.AdmStaticTorsionalStressTauz);

  Result[0, 5]       := 'tau/Rm';
  Result[1, 5]       := '';
  Result[2, 5]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauk1, SpringSolver.TensileStrengthRm);
  Result[3, 5]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauk2, SpringSolver.TensileStrengthRm);
  Result[4, 5]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTaun , SpringSolver.TensileStrengthRm);
  Result[5, 5]       := TryFormatFloatDiv('%s', '---', SpringSolver.TorsionalStressTauc , SpringSolver.TensileStrengthRm);

  Result[0, 6]       := 'De';
  Result[1, 6]       := TryFormatFloat      ('%s', '---', SpringSolver.De);
  Result[2, 6]       := TryFormatFloatSumDiv('%s', '---', SpringSolver.De, SpringSolver.DeltaDe*SpringSolver.DeflectionS1, SpringSolver.DeflectionSc);
  Result[3, 6]       := TryFormatFloatSumDiv('%s', '---', SpringSolver.De, SpringSolver.DeltaDe*SpringSolver.DeflectionS2, SpringSolver.DeflectionSc);
  Result[4, 6]       := TryFormatFloatSumDiv('%s', '---', SpringSolver.De, SpringSolver.DeltaDe*SpringSolver.DeflectionSn, SpringSolver.DeflectionSc);
  Result[5, 6]       := TryFormatFloatSumDiv('%s', '---', SpringSolver.De, SpringSolver.DeltaDe*SpringSolver.DeflectionSc, SpringSolver.DeflectionSc);
end;

function TMainForm.CreateQualityTable(const aScreenScale: double; aSetting: TIniFile): TReportTable;
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
  Result[1, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeDm = 1);
  Result[2, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeL0 = 1);
  Result[3, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF1 = 1);
  Result[4, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF2 = 1);
  Result[5, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee1 = 1);
  Result[6, 1]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee2 = 1);

  Result[0, 2]             := '2';
  Result[1, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeDm = 2);
  Result[2, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeL0 = 2);
  Result[3, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF1 = 2);
  Result[4, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF2 = 2);
  Result[5, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee1 = 2);
  Result[6, 2]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee2 = 2);

  Result[0, 3]             := '3';
  Result[1, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeDm = 3);
  Result[2, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeL0 = 3);
  Result[3, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF1 = 3);
  Result[4, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradeF2 = 3);
  Result[5, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee1 = 3);
  Result[6, 3]             := TryFormatBool('x', ' ', SpringSolver.QualityGradee2 = 3);

  Result[0, 4]             := ' Tol.';
  Result[1, 4]             := TryFormatFloat('%s mm', ' --- ', SpringTolerance.CoilDiameterTolerance);
  Result[2, 4]             := TryFormatFloat('%s mm', ' --- ', SpringTolerance.LengthL0Tolerance);
  Result[3, 4]             := TryFormatFloat('%s N',  ' --- ', SpringTolerance.LoadF1Tolerance);
  Result[4, 4]             := TryFormatFloat('%s N',  ' --- ', SpringTolerance.LoadF1Tolerance);
  Result[5, 4]             := TryFormatFloat('%s mm', ' --- ', SpringTolerance.EccentricityE1);
  Result[6, 4]             := TryFormatFloat('%s mm', ' --- ', SpringTolerance.EccentricityE2);
end;

function TMainForm.CreateReportList0(const aScreenScale: double; aSetting: TIniFile): TReportTable;
begin
  Result                   := TReportTable.Create('ReportList', aSetting);
  Result.Spacer            := Trunc(DefaultSpacer*aScreenScale);
  Result.VerticalAlignment := 1;
  Result.Zoom              := aScreenScale;

  Result.ColumnCount       := 1;
  Result.RowCount          := 20;
  Result.Items[ 0, 0]      := TryFormatFloat('d      = %s',       'd      = ---', SpringSolver.WireDiameter) + TryFormatFloat(' ± %s mm', '', SpringSolver.ToleranceWireDiameter);
  Result.Items[ 1, 0]      := TryFormatFloat('Di     = %s mm',    'Di     = ---', SpringSolver.Di);
  Result.Items[ 2, 0]      := TryFormatFloat('Dm     = %s mm',    'Dm     = ---', SpringSolver.Dm);
  Result.Items[ 3, 0]      := TryFormatFloat('De     = %s',       'De     = ---', SpringSolver.De) + TryFormatFloat(' ± %s mm', '', SpringSolver.ToleranceDm);
  Result.Items[ 4, 0]      := TryFormatFloat('n      = %s coils', 'n      = ---', SpringSolver.ActiveColis);
  Result.Items[ 5, 0]      := TryFormatFloat('nt     = %s coils', 'nt     = ---', SpringSolver.TotalCoils);
  Result.Items[ 6, 0]      := TryFormatFloat('R      = %s N/mm',  'R      = ---', SpringSolver.SpringRateR);
  Result.Items[ 7, 0]      := TryFormatFloat('Dec    = %s mm',    'Dec    = ---', SpringSolver.De + SpringSolver.DeltaDe);
  Result.Items[ 8, 0]      := TryFormatFloat('Di.min = %s mm',    'Di.min = ---', SpringSolver.Di_Min);
  Result.Items[ 9, 0]      := TryFormatFloat('De.max = %s mm',    'De.max = ---', SpringSolver.De_Max);
  Result.Items[10, 0]      := TryFormatFloat('sk     = %s mm',    'sk     = ---', SpringSolver.DeflectionSk);
  Result.Items[11, 0]      := TryFormatFloat('L      = %s mm',    'L      = ---', SpringSolver.WireLength);
  Result.Items[12, 0]      := TryFormatFloat('m      = %s g',     'm      = ---', SpringSolver.Mass);
  Result.Items[13, 0]      := TryFormatFloat('W12    = %s Nmm',   'W12    = ---', SpringSolver.SpringWorkW12);
  Result.Items[14, 0]      := TryFormatFloat('W0n    = %s Nmm',   'W0n    = ---', SpringSolver.SpringWorkW0n);
  Result.Items[15, 0]      := TryFormatFloat('fe     = %s Hz',    'fe     = ---' ,SpringSolver.NaturalFrequency);
  Result.Items[16, 0]      := TryFormatFloat('Pitch  = %s mm',    'Pitch  = ---', SpringSolver.Pitch);
  Result.Items[17, 0]      := TryFormatFloat('PitchRatio = %s',   'PitchRatio = ---', SpringSolver.PitchRatio);
  Result.Items[18, 0]      := TryFormatFloat('nu         = %s',   'nu         = ---', SpringSolver.SeatingCoefficent);

  if SpringSolver.DynamicLoad then
    Result.Items[19, 0] := ('dynamic load = true')
  else
    Result.Items[19, 0] := ('dynamic load = false');
end;

function TMainForm.CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSectionSpringDrawing;
begin
  Result            := TSectionSpringDrawing.Create('SpringDrawing', aSetting);
  Result.d          := SpringSolver.WireDiameter;
  Result.Dm         := SpringSolver.Dm;
  Result.Lc         := SpringSolver.LengthLc;
  Result.Lx         := DrawingForm.SpringLength.Value;
  Result.Text       := TryFormatFloat('L = %s', 'L = ---', Result.Lx);
  Result.n          := SpringSolver.ActiveColis;
  Result.nt1        := GeometryForm.InactiveCoil1.Value;
  Result.nt2        := GeometryForm.InactiveCoil2.Value;
  Result.ClockWise  := True;
  Result.Fit        := True;
  Result.GroundEnds := SpringSolver.GroundEnds;
  Result.Spacer     := Trunc(DefaultSpacer*aScreenScale);
  Result.Zoom       := aScreenScale;
end;

procedure TMainForm.FormPaint(Sender: TObject);
var
  i: longint;
  Check: boolean;
begin
  FormWindowStateChange(Sender);

  Check := False;
  for i := 0 to ViewMenuItem.Count -1 do if ViewMenuItem.Items[i].Checked then Check := True;
  for i := 0 to TempMenuItem.Count -1 do if TempMenuItem.Items[i].Checked then Check := True;
  for i := 0 to DrawMenuItem.Count -1 do if DrawMenuItem.Items[i].Checked then Check := True;
  for i := 0 to DocsMenuItem.Count -1 do if DocsMenuItem.Items[i].Checked then Check := True;

  if Check then
  begin
    ScreenImage.SetSize(
      Trunc(ScreenImageWidth *ScreenScale),
      Trunc(ScreenImageHeight*ScreenScale));
    PaintTo(ScreenImage, ScreenColor ,ScreenScale, ClientFile);
  end else
  begin
    ScreenImage.LoadFromResource('BACKGROUND');
    VirtualScreen.RedrawBitmap;
  end;

end;

end.

