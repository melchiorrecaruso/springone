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
  BGRABitmap, BGRAShape, BGRASVG, BGRATextFX, BGRABitmapTypes, BGRAUnits, BGRAVirtualScreen,
  Classes, Controls, Dialogs, ExtCtrls, ExtDlgs, Forms, GraphBase, EN10270, EN13906, EN15800,
  Graphics, IniFiles, LResources, Math, Menus, PrintersDlgs, Spin, StdCtrls, SysUtils;

type

  { TMainForm }

  TMainForm = class(TForm)


    CloseMenuItem: TMenuItem;
    CustomProfileMenuItem: TMenuItem;
    CustomSectionMenuItem: TMenuItem;

    DrawingTextMenuItem: TMenuItem;
    DrawMenuItem: TMenuItem;

    ExportMenuItem: TMenuItem;
    ExportProductionMenuItem: TMenuItem;
    ExportReportMenuItem: TMenuItem;
    GeometryMenuItem: TMenuItem;

    MaterialMenuItem: TMenuItem;


    PageSetupDialog: TPageSetupDialog;
    PageSetupMenuItem: TMenuItem;
    ProfileMenuItem: TMenuItem;
    ProductionMenuItem: TMenuItem;
    Production2MenuItem: TMenuItem;

    Selection: TBGRAShape;
    SectionMenuItem: TMenuItem;


    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator9: TMenuItem;
    Separator10: TMenuItem;
    Separator11: TMenuItem;





    ShearModulusMenuItem: TMenuItem;
    YoungModulusMenuItem: TMenuItem;
    F1MenuItem: TMenuItem;
    F2MenuItem: TMenuItem;
    SavePictureDialog: TSavePictureDialog;


    QualityMenuItem: TMenuItem;

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

    SaveMenuItem: TMenuItem;
    ExitMenuItem: TMenuItem;
    EditMenuItem: TMenuItem;
    ApplicationMenuItem: TMenuItem;
    ViewMenuItem: TMenuItem;
    MenuItem8: TMenuItem;
    Design: TMenuItem;
    DocsMenuItem: TMenuItem;
    MenuItem9: TMenuItem;


    VirtualScreen: TBGRAVirtualScreen;

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

    procedure PrintMenuItemClick(Sender: TObject);
    procedure ProductionMenuItemClick(Sender: TObject);
    procedure QualityMenuItemClick(Sender: TObject);

    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure ApplicationMenuItemClick(Sender: TObject);
    procedure CustomSectionMenuItemClick(Sender: TObject);
    procedure VirtualScreenDblClick(Sender: TObject);
    procedure VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure TextMenuItemClick(Sender: TObject);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure ReportMenuItemClick(Sender: TObject);
    procedure VirtualScreenResize(Sender: TObject);
  private

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
    SessionFileName: string;
  public
    function CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSectionSpringDrawing;
    function CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
    function CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;

    procedure PaintTo(var aScreen: TBGRABitmap; aScreenColor: TBGRAPixel; aScreenScale: double; aSetting: TIniFile);

  public
    procedure LoadAll(SessionIniFile: TIniFile);
    procedure SaveAll(SessionIniFile: TIniFile);
    procedure ClearAll;
    procedure Clear;
    procedure Solve;
  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  AboutFrm, ApplicationFrm, Compozer, DrawingFrm, TextFrm, GeometryFrm, LCLIntf, LCLType,
  MaterialFrm, Printers, ProductionFrm, QualityFrm, ReportFrm, UtilsBase;

{ Solve routine }

procedure TMainForm.Solve;
begin
  TOL.Clear;
  SOLVER.Clear;
  GeometryForm.SaveToSolver;      // GeometryForm
  MaterialForm.SaveToSolver;      // MaterialForm
  ProductionForm.SaveToSolver;    // ProductionForm
  QualityForm.SaveToSolver;       // QualityForm
  ApplicationForm.SaveToSolver;   // ApplicationForm
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
  Caption    := ApplicationVer;
  ClientFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'client.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);
  PrinterFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'printer.ini',
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  Clear;
  Selection.Visible := False;
  ScreenImage       := TBGRABitmap.Create;
  ScreenImageWidth  := Screen.Width  - (Width  - VirtualScreen.Width);
  ScreenImageHeight := Screen.Height - (Height - VirtualScreen.Height + LCLIntf.GetSystemMetrics(SM_CYCAPTION));
  ScreenColor.FromString(ClientFile.ReadString('Custom', 'Background Color', 'White'));
  VirtualScreen.Color := ScreenColor;

  PaperName := ClientFile.ReadString('Printer', 'Page.Name', '');
  if PaperName <> '' then
  begin
    Printer.PaperSize.PaperName  := PaperName;
    Printer.Orientation          := TPrinterOrientation(ClientFile.ReadInteger('Printer', 'Page.Orientation',  0 ));
    PageSetupDialog.MarginTop    := TryTextToInt       (ClientFile.ReadString ('Printer', 'Page.MarginTop',    '0'));
    PageSetupDialog.MarginLeft   := TryTextToInt       (ClientFile.ReadString ('Printer', 'Page.MarginLeft',   '0'));
    PageSetupDialog.MarginRight  := TryTextToInt       (ClientFile.ReadString ('Printer', 'Page.MarginRight',  '0'));
    PageSetupDialog.MarginBottom := TryTextToInt       (ClientFile.ReadString ('Printer', 'Page.MarginBottom', '0'));
  end;
  WindowState := wsMaximized;

  MoveX := 0;
  MoveY := 0;
  FormPaint(nil);

  {$ifopt D+}
  Logo := TBGRABitmap.Create;
  Logo.SetSize(2560, 2048);
  // DrawLogo(Logo.Canvas, Logo.Width, Logo.Height);
  // Logo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'BACKGROUND.png');
  Logo.Destroy;
  {$endif}
  SessionFileName := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClientFile.Destroy;
  PrinterFile.Destroy;
  ScreenImage.Destroy;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMaximized then
  begin
    ScreenImageWidth  := VirtualScreen.Width;
    ScreenImageHeight := VirtualScreen.Height;
  end;
end;

procedure TMainForm.Clear;
var
  i: LongInt;
begin
  ClearAll;
  for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
  for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
  for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;
  for i := 0 to DocsMenuItem.Count -1 do DocsMenuItem.Items[i].Checked := False;

  MoveX := 0;
  MoveY := 0;
  MouseIsDown := False;
  ScreenScale := 1.00;
  SessionFileName := '';
end;

procedure TMainForm.ClearAll;
begin
  if Assigned(TextForm         ) then TextForm         .Clear;
  if Assigned(GeometryForm     ) then GeometryForm     .Clear;
  if Assigned(MaterialForm     ) then MaterialForm     .Clear;
  if Assigned(QualityForm      ) then QualityForm      .Clear;
  if Assigned(ProductionForm   ) then ProductionForm   .Clear;
  if Assigned(ApplicationForm  ) then ApplicationForm  .Clear;
end;

procedure TMainForm.LoadAll(SessionIniFile: TIniFile);
begin
  TextForm         .Load(SessionIniFile);
  GeometryForm     .Load(SessionIniFile);
  MaterialForm     .Load(SessionIniFile);
  QualityForm      .Load(SessionIniFile);
  ProductionForm   .Load(SessionIniFile);
  ApplicationForm  .Load(SessionIniFile);
end;

procedure TMainForm.SaveAll(SessionIniFile: TIniFile);
begin
  TextForm         .Save(SessionIniFile);
  GeometryForm     .Save(SessionIniFile);
  MaterialForm     .Save(SessionIniFile);
  QualityForm      .Save(SessionIniFile);
  ProductionForm   .Save(SessionIniFile);
  ApplicationForm  .Save(SessionIniFile);
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

procedure TMainForm.VirtualScreenDblClick(Sender: TObject);
begin
  MoveX := 0;
  MoveY := 0;
  ScreenScale := 1.0;

  FormPaint(Sender);
end;

procedure TMainForm.VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Selection.Visible := False;
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    Px := X;
    Py := Y;
  end;
end;

procedure TMainForm.VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if MouseIsDown then
  begin
    Selection.Visible := not (ssCtrl in Shift);
    if Selection.Visible then
    begin
      Selection.SetBounds(Px, Py, X - Px, Y - Py);
    end else
    begin
      MoveX := MoveX + (X - Px);
      MoveY := MoveY + (Y - Py);
      Px    := X;
      Py    := Y;
      VirtualScreenResize(Sender);
      VirtualScreen.RedrawBitmap;
    end;
  end;
end;

procedure TMainForm.VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
const
  MaxScale = 5;
var
  NewScale: double;
begin
  if MouseIsDown then
  begin
    MouseIsDown := False;
    if Selection.Visible then
    begin
      Selection.Visible := False;
      if ((X - Px) > 0) and ((Y - Py) > 0) then
      begin
        NewScale := Min(ScreenImageWidth / (X - Px), ScreenImageHeight / (Y - Py));
        if ScreenScale*NewScale > MaxScale then
        begin
          Newscale := MaxScale / ScreenScale;
        end;
        ScreenScale := ScreenScale * NewScale;

        MoveX := Trunc(ScreenImageWidth /2 - ((Px + X) / 2 - MoveX) * NewScale);
        MoveY := Trunc(ScreenImageHeight/2 - ((Py + Y) / 2 - MoveY) * NewScale);

        FormPaint(Sender);
      end;
    end else
    begin
      VirtualScreenResize(Sender);
      VirtualScreen.RedrawBitmap;
    end;
  end;
end;

// Virtual Screen Events

procedure TMainForm.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(MoveX, MoveY, ScreenImage, dmSet);
end;

procedure TMainForm.VirtualScreenResize(Sender: TObject);
begin
  MoveX := Max(Min(0, MoveX), VirtualScreen.Width  - ScreenImage.Width );
  MoveY := Max(Min(0, MoveY), VirtualScreen.Height - ScreenImage.Height);
end;

// Menu File

procedure TMainForm.NewMenuItemClick(Sender: TObject);
begin
  ClearAll;
  if (TextForm         .ShowModal = mrOk) and
     (GeometryForm     .ShowModal = mrOk) and
     (MaterialForm     .ShowModal = mrOk) and
     (QualityForm      .ShowModal = mrOk) and
     (ProductionForm   .ShowModal = mrOk) and
     (ApplicationForm  .ShowModal = mrOk) then
  begin
    Quick1MenuItem.Checked := True;
  end;
  Solve();
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
begin
  OpenDialog.Filter := 'SpringOne file (*.springone)|*.springone|All files (*.*)|*.*|;';
  if OpenDialog.Execute then
  begin
    SessionFileName := OpenDialog.FileName;
    SessionIniFile  := TIniFile.Create(SessionFileName,
      [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

    LoadAll(SessionIniFile);
    SessionIniFile.Destroy;
    begin
      Quick1MenuItem.Checked := True;
    end;
  end;
  Solve();
end;

procedure TMainForm.SaveMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
begin
  if SessionFileName = '' then
  begin
    SaveAsMenuItemClick(Sender);
  end else
  begin
    SessionIniFile  := TIniFile.Create(SessionFileName,
      [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

    SaveAll(SessionIniFile);
    SessionIniFile.Destroy;
  end;
end;

procedure TMainForm.SaveAsMenuItemClick(Sender: TObject);
begin
  SaveDialog.Filter := 'SpringOne file (*.springone)|*.springone|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    SessionFileName := SaveDialog.FileName;

    SaveMenuItemClick(Sender);
  end;
end;

procedure TMainForm.CloseMenuItemClick(Sender: TObject);
begin
  Clear;
  Solve();
end;

procedure TMainForm.ExportMenuItemClick(Sender: TObject);
var
  Page: TBGRABitmap;
begin
  SavePictureDialog.Filter :=
    'Graphics (*.png;*.xpm;*.bmp;*.jpeg;*.jpg;)|*.png;*.xpm;*.bmp;*.jpeg;*.jpg|' +
    'PNG Files (*.png)|*.png|' + 'Pixmap Files (*.xpm)|*.xpm|' + 'Bitmap Files (*.bmp)|*.bmp)|' +
    'JPEG Files (*.jpeg;*.jpg;)|*.jpeg;*.jpg|' + 'Tutti i file (*.*)|*.*|;';
  if SavePictureDialog.Execute then
  begin
    Solve();
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
    ClientFile.WriteInteger('Printer', 'Page.Orientation',  LongInt(Printer.Orientation));
    ClientFile.WriteInteger('Printer', 'Page.MarginTop',    PageSetupDialog.MarginTop   );
    ClientFile.WriteInteger('Printer', 'Page.MarginLeft',   PageSetupDialog.MarginLeft  );
    ClientFile.WriteInteger('Printer', 'Page.MarginRight',  PageSetupDialog.MarginRight );
    ClientFile.WriteInteger('Printer', 'Page.MarginBottom', PageSetupDialog.MarginBottom);
  end;
end;

procedure TMainForm.PrintMenuItemClick(Sender: TObject);
var
  OffSetX: LongInt;
  OffSetY: LongInt;
  Page: TBGRABitmap;
  Scale: Double;
begin
  if PrintDialog.Execute then
  begin
    Solve();
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

// Menu Edit

procedure TMainForm.TextMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  TextForm.Save(SessionIniFile);
  if TextForm.ShowModal <> mrOk then
  begin
    TextForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.GeometryMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  GeometryForm.Save(SessionIniFile);
  if GeometryForm.ShowModal <> mrOk then
  begin
    GeometryForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.MaterialMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  MaterialForm.Save(SessionIniFile);
  if MaterialForm.ShowModal <> mrOk then
  begin
    MaterialForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.QualityMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  QualityForm.Save(SessionIniFile);
  if QualityForm.ShowModal <> mrOk then
  begin
    QualityForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.ProductionMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  ProductionForm.Save(SessionIniFile);
  if ProductionForm.ShowModal <> mrOk then
  begin
    ProductionForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.ApplicationMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  ApplicationForm.Save(SessionIniFile);
  if ApplicationForm.ShowModal <> mrOk then
  begin
    ApplicationForm.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

// Memu View

procedure TMainForm.ViewMenuItemClick(Sender: TObject);
var
  i: longint;
begin
  if (Sender <> TempMenuItem) and (Sender <> DrawMenuItem) then
  begin
    for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
    for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
    for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;
    for i := 0 to DocsMenuItem.Count -1 do DocsMenuItem.Items[i].Checked := False;

    MoveX := 0;
    MoveY := 0;
    ScreenScale := 1;
    if Assigned(Sender) then
      TMenuItem(Sender).Checked := True;
    FormPaint(Sender);
  end;
end;

procedure TMainForm.CustomSectionMenuItemClick(Sender: TObject);
var
  i: longint;
  Value: double;
begin
  if (Sender <> TempMenuItem) and (Sender <> DrawMenuItem) then
  begin
    DrawingForm.SpringLength.MinValue := SOLVER.LengthLc;
    DrawingForm.SpringLength.MaxValue := SOLVER.LengthL0;

    Value := DrawingForm.SpringLength.Value;
    if DrawingForm.ShowModal = mrOk then
    begin
      for i := 0 to ViewMenuItem.Count -1 do ViewMenuItem.Items[i].Checked := False;
      for i := 0 to TempMenuItem.Count -1 do TempMenuItem.Items[i].Checked := False;
      for i := 0 to DrawMenuItem.Count -1 do DrawMenuItem.Items[i].Checked := False;

      if Assigned(Sender) then
        TMenuItem(Sender).Checked := True;
    end else
      DrawingForm.SpringLength.Value := Value;

    FormPaint(Sender);
  end;
end;

// Menu Documentation

procedure TMainForm.ReportMenuItemClick(Sender: TObject);
begin
  Solve();

  ReportForm.CreateReport;
  ReportForm.ShowModal;
end;

procedure TMainForm.ExportReportMenuItemClick(Sender: TObject);
begin
  Solve();
  SaveDialog.Filter := 'Text file (*.txt)|*.txt|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    ReportForm.CreateReport;
    ReportForm.Memo.Lines.SaveToFile(SaveDialog.FileName);
  end;
  SaveDialog.FileName := SessionFileName;
end;

procedure TMainForm.ExportProductionMenuItemClick(Sender: TObject);
var
  SVG: TBGRASvg;
begin
  Solve();
  SaveDialog.Filter := 'Svg file (*.svg)|*.svg|All files (*.*)|*.*|;';
  if SaveDialog.Execute then
  begin
    SVG := TBGRASVG.Create;
    SVG.LoadFromResource('TEMPLATE');
    SVG.AsUTF8String := CreateProductionDrawing(SVG.AsUTF8String, PrinterFile);
    SVG.SaveToFile(SaveDialog.FileName);
    SVG.Destroy;
  end;
  SaveDialog.FileName := SessionFileName;
end;

// Menu Help

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  AboutForm.AboutNameLabel.Caption := ApplicationVer;
  AboutForm.ShowModal;
end;

// FormPaint

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
    MoveX := (VirtualScreen.Width  - ScreenImageWidth ) div 2;
    MoveY := (VirtualScreen.Height - ScreenImageHeight) div 2;
    ScreenImage.LoadFromResource('BACKGROUND');
    VirtualScreen.RedrawBitmap;
  end;
end;

procedure TMainForm.PaintTo(var aScreen: TBGRABitmap; aScreenColor: TBGRAPixel; aScreenScale: double; aSetting: TIniFile);
var
  i: longint;
  Bit: array of TBGRABitmap = nil;
  BucklingDiagram: TChart;
  ForceDiagram: TChart;
  GoodmanDiagram: TChart;
  Load1Diagram: TChart;
  Load2Diagram: TChart;
  MessageList: TReportTable;
  QualityTable: TReportTable;
  Quick1List: TReportTable;
  ReportList1: TReportTable;
  Quick1Table: TReportTable;
  ShearModulusDiagram: TChart;
  SpringDrawing: TSectionSpringDrawing;
  YoungModulusDiagram: TChart;
  SVG: TBGRASvg;
begin
  ErrorMessage.Clear;
  WarningMessage.Clear;
  aScreen.Fill(aScreenColor);

  SOLVER.Solve;
  ForceDiagram        := CreateForceDisplacementChart  (aScreenScale, aSetting);
  GoodmanDiagram      := CreateGoodmanChart            (aScreenScale, aSetting);
  BucklingDiagram     := CreateBucklingChart           (aScreenScale, aSetting);
  ShearModulusDiagram := CreateShearModulusChart       (aScreenScale, aSetting);
  YoungModulusDiagram := CreateYoungModulusChart       (aScreenScale, aSetting);
  Load1Diagram        := CreateLoadF1Chart             (aScreenScale, aSetting);
  Load2Diagram        := CreateLoadF2Chart             (aScreenScale, aSetting);
  MessageList         := CreateMessageList             (aScreenScale, aSetting);
  Quick1Table         := CreateQuick1Table             (aScreenScale, aSetting);
  QualityTable        := CreateQualityTable            (aScreenScale, aSetting);
  Quick1List          := CreateQuick1List              (aScreenScale, aSetting);
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

    Bit[0].SetSize(Quick1List.Width + Quick1List.Spacer, Quick1List.Height + Quick1List.Spacer);
    Quick1List.Draw(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

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

    Bit[4].SetSize(Quick1Table.Width + Quick1Table.Spacer, Quick1Table.Height + Quick1Table.Spacer);
    Quick1Table.Draw(Bit[4].Canvas, Bit[4].Width, Bit[4].Height);

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
  Quick1Table.Destroy;
  Quick1List.Destroy;
  SpringDrawing.Destroy;

  VirtualScreenResize(nil);
  VirtualScreen.RedrawBitmap;
end;

// Create Diagrams

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

end.

