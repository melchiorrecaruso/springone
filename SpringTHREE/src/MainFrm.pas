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
  Classes, Controls, Dialogs, EN10270, EN13906, EN15800, ExtCtrls, ExtDlgs, Forms, GraphBase,
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
    {$IFDEF ONE}
    function CreateForceDisplacementDiagram(const aScreenScale: double; aSetting: TIniFile): TForceDisplacementDiagram;
    function CreateBucklingDiagram         (const aScreenScale: double; aSetting: TIniFile): TBucklingDiagram;
    function CreateLoad1Diagram            (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateLoad2Diagram            (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    {$ENDIF}
    {$IFDEF THREE}

    {$ENDIF}

    function CreateGoodmanDiagram          (const aScreenScale: double; aSetting: TIniFile): TGoodmanDiagram;
    function CreateShearModulusDiagram     (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;
    function CreateYoungModulusDiagram     (const aScreenScale: double; aSetting: TIniFile): TLinearTemperatureDiagram;


    function CreateMessageList             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateReportTable             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateQualityTable            (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateReportList0             (const aScreenScale: double; aSetting: TIniFile): TReportTable;
    function CreateSpringDrawing           (const aScreenScale: double; aSetting: TIniFile): TSectionSpringDrawing;

  public
    procedure LoadAll(SessionIniFile: TIniFile);
    procedure SaveAll(SessionIniFile: TIniFile);
    procedure ClearAll;


    function CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
    function CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;
    procedure PaintTo(var aScreen: TBGRABitmap; aScreenColor: TBGRAPixel; aScreenScale: double; aSetting: TIniFile);
    procedure Clear;
    procedure Solve;
  end;


var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  AboutFrm, ApplicationFrm, DrawingFrm, TextFrm, GeometryFrm, GeometryFrmThree,
  LCLIntf, LCLType, MaterialFrm, Printers, ProductionFrm, QualityFrm, ReportFrm, UtilsBase;

{ Solve routine }

procedure TMainForm.Solve;
begin
  TOL.Clear;
  SOLVER.Clear;
  {$IFDEF ONE}
  GeometryForm.SaveToSolver;      // GeometryForm
  {$ENDIF}
  {$IFDEF THREE}
  GeometryFormThree.SaveToSolver; // GeometryFormThree
  {$ENDIF}
  MaterialForm.SaveToSolver;      // MaterialForm
//ProductionForm.SaveToSolver;    // ProductionForm
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

  MoveX := 0;
  MoveY := 0;
  FormPaint(nil);

  {$ifopt D+}
  Logo := TBGRABitmap.Create;
  Logo.SetSize(2560, 2048);
  DrawLogo(Logo.Canvas, Logo.Width, Logo.Height);
  Logo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'BACKGROUND.png');
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
  if Assigned(GeometryFormThree) then GeometryFormThree.Clear;
  if Assigned(MaterialForm     ) then MaterialForm     .Clear;
  if Assigned(QualityForm      ) then QualityForm      .Clear;
  if Assigned(ProductionForm   ) then ProductionForm   .Clear;
  if Assigned(ApplicationForm  ) then ApplicationForm  .Clear;
end;

procedure TMainForm.LoadAll(SessionIniFile: TIniFile);
begin
  TextForm         .Load(SessionIniFile);
  GeometryForm     .Load(SessionIniFile);
  GeometryFormThree.Load(SessionIniFile);
  MaterialForm     .Load(SessionIniFile);
  QualityForm      .Load(SessionIniFile);
  ProductionForm   .Load(SessionIniFile);
  ApplicationForm  .Load(SessionIniFile);
end;

procedure TMainForm.SaveAll(SessionIniFile: TIniFile);
begin
  TextForm         .Save(SessionIniFile);
  GeometryForm     .Save(SessionIniFile);
  GeometryFormThree.Save(SessionIniFile);
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

procedure TMainForm.VirtualScreenDblClick(Sender: TObject);
begin
  MoveX := 0;
  MoveY := 0;
  ScreenScale := 1.0;

  FormPaint(Sender);
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
     {$IFDEF ONE}
     (GeometryForm     .ShowModal = mrOk) and
     {$ENDIF}
     {$IFDEF THREE}
     (GeometryFormThree.ShowModal = mrOk) and
     {$ENDIF}
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
  {$IFDEF ONE}
  OpenDialog.Filter := 'SpringOne file (*.springone)|*.springone|All files (*.*)|*.*|;';
  {$ENDIF}
  {$IFDEF THREE}
  OpenDialog.Filter := 'SpringThree file (*.springthree)|*.springthree|All files (*.*)|*.*|;';
  {$ENDIF}

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
  {$IFDEF ONE}
  SaveDialog.Filter := 'SpringOne file (*.springone)|*.springone|All files (*.*)|*.*|;';
  {$ENDIF}
  {$IFDEF THREE}
  SaveDialog.Filter := 'SpringThree file (*.springthree)|*.springthree|All files (*.*)|*.*|;';
  {$ENDIF}

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

  {$IFDEF ONE}
  GeometryForm.Save(SessionIniFile);
  if GeometryForm.ShowModal <> mrOk then
  begin
    GeometryForm.Load(SessionIniFile);
  end;
  {$ENDIF}
  {$IFDEF THREE}
  GeometryFormThree.Save(SessionIniFile);
  if GeometryFormThree.ShowModal <> mrOk then
  begin
    GeometryFormThree.Load(SessionIniFile);
  end;
  {$ENDIF}
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
    {$IFDEF ONE}
    DrawingForm.SpringLength.MinValue := SOLVER.LengthLc;
    DrawingForm.SpringLength.MaxValue := SOLVER.LengthL0;
    {$ENDIF}
    {$IFDEF THREE}
    DrawingForm.SpringLength.MinValue := 0;
    DrawingForm.SpringLength.MinValue := SOLVER.alpha2;
    {$ENDIF}
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


{$IFDEF ONE}
  {$I 'MainFrmRoutines.inc'}
{$ENDIF}
{$IFDEF THREE}

{$ENDIF}

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
  Result[1, 1]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 1);
  Result[2, 1]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 1);
  Result[3, 1]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 1);
  Result[4, 1]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 1);
  Result[5, 1]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 1);
  Result[6, 1]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 1);

  Result[0, 2]             := '2';
  Result[1, 2]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 2);
  Result[2, 2]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 2);
  Result[3, 2]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 2);
  Result[4, 2]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 2);
  Result[5, 2]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 2);
  Result[6, 2]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 2);

  Result[0, 3]             := '3';
  Result[1, 3]             := TryFormatBool('x', ' ', TOL.DmQualityGrade = 3);
  Result[2, 3]             := TryFormatBool('x', ' ', TOL.L0QualityGrade = 3);
  Result[3, 3]             := TryFormatBool('x', ' ', TOL.F1QualityGrade = 3);
  Result[4, 3]             := TryFormatBool('x', ' ', TOL.F2QualityGrade = 3);
  Result[5, 3]             := TryFormatBool('x', ' ', TOL.E1QualityGrade = 3);
  Result[6, 3]             := TryFormatBool('x', ' ', TOL.E2QualityGrade = 3);

  Result[0, 4]             := ' Tol.';
  Result[1, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.CoilDiameterTolerance);
  Result[2, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.LengthL0Tolerance);
  Result[3, 4]             := TryFormatFloat('%s N',  ' --- ', TOL.LoadF1Tolerance);
  Result[4, 4]             := TryFormatFloat('%s N',  ' --- ', TOL.LoadF2Tolerance);
  Result[5, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE1);
  Result[6, 4]             := TryFormatFloat('%s mm', ' --- ', TOL.EccentricityE2);
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
    {$IFDEF ONE}
    ScreenImage.LoadFromResource('BACKONE');
    {$ENDIF}
    {$IFDEF THREE}
    ScreenImage.LoadFromResource('BACKTHREE');
    {$ENDIF}
    VirtualScreen.RedrawBitmap;
  end;

end;

end.

