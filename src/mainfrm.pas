{ EN13906-1 Helical Compression Spring Designer

  Copyright (C) 2022-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
{$i defines.inc}

interface

uses
  BGRABitmap, BGRAShape, BGRASVG, BGRATextFX, BGRABitmapTypes, BGRAUnits,
  BGRAVirtualScreen, Classes, Controls, Dialogs, ExtCtrls, ExtDlgs, Forms,
  GraphBase, SpringMaterials, SpringSolvers, SpringTolerances, Graphics, IniFiles,
  LResources, Math, Menus, PrintersDlgs, Spin, StdCtrls, ActnList, SysUtils;

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
    Separator8: TMenuItem;
    WizardMenuItem: TMenuItem;
    Separator7: TMenuItem;
    UseImperialSystemMenuItem: TMenuItem;


    PageSetupDialog: TPageSetupDialog;
    PageSetupMenuItem: TMenuItem;
    ProfileMenuItem: TMenuItem;
    ProductionMenuItem: TMenuItem;
    ProductionDrawingMenuItem: TMenuItem;

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


    procedure ExportMenuItemClick(Sender: TObject);
    procedure ExportProductionMenuItemClick(Sender: TObject);
    procedure ExportReportMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);

    procedure GeometryMenuItemClick(Sender: TObject);

    procedure MaterialMenuItemClick(Sender: TObject);
    procedure AboutMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);

    procedure PrintMenuItemClick(Sender: TObject);
    procedure ProductionMenuItemClick(Sender: TObject);
    procedure QualityMenuItemClick(Sender: TObject);

    procedure NewMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveAsMenuItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);

    procedure ExitMenuItemClick(Sender: TObject);
    procedure ApplicationMenuItemClick(Sender: TObject);
    procedure CustomSectionMenuItemClick(Sender: TObject);
    procedure SolveExecute(Sender: TObject);
    procedure UseImperialSystemMenuItemClick(Sender: TObject);
    procedure VirtualScreenDblClick(Sender: TObject);
    procedure VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure TextMenuItemClick(Sender: TObject);
    procedure ViewMenuItemClick(Sender: TObject);
    procedure ReportMenuItemClick(Sender: TObject);
    procedure VirtualScreenResize(Sender: TObject);
    procedure WizardMenuItemClick(Sender: TObject);
  private
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
    function CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSpringDrawing;
    function CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
    function CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;

    procedure PaintTo(var aScreen: TBGRABitmap; aScreenScale: double; aSetting: TIniFile);

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
  AboutFrm, ADim, Compozer, DateUtils, DrawingFrm, TextFrm,

  {$IFDEF MODULE1} GeometryFrm1, ApplicationFrm1, {$ENDIF}
  {$IFDEF MODULE3} GeometryFrm3, ApplicationFrm3, {$ENDIF}

  LCLIntf, LCLType, LibLink, MaterialFrm, Printers, ProductionFrm,
  QualityFrm, ReportFrm, Setting, UtilsBase;


{ Solve routine }

procedure TMainForm.Solve;
begin
  if WindowState <> wsMaximized then
  begin
  //WindowState := wsMaximized;
  end;

  SpringTolerance.Clear;
  SpringSolver.Clear;
  {$IFDEF MODULE1}
  GeometryForm1.SaveToSolver;     // GeometryForm1
  MaterialForm.SaveToSolver;      // MaterialForm
  ProductionForm.SaveToSolver;    // ProductionForm
  QualityForm.SaveToSolver;       // QualityForm
  ApplicationForm1.SaveToSolver;  // ApplicationForm
  // Solve
  MainForm.FormPaint(nil);
  {$ENDIF}

  {$IFDEF MODULE3}
  GeometryForm3.SaveToSolver;     // GeometryForm3
  MaterialForm.SaveToSolver;      // MaterialForm
  ProductionForm.SaveToSolver;    // ProductionForm
  QualityForm.SaveToSolver;       // QualityForm
  ApplicationForm3.SaveToSolver;  // ApplicationForm
  // Solve
  MainForm.FormPaint(nil);
  {$ENDIF}
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  PaperName: string;
  {$ifopt D+}
  Logo: TBGRABitmap;
  {$endif}
begin
  Caption := ApplicationVer;
  DefaultFormatSettings.DecimalSeparator  := '.';
  DefaultFormatSettings.ThousandSeparator := ',';

  MainForm.Top    := ClientFile.ReadInteger('MainForm', 'Top',    MainForm.Top);
  MainForm.Left   := ClientFile.ReadInteger('MainForm', 'Left',   MainForm.Left);
  MainForm.Height := ClientFile.ReadInteger('MainForm', 'Height', MainForm.Height);
  MainForm.Width  := ClientFile.ReadInteger('MainForm', 'Width',  MainForm.Width);

  Clear;
  Selection.Visible := False;
  ScreenImage       := TBGRABitmap.Create;
  ScreenImageWidth  := ClientFile.ReadInteger('MainForm', 'Screen.Width',  800);
  ScreenImageHeight := ClientFile.ReadInteger('MainForm', 'Screen.Height', 600);
  ScreenColor.FromString(ClientFile.ReadString('Custom', 'BackgroundColor', 'White'));
  VirtualScreen.Color := ScreenColor;

  PaperName := ClientFile.ReadString('Printer', 'Page.Name', '');
  if PaperName <> '' then
  begin
    Printer.PaperSize.PaperName  := PaperName;
    Printer.Orientation          := TPrinterOrientation(ClientFile.ReadInteger('Printer', 'Page.Orientation',  0));
    PageSetupDialog.MarginTop    :=                     ClientFile.ReadInteger('Printer', 'Page.MarginTop',    0);
    PageSetupDialog.MarginLeft   :=                     ClientFile.ReadInteger('Printer', 'Page.MarginLeft',   0);
    PageSetupDialog.MarginRight  :=                     ClientFile.ReadInteger('Printer', 'Page.MarginRight',  0);
    PageSetupDialog.MarginBottom :=                     ClientFile.ReadInteger('Printer', 'Page.MarginBottom', 0);
  end;
  UseImperialSystem := UseImperialSystemMenuItem.Checked;

  MoveX := 0;
  MoveY := 0;
  FormPaint(nil);

  {$ifopt D+}
  Logo := TBGRABitmap.Create;
  Logo.SetSize(ScreenImageWidth, ScreenImageHeight);
  DrawLogo(Logo.Canvas, Logo.Width, Logo.Height);
  Logo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'BACKGROUND.png');
  Logo.Destroy;
  {$endif}
  SessionFileName := '';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ScreenImage.Destroy;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Windowstate <> wsMaximized then
  begin
    ClientFile.WriteInteger('MainForm', 'Top',    MainForm.Top);
    ClientFile.WriteInteger('MainForm', 'Left',   MainForm.Left);
    ClientFile.WriteInteger('MainForm', 'Height', MainForm.Height);
    ClientFile.WriteInteger('MainForm', 'Width',  MainForm.Width);
  end;
  ClientFile.WriteInteger('MainForm', 'Screen.Width',  ScreenImageWidth );
  ClientFile.WriteInteger('MainForm', 'Screen.Height', ScreenImageHeight);
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
  ScreenScale := 1.0;
  SessionFileName := '';
end;

procedure TMainForm.ClearAll;
begin
  if Assigned(TextForm         ) then TextForm         .Clear;
  {$IFDEF MODULE1}
  if Assigned(GeometryForm1    ) then GeometryForm1    .Clear;
  if Assigned(ApplicationForm1 ) then ApplicationForm1 .Clear;
  {$ENDIF}
  {$IFDEF MODULE3}
  if Assigned(GeometryForm3    ) then GeometryForm3    .Clear;
  if Assigned(ApplicationForm3 ) then ApplicationForm3 .Clear;
  {$ENDIF}
  if Assigned(MaterialForm     ) then MaterialForm     .Clear;
  if Assigned(QualityForm      ) then QualityForm      .Clear;
  if Assigned(ProductionForm   ) then ProductionForm   .Clear;
end;

procedure TMainForm.LoadAll(SessionIniFile: TIniFile);
begin
  TextForm         .Load(SessionIniFile);
  {$IFDEF MODULE1}
  GeometryForm1    .Load(SessionIniFile);
  ApplicationForm1 .Load(SessionIniFile);
  {$ENDIF}
  {$IFDEF MODULE3}
  GeometryForm3    .Load(SessionIniFile);
  ApplicationForm3 .Load(SessionIniFile);
  {$ENDIF}
  MaterialForm     .Load(SessionIniFile);
  QualityForm      .Load(SessionIniFile);
  ProductionForm   .Load(SessionIniFile);
end;

procedure TMainForm.SaveAll(SessionIniFile: TIniFile);
begin
  TextForm         .Save(SessionIniFile);
  {$IFDEF MODULE1}
  GeometryForm1    .Save(SessionIniFile);
  ApplicationForm1 .Save(SessionIniFile);
  {$ENDIF}
  {$IFDEF MODULE3}
  GeometryForm3    .Save(SessionIniFile);
  ApplicationForm3 .Save(SessionIniFile);
  {$ENDIF}
  MaterialForm     .Save(SessionIniFile);
  QualityForm      .Save(SessionIniFile);
  ProductionForm   .Save(SessionIniFile);
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
        NewScale := Math.Min(ScreenImageWidth / (X - Px), ScreenImageHeight / (Y - Py));
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
  MoveX := Math.Max(Math.Min(0, MoveX), VirtualScreen.Width  - ScreenImage.Width );
  MoveY := Math.Max(Math.Min(0, MoveY), VirtualScreen.Height - ScreenImage.Height);
end;

// Menu File

procedure TMainForm.NewMenuItemClick(Sender: TObject);
begin
  ClearAll;
  WizardMenuItemClick(Sender);
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
var
  SessionIniFile: TIniFile;
begin
  {$IFDEF MODULE1} OpenDialog.Filter := 'SpringOne file (*.spring1)|*.spring1|;';   {$ENDIF}
  {$IFDEF MODULE3} OpenDialog.Filter := 'SpringThree file (*.spring3)|*.spring3|;'; {$ENDIF}
  if OpenDialog.Execute then
  begin
    SessionFileName := OpenDialog.FileName;
    SessionIniFile  := TIniFile.Create(SessionFileName,
      [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

    LoadAll(SessionIniFile);
    SessionIniFile.Destroy;
    begin
      ViewMenuItemClick(Quick1MenuItem);
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
  {$IFDEF MODULE1} SaveDialog.Filter := 'SpringOne file (*.spring1)|*.spring1|All files (*.*)|*.*|;'; {$ENDIF}
  {$IFDEF MODULE3} SaveDialog.Filter := 'SpringThree file (*.spring3)|*.spring3|All files (*.*)|*.*|;'; {$ENDIF}
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
    if ProductionDrawingMenuItem.Checked then
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
    if ProductionDrawingMenuItem.Checked then
      Scale := Printer.PageHeight / Printer.PageWidth
    else
      Scale := ScreenImageHeight  / ScreenImageWidth;

    Page := CreatePage(ScreenImageWidth, Trunc(ScreenImageWidth*Scale), 4.0, PrinterFile);
    Printer.BeginDoc;
    Scale := Math.Min(Printer.PageWidth /Page.Width,
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

procedure TMainForm.WizardMenuItemClick(Sender: TObject);
begin
  if (TextForm         .ShowModal = mrOk) and
     {$IFDEF MODULE1}
     (GeometryForm1    .ShowModal = mrOk) and
     {$ENDIF}
     {$IFDEF MODULE3}
     (GeometryForm3    .ShowModal = mrOk) and
     {$ENDIF}
     (MaterialForm     .ShowModal = mrOk) and
     (QualityForm      .ShowModal = mrOk) and
     (ProductionForm   .ShowModal = mrOk) and
     {$IFDEF MODULE1}
     (ApplicationForm1 .ShowModal = mrOk) then
     {$ENDIF}
     {$IFDEF MODULE3}
     (ApplicationForm3 .ShowModal = mrOk) then
     {$ENDIF}
  begin
    Quick1MenuItem.Checked := True;
  end;
  Solve();
end;

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
  {$IFDEF MODULE1}
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  GeometryForm1.Save(SessionIniFile);
  if GeometryForm1.ShowModal <> mrOk then
  begin
    GeometryForm1.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
  {$ENDIF}

  {$IFDEF MODULE3}
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  GeometryForm3.Save(SessionIniFile);
  if GeometryForm3.ShowModal <> mrOk then
  begin
    GeometryForm3.Load(SessionIniFile);
  end;
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
  {$ENDIF}
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

  {$IFDEF MODULE1}
  ApplicationForm1.Save(SessionIniFile);
  if ApplicationForm1.ShowModal <> mrOk then
  begin
    ApplicationForm1.Load(SessionIniFile);
  end;
   {$ENDIF}
  {$IFDEF MODULE3}
  ApplicationForm3.Save(SessionIniFile);
  if ApplicationForm3.ShowModal <> mrOk then
  begin
    ApplicationForm3.Load(SessionIniFile);
  end;
   {$ENDIF}
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
    {$IFDEF MODULE1}
    DrawingForm.SpringLength.MinValue := SpringSolver.LengthLc.Value([pMilli]);
    DrawingForm.SpringLength.MaxValue := SpringSolver.LengthL0.Value([pMilli]);

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
    {$ENDIF}
  end;
end;

procedure TMainForm.SolveExecute(Sender: TObject);
begin
  Solve();
end;

procedure TMainForm.UseImperialSystemMenuItemClick(Sender: TObject);
var
  I: longint;
  Item: TMenuItem = nil;
begin
  UseImperialSystem                := not UseImperialSystem;
  UseImperialSystemMenuItem.Checked :=     UseImperialSystem;

  for I := 0 to ViewMenuItem.Count -1 do
      if ViewMenuItem.Items[I].Checked then Item := ViewMenuItem.Items[I];

  for I := 0 to TempMenuItem.Count -1 do
    if TempMenuItem.Items[I].Checked then Item := ViewMenuItem.Items[I];

  for I := 0 to DrawMenuItem.Count -1 do
    if DrawMenuItem.Items[I].Checked then Item := ViewMenuItem.Items[I];

  for I := 0 to DocsMenuItem.Count -1 do
    if DocsMenuItem.Items[I].Checked then Item := ViewMenuItem.Items[I];

  if Assigned(Item) then
    ViewMenuItemClick(Item);
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
  Check := False;
  for i := 0 to ViewMenuItem.Count -1 do if ViewMenuItem.Items[i].Checked then Check := True;
  for i := 0 to TempMenuItem.Count -1 do if TempMenuItem.Items[i].Checked then Check := True;
  for i := 0 to DrawMenuItem.Count -1 do if DrawMenuItem.Items[i].Checked then Check := True;
  for i := 0 to DocsMenuItem.Count -1 do if DocsMenuItem.Items[i].Checked then Check := True;

  ScreenImageWidth  := Max(ScreenImageWidth,  VirtualScreen.Width );
  ScreenImageHeight := Max(ScreenImageHeight, VirtualScreen.Height);

  if Check then
  begin
    ScreenImage.SetSize(
      Trunc(ScreenImageWidth *ScreenScale),
      Trunc(ScreenImageHeight*ScreenScale));
    PaintTo(ScreenImage, ScreenScale, ClientFile);
  end else
  begin
    MoveX := (VirtualScreen.Width  - ScreenImageWidth ) div 2;
    MoveY := (VirtualScreen.Height - ScreenImageHeight) div 2;
    ScreenImage.LoadFromResource('BACKGROUND');
    VirtualScreen.RedrawBitmap;
  end;
end;

procedure TMainForm.PaintTo(var aScreen: TBGRABitmap; aScreenScale: double; aSetting: TIniFile);
var
  i: longint;
  Chart: TChart;
  Compozer: TCompozer;
  Bit: array of TBGRABitmap = nil;
  SpringDrawing: TSpringDrawing;
  SVG: TBGRASvg;

  Start: TDateTime;
begin
  ErrorMessage.Clear;
  WarningMessage.Clear;

  SpringSolver.Solve(SpringTolerance);
  Compozer := TCompozer.Create(aSetting);

  Start := Now;
  // check error
  if not SpringSolver.Check then
  begin
    Compozer.DrawMessageList(aScreen, aScreenScale);
  end else
  // Quick1
  if Quick1MenuItem.Checked then
  begin
    Compozer.DrawQuick1(aScreen, aScreenScale);
  end else
  // Quick2
  if Quick2MenuItem.Checked then
  begin
    Compozer.DrawQuick2(aScreen, aScreenScale);
  end else
  // Quick3
  if Quick3MenuItem.Checked then
  begin
    Compozer.DrawQuick3(aScreen, aScreenScale);
  end else
  // Force & displacement chart
  if ForceMenuItem.Checked then
  begin
    Chart := Compozer.CreateForceDisplacementChart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // Goodman chart
  if GoodmanMenuItem .Checked then
  begin
    Chart := Compozer.CreateGoodmanChart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // Buckling chart
  if BucklingMenuItem.Checked then
  begin
    Chart := Compozer.CreateBucklingChart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // Shear modulus chart
  if ShearModulusMenuItem.Checked then
  begin
    Chart := Compozer.CreateShearModulusChart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // Young modulus chart
  if YoungModulusMenuItem.Checked then
  begin
    Chart := Compozer.CreateYoungModulusChart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // F1-Load chart
  if F1MenuItem.Checked then
  begin
    Chart := Compozer.CreateLoadF1Chart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // F2-Load chart
  if F2MenuItem.Checked then
  begin
    Chart := Compozer.CreateLoadF2Chart(aScreenScale);
    Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
    Chart.Destroy;
  end else
  // Spring section drawing
  if SectionMenuItem.Checked then
  begin
    SetLength(Bit, 3);
    for i := Low(Bit) to High(Bit) do
      Bit[i] := TBGRABitmap.Create;

    Bit[0].SetSize(aScreen.Width div 3, aScreen.Height);
    Bit[1].SetSize(aScreen.Width div 3, aScreen.Height);
    Bit[2].SetSize(aScreen.Width div 3, aScreen.Height);

    {$IFDEF MODULE1}
    SpringDrawing            := Compozer.CreateSpringDrawing(aScreenScale);
    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := True;
    SpringDrawing.Lx         := SpringSolver.LengthL0.Value([pMilli]);
    SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInSection(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := SpringSolver.LengthL1.Value([pMilli]);
    SpringDrawing.Caption    := Format('L1 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInSection(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := SpringSolver.LengthL2.Value([pMilli]);
    SpringDrawing.Caption    := Format('L2 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInSection(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);

    Bit[0].Draw(aScreen.Canvas, Bit[0].Width * 0, 0, True);
    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 1, 0, True);
    Bit[2].Draw(aScreen.Canvas, Bit[2].Width * 2, 0, True);
    SpringDrawing.Destroy;
    {$ENDIF}

    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil;
  end else

  // Spring profile drawing
  if ProfileMenuItem.Checked then
  begin
    SetLength(Bit, 3);
    for i := Low(Bit) to High(Bit) do
      Bit[i] := TBGRABitmap.Create;

    Bit[0].SetSize(aScreen.Width div 3, aScreen.Height);
    Bit[1].SetSize(aScreen.Width div 3, aScreen.Height);
    Bit[2].SetSize(aScreen.Width div 3, aScreen.Height);

    {$IFDEF MODULE1}
    SpringDrawing            := Compozer.CreateSpringDrawing(aScreenScale);
    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := True;
    SpringDrawing.Lx         := SpringSolver.LengthL0.Value([pMilli]);
    SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInProfile(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := SpringSolver.LengthL1.Value([pMilli]);
    SpringDrawing.Caption    := Format('L1 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInProfile(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := SpringSolver.LengthL2.Value([pMilli]);
    SpringDrawing.Caption    := Format('L2 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInProfile(Bit[2].Canvas, Bit[2].Width, Bit[2].Height);

    Bit[0].Draw(aScreen.Canvas, Bit[0].Width * 0, 0, True);
    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 1, 0, True);
    Bit[2].Draw(aScreen.Canvas, Bit[2].Width * 2, 0, True);
    SpringDrawing.Destroy;
    {$ENDIF}

    for i := Low(Bit) to High(Bit) do
      Bit[i].Destroy;
    Bit := nil;
  end else

  // Custom section spring drawing
  if CustomSectionMenuItem.Checked then
  begin
    SetLength(Bit, 2);
    Bit[0] := TBGRABitmap.Create;
    Bit[1] := TBGRABitmap.Create;
    Bit[0].SetSize(aScreen.Width, aScreen.Height);
    Bit[1].SetSize(aScreen.Width, aScreen.Height);
    {$IFDEF MODULE1}
    SpringDrawing            := Compozer.CreateSpringDrawing(aScreenScale);
    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := True;
    SpringDrawing.Lx         := SpringSolver.LengthL0.Value([pMilli]);
    SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInSection(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := DrawingForm.SpringLength.Value;
    SpringDrawing.Caption    := Format('L = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInSection(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 0, 0, True);
    SpringDrawing.Destroy;
    {$ENDIF}
    Bit[0].Destroy;
    Bit[1].Destroy;
    Bit := nil;
  end else

  // Custom profle spring drawing
  if CustomProfileMenuItem.Checked then
  begin
    SetLength(Bit, 2);
    Bit[0] := TBGRABitmap.Create;
    Bit[1] := TBGRABitmap.Create;
    Bit[0].SetSize(aScreen.Width, aScreen.Height);
    Bit[1].SetSize(aScreen.Width, aScreen.Height);
    {$IFDEF MODULE1}
    SpringDrawing            := Compozer.CreateSpringDrawing(aScreenScale);
    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := True;
    SpringDrawing.Lx         := SpringSolver.LengthL0.Value([pMilli]);
    SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInProfile(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

    SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
    SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
    SpringDrawing.AutoFit    := False;
    SpringDrawing.Lx         := DrawingForm.SpringLength.Value;
    SpringDrawing.Caption    := Format('L = %0.2f', [SpringDrawing.Lx]);
    SpringDrawing.DrawInProfile(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

    Bit[1].Draw(aScreen.Canvas, Bit[1].Width * 0, 0, True);
    SpringDrawing.Destroy;
    {$ENDIF}
    Bit[0].Destroy;
    Bit[1].Destroy;
    Bit := nil;

  end else

  // Production spring drawing
  if ProductionDrawingMenuItem.Checked then
  begin
    SetLength(Bit, 1);
    Bit[0] := TBGRABitmap.Create;
    Bit[0].SetSize(aScreen.Width, aScreen.Height);
    Bit[0].AlphaFill(255);
    Bit[0].Fill(ScreenColor);
    begin
      SVG := TBGRASVG.Create;
      SVG.LoadFromResource('TEMPLATE');
      SVG.AsUTF8String := CreateProductionDrawing(SVG.AsUTF8String, aSetting);
      SVG.StretchDraw(Bit[0].Canvas2D, taLeftJustify, tlCenter, 0, 0, Bit[0].Width, Bit[0].Height, False);
      SVG.Destroy;
    end;
    Bit[0].InvalidateBitmap;
    Bit[0].Draw(aScreen.Canvas, 0, 0);
    Bit[0].Destroy;
    Bit := nil;
  end;
  Compozer.Destroy;

  VirtualScreenResize(nil);
  VirtualScreen.RedrawBitmap;
  {$ifopt D+}
  DEBUG('Draw -> ', MilliSecondsBetween(Now, Start).ToString);
  {$endif}
end;

// Create Diagrams

function TMainForm.CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSpringDrawing;
begin
  {$IFDEF MODULE1}
  Result            := TSpringDrawing.Create;
  Result.d          := SpringSolver.WireDiameter.Value([pMilli]);
  Result.Dm         := SpringSolver.Dm.Value([pMilli]);
  Result.Lc         := SpringSolver.LengthLc.Value([pMilli]);
  Result.Lx         := DrawingForm.SpringLength.Value;
  Result.Caption    := Format('L = %0.2f', [Result.Lx]);
  Result.n          := SpringSolver.ActiveColis;
  Result.nt1        := GeometryForm1.InactiveCoil1.Value;
  Result.nt2        := GeometryForm1.InactiveCoil2.Value;
  Result.ClockWise  := True;
  Result.AutoFit    := True;
  Result.GroundEnds := SpringSolver.GroundEnds;
  Result.Spacer     := Trunc(DefaultSpacer*aScreenScale);
  Result.Scale      := aScreenScale;
  {$ENDIF}
end;

function TMainForm.CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
begin
//Solve();
  {$IFDEF MODULE1}
  Result := Tx;
  Result := StringReplace(Result, '@0.00', Format('e1=%s ' + GetSymbol(SpringSolver.EccentricityE1),
    [GetString(GetValue(SpringSolver.EccentricityE1))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.01', Format('e2=%s ' + GetSymbol(SpringSolver.EccentricityE2),
    [GetString(GetValue(SpringSolver.EccentricityE2))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.02', Format('d=%s ' + GetSymbol(SpringSolver.WireDiameter),
    [GetString(GetValue(SpringSolver.WireDiameter))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.03', Format('Di=%s ' + GetSymbol(SpringSolver.Di),
    [GetString(GetValue(SpringSolver.Di))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.04', Format('Dm=%s ' + GetSymbol(SpringSolver.Dm),
    [GetString(GetValue(SpringSolver.Dm))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.05', Format('De=%s ' + GetSymbol(SpringSolver.De),
    [GetString(GetValue(SpringSolver.De))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.06', Format('L0=%s ' + GetSymbol(SpringSolver.LengthL0),
    [GetString(GetValue(SpringSolver.LengthL0))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.07', Format('L1=%s ' + GetSymbol(SpringSolver.LengthL1),
    [GetString(GetValue(SpringSolver.LengthL1))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.08', Format('L2=%s ' + GetSymbol(SpringSolver.LengthL2),
    [GetString(GetValue(SpringSolver.LengthL2))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.09', Format('Ln=%s ' + GetSymbol(SpringSolver.LengthLn),
    [GetString(GetValue(SpringSolver.LengthLn))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.10', Format('Lc=%s ' + GetSymbol(SpringSolver.LengthLc),
    [GetString(GetValue(SpringSolver.LengthLc))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.11', Format('F1=%s ' + GetSymbol(SpringSolver.LoadF1),
    [GetString(GetValue(SpringSolver.LoadF1))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.13', Format('F2=%s ' + GetSymbol(SpringSolver.LoadF2),
    [GetString(GetValue(SpringSolver.LoadF2))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.15', Format('Fn=%s ' + GetSymbol(SpringSolver.LoadFn),
    [GetString(GetValue(SpringSolver.LoadFn))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.17', Format('Fc=%s ' + GetSymbol(SpringSolver.LoadFc),
    [GetString(GetValue(SpringSolver.LoadFc))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.12', Format('Tauk1=%s ' + GetSymbol(SpringSolver.TorsionalStressTauk1),
    [GetString(GetValue(SpringSolver.TorsionalStressTauk1))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.14', Format('Tauk2=%s ' + GetSymbol(SpringSolver.TorsionalStressTauk1),
    [GetString(GetValue(SpringSolver.TorsionalStressTauk1))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.16', Format('Taukn=%s ' + GetSymbol(SpringSolver.TorsionalStressTaukn),
    [GetString(GetValue(SpringSolver.TorsionalStressTaukn))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.18', Format('Tauc=%s ' + GetSymbol(SpringSolver.TorsionalStressTauc),
    [GetString(GetValue(SpringSolver.TorsionalStressTauc ))]), [rfReplaceAll, rfIgnoreCase]);

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

  Result := StringReplace(Result, '@1.0', Format('n=%s',
    [GetString(SpringSolver.ActiveColis)]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@1.1', Format('nt=%s',
    [GetString(SpringSolver.TotalCoils )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@2.0', Format('R=%s ' + GetSymbol(SpringSolver.SpringRateR),
    [GetString(GetValue(SpringSolver.SpringRateR))]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.DirectionCoils.ItemIndex of
    0: ;
    1: Result := StringReplace(Result, '@3.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@3.1', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@3.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@3.1', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@4.0', Format('Dd=%s ' + GetSymbol(SpringSolver.DiMin),
    [GetString(GetValue(SpringSolver.DiMin))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@4.1', Format('Dh=%s ' + GetSymbol(SpringSolver.DeMax),
    [GetString(GetValue(SpringSolver.DeMax))]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.BurringEnds.ItemIndex of
    0: Result := StringReplace(Result, '@5.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    1: Result := StringReplace(Result, '@5.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@5.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@5.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.2', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@6.0', Format('fe=%s ' + GetSymbol(SpringSolver.NaturalFrequency),
    [GetString(GetValue(SpringSolver.NaturalFrequency))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@7.0', Format('%s C° / %s C°',
    [GetString(MAT.TempetatureMin),
     GetString(MAT.TempetatureMax)]), [rfReplaceAll, rfIgnoreCase]);

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
  Result := StringReplace(Result, '@10.0', MAT.Name, [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@10.1', Format('tauz=%s ' + GetSymbol(SpringSolver.AdmStaticTorsionalStressTauz),
    [GetString(GetValue(SpringSolver.AdmStaticTorsionalStressTauz))]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@10.2', Format('G=%s ' + GetSymbol(MAT.ShearModulusG20),
    [GetString(GetValue(MAT.ShearModulusG20))]), [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnCoilDiameter of
    QualityGrade1: Result := StringReplace(Result, '@11.00', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.01', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.02', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.00', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.01', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.02', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnFreeBodyLength of
    QualityGrade1: Result := StringReplace(Result, '@11.10', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.11', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.12', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.10', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.11', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.12', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnLoad1 of
    QualityGrade1: Result := StringReplace(Result, '@11.20', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.21', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.22', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.20', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.21', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.22', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnLoad2 of
    QualityGrade1: Result := StringReplace(Result, '@11.30', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.31', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.32', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.30', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.31', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.32', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnPerpendicularity of
    QualityGrade1: Result := StringReplace(Result, '@11.40', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.41', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.42', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@11.40', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.41', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@11.42', ' ', [rfReplaceAll, rfIgnoreCase]);

  case SpringTolerance.QualityGradeOnParallelism of
    QualityGrade1: Result := StringReplace(Result, '@11.50', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade2: Result := StringReplace(Result, '@11.51', 'X', [rfReplaceAll, rfIgnoreCase]);
    QualityGrade3: Result := StringReplace(Result, '@11.52', 'X', [rfReplaceAll, rfIgnoreCase]);
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
    Result := StringReplace(Result, '@13.0', Format('Ls=%s ' + ProductionForm.LengthLsUnit.Text,
      [GetString(ProductionForm.LengthLs.Value)]), [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, '@13.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.2', ' ', [rfReplaceAll, rfIgnoreCase]);
  end else
  begin
    Result := StringReplace(Result, '@13.0', 'Ls= ---', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.1', ' ', [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '@13.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;

  Result := StringReplace(Result, '@14.0', TextForm.Note1        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@14.1', TextForm.Note2        .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.0', TextForm.DrawingName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.1', TextForm.DrawingNumber.Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.2', TextForm.CompanyName  .Text, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@15.3', ApplicationVer,              [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '#ff0',    aSetting.ReadString('Printer', 'Page.Color4', '#ff0'   ), [rfReplaceAll, rfIgnoreCase]); // Yellow line
  Result := StringReplace(Result, '#f00',    aSetting.ReadString('Printer', 'Page.Color5', '#f00'   ), [rfReplaceAll, rfIgnoreCase]); // Red    line
  Result := StringReplace(Result, '#0f0',    aSetting.ReadString('Printer', 'Page.Color6', '#0f0'   ), [rfReplaceAll, rfIgnoreCase]); // Green  line
  Result := StringReplace(Result, '#ffff00', aSetting.ReadString('Printer', 'Page.Color1', '#ffff00'), [rfReplaceAll, rfIgnoreCase]); // Yellow
  Result := StringReplace(Result, '#ff0000', aSetting.ReadString('Printer', 'Page.Color2', '#ff0000'), [rfReplaceAll, rfIgnoreCase]); // Red
  Result := StringReplace(Result, '#00ff00', aSetting.ReadString('Printer', 'Page.Color3', '#00ff00'), [rfReplaceAll, rfIgnoreCase]); // Green
  {$ENDIF}
end;

function TMainForm.CreatePage(aWidth, aHeight: longint; aScale: double; aSetting: TIniFile): TBGRABitmap;
var
  PageColor: TBGRAPixel;
begin
  Result := TBGRABitmap.Create;
  Result.SetSize(
    Trunc(aWidth  *aScale),
    Trunc(aHeight *aScale));
  Result.Fill(PageColor);
  PageColor.FromString(aSetting.ReadString('Custom', 'BackgroundColor', 'White'));
  PaintTo(Result, aScale, aSetting);
end;

end.

