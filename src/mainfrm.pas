{ EN13906-1 Helical Compression Spring Designer

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

unit MainFrm;

{$mode ObjFPC}{$H+}

interface

uses
  BGRABitmap, BGRAShape, BGRASVG, BGRATextFX, BGRABitmapTypes, BGRAUnits,
  BGRAVirtualScreen, Classes, Controls, Dialogs, ExtCtrls, ExtDlgs, Forms,
  basegraphics, SpringMaterials, SpringSolvers, SpringTolerances, Graphics, IniFiles,
  LResources, Math, Menus, PrintersDlgs, Spin, StdCtrls, ActnList, SysUtils;

type

  { TPages }

  TScreenPage = (
    spDefault,
    spQuick1,
    spQuick2,
    spQuick3,
    spForceDisplacement,
    spGoodman,
    spBuckling,
    spShearTemperature,
    spYoungTemperature,
    spForce1Temperature,
    spForce2Temperature,
    spSectionDrawing,
    spProfileDrawing,
    spSectionDrawingAtLength,
    spProfileDrawingAtLength,
    spMessages,
    spProductionDrawing);

  { TMainForm }

  TMainForm = class(TForm)
    HelpAbout: TAction;
    HelpLicence: TAction;
    HelpHomePage: TAction;
    DocumentExportProductionDrawing: TAction;
    DocumentExportReport: TAction;
    DocumentProductionDrawing: TAction;
    DocumentReport: TAction;
    ViewMessages: TAction;
    ViewSectionDrawingAtLength: TAction;
    ViewProfileDrawingAtLength: TAction;
    ViewProfileDrawing: TAction;
    ViewSectionDrawing: TAction;
    ViewForce2Temperature: TAction;
    ViewForce1Temperature: TAction;
    ViewYoungTemperature: TAction;
    ViewShearTemperature: TAction;
    ViewBuckling: TAction;
    ViewGoodman: TAction;
    ViewForceAndDisplacements: TAction;
    ViewQuick3: TAction;
    ViewQuick2: TAction;
    ViewQuick1: TAction;
    EditUseImperialSystem: TAction;
    EditDesignMode: TAction;
    EditRecalculationMode: TAction;
    EditApplication: TAction;
    EditProduction: TAction;
    EditQualityGrade: TAction;
    EditMaterial: TAction;
    EditGeometry: TAction;
    EditDrawingText: TAction;
    EditWizard: TAction;
    FileExit: TAction;
    FilePrint: TAction;
    FilePageSetup: TAction;
    FileExport: TAction;
    FileClose: TAction;
    FileSaveAs: TAction;
    FileSave: TAction;
    FileOpen: TAction;
    FileNew: TAction;
    Actions: TActionList;

    CloseMenuItem: TMenuItem;
    ProfileAtLenghtMenuItem: TMenuItem;
    SectionAtLenghtMenuItem: TMenuItem;

    DrawingTextMenuItem: TMenuItem;
    DrawMenuItem: TMenuItem;

    ExportMenuItem: TMenuItem;
    ExportProductionMenuItem: TMenuItem;
    ExportReportMenuItem: TMenuItem;
    GeometryMenuItem: TMenuItem;

    MaterialMenuItem: TMenuItem;
    MessagesMenuItem: TMenuItem;
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
    LicenseMenuItem: TMenuItem;
    HomepageMenuItem: TMenuItem;
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




    procedure DocumentExportProductionDrawingExecute(Sender: TObject);
    procedure DocumentExportReportExecute(Sender: TObject);
    procedure DocumentProductionDrawingExecute(Sender: TObject);
    procedure DocumentReportExecute(Sender: TObject);
    procedure EditApplicationExecute(Sender: TObject);
    procedure EditDesignModeExecute(Sender: TObject);
    procedure EditDrawingTextExecute(Sender: TObject);
    procedure EditGeometryExecute(Sender: TObject);
    procedure EditMaterialExecute(Sender: TObject);
    procedure EditProductionExecute(Sender: TObject);
    procedure EditQualityGradeExecute(Sender: TObject);
    procedure EditRecalculationModeExecute(Sender: TObject);
    procedure EditUseImperialSystemExecute(Sender: TObject);




    procedure EditWizardExecute(Sender: TObject);


    procedure FileNewExecute(Sender: TObject);
    procedure FileOpenExecute(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAsExecute(Sender: TObject);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileExportExecute(Sender: TObject);
    procedure FilePageSetupExecute(Sender: TObject);
    procedure FilePrintExecute(Sender: TObject);
    procedure FileExitExecute(Sender: TObject);


    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);





    procedure HelpAboutExecute(Sender: TObject);

    procedure HelpHomePageExecute(Sender: TObject);
    procedure HelpLicenceExecute(Sender: TObject);


















    procedure ViewBucklingExecute(Sender: TObject);
    procedure ViewForce1TemperatureExecute(Sender: TObject);
    procedure ViewForce2TemperatureExecute(Sender: TObject);
    procedure ViewForceAndDisplacementsExecute(Sender: TObject);
    procedure ViewGoodmanExecute(Sender: TObject);
    procedure ViewMessagesExecute(Sender: TObject);
    procedure ViewProfileDrawingAtLengthExecute(Sender: TObject);
    procedure ViewProfileDrawingExecute(Sender: TObject);
    procedure ViewQuick1Execute(Sender: TObject);
    procedure ViewQuick2Execute(Sender: TObject);
    procedure ViewQuick3Execute(Sender: TObject);
    procedure ViewSectionDrawingAtLengthExecute(Sender: TObject);
    procedure ViewSectionDrawingExecute(Sender: TObject);
    procedure ViewShearTemperatureExecute(Sender: TObject);
    procedure ViewYoungTemperatureExecute(Sender: TObject);

    procedure VirtualScreenDblClick(Sender: TObject);
    procedure VirtualScreenMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);



    procedure VirtualScreenResize(Sender: TObject);

  private
    MoveX, MoveY: longint;
    MouseIsDown: boolean;
    Px, Py: longint;
    ScreenPage: TScreenPage;
    ScreenImage: TBGRABitmap;
    ScreenImageWidth: longint;
    ScreenImageHeight: longint;
    ScreenColor: TBGRAPixel;
    ScreenScale: double;
    SessionFileName: string;
    procedure SetScreenPage(AScreenPage: TScreenPage);
    procedure SetSessionFileName(const AFileName: string);
  public
    function CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSpringDrawing;
    function CreateProductionDrawing(const Tx: string; aSetting: TIniFile): string;
    function CreatePage(ASetting: TIniFile; const AScale: double): TBGRABitmap;

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

  {$IFDEF MODULE1} QualityFrm1, GeometryFrm1, ApplicationFrm1, {$ENDIF}
  {$IFDEF MODULE3} QualityFrm3, GeometryFrm3, ApplicationFrm3, {$ENDIF}

  LCLIntf, LCLType, LibLink, MaterialFrm, Printers, ProductionFrm,
  ReportFrm, Setting, baseutils;


{ Solve routine }

procedure TMainForm.Solve;
begin
  SpringTolerance.Clear;
  SpringSolver.Clear;
  {$IFDEF MODULE1}
  GeometryForm1.SaveToSolver;     // GeometryForm1
  MaterialForm.SaveToSolver;      // MaterialForm
  ProductionForm.SaveToSolver;    // ProductionForm
  QualityForm1.SaveToSolver;      // QualityForm
  ApplicationForm1.SaveToSolver;  // ApplicationForm
  // Solve
  if ScreenPage = spDefault then
    ScreenPage := spQuick1;
  MainForm.FormPaint(nil);
  {$ENDIF}
  {$IFDEF MODULE3}
  GeometryForm3.SaveToSolver;     // GeometryForm3
  MaterialForm.SaveToSolver;      // MaterialForm
  ProductionForm.SaveToSolver;    // ProductionForm
  QualityForm3.SaveToSolver;      // QualityForm
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

  MainForm.Top    := ClientFile.ReadInteger('MainForm', 'Top',    MainForm.Top);
  MainForm.Left   := ClientFile.ReadInteger('MainForm', 'Left',   MainForm.Left);
  MainForm.Height := ClientFile.ReadInteger('MainForm', 'Height', MainForm.Height);
  MainForm.Width  := ClientFile.ReadInteger('MainForm', 'Width',  MainForm.Width);

  Clear;
  Selection.Visible := False;
  ScreenPage        := spDefault;
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
  Logo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'background.png');
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
  if Windowstate = wsNormal then
  begin
    ClientFile.WriteInteger('MainForm', 'Top',    MainForm.Top);
    ClientFile.WriteInteger('MainForm', 'Left',   MainForm.Left);
    ClientFile.WriteInteger('MainForm', 'Height', MainForm.Height);
    ClientFile.WriteInteger('MainForm', 'Width',  MainForm.Width);
  end;
  ClientFile.WriteInteger('MainForm', 'Screen.Width',  ScreenImageWidth );
  ClientFile.WriteInteger('MainForm', 'Screen.Height', ScreenImageHeight);
end;

procedure TMainForm.SetScreenPage(AScreenPage: TScreenPage);
begin
  if AScreenPage <> ScreenPage then
    ScreenScale := 1.0;
  ScreenPage  := AScreenPage;
  // Update main menu
  Quick1MenuItem.Checked := ScreenPage = spQuick1;
  Quick2MenuItem.Checked := ScreenPage = spQuick2;
  Quick3MenuItem.Checked := ScreenPage = spQuick3;
  ForceMenuItem.Checked := ScreenPage = spForceDisplacement;
  GoodmanMenuItem.Checked := ScreenPage = spGoodman;
  BucklingMenuItem.Checked := ScreenPage = spBuckling;
  ShearModulusMenuItem.Checked := ScreenPage = spShearTemperature;
  YoungModulusMenuItem.Checked := ScreenPage = spYoungTemperature;
  F1MenuItem.Checked := ScreenPage = spForce1Temperature;
  F2MenuItem.Checked := ScreenPage = spForce2Temperature;
  SectionMenuItem.Checked := ScreenPage = spSectionDrawing;
  ProfileMenuItem.Checked := ScreenPage = spProfileDrawing;
  SectionAtLenghtMenuItem.Checked := ScreenPage = spSectionDrawingAtLength;
  ProfileAtLenghtMenuItem.Checked := ScreenPage = spProfileDrawingAtLength;
  MessagesMenuItem.Checked := ScreenPage = spMessages;
  ProductionDrawingMenuItem.Checked := ScreenPage = spProductionDrawing;
  UseImperialSystemMenuItem.Checked := UseImperialSystem;
end;

procedure TMainForm.Clear;
begin
  ClearAll;
  MoveX := 0;
  MoveY := 0;
  MouseIsDown := False;
  ScreenScale := 1.0;
  SetScreenPage(spDefault);
  SetSessionFileName('');
end;

procedure TMainForm.ClearAll;
begin
  if Assigned(TextForm        ) then TextForm         .Clear;
  {$IFDEF MODULE1}
  if Assigned(QualityForm1    ) then QualityForm1     .Clear;
  if Assigned(GeometryForm1   ) then GeometryForm1    .Clear;
  if Assigned(ApplicationForm1) then ApplicationForm1 .ClearForm;
  {$ENDIF}
  {$IFDEF MODULE3}
  if Assigned(QualityForm3    ) then QualityForm3     .Clear;
  if Assigned(GeometryForm3   ) then GeometryForm3    .Clear;
  if Assigned(ApplicationForm3) then ApplicationForm3 .Clear;
  {$ENDIF}
  if Assigned(MaterialForm    ) then MaterialForm     .Clear;
  if Assigned(ProductionForm  ) then ProductionForm   .Clear;
end;

procedure TMainForm.LoadAll(SessionIniFile: TIniFile);
begin
  TextForm         .Load(SessionIniFile);
  {$IFDEF MODULE1}
  QualityForm1     .Load(SessionIniFile);
  GeometryForm1    .Load(SessionIniFile);
  ApplicationForm1 .Load(SessionIniFile);
  {$ENDIF}
  {$IFDEF MODULE3}
  QualityForm3     .Load(SessionIniFile);
  GeometryForm3    .Load(SessionIniFile);
  ApplicationForm3 .Load(SessionIniFile);
  {$ENDIF}
  MaterialForm     .Load(SessionIniFile);

  ProductionForm   .Load(SessionIniFile);
end;

procedure TMainForm.SaveAll(SessionIniFile: TIniFile);
begin
  TextForm         .Save(SessionIniFile);
  {$IFDEF MODULE1}
  QualityForm1     .Save(SessionIniFile);
  GeometryForm1    .Save(SessionIniFile);
  ApplicationForm1 .Save(SessionIniFile);
  {$ENDIF}
  {$IFDEF MODULE3}
  QualityForm3     .Save(SessionIniFile);
  GeometryForm3    .Save(SessionIniFile);
  ApplicationForm3 .Save(SessionIniFile);
  {$ENDIF}
  MaterialForm     .Save(SessionIniFile);
  ProductionForm   .Save(SessionIniFile);
end;

procedure TMainForm.SetSessionFileName(const AFileName: string);
begin
  SessionFileName := AFileName;
  if Length(SessionFileName) > 0 then
    Caption := ExtractFileName(SessionFileName) + ' - ' + ApplicationVer
  else
    Caption := ApplicationVer;
end;

(*
procedure TMainForm.Load(IniFile: TIniFile);
begin

end;

procedure TMainForm.Save(IniFile: TIniFile);
begin

end;
*)

// Virtual Screen Events

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

procedure TMainForm.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.PutImage(MoveX, MoveY, ScreenImage, dmSet);
end;

procedure TMainForm.VirtualScreenResize(Sender: TObject);
begin
  MoveX := Math.Max(Math.Min(0, MoveX), VirtualScreen.Width  - ScreenImage.Width );
  MoveY := Math.Max(Math.Min(0, MoveY), VirtualScreen.Height - ScreenImage.Height);
end;

// Actions routines

procedure TMainForm.FileNewExecute(Sender: TObject);
begin
  Clear;
  begin
    EditWizardExecute(Sender);
  end;
end;

procedure TMainForm.FileOpenExecute(Sender: TObject);
var
  SessionIniFile: TIniFile;
begin
  {$IFDEF MODULE1} OpenDialog.Filter := 'SpringOne file (*.spring1)|*.spring1|;';   {$ENDIF}
  {$IFDEF MODULE2} OpenDialog.Filter := 'SpringTwo file (*.spring2)|*.spring2|;';   {$ENDIF}
  {$IFDEF MODULE3} OpenDialog.Filter := 'SpringThree file (*.spring3)|*.spring3|;'; {$ENDIF}
  if OpenDialog.Execute then
  begin
    Clear;
    SetSessionFileName(OpenDialog.FileName);
    SessionIniFile := TIniFile.Create(SessionFileName,
      [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

    LoadAll(SessionIniFile);
    SessionIniFile.Destroy;
    begin
      ViewQuick1Execute(Sender);
    end;
  end;
  Solve();
end;

procedure TMainForm.FileSaveExecute(Sender: TObject);
var
  SessionIniFile: TIniFile;
begin
  if SessionFileName = '' then
  begin
    FileSaveAsExecute(Sender);
  end else
  begin
    SessionIniFile  := TIniFile.Create(SessionFileName,
      [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

    SaveAll(SessionIniFile);
    SessionIniFile.Destroy;
  end;
end;

procedure TMainForm.FileSaveAsExecute(Sender: TObject);
begin
  {$IFDEF MODULE1} SaveDialog.Filter := 'SpringOne file (*.spring1)|*.spring1|All files (*.*)|*.*|;'; {$ENDIF}
  {$IFDEF MODULE3} SaveDialog.Filter := 'SpringThree file (*.spring3)|*.spring3|All files (*.*)|*.*|;'; {$ENDIF}
  SaveDialog.InitialDir := ExtractFileDir(SessionFileName);
  SaveDialog.FileName := ExtractFileName(SessionFileName);
  if SaveDialog.Execute then
  begin
    SetSessionFileName(SaveDialog.FileName);
    FileSaveExecute(Sender);
  end;
end;

procedure TMainForm.FileCloseExecute(Sender: TObject);
begin
  Clear;
  Solve();
end;

procedure TMainForm.FileExportExecute(Sender: TObject);
var
  Page: TBGRABitmap;
begin
  SavePictureDialog.Filter :=
    'Graphics (*.png;*.xpm;*.bmp;*.jpeg;*.jpg;)|*.png;*.xpm;*.bmp;*.jpeg;*.jpg|' +
    'PNG Files (*.png)|*.png|' + 'Pixmap Files (*.xpm)|*.xpm|' + 'Bitmap Files (*.bmp)|*.bmp)|' +
    'JPEG Files (*.jpeg;*.jpg;)|*.jpeg;*.jpg|' + 'Tutti i file (*.*)|*.*|;';
  SavePictureDialog.InitialDir := ExtractFileDir(SessionFileName);
  SavePictureDialog.FileName := ExtractFileName(ChangefileExt(SessionFileName, '.png'));
  if SavePictureDialog.Execute then
  begin
    Solve();
    Page := CreatePage(PrinterFile, ScreenScale);
    Page.SaveToFile(SavePictureDialog.FileName);
    Page.Destroy;
  end;
end;

procedure TMainForm.FilePageSetupExecute(Sender: TObject);
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

procedure TMainForm.FilePrintExecute(Sender: TObject);
var
  OffSetX: LongInt;
  OffSetY: LongInt;
  Page: TBGRABitmap;
  Scale: double;
begin
  if PrintDialog.Execute then
  begin
    Solve();
    Printer.BeginDoc;
    if ProductionDrawingMenuItem.Checked then
      Scale := Math.Min((Printer.PageHeight) / ScreenImageHeight,
                        (Printer.PageWidth ) / ScreenImageHeight / 0.7071)
    else
      Scale := Math.Min((Printer.PageHeight) / ScreenImageHeight,
                        (Printer.PageWidth ) / ScreenImageWidth);

    Page := CreatePage(PrinterFile, Scale);
    OffSetX := (Printer.PageWidth  - Page.Width ) div 2;
    OffSetY := (Printer.PageHeight - Page.Height) div 2;
    Printer.Canvas.Draw(OffSetX, OffSetY, Page.Bitmap);
    Printer.EndDoc;
    Page.Destroy;
  end;
end;

procedure TMainForm.FileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditWizardExecute(Sender: TObject);
begin
  if (TextForm         .ShowModal = mrOk) and
     {$IFDEF MODULE1}
     (GeometryForm1    .ShowModal = mrOk) and
     (QualityForm1     .ShowModal = mrOk) and
     {$ENDIF}
     {$IFDEF MODULE3}
     (GeometryForm3    .ShowModal = mrOk) and
     (QualityForm3     .ShowModal = mrOk) and
     {$ENDIF}
     (MaterialForm     .ShowModal = mrOk) and
     (ProductionForm   .ShowModal = mrOk) and
     {$IFDEF MODULE1}
     (ApplicationForm1 .ShowModal = mrOk) then
     {$ENDIF}
     {$IFDEF MODULE3}
     (ApplicationForm3 .ShowModal = mrOk) then
     {$ENDIF}
  begin
    SetScreenPage(spQuick1);
  end;
  Solve();
end;

procedure TMainForm.EditDrawingTextExecute(Sender: TObject);
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

procedure TMainForm.EditGeometryExecute(Sender: TObject);
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

procedure TMainForm.EditMaterialExecute(Sender: TObject);
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

procedure TMainForm.EditQualityGradeExecute(Sender: TObject);
var
  SessionIniFile: TIniFile;
  SessionStream: TMemoryStream;
begin
  SessionStream  := TMemoryStream.Create;
  SessionIniFile := TIniFile.Create(SessionStream,
    [ifoStripInvalid, ifoFormatSettingsActive, ifoWriteStringBoolean]);

  {$IFDEF MODULE1}
  QualityForm1.Save(SessionIniFile);
  if QualityForm1.ShowModal <> mrOk then
  begin
    QualityForm1.Load(SessionIniFile);
  end;
   {$ENDIF}
  {$IFDEF MODULE3}
  QualityForm3.Save(SessionIniFile);
  if QualityForm3.ShowModal <> mrOk then
  begin
    QualityForm3.Load(SessionIniFile);
  end;
  {$ENDIF}
  SessionIniFile.Destroy;
  SessionStream.Destroy;
  Solve();
end;

procedure TMainForm.EditProductionExecute(Sender: TObject);
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

procedure TMainForm.EditApplicationExecute(Sender: TObject);
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

procedure TMainForm.EditRecalculationModeExecute(Sender: TObject);
begin

end;

procedure TMainForm.EditDesignModeExecute(Sender: TObject);
begin

end;

procedure TMainForm.EditUseImperialSystemExecute(Sender: TObject);
begin
  UseImperialSystem := not UseImperialSystem;
  SetScreenPage(ScreenPage);
  Solve();
end;

procedure TMainForm.ViewQuick1Execute(Sender: TObject);
begin
  SetScreenPage(spQuick1);
  FormPaint(Sender);
end;

procedure TMainForm.ViewQuick2Execute(Sender: TObject);
begin
  SetScreenPage(spQuick2);
  FormPaint(Sender);
end;

procedure TMainForm.ViewQuick3Execute(Sender: TObject);
begin
  SetScreenPage(spQuick3);
  FormPaint(Sender);
end;

procedure TMainForm.ViewForceAndDisplacementsExecute(Sender: TObject);
begin
  SetScreenPage(spForceDisplacement);
  FormPaint(Sender);
end;

procedure TMainForm.ViewGoodmanExecute(Sender: TObject);
begin
  SetScreenPage(spGoodman);
  FormPaint(Sender);
end;

procedure TMainForm.ViewBucklingExecute(Sender: TObject);
begin
  SetScreenPage(spBuckling);
  FormPaint(Sender);
end;

procedure TMainForm.ViewShearTemperatureExecute(Sender: TObject);
begin
  SetScreenPage(spShearTemperature);
  FormPaint(Sender);
end;

procedure TMainForm.ViewYoungTemperatureExecute(Sender: TObject);
begin
  SetScreenPage(spYoungTemperature);
  FormPaint(Sender);
end;

procedure TMainForm.ViewForce1TemperatureExecute(Sender: TObject);
begin
  SetScreenPage(spForce1Temperature);
  FormPaint(Sender);
end;

procedure TMainForm.ViewForce2TemperatureExecute(Sender: TObject);
begin
  SetScreenPage(spForce2Temperature);
  FormPaint(Sender);
end;

procedure TMainForm.ViewSectionDrawingExecute(Sender: TObject);
begin
  SetScreenPage(spSectionDrawing);
  FormPaint(Sender);
end;

procedure TMainForm.ViewProfileDrawingExecute(Sender: TObject);
begin
  SetScreenPage(spProfileDrawing);
  FormPaint(Sender);
end;

procedure TMainForm.ViewSectionDrawingAtLengthExecute(Sender: TObject);
var
  Value: double;
begin
  {$IFDEF MODULE1}
  DrawingForm.SpringLength.MinValue := GetLengthValue(SpringSolver.LengthLc);
  DrawingForm.SpringLength.MaxValue := GetLengthValue(SpringSolver.LengthL0);

  Value := DrawingForm.SpringLength.Value;
  if DrawingForm.ShowModal = mrOk then
  begin
    SetScreenPage(spSectionDrawingAtLength);
    FormPaint(Sender);
  end else
    DrawingForm.SpringLength.Value := Value;
  {$ENDIF}
end;

procedure TMainForm.ViewProfileDrawingAtLengthExecute(Sender: TObject);
var
  Value: double;
begin
  {$IFDEF MODULE1}
  DrawingForm.SpringLength.MinValue := MeterUnit.ToFloat(SpringSolver.LengthLc, [pMilli]);
  DrawingForm.SpringLength.MaxValue := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);

  Value := DrawingForm.SpringLength.Value;
  if DrawingForm.ShowModal = mrOk then
  begin
    SetScreenPage(spProfileDrawingAtLength);
    FormPaint(Sender);
  end else
    DrawingForm.SpringLength.Value := Value;
  {$ENDIF}
end;

procedure TMainForm.ViewMessagesExecute(Sender: TObject);
begin
  SetScreenPage(spMessages);
  FormPaint(Sender);
end;

procedure TMainForm.DocumentReportExecute(Sender: TObject);
begin
  Solve();
  ReportForm.CreateReport;
  ReportForm.ShowModal;
end;

procedure TMainForm.DocumentProductionDrawingExecute(Sender: TObject);
begin
  SetScreenPage(spProductionDrawing);
  FormPaint(Sender);
end;

procedure TMainForm.DocumentExportReportExecute(Sender: TObject);
begin
  Solve();
  SaveDialog.Filter     := 'Text file (*.txt)|*.txt|All files (*.*)|*.*|;';
  SaveDialog.InitialDir := ExtractFileDir(SessionFileName);
  SaveDialog.FileName   := ExtractFileName(ChangeFileExt(SessionFileName, '.txt'));
  if SaveDialog.Execute then
  begin
    ReportForm.CreateReport;
    ReportForm.Memo.Lines.SaveToFile(SaveDialog.FileName);
  end;
  SaveDialog.FileName := SessionFileName;
end;

procedure TMainForm.DocumentExportProductionDrawingExecute(Sender: TObject);
var
  SVG: TBGRASvg;
begin
  Solve();
  SaveDialog.Filter     := 'Svg file (*.svg)|*.svg|All files (*.*)|*.*|;';
  SaveDialog.InitialDir := ExtractFileDir(SessionFileName);
  SaveDialog.FileName   := ExtractFileName(ChangeFileExt(SessionFileName, '.svg'));
  if SaveDialog.Execute then
  begin
    SVG := TBGRASVG.Create;
    SVG.LoadFromResource('TEMPLATEBLACK');
    SVG.AsUTF8String := CreateProductionDrawing(SVG.AsUTF8String, ClientFile);
    SVG.SaveToFile(SaveDialog.FileName);
    SVG.Destroy;
  end;
  SaveDialog.FileName := SessionFileName;
end;

procedure TMainForm.HelpHomePageExecute(Sender: TObject);
begin
  OpenURL('https://github.com/melchiorrecaruso/springone');
end;

procedure TMainForm.HelpLicenceExecute(Sender: TObject);
begin
  OpenURL('https://github.com/melchiorrecaruso/springone/blob/main/LICENSE');
end;

procedure TMainForm.HelpAboutExecute(Sender: TObject);
begin
  AboutForm.AboutVersionLabel.Caption := ApplicationVer;
  AboutForm.AboutBuildLabel.Caption := ApplicationBuild;
  AboutForm.ShowModal;
end;

// FormPaint

procedure TMainForm.FormPaint(Sender: TObject);
begin
  ScreenImageWidth  := Math.Max(ScreenImageWidth,  VirtualScreen.Width );
  ScreenImageHeight := Math.Max(ScreenImageHeight, VirtualScreen.Height);

  if ScreenPage <> spDefault then
  begin
    ScreenImage.SetSize(
      Trunc(ScreenImageWidth*ScreenScale),
      Trunc(ScreenImageHeight*ScreenScale));
    ScreenImage.FillTransparent;
    PaintTo(ScreenImage, ScreenScale, ClientFile);
  end else
  begin
    MoveX := (VirtualScreen.Width  - ScreenImageWidth ) div 2;
    MoveY := (VirtualScreen.Height - ScreenImageHeight) div 2;
    ScreenImage.LoadFromResource('background');
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
begin
  Compozer := TCompozer.Create(aSetting);

  ErrorMessage.Clear;
  WarningMessage.Clear;
  SpringSolver.Solve(SpringTolerance);
  if SpringSolver.Check then
  begin
    case ScreenPage of
      spQuick1: Compozer.DrawQuick1(aScreen, aScreenScale);
      spQuick2: Compozer.DrawQuick2(aScreen, aScreenScale);
      spQuick3: Compozer.DrawQuick3(aScreen, aScreenScale);
      spForceDisplacement:
      begin
        Chart := Compozer.CreateForceDisplacementChart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spGoodman:
      begin
        Chart := Compozer.CreateGoodmanChart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spBuckling:
      begin
        Chart := Compozer.CreateBucklingChart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spShearTemperature:
      begin
        Chart := Compozer.CreateShearModulusChart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spYoungTemperature:
      begin
        Chart := Compozer.CreateYoungModulusChart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spForce1Temperature:
      begin
        Chart := Compozer.CreateLoadF1Chart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spForce2Temperature:
      begin
        Chart := Compozer.CreateLoadF2Chart(aScreenScale);
        Chart.Draw(aScreen.Canvas, aScreen.Width, aScreen.Height);
        Chart.Destroy;
      end;
      spMessages: Compozer.DrawMessageList(aScreen, aScreenScale);
      spSectionDrawing:
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
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
        SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
        SpringDrawing.DrawInSection(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

        SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
        SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
        SpringDrawing.AutoFit    := False;
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL1, [pMilli]);
        SpringDrawing.Caption    := Format('L1 = %0.2f', [SpringDrawing.Lx]);
        SpringDrawing.DrawInSection(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

        SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
        SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
        SpringDrawing.AutoFit    := False;
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL2, [pMilli]);
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
      end;
      spProfileDrawing:
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
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
        SpringDrawing.Caption    := Format('L0 = %0.2f', [SpringDrawing.Lx]);
        SpringDrawing.DrawInProfile(Bit[0].Canvas, Bit[0].Width, Bit[0].Height);

        SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
        SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
        SpringDrawing.AutoFit    := False;
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL1, [pMilli]);
        SpringDrawing.Caption    := Format('L1 = %0.2f', [SpringDrawing.Lx]);
        SpringDrawing.DrawInProfile(Bit[1].Canvas, Bit[1].Width, Bit[1].Height);

        SpringDrawing.ClockWise  := ProductionForm.DirectionCoils.ItemIndex = 2;
        SpringDrawing.GroundEnds := GeometryForm1.EndCoilType.ItemIndex = 1;
        SpringDrawing.AutoFit    := False;
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL2, [pMilli]);
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
      end;
      spSectionDrawingAtLength:
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
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
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
      end;
      spProfileDrawingAtLength:
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
        SpringDrawing.Lx         := MeterUnit.ToFloat(SpringSolver.LengthL0, [pMilli]);
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
      end;
      spProductionDrawing:
      begin
        SetLength(Bit, 1);
        Bit[0] := TBGRABitmap.Create;
        Bit[0].SetSize(aScreen.Width, aScreen.Height);
        Bit[0].AlphaFill(255);

        SVG := TBGRASVG.Create;
        if ASetting = ClientFile then
        begin
          Bit[0].Fill(ScreenColor);
          SVG.LoadFromResource('TEMPLATE');
        end else
        begin
          Bit[0].Fill(clWhite);
          SVG.LoadFromResource('TEMPLATEBLACK');
        end;
        SVG.AsUTF8String := CreateProductionDrawing(SVG.AsUTF8String, aSetting);
        SVG.StretchDraw(Bit[0].Canvas2D, taLeftJustify, tlCenter, 0, 0, Bit[0].Width, Bit[0].Height, False);
        SVG.Destroy;

        Bit[0].InvalidateBitmap;
        Bit[0].Draw(aScreen.Canvas, 0, 0);
        Bit[0].Destroy;
        Bit := nil;
      end;
    end;
  end;

  Compozer.Destroy;

  VirtualScreenResize(nil);
  VirtualScreen.RedrawBitmap;
end;

// Create Diagrams

function TMainForm.CreateSpringDrawing(const aScreenScale: double; aSetting: TIniFile): TSpringDrawing;
begin
  {$IFDEF MODULE1}
  Result            := TSpringDrawing.Create;
  Result.d          := MeterUnit.ToFloat(SpringSolver.WireDiameter, [pMilli]);
  Result.Dm         := MeterUnit.ToFloat(SpringSolver.Dm, [pMilli]);
  Result.Lc         := MeterUnit.ToFloat(SpringSolver.LengthLc, [pMilli]);
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
var
  Ls: TQuantity;
begin
  {$IFDEF MODULE1}
  Result := Tx;
  Result := StringReplace(Result, '@0.00', Format('e1=%s', [GetLengthString(SpringSolver.EccentricityE1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.01', Format('e2=%s', [GetLengthString(SpringSolver.EccentricityE2)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.02', Format('d=%s',  [GetLengthString(SpringSolver.WireDiameter  )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.03', Format('Di=%s', [GetLengthString(SpringSolver.Di            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.04', Format('Dm=%s', [GetLengthString(SpringSolver.Dm            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.05', Format('De=%s', [GetLengthString(SpringSolver.De            )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.06', Format('L0=%s', [GetLengthString(SpringSolver.LengthL0      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.07', Format('L1=%s', [GetLengthString(SpringSolver.LengthL1      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.08', Format('L2=%s', [GetLengthString(SpringSolver.LengthL2      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.09', Format('Ln=%s', [GetLengthString(SpringSolver.LengthLn      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.10', Format('Lc=%s', [GetLengthString(SpringSolver.LengthLc      )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.11', Format('F1=%s', [GetForceString(SpringSolver.LoadF1         )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.13', Format('F2=%s', [GetForceString(SpringSolver.LoadF2         )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.15', Format('Fn=%s', [GetForceString(SpringSolver.LoadFn         )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.17', Format('Fc=%s', [GetForceString(SpringSolver.LoadFc         )]), [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@0.12', Format('Tauk1=%s', [GetPressureString(SpringSolver.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.14', Format('Tauk2=%s', [GetPressureString(SpringSolver.TorsionalStressTauk1)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.16', Format('Taukn=%s', [GetPressureString(SpringSolver.TorsionalStressTaukn)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@0.18', Format('Tauc=%s',  [GetPressureString(SpringSolver.TorsionalStressTauc )]), [rfReplaceAll, rfIgnoreCase]);

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

  Result := StringReplace(Result, '@1.0', Format('n=%s',  [GetString(SpringSolver.ActiveColis         )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@1.1', Format('nt=%s', [GetString(SpringSolver.TotalCoils          )]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@2.0', Format('R=%s',  [GetStiffnessString(SpringSolver.SpringRateR)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.DirectionCoils.ItemIndex of
    0: ;
    1: Result := StringReplace(Result, '@3.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@3.1', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@3.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@3.1', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@4.0', Format('Dd=%s', [GetLengthString(SpringSolver.DiMin)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@4.1', Format('Dh=%s', [GetLengthString(SpringSolver.DeMax)]), [rfReplaceAll, rfIgnoreCase]);

  case ProductionForm.BurringEnds.ItemIndex of
    0: Result := StringReplace(Result, '@5.0', 'X', [rfReplaceAll, rfIgnoreCase]);
    1: Result := StringReplace(Result, '@5.1', 'X', [rfReplaceAll, rfIgnoreCase]);
    2: Result := StringReplace(Result, '@5.2', 'X', [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, '@5.0', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.1', ' ', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@5.2', ' ', [rfReplaceAll, rfIgnoreCase]);

  Result := StringReplace(Result, '@6.0', Format('fe=%s', [GetFrequencyString(SpringSolver.NaturalFrequency)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@7.0', Format('%s / %s', [GetTemperatureString(MAT.TemperatureMin), GetTemperatureString(MAT.TemperatureMax)]), [rfReplaceAll, rfIgnoreCase]);

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

  Result := StringReplace(Result, '@10.1', Format('tauz=%s', [GetPressureString(SpringSolver.AdmStaticTorsionalStressTauz)]), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '@10.2', Format('G=%s', [GetPressureString(MAT.ShearModulusG20)]), [rfReplaceAll, rfIgnoreCase]);

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
    case ProductionForm.LengthLsUnit.ItemIndex of
      0: Ls := ProductionForm.LengthLs.Value*mm;
      1: Ls := ProductionForm.LengthLs.Value*inch;
    else Ls := 0*m;
    end;

    Result := StringReplace(Result, '@13.0', Format('Ls=%s', [GetLengthString(Ls)]), [rfReplaceAll, rfIgnoreCase]);
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

function TMainForm.CreatePage(ASetting: TIniFile; const AScale: double): TBGRABitmap;
var
  PageColor: TBGRAPixel;
begin
  PageColor.FromString(ASetting.ReadString('Custom', 'BackgroundColor', 'White'));

  Result := TBGRABitmap.Create;
  if ProductionDrawingMenuItem.Checked then
  begin
    Result.SetSize(
      Trunc(ScreenImageHeight * AScale * 0.7071),
      Trunc(ScreenImageHeight * AScale));
  end else
  begin
    Result.SetSize(
      Trunc(ScreenImageWidth  * AScale),
      Trunc(ScreenImageHeight * AScale));
  end;
  Result.Fill(PageColor);
  PaintTo(Result, AScale, ASetting);
end;

end.

