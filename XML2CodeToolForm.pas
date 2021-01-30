unit XML2CodeToolForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.DialogService,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Menus, FMXTee.Tree, FMXTee.Procs, System.Rtti, FMX.Grid.Style,
  FMX.ScrollBox, FMX.Grid, FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.Colors,
  FMX.SpinBox, FMX.EditBox, FMX.NumberBox, FMX.DateTimeCtrls,
  XMLCore, XML2CodeBase, XML2Code, XMLInspector, XML2CodeFrame;

type
  TFormXML2CodeTool = class(TForm)
    XML2CodeMenus: TMainMenu;
    CodeMenu: TMenuItem;
    XML2CodeMenuBar: TMenuBar;
    ExportBasePasMenu: TMenuItem;
    ExportPasMenu: TMenuItem;
    ExportBasePasTool: TMenuItem;
    ExportPasTool: TMenuItem;
    VisualCodeMenu: TMenuItem;
    Splitter1: TSplitter;
    ExportFrameTool: TMenuItem;
    ExportFrameMenu: TMenuItem;
    ExitTool: TMenuItem;
    X2C: TFrameXML2Code;
    procedure FormDestroy(Sender: TObject);
    procedure ExitToolClick(Sender: TObject);
    procedure ExportBasePas(Sender: TObject);
    procedure ExportPas(Sender: TObject);
    procedure ExportFrame(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure VisualCodeMenuClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormXML2CodeTool: TFormXML2CodeTool;

implementation

{$R *.fmx}


procedure TFormXML2CodeTool.FormDestroy(Sender: TObject);
begin
  X2C.FrameFinal;
end;

procedure TFormXML2CodeTool.ExitToolClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFormXML2CodeTool.ExportBasePas(Sender: TObject);
var
  path: string;
begin
  path := InputBox('Input Path', 'Path', 'D:\D103\XML2CodeTool\Src\Codes');
  X2C.XML2CodeObj.ExportBaseCode(path);
end;

procedure TFormXML2CodeTool.ExportPas(Sender: TObject);
var
  path: string;
begin
  path := InputBox('Input Path', 'Path', 'D:\D103\XML2CodeTool\Src\Codes');
  X2C.XML2CodeObj.ExportCode(path);
end;

procedure TFormXML2CodeTool.ExportFrame(Sender: TObject);
var
  path, rName, rType: string;
begin
  path := InputBox('Input Path', 'Path', 'D:\D103\XML2CodeTool\Src\Codes');
  rName := InputBox('Input Root Name', 'Name', X2C.XML2CodeObj.Name);
  rType := InputBox('Input Root Type', 'Type',
    X2C.XML2CodeObj.XMLClass[0].DataType);
  X2C.XML2CodeObj.ExportFrame(path, rName, rType);
end;

procedure TFormXML2CodeTool.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  X2C.FrameInit;
end;

procedure TFormXML2CodeTool.VisualCodeMenuClick(Sender: TObject);
begin
  VisualCode := VisualCodeMenu.IsChecked;
  if VisualCodeMenu.IsChecked then
    VisualCodeMenu.Text := 'Visual Code'
  else
    VisualCodeMenu.Text := 'No Visual Code';
end;

end.
