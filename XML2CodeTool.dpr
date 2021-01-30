program XML2CodeTool;

uses
  System.StartUpCopy,
  FMX.Forms,
  XML2CodeToolForm in 'XML2CodeToolForm.pas' {FormXML2CodeTool},
  XMLCore in 'Src\Core\XMLCore.pas',
  XMLLeafTypes in 'Src\Core\XMLLeafTypes.pas',
  ClientScreen in 'Src\Core\ClientScreen.pas',
  XMLInspector in 'Src\Core\XMLInspector.pas',
  LinesEditorForm in 'Src\Forms\LinesEditorForm.pas' {FormLinesEditor},
  LinesEditorFrame in 'Src\Frames\LinesEditorFrame.pas' {FrameLinesEditor: TFrame},
  TextEditorFrame in 'Src\Frames\TextEditorFrame.pas' {FrameTextEditor: TFrame},
  TextEditorForm in 'Src\Forms\TextEditorForm.pas' {FormTextEditor},
  XMLEvent in 'Src\Core\XMLEvent.pas',
  XML2Code in 'Src\XML2Code\XML2Code.pas',
  XML2CodeBase in 'Src\XML2Code\XML2CodeBase.pas',
  XML2CodeFrame in 'Src\XML2Code\XML2CodeFrame.pas' {FrameXML2Code: TFrame},
  XMLTree in 'Src\Core\XMLTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormXML2CodeTool, FormXML2CodeTool);
  Application.CreateForm(TFormLinesEditor, FormLinesEditor);
  Application.CreateForm(TFormTextEditor, FormTextEditor);
  Application.Run;

end.
