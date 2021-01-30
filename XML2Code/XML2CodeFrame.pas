unit XML2CodeFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,
  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,
  XML2Code, XML2CodeBase, FMX.Layouts;

type
  TFrameXML2Code = class(TFrame)
    XML2CodeTree: TXMLTree;
    XML2CodeIns: TXMLInspector;
    SplitterBottom: TSplitter;
    XML2CodeMenuBar: TMenuBar;
    NewXML2CodeButton: TMenuItem;
    OpenXML2CodeButton: TMenuItem;
    SaveXML2CodeButton: TMenuItem;
    OpenDialogXML2Code: TOpenDialog;
    SaveDialogXML2Code: TSaveDialog;
    procedure NewXML2Code(Sender: TObject);
    procedure OpenXML2Code(Sender: TObject);
    procedure SaveXML2Code(Sender: TObject);
  private
    FXML2CodeObj: TXML2Code;

    FTXML2CodePop: TPopupMenu;
    FTXML2CodeClassPop: TPopupMenu;
    FTXML2CodeChildPop: TPopupMenu;

    XML2CodeTreeRoot: TTreeNodeShape;

  public
    function GetXML2CodeObj: TXML2Code;
    procedure SetXML2CodeObj(const _Value: TXML2Code);
    procedure FrameInit;
    procedure FrameFinal;
    property XML2CodeObj: TXML2Code read GetXML2CodeObj write SetXML2CodeObj;

  end;

implementation

{$R *.fmx}


procedure TFrameXML2Code.FrameFinal;
begin
  FXML2CodeObj.Free;
end;

procedure TFrameXML2Code.FrameInit;
begin
  FTXML2CodePop := TPopupMenu.Create(Self);
  FTXML2CodePop.Parent := Self;
  TXML2CodePop := FTXML2CodePop;

  FTXML2CodeClassPop := TPopupMenu.Create(Self);
  FTXML2CodeClassPop.Parent := Self;
  TXML2CodeClassPop := FTXML2CodeClassPop;

  FTXML2CodeChildPop := TPopupMenu.Create(Self);
  FTXML2CodeChildPop.Parent := Self;
  TXML2CodeChildPop := FTXML2CodeChildPop;

  if Not Assigned(FXML2CodeObj) then
    FXML2CodeObj := TXML2Code.Create(nil);
  if Not Assigned(XML2CodeTreeRoot) then
    XML2CodeTreeRoot := XML2CodeTree.AddRoot('XML2Code');
  FXML2CodeObj.TreeNodeShape := XML2CodeTreeRoot;
  FXML2CodeObj.ToTree;

  TXML2CodeXMLInspector := XML2CodeIns;
  TXML2CodeTreeComponent := XML2CodeTree;
  TXML2CodeClassXMLInspector := XML2CodeIns;
  TXML2CodeClassTreeComponent := XML2CodeTree;
  TXML2CodeChildXMLInspector := XML2CodeIns;
  TXML2CodeChildTreeComponent := XML2CodeTree;
end;

function TFrameXML2Code.GetXML2CodeObj: TXML2Code;
begin
  Result := FXML2CodeObj;
end;

procedure TFrameXML2Code.NewXML2Code(Sender: TObject);
begin
  FXML2CodeObj.TreeNodeShape := nil;
  FXML2CodeObj.Free;
  FXML2CodeObj := TXML2Code.Create(nil);
  FXML2CodeObj.TreeNodeShape := XML2CodeTreeRoot;
  FXML2CodeObj.ToTree;
end;

procedure TFrameXML2Code.OpenXML2Code(Sender: TObject);
begin
  if OpenDialogXML2Code.Execute then
  begin
    FXML2CodeObj.TreeNodeShape := nil;
    FXML2CodeObj.Free;
    FXML2CodeObj := TXML2Code.Create(nil);
    FXML2CodeObj.Load(OpenDialogXML2Code.FileName);
    FXML2CodeObj.TreeNodeShape := XML2CodeTreeRoot;
    FXML2CodeObj.ToTree;
  end;
end;

procedure TFrameXML2Code.SaveXML2Code(Sender: TObject);
begin
  if SaveDialogXML2Code.Execute then
  begin
    FXML2CodeObj.Save(SaveDialogXML2Code.FileName);
  end;
end;

procedure TFrameXML2Code.SetXML2CodeObj(const _Value: TXML2Code);
begin
  if Not Assigned(_Value) then
    Exit;
  FXML2CodeObj.TreeNodeShape := nil;
  FXML2CodeObj.Free;
  FXML2CodeObj := _Value;
end;

end.
