unit GeoXMLFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,
  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,
  GeoXML, GeoXMLBase, FMX.Layouts;

type
  TFrameGeoXML = class(TFrame)
    GeoXMLTree: TXMLTree;
    GeoXMLIns: TXMLInspector;
    SplitterBottom: TSplitter;
    GeoXMLMenuBar: TMenuBar;
    NewGeoXMLButton: TMenuItem;
    OpenGeoXMLButton: TMenuItem;
    SaveGeoXMLButton: TMenuItem;
    OpenDialogGeoXML: TOpenDialog;
    SaveDialogGeoXML: TSaveDialog;
    procedure NewGeoXML(Sender: TObject);
    procedure OpenGeoXML(Sender: TObject);
    procedure SaveGeoXML(Sender: TObject);
  private
    FGeoXMLObj: TGeoXML;

    FTGeoXMLPop: TPopupMenu;
    FTGeoFolderPop: TPopupMenu;
    FTGeoElementPop: TPopupMenu;
    FTGeoPointPop: TPopupMenu;
    FTGeoLinePop: TPopupMenu;
    FTGeoLinesPop: TPopupMenu;
    FTGeoPolygonPop: TPopupMenu;
    FTGeoStylePop: TPopupMenu;
    FTGeoPointStylePop: TPopupMenu;
    FTGeoLineStylePop: TPopupMenu;
    FTGeoPolygonStylePop: TPopupMenu;

    GeoXMLTreeRoot: TTreeNodeShape;

  public
    function GetGeoXMLObj: TGeoXML;
    procedure SetGeoXMLObj(const _Value: TGeoXML);
    procedure FrameInit;
    property GeoXMLObj: TGeoXML read GetGeoXMLObj write SetGeoXMLObj;

  end;

implementation

{$R *.fmx}

procedure TFrameGeoXML.FrameInit;
begin
  FTGeoXMLPop := TPopupMenu.Create(Self);
  FTGeoXMLPop.Parent := Self;
  TGeoXMLPop := FTGeoXMLPop;

  FTGeoFolderPop := TPopupMenu.Create(Self);
  FTGeoFolderPop.Parent := Self;
  TGeoFolderPop := FTGeoFolderPop;

  FTGeoElementPop := TPopupMenu.Create(Self);
  FTGeoElementPop.Parent := Self;
  TGeoElementPop := FTGeoElementPop;

  FTGeoPointPop := TPopupMenu.Create(Self);
  FTGeoPointPop.Parent := Self;
  TGeoPointPop := FTGeoPointPop;

  FTGeoLinePop := TPopupMenu.Create(Self);
  FTGeoLinePop.Parent := Self;
  TGeoLinePop := FTGeoLinePop;

  FTGeoLinesPop := TPopupMenu.Create(Self);
  FTGeoLinesPop.Parent := Self;
  TGeoLinesPop := FTGeoLinesPop;

  FTGeoPolygonPop := TPopupMenu.Create(Self);
  FTGeoPolygonPop.Parent := Self;
  TGeoPolygonPop := FTGeoPolygonPop;

  FTGeoStylePop := TPopupMenu.Create(Self);
  FTGeoStylePop.Parent := Self;
  TGeoStylePop := FTGeoStylePop;

  FTGeoPointStylePop := TPopupMenu.Create(Self);
  FTGeoPointStylePop.Parent := Self;
  TGeoPointStylePop := FTGeoPointStylePop;

  FTGeoLineStylePop := TPopupMenu.Create(Self);
  FTGeoLineStylePop.Parent := Self;
  TGeoLineStylePop := FTGeoLineStylePop;

  FTGeoPolygonStylePop := TPopupMenu.Create(Self);
  FTGeoPolygonStylePop.Parent := Self;
  TGeoPolygonStylePop := FTGeoPolygonStylePop;

  if Not Assigned(FGeoXMLObj) then
    FGeoXMLObj := TGeoXML.Create(nil);
  if Not Assigned(GeoXMLTreeRoot) then
    GeoXMLTreeRoot := GeoXMLTree.AddRoot('GeoXML');
  FGeoXMLObj.TreeNodeShape := GeoXMLTreeRoot;
  FGeoXMLObj.ToTree;

  TGeoXMLXMLInspector := GeoXMLIns;
  TGeoXMLTreeComponent := GeoXMLTree;
  TGeoFolderXMLInspector := GeoXMLIns;
  TGeoFolderTreeComponent := GeoXMLTree;
  TGeoElementXMLInspector := GeoXMLIns;
  TGeoElementTreeComponent := GeoXMLTree;
  TGeoPointXMLInspector := GeoXMLIns;
  TGeoPointTreeComponent := GeoXMLTree;
  TGeoLineXMLInspector := GeoXMLIns;
  TGeoLineTreeComponent := GeoXMLTree;
  TGeoLinesXMLInspector := GeoXMLIns;
  TGeoLinesTreeComponent := GeoXMLTree;
  TGeoPolygonXMLInspector := GeoXMLIns;
  TGeoPolygonTreeComponent := GeoXMLTree;
  TGeoStyleXMLInspector := GeoXMLIns;
  TGeoStyleTreeComponent := GeoXMLTree;
  TGeoPointStyleXMLInspector := GeoXMLIns;
  TGeoPointStyleTreeComponent := GeoXMLTree;
  TGeoLineStyleXMLInspector := GeoXMLIns;
  TGeoLineStyleTreeComponent := GeoXMLTree;
  TGeoPolygonStyleXMLInspector := GeoXMLIns;
  TGeoPolygonStyleTreeComponent := GeoXMLTree;
end;

function TFrameGeoXML.GetGeoXMLObj: TGeoXML;
begin
  Result := FGeoXMLObj;
end;

procedure TFrameGeoXML.NewGeoXML(Sender: TObject);
begin
  FGeoXMLObj.TreeNodeShape := nil;
  FGeoXMLObj.Free;
  FGeoXMLObj := TGeoXML.Create(nil);
  FGeoXMLObj.TreeNodeShape := GeoXMLTreeRoot;
  FGeoXMLObj.ToTree;
end;

procedure TFrameGeoXML.OpenGeoXML(Sender: TObject);
begin
  if OpenDialogGeoXML.Execute then
  begin
    FGeoXMLObj.TreeNodeShape := nil;
    FGeoXMLObj.Free;
    FGeoXMLObj := TGeoXML.Create(nil);
    FGeoXMLObj.Load(OpenDialogGeoXML.FileName);
    FGeoXMLObj.TreeNodeShape := GeoXMLTreeRoot;
    FGeoXMLObj.ToTree;
  end;
end;

procedure TFrameGeoXML.SaveGeoXML(Sender: TObject);
begin
  if SaveDialogGeoXML.Execute then
  begin
    FGeoXMLObj.Save(SaveDialogGeoXML.FileName);
  end;
end;

procedure TFrameGeoXML.SetGeoXMLObj(const _Value: TGeoXML);
begin
  if Not Assigned(_Value) then
    Exit;
  FGeoXMLObj.TreeNodeShape := nil;
  FGeoXMLObj.Free;
  FGeoXMLObj := _Value;
end;

end.


