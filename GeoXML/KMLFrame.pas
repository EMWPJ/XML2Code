unit KMLFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,
  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,
  KML, KMLBase, FMX.Layouts;

type
  TFrameKML = class(TFrame)
    KMLTree: TXMLTree;
    KMLIns: TXMLInspector;
    SplitterBottom: TSplitter;
    KMLMenuBar: TMenuBar;
    NewKMLButton: TMenuItem;
    OpenKMLButton: TMenuItem;
    SaveKMLButton: TMenuItem;
    OpenDialogKML: TOpenDialog;
    SaveDialogKML: TSaveDialog;
    procedure NewKML(Sender: TObject);
    procedure OpenKML(Sender: TObject);
    procedure SaveKML(Sender: TObject);
  private
    FKMLObj: TKML;

    FTKMLPop: TPopupMenu;
    FTKMLDocumentPop: TPopupMenu;
    FTKMLFolderPop: TPopupMenu;
    FTKMLPlacemarkPop: TPopupMenu;
    FTKMLGeometryPop: TPopupMenu;
    FTKMLMultiGeometryPop: TPopupMenu;
    FTKMLPointPop: TPopupMenu;
    FTKMLPolygonPop: TPopupMenu;
    FTKMLBoundaryPop: TPopupMenu;
    FTKMLLinearRingPop: TPopupMenu;
    FTKMLLineStringPop: TPopupMenu;
    FTKMLLookAtPop: TPopupMenu;
    FTKMLLatLonBoxPop: TPopupMenu;
    FTKMLGroundOverlayPop: TPopupMenu;
    FTKMLExtendedDataPop: TPopupMenu;
    FTKMLDataPop: TPopupMenu;
    FTKMLSizePop: TPopupMenu;
    FTKMLStylePop: TPopupMenu;
    FTKMLIconStylePop: TPopupMenu;
    FTKMLIconPop: TPopupMenu;
    FTKMLLineStylePop: TPopupMenu;
    FTKMLBalloonStylePop: TPopupMenu;
    FTKMLStyleMapPop: TPopupMenu;
    FTKMLScreenOverlayPop: TPopupMenu;
    FTKMLPairPop: TPopupMenu;

    KMLTreeRoot: TTreeNodeShape;

  public
    function GetKMLObj: TKML;
    procedure SetKMLObj(const _Value: TKML);
    procedure FrameInit;
    property KMLObj: TKML read GetKMLObj write SetKMLObj;

  end;

implementation

{$R *.fmx}

procedure TFrameKML.FrameInit;
begin
  FTKMLPop := TPopupMenu.Create(Self);
  FTKMLPop.Parent := Self;
  TKMLPop := FTKMLPop;

  FTKMLDocumentPop := TPopupMenu.Create(Self);
  FTKMLDocumentPop.Parent := Self;
  TKMLDocumentPop := FTKMLDocumentPop;

  FTKMLFolderPop := TPopupMenu.Create(Self);
  FTKMLFolderPop.Parent := Self;
  TKMLFolderPop := FTKMLFolderPop;

  FTKMLPlacemarkPop := TPopupMenu.Create(Self);
  FTKMLPlacemarkPop.Parent := Self;
  TKMLPlacemarkPop := FTKMLPlacemarkPop;

  FTKMLGeometryPop := TPopupMenu.Create(Self);
  FTKMLGeometryPop.Parent := Self;
  TKMLGeometryPop := FTKMLGeometryPop;

  FTKMLMultiGeometryPop := TPopupMenu.Create(Self);
  FTKMLMultiGeometryPop.Parent := Self;
  TKMLMultiGeometryPop := FTKMLMultiGeometryPop;

  FTKMLPointPop := TPopupMenu.Create(Self);
  FTKMLPointPop.Parent := Self;
  TKMLPointPop := FTKMLPointPop;

  FTKMLPolygonPop := TPopupMenu.Create(Self);
  FTKMLPolygonPop.Parent := Self;
  TKMLPolygonPop := FTKMLPolygonPop;

  FTKMLBoundaryPop := TPopupMenu.Create(Self);
  FTKMLBoundaryPop.Parent := Self;
  TKMLBoundaryPop := FTKMLBoundaryPop;

  FTKMLLinearRingPop := TPopupMenu.Create(Self);
  FTKMLLinearRingPop.Parent := Self;
  TKMLLinearRingPop := FTKMLLinearRingPop;

  FTKMLLineStringPop := TPopupMenu.Create(Self);
  FTKMLLineStringPop.Parent := Self;
  TKMLLineStringPop := FTKMLLineStringPop;

  FTKMLLookAtPop := TPopupMenu.Create(Self);
  FTKMLLookAtPop.Parent := Self;
  TKMLLookAtPop := FTKMLLookAtPop;

  FTKMLLatLonBoxPop := TPopupMenu.Create(Self);
  FTKMLLatLonBoxPop.Parent := Self;
  TKMLLatLonBoxPop := FTKMLLatLonBoxPop;

  FTKMLGroundOverlayPop := TPopupMenu.Create(Self);
  FTKMLGroundOverlayPop.Parent := Self;
  TKMLGroundOverlayPop := FTKMLGroundOverlayPop;

  FTKMLExtendedDataPop := TPopupMenu.Create(Self);
  FTKMLExtendedDataPop.Parent := Self;
  TKMLExtendedDataPop := FTKMLExtendedDataPop;

  FTKMLDataPop := TPopupMenu.Create(Self);
  FTKMLDataPop.Parent := Self;
  TKMLDataPop := FTKMLDataPop;

  FTKMLSizePop := TPopupMenu.Create(Self);
  FTKMLSizePop.Parent := Self;
  TKMLSizePop := FTKMLSizePop;

  FTKMLStylePop := TPopupMenu.Create(Self);
  FTKMLStylePop.Parent := Self;
  TKMLStylePop := FTKMLStylePop;

  FTKMLIconStylePop := TPopupMenu.Create(Self);
  FTKMLIconStylePop.Parent := Self;
  TKMLIconStylePop := FTKMLIconStylePop;

  FTKMLIconPop := TPopupMenu.Create(Self);
  FTKMLIconPop.Parent := Self;
  TKMLIconPop := FTKMLIconPop;

  FTKMLLineStylePop := TPopupMenu.Create(Self);
  FTKMLLineStylePop.Parent := Self;
  TKMLLineStylePop := FTKMLLineStylePop;

  FTKMLBalloonStylePop := TPopupMenu.Create(Self);
  FTKMLBalloonStylePop.Parent := Self;
  TKMLBalloonStylePop := FTKMLBalloonStylePop;

  FTKMLStyleMapPop := TPopupMenu.Create(Self);
  FTKMLStyleMapPop.Parent := Self;
  TKMLStyleMapPop := FTKMLStyleMapPop;

  FTKMLScreenOverlayPop := TPopupMenu.Create(Self);
  FTKMLScreenOverlayPop.Parent := Self;
  TKMLScreenOverlayPop := FTKMLScreenOverlayPop;

  FTKMLPairPop := TPopupMenu.Create(Self);
  FTKMLPairPop.Parent := Self;
  TKMLPairPop := FTKMLPairPop;

  if Not Assigned(FKMLObj) then
    FKMLObj := TKML.Create(nil);
  if Not Assigned(KMLTreeRoot) then
    KMLTreeRoot := KMLTree.AddRoot('KML');
  FKMLObj.TreeNodeShape := KMLTreeRoot;
  FKMLObj.ToTree;

  TKMLXMLInspector := KMLIns;
  TKMLTreeComponent := KMLTree;
  TKMLDocumentXMLInspector := KMLIns;
  TKMLDocumentTreeComponent := KMLTree;
  TKMLFolderXMLInspector := KMLIns;
  TKMLFolderTreeComponent := KMLTree;
  TKMLPlacemarkXMLInspector := KMLIns;
  TKMLPlacemarkTreeComponent := KMLTree;
  TKMLGeometryXMLInspector := KMLIns;
  TKMLGeometryTreeComponent := KMLTree;
  TKMLMultiGeometryXMLInspector := KMLIns;
  TKMLMultiGeometryTreeComponent := KMLTree;
  TKMLPointXMLInspector := KMLIns;
  TKMLPointTreeComponent := KMLTree;
  TKMLPolygonXMLInspector := KMLIns;
  TKMLPolygonTreeComponent := KMLTree;
  TKMLBoundaryXMLInspector := KMLIns;
  TKMLBoundaryTreeComponent := KMLTree;
  TKMLLinearRingXMLInspector := KMLIns;
  TKMLLinearRingTreeComponent := KMLTree;
  TKMLLineStringXMLInspector := KMLIns;
  TKMLLineStringTreeComponent := KMLTree;
  TKMLLookAtXMLInspector := KMLIns;
  TKMLLookAtTreeComponent := KMLTree;
  TKMLLatLonBoxXMLInspector := KMLIns;
  TKMLLatLonBoxTreeComponent := KMLTree;
  TKMLGroundOverlayXMLInspector := KMLIns;
  TKMLGroundOverlayTreeComponent := KMLTree;
  TKMLExtendedDataXMLInspector := KMLIns;
  TKMLExtendedDataTreeComponent := KMLTree;
  TKMLDataXMLInspector := KMLIns;
  TKMLDataTreeComponent := KMLTree;
  TKMLSizeXMLInspector := KMLIns;
  TKMLSizeTreeComponent := KMLTree;
  TKMLStyleXMLInspector := KMLIns;
  TKMLStyleTreeComponent := KMLTree;
  TKMLIconStyleXMLInspector := KMLIns;
  TKMLIconStyleTreeComponent := KMLTree;
  TKMLIconXMLInspector := KMLIns;
  TKMLIconTreeComponent := KMLTree;
  TKMLLineStyleXMLInspector := KMLIns;
  TKMLLineStyleTreeComponent := KMLTree;
  TKMLBalloonStyleXMLInspector := KMLIns;
  TKMLBalloonStyleTreeComponent := KMLTree;
  TKMLStyleMapXMLInspector := KMLIns;
  TKMLStyleMapTreeComponent := KMLTree;
  TKMLScreenOverlayXMLInspector := KMLIns;
  TKMLScreenOverlayTreeComponent := KMLTree;
  TKMLPairXMLInspector := KMLIns;
  TKMLPairTreeComponent := KMLTree;
end;

function TFrameKML.GetKMLObj: TKML;
begin
  Result := FKMLObj;
end;

procedure TFrameKML.NewKML(Sender: TObject);
begin
  FKMLObj.TreeNodeShape := nil;
  FKMLObj.Free;
  FKMLObj := TKML.Create(nil);
  FKMLObj.TreeNodeShape := KMLTreeRoot;
  FKMLObj.ToTree;
end;

procedure TFrameKML.OpenKML(Sender: TObject);
begin
  if OpenDialogKML.Execute then
  begin
    FKMLObj.TreeNodeShape := nil;
    FKMLObj.Free;
    FKMLObj := TKML.Create(nil);
    FKMLObj.Load(OpenDialogKML.FileName);
    FKMLObj.TreeNodeShape := KMLTreeRoot;
    FKMLObj.ToTree;
  end;
end;

procedure TFrameKML.SaveKML(Sender: TObject);
begin
  if SaveDialogKML.Execute then
  begin
    FKMLObj.Save(SaveDialogKML.FileName);
  end;
end;

procedure TFrameKML.SetKMLObj(const _Value: TKML);
begin
  if Not Assigned(_Value) then
    Exit;
  FKMLObj.TreeNodeShape := nil;
  FKMLObj.Free;
  FKMLObj := _Value;
end;

end.
