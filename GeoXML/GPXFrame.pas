unit GPXFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,
  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,
  GPX, GPXBase, FMX.Layouts;

type
  TFrameGPX = class(TFrame)
    GPXTree: TXMLTree;
    GPXIns: TXMLInspector;
    SplitterBottom: TSplitter;
    GPXMenuBar: TMenuBar;
    NewGPXButton: TMenuItem;
    OpenGPXButton: TMenuItem;
    SaveGPXButton: TMenuItem;
    OpenDialogGPX: TOpenDialog;
    SaveDialogGPX: TSaveDialog;
    procedure NewGPX(Sender: TObject);
    procedure OpenGPX(Sender: TObject);
    procedure SaveGPX(Sender: TObject);
  private
    FGPXObj: TGPX;

    FTGPXPop: TPopupMenu;
    FTGPXElementPop: TPopupMenu;
    FTGPXWayPointPop: TPopupMenu;
    FTGPXRoutePop: TPopupMenu;
    FTGPXTrackPop: TPopupMenu;
    FTGPXTrackSegmentPop: TPopupMenu;
    FTGPXMetadataPop: TPopupMenu;
    FTGPXCopyrightPop: TPopupMenu;
    FTGPXLinkPop: TPopupMenu;
    FTGPXEmailPop: TPopupMenu;
    FTGPXPersonPop: TPopupMenu;
    FTGPXPointPop: TPopupMenu;
    FTGPXBoundsPop: TPopupMenu;

    GPXTreeRoot: TTreeNodeShape;

  public
    function GetGPXObj: TGPX;
    procedure SetGPXObj(const _Value: TGPX);
    procedure FrameInit;
    property GPXObj: TGPX read GetGPXObj write SetGPXObj;

  end;

implementation

{$R *.fmx}

procedure TFrameGPX.FrameInit;
begin
  FTGPXPop := TPopupMenu.Create(Self);
  FTGPXPop.Parent := Self;
  TGPXPop := FTGPXPop;

  FTGPXElementPop := TPopupMenu.Create(Self);
  FTGPXElementPop.Parent := Self;
  TGPXElementPop := FTGPXElementPop;

  FTGPXWayPointPop := TPopupMenu.Create(Self);
  FTGPXWayPointPop.Parent := Self;
  TGPXWayPointPop := FTGPXWayPointPop;

  FTGPXRoutePop := TPopupMenu.Create(Self);
  FTGPXRoutePop.Parent := Self;
  TGPXRoutePop := FTGPXRoutePop;

  FTGPXTrackPop := TPopupMenu.Create(Self);
  FTGPXTrackPop.Parent := Self;
  TGPXTrackPop := FTGPXTrackPop;

  FTGPXTrackSegmentPop := TPopupMenu.Create(Self);
  FTGPXTrackSegmentPop.Parent := Self;
  TGPXTrackSegmentPop := FTGPXTrackSegmentPop;

  FTGPXMetadataPop := TPopupMenu.Create(Self);
  FTGPXMetadataPop.Parent := Self;
  TGPXMetadataPop := FTGPXMetadataPop;

  FTGPXCopyrightPop := TPopupMenu.Create(Self);
  FTGPXCopyrightPop.Parent := Self;
  TGPXCopyrightPop := FTGPXCopyrightPop;

  FTGPXLinkPop := TPopupMenu.Create(Self);
  FTGPXLinkPop.Parent := Self;
  TGPXLinkPop := FTGPXLinkPop;

  FTGPXEmailPop := TPopupMenu.Create(Self);
  FTGPXEmailPop.Parent := Self;
  TGPXEmailPop := FTGPXEmailPop;

  FTGPXPersonPop := TPopupMenu.Create(Self);
  FTGPXPersonPop.Parent := Self;
  TGPXPersonPop := FTGPXPersonPop;

  FTGPXPointPop := TPopupMenu.Create(Self);
  FTGPXPointPop.Parent := Self;
  TGPXPointPop := FTGPXPointPop;

  FTGPXBoundsPop := TPopupMenu.Create(Self);
  FTGPXBoundsPop.Parent := Self;
  TGPXBoundsPop := FTGPXBoundsPop;

  if Not Assigned(FGPXObj) then
    FGPXObj := TGPX.Create(nil);
  if Not Assigned(GPXTreeRoot) then
    GPXTreeRoot := GPXTree.AddRoot('GPX');
  FGPXObj.TreeNodeShape := GPXTreeRoot;
  FGPXObj.ToTree;

  TGPXXMLInspector := GPXIns;
  TGPXTreeComponent := GPXTree;
  TGPXElementXMLInspector := GPXIns;
  TGPXElementTreeComponent := GPXTree;
  TGPXWayPointXMLInspector := GPXIns;
  TGPXWayPointTreeComponent := GPXTree;
  TGPXRouteXMLInspector := GPXIns;
  TGPXRouteTreeComponent := GPXTree;
  TGPXTrackXMLInspector := GPXIns;
  TGPXTrackTreeComponent := GPXTree;
  TGPXTrackSegmentXMLInspector := GPXIns;
  TGPXTrackSegmentTreeComponent := GPXTree;
  TGPXMetadataXMLInspector := GPXIns;
  TGPXMetadataTreeComponent := GPXTree;
  TGPXCopyrightXMLInspector := GPXIns;
  TGPXCopyrightTreeComponent := GPXTree;
  TGPXLinkXMLInspector := GPXIns;
  TGPXLinkTreeComponent := GPXTree;
  TGPXEmailXMLInspector := GPXIns;
  TGPXEmailTreeComponent := GPXTree;
  TGPXPersonXMLInspector := GPXIns;
  TGPXPersonTreeComponent := GPXTree;
  TGPXPointXMLInspector := GPXIns;
  TGPXPointTreeComponent := GPXTree;
  TGPXBoundsXMLInspector := GPXIns;
  TGPXBoundsTreeComponent := GPXTree;
end;

function TFrameGPX.GetGPXObj: TGPX;
begin
  Result := FGPXObj;
end;

procedure TFrameGPX.NewGPX(Sender: TObject);
begin
  FGPXObj.TreeNodeShape := nil;
  FGPXObj.Free;
  FGPXObj := TGPX.Create(nil);
  FGPXObj.TreeNodeShape := GPXTreeRoot;
  FGPXObj.ToTree;
end;

procedure TFrameGPX.OpenGPX(Sender: TObject);
begin
  if OpenDialogGPX.Execute then
  begin
    FGPXObj.TreeNodeShape := nil;
    FGPXObj.Free;
    FGPXObj := TGPX.Create(nil);
    FGPXObj.Load(OpenDialogGPX.FileName);
    FGPXObj.TreeNodeShape := GPXTreeRoot;
    FGPXObj.ToTree;
  end;
end;

procedure TFrameGPX.SaveGPX(Sender: TObject);
begin
  if SaveDialogGPX.Execute then
  begin
    FGPXObj.Save(SaveDialogGPX.FileName);
  end;
end;

procedure TFrameGPX.SetGPXObj(const _Value: TGPX);
begin
  if Not Assigned(_Value) then
    Exit;
  FGPXObj.TreeNodeShape := nil;
  FGPXObj.Free;
  FGPXObj := _Value;
end;

end.


