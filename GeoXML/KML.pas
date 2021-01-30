unit KML;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections,
  XMLCore, XMLLeafTypes,
  ClientScreen, FMXTee.Tree, FMX.Menus,
  KMLBase;

type

  TKMLElementsEvent = procedure(Sender: TObject; value: TList<TKMLGeometry>)
    of object;

  TKMLHelper = class helper for TKML
  private
  protected
    procedure ExportGeometryEvent(Sender: TObject);
  public
    procedure GetAllFoler(var folders: TList<TKMLFolder>);
    procedure GetAllGeometry(var geometrys: TList<TKMLGeometry>);
    procedure GetAllPlacemark(var placemarks: TList<TKMLPlacemark>);
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); overload;
    procedure ToTree; overload;
  end;

  TKMLDocumentHelper = class helper for TKMLDocument
  private
  protected
    procedure ExportGeometryEvent(Sender: TObject);
  public
    procedure GetAllFoler(var folders: TList<TKMLFolder>);
    procedure GetAllGeometry(var geometrys: TList<TKMLGeometry>);
    procedure GetAllPlacemark(var placemarks: TList<TKMLPlacemark>);
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); overload;
    procedure ToTree; overload;
  end;

  TKMLFolderHelper = class helper for TKMLFolder
  private
  protected
    procedure ExportGeometryEvent(Sender: TObject);
  public
    procedure GetAllFoler(var folders: TList<TKMLFolder>);
    procedure GetAllGeometry(var geometrys: TList<TKMLGeometry>);
    procedure GetAllPlacemark(var placemarks: TList<TKMLPlacemark>);
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); overload;
    procedure ToTree; overload;
  end;

  TKMLPlacemarkHelper = class helper for TKMLPlacemark
  private
  protected
    procedure ExportGeometryEvent(Sender: TObject);
  public
    procedure GetAllGeometry(var geometrys: TList<TKMLGeometry>);
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); overload;
    procedure ToTree; overload;
  end;

  TKMLGeometryHelper = class helper for TKMLGeometry
  private
  protected
  public
  end;

  TKMLMultiGeometryHelper = class helper for TKMLMultiGeometry
  private
  protected
    procedure ExportGeometryEvent(Sender: TObject);
  public
    procedure GetAllGeometry(var geometrys: TList<TKMLGeometry>);
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); overload;
    procedure ToTree; overload;
  end;

  TKMLPointHelper = class helper for TKMLPoint
  private
  protected
  public
  end;

  TKMLPolygonHelper = class helper for TKMLPolygon
  private
  protected
  public
  end;

  TKMLBoundaryHelper = class helper for TKMLBoundary
  private
  protected
  public
  end;

  TKMLLinearRingHelper = class helper for TKMLLinearRing
  private
  protected
  public
  end;

  TKMLLineStringHelper = class helper for TKMLLineString
  private
  protected
  public
  end;

  TKMLLookAtHelper = class helper for TKMLLookAt
  private
  protected
  public
  end;

  TKMLLatLonBoxHelper = class helper for TKMLLatLonBox
  private
  protected
  public
  end;

  TKMLGroundOverlayHelper = class helper for TKMLGroundOverlay
  private
  protected
  public
  end;

  TKMLExtendedDataHelper = class helper for TKMLExtendedData
  private
  protected
  public
  end;

  TKMLDataHelper = class helper for TKMLData
  private
  protected
  public
  end;

  TKMLSizeHelper = class helper for TKMLSize
  private
  protected
  public
  end;

  TKMLStyleHelper = class helper for TKMLStyle
  private
  protected
  public
  end;

  TKMLIconStyleHelper = class helper for TKMLIconStyle
  private
  protected
  public
  end;

  TKMLIconHelper = class helper for TKMLIcon
  private
  protected
  public
  end;

  TKMLLineStyleHelper = class helper for TKMLLineStyle
  private
  protected
  public
  end;

  TKMLBalloonStyleHelper = class helper for TKMLBalloonStyle
  private
  protected
  public
  end;

  TKMLStyleMapHelper = class helper for TKMLStyleMap
  private
  protected
  public
  end;

  TKMLScreenOverlayHelper = class helper for TKMLScreenOverlay
  private
  protected
  public
  end;

  TKMLPairHelper = class helper for TKMLPair
  private
  protected
  public
  end;

var
  ExportKMLElementsEvent: TKMLElementsEvent;

implementation

{ kml }
procedure TKMLHelper.ExportGeometryEvent(Sender: TObject);
var
  geos: TList<TKMLGeometry>;
begin
  geos := TList<TKMLGeometry>.Create;
  Self.GetAllGeometry(geos);
  ExportKMLElementsEvent(Self, geos);
end;

procedure TKMLHelper.GetAllFoler(var folders: TList<TKMLFolder>);
begin
  Self.Document.GetAllFoler(folders);
end;

procedure TKMLHelper.GetAllGeometry(var geometrys: TList<TKMLGeometry>);
begin
  Self.Document.GetAllGeometry(geometrys);
end;

procedure TKMLHelper.GetAllPlacemark(var placemarks: TList<TKMLPlacemark>);
begin
  Self.Document.GetAllPlacemark(placemarks);
end;

procedure TKMLHelper.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  XmlnsAddMenu: TMenuItem;
  ExportGeometryMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLPop) and Assigned(TKMLTreeComponent) then
    begin
      TKMLPop.Clear;
      XmlnsAddMenu := TMenuItem.Create(TKMLPop);
      XmlnsAddMenu.text := 'Add Xmlns';
      XmlnsAddMenu.OnClick := AddXmlnsEvent;
      TKMLPop.AddObject(XmlnsAddMenu);

      ExportGeometryMenu := TMenuItem.Create(TKMLPop);
      ExportGeometryMenu.text := 'Export Geometry';
      ExportGeometryMenu.OnClick := ExportGeometryEvent;
      TKMLPop.AddObject(ExportGeometryMenu);

      pt := TPointF.Create(X, Y);
      pt := TKMLTreeComponent.ClientToScreen(pt);
      TKMLPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if XmlnsExsit then
    TreeNodeShape.AddChild('Xmlns');
  Document.TreeNodeShape := TreeNodeShape.AddChildObject('Document', Document);
  Document.ToTree;
end;

{ Document }
procedure TKMLDocumentHelper.ExportGeometryEvent(Sender: TObject);
var
  geos: TList<TKMLGeometry>;
begin
  geos := TList<TKMLGeometry>.Create;
  Self.GetAllGeometry(geos);
  ExportKMLElementsEvent(Self, geos);
end;

procedure TKMLDocumentHelper.GetAllFoler(var folders: TList<TKMLFolder>);
var
  I: Integer;
begin
  for I := 0 to FolderCount - 1 do
  begin
    folders.Add(Folder[I]);
    Folder[I].GetAllFoler(folders);
  end;
end;

procedure TKMLDocumentHelper.GetAllGeometry(var geometrys: TList<TKMLGeometry>);
var
  I: Integer;
begin
  for I := 0 to PlacemarkCount - 1 do
  begin
    Placemark[I].GetAllGeometry(geometrys);
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllGeometry(geometrys);
  end;
end;

procedure TKMLDocumentHelper.GetAllPlacemark(var placemarks
  : TList<TKMLPlacemark>);
var
  I: Integer;
begin
  for I := 0 to PlacemarkCount - 1 do
  begin
    placemarks.Add(Placemark[I]);
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllPlacemark(placemarks);
  end;
end;

procedure TKMLDocumentHelper.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  OpenAddMenu: TMenuItem;
  VisibilityAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  LookAtAddMenu: TMenuItem;
  StyleMapAddMenu: TMenuItem;
  StyleAddMenu: TMenuItem;
  PlacemarkAddMenu: TMenuItem;
  FolderAddMenu: TMenuItem;
  ExportGeometryMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLDocumentPop) and Assigned(TKMLDocumentTreeComponent) then
    begin
      TKMLDocumentPop.Clear;
      NameAddMenu := TMenuItem.Create(TKMLDocumentPop);
      NameAddMenu.text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TKMLDocumentPop.AddObject(NameAddMenu);
      OpenAddMenu := TMenuItem.Create(TKMLDocumentPop);
      OpenAddMenu.text := 'Add Open';
      OpenAddMenu.OnClick := AddOpenEvent;
      TKMLDocumentPop.AddObject(OpenAddMenu);
      VisibilityAddMenu := TMenuItem.Create(TKMLDocumentPop);
      VisibilityAddMenu.text := 'Add Visibility';
      VisibilityAddMenu.OnClick := AddVisibilityEvent;
      TKMLDocumentPop.AddObject(VisibilityAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TKMLDocumentPop);
      DescriptionAddMenu.text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TKMLDocumentPop.AddObject(DescriptionAddMenu);
      LookAtAddMenu := TMenuItem.Create(TKMLDocumentPop);
      LookAtAddMenu.text := 'Add LookAt';
      LookAtAddMenu.OnClick := AddLookAtEvent;
      TKMLDocumentPop.AddObject(LookAtAddMenu);
      StyleMapAddMenu := TMenuItem.Create(TKMLDocumentPop);
      StyleMapAddMenu.text := 'Add StyleMap';
      StyleMapAddMenu.OnClick := AddStyleMapEvent;
      TKMLDocumentPop.AddObject(StyleMapAddMenu);
      StyleAddMenu := TMenuItem.Create(TKMLDocumentPop);
      StyleAddMenu.text := 'Add Style';
      StyleAddMenu.OnClick := AddStyleEvent;
      TKMLDocumentPop.AddObject(StyleAddMenu);
      PlacemarkAddMenu := TMenuItem.Create(TKMLDocumentPop);
      PlacemarkAddMenu.text := 'Add Placemark';
      PlacemarkAddMenu.OnClick := AddPlacemarkEvent;
      TKMLDocumentPop.AddObject(PlacemarkAddMenu);
      FolderAddMenu := TMenuItem.Create(TKMLDocumentPop);
      FolderAddMenu.text := 'Add Folder';
      FolderAddMenu.OnClick := AddFolderEvent;
      TKMLDocumentPop.AddObject(FolderAddMenu);

      ExportGeometryMenu := TMenuItem.Create(TKMLDocumentPop);
      ExportGeometryMenu.text := 'Export Geometry';
      ExportGeometryMenu.OnClick := ExportGeometryEvent;
      TKMLDocumentPop.AddObject(ExportGeometryMenu);

      pt := TPointF.Create(X, Y);
      pt := TKMLDocumentTreeComponent.ClientToScreen(pt);
      TKMLDocumentPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLDocumentHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if NameExsit then
    TreeNodeShape.AddChild('Name');
  if OpenExsit then
    TreeNodeShape.AddChild('Open');
  if VisibilityExsit then
    TreeNodeShape.AddChild('Visibility');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if LookAtExsit then
  begin
    LookAt.TreeNodeShape := TreeNodeShape.AddChildObject('LookAt', LookAt);
    LookAt.ToTree;
  end;
  for I := 0 to StyleMapCount - 1 do
  begin
    StyleMaps[I].TreeNodeShape := TreeNodeShape.AddChildObject('StyleMap',
      StyleMap[I]);
    StyleMap[I].ToTree;
  end;
  for I := 0 to StyleCount - 1 do
  begin
    Styles[I].TreeNodeShape := TreeNodeShape.AddChildObject('Style', Style[I]);
    Style[I].ToTree;
  end;
  for I := 0 to PlacemarkCount - 1 do
  begin
    placemarks[I].TreeNodeShape := TreeNodeShape.AddChildObject('Placemark',
      Placemark[I]);
    Placemark[I].ToTree;
  end;
  for I := 0 to FolderCount - 1 do
  begin
    folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder',
      Folder[I]);
    Folder[I].ToTree;
  end;
end;

{ Folder }
procedure TKMLFolderHelper.ExportGeometryEvent(Sender: TObject);
var
  geos: TList<TKMLGeometry>;
begin
  geos := TList<TKMLGeometry>.Create;
  Self.GetAllGeometry(geos);
  ExportKMLElementsEvent(Self, geos);
end;

procedure TKMLFolderHelper.GetAllFoler(var folders: TList<TKMLFolder>);
var
  I: Integer;
begin
  for I := 0 to FolderCount - 1 do
  begin
    folders.Add(Folder[I]);
    Folder[I].GetAllFoler(folders);
  end;
  if DocumentExsit then
    Document.GetAllFoler(folders);
end;

procedure TKMLFolderHelper.GetAllGeometry(var geometrys: TList<TKMLGeometry>);
var
  I: Integer;
begin
  for I := 0 to PlacemarkCount - 1 do
  begin
    Placemark[I].GetAllGeometry(geometrys);
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllGeometry(geometrys);
  end;
  if DocumentExsit then
    Document.GetAllGeometry(geometrys);
end;

procedure TKMLFolderHelper.GetAllPlacemark(var placemarks
  : TList<TKMLPlacemark>);
var
  I: Integer;
begin
  for I := 0 to PlacemarkCount - 1 do
  begin
    placemarks.Add(Placemark[I]);
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllPlacemark(placemarks);
  end;
  if DocumentExsit then
    Document.GetAllPlacemark(placemarks);
end;

procedure TKMLFolderHelper.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  OpenAddMenu: TMenuItem;
  VisibilityAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  LookAtAddMenu: TMenuItem;
  DocumentAddMenu: TMenuItem;
  GroundOverlayAddMenu: TMenuItem;
  PlacemarkAddMenu: TMenuItem;
  ScreenOverlayAddMenu: TMenuItem;
  FolderAddMenu: TMenuItem;
  ExportGeometryMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLFolderPop) and Assigned(TKMLFolderTreeComponent) then
    begin
      TKMLFolderPop.Clear;
      NameAddMenu := TMenuItem.Create(TKMLFolderPop);
      NameAddMenu.text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TKMLFolderPop.AddObject(NameAddMenu);
      OpenAddMenu := TMenuItem.Create(TKMLFolderPop);
      OpenAddMenu.text := 'Add Open';
      OpenAddMenu.OnClick := AddOpenEvent;
      TKMLFolderPop.AddObject(OpenAddMenu);
      VisibilityAddMenu := TMenuItem.Create(TKMLFolderPop);
      VisibilityAddMenu.text := 'Add Visibility';
      VisibilityAddMenu.OnClick := AddVisibilityEvent;
      TKMLFolderPop.AddObject(VisibilityAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TKMLFolderPop);
      DescriptionAddMenu.text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TKMLFolderPop.AddObject(DescriptionAddMenu);
      LookAtAddMenu := TMenuItem.Create(TKMLFolderPop);
      LookAtAddMenu.text := 'Add LookAt';
      LookAtAddMenu.OnClick := AddLookAtEvent;
      TKMLFolderPop.AddObject(LookAtAddMenu);
      DocumentAddMenu := TMenuItem.Create(TKMLFolderPop);
      DocumentAddMenu.text := 'Add Document';
      DocumentAddMenu.OnClick := AddDocumentEvent;
      TKMLFolderPop.AddObject(DocumentAddMenu);
      GroundOverlayAddMenu := TMenuItem.Create(TKMLFolderPop);
      GroundOverlayAddMenu.text := 'Add GroundOverlay';
      GroundOverlayAddMenu.OnClick := AddGroundOverlayEvent;
      TKMLFolderPop.AddObject(GroundOverlayAddMenu);
      PlacemarkAddMenu := TMenuItem.Create(TKMLFolderPop);
      PlacemarkAddMenu.text := 'Add Placemark';
      PlacemarkAddMenu.OnClick := AddPlacemarkEvent;
      TKMLFolderPop.AddObject(PlacemarkAddMenu);
      ScreenOverlayAddMenu := TMenuItem.Create(TKMLFolderPop);
      ScreenOverlayAddMenu.text := 'Add ScreenOverlay';
      ScreenOverlayAddMenu.OnClick := AddScreenOverlayEvent;
      TKMLFolderPop.AddObject(ScreenOverlayAddMenu);
      FolderAddMenu := TMenuItem.Create(TKMLFolderPop);
      FolderAddMenu.text := 'Add Folder';
      FolderAddMenu.OnClick := AddFolderEvent;
      TKMLFolderPop.AddObject(FolderAddMenu);

      ExportGeometryMenu := TMenuItem.Create(TKMLFolderPop);
      ExportGeometryMenu.text := 'Export Geometry';
      ExportGeometryMenu.OnClick := ExportGeometryEvent;
      TKMLFolderPop.AddObject(ExportGeometryMenu);

      pt := TPointF.Create(X, Y);
      pt := TKMLFolderTreeComponent.ClientToScreen(pt);
      TKMLFolderPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLFolderHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if NameExsit then
    TreeNodeShape.AddChild('Name');
  if OpenExsit then
    TreeNodeShape.AddChild('Open');
  if VisibilityExsit then
    TreeNodeShape.AddChild('Visibility');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if LookAtExsit then
  begin
    LookAt.TreeNodeShape := TreeNodeShape.AddChildObject('LookAt', LookAt);
    LookAt.ToTree;
  end;
  if DocumentExsit then
  begin
    Document.TreeNodeShape := TreeNodeShape.AddChildObject('Document',
      Document);
    Document.ToTree;
  end;
  for I := 0 to GroundOverlayCount - 1 do
  begin
    GroundOverlays[I].TreeNodeShape := TreeNodeShape.AddChildObject
      ('GroundOverlay', GroundOverlay[I]);
    GroundOverlay[I].ToTree;
  end;
  for I := 0 to PlacemarkCount - 1 do
  begin
    placemarks[I].TreeNodeShape := TreeNodeShape.AddChildObject('Placemark',
      Placemark[I]);
    Placemark[I].ToTree;
  end;
  for I := 0 to ScreenOverlayCount - 1 do
  begin
    ScreenOverlays[I].TreeNodeShape := TreeNodeShape.AddChildObject
      ('ScreenOverlay', ScreenOverlay[I]);
    ScreenOverlay[I].ToTree;
  end;
  for I := 0 to FolderCount - 1 do
  begin
    folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder',
      Folder[I]);
    Folder[I].ToTree;
  end;
end;

{ Placemark }
procedure TKMLPlacemarkHelper.ExportGeometryEvent(Sender: TObject);
var
  geos: TList<TKMLGeometry>;
begin
  geos := TList<TKMLGeometry>.Create;
  Self.GetAllGeometry(geos);
  ExportKMLElementsEvent(Self, geos);
end;

procedure TKMLPlacemarkHelper.GetAllGeometry(var geometrys
  : TList<TKMLGeometry>);
begin
  if not GeometryExsit then
    Exit;
  if Geometry is TKMLMultiGeometry then
  begin
    TKMLMultiGeometry(Geometry).GetAllGeometry(geometrys);
  end
  else
  begin
    geometrys.Add(Geometry);
  end;
end;

procedure TKMLPlacemarkHelper.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  VisibilityAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  LookAtAddMenu: TMenuItem;
  StyleUrlAddMenu: TMenuItem;
  GeometryAddMenu: TMenuItem;
  PointAddMenu: TMenuItem;
  PolygonAddMenu: TMenuItem;
  LinearRingAddMenu: TMenuItem;
  LineStringAddMenu: TMenuItem;
  MultiGeometryAddMenu: TMenuItem;
  ExtendedDataAddMenu: TMenuItem;
  ExportGeometryMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLPlacemarkPop) and Assigned(TKMLPlacemarkTreeComponent) then
    begin
      TKMLPlacemarkPop.Clear;
      NameAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      NameAddMenu.text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TKMLPlacemarkPop.AddObject(NameAddMenu);
      VisibilityAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      VisibilityAddMenu.text := 'Add Visibility';
      VisibilityAddMenu.OnClick := AddVisibilityEvent;
      TKMLPlacemarkPop.AddObject(VisibilityAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      DescriptionAddMenu.text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TKMLPlacemarkPop.AddObject(DescriptionAddMenu);
      LookAtAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      LookAtAddMenu.text := 'Add LookAt';
      LookAtAddMenu.OnClick := AddLookAtEvent;
      TKMLPlacemarkPop.AddObject(LookAtAddMenu);
      StyleUrlAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      StyleUrlAddMenu.text := 'Add StyleUrl';
      StyleUrlAddMenu.OnClick := AddStyleUrlEvent;
      TKMLPlacemarkPop.AddObject(StyleUrlAddMenu);
      GeometryAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      GeometryAddMenu.text := 'Add Geometry';
      TKMLPlacemarkPop.AddObject(GeometryAddMenu);
      PointAddMenu := TMenuItem.Create(GeometryAddMenu);
      PointAddMenu.text := 'Point';
      PointAddMenu.OnClick := AddPointEvent;
      GeometryAddMenu.AddObject(PointAddMenu);
      PolygonAddMenu := TMenuItem.Create(GeometryAddMenu);
      PolygonAddMenu.text := 'Polygon';
      PolygonAddMenu.OnClick := AddPolygonEvent;
      GeometryAddMenu.AddObject(PolygonAddMenu);
      LinearRingAddMenu := TMenuItem.Create(GeometryAddMenu);
      LinearRingAddMenu.text := 'LinearRing';
      LinearRingAddMenu.OnClick := AddLinearRingEvent;
      GeometryAddMenu.AddObject(LinearRingAddMenu);
      LineStringAddMenu := TMenuItem.Create(GeometryAddMenu);
      LineStringAddMenu.text := 'LineString';
      LineStringAddMenu.OnClick := AddLineStringEvent;
      GeometryAddMenu.AddObject(LineStringAddMenu);
      MultiGeometryAddMenu := TMenuItem.Create(GeometryAddMenu);
      MultiGeometryAddMenu.text := 'MultiGeometry';
      MultiGeometryAddMenu.OnClick := AddMultiGeometryEvent;
      GeometryAddMenu.AddObject(MultiGeometryAddMenu);
      ExtendedDataAddMenu := TMenuItem.Create(TKMLPlacemarkPop);
      ExtendedDataAddMenu.text := 'Add ExtendedData';
      ExtendedDataAddMenu.OnClick := AddExtendedDataEvent;

      ExportGeometryMenu := TMenuItem.Create(TKMLPlacemarkPop);
      ExportGeometryMenu.text := 'Export Geometry';
      ExportGeometryMenu.OnClick := ExportGeometryEvent;
      TKMLPlacemarkPop.AddObject(ExportGeometryMenu);

      pt := TPointF.Create(X, Y);
      pt := TKMLPlacemarkTreeComponent.ClientToScreen(pt);
      TKMLPlacemarkPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLPlacemarkHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if NameExsit then
    TreeNodeShape.AddChild('Name');
  if VisibilityExsit then
    TreeNodeShape.AddChild('Visibility');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if LookAtExsit then
  begin
    LookAt.TreeNodeShape := TreeNodeShape.AddChildObject('LookAt', LookAt);
    LookAt.ToTree;
  end;
  if StyleUrlExsit then
    TreeNodeShape.AddChild('StyleUrl');
  if GeometryExsit then
  begin
    Geometry.TreeNodeShape := TreeNodeShape.AddChildObject('Geometry',
      Geometry);
    Geometry.ToTree;
  end;
  if ExtendedDataExsit then
  begin
    ExtendedData.TreeNodeShape := TreeNodeShape.AddChildObject('ExtendedData',
      ExtendedData);
    ExtendedData.ToTree;
  end;
end;

{ Geometry }
{ MultiGeometry }
procedure TKMLMultiGeometryHelper.ExportGeometryEvent(Sender: TObject);
var
  geos: TList<TKMLGeometry>;
begin
  geos := TList<TKMLGeometry>.Create;
  Self.GetAllGeometry(geos);
  ExportKMLElementsEvent(Self, geos);
end;

procedure TKMLMultiGeometryHelper.GetAllGeometry(var geometrys
  : TList<TKMLGeometry>);
var
  I: Integer;
begin
  if not Assigned(geometrys) then

    for I := 0 to GeometryCount - 1 do
    begin
      geometrys.Add(Self);
    end;
end;

procedure TKMLMultiGeometryHelper.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  GeometryAddMenu: TMenuItem;
  PointAddMenu: TMenuItem;
  PolygonAddMenu: TMenuItem;
  LinearRingAddMenu: TMenuItem;
  LineStringAddMenu: TMenuItem;
  MultiGeometryAddMenu: TMenuItem;
  ExportGeometryMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLMultiGeometryPop) and
      Assigned(TKMLMultiGeometryTreeComponent) then
    begin
      TKMLMultiGeometryPop.Clear;
      GeometryAddMenu := TMenuItem.Create(TKMLMultiGeometryPop);
      GeometryAddMenu.text := 'Add Geometry';
      TKMLMultiGeometryPop.AddObject(GeometryAddMenu);
      PointAddMenu := TMenuItem.Create(GeometryAddMenu);
      PointAddMenu.text := 'Point';
      PointAddMenu.OnClick := AddPointEvent;
      GeometryAddMenu.AddObject(PointAddMenu);
      PolygonAddMenu := TMenuItem.Create(GeometryAddMenu);
      PolygonAddMenu.text := 'Polygon';
      PolygonAddMenu.OnClick := AddPolygonEvent;
      GeometryAddMenu.AddObject(PolygonAddMenu);
      LinearRingAddMenu := TMenuItem.Create(GeometryAddMenu);
      LinearRingAddMenu.text := 'LinearRing';
      LinearRingAddMenu.OnClick := AddLinearRingEvent;
      GeometryAddMenu.AddObject(LinearRingAddMenu);
      LineStringAddMenu := TMenuItem.Create(GeometryAddMenu);
      LineStringAddMenu.text := 'LineString';
      LineStringAddMenu.OnClick := AddLineStringEvent;
      GeometryAddMenu.AddObject(LineStringAddMenu);
      MultiGeometryAddMenu := TMenuItem.Create(GeometryAddMenu);
      MultiGeometryAddMenu.text := 'MultiGeometry';
      MultiGeometryAddMenu.OnClick := AddMultiGeometryEvent;
      GeometryAddMenu.AddObject(MultiGeometryAddMenu);

      ExportGeometryMenu := TMenuItem.Create(TKMLMultiGeometryPop);
      ExportGeometryMenu.text := 'Export Geometry';
      ExportGeometryMenu.OnClick := ExportGeometryEvent;
      TKMLMultiGeometryPop.AddObject(ExportGeometryMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLMultiGeometryTreeComponent.ClientToScreen(pt);
      TKMLMultiGeometryPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLMultiGeometryHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to GeometryCount - 1 do
  begin
    geometrys[I].TreeNodeShape := TreeNodeShape.AddChildObject('Geometry',
      Geometry[I]);
    Geometry[I].ToTree;
  end;
end;

{ Point }
{ Polygon }
{ Boundary }
{ LinearRing }
{ LineString }
{ LookAt }
{ LatLonBox }
{ GroundOverlay }
{ ExtendedData }
{ Data }
{ Size }
{ Style }
{ IconStyle }
{ Icon }
{ LineStyle }
{ BalloonStyle }
{ StyleMap }
{ ScreenOverlay }
{ Pair }

end.
