unit KMLBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,
  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,
  ClientScreen, XMLInspector;

type

  TKML = class;
  TKMLDocument = class;
  TKMLFolder = class;
  TKMLPlacemark = class;
  TKMLGeometry = class;
  TKMLMultiGeometry = class;
  TKMLPoint = class;
  TKMLPolygon = class;
  TKMLBoundary = class;
  TKMLLinearRing = class;
  TKMLLineString = class;
  TKMLLookAt = class;
  TKMLLatLonBox = class;
  TKMLGroundOverlay = class;
  TKMLExtendedData = class;
  TKMLData = class;
  TKMLSize = class;
  TKMLStyle = class;
  TKMLIconStyle = class;
  TKMLIcon = class;
  TKMLLineStyle = class;
  TKMLBalloonStyle = class;
  TKMLStyleMap = class;
  TKMLScreenOverlay = class;
  TKMLPair = class;

  TKML = class(TXML)
  private
    FXmlns: String;
    FXmlnsExsit: Boolean;
    FDocument: TKMLDocument;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetXmlns(const _Value: String);
    procedure SetDocument(const _Value: TKMLDocument);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddXmlnsEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddXmlns: String;
    procedure XmlnsRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Xmlns: String read FXmlns write SetXmlns;
    property Document: TKMLDocument read FDocument write SetDocument;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property XmlnsExsit: Boolean read FXmlnsExsit;
  end;

  TKMLDocument = class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FOpen: Integer;
    FOpenExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FStyleMaps: TList<TKMLStyleMap>;
    FStyles: TList<TKMLStyle>;
    FPlacemarks: TList<TKMLPlacemark>;
    FFolders: TList<TKMLFolder>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetOpen(const _Value: Integer);
    procedure SetVisibility(const _Value: Integer);
    procedure SetDescription(const _Value: String);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetStyleMaps(const _Value: TList<TKMLStyleMap>);
    function GetStyleMap(Index: Integer): TKMLStyleMap;
    procedure SetStyleMap(Index: Integer; const _Value: TKMLStyleMap);
    procedure SetStyles(const _Value: TList<TKMLStyle>);
    function GetStyle(Index: Integer): TKMLStyle;
    procedure SetStyle(Index: Integer; const _Value: TKMLStyle);
    procedure SetPlacemarks(const _Value: TList<TKMLPlacemark>);
    function GetPlacemark(Index: Integer): TKMLPlacemark;
    procedure SetPlacemark(Index: Integer; const _Value: TKMLPlacemark);
    procedure SetFolders(const _Value: TList<TKMLFolder>);
    function GetFolder(Index: Integer): TKMLFolder;
    procedure SetFolder(Index: Integer; const _Value: TKMLFolder);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddOpenEvent(Sender: TObject);
    procedure AddVisibilityEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddLookAtEvent(Sender: TObject);
    procedure AddStyleMapEvent(Sender: TObject);
    procedure AddStyleEvent(Sender: TObject);
    procedure AddPlacemarkEvent(Sender: TObject);
    procedure AddFolderEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddOpen: Integer;
    procedure OpenRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddStyleMap: TKMLStyleMap;
    procedure StyleMapClear;
    function StyleMapCount: Integer;
    procedure RemoveStyleMap(_Value: TKMLStyleMap);
    procedure DeleteStyleMap(Index: Integer);
    function AddStyle: TKMLStyle;
    procedure StyleClear;
    function StyleCount: Integer;
    procedure RemoveStyle(_Value: TKMLStyle);
    procedure DeleteStyle(Index: Integer);
    function AddPlacemark: TKMLPlacemark;
    procedure PlacemarkClear;
    function PlacemarkCount: Integer;
    procedure RemovePlacemark(_Value: TKMLPlacemark);
    procedure DeletePlacemark(Index: Integer);
    function AddFolder: TKMLFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TKMLFolder);
    procedure DeleteFolder(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Open: Integer read FOpen write SetOpen;
    property Visibility: Integer read FVisibility write SetVisibility;
    property Description: String read FDescription write SetDescription;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property StyleMaps: TList<TKMLStyleMap> read FStyleMaps write SetStyleMaps;
    property StyleMap[Index: Integer]: TKMLStyleMap read GetStyleMap
      write SetStyleMap;
    property Styles: TList<TKMLStyle> read FStyles write SetStyles;
    property Style[Index: Integer]: TKMLStyle read GetStyle write SetStyle;
    property Placemarks: TList<TKMLPlacemark> read FPlacemarks
      write SetPlacemarks;
    property Placemark[Index: Integer]: TKMLPlacemark read GetPlacemark
      write SetPlacemark;
    property Folders: TList<TKMLFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TKMLFolder read GetFolder write SetFolder;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property OpenExsit: Boolean read FOpenExsit;
    property VisibilityExsit: Boolean read FVisibilityExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property LookAtExsit: Boolean read FLookAtExsit;
  end;

  TKMLFolder = class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FOpen: Integer;
    FOpenExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FDocument: TKMLDocument;
    FDocumentExsit: Boolean;
    FGroundOverlays: TList<TKMLGroundOverlay>;
    FPlacemarks: TList<TKMLPlacemark>;
    FScreenOverlays: TList<TKMLScreenOverlay>;
    FFolders: TList<TKMLFolder>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetOpen(const _Value: Integer);
    procedure SetVisibility(const _Value: Integer);
    procedure SetDescription(const _Value: String);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetDocument(const _Value: TKMLDocument);
    procedure SetGroundOverlays(const _Value: TList<TKMLGroundOverlay>);
    function GetGroundOverlay(Index: Integer): TKMLGroundOverlay;
    procedure SetGroundOverlay(Index: Integer; const _Value: TKMLGroundOverlay);
    procedure SetPlacemarks(const _Value: TList<TKMLPlacemark>);
    function GetPlacemark(Index: Integer): TKMLPlacemark;
    procedure SetPlacemark(Index: Integer; const _Value: TKMLPlacemark);
    procedure SetScreenOverlays(const _Value: TList<TKMLScreenOverlay>);
    function GetScreenOverlay(Index: Integer): TKMLScreenOverlay;
    procedure SetScreenOverlay(Index: Integer; const _Value: TKMLScreenOverlay);
    procedure SetFolders(const _Value: TList<TKMLFolder>);
    function GetFolder(Index: Integer): TKMLFolder;
    procedure SetFolder(Index: Integer; const _Value: TKMLFolder);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddOpenEvent(Sender: TObject);
    procedure AddVisibilityEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddLookAtEvent(Sender: TObject);
    procedure AddDocumentEvent(Sender: TObject);
    procedure AddGroundOverlayEvent(Sender: TObject);
    procedure AddPlacemarkEvent(Sender: TObject);
    procedure AddScreenOverlayEvent(Sender: TObject);
    procedure AddFolderEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddOpen: Integer;
    procedure OpenRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddDocument: TKMLDocument;
    procedure DocumentRemove;
    function AddGroundOverlay: TKMLGroundOverlay;
    procedure GroundOverlayClear;
    function GroundOverlayCount: Integer;
    procedure RemoveGroundOverlay(_Value: TKMLGroundOverlay);
    procedure DeleteGroundOverlay(Index: Integer);
    function AddPlacemark: TKMLPlacemark;
    procedure PlacemarkClear;
    function PlacemarkCount: Integer;
    procedure RemovePlacemark(_Value: TKMLPlacemark);
    procedure DeletePlacemark(Index: Integer);
    function AddScreenOverlay: TKMLScreenOverlay;
    procedure ScreenOverlayClear;
    function ScreenOverlayCount: Integer;
    procedure RemoveScreenOverlay(_Value: TKMLScreenOverlay);
    procedure DeleteScreenOverlay(Index: Integer);
    function AddFolder: TKMLFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TKMLFolder);
    procedure DeleteFolder(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Open: Integer read FOpen write SetOpen;
    property Visibility: Integer read FVisibility write SetVisibility;
    property Description: String read FDescription write SetDescription;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property Document: TKMLDocument read FDocument write SetDocument;
    property GroundOverlays: TList<TKMLGroundOverlay> read FGroundOverlays
      write SetGroundOverlays;
    property GroundOverlay[Index: Integer]: TKMLGroundOverlay
      read GetGroundOverlay write SetGroundOverlay;
    property Placemarks: TList<TKMLPlacemark> read FPlacemarks
      write SetPlacemarks;
    property Placemark[Index: Integer]: TKMLPlacemark read GetPlacemark
      write SetPlacemark;
    property ScreenOverlays: TList<TKMLScreenOverlay> read FScreenOverlays
      write SetScreenOverlays;
    property ScreenOverlay[Index: Integer]: TKMLScreenOverlay
      read GetScreenOverlay write SetScreenOverlay;
    property Folders: TList<TKMLFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TKMLFolder read GetFolder write SetFolder;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property OpenExsit: Boolean read FOpenExsit;
    property VisibilityExsit: Boolean read FVisibilityExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property LookAtExsit: Boolean read FLookAtExsit;
    property DocumentExsit: Boolean read FDocumentExsit;
  end;

  TKMLPlacemark = class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FStyleUrl: String;
    FStyleUrlExsit: Boolean;
    FGeometry: TKMLGeometry;
    FGeometryExsit: Boolean;
    FExtendedData: TKMLExtendedData;
    FExtendedDataExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetVisibility(const _Value: Integer);
    procedure SetDescription(const _Value: String);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetStyleUrl(const _Value: String);
    procedure SetGeometry(const _Value: TKMLGeometry);
    procedure SetExtendedData(const _Value: TKMLExtendedData);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddVisibilityEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddLookAtEvent(Sender: TObject);
    procedure AddStyleUrlEvent(Sender: TObject);
    procedure AddPointEvent(Sender: TObject);
    procedure AddPolygonEvent(Sender: TObject);
    procedure AddLinearRingEvent(Sender: TObject);
    procedure AddLineStringEvent(Sender: TObject);
    procedure AddMultiGeometryEvent(Sender: TObject);
    procedure AddExtendedDataEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddStyleUrl: String;
    procedure StyleUrlRemove;
    function AddPoint: TKMLPoint;
    function AddPolygon: TKMLPolygon;
    function AddLinearRing: TKMLLinearRing;
    function AddLineString: TKMLLineString;
    function AddMultiGeometry: TKMLMultiGeometry;
    procedure GeometryRemove;
    function AddExtendedData: TKMLExtendedData;
    procedure ExtendedDataRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Visibility: Integer read FVisibility write SetVisibility;
    property Description: String read FDescription write SetDescription;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property StyleUrl: String read FStyleUrl write SetStyleUrl;
    property Geometry: TKMLGeometry read FGeometry write SetGeometry;
    property ExtendedData: TKMLExtendedData read FExtendedData
      write SetExtendedData;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property VisibilityExsit: Boolean read FVisibilityExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property LookAtExsit: Boolean read FLookAtExsit;
    property StyleUrlExsit: Boolean read FStyleUrlExsit;
    property GeometryExsit: Boolean read FGeometryExsit;
    property ExtendedDataExsit: Boolean read FExtendedDataExsit;
  end;

  TKMLGeometry = class abstract(TXML)
  private
    FTreeNodeShape: TTreeNodeShape;
  protected
    procedure FromXML(node: IXMLNode); virtual;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; virtual;
    function AppendToXML(node: IXMLNode; pt: string = ''): IXMLNode;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLMultiGeometry = class(TKMLGeometry)
  private
    FGeometrys: TList<TKMLGeometry>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetGeometrys(const _Value: TList<TKMLGeometry>);
    function GetGeometry(Index: Integer): TKMLGeometry;
    procedure SetGeometry(Index: Integer; const _Value: TKMLGeometry);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddPointEvent(Sender: TObject);
    procedure AddPolygonEvent(Sender: TObject);
    procedure AddLinearRingEvent(Sender: TObject);
    procedure AddLineStringEvent(Sender: TObject);
    procedure AddMultiGeometryEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddPoint: TKMLPoint;
    function AddPolygon: TKMLPolygon;
    function AddLinearRing: TKMLLinearRing;
    function AddLineString: TKMLLineString;
    function AddMultiGeometry: TKMLMultiGeometry;
    procedure GeometryClear;
    function GeometryCount: Integer;
    procedure RemoveGeometry(_Value: TKMLGeometry);
    procedure DeleteGeometry(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Geometrys: TList<TKMLGeometry> read FGeometrys write SetGeometrys;
    property Geometry[Index: Integer]: TKMLGeometry read GetGeometry
      write SetGeometry;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLPoint = class(TKMLGeometry)
  private
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FCoordinates: ArrayCoordinates;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetExtrude(const _Value: String);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddExtrudeEvent(Sender: TObject);
    procedure AddAltitudeModeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Extrude: String read FExtrude write SetExtrude;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property Coordinates: ArrayCoordinates read FCoordinates
      write SetCoordinates;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ExtrudeExsit: Boolean read FExtrudeExsit;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit;
  end;

  TKMLPolygon = class(TKMLGeometry)
  private
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FOuterBoundaryIss: TList<TKMLBoundary>;
    FInnerBoundaryIss: TList<TKMLBoundary>;
    FTessellate: String;
    FTessellateExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetExtrude(const _Value: String);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetOuterBoundaryIss(const _Value: TList<TKMLBoundary>);
    function GetOuterBoundaryIs(Index: Integer): TKMLBoundary;
    procedure SetOuterBoundaryIs(Index: Integer; const _Value: TKMLBoundary);
    procedure SetInnerBoundaryIss(const _Value: TList<TKMLBoundary>);
    function GetInnerBoundaryIs(Index: Integer): TKMLBoundary;
    procedure SetInnerBoundaryIs(Index: Integer; const _Value: TKMLBoundary);
    procedure SetTessellate(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddExtrudeEvent(Sender: TObject);
    procedure AddAltitudeModeEvent(Sender: TObject);
    procedure AddOuterBoundaryIsEvent(Sender: TObject);
    procedure AddInnerBoundaryIsEvent(Sender: TObject);
    procedure AddTessellateEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    function AddOuterBoundaryIs: TKMLBoundary;
    procedure OuterBoundaryIsClear;
    function OuterBoundaryIsCount: Integer;
    procedure RemoveOuterBoundaryIs(_Value: TKMLBoundary);
    procedure DeleteOuterBoundaryIs(Index: Integer);
    function AddInnerBoundaryIs: TKMLBoundary;
    procedure InnerBoundaryIsClear;
    function InnerBoundaryIsCount: Integer;
    procedure RemoveInnerBoundaryIs(_Value: TKMLBoundary);
    procedure DeleteInnerBoundaryIs(Index: Integer);
    function AddTessellate: String;
    procedure TessellateRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Extrude: String read FExtrude write SetExtrude;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property OuterBoundaryIss: TList<TKMLBoundary> read FOuterBoundaryIss
      write SetOuterBoundaryIss;
    property OuterBoundaryIs[Index: Integer]: TKMLBoundary
      read GetOuterBoundaryIs write SetOuterBoundaryIs;
    property InnerBoundaryIss: TList<TKMLBoundary> read FInnerBoundaryIss
      write SetInnerBoundaryIss;
    property InnerBoundaryIs[Index: Integer]: TKMLBoundary
      read GetInnerBoundaryIs write SetInnerBoundaryIs;
    property Tessellate: String read FTessellate write SetTessellate;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ExtrudeExsit: Boolean read FExtrudeExsit;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit;
    property TessellateExsit: Boolean read FTessellateExsit;
  end;

  TKMLBoundary = class(TXML)
  private
    FLinearRing: TKMLLinearRing;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLinearRing(const _Value: TKMLLinearRing);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property LinearRing: TKMLLinearRing read FLinearRing write SetLinearRing;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLLinearRing = class(TKMLGeometry)
  private
    FCoordinates: ArrayCoordinates;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Coordinates: ArrayCoordinates read FCoordinates
      write SetCoordinates;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLLineString = class(TKMLGeometry)
  private
    FTessellate: String;
    FTessellateExsit: Boolean;
    FAltitudeMode: String;
    FAltitudeModeExsit: Boolean;
    FExtrude: String;
    FExtrudeExsit: Boolean;
    FCoordinates: ArrayCoordinates;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetTessellate(const _Value: String);
    procedure SetAltitudeMode(const _Value: String);
    procedure SetExtrude(const _Value: String);
    procedure SetCoordinates(const _Value: ArrayCoordinates);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddTessellateEvent(Sender: TObject);
    procedure AddAltitudeModeEvent(Sender: TObject);
    procedure AddExtrudeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddTessellate: String;
    procedure TessellateRemove;
    function AddAltitudeMode: String;
    procedure AltitudeModeRemove;
    function AddExtrude: String;
    procedure ExtrudeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Tessellate: String read FTessellate write SetTessellate;
    property AltitudeMode: String read FAltitudeMode write SetAltitudeMode;
    property Extrude: String read FExtrude write SetExtrude;
    property Coordinates: ArrayCoordinates read FCoordinates
      write SetCoordinates;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property TessellateExsit: Boolean read FTessellateExsit;
    property AltitudeModeExsit: Boolean read FAltitudeModeExsit;
    property ExtrudeExsit: Boolean read FExtrudeExsit;
  end;

  TKMLLookAt = class(TXML)
  private
    FLongitude: Double;
    FLongitudeExsit: Boolean;
    FLatitude: Double;
    FLatitudeExsit: Boolean;
    FAltitude: Double;
    FAltitudeExsit: Boolean;
    FHeading: Double;
    FHeadingExsit: Boolean;
    FTilt: Double;
    FTiltExsit: Boolean;
    FRange: Double;
    FRangeExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLongitude(const _Value: Double);
    procedure SetLatitude(const _Value: Double);
    procedure SetAltitude(const _Value: Double);
    procedure SetHeading(const _Value: Double);
    procedure SetTilt(const _Value: Double);
    procedure SetRange(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddLongitudeEvent(Sender: TObject);
    procedure AddLatitudeEvent(Sender: TObject);
    procedure AddAltitudeEvent(Sender: TObject);
    procedure AddHeadingEvent(Sender: TObject);
    procedure AddTiltEvent(Sender: TObject);
    procedure AddRangeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddLongitude: Double;
    procedure LongitudeRemove;
    function AddLatitude: Double;
    procedure LatitudeRemove;
    function AddAltitude: Double;
    procedure AltitudeRemove;
    function AddHeading: Double;
    procedure HeadingRemove;
    function AddTilt: Double;
    procedure TiltRemove;
    function AddRange: Double;
    procedure RangeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Longitude: Double read FLongitude write SetLongitude;
    property Latitude: Double read FLatitude write SetLatitude;
    property Altitude: Double read FAltitude write SetAltitude;
    property Heading: Double read FHeading write SetHeading;
    property Tilt: Double read FTilt write SetTilt;
    property Range: Double read FRange write SetRange;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property LongitudeExsit: Boolean read FLongitudeExsit;
    property LatitudeExsit: Boolean read FLatitudeExsit;
    property AltitudeExsit: Boolean read FAltitudeExsit;
    property HeadingExsit: Boolean read FHeadingExsit;
    property TiltExsit: Boolean read FTiltExsit;
    property RangeExsit: Boolean read FRangeExsit;
  end;

  TKMLLatLonBox = class(TXML)
  private
    FNorth: Double;
    FSouth: Double;
    FEast: Double;
    FWest: Double;
    FRotation: Double;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetNorth(const _Value: Double);
    procedure SetSouth(const _Value: Double);
    procedure SetEast(const _Value: Double);
    procedure SetWest(const _Value: Double);
    procedure SetRotation(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property North: Double read FNorth write SetNorth;
    property South: Double read FSouth write SetSouth;
    property East: Double read FEast write SetEast;
    property West: Double read FWest write SetWest;
    property Rotation: Double read FRotation write SetRotation;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLGroundOverlay = class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FLookAt: TKMLLookAt;
    FLookAtExsit: Boolean;
    FIcon: TKMLIcon;
    FIconExsit: Boolean;
    FLatLonBox: TKMLLatLonBox;
    FLatLonBoxExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetVisibility(const _Value: Integer);
    procedure SetDescription(const _Value: String);
    procedure SetLookAt(const _Value: TKMLLookAt);
    procedure SetIcon(const _Value: TKMLIcon);
    procedure SetLatLonBox(const _Value: TKMLLatLonBox);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddVisibilityEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddLookAtEvent(Sender: TObject);
    procedure AddIconEvent(Sender: TObject);
    procedure AddLatLonBoxEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddLookAt: TKMLLookAt;
    procedure LookAtRemove;
    function AddIcon: TKMLIcon;
    procedure IconRemove;
    function AddLatLonBox: TKMLLatLonBox;
    procedure LatLonBoxRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Visibility: Integer read FVisibility write SetVisibility;
    property Description: String read FDescription write SetDescription;
    property LookAt: TKMLLookAt read FLookAt write SetLookAt;
    property Icon: TKMLIcon read FIcon write SetIcon;
    property LatLonBox: TKMLLatLonBox read FLatLonBox write SetLatLonBox;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property VisibilityExsit: Boolean read FVisibilityExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property LookAtExsit: Boolean read FLookAtExsit;
    property IconExsit: Boolean read FIconExsit;
    property LatLonBoxExsit: Boolean read FLatLonBoxExsit;
  end;

  TKMLExtendedData = class(TXML)
  private
    FDatas: TList<TKMLData>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetDatas(const _Value: TList<TKMLData>);
    function GetData(Index: Integer): TKMLData;
    procedure SetData(Index: Integer; const _Value: TKMLData);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddDataEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddData: TKMLData;
    procedure DataClear;
    function DataCount: Integer;
    procedure RemoveData(_Value: TKMLData);
    procedure DeleteData(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Datas: TList<TKMLData> read FDatas write SetDatas;
    property Data[Index: Integer]: TKMLData read GetData write SetData;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLData = class(TXML)
  private
    FName: String;
    FValue: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetValue(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLSize = class(TXML)
  private
    FX: Double;
    FY: Double;
    FXUnits: String;
    FYUnits: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetX(const _Value: Double);
    procedure SetY(const _Value: Double);
    procedure SetXUnits(const _Value: String);
    procedure SetYUnits(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property XUnits: String read FXUnits write SetXUnits;
    property YUnits: String read FYUnits write SetYUnits;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLStyle = class(TXML)
  private
    FID: String;
    FIconStyle: TKMLIconStyle;
    FIconStyleExsit: Boolean;
    FLineStyle: TKMLLineStyle;
    FLineStyleExsit: Boolean;
    FPolyStyle: TKMLLineStyle;
    FPolyStyleExsit: Boolean;
    FBalloonStyle: TKMLBalloonStyle;
    FBalloonStyleExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetID(const _Value: String);
    procedure SetIconStyle(const _Value: TKMLIconStyle);
    procedure SetLineStyle(const _Value: TKMLLineStyle);
    procedure SetPolyStyle(const _Value: TKMLLineStyle);
    procedure SetBalloonStyle(const _Value: TKMLBalloonStyle);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddIconStyleEvent(Sender: TObject);
    procedure AddLineStyleEvent(Sender: TObject);
    procedure AddPolyStyleEvent(Sender: TObject);
    procedure AddBalloonStyleEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddIconStyle: TKMLIconStyle;
    procedure IconStyleRemove;
    function AddLineStyle: TKMLLineStyle;
    procedure LineStyleRemove;
    function AddPolyStyle: TKMLLineStyle;
    procedure PolyStyleRemove;
    function AddBalloonStyle: TKMLBalloonStyle;
    procedure BalloonStyleRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property ID: String read FID write SetID;
    property IconStyle: TKMLIconStyle read FIconStyle write SetIconStyle;
    property LineStyle: TKMLLineStyle read FLineStyle write SetLineStyle;
    property PolyStyle: TKMLLineStyle read FPolyStyle write SetPolyStyle;
    property BalloonStyle: TKMLBalloonStyle read FBalloonStyle
      write SetBalloonStyle;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property IconStyleExsit: Boolean read FIconStyleExsit;
    property LineStyleExsit: Boolean read FLineStyleExsit;
    property PolyStyleExsit: Boolean read FPolyStyleExsit;
    property BalloonStyleExsit: Boolean read FBalloonStyleExsit;
  end;

  TKMLIconStyle = class(TXML)
  private
    FIcon: TKMLIcon;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetIcon(const _Value: TKMLIcon);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Icon: TKMLIcon read FIcon write SetIcon;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLIcon = class(TXML)
  private
    Fhref: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure Sethref(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property href: String read Fhref write Sethref;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLLineStyle = class(TXML)
  private
    Fcolor: String;
    FcolorExsit: Boolean;
    Fwidth: Double;
    FwidthExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setcolor(const _Value: String);
    procedure Setwidth(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddcolorEvent(Sender: TObject);
    procedure AddwidthEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addcolor: String;
    procedure colorRemove;
    function Addwidth: Double;
    procedure widthRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property color: String read Fcolor write Setcolor;
    property width: Double read Fwidth write Setwidth;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property colorExsit: Boolean read FcolorExsit;
    property widthExsit: Boolean read FwidthExsit;
  end;

  TKMLBalloonStyle = class(TXML)
  private
    Ftext: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure Settext(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property text: String read Ftext write Settext;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLStyleMap = class(TXML)
  private
    FPairs: TList<TKMLPair>;
    FID: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetPairs(const _Value: TList<TKMLPair>);
    function GetPair(Index: Integer): TKMLPair;
    procedure SetPair(Index: Integer; const _Value: TKMLPair);
    procedure SetID(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddPairEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddPair: TKMLPair;
    procedure PairClear;
    function PairCount: Integer;
    procedure RemovePair(_Value: TKMLPair);
    procedure DeletePair(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Pairs: TList<TKMLPair> read FPairs write SetPairs;
    property Pair[Index: Integer]: TKMLPair read GetPair write SetPair;
    property ID: String read FID write SetID;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TKMLScreenOverlay = class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FVisibility: Integer;
    FVisibilityExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FIcon: TKMLIcon;
    FIconExsit: Boolean;
    FOverlayXY: TKMLSize;
    FOverlayXYExsit: Boolean;
    FScreenXY: TKMLSize;
    FScreenXYExsit: Boolean;
    FRotationXY: TKMLSize;
    FRotationXYExsit: Boolean;
    FSize: TKMLSize;
    FSizeExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetVisibility(const _Value: Integer);
    procedure SetDescription(const _Value: String);
    procedure SetIcon(const _Value: TKMLIcon);
    procedure SetOverlayXY(const _Value: TKMLSize);
    procedure SetScreenXY(const _Value: TKMLSize);
    procedure SetRotationXY(const _Value: TKMLSize);
    procedure SetSize(const _Value: TKMLSize);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddVisibilityEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddIconEvent(Sender: TObject);
    procedure AddOverlayXYEvent(Sender: TObject);
    procedure AddScreenXYEvent(Sender: TObject);
    procedure AddRotationXYEvent(Sender: TObject);
    procedure AddSizeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddVisibility: Integer;
    procedure VisibilityRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddIcon: TKMLIcon;
    procedure IconRemove;
    function AddOverlayXY: TKMLSize;
    procedure OverlayXYRemove;
    function AddScreenXY: TKMLSize;
    procedure ScreenXYRemove;
    function AddRotationXY: TKMLSize;
    procedure RotationXYRemove;
    function AddSize: TKMLSize;
    procedure SizeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Visibility: Integer read FVisibility write SetVisibility;
    property Description: String read FDescription write SetDescription;
    property Icon: TKMLIcon read FIcon write SetIcon;
    property OverlayXY: TKMLSize read FOverlayXY write SetOverlayXY;
    property ScreenXY: TKMLSize read FScreenXY write SetScreenXY;
    property RotationXY: TKMLSize read FRotationXY write SetRotationXY;
    property Size: TKMLSize read FSize write SetSize;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property VisibilityExsit: Boolean read FVisibilityExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property IconExsit: Boolean read FIconExsit;
    property OverlayXYExsit: Boolean read FOverlayXYExsit;
    property ScreenXYExsit: Boolean read FScreenXYExsit;
    property RotationXYExsit: Boolean read FRotationXYExsit;
    property SizeExsit: Boolean read FSizeExsit;
  end;

  TKMLPair = class(TXML)
  private
    FKey: String;
    FKeyExsit: Boolean;
    FStyleUrl: String;
    FStyleUrlExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetKey(const _Value: String);
    procedure SetStyleUrl(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddKeyEvent(Sender: TObject);
    procedure AddStyleUrlEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddKey: String;
    procedure KeyRemove;
    function AddStyleUrl: String;
    procedure StyleUrlRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Key: String read FKey write SetKey;
    property StyleUrl: String read FStyleUrl write SetStyleUrl;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property KeyExsit: Boolean read FKeyExsit;
    property StyleUrlExsit: Boolean read FStyleUrlExsit;
  end;

var
  TKMLPop: TPopupMenu;
  TKMLXMLInspector: TXMLInspector;
  TKMLTreeComponent: TTree;
  TKMLDocumentPop: TPopupMenu;
  TKMLDocumentXMLInspector: TXMLInspector;
  TKMLDocumentTreeComponent: TTree;
  TKMLFolderPop: TPopupMenu;
  TKMLFolderXMLInspector: TXMLInspector;
  TKMLFolderTreeComponent: TTree;
  TKMLPlacemarkPop: TPopupMenu;
  TKMLPlacemarkXMLInspector: TXMLInspector;
  TKMLPlacemarkTreeComponent: TTree;
  TKMLGeometryPop: TPopupMenu;
  TKMLGeometryXMLInspector: TXMLInspector;
  TKMLGeometryTreeComponent: TTree;
  TKMLMultiGeometryPop: TPopupMenu;
  TKMLMultiGeometryXMLInspector: TXMLInspector;
  TKMLMultiGeometryTreeComponent: TTree;
  TKMLPointPop: TPopupMenu;
  TKMLPointXMLInspector: TXMLInspector;
  TKMLPointTreeComponent: TTree;
  TKMLPolygonPop: TPopupMenu;
  TKMLPolygonXMLInspector: TXMLInspector;
  TKMLPolygonTreeComponent: TTree;
  TKMLBoundaryPop: TPopupMenu;
  TKMLBoundaryXMLInspector: TXMLInspector;
  TKMLBoundaryTreeComponent: TTree;
  TKMLLinearRingPop: TPopupMenu;
  TKMLLinearRingXMLInspector: TXMLInspector;
  TKMLLinearRingTreeComponent: TTree;
  TKMLLineStringPop: TPopupMenu;
  TKMLLineStringXMLInspector: TXMLInspector;
  TKMLLineStringTreeComponent: TTree;
  TKMLLookAtPop: TPopupMenu;
  TKMLLookAtXMLInspector: TXMLInspector;
  TKMLLookAtTreeComponent: TTree;
  TKMLLatLonBoxPop: TPopupMenu;
  TKMLLatLonBoxXMLInspector: TXMLInspector;
  TKMLLatLonBoxTreeComponent: TTree;
  TKMLGroundOverlayPop: TPopupMenu;
  TKMLGroundOverlayXMLInspector: TXMLInspector;
  TKMLGroundOverlayTreeComponent: TTree;
  TKMLExtendedDataPop: TPopupMenu;
  TKMLExtendedDataXMLInspector: TXMLInspector;
  TKMLExtendedDataTreeComponent: TTree;
  TKMLDataPop: TPopupMenu;
  TKMLDataXMLInspector: TXMLInspector;
  TKMLDataTreeComponent: TTree;
  TKMLSizePop: TPopupMenu;
  TKMLSizeXMLInspector: TXMLInspector;
  TKMLSizeTreeComponent: TTree;
  TKMLStylePop: TPopupMenu;
  TKMLStyleXMLInspector: TXMLInspector;
  TKMLStyleTreeComponent: TTree;
  TKMLIconStylePop: TPopupMenu;
  TKMLIconStyleXMLInspector: TXMLInspector;
  TKMLIconStyleTreeComponent: TTree;
  TKMLIconPop: TPopupMenu;
  TKMLIconXMLInspector: TXMLInspector;
  TKMLIconTreeComponent: TTree;
  TKMLLineStylePop: TPopupMenu;
  TKMLLineStyleXMLInspector: TXMLInspector;
  TKMLLineStyleTreeComponent: TTree;
  TKMLBalloonStylePop: TPopupMenu;
  TKMLBalloonStyleXMLInspector: TXMLInspector;
  TKMLBalloonStyleTreeComponent: TTree;
  TKMLStyleMapPop: TPopupMenu;
  TKMLStyleMapXMLInspector: TXMLInspector;
  TKMLStyleMapTreeComponent: TTree;
  TKMLScreenOverlayPop: TPopupMenu;
  TKMLScreenOverlayXMLInspector: TXMLInspector;
  TKMLScreenOverlayTreeComponent: TTree;
  TKMLPairPop: TPopupMenu;
  TKMLPairXMLInspector: TXMLInspector;
  TKMLPairTreeComponent: TTree;
  KMLObject: TObject;

implementation

{ kml }
constructor TKML.Create(par: TXML = nil);
begin
  inherited Create(par);
  FDocument := TKMLDocument.Create(Self);
end;

destructor TKML.Destroy;
begin
  FDocument.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKML.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Document' then
      begin
        FDocument := TKMLDocument.Create(Self);
        FDocument.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'xmlns' then
      begin
        FXmlns := nodeTmp.text;
        FXmlnsExsit := True;
      end;
    end;
  except
    raise Exception.Create('kml Read XML Error!' + node.Xml);
  end;
end;

function TKML.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  XmlnsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'kml';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FDocument.ToXML(node, 'Document');
    if FXmlnsExsit then
    begin
      XmlnsTmp := doc.CreateNode('xmlns', ntAttribute);
      XmlnsTmp.NodeValue := FXmlns;
      node.AttributeNodes.Add(XmlnsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKML.ToTree;
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

procedure TKML.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  XmlnsAddMenu: TMenuItem;
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
      pt := TPointF.Create(X, Y);
      pt := TKMLTreeComponent.ClientToScreen(pt);
      TKMLPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKML.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Xmlns');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Xmlns);
  TKMLXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKML.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Xmlns := _Value;
      end;
  end;
  ToTree;
end;

function TKML.AddXmlns: String;
begin;
  Result := FXmlns;
  FXmlnsExsit := True;
end;

procedure TKML.SetXmlns(const _Value: String);
begin
  FXmlnsExsit := True;
  FXmlns := _Value;
end;

procedure TKML.XmlnsRemove;
begin
  if FXmlnsExsit then
  begin
    FXmlnsExsit := False;
  end;
end;

procedure TKML.AddXmlnsEvent(Sender: TObject);
begin
  AddXmlns;
end;

procedure TKML.SetDocument(const _Value: TKMLDocument);
begin
  FDocument.Free;
  FDocument := _Value;
  FDocument.Parent := Self;
end;

{ Document }
constructor TKMLDocument.Create(par: TXML = nil);
begin
  inherited Create(par);
  FStyleMaps := TList<TKMLStyleMap>.Create;
  FStyles := TList<TKMLStyle>.Create;
  FPlacemarks := TList<TKMLPlacemark>.Create;
  FFolders := TList<TKMLFolder>.Create;
end;

destructor TKMLDocument.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  StyleMapClear;
  FStyleMaps.Free;
  StyleClear;
  FStyles.Free;
  PlacemarkClear;
  FPlacemarks.Free;
  FolderClear;
  FFolders.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLDocument.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  StyleMapTmp: TKMLStyleMap;
  StyleTmp: TKMLStyle;
  PlacemarkTmp: TKMLPlacemark;
  FolderTmp: TKMLFolder;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'open' then
      begin
        FOpen := nodeTmp.text.ToInteger;
        FOpenExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        FVisibility := nodeTmp.text.ToInteger;
        FVisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        FDescription := nodeTmp.text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        FLookAt := TKMLLookAt.Create(Self);
        FLookAt.FromXML(nodeTmp);
        FLookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'StyleMap' then
      begin
        StyleMapTmp := TKMLStyleMap.Create(Self);
        StyleMapTmp.FromXML(nodeTmp);
        FStyleMaps.Add(StyleMapTmp);
      end
      else if nodeTmp.NodeName = 'Style' then
      begin
        StyleTmp := TKMLStyle.Create(Self);
        StyleTmp.FromXML(nodeTmp);
        FStyles.Add(StyleTmp);
      end
      else if nodeTmp.NodeName = 'Placemark' then
      begin
        PlacemarkTmp := TKMLPlacemark.Create(Self);
        PlacemarkTmp.FromXML(nodeTmp);
        FPlacemarks.Add(PlacemarkTmp);
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TKMLFolder.Create(Self);
        FolderTmp.FromXML(nodeTmp);
        FFolders.Add(FolderTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Document Read XML Error!' + node.Xml);
  end;
end;

function TKMLDocument.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  OpenTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Document';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FOpenExsit then
    begin
      OpenTmp := doc.CreateNode('open', ntElement);
      OpenTmp.NodeValue := FOpen.toString;
      node.ChildNodes.Add(OpenTmp);
    end;
    if FVisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := FVisibility.toString;
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FLookAtExsit then
      FLookAt.ToXML(node, 'LookAt');
    for I := 0 to FStyleMaps.Count - 1 do
      FStyleMaps.Items[I].ToXML(node, 'StyleMap');
    for I := 0 to FStyles.Count - 1 do
      FStyles.Items[I].ToXML(node, 'Style');
    for I := 0 to FPlacemarks.Count - 1 do
      FPlacemarks.Items[I].ToXML(node, 'Placemark');
    for I := 0 to FFolders.Count - 1 do
      FFolders.Items[I].ToXML(node, 'Folder');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLDocument.ToTree;
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
    Placemarks[I].TreeNodeShape := TreeNodeShape.AddChildObject('Placemark',
      Placemark[I]);
    Placemark[I].ToTree;
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder',
      Folder[I]);
    Folder[I].ToTree;
  end;
end;

procedure TKMLDocument.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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
      pt := TPointF.Create(X, Y);
      pt := TKMLDocumentTreeComponent.ClientToScreen(pt);
      TKMLDocumentPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLDocument.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLDocumentXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Open');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Open.toString);
  Names_Value.Add('Visibility');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Visibility.toString);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TKMLDocumentXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLDocument.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Open := _Value.ToInteger;
      end;
    2:
      begin
        Visibility := _Value.ToInteger;
      end;
    3:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

function TKMLDocument.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLDocument.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLDocument.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLDocument.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TKMLDocument.AddOpen: Integer;
begin;
  Result := FOpen;
  FOpenExsit := True;
end;

procedure TKMLDocument.SetOpen(const _Value: Integer);
begin
  FOpenExsit := True;
  FOpen := _Value;
end;

procedure TKMLDocument.OpenRemove;
begin
  if FOpenExsit then
  begin
    FOpenExsit := False;
  end;
end;

procedure TKMLDocument.AddOpenEvent(Sender: TObject);
begin
  AddOpen;
end;

function TKMLDocument.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLDocument.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLDocument.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLDocument.AddVisibilityEvent(Sender: TObject);
begin
  AddVisibility;
end;

function TKMLDocument.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLDocument.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLDocument.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLDocument.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TKMLDocument.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create(Self);
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLDocument.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
  FLookAt.Parent := Self;
end;

procedure TKMLDocument.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLDocument.AddLookAtEvent(Sender: TObject);
begin
  AddLookAt;
  FLookAt.ToTree;
end;

function TKMLDocument.AddStyleMap: TKMLStyleMap;
var
  StyleMapTmp: TKMLStyleMap;
begin;
  StyleMapTmp := TKMLStyleMap.Create(Self);
  FStyleMaps.Add(StyleMapTmp);
  Result := StyleMapTmp;
end;

procedure TKMLDocument.SetStyleMaps(const _Value: TList<TKMLStyleMap>);
begin
  StyleMapClear;
  FStyleMaps := _Value;
end;

procedure TKMLDocument.StyleMapClear;
begin
  while FStyleMaps.Count > 0 do
  begin
    FStyleMaps.Items[0].Free;
    FStyleMaps.Delete(0);
  end;
end;

function TKMLDocument.StyleMapCount: Integer;
begin
  Result := FStyleMaps.Count;
end;

function TKMLDocument.GetStyleMap(Index: Integer): TKMLStyleMap;
begin
  Result := FStyleMaps[Index];
end;

procedure TKMLDocument.SetStyleMap(Index: Integer; const _Value: TKMLStyleMap);
begin
  _Value.Parent := Self;
  FStyleMaps[Index].Free;
  FStyleMaps[Index] := _Value;
end;

procedure TKMLDocument.RemoveStyleMap(_Value: TKMLStyleMap);
begin
  FStyleMaps.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteStyleMap(Index: Integer);
begin
  FStyleMaps.Items[Index].Free;
  FStyleMaps.Delete(Index);
end;

procedure TKMLDocument.AddStyleMapEvent(Sender: TObject);
var
  tmp: TKMLStyleMap;
begin
  tmp := AddStyleMap;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('StyleMap', tmp);
  tmp.ToTree;
end;

function TKMLDocument.AddStyle: TKMLStyle;
var
  StyleTmp: TKMLStyle;
begin;
  StyleTmp := TKMLStyle.Create(Self);
  FStyles.Add(StyleTmp);
  Result := StyleTmp;
end;

procedure TKMLDocument.SetStyles(const _Value: TList<TKMLStyle>);
begin
  StyleClear;
  FStyles := _Value;
end;

procedure TKMLDocument.StyleClear;
begin
  while FStyles.Count > 0 do
  begin
    FStyles.Items[0].Free;
    FStyles.Delete(0);
  end;
end;

function TKMLDocument.StyleCount: Integer;
begin
  Result := FStyles.Count;
end;

function TKMLDocument.GetStyle(Index: Integer): TKMLStyle;
begin
  Result := FStyles[Index];
end;

procedure TKMLDocument.SetStyle(Index: Integer; const _Value: TKMLStyle);
begin
  _Value.Parent := Self;
  FStyles[Index].Free;
  FStyles[Index] := _Value;
end;

procedure TKMLDocument.RemoveStyle(_Value: TKMLStyle);
begin
  FStyles.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteStyle(Index: Integer);
begin
  FStyles.Items[Index].Free;
  FStyles.Delete(Index);
end;

procedure TKMLDocument.AddStyleEvent(Sender: TObject);
var
  tmp: TKMLStyle;
begin
  tmp := AddStyle;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Style', tmp);
  tmp.ToTree;
end;

function TKMLDocument.AddPlacemark: TKMLPlacemark;
var
  PlacemarkTmp: TKMLPlacemark;
begin;
  PlacemarkTmp := TKMLPlacemark.Create(Self);
  FPlacemarks.Add(PlacemarkTmp);
  Result := PlacemarkTmp;
end;

procedure TKMLDocument.SetPlacemarks(const _Value: TList<TKMLPlacemark>);
begin
  PlacemarkClear;
  FPlacemarks := _Value;
end;

procedure TKMLDocument.PlacemarkClear;
begin
  while FPlacemarks.Count > 0 do
  begin
    FPlacemarks.Items[0].Free;
    FPlacemarks.Delete(0);
  end;
end;

function TKMLDocument.PlacemarkCount: Integer;
begin
  Result := FPlacemarks.Count;
end;

function TKMLDocument.GetPlacemark(Index: Integer): TKMLPlacemark;
begin
  Result := FPlacemarks[Index];
end;

procedure TKMLDocument.SetPlacemark(Index: Integer;
  const _Value: TKMLPlacemark);
begin
  _Value.Parent := Self;
  FPlacemarks[Index].Free;
  FPlacemarks[Index] := _Value;
end;

procedure TKMLDocument.RemovePlacemark(_Value: TKMLPlacemark);
begin
  FPlacemarks.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeletePlacemark(Index: Integer);
begin
  FPlacemarks.Items[Index].Free;
  FPlacemarks.Delete(Index);
end;

procedure TKMLDocument.AddPlacemarkEvent(Sender: TObject);
var
  tmp: TKMLPlacemark;
begin
  tmp := AddPlacemark;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Placemark', tmp);
  tmp.ToTree;
end;

function TKMLDocument.AddFolder: TKMLFolder;
var
  FolderTmp: TKMLFolder;
begin;
  FolderTmp := TKMLFolder.Create(Self);
  FFolders.Add(FolderTmp);
  Result := FolderTmp;
end;

procedure TKMLDocument.SetFolders(const _Value: TList<TKMLFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

procedure TKMLDocument.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TKMLDocument.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

function TKMLDocument.GetFolder(Index: Integer): TKMLFolder;
begin
  Result := FFolders[Index];
end;

procedure TKMLDocument.SetFolder(Index: Integer; const _Value: TKMLFolder);
begin
  _Value.Parent := Self;
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TKMLDocument.RemoveFolder(_Value: TKMLFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TKMLDocument.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

procedure TKMLDocument.AddFolderEvent(Sender: TObject);
var
  tmp: TKMLFolder;
begin
  tmp := AddFolder;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Folder', tmp);
  tmp.ToTree;
end;

{ Folder }
constructor TKMLFolder.Create(par: TXML = nil);
begin
  inherited Create(par);
  FGroundOverlays := TList<TKMLGroundOverlay>.Create;
  FPlacemarks := TList<TKMLPlacemark>.Create;
  FScreenOverlays := TList<TKMLScreenOverlay>.Create;
  FFolders := TList<TKMLFolder>.Create;
end;

destructor TKMLFolder.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FDocumentExsit then
    FDocument.Free;
  GroundOverlayClear;
  FGroundOverlays.Free;
  PlacemarkClear;
  FPlacemarks.Free;
  ScreenOverlayClear;
  FScreenOverlays.Free;
  FolderClear;
  FFolders.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLFolder.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  GroundOverlayTmp: TKMLGroundOverlay;
  PlacemarkTmp: TKMLPlacemark;
  ScreenOverlayTmp: TKMLScreenOverlay;
  FolderTmp: TKMLFolder;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'open' then
      begin
        FOpen := nodeTmp.text.ToInteger;
        FOpenExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        FVisibility := nodeTmp.text.ToInteger;
        FVisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        FDescription := nodeTmp.text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        FLookAt := TKMLLookAt.Create(Self);
        FLookAt.FromXML(nodeTmp);
        FLookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'Document' then
      begin
        FDocument := TKMLDocument.Create(Self);
        FDocument.FromXML(nodeTmp);
        FDocumentExsit := True;
      end
      else if nodeTmp.NodeName = 'GroundOverlay' then
      begin
        GroundOverlayTmp := TKMLGroundOverlay.Create(Self);
        GroundOverlayTmp.FromXML(nodeTmp);
        FGroundOverlays.Add(GroundOverlayTmp);
      end
      else if nodeTmp.NodeName = 'Placemark' then
      begin
        PlacemarkTmp := TKMLPlacemark.Create(Self);
        PlacemarkTmp.FromXML(nodeTmp);
        FPlacemarks.Add(PlacemarkTmp);
      end
      else if nodeTmp.NodeName = 'ScreenOverlay' then
      begin
        ScreenOverlayTmp := TKMLScreenOverlay.Create(Self);
        ScreenOverlayTmp.FromXML(nodeTmp);
        FScreenOverlays.Add(ScreenOverlayTmp);
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TKMLFolder.Create(Self);
        FolderTmp.FromXML(nodeTmp);
        FFolders.Add(FolderTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Folder Read XML Error!' + node.Xml);
  end;
end;

function TKMLFolder.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  OpenTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Folder';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FOpenExsit then
    begin
      OpenTmp := doc.CreateNode('open', ntElement);
      OpenTmp.NodeValue := FOpen.toString;
      node.ChildNodes.Add(OpenTmp);
    end;
    if FVisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := FVisibility.toString;
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FLookAtExsit then
      FLookAt.ToXML(node, 'LookAt');
    if FDocumentExsit then
      FDocument.ToXML(node, 'Document');
    for I := 0 to FGroundOverlays.Count - 1 do
      FGroundOverlays.Items[I].ToXML(node, 'GroundOverlay');
    for I := 0 to FPlacemarks.Count - 1 do
      FPlacemarks.Items[I].ToXML(node, 'Placemark');
    for I := 0 to FScreenOverlays.Count - 1 do
      FScreenOverlays.Items[I].ToXML(node, 'ScreenOverlay');
    for I := 0 to FFolders.Count - 1 do
      FFolders.Items[I].ToXML(node, 'Folder');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLFolder.ToTree;
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
    Placemarks[I].TreeNodeShape := TreeNodeShape.AddChildObject('Placemark',
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
    Folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder',
      Folder[I]);
    Folder[I].ToTree;
  end;
end;

procedure TKMLFolder.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
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
      pt := TPointF.Create(X, Y);
      pt := TKMLFolderTreeComponent.ClientToScreen(pt);
      TKMLFolderPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLFolder.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLFolderXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Open');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Open.toString);
  Names_Value.Add('Visibility');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Visibility.toString);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TKMLFolderXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLFolder.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Open := _Value.ToInteger;
      end;
    2:
      begin
        Visibility := _Value.ToInteger;
      end;
    3:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

function TKMLFolder.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLFolder.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLFolder.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLFolder.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TKMLFolder.AddOpen: Integer;
begin;
  Result := FOpen;
  FOpenExsit := True;
end;

procedure TKMLFolder.SetOpen(const _Value: Integer);
begin
  FOpenExsit := True;
  FOpen := _Value;
end;

procedure TKMLFolder.OpenRemove;
begin
  if FOpenExsit then
  begin
    FOpenExsit := False;
  end;
end;

procedure TKMLFolder.AddOpenEvent(Sender: TObject);
begin
  AddOpen;
end;

function TKMLFolder.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLFolder.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLFolder.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLFolder.AddVisibilityEvent(Sender: TObject);
begin
  AddVisibility;
end;

function TKMLFolder.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLFolder.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLFolder.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLFolder.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TKMLFolder.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create(Self);
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLFolder.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
  FLookAt.Parent := Self;
end;

procedure TKMLFolder.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLFolder.AddLookAtEvent(Sender: TObject);
begin
  AddLookAt;
  FLookAt.ToTree;
end;

function TKMLFolder.AddDocument: TKMLDocument;
begin;
  if not FDocumentExsit then
    FDocument := TKMLDocument.Create(Self);
  Result := FDocument;
  FDocumentExsit := True;
end;

procedure TKMLFolder.SetDocument(const _Value: TKMLDocument);
begin
  if FDocumentExsit then
    FDocument.Free;
  FDocumentExsit := True;
  FDocument := _Value;
  FDocument.Parent := Self;
end;

procedure TKMLFolder.DocumentRemove;
begin
  if FDocumentExsit then
  begin
    FDocument.Free;
    FDocumentExsit := False;
  end;
end;

procedure TKMLFolder.AddDocumentEvent(Sender: TObject);
begin
  AddDocument;
  FDocument.ToTree;
end;

function TKMLFolder.AddGroundOverlay: TKMLGroundOverlay;
var
  GroundOverlayTmp: TKMLGroundOverlay;
begin;
  GroundOverlayTmp := TKMLGroundOverlay.Create(Self);
  FGroundOverlays.Add(GroundOverlayTmp);
  Result := GroundOverlayTmp;
end;

procedure TKMLFolder.SetGroundOverlays(const _Value: TList<TKMLGroundOverlay>);
begin
  GroundOverlayClear;
  FGroundOverlays := _Value;
end;

procedure TKMLFolder.GroundOverlayClear;
begin
  while FGroundOverlays.Count > 0 do
  begin
    FGroundOverlays.Items[0].Free;
    FGroundOverlays.Delete(0);
  end;
end;

function TKMLFolder.GroundOverlayCount: Integer;
begin
  Result := FGroundOverlays.Count;
end;

function TKMLFolder.GetGroundOverlay(Index: Integer): TKMLGroundOverlay;
begin
  Result := FGroundOverlays[Index];
end;

procedure TKMLFolder.SetGroundOverlay(Index: Integer;
  const _Value: TKMLGroundOverlay);
begin
  _Value.Parent := Self;
  FGroundOverlays[Index].Free;
  FGroundOverlays[Index] := _Value;
end;

procedure TKMLFolder.RemoveGroundOverlay(_Value: TKMLGroundOverlay);
begin
  FGroundOverlays.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteGroundOverlay(Index: Integer);
begin
  FGroundOverlays.Items[Index].Free;
  FGroundOverlays.Delete(Index);
end;

procedure TKMLFolder.AddGroundOverlayEvent(Sender: TObject);
var
  tmp: TKMLGroundOverlay;
begin
  tmp := AddGroundOverlay;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('GroundOverlay', tmp);
  tmp.ToTree;
end;

function TKMLFolder.AddPlacemark: TKMLPlacemark;
var
  PlacemarkTmp: TKMLPlacemark;
begin;
  PlacemarkTmp := TKMLPlacemark.Create(Self);
  FPlacemarks.Add(PlacemarkTmp);
  Result := PlacemarkTmp;
end;

procedure TKMLFolder.SetPlacemarks(const _Value: TList<TKMLPlacemark>);
begin
  PlacemarkClear;
  FPlacemarks := _Value;
end;

procedure TKMLFolder.PlacemarkClear;
begin
  while FPlacemarks.Count > 0 do
  begin
    FPlacemarks.Items[0].Free;
    FPlacemarks.Delete(0);
  end;
end;

function TKMLFolder.PlacemarkCount: Integer;
begin
  Result := FPlacemarks.Count;
end;

function TKMLFolder.GetPlacemark(Index: Integer): TKMLPlacemark;
begin
  Result := FPlacemarks[Index];
end;

procedure TKMLFolder.SetPlacemark(Index: Integer; const _Value: TKMLPlacemark);
begin
  _Value.Parent := Self;
  FPlacemarks[Index].Free;
  FPlacemarks[Index] := _Value;
end;

procedure TKMLFolder.RemovePlacemark(_Value: TKMLPlacemark);
begin
  FPlacemarks.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeletePlacemark(Index: Integer);
begin
  FPlacemarks.Items[Index].Free;
  FPlacemarks.Delete(Index);
end;

procedure TKMLFolder.AddPlacemarkEvent(Sender: TObject);
var
  tmp: TKMLPlacemark;
begin
  tmp := AddPlacemark;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Placemark', tmp);
  tmp.ToTree;
end;

function TKMLFolder.AddScreenOverlay: TKMLScreenOverlay;
var
  ScreenOverlayTmp: TKMLScreenOverlay;
begin;
  ScreenOverlayTmp := TKMLScreenOverlay.Create(Self);
  FScreenOverlays.Add(ScreenOverlayTmp);
  Result := ScreenOverlayTmp;
end;

procedure TKMLFolder.SetScreenOverlays(const _Value: TList<TKMLScreenOverlay>);
begin
  ScreenOverlayClear;
  FScreenOverlays := _Value;
end;

procedure TKMLFolder.ScreenOverlayClear;
begin
  while FScreenOverlays.Count > 0 do
  begin
    FScreenOverlays.Items[0].Free;
    FScreenOverlays.Delete(0);
  end;
end;

function TKMLFolder.ScreenOverlayCount: Integer;
begin
  Result := FScreenOverlays.Count;
end;

function TKMLFolder.GetScreenOverlay(Index: Integer): TKMLScreenOverlay;
begin
  Result := FScreenOverlays[Index];
end;

procedure TKMLFolder.SetScreenOverlay(Index: Integer;
  const _Value: TKMLScreenOverlay);
begin
  _Value.Parent := Self;
  FScreenOverlays[Index].Free;
  FScreenOverlays[Index] := _Value;
end;

procedure TKMLFolder.RemoveScreenOverlay(_Value: TKMLScreenOverlay);
begin
  FScreenOverlays.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteScreenOverlay(Index: Integer);
begin
  FScreenOverlays.Items[Index].Free;
  FScreenOverlays.Delete(Index);
end;

procedure TKMLFolder.AddScreenOverlayEvent(Sender: TObject);
var
  tmp: TKMLScreenOverlay;
begin
  tmp := AddScreenOverlay;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('ScreenOverlay', tmp);
  tmp.ToTree;
end;

function TKMLFolder.AddFolder: TKMLFolder;
var
  FolderTmp: TKMLFolder;
begin;
  FolderTmp := TKMLFolder.Create(Self);
  FFolders.Add(FolderTmp);
  Result := FolderTmp;
end;

procedure TKMLFolder.SetFolders(const _Value: TList<TKMLFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

procedure TKMLFolder.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TKMLFolder.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

function TKMLFolder.GetFolder(Index: Integer): TKMLFolder;
begin
  Result := FFolders[Index];
end;

procedure TKMLFolder.SetFolder(Index: Integer; const _Value: TKMLFolder);
begin
  _Value.Parent := Self;
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TKMLFolder.RemoveFolder(_Value: TKMLFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TKMLFolder.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

procedure TKMLFolder.AddFolderEvent(Sender: TObject);
var
  tmp: TKMLFolder;
begin
  tmp := AddFolder;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Folder', tmp);
  tmp.ToTree;
end;

{ Placemark }
constructor TKMLPlacemark.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLPlacemark.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FGeometryExsit then
    FGeometry.Free;
  if FExtendedDataExsit then
    FExtendedData.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLPlacemark.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        FVisibility := nodeTmp.text.ToInteger;
        FVisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        FDescription := nodeTmp.text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        FLookAt := TKMLLookAt.Create(Self);
        FLookAt.FromXML(nodeTmp);
        FLookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'styleUrl' then
      begin
        FStyleUrl := nodeTmp.text;
        FStyleUrlExsit := True;
      end
      else if nodeTmp.NodeName = 'Point' then
      begin
        FGeometry := TKMLPoint.Create(Self);
        FGeometry.FromXML(nodeTmp);
        FGeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'Polygon' then
      begin
        FGeometry := TKMLPolygon.Create(Self);
        FGeometry.FromXML(nodeTmp);
        FGeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'LinearRing' then
      begin
        FGeometry := TKMLLinearRing.Create(Self);
        FGeometry.FromXML(nodeTmp);
        FGeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'LineString' then
      begin
        FGeometry := TKMLLineString.Create(Self);
        FGeometry.FromXML(nodeTmp);
        FGeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'MultiGeometry' then
      begin
        FGeometry := TKMLMultiGeometry.Create(Self);
        FGeometry.FromXML(nodeTmp);
        FGeometryExsit := True;
      end
      else if nodeTmp.NodeName = 'ExtendedData' then
      begin
        FExtendedData := TKMLExtendedData.Create(Self);
        FExtendedData.FromXML(nodeTmp);
        FExtendedDataExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Placemark Read XML Error!' + node.Xml);
  end;
end;

function TKMLPlacemark.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  StyleUrlTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Placemark';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FVisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := FVisibility.toString;
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FLookAtExsit then
      FLookAt.ToXML(node, 'LookAt');
    if FStyleUrlExsit then
    begin
      StyleUrlTmp := doc.CreateNode('styleUrl', ntElement);
      StyleUrlTmp.NodeValue := FStyleUrl;
      node.ChildNodes.Add(StyleUrlTmp);
    end;
    if FGeometryExsit then
      FGeometry.ToXML(node, '#Optional');
    if FExtendedDataExsit then
      FExtendedData.ToXML(node, 'ExtendedData');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLPlacemark.ToTree;
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

procedure TKMLPlacemark.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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
      TKMLPlacemarkPop.AddObject(ExtendedDataAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLPlacemarkTreeComponent.ClientToScreen(pt);
      TKMLPlacemarkPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLPlacemark.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLPlacemarkXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Visibility');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Visibility.toString);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('StyleUrl');
  Types_Value.Add(xs_string);
  _Values_Value.Add(StyleUrl);
  TKMLPlacemarkXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLPlacemark.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Visibility := _Value.ToInteger;
      end;
    2:
      begin
        Description := _Value;
      end;
    3:
      begin
        StyleUrl := _Value;
      end;
  end;
  ToTree;
end;

function TKMLPlacemark.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLPlacemark.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLPlacemark.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLPlacemark.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TKMLPlacemark.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLPlacemark.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLPlacemark.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLPlacemark.AddVisibilityEvent(Sender: TObject);
begin
  AddVisibility;
end;

function TKMLPlacemark.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLPlacemark.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLPlacemark.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLPlacemark.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TKMLPlacemark.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create(Self);
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLPlacemark.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
  FLookAt.Parent := Self;
end;

procedure TKMLPlacemark.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLPlacemark.AddLookAtEvent(Sender: TObject);
begin
  AddLookAt;
  FLookAt.ToTree;
end;

function TKMLPlacemark.AddStyleUrl: String;
begin;
  Result := FStyleUrl;
  FStyleUrlExsit := True;
end;

procedure TKMLPlacemark.SetStyleUrl(const _Value: String);
begin
  FStyleUrlExsit := True;
  FStyleUrl := _Value;
end;

procedure TKMLPlacemark.StyleUrlRemove;
begin
  if FStyleUrlExsit then
  begin
    FStyleUrlExsit := False;
  end;
end;

procedure TKMLPlacemark.AddStyleUrlEvent(Sender: TObject);
begin
  AddStyleUrl;
end;

function TKMLPlacemark.AddPoint: TKMLPoint;
begin;
  if not FGeometryExsit then
    FGeometry.Free;
  FGeometry := TKMLPoint.Create(Self);
  Result := TKMLPoint(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddPolygon: TKMLPolygon;
begin;
  if not FGeometryExsit then
    FGeometry.Free;
  FGeometry := TKMLPolygon.Create(Self);
  Result := TKMLPolygon(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddLinearRing: TKMLLinearRing;
begin;
  if not FGeometryExsit then
    FGeometry.Free;
  FGeometry := TKMLLinearRing.Create(Self);
  Result := TKMLLinearRing(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddLineString: TKMLLineString;
begin;
  if not FGeometryExsit then
    FGeometry.Free;
  FGeometry := TKMLLineString.Create(Self);
  Result := TKMLLineString(FGeometry);
  FGeometryExsit := True;
end;

function TKMLPlacemark.AddMultiGeometry: TKMLMultiGeometry;
begin;
  if not FGeometryExsit then
    FGeometry.Free;
  FGeometry := TKMLMultiGeometry.Create(Self);
  Result := TKMLMultiGeometry(FGeometry);
  FGeometryExsit := True;
end;

procedure TKMLPlacemark.SetGeometry(const _Value: TKMLGeometry);
begin
  if FGeometryExsit then
    FGeometry.Free;
  FGeometryExsit := True;
  FGeometry := _Value;
  FGeometry.Parent := Self;
end;

procedure TKMLPlacemark.GeometryRemove;
begin
  if FGeometryExsit then
  begin
    FGeometry.Free;
    FGeometryExsit := False;
  end;
end;

procedure TKMLPlacemark.AddPointEvent(Sender: TObject);
begin
  AddPoint;
  FGeometry.ToTree;
end;

procedure TKMLPlacemark.AddPolygonEvent(Sender: TObject);
begin
  AddPolygon;
  FGeometry.ToTree;
end;

procedure TKMLPlacemark.AddLinearRingEvent(Sender: TObject);
begin
  AddLinearRing;
  FGeometry.ToTree;
end;

procedure TKMLPlacemark.AddLineStringEvent(Sender: TObject);
begin
  AddLineString;
  FGeometry.ToTree;
end;

procedure TKMLPlacemark.AddMultiGeometryEvent(Sender: TObject);
begin
  AddMultiGeometry;
  FGeometry.ToTree;
end;

function TKMLPlacemark.AddExtendedData: TKMLExtendedData;
begin;
  if not FExtendedDataExsit then
    FExtendedData := TKMLExtendedData.Create(Self);
  Result := FExtendedData;
  FExtendedDataExsit := True;
end;

procedure TKMLPlacemark.SetExtendedData(const _Value: TKMLExtendedData);
begin
  if FExtendedDataExsit then
    FExtendedData.Free;
  FExtendedDataExsit := True;
  FExtendedData := _Value;
  FExtendedData.Parent := Self;
end;

procedure TKMLPlacemark.ExtendedDataRemove;
begin
  if FExtendedDataExsit then
  begin
    FExtendedData.Free;
    FExtendedDataExsit := False;
  end;
end;

procedure TKMLPlacemark.AddExtendedDataEvent(Sender: TObject);
begin
  AddExtendedData;
  FExtendedData.ToTree;
end;

{ Geometry }
constructor TKMLGeometry.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLGeometry.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLGeometry.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Geometry Read XML Error!' + node.Xml);
  end;
end;

function TKMLGeometry.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Geometry';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

function TKMLGeometry.AppendToXML(node: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  I: Integer;
begin
  try
    doc := node.OwnerDocument;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLGeometry.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
end;

procedure TKMLGeometry.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLGeometryPop) and Assigned(TKMLGeometryTreeComponent) then
    begin
      TKMLGeometryPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLGeometryTreeComponent.ClientToScreen(pt);
      TKMLGeometryPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLGeometry.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLGeometryXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TKMLGeometryXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLGeometry.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

{ MultiGeometry }
constructor TKMLMultiGeometry.Create(par: TXML = nil);
begin
  inherited Create(par);
  FGeometrys := TList<TKMLGeometry>.Create;
end;

destructor TKMLMultiGeometry.Destroy;
begin
  GeometryClear;
  FGeometrys.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLMultiGeometry.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  GeometryTmp: TKMLGeometry;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Point' then
      begin
        GeometryTmp := TKMLPoint.Create(Self);
        GeometryTmp.FromXML(nodeTmp);
        FGeometrys.Add(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'Polygon' then
      begin
        GeometryTmp := TKMLPolygon.Create(Self);
        GeometryTmp.FromXML(nodeTmp);
        FGeometrys.Add(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'LinearRing' then
      begin
        GeometryTmp := TKMLLinearRing.Create(Self);
        GeometryTmp.FromXML(nodeTmp);
        FGeometrys.Add(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'LineString' then
      begin
        GeometryTmp := TKMLLineString.Create(Self);
        GeometryTmp.FromXML(nodeTmp);
        FGeometrys.Add(GeometryTmp);
      end
      else if nodeTmp.NodeName = 'MultiGeometry' then
      begin
        GeometryTmp := TKMLMultiGeometry.Create(Self);
        GeometryTmp.FromXML(nodeTmp);
        FGeometrys.Add(GeometryTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('MultiGeometry Read XML Error!' + node.Xml);
  end;
end;

function TKMLMultiGeometry.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'MultiGeometry';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    for I := 0 to FGeometrys.Count - 1 do
      FGeometrys.Items[I].ToXML(node, '#Optional');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLMultiGeometry.ToTree;
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
    Geometrys[I].TreeNodeShape := TreeNodeShape.AddChildObject('Geometry',
      Geometry[I]);
    Geometry[I].ToTree;
  end;
end;

procedure TKMLMultiGeometry.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  GeometryAddMenu: TMenuItem;
  PointAddMenu: TMenuItem;
  PolygonAddMenu: TMenuItem;
  LinearRingAddMenu: TMenuItem;
  LineStringAddMenu: TMenuItem;
  MultiGeometryAddMenu: TMenuItem;
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
      pt := TPointF.Create(X, Y);
      pt := TKMLMultiGeometryTreeComponent.ClientToScreen(pt);
      TKMLMultiGeometryPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLMultiGeometry.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLMultiGeometryXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TKMLMultiGeometryXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLMultiGeometry.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

function TKMLMultiGeometry.AddPoint: TKMLPoint;
var
  Pointtmp: TKMLPoint;
begin;
  Pointtmp := TKMLPoint.Create(Self);
  FGeometrys.Add(Pointtmp);
  Result := Pointtmp;
end;

function TKMLMultiGeometry.AddPolygon: TKMLPolygon;
var
  Polygontmp: TKMLPolygon;
begin;
  Polygontmp := TKMLPolygon.Create(Self);
  FGeometrys.Add(Polygontmp);
  Result := Polygontmp;
end;

function TKMLMultiGeometry.AddLinearRing: TKMLLinearRing;
var
  LinearRingtmp: TKMLLinearRing;
begin;
  LinearRingtmp := TKMLLinearRing.Create(Self);
  FGeometrys.Add(LinearRingtmp);
  Result := LinearRingtmp;
end;

function TKMLMultiGeometry.AddLineString: TKMLLineString;
var
  LineStringtmp: TKMLLineString;
begin;
  LineStringtmp := TKMLLineString.Create(Self);
  FGeometrys.Add(LineStringtmp);
  Result := LineStringtmp;
end;

function TKMLMultiGeometry.AddMultiGeometry: TKMLMultiGeometry;
var
  MultiGeometrytmp: TKMLMultiGeometry;
begin;
  MultiGeometrytmp := TKMLMultiGeometry.Create(Self);
  FGeometrys.Add(MultiGeometrytmp);
  Result := MultiGeometrytmp;
end;

procedure TKMLMultiGeometry.SetGeometrys(const _Value: TList<TKMLGeometry>);
begin
  GeometryClear;
  FGeometrys := _Value;
end;

procedure TKMLMultiGeometry.GeometryClear;
begin
  while FGeometrys.Count > 0 do
  begin
    FGeometrys.Items[0].Free;
    FGeometrys.Delete(0);
  end;
end;

function TKMLMultiGeometry.GeometryCount: Integer;
begin
  Result := FGeometrys.Count;
end;

function TKMLMultiGeometry.GetGeometry(Index: Integer): TKMLGeometry;
begin
  Result := FGeometrys[Index];
end;

procedure TKMLMultiGeometry.SetGeometry(Index: Integer;
  const _Value: TKMLGeometry);
begin
  _Value.Parent := Self;
  FGeometrys[Index].Free;
  FGeometrys[Index] := _Value;
end;

procedure TKMLMultiGeometry.RemoveGeometry(_Value: TKMLGeometry);
begin
  FGeometrys.Remove(_Value);
  _Value.Free;
end;

procedure TKMLMultiGeometry.DeleteGeometry(Index: Integer);
begin
  FGeometrys.Items[Index].Free;
  FGeometrys.Delete(Index);
end;

procedure TKMLMultiGeometry.AddPointEvent(Sender: TObject);
var
  tmp: TKMLPoint;
begin
  tmp := AddPoint;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Point', tmp);
  tmp.ToTree;
end;

procedure TKMLMultiGeometry.AddPolygonEvent(Sender: TObject);
var
  tmp: TKMLPolygon;
begin
  tmp := AddPolygon;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Polygon', tmp);
  tmp.ToTree;
end;

procedure TKMLMultiGeometry.AddLinearRingEvent(Sender: TObject);
var
  tmp: TKMLLinearRing;
begin
  tmp := AddLinearRing;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('LinearRing', tmp);
  tmp.ToTree;
end;

procedure TKMLMultiGeometry.AddLineStringEvent(Sender: TObject);
var
  tmp: TKMLLineString;
begin
  tmp := AddLineString;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('LineString', tmp);
  tmp.ToTree;
end;

procedure TKMLMultiGeometry.AddMultiGeometryEvent(Sender: TObject);
var
  tmp: TKMLMultiGeometry;
begin
  tmp := AddMultiGeometry;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('MultiGeometry', tmp);
  tmp.ToTree;
end;

{ Point }
constructor TKMLPoint.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLPoint.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLPoint.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'extrude' then
      begin
        FExtrude := nodeTmp.text;
        FExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        FAltitudeMode := nodeTmp.text;
        FAltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'coordinates' then
      begin
        FCoordinates := StringToCoordinates(nodeTmp.text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Point Read XML Error!' + node.Xml);
  end;
end;

function TKMLPoint.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ExtrudeTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Point';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    if FExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := FExtrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    if FAltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := FAltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(FCoordinates);
    node.ChildNodes.Add(CoordinatesTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLPoint.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if ExtrudeExsit then
    TreeNodeShape.AddChild('Extrude');
  if AltitudeModeExsit then
    TreeNodeShape.AddChild('AltitudeMode');
  TreeNodeShape.AddChild('Coordinates');
end;

procedure TKMLPoint.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ExtrudeAddMenu: TMenuItem;
  AltitudeModeAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLPointPop) and Assigned(TKMLPointTreeComponent) then
    begin
      TKMLPointPop.Clear;
      ExtrudeAddMenu := TMenuItem.Create(TKMLPointPop);
      ExtrudeAddMenu.text := 'Add Extrude';
      ExtrudeAddMenu.OnClick := AddExtrudeEvent;
      TKMLPointPop.AddObject(ExtrudeAddMenu);
      AltitudeModeAddMenu := TMenuItem.Create(TKMLPointPop);
      AltitudeModeAddMenu.text := 'Add AltitudeMode';
      AltitudeModeAddMenu.OnClick := AddAltitudeModeEvent;
      TKMLPointPop.AddObject(AltitudeModeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLPointTreeComponent.ClientToScreen(pt);
      TKMLPointPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLPoint.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLPointXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Extrude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Extrude);
  Names_Value.Add('AltitudeMode');
  Types_Value.Add(xs_string);
  _Values_Value.Add(AltitudeMode);
  Names_Value.Add('Coordinates');
  Types_Value.Add(xml_Coordinate);
  _Values_Value.Add(CoordinatesToString(Coordinates));
  TKMLPointXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLPoint.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Extrude := _Value;
      end;
    1:
      begin
        AltitudeMode := _Value;
      end;
    2:
      begin
        Coordinates := StringToCoordinates(_Value);
      end;
  end;
  ToTree;
end;

function TKMLPoint.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLPoint.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLPoint.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLPoint.AddExtrudeEvent(Sender: TObject);
begin
  AddExtrude;
end;

function TKMLPoint.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLPoint.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLPoint.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLPoint.AddAltitudeModeEvent(Sender: TObject);
begin
  AddAltitudeMode;
end;

procedure TKMLPoint.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{ Polygon }
constructor TKMLPolygon.Create(par: TXML = nil);
begin
  inherited Create(par);
  FOuterBoundaryIss := TList<TKMLBoundary>.Create;
  FInnerBoundaryIss := TList<TKMLBoundary>.Create;
end;

destructor TKMLPolygon.Destroy;
begin
  OuterBoundaryIsClear;
  FOuterBoundaryIss.Free;
  InnerBoundaryIsClear;
  FInnerBoundaryIss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLPolygon.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  OuterBoundaryIsTmp: TKMLBoundary;
  InnerBoundaryIsTmp: TKMLBoundary;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'extrude' then
      begin
        FExtrude := nodeTmp.text;
        FExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        FAltitudeMode := nodeTmp.text;
        FAltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'outerBoundaryIs' then
      begin
        OuterBoundaryIsTmp := TKMLBoundary.Create(Self);
        OuterBoundaryIsTmp.FromXML(nodeTmp);
        FOuterBoundaryIss.Add(OuterBoundaryIsTmp);
      end
      else if nodeTmp.NodeName = 'innerBoundaryIs' then
      begin
        InnerBoundaryIsTmp := TKMLBoundary.Create(Self);
        InnerBoundaryIsTmp.FromXML(nodeTmp);
        FInnerBoundaryIss.Add(InnerBoundaryIsTmp);
      end
      else if nodeTmp.NodeName = 'tessellate' then
      begin
        FTessellate := nodeTmp.text;
        FTessellateExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Polygon Read XML Error!' + node.Xml);
  end;
end;

function TKMLPolygon.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ExtrudeTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  TessellateTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Polygon';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    if FExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := FExtrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    if FAltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := FAltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    for I := 0 to FOuterBoundaryIss.Count - 1 do
      FOuterBoundaryIss.Items[I].ToXML(node, 'outerBoundaryIs');
    for I := 0 to FInnerBoundaryIss.Count - 1 do
      FInnerBoundaryIss.Items[I].ToXML(node, 'innerBoundaryIs');
    if FTessellateExsit then
    begin
      TessellateTmp := doc.CreateNode('tessellate', ntElement);
      TessellateTmp.NodeValue := FTessellate;
      node.ChildNodes.Add(TessellateTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLPolygon.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if ExtrudeExsit then
    TreeNodeShape.AddChild('Extrude');
  if AltitudeModeExsit then
    TreeNodeShape.AddChild('AltitudeMode');
  for I := 0 to OuterBoundaryIsCount - 1 do
  begin
    OuterBoundaryIss[I].TreeNodeShape := TreeNodeShape.AddChildObject
      ('OuterBoundaryIs', OuterBoundaryIs[I]);
    OuterBoundaryIs[I].ToTree;
  end;
  for I := 0 to InnerBoundaryIsCount - 1 do
  begin
    InnerBoundaryIss[I].TreeNodeShape := TreeNodeShape.AddChildObject
      ('InnerBoundaryIs', InnerBoundaryIs[I]);
    InnerBoundaryIs[I].ToTree;
  end;
  if TessellateExsit then
    TreeNodeShape.AddChild('Tessellate');
end;

procedure TKMLPolygon.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ExtrudeAddMenu: TMenuItem;
  AltitudeModeAddMenu: TMenuItem;
  OuterBoundaryIsAddMenu: TMenuItem;
  InnerBoundaryIsAddMenu: TMenuItem;
  TessellateAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLPolygonPop) and Assigned(TKMLPolygonTreeComponent) then
    begin
      TKMLPolygonPop.Clear;
      ExtrudeAddMenu := TMenuItem.Create(TKMLPolygonPop);
      ExtrudeAddMenu.text := 'Add Extrude';
      ExtrudeAddMenu.OnClick := AddExtrudeEvent;
      TKMLPolygonPop.AddObject(ExtrudeAddMenu);
      AltitudeModeAddMenu := TMenuItem.Create(TKMLPolygonPop);
      AltitudeModeAddMenu.text := 'Add AltitudeMode';
      AltitudeModeAddMenu.OnClick := AddAltitudeModeEvent;
      TKMLPolygonPop.AddObject(AltitudeModeAddMenu);
      OuterBoundaryIsAddMenu := TMenuItem.Create(TKMLPolygonPop);
      OuterBoundaryIsAddMenu.text := 'Add OuterBoundaryIs';
      OuterBoundaryIsAddMenu.OnClick := AddOuterBoundaryIsEvent;
      TKMLPolygonPop.AddObject(OuterBoundaryIsAddMenu);
      InnerBoundaryIsAddMenu := TMenuItem.Create(TKMLPolygonPop);
      InnerBoundaryIsAddMenu.text := 'Add InnerBoundaryIs';
      InnerBoundaryIsAddMenu.OnClick := AddInnerBoundaryIsEvent;
      TKMLPolygonPop.AddObject(InnerBoundaryIsAddMenu);
      TessellateAddMenu := TMenuItem.Create(TKMLPolygonPop);
      TessellateAddMenu.text := 'Add Tessellate';
      TessellateAddMenu.OnClick := AddTessellateEvent;
      TKMLPolygonPop.AddObject(TessellateAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLPolygonTreeComponent.ClientToScreen(pt);
      TKMLPolygonPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLPolygon.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLPolygonXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Extrude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Extrude);
  Names_Value.Add('AltitudeMode');
  Types_Value.Add(xs_string);
  _Values_Value.Add(AltitudeMode);
  Names_Value.Add('Tessellate');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Tessellate);
  TKMLPolygonXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLPolygon.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Extrude := _Value;
      end;
    1:
      begin
        AltitudeMode := _Value;
      end;
    2:
      begin
        Tessellate := _Value;
      end;
  end;
  ToTree;
end;

function TKMLPolygon.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLPolygon.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLPolygon.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLPolygon.AddExtrudeEvent(Sender: TObject);
begin
  AddExtrude;
end;

function TKMLPolygon.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLPolygon.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLPolygon.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLPolygon.AddAltitudeModeEvent(Sender: TObject);
begin
  AddAltitudeMode;
end;

function TKMLPolygon.AddOuterBoundaryIs: TKMLBoundary;
var
  OuterBoundaryIsTmp: TKMLBoundary;
begin;
  OuterBoundaryIsTmp := TKMLBoundary.Create(Self);
  FOuterBoundaryIss.Add(OuterBoundaryIsTmp);
  Result := OuterBoundaryIsTmp;
end;

procedure TKMLPolygon.SetOuterBoundaryIss(const _Value: TList<TKMLBoundary>);
begin
  OuterBoundaryIsClear;
  FOuterBoundaryIss := _Value;
end;

procedure TKMLPolygon.OuterBoundaryIsClear;
begin
  while FOuterBoundaryIss.Count > 0 do
  begin
    FOuterBoundaryIss.Items[0].Free;
    FOuterBoundaryIss.Delete(0);
  end;
end;

function TKMLPolygon.OuterBoundaryIsCount: Integer;
begin
  Result := FOuterBoundaryIss.Count;
end;

function TKMLPolygon.GetOuterBoundaryIs(Index: Integer): TKMLBoundary;
begin
  Result := FOuterBoundaryIss[Index];
end;

procedure TKMLPolygon.SetOuterBoundaryIs(Index: Integer;
  const _Value: TKMLBoundary);
begin
  _Value.Parent := Self;
  FOuterBoundaryIss[Index].Free;
  FOuterBoundaryIss[Index] := _Value;
end;

procedure TKMLPolygon.RemoveOuterBoundaryIs(_Value: TKMLBoundary);
begin
  FOuterBoundaryIss.Remove(_Value);
  _Value.Free;
end;

procedure TKMLPolygon.DeleteOuterBoundaryIs(Index: Integer);
begin
  FOuterBoundaryIss.Items[Index].Free;
  FOuterBoundaryIss.Delete(Index);
end;

procedure TKMLPolygon.AddOuterBoundaryIsEvent(Sender: TObject);
var
  tmp: TKMLBoundary;
begin
  tmp := AddOuterBoundaryIs;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('OuterBoundaryIs', tmp);
  tmp.ToTree;
end;

function TKMLPolygon.AddInnerBoundaryIs: TKMLBoundary;
var
  InnerBoundaryIsTmp: TKMLBoundary;
begin;
  InnerBoundaryIsTmp := TKMLBoundary.Create(Self);
  FInnerBoundaryIss.Add(InnerBoundaryIsTmp);
  Result := InnerBoundaryIsTmp;
end;

procedure TKMLPolygon.SetInnerBoundaryIss(const _Value: TList<TKMLBoundary>);
begin
  InnerBoundaryIsClear;
  FInnerBoundaryIss := _Value;
end;

procedure TKMLPolygon.InnerBoundaryIsClear;
begin
  while FInnerBoundaryIss.Count > 0 do
  begin
    FInnerBoundaryIss.Items[0].Free;
    FInnerBoundaryIss.Delete(0);
  end;
end;

function TKMLPolygon.InnerBoundaryIsCount: Integer;
begin
  Result := FInnerBoundaryIss.Count;
end;

function TKMLPolygon.GetInnerBoundaryIs(Index: Integer): TKMLBoundary;
begin
  Result := FInnerBoundaryIss[Index];
end;

procedure TKMLPolygon.SetInnerBoundaryIs(Index: Integer;
  const _Value: TKMLBoundary);
begin
  _Value.Parent := Self;
  FInnerBoundaryIss[Index].Free;
  FInnerBoundaryIss[Index] := _Value;
end;

procedure TKMLPolygon.RemoveInnerBoundaryIs(_Value: TKMLBoundary);
begin
  FInnerBoundaryIss.Remove(_Value);
  _Value.Free;
end;

procedure TKMLPolygon.DeleteInnerBoundaryIs(Index: Integer);
begin
  FInnerBoundaryIss.Items[Index].Free;
  FInnerBoundaryIss.Delete(Index);
end;

procedure TKMLPolygon.AddInnerBoundaryIsEvent(Sender: TObject);
var
  tmp: TKMLBoundary;
begin
  tmp := AddInnerBoundaryIs;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('InnerBoundaryIs', tmp);
  tmp.ToTree;
end;

function TKMLPolygon.AddTessellate: String;
begin;
  Result := FTessellate;
  FTessellateExsit := True;
end;

procedure TKMLPolygon.SetTessellate(const _Value: String);
begin
  FTessellateExsit := True;
  FTessellate := _Value;
end;

procedure TKMLPolygon.TessellateRemove;
begin
  if FTessellateExsit then
  begin
    FTessellateExsit := False;
  end;
end;

procedure TKMLPolygon.AddTessellateEvent(Sender: TObject);
begin
  AddTessellate;
end;

{ Boundary }
constructor TKMLBoundary.Create(par: TXML = nil);
begin
  inherited Create(par);
  FLinearRing := TKMLLinearRing.Create(Self);
end;

destructor TKMLBoundary.Destroy;
begin
  FLinearRing.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLBoundary.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'LinearRing' then
      begin
        FLinearRing := TKMLLinearRing.Create(Self);
        FLinearRing.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Boundary Read XML Error!' + node.Xml);
  end;
end;

function TKMLBoundary.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Boundary';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FLinearRing.ToXML(node, 'LinearRing');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLBoundary.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  LinearRing.TreeNodeShape := TreeNodeShape.AddChildObject('LinearRing',
    LinearRing);
  LinearRing.ToTree;
end;

procedure TKMLBoundary.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLBoundaryPop) and Assigned(TKMLBoundaryTreeComponent) then
    begin
      TKMLBoundaryPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLBoundaryTreeComponent.ClientToScreen(pt);
      TKMLBoundaryPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLBoundary.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLBoundaryXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TKMLBoundaryXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLBoundary.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

procedure TKMLBoundary.SetLinearRing(const _Value: TKMLLinearRing);
begin
  FLinearRing.Free;
  FLinearRing := _Value;
  FLinearRing.Parent := Self;
end;

{ LinearRing }
constructor TKMLLinearRing.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLLinearRing.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLLinearRing.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'coordinates' then
      begin
        FCoordinates := StringToCoordinates(nodeTmp.text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('LinearRing Read XML Error!' + node.Xml);
  end;
end;

function TKMLLinearRing.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'LinearRing';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(FCoordinates);
    node.ChildNodes.Add(CoordinatesTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLLinearRing.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Coordinates');
end;

procedure TKMLLinearRing.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLLinearRingPop) and Assigned(TKMLLinearRingTreeComponent)
    then
    begin
      TKMLLinearRingPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLLinearRingTreeComponent.ClientToScreen(pt);
      TKMLLinearRingPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLLinearRing.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLLinearRingXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Coordinates');
  Types_Value.Add(xml_Coordinate);
  _Values_Value.Add(CoordinatesToString(Coordinates));
  TKMLLinearRingXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLLinearRing.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Coordinates := StringToCoordinates(_Value);
      end;
  end;
  ToTree;
end;

procedure TKMLLinearRing.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{ LineString }
constructor TKMLLineString.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLLineString.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLLineString.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'tessellate' then
      begin
        FTessellate := nodeTmp.text;
        FTessellateExsit := True;
      end
      else if nodeTmp.NodeName = 'altitudeMode' then
      begin
        FAltitudeMode := nodeTmp.text;
        FAltitudeModeExsit := True;
      end
      else if nodeTmp.NodeName = 'extrude' then
      begin
        FExtrude := nodeTmp.text;
        FExtrudeExsit := True;
      end
      else if nodeTmp.NodeName = 'coordinates' then
      begin
        FCoordinates := StringToCoordinates(nodeTmp.text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('LineString Read XML Error!' + node.Xml);
  end;
end;

function TKMLLineString.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  TessellateTmp: IXMLNode;
  AltitudeModeTmp: IXMLNode;
  ExtrudeTmp: IXMLNode;
  CoordinatesTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'LineString';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    if FTessellateExsit then
    begin
      TessellateTmp := doc.CreateNode('tessellate', ntElement);
      TessellateTmp.NodeValue := FTessellate;
      node.ChildNodes.Add(TessellateTmp);
    end;
    if FAltitudeModeExsit then
    begin
      AltitudeModeTmp := doc.CreateNode('altitudeMode', ntElement);
      AltitudeModeTmp.NodeValue := FAltitudeMode;
      node.ChildNodes.Add(AltitudeModeTmp);
    end;
    if FExtrudeExsit then
    begin
      ExtrudeTmp := doc.CreateNode('extrude', ntElement);
      ExtrudeTmp.NodeValue := FExtrude;
      node.ChildNodes.Add(ExtrudeTmp);
    end;
    CoordinatesTmp := doc.CreateNode('coordinates', ntElement);
    CoordinatesTmp.NodeValue := CoordinatesToString(FCoordinates);
    node.ChildNodes.Add(CoordinatesTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLLineString.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if TessellateExsit then
    TreeNodeShape.AddChild('Tessellate');
  if AltitudeModeExsit then
    TreeNodeShape.AddChild('AltitudeMode');
  if ExtrudeExsit then
    TreeNodeShape.AddChild('Extrude');
  TreeNodeShape.AddChild('Coordinates');
end;

procedure TKMLLineString.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  TessellateAddMenu: TMenuItem;
  AltitudeModeAddMenu: TMenuItem;
  ExtrudeAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLLineStringPop) and Assigned(TKMLLineStringTreeComponent)
    then
    begin
      TKMLLineStringPop.Clear;
      TessellateAddMenu := TMenuItem.Create(TKMLLineStringPop);
      TessellateAddMenu.text := 'Add Tessellate';
      TessellateAddMenu.OnClick := AddTessellateEvent;
      TKMLLineStringPop.AddObject(TessellateAddMenu);
      AltitudeModeAddMenu := TMenuItem.Create(TKMLLineStringPop);
      AltitudeModeAddMenu.text := 'Add AltitudeMode';
      AltitudeModeAddMenu.OnClick := AddAltitudeModeEvent;
      TKMLLineStringPop.AddObject(AltitudeModeAddMenu);
      ExtrudeAddMenu := TMenuItem.Create(TKMLLineStringPop);
      ExtrudeAddMenu.text := 'Add Extrude';
      ExtrudeAddMenu.OnClick := AddExtrudeEvent;
      TKMLLineStringPop.AddObject(ExtrudeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLLineStringTreeComponent.ClientToScreen(pt);
      TKMLLineStringPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLLineString.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLLineStringXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Tessellate');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Tessellate);
  Names_Value.Add('AltitudeMode');
  Types_Value.Add(xs_string);
  _Values_Value.Add(AltitudeMode);
  Names_Value.Add('Extrude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Extrude);
  Names_Value.Add('Coordinates');
  Types_Value.Add(xml_Coordinate);
  _Values_Value.Add(CoordinatesToString(Coordinates));
  TKMLLineStringXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLLineString.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Tessellate := _Value;
      end;
    1:
      begin
        AltitudeMode := _Value;
      end;
    2:
      begin
        Extrude := _Value;
      end;
    3:
      begin
        Coordinates := StringToCoordinates(_Value);
      end;
  end;
  ToTree;
end;

function TKMLLineString.AddTessellate: String;
begin;
  Result := FTessellate;
  FTessellateExsit := True;
end;

procedure TKMLLineString.SetTessellate(const _Value: String);
begin
  FTessellateExsit := True;
  FTessellate := _Value;
end;

procedure TKMLLineString.TessellateRemove;
begin
  if FTessellateExsit then
  begin
    FTessellateExsit := False;
  end;
end;

procedure TKMLLineString.AddTessellateEvent(Sender: TObject);
begin
  AddTessellate;
end;

function TKMLLineString.AddAltitudeMode: String;
begin;
  Result := FAltitudeMode;
  FAltitudeModeExsit := True;
end;

procedure TKMLLineString.SetAltitudeMode(const _Value: String);
begin
  FAltitudeModeExsit := True;
  FAltitudeMode := _Value;
end;

procedure TKMLLineString.AltitudeModeRemove;
begin
  if FAltitudeModeExsit then
  begin
    FAltitudeModeExsit := False;
  end;
end;

procedure TKMLLineString.AddAltitudeModeEvent(Sender: TObject);
begin
  AddAltitudeMode;
end;

function TKMLLineString.AddExtrude: String;
begin;
  Result := FExtrude;
  FExtrudeExsit := True;
end;

procedure TKMLLineString.SetExtrude(const _Value: String);
begin
  FExtrudeExsit := True;
  FExtrude := _Value;
end;

procedure TKMLLineString.ExtrudeRemove;
begin
  if FExtrudeExsit then
  begin
    FExtrudeExsit := False;
  end;
end;

procedure TKMLLineString.AddExtrudeEvent(Sender: TObject);
begin
  AddExtrude;
end;

procedure TKMLLineString.SetCoordinates(const _Value: ArrayCoordinates);
begin
  FCoordinates := _Value;
end;

{ LookAt }
constructor TKMLLookAt.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLLookAt.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLLookAt.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'longitude' then
      begin
        FLongitude := nodeTmp.text.ToDouble;
        FLongitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'latitude' then
      begin
        FLatitude := nodeTmp.text.ToDouble;
        FLatitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'altitude' then
      begin
        FAltitude := nodeTmp.text.ToDouble;
        FAltitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'heading' then
      begin
        FHeading := nodeTmp.text.ToDouble;
        FHeadingExsit := True;
      end
      else if nodeTmp.NodeName = 'tilt' then
      begin
        FTilt := nodeTmp.text.ToDouble;
        FTiltExsit := True;
      end
      else if nodeTmp.NodeName = 'range' then
      begin
        FRange := nodeTmp.text.ToDouble;
        FRangeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('LookAt Read XML Error!' + node.Xml);
  end;
end;

function TKMLLookAt.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  LongitudeTmp: IXMLNode;
  LatitudeTmp: IXMLNode;
  AltitudeTmp: IXMLNode;
  HeadingTmp: IXMLNode;
  TiltTmp: IXMLNode;
  RangeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'LookAt';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FLongitudeExsit then
    begin
      LongitudeTmp := doc.CreateNode('longitude', ntElement);
      LongitudeTmp.NodeValue := FLongitude.toString;
      node.ChildNodes.Add(LongitudeTmp);
    end;
    if FLatitudeExsit then
    begin
      LatitudeTmp := doc.CreateNode('latitude', ntElement);
      LatitudeTmp.NodeValue := FLatitude.toString;
      node.ChildNodes.Add(LatitudeTmp);
    end;
    if FAltitudeExsit then
    begin
      AltitudeTmp := doc.CreateNode('altitude', ntElement);
      AltitudeTmp.NodeValue := FAltitude.toString;
      node.ChildNodes.Add(AltitudeTmp);
    end;
    if FHeadingExsit then
    begin
      HeadingTmp := doc.CreateNode('heading', ntElement);
      HeadingTmp.NodeValue := FHeading.toString;
      node.ChildNodes.Add(HeadingTmp);
    end;
    if FTiltExsit then
    begin
      TiltTmp := doc.CreateNode('tilt', ntElement);
      TiltTmp.NodeValue := FTilt.toString;
      node.ChildNodes.Add(TiltTmp);
    end;
    if FRangeExsit then
    begin
      RangeTmp := doc.CreateNode('range', ntElement);
      RangeTmp.NodeValue := FRange.toString;
      node.ChildNodes.Add(RangeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLLookAt.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if LongitudeExsit then
    TreeNodeShape.AddChild('Longitude');
  if LatitudeExsit then
    TreeNodeShape.AddChild('Latitude');
  if AltitudeExsit then
    TreeNodeShape.AddChild('Altitude');
  if HeadingExsit then
    TreeNodeShape.AddChild('Heading');
  if TiltExsit then
    TreeNodeShape.AddChild('Tilt');
  if RangeExsit then
    TreeNodeShape.AddChild('Range');
end;

procedure TKMLLookAt.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  LongitudeAddMenu: TMenuItem;
  LatitudeAddMenu: TMenuItem;
  AltitudeAddMenu: TMenuItem;
  HeadingAddMenu: TMenuItem;
  TiltAddMenu: TMenuItem;
  RangeAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLLookAtPop) and Assigned(TKMLLookAtTreeComponent) then
    begin
      TKMLLookAtPop.Clear;
      LongitudeAddMenu := TMenuItem.Create(TKMLLookAtPop);
      LongitudeAddMenu.text := 'Add Longitude';
      LongitudeAddMenu.OnClick := AddLongitudeEvent;
      TKMLLookAtPop.AddObject(LongitudeAddMenu);
      LatitudeAddMenu := TMenuItem.Create(TKMLLookAtPop);
      LatitudeAddMenu.text := 'Add Latitude';
      LatitudeAddMenu.OnClick := AddLatitudeEvent;
      TKMLLookAtPop.AddObject(LatitudeAddMenu);
      AltitudeAddMenu := TMenuItem.Create(TKMLLookAtPop);
      AltitudeAddMenu.text := 'Add Altitude';
      AltitudeAddMenu.OnClick := AddAltitudeEvent;
      TKMLLookAtPop.AddObject(AltitudeAddMenu);
      HeadingAddMenu := TMenuItem.Create(TKMLLookAtPop);
      HeadingAddMenu.text := 'Add Heading';
      HeadingAddMenu.OnClick := AddHeadingEvent;
      TKMLLookAtPop.AddObject(HeadingAddMenu);
      TiltAddMenu := TMenuItem.Create(TKMLLookAtPop);
      TiltAddMenu.text := 'Add Tilt';
      TiltAddMenu.OnClick := AddTiltEvent;
      TKMLLookAtPop.AddObject(TiltAddMenu);
      RangeAddMenu := TMenuItem.Create(TKMLLookAtPop);
      RangeAddMenu.text := 'Add Range';
      RangeAddMenu.OnClick := AddRangeEvent;
      TKMLLookAtPop.AddObject(RangeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLLookAtTreeComponent.ClientToScreen(pt);
      TKMLLookAtPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLLookAt.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLLookAtXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Longitude');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Longitude.toString);
  Names_Value.Add('Latitude');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Latitude.toString);
  Names_Value.Add('Altitude');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Altitude.toString);
  Names_Value.Add('Heading');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Heading.toString);
  Names_Value.Add('Tilt');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Tilt.toString);
  Names_Value.Add('Range');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Range.toString);
  TKMLLookAtXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLLookAt.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Longitude := _Value.ToDouble;
      end;
    1:
      begin
        Latitude := _Value.ToDouble;
      end;
    2:
      begin
        Altitude := _Value.ToDouble;
      end;
    3:
      begin
        Heading := _Value.ToDouble;
      end;
    4:
      begin
        Tilt := _Value.ToDouble;
      end;
    5:
      begin
        Range := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TKMLLookAt.AddLongitude: Double;
begin;
  Result := FLongitude;
  FLongitudeExsit := True;
end;

procedure TKMLLookAt.SetLongitude(const _Value: Double);
begin
  FLongitudeExsit := True;
  FLongitude := _Value;
end;

procedure TKMLLookAt.LongitudeRemove;
begin
  if FLongitudeExsit then
  begin
    FLongitudeExsit := False;
  end;
end;

procedure TKMLLookAt.AddLongitudeEvent(Sender: TObject);
begin
  AddLongitude;
end;

function TKMLLookAt.AddLatitude: Double;
begin;
  Result := FLatitude;
  FLatitudeExsit := True;
end;

procedure TKMLLookAt.SetLatitude(const _Value: Double);
begin
  FLatitudeExsit := True;
  FLatitude := _Value;
end;

procedure TKMLLookAt.LatitudeRemove;
begin
  if FLatitudeExsit then
  begin
    FLatitudeExsit := False;
  end;
end;

procedure TKMLLookAt.AddLatitudeEvent(Sender: TObject);
begin
  AddLatitude;
end;

function TKMLLookAt.AddAltitude: Double;
begin;
  Result := FAltitude;
  FAltitudeExsit := True;
end;

procedure TKMLLookAt.SetAltitude(const _Value: Double);
begin
  FAltitudeExsit := True;
  FAltitude := _Value;
end;

procedure TKMLLookAt.AltitudeRemove;
begin
  if FAltitudeExsit then
  begin
    FAltitudeExsit := False;
  end;
end;

procedure TKMLLookAt.AddAltitudeEvent(Sender: TObject);
begin
  AddAltitude;
end;

function TKMLLookAt.AddHeading: Double;
begin;
  Result := FHeading;
  FHeadingExsit := True;
end;

procedure TKMLLookAt.SetHeading(const _Value: Double);
begin
  FHeadingExsit := True;
  FHeading := _Value;
end;

procedure TKMLLookAt.HeadingRemove;
begin
  if FHeadingExsit then
  begin
    FHeadingExsit := False;
  end;
end;

procedure TKMLLookAt.AddHeadingEvent(Sender: TObject);
begin
  AddHeading;
end;

function TKMLLookAt.AddTilt: Double;
begin;
  Result := FTilt;
  FTiltExsit := True;
end;

procedure TKMLLookAt.SetTilt(const _Value: Double);
begin
  FTiltExsit := True;
  FTilt := _Value;
end;

procedure TKMLLookAt.TiltRemove;
begin
  if FTiltExsit then
  begin
    FTiltExsit := False;
  end;
end;

procedure TKMLLookAt.AddTiltEvent(Sender: TObject);
begin
  AddTilt;
end;

function TKMLLookAt.AddRange: Double;
begin;
  Result := FRange;
  FRangeExsit := True;
end;

procedure TKMLLookAt.SetRange(const _Value: Double);
begin
  FRangeExsit := True;
  FRange := _Value;
end;

procedure TKMLLookAt.RangeRemove;
begin
  if FRangeExsit then
  begin
    FRangeExsit := False;
  end;
end;

procedure TKMLLookAt.AddRangeEvent(Sender: TObject);
begin
  AddRange;
end;

{ LatLonBox }
constructor TKMLLatLonBox.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLLatLonBox.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLLatLonBox.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'north' then
      begin
        FNorth := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'south' then
      begin
        FSouth := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'east' then
      begin
        FEast := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'west' then
      begin
        FWest := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'rotation' then
      begin
        FRotation := nodeTmp.text.ToDouble;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('LatLonBox Read XML Error!' + node.Xml);
  end;
end;

function TKMLLatLonBox.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NorthTmp: IXMLNode;
  SouthTmp: IXMLNode;
  EastTmp: IXMLNode;
  WestTmp: IXMLNode;
  RotationTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'LatLonBox';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    NorthTmp := doc.CreateNode('north', ntElement);
    NorthTmp.NodeValue := FNorth.toString;
    node.ChildNodes.Add(NorthTmp);
    SouthTmp := doc.CreateNode('south', ntElement);
    SouthTmp.NodeValue := FSouth.toString;
    node.ChildNodes.Add(SouthTmp);
    EastTmp := doc.CreateNode('east', ntElement);
    EastTmp.NodeValue := FEast.toString;
    node.ChildNodes.Add(EastTmp);
    WestTmp := doc.CreateNode('west', ntElement);
    WestTmp.NodeValue := FWest.toString;
    node.ChildNodes.Add(WestTmp);
    RotationTmp := doc.CreateNode('rotation', ntElement);
    RotationTmp.NodeValue := FRotation.toString;
    node.ChildNodes.Add(RotationTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLLatLonBox.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('North');
  TreeNodeShape.AddChild('South');
  TreeNodeShape.AddChild('East');
  TreeNodeShape.AddChild('West');
  TreeNodeShape.AddChild('Rotation');
end;

procedure TKMLLatLonBox.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLLatLonBoxPop) and Assigned(TKMLLatLonBoxTreeComponent) then
    begin
      TKMLLatLonBoxPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLLatLonBoxTreeComponent.ClientToScreen(pt);
      TKMLLatLonBoxPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLLatLonBox.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLLatLonBoxXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('North');
  Types_Value.Add(xs_double);
  _Values_Value.Add(North.toString);
  Names_Value.Add('South');
  Types_Value.Add(xs_double);
  _Values_Value.Add(South.toString);
  Names_Value.Add('East');
  Types_Value.Add(xs_double);
  _Values_Value.Add(East.toString);
  Names_Value.Add('West');
  Types_Value.Add(xs_double);
  _Values_Value.Add(West.toString);
  Names_Value.Add('Rotation');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Rotation.toString);
  TKMLLatLonBoxXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLLatLonBox.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        North := _Value.ToDouble;
      end;
    1:
      begin
        South := _Value.ToDouble;
      end;
    2:
      begin
        East := _Value.ToDouble;
      end;
    3:
      begin
        West := _Value.ToDouble;
      end;
    4:
      begin
        Rotation := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

procedure TKMLLatLonBox.SetNorth(const _Value: Double);
begin
  FNorth := _Value;
end;

procedure TKMLLatLonBox.SetSouth(const _Value: Double);
begin
  FSouth := _Value;
end;

procedure TKMLLatLonBox.SetEast(const _Value: Double);
begin
  FEast := _Value;
end;

procedure TKMLLatLonBox.SetWest(const _Value: Double);
begin
  FWest := _Value;
end;

procedure TKMLLatLonBox.SetRotation(const _Value: Double);
begin
  FRotation := _Value;
end;

{ GroundOverlay }
constructor TKMLGroundOverlay.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLGroundOverlay.Destroy;
begin
  if FLookAtExsit then
    FLookAt.Free;
  if FIconExsit then
    FIcon.Free;
  if FLatLonBoxExsit then
    FLatLonBox.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLGroundOverlay.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        FVisibility := nodeTmp.text.ToInteger;
        FVisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        FDescription := nodeTmp.text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'LookAt' then
      begin
        FLookAt := TKMLLookAt.Create(Self);
        FLookAt.FromXML(nodeTmp);
        FLookAtExsit := True;
      end
      else if nodeTmp.NodeName = 'Icon' then
      begin
        FIcon := TKMLIcon.Create(Self);
        FIcon.FromXML(nodeTmp);
        FIconExsit := True;
      end
      else if nodeTmp.NodeName = 'LatLonBox' then
      begin
        FLatLonBox := TKMLLatLonBox.Create(Self);
        FLatLonBox.FromXML(nodeTmp);
        FLatLonBoxExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('GroundOverlay Read XML Error!' + node.Xml);
  end;
end;

function TKMLGroundOverlay.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'GroundOverlay';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FVisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := FVisibility.toString;
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FLookAtExsit then
      FLookAt.ToXML(node, 'LookAt');
    if FIconExsit then
      FIcon.ToXML(node, 'Icon');
    if FLatLonBoxExsit then
      FLatLonBox.ToXML(node, 'LatLonBox');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLGroundOverlay.ToTree;
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
  if IconExsit then
  begin
    Icon.TreeNodeShape := TreeNodeShape.AddChildObject('Icon', Icon);
    Icon.ToTree;
  end;
  if LatLonBoxExsit then
  begin
    LatLonBox.TreeNodeShape := TreeNodeShape.AddChildObject('LatLonBox',
      LatLonBox);
    LatLonBox.ToTree;
  end;
end;

procedure TKMLGroundOverlay.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  VisibilityAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  LookAtAddMenu: TMenuItem;
  IconAddMenu: TMenuItem;
  LatLonBoxAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLGroundOverlayPop) and
      Assigned(TKMLGroundOverlayTreeComponent) then
    begin
      TKMLGroundOverlayPop.Clear;
      NameAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      NameAddMenu.text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TKMLGroundOverlayPop.AddObject(NameAddMenu);
      VisibilityAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      VisibilityAddMenu.text := 'Add Visibility';
      VisibilityAddMenu.OnClick := AddVisibilityEvent;
      TKMLGroundOverlayPop.AddObject(VisibilityAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      DescriptionAddMenu.text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TKMLGroundOverlayPop.AddObject(DescriptionAddMenu);
      LookAtAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      LookAtAddMenu.text := 'Add LookAt';
      LookAtAddMenu.OnClick := AddLookAtEvent;
      TKMLGroundOverlayPop.AddObject(LookAtAddMenu);
      IconAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      IconAddMenu.text := 'Add Icon';
      IconAddMenu.OnClick := AddIconEvent;
      TKMLGroundOverlayPop.AddObject(IconAddMenu);
      LatLonBoxAddMenu := TMenuItem.Create(TKMLGroundOverlayPop);
      LatLonBoxAddMenu.text := 'Add LatLonBox';
      LatLonBoxAddMenu.OnClick := AddLatLonBoxEvent;
      TKMLGroundOverlayPop.AddObject(LatLonBoxAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLGroundOverlayTreeComponent.ClientToScreen(pt);
      TKMLGroundOverlayPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLGroundOverlay.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLGroundOverlayXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Visibility');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Visibility.toString);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TKMLGroundOverlayXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLGroundOverlay.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Visibility := _Value.ToInteger;
      end;
    2:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

function TKMLGroundOverlay.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLGroundOverlay.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLGroundOverlay.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TKMLGroundOverlay.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLGroundOverlay.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLGroundOverlay.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddVisibilityEvent(Sender: TObject);
begin
  AddVisibility;
end;

function TKMLGroundOverlay.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLGroundOverlay.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLGroundOverlay.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TKMLGroundOverlay.AddLookAt: TKMLLookAt;
begin;
  if not FLookAtExsit then
    FLookAt := TKMLLookAt.Create(Self);
  Result := FLookAt;
  FLookAtExsit := True;
end;

procedure TKMLGroundOverlay.SetLookAt(const _Value: TKMLLookAt);
begin
  if FLookAtExsit then
    FLookAt.Free;
  FLookAtExsit := True;
  FLookAt := _Value;
  FLookAt.Parent := Self;
end;

procedure TKMLGroundOverlay.LookAtRemove;
begin
  if FLookAtExsit then
  begin
    FLookAt.Free;
    FLookAtExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddLookAtEvent(Sender: TObject);
begin
  AddLookAt;
  FLookAt.ToTree;
end;

function TKMLGroundOverlay.AddIcon: TKMLIcon;
begin;
  if not FIconExsit then
    FIcon := TKMLIcon.Create(Self);
  Result := FIcon;
  FIconExsit := True;
end;

procedure TKMLGroundOverlay.SetIcon(const _Value: TKMLIcon);
begin
  if FIconExsit then
    FIcon.Free;
  FIconExsit := True;
  FIcon := _Value;
  FIcon.Parent := Self;
end;

procedure TKMLGroundOverlay.IconRemove;
begin
  if FIconExsit then
  begin
    FIcon.Free;
    FIconExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddIconEvent(Sender: TObject);
begin
  AddIcon;
  FIcon.ToTree;
end;

function TKMLGroundOverlay.AddLatLonBox: TKMLLatLonBox;
begin;
  if not FLatLonBoxExsit then
    FLatLonBox := TKMLLatLonBox.Create(Self);
  Result := FLatLonBox;
  FLatLonBoxExsit := True;
end;

procedure TKMLGroundOverlay.SetLatLonBox(const _Value: TKMLLatLonBox);
begin
  if FLatLonBoxExsit then
    FLatLonBox.Free;
  FLatLonBoxExsit := True;
  FLatLonBox := _Value;
  FLatLonBox.Parent := Self;
end;

procedure TKMLGroundOverlay.LatLonBoxRemove;
begin
  if FLatLonBoxExsit then
  begin
    FLatLonBox.Free;
    FLatLonBoxExsit := False;
  end;
end;

procedure TKMLGroundOverlay.AddLatLonBoxEvent(Sender: TObject);
begin
  AddLatLonBox;
  FLatLonBox.ToTree;
end;

{ ExtendedData }
constructor TKMLExtendedData.Create(par: TXML = nil);
begin
  inherited Create(par);
  FDatas := TList<TKMLData>.Create;
end;

destructor TKMLExtendedData.Destroy;
begin
  DataClear;
  FDatas.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLExtendedData.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  DataTmp: TKMLData;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Data' then
      begin
        DataTmp := TKMLData.Create(Self);
        DataTmp.FromXML(nodeTmp);
        FDatas.Add(DataTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('ExtendedData Read XML Error!' + node.Xml);
  end;
end;

function TKMLExtendedData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'ExtendedData';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FDatas.Count - 1 do
      FDatas.Items[I].ToXML(node, 'Data');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLExtendedData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to DataCount - 1 do
  begin
    Datas[I].TreeNodeShape := TreeNodeShape.AddChildObject('Data', Data[I]);
    Data[I].ToTree;
  end;
end;

procedure TKMLExtendedData.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  DataAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLExtendedDataPop) and Assigned(TKMLExtendedDataTreeComponent)
    then
    begin
      TKMLExtendedDataPop.Clear;
      DataAddMenu := TMenuItem.Create(TKMLExtendedDataPop);
      DataAddMenu.text := 'Add Data';
      DataAddMenu.OnClick := AddDataEvent;
      TKMLExtendedDataPop.AddObject(DataAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLExtendedDataTreeComponent.ClientToScreen(pt);
      TKMLExtendedDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLExtendedData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLExtendedDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TKMLExtendedDataXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLExtendedData.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

function TKMLExtendedData.AddData: TKMLData;
var
  DataTmp: TKMLData;
begin;
  DataTmp := TKMLData.Create(Self);
  FDatas.Add(DataTmp);
  Result := DataTmp;
end;

procedure TKMLExtendedData.SetDatas(const _Value: TList<TKMLData>);
begin
  DataClear;
  FDatas := _Value;
end;

procedure TKMLExtendedData.DataClear;
begin
  while FDatas.Count > 0 do
  begin
    FDatas.Items[0].Free;
    FDatas.Delete(0);
  end;
end;

function TKMLExtendedData.DataCount: Integer;
begin
  Result := FDatas.Count;
end;

function TKMLExtendedData.GetData(Index: Integer): TKMLData;
begin
  Result := FDatas[Index];
end;

procedure TKMLExtendedData.SetData(Index: Integer; const _Value: TKMLData);
begin
  _Value.Parent := Self;
  FDatas[Index].Free;
  FDatas[Index] := _Value;
end;

procedure TKMLExtendedData.RemoveData(_Value: TKMLData);
begin
  FDatas.Remove(_Value);
  _Value.Free;
end;

procedure TKMLExtendedData.DeleteData(Index: Integer);
begin
  FDatas.Items[Index].Free;
  FDatas.Delete(Index);
end;

procedure TKMLExtendedData.AddDataEvent(Sender: TObject);
var
  tmp: TKMLData;
begin
  tmp := AddData;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Data', tmp);
  tmp.ToTree;
end;

{ Data }
constructor TKMLData.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLData.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLData.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'value' then
      begin
        FValue := nodeTmp.text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
      end;
    end;
  except
    raise Exception.Create('Data Read XML Error!' + node.Xml);
  end;
end;

function TKMLData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  ValueTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Data';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    ValueTmp := doc.CreateNode('value', ntElement);
    ValueTmp.NodeValue := FValue;
    node.ChildNodes.Add(ValueTmp);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Name');
  TreeNodeShape.AddChild('Value');
end;

procedure TKMLData.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLDataPop) and Assigned(TKMLDataTreeComponent) then
    begin
      TKMLDataPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLDataTreeComponent.ClientToScreen(pt);
      TKMLDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Value');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Value);
  TKMLDataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLData.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Value := _Value;
      end;
  end;
  ToTree;
end;

procedure TKMLData.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TKMLData.SetValue(const _Value: String);
begin
  FValue := _Value;
end;

{ Size }
constructor TKMLSize.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLSize.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLSize.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'x' then
      begin
        FX := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'y' then
      begin
        FY := nodeTmp.text.ToDouble;
      end
      else if nodeTmp.NodeName = 'xunits' then
      begin
        FXUnits := nodeTmp.text;
      end
      else if nodeTmp.NodeName = 'yunits' then
      begin
        FYUnits := nodeTmp.text;
      end;
    end;
  except
    raise Exception.Create('Size Read XML Error!' + node.Xml);
  end;
end;

function TKMLSize.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  XTmp: IXMLNode;
  YTmp: IXMLNode;
  XUnitsTmp: IXMLNode;
  YUnitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Size';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    XTmp := doc.CreateNode('x', ntAttribute);
    XTmp.NodeValue := FX.toString;
    node.AttributeNodes.Add(XTmp);
    YTmp := doc.CreateNode('y', ntAttribute);
    YTmp.NodeValue := FY.toString;
    node.AttributeNodes.Add(YTmp);
    XUnitsTmp := doc.CreateNode('xunits', ntAttribute);
    XUnitsTmp.NodeValue := FXUnits;
    node.AttributeNodes.Add(XUnitsTmp);
    YUnitsTmp := doc.CreateNode('yunits', ntAttribute);
    YUnitsTmp.NodeValue := FYUnits;
    node.AttributeNodes.Add(YUnitsTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLSize.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('X');
  TreeNodeShape.AddChild('Y');
  TreeNodeShape.AddChild('XUnits');
  TreeNodeShape.AddChild('YUnits');
end;

procedure TKMLSize.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLSizePop) and Assigned(TKMLSizeTreeComponent) then
    begin
      TKMLSizePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLSizeTreeComponent.ClientToScreen(pt);
      TKMLSizePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLSize.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLSizeXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('X');
  Types_Value.Add(xs_double);
  _Values_Value.Add(X.toString);
  Names_Value.Add('Y');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Y.toString);
  Names_Value.Add('XUnits');
  Types_Value.Add(xs_string);
  _Values_Value.Add(XUnits);
  Names_Value.Add('YUnits');
  Types_Value.Add(xs_string);
  _Values_Value.Add(YUnits);
  TKMLSizeXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLSize.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        X := _Value.ToDouble;
      end;
    1:
      begin
        Y := _Value.ToDouble;
      end;
    2:
      begin
        XUnits := _Value;
      end;
    3:
      begin
        YUnits := _Value;
      end;
  end;
  ToTree;
end;

procedure TKMLSize.SetX(const _Value: Double);
begin
  FX := _Value;
end;

procedure TKMLSize.SetY(const _Value: Double);
begin
  FY := _Value;
end;

procedure TKMLSize.SetXUnits(const _Value: String);
begin
  FXUnits := _Value;
end;

procedure TKMLSize.SetYUnits(const _Value: String);
begin
  FYUnits := _Value;
end;

{ Style }
constructor TKMLStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLStyle.Destroy;
begin
  if FIconStyleExsit then
    FIconStyle.Free;
  if FLineStyleExsit then
    FLineStyle.Free;
  if FPolyStyleExsit then
    FPolyStyle.Free;
  if FBalloonStyleExsit then
    FBalloonStyle.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'IconStyle' then
      begin
        FIconStyle := TKMLIconStyle.Create(Self);
        FIconStyle.FromXML(nodeTmp);
        FIconStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'LineStyle' then
      begin
        FLineStyle := TKMLLineStyle.Create(Self);
        FLineStyle.FromXML(nodeTmp);
        FLineStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'PolyStyle' then
      begin
        FPolyStyle := TKMLLineStyle.Create(Self);
        FPolyStyle.FromXML(nodeTmp);
        FPolyStyleExsit := True;
      end
      else if nodeTmp.NodeName = 'BalloonStyle' then
      begin
        FBalloonStyle := TKMLBalloonStyle.Create(Self);
        FBalloonStyle.FromXML(nodeTmp);
        FBalloonStyleExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'id' then
      begin
        FID := nodeTmp.text;
      end;
    end;
  except
    raise Exception.Create('Style Read XML Error!' + node.Xml);
  end;
end;

function TKMLStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  IDTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Style';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FIconStyleExsit then
      FIconStyle.ToXML(node, 'IconStyle');
    if FLineStyleExsit then
      FLineStyle.ToXML(node, 'LineStyle');
    if FPolyStyleExsit then
      FPolyStyle.ToXML(node, 'PolyStyle');
    if FBalloonStyleExsit then
      FBalloonStyle.ToXML(node, 'BalloonStyle');
    IDTmp := doc.CreateNode('id', ntAttribute);
    IDTmp.NodeValue := FID;
    node.AttributeNodes.Add(IDTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('ID');
  if IconStyleExsit then
  begin
    IconStyle.TreeNodeShape := TreeNodeShape.AddChildObject('IconStyle',
      IconStyle);
    IconStyle.ToTree;
  end;
  if LineStyleExsit then
  begin
    LineStyle.TreeNodeShape := TreeNodeShape.AddChildObject('LineStyle',
      LineStyle);
    LineStyle.ToTree;
  end;
  if PolyStyleExsit then
  begin
    PolyStyle.TreeNodeShape := TreeNodeShape.AddChildObject('PolyStyle',
      PolyStyle);
    PolyStyle.ToTree;
  end;
  if BalloonStyleExsit then
  begin
    BalloonStyle.TreeNodeShape := TreeNodeShape.AddChildObject('BalloonStyle',
      BalloonStyle);
    BalloonStyle.ToTree;
  end;
end;

procedure TKMLStyle.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  IconStyleAddMenu: TMenuItem;
  LineStyleAddMenu: TMenuItem;
  PolyStyleAddMenu: TMenuItem;
  BalloonStyleAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLStylePop) and Assigned(TKMLStyleTreeComponent) then
    begin
      TKMLStylePop.Clear;
      IconStyleAddMenu := TMenuItem.Create(TKMLStylePop);
      IconStyleAddMenu.text := 'Add IconStyle';
      IconStyleAddMenu.OnClick := AddIconStyleEvent;
      TKMLStylePop.AddObject(IconStyleAddMenu);
      LineStyleAddMenu := TMenuItem.Create(TKMLStylePop);
      LineStyleAddMenu.text := 'Add LineStyle';
      LineStyleAddMenu.OnClick := AddLineStyleEvent;
      TKMLStylePop.AddObject(LineStyleAddMenu);
      PolyStyleAddMenu := TMenuItem.Create(TKMLStylePop);
      PolyStyleAddMenu.text := 'Add PolyStyle';
      PolyStyleAddMenu.OnClick := AddPolyStyleEvent;
      TKMLStylePop.AddObject(PolyStyleAddMenu);
      BalloonStyleAddMenu := TMenuItem.Create(TKMLStylePop);
      BalloonStyleAddMenu.text := 'Add BalloonStyle';
      BalloonStyleAddMenu.OnClick := AddBalloonStyleEvent;
      TKMLStylePop.AddObject(BalloonStyleAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLStyleTreeComponent.ClientToScreen(pt);
      TKMLStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('ID');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ID);
  TKMLStyleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        ID := _Value;
      end;
  end;
  ToTree;
end;

procedure TKMLStyle.SetID(const _Value: String);
begin
  FID := _Value;
end;

function TKMLStyle.AddIconStyle: TKMLIconStyle;
begin;
  if not FIconStyleExsit then
    FIconStyle := TKMLIconStyle.Create(Self);
  Result := FIconStyle;
  FIconStyleExsit := True;
end;

procedure TKMLStyle.SetIconStyle(const _Value: TKMLIconStyle);
begin
  if FIconStyleExsit then
    FIconStyle.Free;
  FIconStyleExsit := True;
  FIconStyle := _Value;
  FIconStyle.Parent := Self;
end;

procedure TKMLStyle.IconStyleRemove;
begin
  if FIconStyleExsit then
  begin
    FIconStyle.Free;
    FIconStyleExsit := False;
  end;
end;

procedure TKMLStyle.AddIconStyleEvent(Sender: TObject);
begin
  AddIconStyle;
  FIconStyle.ToTree;
end;

function TKMLStyle.AddLineStyle: TKMLLineStyle;
begin;
  if not FLineStyleExsit then
    FLineStyle := TKMLLineStyle.Create(Self);
  Result := FLineStyle;
  FLineStyleExsit := True;
end;

procedure TKMLStyle.SetLineStyle(const _Value: TKMLLineStyle);
begin
  if FLineStyleExsit then
    FLineStyle.Free;
  FLineStyleExsit := True;
  FLineStyle := _Value;
  FLineStyle.Parent := Self;
end;

procedure TKMLStyle.LineStyleRemove;
begin
  if FLineStyleExsit then
  begin
    FLineStyle.Free;
    FLineStyleExsit := False;
  end;
end;

procedure TKMLStyle.AddLineStyleEvent(Sender: TObject);
begin
  AddLineStyle;
  FLineStyle.ToTree;
end;

function TKMLStyle.AddPolyStyle: TKMLLineStyle;
begin;
  if not FPolyStyleExsit then
    FPolyStyle := TKMLLineStyle.Create(Self);
  Result := FPolyStyle;
  FPolyStyleExsit := True;
end;

procedure TKMLStyle.SetPolyStyle(const _Value: TKMLLineStyle);
begin
  if FPolyStyleExsit then
    FPolyStyle.Free;
  FPolyStyleExsit := True;
  FPolyStyle := _Value;
  FPolyStyle.Parent := Self;
end;

procedure TKMLStyle.PolyStyleRemove;
begin
  if FPolyStyleExsit then
  begin
    FPolyStyle.Free;
    FPolyStyleExsit := False;
  end;
end;

procedure TKMLStyle.AddPolyStyleEvent(Sender: TObject);
begin
  AddPolyStyle;
  FPolyStyle.ToTree;
end;

function TKMLStyle.AddBalloonStyle: TKMLBalloonStyle;
begin;
  if not FBalloonStyleExsit then
    FBalloonStyle := TKMLBalloonStyle.Create(Self);
  Result := FBalloonStyle;
  FBalloonStyleExsit := True;
end;

procedure TKMLStyle.SetBalloonStyle(const _Value: TKMLBalloonStyle);
begin
  if FBalloonStyleExsit then
    FBalloonStyle.Free;
  FBalloonStyleExsit := True;
  FBalloonStyle := _Value;
  FBalloonStyle.Parent := Self;
end;

procedure TKMLStyle.BalloonStyleRemove;
begin
  if FBalloonStyleExsit then
  begin
    FBalloonStyle.Free;
    FBalloonStyleExsit := False;
  end;
end;

procedure TKMLStyle.AddBalloonStyleEvent(Sender: TObject);
begin
  AddBalloonStyle;
  FBalloonStyle.ToTree;
end;

{ IconStyle }
constructor TKMLIconStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
  FIcon := TKMLIcon.Create(Self);
end;

destructor TKMLIconStyle.Destroy;
begin
  FIcon.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLIconStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Icon' then
      begin
        FIcon := TKMLIcon.Create(Self);
        FIcon.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('IconStyle Read XML Error!' + node.Xml);
  end;
end;

function TKMLIconStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'IconStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FIcon.ToXML(node, 'Icon');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLIconStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  Icon.TreeNodeShape := TreeNodeShape.AddChildObject('Icon', Icon);
  Icon.ToTree;
end;

procedure TKMLIconStyle.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLIconStylePop) and Assigned(TKMLIconStyleTreeComponent) then
    begin
      TKMLIconStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLIconStyleTreeComponent.ClientToScreen(pt);
      TKMLIconStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLIconStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLIconStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TKMLIconStyleXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLIconStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

procedure TKMLIconStyle.SetIcon(const _Value: TKMLIcon);
begin
  FIcon.Free;
  FIcon := _Value;
  FIcon.Parent := Self;
end;

{ Icon }
constructor TKMLIcon.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLIcon.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLIcon.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'href' then
      begin
        Fhref := nodeTmp.text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Icon Read XML Error!' + node.Xml);
  end;
end;

function TKMLIcon.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  hrefTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Icon';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    hrefTmp := doc.CreateNode('href', ntElement);
    hrefTmp.NodeValue := Fhref;
    node.ChildNodes.Add(hrefTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLIcon.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('href');
end;

procedure TKMLIcon.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLIconPop) and Assigned(TKMLIconTreeComponent) then
    begin
      TKMLIconPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLIconTreeComponent.ClientToScreen(pt);
      TKMLIconPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLIcon.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLIconXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('href');
  Types_Value.Add(xs_string);
  _Values_Value.Add(href);
  TKMLIconXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLIcon.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        href := _Value;
      end;
  end;
  ToTree;
end;

procedure TKMLIcon.Sethref(const _Value: String);
begin
  Fhref := _Value;
end;

{ LineStyle }
constructor TKMLLineStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLLineStyle.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLLineStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'color' then
      begin
        Fcolor := nodeTmp.text;
        FcolorExsit := True;
      end
      else if nodeTmp.NodeName = 'width' then
      begin
        Fwidth := nodeTmp.text.ToDouble;
        FwidthExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('LineStyle Read XML Error!' + node.Xml);
  end;
end;

function TKMLLineStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  colorTmp: IXMLNode;
  widthTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'LineStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FcolorExsit then
    begin
      colorTmp := doc.CreateNode('color', ntElement);
      colorTmp.NodeValue := Fcolor;
      node.ChildNodes.Add(colorTmp);
    end;
    if FwidthExsit then
    begin
      widthTmp := doc.CreateNode('width', ntElement);
      widthTmp.NodeValue := Fwidth.toString;
      node.ChildNodes.Add(widthTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLLineStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if colorExsit then
    TreeNodeShape.AddChild('color');
  if widthExsit then
    TreeNodeShape.AddChild('width');
end;

procedure TKMLLineStyle.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  colorAddMenu: TMenuItem;
  widthAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLLineStylePop) and Assigned(TKMLLineStyleTreeComponent) then
    begin
      TKMLLineStylePop.Clear;
      colorAddMenu := TMenuItem.Create(TKMLLineStylePop);
      colorAddMenu.text := 'Add color';
      colorAddMenu.OnClick := AddcolorEvent;
      TKMLLineStylePop.AddObject(colorAddMenu);
      widthAddMenu := TMenuItem.Create(TKMLLineStylePop);
      widthAddMenu.text := 'Add width';
      widthAddMenu.OnClick := AddwidthEvent;
      TKMLLineStylePop.AddObject(widthAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLLineStyleTreeComponent.ClientToScreen(pt);
      TKMLLineStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLLineStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLLineStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('color');
  Types_Value.Add(xs_string);
  _Values_Value.Add(color);
  Names_Value.Add('width');
  Types_Value.Add(xs_double);
  _Values_Value.Add(width.toString);
  TKMLLineStyleXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLLineStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        color := _Value;
      end;
    1:
      begin
        width := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TKMLLineStyle.Addcolor: String;
begin;
  Result := Fcolor;
  FcolorExsit := True;
end;

procedure TKMLLineStyle.Setcolor(const _Value: String);
begin
  FcolorExsit := True;
  Fcolor := _Value;
end;

procedure TKMLLineStyle.colorRemove;
begin
  if FcolorExsit then
  begin
    FcolorExsit := False;
  end;
end;

procedure TKMLLineStyle.AddcolorEvent(Sender: TObject);
begin
  Addcolor;
end;

function TKMLLineStyle.Addwidth: Double;
begin;
  Result := Fwidth;
  FwidthExsit := True;
end;

procedure TKMLLineStyle.Setwidth(const _Value: Double);
begin
  FwidthExsit := True;
  Fwidth := _Value;
end;

procedure TKMLLineStyle.widthRemove;
begin
  if FwidthExsit then
  begin
    FwidthExsit := False;
  end;
end;

procedure TKMLLineStyle.AddwidthEvent(Sender: TObject);
begin
  Addwidth;
end;

{ BalloonStyle }
constructor TKMLBalloonStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLBalloonStyle.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLBalloonStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = '~text' then
      begin
        Ftext := nodeTmp.text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('BalloonStyle Read XML Error!' + node.Xml);
  end;
end;

function TKMLBalloonStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  textTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'BalloonStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    textTmp := doc.CreateNode('~ext', ntCData);
    textTmp.NodeValue := Ftext;
    node.ChildNodes.Add(textTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLBalloonStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('text');
end;

procedure TKMLBalloonStyle.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLBalloonStylePop) and Assigned(TKMLBalloonStyleTreeComponent)
    then
    begin
      TKMLBalloonStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TKMLBalloonStyleTreeComponent.ClientToScreen(pt);
      TKMLBalloonStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLBalloonStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLBalloonStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('text');
  Types_Value.Add(xs_string);
  _Values_Value.Add(text);
  TKMLBalloonStyleXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLBalloonStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        text := _Value;
      end;
  end;
  ToTree;
end;

procedure TKMLBalloonStyle.Settext(const _Value: String);
begin
  Ftext := _Value;
end;

{ StyleMap }
constructor TKMLStyleMap.Create(par: TXML = nil);
begin
  inherited Create(par);
  FPairs := TList<TKMLPair>.Create;
end;

destructor TKMLStyleMap.Destroy;
begin
  PairClear;
  FPairs.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLStyleMap.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  PairTmp: TKMLPair;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Pair' then
      begin
        PairTmp := TKMLPair.Create(Self);
        PairTmp.FromXML(nodeTmp);
        FPairs.Add(PairTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'id' then
      begin
        FID := nodeTmp.text;
      end;
    end;
  except
    raise Exception.Create('StyleMap Read XML Error!' + node.Xml);
  end;
end;

function TKMLStyleMap.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  IDTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'StyleMap';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FPairs.Count - 1 do
      FPairs.Items[I].ToXML(node, 'Pair');
    IDTmp := doc.CreateNode('id', ntAttribute);
    IDTmp.NodeValue := FID;
    node.AttributeNodes.Add(IDTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLStyleMap.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to PairCount - 1 do
  begin
    Pairs[I].TreeNodeShape := TreeNodeShape.AddChildObject('Pair', Pair[I]);
    Pair[I].ToTree;
  end;
  TreeNodeShape.AddChild('ID');
end;

procedure TKMLStyleMap.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  PairAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLStyleMapPop) and Assigned(TKMLStyleMapTreeComponent) then
    begin
      TKMLStyleMapPop.Clear;
      PairAddMenu := TMenuItem.Create(TKMLStyleMapPop);
      PairAddMenu.text := 'Add Pair';
      PairAddMenu.OnClick := AddPairEvent;
      TKMLStyleMapPop.AddObject(PairAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLStyleMapTreeComponent.ClientToScreen(pt);
      TKMLStyleMapPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLStyleMap.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLStyleMapXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('ID');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ID);
  TKMLStyleMapXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLStyleMap.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        ID := _Value;
      end;
  end;
  ToTree;
end;

function TKMLStyleMap.AddPair: TKMLPair;
var
  PairTmp: TKMLPair;
begin;
  PairTmp := TKMLPair.Create(Self);
  FPairs.Add(PairTmp);
  Result := PairTmp;
end;

procedure TKMLStyleMap.SetPairs(const _Value: TList<TKMLPair>);
begin
  PairClear;
  FPairs := _Value;
end;

procedure TKMLStyleMap.PairClear;
begin
  while FPairs.Count > 0 do
  begin
    FPairs.Items[0].Free;
    FPairs.Delete(0);
  end;
end;

function TKMLStyleMap.PairCount: Integer;
begin
  Result := FPairs.Count;
end;

function TKMLStyleMap.GetPair(Index: Integer): TKMLPair;
begin
  Result := FPairs[Index];
end;

procedure TKMLStyleMap.SetPair(Index: Integer; const _Value: TKMLPair);
begin
  _Value.Parent := Self;
  FPairs[Index].Free;
  FPairs[Index] := _Value;
end;

procedure TKMLStyleMap.RemovePair(_Value: TKMLPair);
begin
  FPairs.Remove(_Value);
  _Value.Free;
end;

procedure TKMLStyleMap.DeletePair(Index: Integer);
begin
  FPairs.Items[Index].Free;
  FPairs.Delete(Index);
end;

procedure TKMLStyleMap.AddPairEvent(Sender: TObject);
var
  tmp: TKMLPair;
begin
  tmp := AddPair;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Pair', tmp);
  tmp.ToTree;
end;

procedure TKMLStyleMap.SetID(const _Value: String);
begin
  FID := _Value;
end;

{ ScreenOverlay }
constructor TKMLScreenOverlay.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLScreenOverlay.Destroy;
begin
  if FIconExsit then
    FIcon.Free;
  if FOverlayXYExsit then
    FOverlayXY.Free;
  if FScreenXYExsit then
    FScreenXY.Free;
  if FRotationXYExsit then
    FRotationXY.Free;
  if FSizeExsit then
    FSize.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLScreenOverlay.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'visibility' then
      begin
        FVisibility := nodeTmp.text.ToInteger;
        FVisibilityExsit := True;
      end
      else if nodeTmp.NodeName = 'description' then
      begin
        FDescription := nodeTmp.text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'Icon' then
      begin
        FIcon := TKMLIcon.Create(Self);
        FIcon.FromXML(nodeTmp);
        FIconExsit := True;
      end
      else if nodeTmp.NodeName = 'overlayXY' then
      begin
        FOverlayXY := TKMLSize.Create(Self);
        FOverlayXY.FromXML(nodeTmp);
        FOverlayXYExsit := True;
      end
      else if nodeTmp.NodeName = 'screenXY' then
      begin
        FScreenXY := TKMLSize.Create(Self);
        FScreenXY.FromXML(nodeTmp);
        FScreenXYExsit := True;
      end
      else if nodeTmp.NodeName = 'rotationXY' then
      begin
        FRotationXY := TKMLSize.Create(Self);
        FRotationXY.FromXML(nodeTmp);
        FRotationXYExsit := True;
      end
      else if nodeTmp.NodeName = 'size' then
      begin
        FSize := TKMLSize.Create(Self);
        FSize.FromXML(nodeTmp);
        FSizeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('ScreenOverlay Read XML Error!' + node.Xml);
  end;
end;

function TKMLScreenOverlay.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  VisibilityTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'ScreenOverlay';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FVisibilityExsit then
    begin
      VisibilityTmp := doc.CreateNode('visibility', ntElement);
      VisibilityTmp.NodeValue := FVisibility.toString;
      node.ChildNodes.Add(VisibilityTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FIconExsit then
      FIcon.ToXML(node, 'Icon');
    if FOverlayXYExsit then
      FOverlayXY.ToXML(node, 'overlayXY');
    if FScreenXYExsit then
      FScreenXY.ToXML(node, 'screenXY');
    if FRotationXYExsit then
      FRotationXY.ToXML(node, 'rotationXY');
    if FSizeExsit then
      FSize.ToXML(node, 'size');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLScreenOverlay.ToTree;
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
  if IconExsit then
  begin
    Icon.TreeNodeShape := TreeNodeShape.AddChildObject('Icon', Icon);
    Icon.ToTree;
  end;
  if OverlayXYExsit then
  begin
    OverlayXY.TreeNodeShape := TreeNodeShape.AddChildObject('OverlayXY',
      OverlayXY);
    OverlayXY.ToTree;
  end;
  if ScreenXYExsit then
  begin
    ScreenXY.TreeNodeShape := TreeNodeShape.AddChildObject('ScreenXY',
      ScreenXY);
    ScreenXY.ToTree;
  end;
  if RotationXYExsit then
  begin
    RotationXY.TreeNodeShape := TreeNodeShape.AddChildObject('RotationXY',
      RotationXY);
    RotationXY.ToTree;
  end;
  if SizeExsit then
  begin
    Size.TreeNodeShape := TreeNodeShape.AddChildObject('Size', Size);
    Size.ToTree;
  end;
end;

procedure TKMLScreenOverlay.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  VisibilityAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  IconAddMenu: TMenuItem;
  OverlayXYAddMenu: TMenuItem;
  ScreenXYAddMenu: TMenuItem;
  RotationXYAddMenu: TMenuItem;
  SizeAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLScreenOverlayPop) and
      Assigned(TKMLScreenOverlayTreeComponent) then
    begin
      TKMLScreenOverlayPop.Clear;
      NameAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      NameAddMenu.text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TKMLScreenOverlayPop.AddObject(NameAddMenu);
      VisibilityAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      VisibilityAddMenu.text := 'Add Visibility';
      VisibilityAddMenu.OnClick := AddVisibilityEvent;
      TKMLScreenOverlayPop.AddObject(VisibilityAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      DescriptionAddMenu.text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TKMLScreenOverlayPop.AddObject(DescriptionAddMenu);
      IconAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      IconAddMenu.text := 'Add Icon';
      IconAddMenu.OnClick := AddIconEvent;
      TKMLScreenOverlayPop.AddObject(IconAddMenu);
      OverlayXYAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      OverlayXYAddMenu.text := 'Add OverlayXY';
      OverlayXYAddMenu.OnClick := AddOverlayXYEvent;
      TKMLScreenOverlayPop.AddObject(OverlayXYAddMenu);
      ScreenXYAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      ScreenXYAddMenu.text := 'Add ScreenXY';
      ScreenXYAddMenu.OnClick := AddScreenXYEvent;
      TKMLScreenOverlayPop.AddObject(ScreenXYAddMenu);
      RotationXYAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      RotationXYAddMenu.text := 'Add RotationXY';
      RotationXYAddMenu.OnClick := AddRotationXYEvent;
      TKMLScreenOverlayPop.AddObject(RotationXYAddMenu);
      SizeAddMenu := TMenuItem.Create(TKMLScreenOverlayPop);
      SizeAddMenu.text := 'Add Size';
      SizeAddMenu.OnClick := AddSizeEvent;
      TKMLScreenOverlayPop.AddObject(SizeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLScreenOverlayTreeComponent.ClientToScreen(pt);
      TKMLScreenOverlayPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLScreenOverlay.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLScreenOverlayXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Visibility');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Visibility.toString);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TKMLScreenOverlayXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TKMLScreenOverlay.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Visibility := _Value.ToInteger;
      end;
    2:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

function TKMLScreenOverlay.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TKMLScreenOverlay.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TKMLScreenOverlay.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TKMLScreenOverlay.AddVisibility: Integer;
begin;
  Result := FVisibility;
  FVisibilityExsit := True;
end;

procedure TKMLScreenOverlay.SetVisibility(const _Value: Integer);
begin
  FVisibilityExsit := True;
  FVisibility := _Value;
end;

procedure TKMLScreenOverlay.VisibilityRemove;
begin
  if FVisibilityExsit then
  begin
    FVisibilityExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddVisibilityEvent(Sender: TObject);
begin
  AddVisibility;
end;

function TKMLScreenOverlay.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TKMLScreenOverlay.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TKMLScreenOverlay.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TKMLScreenOverlay.AddIcon: TKMLIcon;
begin;
  if not FIconExsit then
    FIcon := TKMLIcon.Create(Self);
  Result := FIcon;
  FIconExsit := True;
end;

procedure TKMLScreenOverlay.SetIcon(const _Value: TKMLIcon);
begin
  if FIconExsit then
    FIcon.Free;
  FIconExsit := True;
  FIcon := _Value;
  FIcon.Parent := Self;
end;

procedure TKMLScreenOverlay.IconRemove;
begin
  if FIconExsit then
  begin
    FIcon.Free;
    FIconExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddIconEvent(Sender: TObject);
begin
  AddIcon;
  FIcon.ToTree;
end;

function TKMLScreenOverlay.AddOverlayXY: TKMLSize;
begin;
  if not FOverlayXYExsit then
    FOverlayXY := TKMLSize.Create(Self);
  Result := FOverlayXY;
  FOverlayXYExsit := True;
end;

procedure TKMLScreenOverlay.SetOverlayXY(const _Value: TKMLSize);
begin
  if FOverlayXYExsit then
    FOverlayXY.Free;
  FOverlayXYExsit := True;
  FOverlayXY := _Value;
  FOverlayXY.Parent := Self;
end;

procedure TKMLScreenOverlay.OverlayXYRemove;
begin
  if FOverlayXYExsit then
  begin
    FOverlayXY.Free;
    FOverlayXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddOverlayXYEvent(Sender: TObject);
begin
  AddOverlayXY;
  FOverlayXY.ToTree;
end;

function TKMLScreenOverlay.AddScreenXY: TKMLSize;
begin;
  if not FScreenXYExsit then
    FScreenXY := TKMLSize.Create(Self);
  Result := FScreenXY;
  FScreenXYExsit := True;
end;

procedure TKMLScreenOverlay.SetScreenXY(const _Value: TKMLSize);
begin
  if FScreenXYExsit then
    FScreenXY.Free;
  FScreenXYExsit := True;
  FScreenXY := _Value;
  FScreenXY.Parent := Self;
end;

procedure TKMLScreenOverlay.ScreenXYRemove;
begin
  if FScreenXYExsit then
  begin
    FScreenXY.Free;
    FScreenXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddScreenXYEvent(Sender: TObject);
begin
  AddScreenXY;
  FScreenXY.ToTree;
end;

function TKMLScreenOverlay.AddRotationXY: TKMLSize;
begin;
  if not FRotationXYExsit then
    FRotationXY := TKMLSize.Create(Self);
  Result := FRotationXY;
  FRotationXYExsit := True;
end;

procedure TKMLScreenOverlay.SetRotationXY(const _Value: TKMLSize);
begin
  if FRotationXYExsit then
    FRotationXY.Free;
  FRotationXYExsit := True;
  FRotationXY := _Value;
  FRotationXY.Parent := Self;
end;

procedure TKMLScreenOverlay.RotationXYRemove;
begin
  if FRotationXYExsit then
  begin
    FRotationXY.Free;
    FRotationXYExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddRotationXYEvent(Sender: TObject);
begin
  AddRotationXY;
  FRotationXY.ToTree;
end;

function TKMLScreenOverlay.AddSize: TKMLSize;
begin;
  if not FSizeExsit then
    FSize := TKMLSize.Create(Self);
  Result := FSize;
  FSizeExsit := True;
end;

procedure TKMLScreenOverlay.SetSize(const _Value: TKMLSize);
begin
  if FSizeExsit then
    FSize.Free;
  FSizeExsit := True;
  FSize := _Value;
  FSize.Parent := Self;
end;

procedure TKMLScreenOverlay.SizeRemove;
begin
  if FSizeExsit then
  begin
    FSize.Free;
    FSizeExsit := False;
  end;
end;

procedure TKMLScreenOverlay.AddSizeEvent(Sender: TObject);
begin
  AddSize;
  FSize.ToTree;
end;

{ Pair }
constructor TKMLPair.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TKMLPair.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TKMLPair.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'key' then
      begin
        FKey := nodeTmp.text;
        FKeyExsit := True;
      end
      else if nodeTmp.NodeName = 'styleUrl' then
      begin
        FStyleUrl := nodeTmp.text;
        FStyleUrlExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);;
    end;
  except
    raise Exception.Create('Pair Read XML Error!' + node.Xml);
  end;
end;

function TKMLPair.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  KeyTmp: IXMLNode;
  StyleUrlTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Pair';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FKeyExsit then
    begin
      KeyTmp := doc.CreateNode('key', ntElement);
      KeyTmp.NodeValue := FKey;
      node.ChildNodes.Add(KeyTmp);
    end;
    if FStyleUrlExsit then
    begin
      StyleUrlTmp := doc.CreateNode('styleUrl', ntElement);
      StyleUrlTmp.NodeValue := FStyleUrl;
      node.ChildNodes.Add(StyleUrlTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TKMLPair.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if KeyExsit then
    TreeNodeShape.AddChild('Key');
  if StyleUrlExsit then
    TreeNodeShape.AddChild('StyleUrl');
end;

procedure TKMLPair.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  KeyAddMenu: TMenuItem;
  StyleUrlAddMenu: TMenuItem;
begin
  ToInspector;
  KMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TKMLPairPop) and Assigned(TKMLPairTreeComponent) then
    begin
      TKMLPairPop.Clear;
      KeyAddMenu := TMenuItem.Create(TKMLPairPop);
      KeyAddMenu.text := 'Add Key';
      KeyAddMenu.OnClick := AddKeyEvent;
      TKMLPairPop.AddObject(KeyAddMenu);
      StyleUrlAddMenu := TMenuItem.Create(TKMLPairPop);
      StyleUrlAddMenu.text := 'Add StyleUrl';
      StyleUrlAddMenu.OnClick := AddStyleUrlEvent;
      TKMLPairPop.AddObject(StyleUrlAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TKMLPairTreeComponent.ClientToScreen(pt);
      TKMLPairPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TKMLPair.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TKMLPairXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Key');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Key);
  Names_Value.Add('StyleUrl');
  Types_Value.Add(xs_string);
  _Values_Value.Add(StyleUrl);
  TKMLPairXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TKMLPair.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Key := _Value;
      end;
    1:
      begin
        StyleUrl := _Value;
      end;
  end;
  ToTree;
end;

function TKMLPair.AddKey: String;
begin;
  Result := FKey;
  FKeyExsit := True;
end;

procedure TKMLPair.SetKey(const _Value: String);
begin
  FKeyExsit := True;
  FKey := _Value;
end;

procedure TKMLPair.KeyRemove;
begin
  if FKeyExsit then
  begin
    FKeyExsit := False;
  end;
end;

procedure TKMLPair.AddKeyEvent(Sender: TObject);
begin
  AddKey;
end;

function TKMLPair.AddStyleUrl: String;
begin;
  Result := FStyleUrl;
  FStyleUrlExsit := True;
end;

procedure TKMLPair.SetStyleUrl(const _Value: String);
begin
  FStyleUrlExsit := True;
  FStyleUrl := _Value;
end;

procedure TKMLPair.StyleUrlRemove;
begin
  if FStyleUrlExsit then
  begin
    FStyleUrlExsit := False;
  end;
end;

procedure TKMLPair.AddStyleUrlEvent(Sender: TObject);
begin
  AddStyleUrl;
end;

end.
