unit GPXBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,
  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,
  ClientScreen, XMLInspector;


type

  TGPX= class;
  TGPXElement= class;
  TGPXWayPoint= class;
  TGPXRoute= class;
  TGPXTrack= class;
  TGPXTrackSegment= class;
  TGPXMetadata= class;
  TGPXCopyright= class;
  TGPXLink= class;
  TGPXEmail= class;
  TGPXPerson= class;
  TGPXPoint= class;
  TGPXBounds= class;

  TGPX= class(TXML)
  private
    Fxmlns: String;
    FxmlnsExsit: Boolean;
    FCreator: String;
    FCreatorExsit: Boolean;
    FMetadata: TGPXMetadata;
    FMetadataExsit: Boolean;
    FElements: TList<TGPXElement>;
    FExtensionss: TList<TXMLBase>;
    FVersion: String;
    FVersionExsit: Boolean;
    Fxmlnsxsi: String;
    FxmlnsxsiExsit: Boolean;
    FxsischemaLocation: String;
    FxsischemaLocationExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setxmlns(const _Value: String);
    procedure SetCreator(const _Value: String);
    procedure SetMetadata(const _Value: TGPXMetadata);
    procedure SetElements(const _Value: TList<TGPXElement>);
    function GetElement(Index: Integer): TGPXElement;
    procedure SetElement(Index: Integer; const _Value: TGPXElement);
    procedure SetExtensionss(const _Value: TList<TXMLBase>);
    function GetExtensions(Index: Integer): TXMLBase;
    procedure SetExtensions(Index: Integer; const _Value: TXMLBase);
    procedure SetVersion(const _Value: String);
    procedure Setxmlnsxsi(const _Value: String);
    procedure SetxsischemaLocation(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddxmlnsEvent(Sender: TObject);
    procedure AddCreatorEvent(Sender: TObject);
    procedure AddMetadataEvent(Sender: TObject);
    procedure AddWayPointEvent(Sender: TObject);
    procedure AddRouteEvent(Sender: TObject);
    procedure AddTrackEvent(Sender: TObject);
    procedure AddExtensionsEvent(Sender: TObject);
    procedure AddVersionEvent(Sender: TObject);
    procedure AddxmlnsxsiEvent(Sender: TObject);
    procedure AddxsischemaLocationEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addxmlns: String;
    procedure xmlnsRemove;
    function AddCreator: String;
    procedure CreatorRemove;
    function AddMetadata: TGPXMetadata;
    procedure MetadataRemove;
    function AddWayPoint: TGPXWayPoint;
    function AddRoute: TGPXRoute;
    function AddTrack: TGPXTrack;
    procedure ElementClear;
    function ElementCount: Integer;
    procedure RemoveElement(_Value: TGPXElement);
    procedure DeleteElement(Index: Integer);
    function AddExtensions: TXMLBase;
    procedure ExtensionsClear;
    function ExtensionsCount: Integer;
    procedure RemoveExtensions(_Value: TXMLBase);
    procedure DeleteExtensions(Index: Integer);
    function AddVersion: String;
    procedure VersionRemove;
    function Addxmlnsxsi: String;
    procedure xmlnsxsiRemove;
    function AddxsischemaLocation: String;
    procedure xsischemaLocationRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property xmlns: String read Fxmlns write Setxmlns;
    property Creator: String read FCreator write SetCreator;
    property Metadata: TGPXMetadata read FMetadata write SetMetadata;
    property Elements: TList<TGPXElement> read FElements write SetElements;
    property Element[Index: Integer]: TGPXElement read GetElement write SetElement;
    property Extensionss: TList<TXMLBase> read FExtensionss write SetExtensionss;
    property Extensions[Index: Integer]: TXMLBase read GetExtensions write SetExtensions;
    property Version: String read FVersion write SetVersion;
    property xmlnsxsi: String read Fxmlnsxsi write Setxmlnsxsi;
    property xsischemaLocation: String read FxsischemaLocation write SetxsischemaLocation;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property xmlnsExsit: Boolean read FxmlnsExsit;
    property CreatorExsit: Boolean read FCreatorExsit;
    property MetadataExsit: Boolean read FMetadataExsit;
    property VersionExsit: Boolean read FVersionExsit;
    property xmlnsxsiExsit: Boolean read FxmlnsxsiExsit;
    property xsischemaLocationExsit: Boolean read FxsischemaLocationExsit;
  end;

  TGPXElement= class abstract(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FComment: String;
    FCommentExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FSource: String;
    FSourceExsit: Boolean;
    FExtensionss: TList<TXMLBase>;
    FLinks: TList<TGPXLink>;
    FTypeS: String;
    FTypeSExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetComment(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetSource(const _Value: String);
    procedure SetExtensionss(const _Value: TList<TXMLBase>);
    function GetExtensions(Index: Integer): TXMLBase;
    procedure SetExtensions(Index: Integer; const _Value: TXMLBase);
    procedure SetLinks(const _Value: TList<TGPXLink>);
    function GetLink(Index: Integer): TGPXLink;
    procedure SetLink(Index: Integer; const _Value: TGPXLink);
    procedure SetTypeS(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); virtual;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; virtual;
    function AppendToXML(node: IXMLNode; pt: string = ''): IXMLNode;
    procedure AddNameEvent(Sender: TObject);
    procedure AddCommentEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddSourceEvent(Sender: TObject);
    procedure AddExtensionsEvent(Sender: TObject);
    procedure AddLinkEvent(Sender: TObject);
    procedure AddTypeSEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddComment: String;
    procedure CommentRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddSource: String;
    procedure SourceRemove;
    function AddExtensions: TXMLBase;
    procedure ExtensionsClear;
    function ExtensionsCount: Integer;
    procedure RemoveExtensions(_Value: TXMLBase);
    procedure DeleteExtensions(Index: Integer);
    function AddLink: TGPXLink;
    procedure LinkClear;
    function LinkCount: Integer;
    procedure RemoveLink(_Value: TGPXLink);
    procedure DeleteLink(Index: Integer);
    function AddTypeS: String;
    procedure TypeSRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Comment: String read FComment write SetComment;
    property Description: String read FDescription write SetDescription;
    property Source: String read FSource write SetSource;
    property Extensionss: TList<TXMLBase> read FExtensionss write SetExtensionss;
    property Extensions[Index: Integer]: TXMLBase read GetExtensions write SetExtensions;
    property Links: TList<TGPXLink> read FLinks write SetLinks;
    property Link[Index: Integer]: TGPXLink read GetLink write SetLink;
    property TypeS: String read FTypeS write SetTypeS;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property CommentExsit: Boolean read FCommentExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property SourceExsit: Boolean read FSourceExsit;
    property TypeSExsit: Boolean read FTypeSExsit;
  end;

  TGPXWayPoint= class(TGPXElement)
  private
    FDgpsid: Integer;
    FDgpsidExsit: Boolean;
    FLatitude: String;
    FLatitudeExsit: Boolean;
    FLongitude: String;
    FLongitudeExsit: Boolean;
    FElevation: String;
    FElevationExsit: Boolean;
    FTime: TDateTime;
    FTimeExsit: Boolean;
    FGeoidheight: Double;
    FGeoidheightExsit: Boolean;
    FSymbol: String;
    FSymbolExsit: Boolean;
    FSatellites: Integer;
    FSatellitesExsit: Boolean;
    FHorizontal: Double;
    FHorizontalExsit: Boolean;
    FFix: String;
    FFixExsit: Boolean;
    FVertical: Double;
    FVerticalExsit: Boolean;
    FPosition: Double;
    FPositionExsit: Boolean;
    FAgeofdgpsdata: Double;
    FAgeofdgpsdataExsit: Boolean;
    FMagvar: Double;
    FMagvarExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetDgpsid(const _Value: Integer);
    procedure SetLatitude(const _Value: String);
    procedure SetLongitude(const _Value: String);
    procedure SetElevation(const _Value: String);
    procedure SetTime(const _Value: TDateTime);
    procedure SetGeoidheight(const _Value: Double);
    procedure SetSymbol(const _Value: String);
    procedure SetSatellites(const _Value: Integer);
    procedure SetHorizontal(const _Value: Double);
    procedure SetFix(const _Value: String);
    procedure SetVertical(const _Value: Double);
    procedure SetPosition(const _Value: Double);
    procedure SetAgeofdgpsdata(const _Value: Double);
    procedure SetMagvar(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddDgpsidEvent(Sender: TObject);
    procedure AddLatitudeEvent(Sender: TObject);
    procedure AddLongitudeEvent(Sender: TObject);
    procedure AddElevationEvent(Sender: TObject);
    procedure AddTimeEvent(Sender: TObject);
    procedure AddGeoidheightEvent(Sender: TObject);
    procedure AddSymbolEvent(Sender: TObject);
    procedure AddSatellitesEvent(Sender: TObject);
    procedure AddHorizontalEvent(Sender: TObject);
    procedure AddFixEvent(Sender: TObject);
    procedure AddVerticalEvent(Sender: TObject);
    procedure AddPositionEvent(Sender: TObject);
    procedure AddAgeofdgpsdataEvent(Sender: TObject);
    procedure AddMagvarEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddDgpsid: Integer;
    procedure DgpsidRemove;
    function AddLatitude: String;
    procedure LatitudeRemove;
    function AddLongitude: String;
    procedure LongitudeRemove;
    function AddElevation: String;
    procedure ElevationRemove;
    function AddTime: TDateTime;
    procedure TimeRemove;
    function AddGeoidheight: Double;
    procedure GeoidheightRemove;
    function AddSymbol: String;
    procedure SymbolRemove;
    function AddSatellites: Integer;
    procedure SatellitesRemove;
    function AddHorizontal: Double;
    procedure HorizontalRemove;
    function AddFix: String;
    procedure FixRemove;
    function AddVertical: Double;
    procedure VerticalRemove;
    function AddPosition: Double;
    procedure PositionRemove;
    function AddAgeofdgpsdata: Double;
    procedure AgeofdgpsdataRemove;
    function AddMagvar: Double;
    procedure MagvarRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Dgpsid: Integer read FDgpsid write SetDgpsid;
    property Latitude: String read FLatitude write SetLatitude;
    property Longitude: String read FLongitude write SetLongitude;
    property Elevation: String read FElevation write SetElevation;
    property Time: TDateTime read FTime write SetTime;
    property Geoidheight: Double read FGeoidheight write SetGeoidheight;
    property Symbol: String read FSymbol write SetSymbol;
    property Satellites: Integer read FSatellites write SetSatellites;
    property Horizontal: Double read FHorizontal write SetHorizontal;
    property Fix: String read FFix write SetFix;
    property Vertical: Double read FVertical write SetVertical;
    property Position: Double read FPosition write SetPosition;
    property Ageofdgpsdata: Double read FAgeofdgpsdata write SetAgeofdgpsdata;
    property Magvar: Double read FMagvar write SetMagvar;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property DgpsidExsit: Boolean read FDgpsidExsit;
    property LatitudeExsit: Boolean read FLatitudeExsit;
    property LongitudeExsit: Boolean read FLongitudeExsit;
    property ElevationExsit: Boolean read FElevationExsit;
    property TimeExsit: Boolean read FTimeExsit;
    property GeoidheightExsit: Boolean read FGeoidheightExsit;
    property SymbolExsit: Boolean read FSymbolExsit;
    property SatellitesExsit: Boolean read FSatellitesExsit;
    property HorizontalExsit: Boolean read FHorizontalExsit;
    property FixExsit: Boolean read FFixExsit;
    property VerticalExsit: Boolean read FVerticalExsit;
    property PositionExsit: Boolean read FPositionExsit;
    property AgeofdgpsdataExsit: Boolean read FAgeofdgpsdataExsit;
    property MagvarExsit: Boolean read FMagvarExsit;
  end;

  TGPXRoute= class(TGPXElement)
  private
    FPointss: TList<TGPXWayPoint>;
    FNumber: Integer;
    FNumberExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetPointss(const _Value: TList<TGPXWayPoint>);
    function GetPoints(Index: Integer): TGPXWayPoint;
    procedure SetPoints(Index: Integer; const _Value: TGPXWayPoint);
    procedure SetNumber(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddPointsEvent(Sender: TObject);
    procedure AddNumberEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddPoints: TGPXWayPoint;
    procedure PointsClear;
    function PointsCount: Integer;
    procedure RemovePoints(_Value: TGPXWayPoint);
    procedure DeletePoints(Index: Integer);
    function AddNumber: Integer;
    procedure NumberRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Pointss: TList<TGPXWayPoint> read FPointss write SetPointss;
    property Points[Index: Integer]: TGPXWayPoint read GetPoints write SetPoints;
    property Number: Integer read FNumber write SetNumber;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NumberExsit: Boolean read FNumberExsit;
  end;

  TGPXTrack= class(TGPXElement)
  private
    FTrackSegmentss: TList<TGPXTrackSegment>;
    FNumber: Integer;
    FNumberExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetTrackSegmentss(const _Value: TList<TGPXTrackSegment>);
    function GetTrackSegments(Index: Integer): TGPXTrackSegment;
    procedure SetTrackSegments(Index: Integer; const _Value: TGPXTrackSegment);
    procedure SetNumber(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddTrackSegmentsEvent(Sender: TObject);
    procedure AddNumberEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddTrackSegments: TGPXTrackSegment;
    procedure TrackSegmentsClear;
    function TrackSegmentsCount: Integer;
    procedure RemoveTrackSegments(_Value: TGPXTrackSegment);
    procedure DeleteTrackSegments(Index: Integer);
    function AddNumber: Integer;
    procedure NumberRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property TrackSegmentss: TList<TGPXTrackSegment> read FTrackSegmentss write SetTrackSegmentss;
    property TrackSegments[Index: Integer]: TGPXTrackSegment read GetTrackSegments write SetTrackSegments;
    property Number: Integer read FNumber write SetNumber;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NumberExsit: Boolean read FNumberExsit;
  end;

  TGPXTrackSegment= class(TXML)
  private
    FPointss: TList<TGPXWayPoint>;
    FExtensionss: TList<TXMLBase>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetPointss(const _Value: TList<TGPXWayPoint>);
    function GetPoints(Index: Integer): TGPXWayPoint;
    procedure SetPoints(Index: Integer; const _Value: TGPXWayPoint);
    procedure SetExtensionss(const _Value: TList<TXMLBase>);
    function GetExtensions(Index: Integer): TXMLBase;
    procedure SetExtensions(Index: Integer; const _Value: TXMLBase);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddPointsEvent(Sender: TObject);
    procedure AddExtensionsEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddPoints: TGPXWayPoint;
    procedure PointsClear;
    function PointsCount: Integer;
    procedure RemovePoints(_Value: TGPXWayPoint);
    procedure DeletePoints(Index: Integer);
    function AddExtensions: TXMLBase;
    procedure ExtensionsClear;
    function ExtensionsCount: Integer;
    procedure RemoveExtensions(_Value: TXMLBase);
    procedure DeleteExtensions(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Pointss: TList<TGPXWayPoint> read FPointss write SetPointss;
    property Points[Index: Integer]: TGPXWayPoint read GetPoints write SetPoints;
    property Extensionss: TList<TXMLBase> read FExtensionss write SetExtensionss;
    property Extensions[Index: Integer]: TXMLBase read GetExtensions write SetExtensions;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGPXMetadata= class(TXML)
  private
    Flink: TGPXLink;
    FlinkExsit: Boolean;
    Fauthor: TGPXPerson;
    FauthorExsit: Boolean;
    Fcopyright: TGPXCopyright;
    FcopyrightExsit: Boolean;
    Fbounds: TGPXBounds;
    FboundsExsit: Boolean;
    Fextensionss: TList<TXMLBase>;
    Fname: String;
    FnameExsit: Boolean;
    Fdescription: String;
    FdescriptionExsit: Boolean;
    Ftime: TDateTime;
    FtimeExsit: Boolean;
    Fkeywords: String;
    FkeywordsExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setlink(const _Value: TGPXLink);
    procedure Setauthor(const _Value: TGPXPerson);
    procedure Setcopyright(const _Value: TGPXCopyright);
    procedure Setbounds(const _Value: TGPXBounds);
    procedure Setextensionss(const _Value: TList<TXMLBase>);
    function Getextensions(Index: Integer): TXMLBase;
    procedure Setextensions(Index: Integer; const _Value: TXMLBase);
    procedure Setname(const _Value: String);
    procedure Setdescription(const _Value: String);
    procedure Settime(const _Value: TDateTime);
    procedure Setkeywords(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddlinkEvent(Sender: TObject);
    procedure AddauthorEvent(Sender: TObject);
    procedure AddcopyrightEvent(Sender: TObject);
    procedure AddboundsEvent(Sender: TObject);
    procedure AddextensionsEvent(Sender: TObject);
    procedure AddnameEvent(Sender: TObject);
    procedure AdddescriptionEvent(Sender: TObject);
    procedure AddtimeEvent(Sender: TObject);
    procedure AddkeywordsEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addlink: TGPXLink;
    procedure linkRemove;
    function Addauthor: TGPXPerson;
    procedure authorRemove;
    function Addcopyright: TGPXCopyright;
    procedure copyrightRemove;
    function Addbounds: TGPXBounds;
    procedure boundsRemove;
    function Addextensions: TXMLBase;
    procedure extensionsClear;
    function extensionsCount: Integer;
    procedure Removeextensions(_Value: TXMLBase);
    procedure Deleteextensions(Index: Integer);
    function Addname: String;
    procedure nameRemove;
    function Adddescription: String;
    procedure descriptionRemove;
    function Addtime: TDateTime;
    procedure timeRemove;
    function Addkeywords: String;
    procedure keywordsRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property link: TGPXLink read Flink write Setlink;
    property author: TGPXPerson read Fauthor write Setauthor;
    property copyright: TGPXCopyright read Fcopyright write Setcopyright;
    property bounds: TGPXBounds read Fbounds write Setbounds;
    property extensionss: TList<TXMLBase> read Fextensionss write Setextensionss;
    property extensions[Index: Integer]: TXMLBase read Getextensions write Setextensions;
    property name: String read Fname write Setname;
    property description: String read Fdescription write Setdescription;
    property time: TDateTime read Ftime write Settime;
    property keywords: String read Fkeywords write Setkeywords;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property linkExsit: Boolean read FlinkExsit;
    property authorExsit: Boolean read FauthorExsit;
    property copyrightExsit: Boolean read FcopyrightExsit;
    property boundsExsit: Boolean read FboundsExsit;
    property nameExsit: Boolean read FnameExsit;
    property descriptionExsit: Boolean read FdescriptionExsit;
    property timeExsit: Boolean read FtimeExsit;
    property keywordsExsit: Boolean read FkeywordsExsit;
  end;

  TGPXCopyright= class(TXML)
  private
    Fyear: Integer;
    FyearExsit: Boolean;
    Fauthor: String;
    FauthorExsit: Boolean;
    Flicense: String;
    FlicenseExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setyear(const _Value: Integer);
    procedure Setauthor(const _Value: String);
    procedure Setlicense(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddyearEvent(Sender: TObject);
    procedure AddauthorEvent(Sender: TObject);
    procedure AddlicenseEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addyear: Integer;
    procedure yearRemove;
    function Addauthor: String;
    procedure authorRemove;
    function Addlicense: String;
    procedure licenseRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property year: Integer read Fyear write Setyear;
    property author: String read Fauthor write Setauthor;
    property license: String read Flicense write Setlicense;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property yearExsit: Boolean read FyearExsit;
    property authorExsit: Boolean read FauthorExsit;
    property licenseExsit: Boolean read FlicenseExsit;
  end;

  TGPXLink= class(TXML)
  private
    FText: String;
    FTextExsit: Boolean;
    FTypeS: String;
    FTypeSExsit: Boolean;
    FHref: String;
    FHrefExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetText(const _Value: String);
    procedure SetTypeS(const _Value: String);
    procedure SetHref(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddTextEvent(Sender: TObject);
    procedure AddTypeSEvent(Sender: TObject);
    procedure AddHrefEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddText: String;
    procedure TextRemove;
    function AddTypeS: String;
    procedure TypeSRemove;
    function AddHref: String;
    procedure HrefRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Text: String read FText write SetText;
    property TypeS: String read FTypeS write SetTypeS;
    property Href: String read FHref write SetHref;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property TextExsit: Boolean read FTextExsit;
    property TypeSExsit: Boolean read FTypeSExsit;
    property HrefExsit: Boolean read FHrefExsit;
  end;

  TGPXEmail= class(TXML)
  private
    FID: String;
    FIDExsit: Boolean;
    FDomain: String;
    FDomainExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetID(const _Value: String);
    procedure SetDomain(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddIDEvent(Sender: TObject);
    procedure AddDomainEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddID: String;
    procedure IDRemove;
    function AddDomain: String;
    procedure DomainRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property ID: String read FID write SetID;
    property Domain: String read FDomain write SetDomain;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property IDExsit: Boolean read FIDExsit;
    property DomainExsit: Boolean read FDomainExsit;
  end;

  TGPXPerson= class(TXML)
  private
    FEmail: TGPXEmail;
    FEmailExsit: Boolean;
    FLink: TGPXLink;
    FLinkExsit: Boolean;
    FName: String;
    FNameExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetEmail(const _Value: TGPXEmail);
    procedure SetLink(const _Value: TGPXLink);
    procedure SetName(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddEmailEvent(Sender: TObject);
    procedure AddLinkEvent(Sender: TObject);
    procedure AddNameEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddEmail: TGPXEmail;
    procedure EmailRemove;
    function AddLink: TGPXLink;
    procedure LinkRemove;
    function AddName: String;
    procedure NameRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Email: TGPXEmail read FEmail write SetEmail;
    property Link: TGPXLink read FLink write SetLink;
    property Name: String read FName write SetName;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property EmailExsit: Boolean read FEmailExsit;
    property LinkExsit: Boolean read FLinkExsit;
    property NameExsit: Boolean read FNameExsit;
  end;

  TGPXPoint= class(TXML)
  private
    FLatitude: String;
    FLatitudeExsit: Boolean;
    FLongitude: String;
    FLongitudeExsit: Boolean;
    FElevation: String;
    FElevationExsit: Boolean;
    FTime: TDateTime;
    FTimeExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLatitude(const _Value: String);
    procedure SetLongitude(const _Value: String);
    procedure SetElevation(const _Value: String);
    procedure SetTime(const _Value: TDateTime);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddLatitudeEvent(Sender: TObject);
    procedure AddLongitudeEvent(Sender: TObject);
    procedure AddElevationEvent(Sender: TObject);
    procedure AddTimeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddLatitude: String;
    procedure LatitudeRemove;
    function AddLongitude: String;
    procedure LongitudeRemove;
    function AddElevation: String;
    procedure ElevationRemove;
    function AddTime: TDateTime;
    procedure TimeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Latitude: String read FLatitude write SetLatitude;
    property Longitude: String read FLongitude write SetLongitude;
    property Elevation: String read FElevation write SetElevation;
    property Time: TDateTime read FTime write SetTime;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property LatitudeExsit: Boolean read FLatitudeExsit;
    property LongitudeExsit: Boolean read FLongitudeExsit;
    property ElevationExsit: Boolean read FElevationExsit;
    property TimeExsit: Boolean read FTimeExsit;
  end;

  TGPXBounds= class(TXML)
  private
    Fmaxlat: Double;
    FmaxlatExsit: Boolean;
    Fmaxlon: Double;
    FmaxlonExsit: Boolean;
    Fminlat: Double;
    FminlatExsit: Boolean;
    Fminlon: Double;
    FminlonExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setmaxlat(const _Value: Double);
    procedure Setmaxlon(const _Value: Double);
    procedure Setminlat(const _Value: Double);
    procedure Setminlon(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddmaxlatEvent(Sender: TObject);
    procedure AddmaxlonEvent(Sender: TObject);
    procedure AddminlatEvent(Sender: TObject);
    procedure AddminlonEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addmaxlat: Double;
    procedure maxlatRemove;
    function Addmaxlon: Double;
    procedure maxlonRemove;
    function Addminlat: Double;
    procedure minlatRemove;
    function Addminlon: Double;
    procedure minlonRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property maxlat: Double read Fmaxlat write Setmaxlat;
    property maxlon: Double read Fmaxlon write Setmaxlon;
    property minlat: Double read Fminlat write Setminlat;
    property minlon: Double read Fminlon write Setminlon;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property maxlatExsit: Boolean read FmaxlatExsit;
    property maxlonExsit: Boolean read FmaxlonExsit;
    property minlatExsit: Boolean read FminlatExsit;
    property minlonExsit: Boolean read FminlonExsit;
  end;


var
  TGPXPop: TPopupMenu;
  TGPXXMLInspector: TXMLInspector;
  TGPXTreeComponent: TTree;
  TGPXElementPop: TPopupMenu;
  TGPXElementXMLInspector: TXMLInspector;
  TGPXElementTreeComponent: TTree;
  TGPXWayPointPop: TPopupMenu;
  TGPXWayPointXMLInspector: TXMLInspector;
  TGPXWayPointTreeComponent: TTree;
  TGPXRoutePop: TPopupMenu;
  TGPXRouteXMLInspector: TXMLInspector;
  TGPXRouteTreeComponent: TTree;
  TGPXTrackPop: TPopupMenu;
  TGPXTrackXMLInspector: TXMLInspector;
  TGPXTrackTreeComponent: TTree;
  TGPXTrackSegmentPop: TPopupMenu;
  TGPXTrackSegmentXMLInspector: TXMLInspector;
  TGPXTrackSegmentTreeComponent: TTree;
  TGPXMetadataPop: TPopupMenu;
  TGPXMetadataXMLInspector: TXMLInspector;
  TGPXMetadataTreeComponent: TTree;
  TGPXCopyrightPop: TPopupMenu;
  TGPXCopyrightXMLInspector: TXMLInspector;
  TGPXCopyrightTreeComponent: TTree;
  TGPXLinkPop: TPopupMenu;
  TGPXLinkXMLInspector: TXMLInspector;
  TGPXLinkTreeComponent: TTree;
  TGPXEmailPop: TPopupMenu;
  TGPXEmailXMLInspector: TXMLInspector;
  TGPXEmailTreeComponent: TTree;
  TGPXPersonPop: TPopupMenu;
  TGPXPersonXMLInspector: TXMLInspector;
  TGPXPersonTreeComponent: TTree;
  TGPXPointPop: TPopupMenu;
  TGPXPointXMLInspector: TXMLInspector;
  TGPXPointTreeComponent: TTree;
  TGPXBoundsPop: TPopupMenu;
  TGPXBoundsXMLInspector: TXMLInspector;
  TGPXBoundsTreeComponent: TTree;
  GPXObject: TObject;

implementation

{  gpx}
constructor TGPX.Create(par: TXML = nil);
begin
  inherited Create(par);
  FElements := TList<TGPXElement>.Create;
  FExtensionss := TList<TXMLBase>.Create;
end;

destructor TGPX.Destroy;
begin
  if FMetadataExsit then
    FMetadata.Free;
  ElementClear;
  FElements.Free;
  ExtensionsClear;
  FExtensionss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPX.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ElementTmp: TGPXElement;
  ExtensionsTmp: TXMLBase;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'metadata' then
      begin
        FMetadata := TGPXMetadata.Create(Self);
        FMetadata.FromXML(nodeTmp);
        FMetadataExsit := True;
      end
      else if nodeTmp.NodeName = 'wpt' then
      begin
        ElementTmp := TGPXWayPoint.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'rte' then
      begin
        ElementTmp := TGPXRoute.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'trk' then
      begin
        ElementTmp := TGPXTrack.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'extensions' then
      begin
        ExtensionsTmp := TXMLBase.Create(Self);
        ExtensionsTmp.FromXML(nodeTmp);
        FExtensionss.Add(ExtensionsTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'xmlns' then
      begin
        Fxmlns := nodeTmp.Text;
        FxmlnsExsit := True;
      end
      else if nodeTmp.NodeName = 'creator' then
      begin
        FCreator := nodeTmp.Text;
        FCreatorExsit := True;
      end
      else if nodeTmp.NodeName = 'version' then
      begin
        FVersion := nodeTmp.Text;
        FVersionExsit := True;
      end
      else if nodeTmp.NodeName = 'xmlns:xsi' then
      begin
        Fxmlnsxsi := nodeTmp.Text;
        FxmlnsxsiExsit := True;
      end
      else if nodeTmp.NodeName = 'xsi:schemaLocation' then
      begin
        FxsischemaLocation := nodeTmp.Text;
        FxsischemaLocationExsit := True;
      end;
    end;
  except
    raise Exception.Create('gpx Read XML Error!' + node.Xml);
  end;
end;

function TGPX.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  xmlnsTmp: IXMLNode;
  CreatorTmp: IXMLNode;
  VersionTmp: IXMLNode;
  xmlnsxsiTmp: IXMLNode;
  xsischemaLocationTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'gpx';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FMetadataExsit then
      FMetadata.ToXML(node, 'metadata');
    for I := 0 to FElements.Count - 1 do
       FElements.Items[I].ToXML(node, '#Optional');
    for I := 0 to FExtensionss.Count - 1 do
       FExtensionss.Items[I].ToXML(node, 'extensions');
    if FxmlnsExsit then 
    begin
      xmlnsTmp := doc.CreateNode('xmlns', ntAttribute);
      xmlnsTmp.NodeValue := Fxmlns;
      node.AttributeNodes.Add(xmlnsTmp);
    end;
    if FCreatorExsit then 
    begin
      CreatorTmp := doc.CreateNode('creator', ntAttribute);
      CreatorTmp.NodeValue := FCreator;
      node.AttributeNodes.Add(CreatorTmp);
    end;
    if FVersionExsit then 
    begin
      VersionTmp := doc.CreateNode('version', ntAttribute);
      VersionTmp.NodeValue := FVersion;
      node.AttributeNodes.Add(VersionTmp);
    end;
    if FxmlnsxsiExsit then 
    begin
      xmlnsxsiTmp := doc.CreateNode('xmlns:xsi', ntAttribute);
      xmlnsxsiTmp.NodeValue := Fxmlnsxsi;
      node.AttributeNodes.Add(xmlnsxsiTmp);
    end;
    if FxsischemaLocationExsit then 
    begin
      xsischemaLocationTmp := doc.CreateNode('xsi:schemaLocation', ntAttribute);
      xsischemaLocationTmp.NodeValue := FxsischemaLocation;
      node.AttributeNodes.Add(xsischemaLocationTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPX.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if xmlnsExsit then
    TreeNodeShape.AddChild('xmlns');
  if CreatorExsit then
    TreeNodeShape.AddChild('Creator');
  if MetadataExsit then
  begin
    Metadata.TreeNodeShape := TreeNodeShape.AddChildObject('Metadata', Metadata);
    Metadata.ToTree;
  end;
  for I := 0 to ElementCount - 1 do
  begin
    Elements[I].TreeNodeShape := TreeNodeShape.AddChildObject('Element', Element[I]);
    Element[I].ToTree;
  end;
  for I := 0 to ExtensionsCount - 1 do
  begin
    Extensionss[I].TreeNodeShape := TreeNodeShape.AddChildObject('Extensions', Extensions[I]);
    Extensions[I].ToTree;
  end;
  if VersionExsit then
    TreeNodeShape.AddChild('Version');
  if xmlnsxsiExsit then
    TreeNodeShape.AddChild('xmlnsxsi');
  if xsischemaLocationExsit then
    TreeNodeShape.AddChild('xsischemaLocation');
end;

procedure TGPX.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  xmlnsAddMenu: TMenuItem;
  CreatorAddMenu: TMenuItem;
  MetadataAddMenu: TMenuItem;
  ElementAddMenu: TMenuItem;
  WayPointAddMenu: TMenuItem;
  RouteAddMenu: TMenuItem;
  TrackAddMenu: TMenuItem;
  ExtensionsAddMenu: TMenuItem;
  VersionAddMenu: TMenuItem;
  xmlnsxsiAddMenu: TMenuItem;
  xsischemaLocationAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXPop) and Assigned(TGPXTreeComponent) then
    begin
      TGPXPop.Clear;
      xmlnsAddMenu := TMenuItem.Create(TGPXPop);
      xmlnsAddMenu.Text := 'Add xmlns';
      xmlnsAddMenu.OnClick := AddxmlnsEvent;
      TGPXPop.AddObject(xmlnsAddMenu);
      CreatorAddMenu := TMenuItem.Create(TGPXPop);
      CreatorAddMenu.Text := 'Add Creator';
      CreatorAddMenu.OnClick := AddCreatorEvent;
      TGPXPop.AddObject(CreatorAddMenu);
      MetadataAddMenu := TMenuItem.Create(TGPXPop);
      MetadataAddMenu.Text := 'Add Metadata';
      MetadataAddMenu.OnClick := AddMetadataEvent;
      TGPXPop.AddObject(MetadataAddMenu);
      ElementAddMenu := TMenuItem.Create(TGPXPop);
      ElementAddMenu.Text := 'Add Element';
      TGPXPop.AddObject(ElementAddMenu);
      WayPointAddMenu := TMenuItem.Create(ElementAddMenu);
      WayPointAddMenu.Text := 'WayPoint';
      WayPointAddMenu.OnClick := AddWayPointEvent;
      ElementAddMenu.AddObject(WayPointAddMenu);
      RouteAddMenu := TMenuItem.Create(ElementAddMenu);
      RouteAddMenu.Text := 'Route';
      RouteAddMenu.OnClick := AddRouteEvent;
      ElementAddMenu.AddObject(RouteAddMenu);
      TrackAddMenu := TMenuItem.Create(ElementAddMenu);
      TrackAddMenu.Text := 'Track';
      TrackAddMenu.OnClick := AddTrackEvent;
      ElementAddMenu.AddObject(TrackAddMenu);
      ExtensionsAddMenu := TMenuItem.Create(TGPXPop);
      ExtensionsAddMenu.Text := 'Add Extensions';
      ExtensionsAddMenu.OnClick := AddExtensionsEvent;
      TGPXPop.AddObject(ExtensionsAddMenu);
      VersionAddMenu := TMenuItem.Create(TGPXPop);
      VersionAddMenu.Text := 'Add Version';
      VersionAddMenu.OnClick := AddVersionEvent;
      TGPXPop.AddObject(VersionAddMenu);
      xmlnsxsiAddMenu := TMenuItem.Create(TGPXPop);
      xmlnsxsiAddMenu.Text := 'Add xmlnsxsi';
      xmlnsxsiAddMenu.OnClick := AddxmlnsxsiEvent;
      TGPXPop.AddObject(xmlnsxsiAddMenu);
      xsischemaLocationAddMenu := TMenuItem.Create(TGPXPop);
      xsischemaLocationAddMenu.Text := 'Add xsischemaLocation';
      xsischemaLocationAddMenu.OnClick := AddxsischemaLocationEvent;
      TGPXPop.AddObject(xsischemaLocationAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXTreeComponent.ClientToScreen(pt);
      TGPXPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPX.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('xmlns');
  Types_Value.Add(xs_string);
  _Values_Value.Add(xmlns);
  Names_Value.Add('Creator');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Creator);
  Names_Value.Add('Version');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Version);
  Names_Value.Add('xmlnsxsi');
  Types_Value.Add(xs_string);
  _Values_Value.Add(xmlnsxsi);
  Names_Value.Add('xsischemaLocation');
  Types_Value.Add(xs_string);
  _Values_Value.Add(xsischemaLocation);
  TGPXXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPX.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        xmlns := _Value;
      end;
    1:
      begin
        Creator := _Value;
      end;
    2:
      begin
        Version := _Value;
      end;
    3:
      begin
        xmlnsxsi := _Value;
      end;
    4:
      begin
        xsischemaLocation := _Value;
      end;
  end;
  ToTree;
end;

function TGPX.Addxmlns: String;
begin;
  Result := Fxmlns;
  FxmlnsExsit := True;
end;

procedure TGPX.Setxmlns(const _Value: String);
begin
  FxmlnsExsit := True;
  Fxmlns := _Value;
end;

procedure TGPX.xmlnsRemove;
begin
  if FxmlnsExsit then
  begin
    FxmlnsExsit := False;
  end;
end;

procedure TGPX.AddxmlnsEvent(Sender: TObject);
begin
  Addxmlns;
end;

function TGPX.AddCreator: String;
begin;
  Result := FCreator;
  FCreatorExsit := True;
end;

procedure TGPX.SetCreator(const _Value: String);
begin
  FCreatorExsit := True;
  FCreator := _Value;
end;

procedure TGPX.CreatorRemove;
begin
  if FCreatorExsit then
  begin
    FCreatorExsit := False;
  end;
end;

procedure TGPX.AddCreatorEvent(Sender: TObject);
begin
  AddCreator;
end;

function TGPX.AddMetadata: TGPXMetadata;
begin;
  if not FMetadataExsit then
    FMetadata := TGPXMetadata.Create(Self);
  Result := FMetadata;
  FMetadataExsit := True;
end;

procedure TGPX.SetMetadata(const _Value: TGPXMetadata);
begin
  if FMetadataExsit then
    FMetadata.Free;
  FMetadataExsit := True;
  FMetadata := _Value;
  FMetadata.Parent := Self;
end;

procedure TGPX.MetadataRemove;
begin
  if FMetadataExsit then
  begin
    FMetadata.Free;
    FMetadataExsit := False;
  end;
end;

procedure TGPX.AddMetadataEvent(Sender: TObject);
begin
  AddMetadata;
  FMetadata.ToTree;
end;

function TGPX.AddWayPoint: TGPXWayPoint;
var
  WayPointtmp: TGPXWayPoint;
begin;
  WayPointtmp := TGPXWayPoint.Create(Self);
  FElements.Add(WayPointtmp);
  Result := WayPointtmp;
end;

function TGPX.AddRoute: TGPXRoute;
var
  Routetmp: TGPXRoute;
begin;
  Routetmp := TGPXRoute.Create(Self);
  FElements.Add(Routetmp);
  Result := Routetmp;
end;

function TGPX.AddTrack: TGPXTrack;
var
  Tracktmp: TGPXTrack;
begin;
  Tracktmp := TGPXTrack.Create(Self);
  FElements.Add(Tracktmp);
  Result := Tracktmp;
end;

procedure TGPX.SetElements(const _Value: TList<TGPXElement>);
begin
  ElementClear;
  FElements := _Value;
end;

procedure TGPX.ElementClear;
begin
  while FElements.Count > 0 do
  begin
    FElements.Items[0].Free;
    FElements.Delete(0);
  end;
end;

function TGPX.ElementCount: Integer;
begin
  Result := FElements.Count;
end;

function TGPX.GetElement(Index: Integer): TGPXElement;
begin
  Result := FElements[Index];
end;

procedure TGPX.SetElement(Index: Integer;
  const _Value: TGPXElement);
begin
  _Value.Parent := Self;
  FElements[Index].Free;
  FElements[Index] := _Value;
end;

procedure TGPX.RemoveElement(_Value: TGPXElement);
begin
  FElements.Remove(_Value);
  _Value.Free;
end;

procedure TGPX.DeleteElement(Index: Integer);
begin
  FElements.Items[Index].Free;
  FElements.Delete(Index);
end;

procedure TGPX.AddWayPointEvent(Sender: TObject);
var
  tmp: TGPXWayPoint;
begin
  tmp := AddWayPoint;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('WayPoint', tmp);
  tmp.ToTree;
end;

procedure TGPX.AddRouteEvent(Sender: TObject);
var
  tmp: TGPXRoute;
begin
  tmp := AddRoute;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Route', tmp);
  tmp.ToTree;
end;

procedure TGPX.AddTrackEvent(Sender: TObject);
var
  tmp: TGPXTrack;
begin
  tmp := AddTrack;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Track', tmp);
  tmp.ToTree;
end;

function TGPX.AddExtensions: TXMLBase;
var
  Extensionstmp: TXMLBase;
begin;
  Extensionstmp := TXMLBase.Create(Self);
  FExtensionss.Add(Extensionstmp);
  Result := Extensionstmp;
end;

procedure TGPX.SetExtensionss(const _Value: TList<TXMLBase>);
begin
  ExtensionsClear;
  FExtensionss := _Value;
end;

procedure TGPX.ExtensionsClear;
begin
  while FExtensionss.Count > 0 do
  begin
    FExtensionss.Items[0].Free;
    FExtensionss.Delete(0);
  end;
end;

function TGPX.ExtensionsCount: Integer;
begin
  Result := FExtensionss.Count;
end;

function TGPX.GetExtensions(Index: Integer): TXMLBase;
begin
  Result := FExtensionss[Index];
end;

procedure TGPX.SetExtensions(Index: Integer;
  const _Value: TXMLBase);
begin
  _Value.Parent := Self;
  FExtensionss[Index].Free;
  FExtensionss[Index] := _Value;
end;

procedure TGPX.RemoveExtensions(_Value: TXMLBase);
begin
  FExtensionss.Remove(_Value);
  _Value.Free;
end;

procedure TGPX.DeleteExtensions(Index: Integer);
begin
  FExtensionss.Items[Index].Free;
  FExtensionss.Delete(Index);
end;

procedure TGPX.AddExtensionsEvent(Sender: TObject);
var
  tmp: TXMLBase;
begin
  tmp := AddExtensions;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Extensions', tmp);
  tmp.ToTree;
end;

function TGPX.AddVersion: String;
begin;
  Result := FVersion;
  FVersionExsit := True;
end;

procedure TGPX.SetVersion(const _Value: String);
begin
  FVersionExsit := True;
  FVersion := _Value;
end;

procedure TGPX.VersionRemove;
begin
  if FVersionExsit then
  begin
    FVersionExsit := False;
  end;
end;

procedure TGPX.AddVersionEvent(Sender: TObject);
begin
  AddVersion;
end;

function TGPX.Addxmlnsxsi: String;
begin;
  Result := Fxmlnsxsi;
  FxmlnsxsiExsit := True;
end;

procedure TGPX.Setxmlnsxsi(const _Value: String);
begin
  FxmlnsxsiExsit := True;
  Fxmlnsxsi := _Value;
end;

procedure TGPX.xmlnsxsiRemove;
begin
  if FxmlnsxsiExsit then
  begin
    FxmlnsxsiExsit := False;
  end;
end;

procedure TGPX.AddxmlnsxsiEvent(Sender: TObject);
begin
  Addxmlnsxsi;
end;

function TGPX.AddxsischemaLocation: String;
begin;
  Result := FxsischemaLocation;
  FxsischemaLocationExsit := True;
end;

procedure TGPX.SetxsischemaLocation(const _Value: String);
begin
  FxsischemaLocationExsit := True;
  FxsischemaLocation := _Value;
end;

procedure TGPX.xsischemaLocationRemove;
begin
  if FxsischemaLocationExsit then
  begin
    FxsischemaLocationExsit := False;
  end;
end;

procedure TGPX.AddxsischemaLocationEvent(Sender: TObject);
begin
  AddxsischemaLocation;
end;

{  Element}
constructor TGPXElement.Create(par: TXML = nil);
begin
  inherited Create(par);
  FExtensionss := TList<TXMLBase>.Create;
  FLinks := TList<TGPXLink>.Create;
end;

destructor TGPXElement.Destroy;
begin
  ExtensionsClear;
  FExtensionss.Free;
  LinkClear;
  FLinks.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXElement.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ExtensionsTmp: TXMLBase;
  LinkTmp: TGPXLink;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'cmt' then
      begin
        FComment := nodeTmp.Text;
        FCommentExsit := True;
      end
      else if nodeTmp.NodeName = 'desc' then
      begin
        FDescription := nodeTmp.Text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'src' then
      begin
        FSource := nodeTmp.Text;
        FSourceExsit := True;
      end
      else if nodeTmp.NodeName = 'extensions' then
      begin
        ExtensionsTmp := TXMLBase.Create(Self);
        ExtensionsTmp.FromXML(nodeTmp);
        FExtensionss.Add(ExtensionsTmp);
      end
      else if nodeTmp.NodeName = 'link' then
      begin
        LinkTmp := TGPXLink.Create(Self);
        LinkTmp.FromXML(nodeTmp);
        FLinks.Add(LinkTmp);
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        FTypeS := nodeTmp.Text;
        FTypeSExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Element Read XML Error!' + node.Xml);
  end;
end;

function TGPXElement.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  CommentTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  SourceTmp: IXMLNode;
  TypeSTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := '#Optional';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FCommentExsit then
    begin
      CommentTmp := doc.CreateNode('cmt', ntElement);
      CommentTmp.NodeValue := FComment;
      node.ChildNodes.Add(CommentTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('desc', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FSourceExsit then
    begin
      SourceTmp := doc.CreateNode('src', ntElement);
      SourceTmp.NodeValue := FSource;
      node.ChildNodes.Add(SourceTmp);
    end;
    for I := 0 to FExtensionss.Count - 1 do
       FExtensionss.Items[I].ToXML(node, 'extensions');
    for I := 0 to FLinks.Count - 1 do
       FLinks.Items[I].ToXML(node, 'link');
    if FTypeSExsit then
    begin
      TypeSTmp := doc.CreateNode('type', ntElement);
      TypeSTmp.NodeValue := FTypeS;
      node.ChildNodes.Add(TypeSTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

function TGPXElement.AppendToXML(node: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  NameTmp: IXMLNode;
  CommentTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  SourceTmp: IXMLNode;
  TypeSTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := node.OwnerDocument;
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FCommentExsit then
    begin
      CommentTmp := doc.CreateNode('cmt', ntElement);
      CommentTmp.NodeValue := FComment;
      node.ChildNodes.Add(CommentTmp);
    end;
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('desc', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FSourceExsit then
    begin
      SourceTmp := doc.CreateNode('src', ntElement);
      SourceTmp.NodeValue := FSource;
      node.ChildNodes.Add(SourceTmp);
    end;
    for I := 0 to FExtensionss.Count - 1 do
       FExtensionss.Items[I].ToXML(node, 'extensions');
    for I := 0 to FLinks.Count - 1 do
       FLinks.Items[I].ToXML(node, 'link');
    if FTypeSExsit then
    begin
      TypeSTmp := doc.CreateNode('type', ntElement);
      TypeSTmp.NodeValue := FTypeS;
      node.ChildNodes.Add(TypeSTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXElement.ToTree;
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
  if CommentExsit then
    TreeNodeShape.AddChild('Comment');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if SourceExsit then
    TreeNodeShape.AddChild('Source');
  for I := 0 to ExtensionsCount - 1 do
  begin
    Extensionss[I].TreeNodeShape := TreeNodeShape.AddChildObject('Extensions', Extensions[I]);
    Extensions[I].ToTree;
  end;
  for I := 0 to LinkCount - 1 do
  begin
    Links[I].TreeNodeShape := TreeNodeShape.AddChildObject('Link', Link[I]);
    Link[I].ToTree;
  end;
  if TypeSExsit then
    TreeNodeShape.AddChild('TypeS');
end;

procedure TGPXElement.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  CommentAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  SourceAddMenu: TMenuItem;
  ExtensionsAddMenu: TMenuItem;
  LinkAddMenu: TMenuItem;
  TypeSAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXElementPop) and Assigned(TGPXElementTreeComponent) then
    begin
      TGPXElementPop.Clear;
      NameAddMenu := TMenuItem.Create(TGPXElementPop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TGPXElementPop.AddObject(NameAddMenu);
      CommentAddMenu := TMenuItem.Create(TGPXElementPop);
      CommentAddMenu.Text := 'Add Comment';
      CommentAddMenu.OnClick := AddCommentEvent;
      TGPXElementPop.AddObject(CommentAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TGPXElementPop);
      DescriptionAddMenu.Text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TGPXElementPop.AddObject(DescriptionAddMenu);
      SourceAddMenu := TMenuItem.Create(TGPXElementPop);
      SourceAddMenu.Text := 'Add Source';
      SourceAddMenu.OnClick := AddSourceEvent;
      TGPXElementPop.AddObject(SourceAddMenu);
      ExtensionsAddMenu := TMenuItem.Create(TGPXElementPop);
      ExtensionsAddMenu.Text := 'Add Extensions';
      ExtensionsAddMenu.OnClick := AddExtensionsEvent;
      TGPXElementPop.AddObject(ExtensionsAddMenu);
      LinkAddMenu := TMenuItem.Create(TGPXElementPop);
      LinkAddMenu.Text := 'Add Link';
      LinkAddMenu.OnClick := AddLinkEvent;
      TGPXElementPop.AddObject(LinkAddMenu);
      TypeSAddMenu := TMenuItem.Create(TGPXElementPop);
      TypeSAddMenu.Text := 'Add TypeS';
      TypeSAddMenu.OnClick := AddTypeSEvent;
      TGPXElementPop.AddObject(TypeSAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXElementTreeComponent.ClientToScreen(pt);
      TGPXElementPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXElement.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXElementXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Comment');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Comment);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('Source');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Source);
  Names_Value.Add('TypeS');
  Types_Value.Add(xs_string);
  _Values_Value.Add(TypeS);
  TGPXElementXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXElement.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Comment := _Value;
      end;
    2:
      begin
        Description := _Value;
      end;
    3:
      begin
        Source := _Value;
      end;
    4:
      begin
        TypeS := _Value;
      end;
  end;
  ToTree;
end;

function TGPXElement.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TGPXElement.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TGPXElement.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TGPXElement.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TGPXElement.AddComment: String;
begin;
  Result := FComment;
  FCommentExsit := True;
end;

procedure TGPXElement.SetComment(const _Value: String);
begin
  FCommentExsit := True;
  FComment := _Value;
end;

procedure TGPXElement.CommentRemove;
begin
  if FCommentExsit then
  begin
    FCommentExsit := False;
  end;
end;

procedure TGPXElement.AddCommentEvent(Sender: TObject);
begin
  AddComment;
end;

function TGPXElement.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TGPXElement.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TGPXElement.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TGPXElement.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TGPXElement.AddSource: String;
begin;
  Result := FSource;
  FSourceExsit := True;
end;

procedure TGPXElement.SetSource(const _Value: String);
begin
  FSourceExsit := True;
  FSource := _Value;
end;

procedure TGPXElement.SourceRemove;
begin
  if FSourceExsit then
  begin
    FSourceExsit := False;
  end;
end;

procedure TGPXElement.AddSourceEvent(Sender: TObject);
begin
  AddSource;
end;

function TGPXElement.AddExtensions: TXMLBase;
var
  Extensionstmp: TXMLBase;
begin;
  Extensionstmp := TXMLBase.Create(Self);
  FExtensionss.Add(Extensionstmp);
  Result := Extensionstmp;
end;

procedure TGPXElement.SetExtensionss(const _Value: TList<TXMLBase>);
begin
  ExtensionsClear;
  FExtensionss := _Value;
end;

procedure TGPXElement.ExtensionsClear;
begin
  while FExtensionss.Count > 0 do
  begin
    FExtensionss.Items[0].Free;
    FExtensionss.Delete(0);
  end;
end;

function TGPXElement.ExtensionsCount: Integer;
begin
  Result := FExtensionss.Count;
end;

function TGPXElement.GetExtensions(Index: Integer): TXMLBase;
begin
  Result := FExtensionss[Index];
end;

procedure TGPXElement.SetExtensions(Index: Integer;
  const _Value: TXMLBase);
begin
  _Value.Parent := Self;
  FExtensionss[Index].Free;
  FExtensionss[Index] := _Value;
end;

procedure TGPXElement.RemoveExtensions(_Value: TXMLBase);
begin
  FExtensionss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXElement.DeleteExtensions(Index: Integer);
begin
  FExtensionss.Items[Index].Free;
  FExtensionss.Delete(Index);
end;

procedure TGPXElement.AddExtensionsEvent(Sender: TObject);
var
  tmp: TXMLBase;
begin
  tmp := AddExtensions;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Extensions', tmp);
  tmp.ToTree;
end;

function TGPXElement.AddLink: TGPXLink;
var
  Linktmp: TGPXLink;
begin;
  Linktmp := TGPXLink.Create(Self);
  FLinks.Add(Linktmp);
  Result := Linktmp;
end;

procedure TGPXElement.SetLinks(const _Value: TList<TGPXLink>);
begin
  LinkClear;
  FLinks := _Value;
end;

procedure TGPXElement.LinkClear;
begin
  while FLinks.Count > 0 do
  begin
    FLinks.Items[0].Free;
    FLinks.Delete(0);
  end;
end;

function TGPXElement.LinkCount: Integer;
begin
  Result := FLinks.Count;
end;

function TGPXElement.GetLink(Index: Integer): TGPXLink;
begin
  Result := FLinks[Index];
end;

procedure TGPXElement.SetLink(Index: Integer;
  const _Value: TGPXLink);
begin
  _Value.Parent := Self;
  FLinks[Index].Free;
  FLinks[Index] := _Value;
end;

procedure TGPXElement.RemoveLink(_Value: TGPXLink);
begin
  FLinks.Remove(_Value);
  _Value.Free;
end;

procedure TGPXElement.DeleteLink(Index: Integer);
begin
  FLinks.Items[Index].Free;
  FLinks.Delete(Index);
end;

procedure TGPXElement.AddLinkEvent(Sender: TObject);
var
  tmp: TGPXLink;
begin
  tmp := AddLink;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Link', tmp);
  tmp.ToTree;
end;

function TGPXElement.AddTypeS: String;
begin;
  Result := FTypeS;
  FTypeSExsit := True;
end;

procedure TGPXElement.SetTypeS(const _Value: String);
begin
  FTypeSExsit := True;
  FTypeS := _Value;
end;

procedure TGPXElement.TypeSRemove;
begin
  if FTypeSExsit then
  begin
    FTypeSExsit := False;
  end;
end;

procedure TGPXElement.AddTypeSEvent(Sender: TObject);
begin
  AddTypeS;
end;

{  WayPoint}
constructor TGPXWayPoint.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXWayPoint.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXWayPoint.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'dgpsid' then
      begin
        FDgpsid := nodeTmp.Text.ToInteger;
        FDgpsidExsit := True;
      end
      else if nodeTmp.NodeName = 'ele' then
      begin
        FElevation := nodeTmp.Text;
        FElevationExsit := True;
      end
      else if nodeTmp.NodeName = 'time' then
      begin
        FTime := StrToDateTimeDef(nodeTmp.Text, Now());
        FTimeExsit := True;
      end
      else if nodeTmp.NodeName = 'geoidheight' then
      begin
        FGeoidheight := nodeTmp.Text.ToDouble;
        FGeoidheightExsit := True;
      end
      else if nodeTmp.NodeName = 'sym' then
      begin
        FSymbol := nodeTmp.Text;
        FSymbolExsit := True;
      end
      else if nodeTmp.NodeName = 'sat' then
      begin
        FSatellites := nodeTmp.Text.ToInteger;
        FSatellitesExsit := True;
      end
      else if nodeTmp.NodeName = 'hdop' then
      begin
        FHorizontal := nodeTmp.Text.ToDouble;
        FHorizontalExsit := True;
      end
      else if nodeTmp.NodeName = 'fix' then
      begin
        FFix := nodeTmp.Text;
        FFixExsit := True;
      end
      else if nodeTmp.NodeName = 'vdop' then
      begin
        FVertical := nodeTmp.Text.ToDouble;
        FVerticalExsit := True;
      end
      else if nodeTmp.NodeName = 'pdop' then
      begin
        FPosition := nodeTmp.Text.ToDouble;
        FPositionExsit := True;
      end
      else if nodeTmp.NodeName = 'ageofdgpsdata' then
      begin
        FAgeofdgpsdata := nodeTmp.Text.ToDouble;
        FAgeofdgpsdataExsit := True;
      end
      else if nodeTmp.NodeName = 'magvar' then
      begin
        FMagvar := nodeTmp.Text.ToDouble;
        FMagvarExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'lat' then
      begin
        FLatitude := nodeTmp.Text;
        FLatitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'lon' then
      begin
        FLongitude := nodeTmp.Text;
        FLongitudeExsit := True;
      end;
    end;
  except
    raise Exception.Create('WayPoint Read XML Error!' + node.Xml);
  end;
end;

function TGPXWayPoint.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  DgpsidTmp: IXMLNode;
  LatitudeTmp: IXMLNode;
  LongitudeTmp: IXMLNode;
  ElevationTmp: IXMLNode;
  TimeTmp: IXMLNode;
  GeoidheightTmp: IXMLNode;
  SymbolTmp: IXMLNode;
  SatellitesTmp: IXMLNode;
  HorizontalTmp: IXMLNode;
  FixTmp: IXMLNode;
  VerticalTmp: IXMLNode;
  PositionTmp: IXMLNode;
  AgeofdgpsdataTmp: IXMLNode;
  MagvarTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'wpt';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    if FDgpsidExsit then
    begin
      DgpsidTmp := doc.CreateNode('dgpsid', ntElement);
      DgpsidTmp.NodeValue := FDgpsid.toString;
      node.ChildNodes.Add(DgpsidTmp);
    end;
    if FElevationExsit then
    begin
      ElevationTmp := doc.CreateNode('ele', ntElement);
      ElevationTmp.NodeValue := FElevation;
      node.ChildNodes.Add(ElevationTmp);
    end;
    if FTimeExsit then
    begin
      TimeTmp := doc.CreateNode('time', ntElement);
      TimeTmp.NodeValue := FormatDateTime(XMLDTFormat, FTime);
      node.ChildNodes.Add(TimeTmp);
    end;
    if FGeoidheightExsit then
    begin
      GeoidheightTmp := doc.CreateNode('geoidheight', ntElement);
      GeoidheightTmp.NodeValue := FGeoidheight.ToString;
      node.ChildNodes.Add(GeoidheightTmp);
    end;
    if FSymbolExsit then
    begin
      SymbolTmp := doc.CreateNode('sym', ntElement);
      SymbolTmp.NodeValue := FSymbol;
      node.ChildNodes.Add(SymbolTmp);
    end;
    if FSatellitesExsit then
    begin
      SatellitesTmp := doc.CreateNode('sat', ntElement);
      SatellitesTmp.NodeValue := FSatellites.toString;
      node.ChildNodes.Add(SatellitesTmp);
    end;
    if FHorizontalExsit then
    begin
      HorizontalTmp := doc.CreateNode('hdop', ntElement);
      HorizontalTmp.NodeValue := FHorizontal.ToString;
      node.ChildNodes.Add(HorizontalTmp);
    end;
    if FFixExsit then
    begin
      FixTmp := doc.CreateNode('fix', ntElement);
      FixTmp.NodeValue := FFix;
      node.ChildNodes.Add(FixTmp);
    end;
    if FVerticalExsit then
    begin
      VerticalTmp := doc.CreateNode('vdop', ntElement);
      VerticalTmp.NodeValue := FVertical.ToString;
      node.ChildNodes.Add(VerticalTmp);
    end;
    if FPositionExsit then
    begin
      PositionTmp := doc.CreateNode('pdop', ntElement);
      PositionTmp.NodeValue := FPosition.ToString;
      node.ChildNodes.Add(PositionTmp);
    end;
    if FAgeofdgpsdataExsit then
    begin
      AgeofdgpsdataTmp := doc.CreateNode('ageofdgpsdata', ntElement);
      AgeofdgpsdataTmp.NodeValue := FAgeofdgpsdata.ToString;
      node.ChildNodes.Add(AgeofdgpsdataTmp);
    end;
    if FMagvarExsit then
    begin
      MagvarTmp := doc.CreateNode('magvar', ntElement);
      MagvarTmp.NodeValue := FMagvar.ToString;
      node.ChildNodes.Add(MagvarTmp);
    end;
    if FLatitudeExsit then 
    begin
      LatitudeTmp := doc.CreateNode('lat', ntAttribute);
      LatitudeTmp.NodeValue := FLatitude;
      node.AttributeNodes.Add(LatitudeTmp);
    end;
    if FLongitudeExsit then 
    begin
      LongitudeTmp := doc.CreateNode('lon', ntAttribute);
      LongitudeTmp.NodeValue := FLongitude;
      node.AttributeNodes.Add(LongitudeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXWayPoint.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if DgpsidExsit then
    TreeNodeShape.AddChild('Dgpsid');
  if LatitudeExsit then
    TreeNodeShape.AddChild('Latitude');
  if LongitudeExsit then
    TreeNodeShape.AddChild('Longitude');
  if ElevationExsit then
    TreeNodeShape.AddChild('Elevation');
  if TimeExsit then
    TreeNodeShape.AddChild('Time');
  if GeoidheightExsit then
    TreeNodeShape.AddChild('Geoidheight');
  if SymbolExsit then
    TreeNodeShape.AddChild('Symbol');
  if SatellitesExsit then
    TreeNodeShape.AddChild('Satellites');
  if HorizontalExsit then
    TreeNodeShape.AddChild('Horizontal');
  if FixExsit then
    TreeNodeShape.AddChild('Fix');
  if VerticalExsit then
    TreeNodeShape.AddChild('Vertical');
  if PositionExsit then
    TreeNodeShape.AddChild('Position');
  if AgeofdgpsdataExsit then
    TreeNodeShape.AddChild('Ageofdgpsdata');
  if MagvarExsit then
    TreeNodeShape.AddChild('Magvar');
end;

procedure TGPXWayPoint.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  DgpsidAddMenu: TMenuItem;
  LatitudeAddMenu: TMenuItem;
  LongitudeAddMenu: TMenuItem;
  ElevationAddMenu: TMenuItem;
  TimeAddMenu: TMenuItem;
  GeoidheightAddMenu: TMenuItem;
  SymbolAddMenu: TMenuItem;
  SatellitesAddMenu: TMenuItem;
  HorizontalAddMenu: TMenuItem;
  FixAddMenu: TMenuItem;
  VerticalAddMenu: TMenuItem;
  PositionAddMenu: TMenuItem;
  AgeofdgpsdataAddMenu: TMenuItem;
  MagvarAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXWayPointPop) and Assigned(TGPXWayPointTreeComponent) then
    begin
      TGPXWayPointPop.Clear;
      DgpsidAddMenu := TMenuItem.Create(TGPXWayPointPop);
      DgpsidAddMenu.Text := 'Add Dgpsid';
      DgpsidAddMenu.OnClick := AddDgpsidEvent;
      TGPXWayPointPop.AddObject(DgpsidAddMenu);
      LatitudeAddMenu := TMenuItem.Create(TGPXWayPointPop);
      LatitudeAddMenu.Text := 'Add Latitude';
      LatitudeAddMenu.OnClick := AddLatitudeEvent;
      TGPXWayPointPop.AddObject(LatitudeAddMenu);
      LongitudeAddMenu := TMenuItem.Create(TGPXWayPointPop);
      LongitudeAddMenu.Text := 'Add Longitude';
      LongitudeAddMenu.OnClick := AddLongitudeEvent;
      TGPXWayPointPop.AddObject(LongitudeAddMenu);
      ElevationAddMenu := TMenuItem.Create(TGPXWayPointPop);
      ElevationAddMenu.Text := 'Add Elevation';
      ElevationAddMenu.OnClick := AddElevationEvent;
      TGPXWayPointPop.AddObject(ElevationAddMenu);
      TimeAddMenu := TMenuItem.Create(TGPXWayPointPop);
      TimeAddMenu.Text := 'Add Time';
      TimeAddMenu.OnClick := AddTimeEvent;
      TGPXWayPointPop.AddObject(TimeAddMenu);
      GeoidheightAddMenu := TMenuItem.Create(TGPXWayPointPop);
      GeoidheightAddMenu.Text := 'Add Geoidheight';
      GeoidheightAddMenu.OnClick := AddGeoidheightEvent;
      TGPXWayPointPop.AddObject(GeoidheightAddMenu);
      SymbolAddMenu := TMenuItem.Create(TGPXWayPointPop);
      SymbolAddMenu.Text := 'Add Symbol';
      SymbolAddMenu.OnClick := AddSymbolEvent;
      TGPXWayPointPop.AddObject(SymbolAddMenu);
      SatellitesAddMenu := TMenuItem.Create(TGPXWayPointPop);
      SatellitesAddMenu.Text := 'Add Satellites';
      SatellitesAddMenu.OnClick := AddSatellitesEvent;
      TGPXWayPointPop.AddObject(SatellitesAddMenu);
      HorizontalAddMenu := TMenuItem.Create(TGPXWayPointPop);
      HorizontalAddMenu.Text := 'Add Horizontal';
      HorizontalAddMenu.OnClick := AddHorizontalEvent;
      TGPXWayPointPop.AddObject(HorizontalAddMenu);
      FixAddMenu := TMenuItem.Create(TGPXWayPointPop);
      FixAddMenu.Text := 'Add Fix';
      FixAddMenu.OnClick := AddFixEvent;
      TGPXWayPointPop.AddObject(FixAddMenu);
      VerticalAddMenu := TMenuItem.Create(TGPXWayPointPop);
      VerticalAddMenu.Text := 'Add Vertical';
      VerticalAddMenu.OnClick := AddVerticalEvent;
      TGPXWayPointPop.AddObject(VerticalAddMenu);
      PositionAddMenu := TMenuItem.Create(TGPXWayPointPop);
      PositionAddMenu.Text := 'Add Position';
      PositionAddMenu.OnClick := AddPositionEvent;
      TGPXWayPointPop.AddObject(PositionAddMenu);
      AgeofdgpsdataAddMenu := TMenuItem.Create(TGPXWayPointPop);
      AgeofdgpsdataAddMenu.Text := 'Add Ageofdgpsdata';
      AgeofdgpsdataAddMenu.OnClick := AddAgeofdgpsdataEvent;
      TGPXWayPointPop.AddObject(AgeofdgpsdataAddMenu);
      MagvarAddMenu := TMenuItem.Create(TGPXWayPointPop);
      MagvarAddMenu.Text := 'Add Magvar';
      MagvarAddMenu.OnClick := AddMagvarEvent;
      TGPXWayPointPop.AddObject(MagvarAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXWayPointTreeComponent.ClientToScreen(pt);
      TGPXWayPointPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXWayPoint.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXWayPointXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Dgpsid');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Dgpsid.toString);
  Names_Value.Add('Latitude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Latitude);
  Names_Value.Add('Longitude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Longitude);
  Names_Value.Add('Elevation');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Elevation);
  Names_Value.Add('Time');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, Time));
  Names_Value.Add('Geoidheight');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Geoidheight.ToString);
  Names_Value.Add('Symbol');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Symbol);
  Names_Value.Add('Satellites');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Satellites.toString);
  Names_Value.Add('Horizontal');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Horizontal.ToString);
  Names_Value.Add('Fix');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Fix);
  Names_Value.Add('Vertical');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Vertical.ToString);
  Names_Value.Add('Position');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Position.ToString);
  Names_Value.Add('Ageofdgpsdata');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Ageofdgpsdata.ToString);
  Names_Value.Add('Magvar');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Magvar.ToString);
  TGPXWayPointXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXWayPoint.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Dgpsid := _Value.ToInteger;
      end;
    1:
      begin
        Latitude := _Value;
      end;
    2:
      begin
        Longitude := _Value;
      end;
    3:
      begin
        Elevation := _Value;
      end;
    4:
      begin
        Time := StrToDateTimeDef(_Value, Now());
      end;
    5:
      begin
        Geoidheight := _Value.ToDouble;
      end;
    6:
      begin
        Symbol := _Value;
      end;
    7:
      begin
        Satellites := _Value.ToInteger;
      end;
    8:
      begin
        Horizontal := _Value.ToDouble;
      end;
    9:
      begin
        Fix := _Value;
      end;
    10:
      begin
        Vertical := _Value.ToDouble;
      end;
    11:
      begin
        Position := _Value.ToDouble;
      end;
    12:
      begin
        Ageofdgpsdata := _Value.ToDouble;
      end;
    13:
      begin
        Magvar := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TGPXWayPoint.AddDgpsid: Integer;
begin;
  Result := FDgpsid;
  FDgpsidExsit := True;
end;

procedure TGPXWayPoint.SetDgpsid(const _Value: Integer);
begin
  FDgpsidExsit := True;
  FDgpsid := _Value;
end;

procedure TGPXWayPoint.DgpsidRemove;
begin
  if FDgpsidExsit then
  begin
    FDgpsidExsit := False;
  end;
end;

procedure TGPXWayPoint.AddDgpsidEvent(Sender: TObject);
begin
  AddDgpsid;
end;

function TGPXWayPoint.AddLatitude: String;
begin;
  Result := FLatitude;
  FLatitudeExsit := True;
end;

procedure TGPXWayPoint.SetLatitude(const _Value: String);
begin
  FLatitudeExsit := True;
  FLatitude := _Value;
end;

procedure TGPXWayPoint.LatitudeRemove;
begin
  if FLatitudeExsit then
  begin
    FLatitudeExsit := False;
  end;
end;

procedure TGPXWayPoint.AddLatitudeEvent(Sender: TObject);
begin
  AddLatitude;
end;

function TGPXWayPoint.AddLongitude: String;
begin;
  Result := FLongitude;
  FLongitudeExsit := True;
end;

procedure TGPXWayPoint.SetLongitude(const _Value: String);
begin
  FLongitudeExsit := True;
  FLongitude := _Value;
end;

procedure TGPXWayPoint.LongitudeRemove;
begin
  if FLongitudeExsit then
  begin
    FLongitudeExsit := False;
  end;
end;

procedure TGPXWayPoint.AddLongitudeEvent(Sender: TObject);
begin
  AddLongitude;
end;

function TGPXWayPoint.AddElevation: String;
begin;
  Result := FElevation;
  FElevationExsit := True;
end;

procedure TGPXWayPoint.SetElevation(const _Value: String);
begin
  FElevationExsit := True;
  FElevation := _Value;
end;

procedure TGPXWayPoint.ElevationRemove;
begin
  if FElevationExsit then
  begin
    FElevationExsit := False;
  end;
end;

procedure TGPXWayPoint.AddElevationEvent(Sender: TObject);
begin
  AddElevation;
end;

function TGPXWayPoint.AddTime: TDateTime;
begin;
  Result := FTime;
  FTimeExsit := True;
end;

procedure TGPXWayPoint.SetTime(const _Value: TDateTime);
begin
  FTimeExsit := True;
  FTime := _Value;
end;

procedure TGPXWayPoint.TimeRemove;
begin
  if FTimeExsit then
  begin
    FTimeExsit := False;
  end;
end;

procedure TGPXWayPoint.AddTimeEvent(Sender: TObject);
begin
  AddTime;
end;

function TGPXWayPoint.AddGeoidheight: Double;
begin;
  Result := FGeoidheight;
  FGeoidheightExsit := True;
end;

procedure TGPXWayPoint.SetGeoidheight(const _Value: Double);
begin
  FGeoidheightExsit := True;
  FGeoidheight := _Value;
end;

procedure TGPXWayPoint.GeoidheightRemove;
begin
  if FGeoidheightExsit then
  begin
    FGeoidheightExsit := False;
  end;
end;

procedure TGPXWayPoint.AddGeoidheightEvent(Sender: TObject);
begin
  AddGeoidheight;
end;

function TGPXWayPoint.AddSymbol: String;
begin;
  Result := FSymbol;
  FSymbolExsit := True;
end;

procedure TGPXWayPoint.SetSymbol(const _Value: String);
begin
  FSymbolExsit := True;
  FSymbol := _Value;
end;

procedure TGPXWayPoint.SymbolRemove;
begin
  if FSymbolExsit then
  begin
    FSymbolExsit := False;
  end;
end;

procedure TGPXWayPoint.AddSymbolEvent(Sender: TObject);
begin
  AddSymbol;
end;

function TGPXWayPoint.AddSatellites: Integer;
begin;
  Result := FSatellites;
  FSatellitesExsit := True;
end;

procedure TGPXWayPoint.SetSatellites(const _Value: Integer);
begin
  FSatellitesExsit := True;
  FSatellites := _Value;
end;

procedure TGPXWayPoint.SatellitesRemove;
begin
  if FSatellitesExsit then
  begin
    FSatellitesExsit := False;
  end;
end;

procedure TGPXWayPoint.AddSatellitesEvent(Sender: TObject);
begin
  AddSatellites;
end;

function TGPXWayPoint.AddHorizontal: Double;
begin;
  Result := FHorizontal;
  FHorizontalExsit := True;
end;

procedure TGPXWayPoint.SetHorizontal(const _Value: Double);
begin
  FHorizontalExsit := True;
  FHorizontal := _Value;
end;

procedure TGPXWayPoint.HorizontalRemove;
begin
  if FHorizontalExsit then
  begin
    FHorizontalExsit := False;
  end;
end;

procedure TGPXWayPoint.AddHorizontalEvent(Sender: TObject);
begin
  AddHorizontal;
end;

function TGPXWayPoint.AddFix: String;
begin;
  Result := FFix;
  FFixExsit := True;
end;

procedure TGPXWayPoint.SetFix(const _Value: String);
begin
  FFixExsit := True;
  FFix := _Value;
end;

procedure TGPXWayPoint.FixRemove;
begin
  if FFixExsit then
  begin
    FFixExsit := False;
  end;
end;

procedure TGPXWayPoint.AddFixEvent(Sender: TObject);
begin
  AddFix;
end;

function TGPXWayPoint.AddVertical: Double;
begin;
  Result := FVertical;
  FVerticalExsit := True;
end;

procedure TGPXWayPoint.SetVertical(const _Value: Double);
begin
  FVerticalExsit := True;
  FVertical := _Value;
end;

procedure TGPXWayPoint.VerticalRemove;
begin
  if FVerticalExsit then
  begin
    FVerticalExsit := False;
  end;
end;

procedure TGPXWayPoint.AddVerticalEvent(Sender: TObject);
begin
  AddVertical;
end;

function TGPXWayPoint.AddPosition: Double;
begin;
  Result := FPosition;
  FPositionExsit := True;
end;

procedure TGPXWayPoint.SetPosition(const _Value: Double);
begin
  FPositionExsit := True;
  FPosition := _Value;
end;

procedure TGPXWayPoint.PositionRemove;
begin
  if FPositionExsit then
  begin
    FPositionExsit := False;
  end;
end;

procedure TGPXWayPoint.AddPositionEvent(Sender: TObject);
begin
  AddPosition;
end;

function TGPXWayPoint.AddAgeofdgpsdata: Double;
begin;
  Result := FAgeofdgpsdata;
  FAgeofdgpsdataExsit := True;
end;

procedure TGPXWayPoint.SetAgeofdgpsdata(const _Value: Double);
begin
  FAgeofdgpsdataExsit := True;
  FAgeofdgpsdata := _Value;
end;

procedure TGPXWayPoint.AgeofdgpsdataRemove;
begin
  if FAgeofdgpsdataExsit then
  begin
    FAgeofdgpsdataExsit := False;
  end;
end;

procedure TGPXWayPoint.AddAgeofdgpsdataEvent(Sender: TObject);
begin
  AddAgeofdgpsdata;
end;

function TGPXWayPoint.AddMagvar: Double;
begin;
  Result := FMagvar;
  FMagvarExsit := True;
end;

procedure TGPXWayPoint.SetMagvar(const _Value: Double);
begin
  FMagvarExsit := True;
  FMagvar := _Value;
end;

procedure TGPXWayPoint.MagvarRemove;
begin
  if FMagvarExsit then
  begin
    FMagvarExsit := False;
  end;
end;

procedure TGPXWayPoint.AddMagvarEvent(Sender: TObject);
begin
  AddMagvar;
end;

{  Route}
constructor TGPXRoute.Create(par: TXML = nil);
begin
  inherited Create(par);
  FPointss := TList<TGPXWayPoint>.Create;
end;

destructor TGPXRoute.Destroy;
begin
  PointsClear;
  FPointss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXRoute.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  PointsTmp: TGPXWayPoint;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'rtept' then
      begin
        PointsTmp := TGPXWayPoint.Create(Self);
        PointsTmp.FromXML(nodeTmp);
        FPointss.Add(PointsTmp);
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        FNumber := nodeTmp.Text.ToInteger;
        FNumberExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Route Read XML Error!' + node.Xml);
  end;
end;

function TGPXRoute.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NumberTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'rte';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    for I := 0 to FPointss.Count - 1 do
       FPointss.Items[I].ToXML(node, 'rtept');
    if FNumberExsit then
    begin
      NumberTmp := doc.CreateNode('number', ntElement);
      NumberTmp.NodeValue := FNumber.toString;
      node.ChildNodes.Add(NumberTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXRoute.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to PointsCount - 1 do
  begin
    Pointss[I].TreeNodeShape := TreeNodeShape.AddChildObject('Points', Points[I]);
    Points[I].ToTree;
  end;
  if NumberExsit then
    TreeNodeShape.AddChild('Number');
end;

procedure TGPXRoute.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  PointsAddMenu: TMenuItem;
  NumberAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXRoutePop) and Assigned(TGPXRouteTreeComponent) then
    begin
      TGPXRoutePop.Clear;
      PointsAddMenu := TMenuItem.Create(TGPXRoutePop);
      PointsAddMenu.Text := 'Add Points';
      PointsAddMenu.OnClick := AddPointsEvent;
      TGPXRoutePop.AddObject(PointsAddMenu);
      NumberAddMenu := TMenuItem.Create(TGPXRoutePop);
      NumberAddMenu.Text := 'Add Number';
      NumberAddMenu.OnClick := AddNumberEvent;
      TGPXRoutePop.AddObject(NumberAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXRouteTreeComponent.ClientToScreen(pt);
      TGPXRoutePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXRoute.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXRouteXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Number');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Number.toString);
  TGPXRouteXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXRoute.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Number := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

function TGPXRoute.AddPoints: TGPXWayPoint;
var
  Pointstmp: TGPXWayPoint;
begin;
  Pointstmp := TGPXWayPoint.Create(Self);
  FPointss.Add(Pointstmp);
  Result := Pointstmp;
end;

procedure TGPXRoute.SetPointss(const _Value: TList<TGPXWayPoint>);
begin
  PointsClear;
  FPointss := _Value;
end;

procedure TGPXRoute.PointsClear;
begin
  while FPointss.Count > 0 do
  begin
    FPointss.Items[0].Free;
    FPointss.Delete(0);
  end;
end;

function TGPXRoute.PointsCount: Integer;
begin
  Result := FPointss.Count;
end;

function TGPXRoute.GetPoints(Index: Integer): TGPXWayPoint;
begin
  Result := FPointss[Index];
end;

procedure TGPXRoute.SetPoints(Index: Integer;
  const _Value: TGPXWayPoint);
begin
  _Value.Parent := Self;
  FPointss[Index].Free;
  FPointss[Index] := _Value;
end;

procedure TGPXRoute.RemovePoints(_Value: TGPXWayPoint);
begin
  FPointss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXRoute.DeletePoints(Index: Integer);
begin
  FPointss.Items[Index].Free;
  FPointss.Delete(Index);
end;

procedure TGPXRoute.AddPointsEvent(Sender: TObject);
var
  tmp: TGPXWayPoint;
begin
  tmp := AddPoints;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Points', tmp);
  tmp.ToTree;
end;

function TGPXRoute.AddNumber: Integer;
begin;
  Result := FNumber;
  FNumberExsit := True;
end;

procedure TGPXRoute.SetNumber(const _Value: Integer);
begin
  FNumberExsit := True;
  FNumber := _Value;
end;

procedure TGPXRoute.NumberRemove;
begin
  if FNumberExsit then
  begin
    FNumberExsit := False;
  end;
end;

procedure TGPXRoute.AddNumberEvent(Sender: TObject);
begin
  AddNumber;
end;

{  Track}
constructor TGPXTrack.Create(par: TXML = nil);
begin
  inherited Create(par);
  FTrackSegmentss := TList<TGPXTrackSegment>.Create;
end;

destructor TGPXTrack.Destroy;
begin
  TrackSegmentsClear;
  FTrackSegmentss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXTrack.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  TrackSegmentsTmp: TGPXTrackSegment;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'trkseg' then
      begin
        TrackSegmentsTmp := TGPXTrackSegment.Create(Self);
        TrackSegmentsTmp.FromXML(nodeTmp);
        FTrackSegmentss.Add(TrackSegmentsTmp);
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        FNumber := nodeTmp.Text.ToInteger;
        FNumberExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Track Read XML Error!' + node.Xml);
  end;
end;

function TGPXTrack.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NumberTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'trk';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    for I := 0 to FTrackSegmentss.Count - 1 do
       FTrackSegmentss.Items[I].ToXML(node, 'trkseg');
    if FNumberExsit then
    begin
      NumberTmp := doc.CreateNode('number', ntElement);
      NumberTmp.NodeValue := FNumber.toString;
      node.ChildNodes.Add(NumberTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXTrack.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to TrackSegmentsCount - 1 do
  begin
    TrackSegmentss[I].TreeNodeShape := TreeNodeShape.AddChildObject('TrackSegments', TrackSegments[I]);
    TrackSegments[I].ToTree;
  end;
  if NumberExsit then
    TreeNodeShape.AddChild('Number');
end;

procedure TGPXTrack.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  TrackSegmentsAddMenu: TMenuItem;
  NumberAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXTrackPop) and Assigned(TGPXTrackTreeComponent) then
    begin
      TGPXTrackPop.Clear;
      TrackSegmentsAddMenu := TMenuItem.Create(TGPXTrackPop);
      TrackSegmentsAddMenu.Text := 'Add TrackSegments';
      TrackSegmentsAddMenu.OnClick := AddTrackSegmentsEvent;
      TGPXTrackPop.AddObject(TrackSegmentsAddMenu);
      NumberAddMenu := TMenuItem.Create(TGPXTrackPop);
      NumberAddMenu.Text := 'Add Number';
      NumberAddMenu.OnClick := AddNumberEvent;
      TGPXTrackPop.AddObject(NumberAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXTrackTreeComponent.ClientToScreen(pt);
      TGPXTrackPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXTrack.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXTrackXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Number');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Number.toString);
  TGPXTrackXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXTrack.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Number := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

function TGPXTrack.AddTrackSegments: TGPXTrackSegment;
var
  TrackSegmentstmp: TGPXTrackSegment;
begin;
  TrackSegmentstmp := TGPXTrackSegment.Create(Self);
  FTrackSegmentss.Add(TrackSegmentstmp);
  Result := TrackSegmentstmp;
end;

procedure TGPXTrack.SetTrackSegmentss(const _Value: TList<TGPXTrackSegment>);
begin
  TrackSegmentsClear;
  FTrackSegmentss := _Value;
end;

procedure TGPXTrack.TrackSegmentsClear;
begin
  while FTrackSegmentss.Count > 0 do
  begin
    FTrackSegmentss.Items[0].Free;
    FTrackSegmentss.Delete(0);
  end;
end;

function TGPXTrack.TrackSegmentsCount: Integer;
begin
  Result := FTrackSegmentss.Count;
end;

function TGPXTrack.GetTrackSegments(Index: Integer): TGPXTrackSegment;
begin
  Result := FTrackSegmentss[Index];
end;

procedure TGPXTrack.SetTrackSegments(Index: Integer;
  const _Value: TGPXTrackSegment);
begin
  _Value.Parent := Self;
  FTrackSegmentss[Index].Free;
  FTrackSegmentss[Index] := _Value;
end;

procedure TGPXTrack.RemoveTrackSegments(_Value: TGPXTrackSegment);
begin
  FTrackSegmentss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXTrack.DeleteTrackSegments(Index: Integer);
begin
  FTrackSegmentss.Items[Index].Free;
  FTrackSegmentss.Delete(Index);
end;

procedure TGPXTrack.AddTrackSegmentsEvent(Sender: TObject);
var
  tmp: TGPXTrackSegment;
begin
  tmp := AddTrackSegments;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('TrackSegments', tmp);
  tmp.ToTree;
end;

function TGPXTrack.AddNumber: Integer;
begin;
  Result := FNumber;
  FNumberExsit := True;
end;

procedure TGPXTrack.SetNumber(const _Value: Integer);
begin
  FNumberExsit := True;
  FNumber := _Value;
end;

procedure TGPXTrack.NumberRemove;
begin
  if FNumberExsit then
  begin
    FNumberExsit := False;
  end;
end;

procedure TGPXTrack.AddNumberEvent(Sender: TObject);
begin
  AddNumber;
end;

{  TrackSegment}
constructor TGPXTrackSegment.Create(par: TXML = nil);
begin
  inherited Create(par);
  FPointss := TList<TGPXWayPoint>.Create;
  FExtensionss := TList<TXMLBase>.Create;
end;

destructor TGPXTrackSegment.Destroy;
begin
  PointsClear;
  FPointss.Free;
  ExtensionsClear;
  FExtensionss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXTrackSegment.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  PointsTmp: TGPXWayPoint;
  ExtensionsTmp: TXMLBase;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'trkpt' then
      begin
        PointsTmp := TGPXWayPoint.Create(Self);
        PointsTmp.FromXML(nodeTmp);
        FPointss.Add(PointsTmp);
      end
      else if nodeTmp.NodeName = 'extensions' then
      begin
        ExtensionsTmp := TXMLBase.Create(Self);
        ExtensionsTmp.FromXML(nodeTmp);
        FExtensionss.Add(ExtensionsTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('TrackSegment Read XML Error!' + node.Xml);
  end;
end;

function TGPXTrackSegment.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'trkseg';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FPointss.Count - 1 do
       FPointss.Items[I].ToXML(node, 'trkpt');
    for I := 0 to FExtensionss.Count - 1 do
       FExtensionss.Items[I].ToXML(node, 'extensions');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXTrackSegment.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to PointsCount - 1 do
  begin
    Pointss[I].TreeNodeShape := TreeNodeShape.AddChildObject('Points', Points[I]);
    Points[I].ToTree;
  end;
  for I := 0 to ExtensionsCount - 1 do
  begin
    Extensionss[I].TreeNodeShape := TreeNodeShape.AddChildObject('Extensions', Extensions[I]);
    Extensions[I].ToTree;
  end;
end;

procedure TGPXTrackSegment.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  PointsAddMenu: TMenuItem;
  ExtensionsAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXTrackSegmentPop) and Assigned(TGPXTrackSegmentTreeComponent) then
    begin
      TGPXTrackSegmentPop.Clear;
      PointsAddMenu := TMenuItem.Create(TGPXTrackSegmentPop);
      PointsAddMenu.Text := 'Add Points';
      PointsAddMenu.OnClick := AddPointsEvent;
      TGPXTrackSegmentPop.AddObject(PointsAddMenu);
      ExtensionsAddMenu := TMenuItem.Create(TGPXTrackSegmentPop);
      ExtensionsAddMenu.Text := 'Add Extensions';
      ExtensionsAddMenu.OnClick := AddExtensionsEvent;
      TGPXTrackSegmentPop.AddObject(ExtensionsAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXTrackSegmentTreeComponent.ClientToScreen(pt);
      TGPXTrackSegmentPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXTrackSegment.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXTrackSegmentXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TGPXTrackSegmentXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXTrackSegment.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

function TGPXTrackSegment.AddPoints: TGPXWayPoint;
var
  Pointstmp: TGPXWayPoint;
begin;
  Pointstmp := TGPXWayPoint.Create(Self);
  FPointss.Add(Pointstmp);
  Result := Pointstmp;
end;

procedure TGPXTrackSegment.SetPointss(const _Value: TList<TGPXWayPoint>);
begin
  PointsClear;
  FPointss := _Value;
end;

procedure TGPXTrackSegment.PointsClear;
begin
  while FPointss.Count > 0 do
  begin
    FPointss.Items[0].Free;
    FPointss.Delete(0);
  end;
end;

function TGPXTrackSegment.PointsCount: Integer;
begin
  Result := FPointss.Count;
end;

function TGPXTrackSegment.GetPoints(Index: Integer): TGPXWayPoint;
begin
  Result := FPointss[Index];
end;

procedure TGPXTrackSegment.SetPoints(Index: Integer;
  const _Value: TGPXWayPoint);
begin
  _Value.Parent := Self;
  FPointss[Index].Free;
  FPointss[Index] := _Value;
end;

procedure TGPXTrackSegment.RemovePoints(_Value: TGPXWayPoint);
begin
  FPointss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXTrackSegment.DeletePoints(Index: Integer);
begin
  FPointss.Items[Index].Free;
  FPointss.Delete(Index);
end;

procedure TGPXTrackSegment.AddPointsEvent(Sender: TObject);
var
  tmp: TGPXWayPoint;
begin
  tmp := AddPoints;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Points', tmp);
  tmp.ToTree;
end;

function TGPXTrackSegment.AddExtensions: TXMLBase;
var
  Extensionstmp: TXMLBase;
begin;
  Extensionstmp := TXMLBase.Create(Self);
  FExtensionss.Add(Extensionstmp);
  Result := Extensionstmp;
end;

procedure TGPXTrackSegment.SetExtensionss(const _Value: TList<TXMLBase>);
begin
  ExtensionsClear;
  FExtensionss := _Value;
end;

procedure TGPXTrackSegment.ExtensionsClear;
begin
  while FExtensionss.Count > 0 do
  begin
    FExtensionss.Items[0].Free;
    FExtensionss.Delete(0);
  end;
end;

function TGPXTrackSegment.ExtensionsCount: Integer;
begin
  Result := FExtensionss.Count;
end;

function TGPXTrackSegment.GetExtensions(Index: Integer): TXMLBase;
begin
  Result := FExtensionss[Index];
end;

procedure TGPXTrackSegment.SetExtensions(Index: Integer;
  const _Value: TXMLBase);
begin
  _Value.Parent := Self;
  FExtensionss[Index].Free;
  FExtensionss[Index] := _Value;
end;

procedure TGPXTrackSegment.RemoveExtensions(_Value: TXMLBase);
begin
  FExtensionss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXTrackSegment.DeleteExtensions(Index: Integer);
begin
  FExtensionss.Items[Index].Free;
  FExtensionss.Delete(Index);
end;

procedure TGPXTrackSegment.AddExtensionsEvent(Sender: TObject);
var
  tmp: TXMLBase;
begin
  tmp := AddExtensions;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Extensions', tmp);
  tmp.ToTree;
end;

{  GPXMetadata}
constructor TGPXMetadata.Create(par: TXML = nil);
begin
  inherited Create(par);
  Fextensionss := TList<TXMLBase>.Create;
end;

destructor TGPXMetadata.Destroy;
begin
  if FlinkExsit then
    Flink.Free;
  if FauthorExsit then
    Fauthor.Free;
  if FcopyrightExsit then
    Fcopyright.Free;
  if FboundsExsit then
    Fbounds.Free;
  extensionsClear;
  Fextensionss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXMetadata.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  extensionsTmp: TXMLBase;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'link' then
      begin
        Flink := TGPXLink.Create(Self);
        Flink.FromXML(nodeTmp);
        FlinkExsit := True;
      end
      else if nodeTmp.NodeName = 'author' then
      begin
        Fauthor := TGPXPerson.Create(Self);
        Fauthor.FromXML(nodeTmp);
        FauthorExsit := True;
      end
      else if nodeTmp.NodeName = 'copyright' then
      begin
        Fcopyright := TGPXCopyright.Create(Self);
        Fcopyright.FromXML(nodeTmp);
        FcopyrightExsit := True;
      end
      else if nodeTmp.NodeName = 'bounds' then
      begin
        Fbounds := TGPXBounds.Create(Self);
        Fbounds.FromXML(nodeTmp);
        FboundsExsit := True;
      end
      else if nodeTmp.NodeName = 'extensions' then
      begin
        extensionsTmp := TXMLBase.Create(Self);
        extensionsTmp.FromXML(nodeTmp);
        Fextensionss.Add(extensionsTmp);
      end
      else if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'desc' then
      begin
        Fdescription := nodeTmp.Text;
        FdescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'time' then
      begin
        Ftime := StrToDateTimeDef(nodeTmp.Text, Now());
        FtimeExsit := True;
      end
      else if nodeTmp.NodeName = 'keywords' then
      begin
        Fkeywords := nodeTmp.Text;
        FkeywordsExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('GPXMetadata Read XML Error!' + node.Xml);
  end;
end;

function TGPXMetadata.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  descriptionTmp: IXMLNode;
  timeTmp: IXMLNode;
  keywordsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'metadata';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FlinkExsit then
      Flink.ToXML(node, 'link');
    if FauthorExsit then
      Fauthor.ToXML(node, 'author');
    if FcopyrightExsit then
      Fcopyright.ToXML(node, 'copyright');
    if FboundsExsit then
      Fbounds.ToXML(node, 'bounds');
    for I := 0 to Fextensionss.Count - 1 do
       Fextensionss.Items[I].ToXML(node, 'extensions');
    if FnameExsit then
    begin
      nameTmp := doc.CreateNode('name', ntElement);
      nameTmp.NodeValue := Fname;
      node.ChildNodes.Add(nameTmp);
    end;
    if FdescriptionExsit then
    begin
      descriptionTmp := doc.CreateNode('desc', ntElement);
      descriptionTmp.NodeValue := Fdescription;
      node.ChildNodes.Add(descriptionTmp);
    end;
    if FtimeExsit then
    begin
      timeTmp := doc.CreateNode('time', ntElement);
      timeTmp.NodeValue := FormatDateTime(XMLDTFormat, Ftime);
      node.ChildNodes.Add(timeTmp);
    end;
    if FkeywordsExsit then
    begin
      keywordsTmp := doc.CreateNode('keywords', ntElement);
      keywordsTmp.NodeValue := Fkeywords;
      node.ChildNodes.Add(keywordsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXMetadata.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if linkExsit then
  begin
    link.TreeNodeShape := TreeNodeShape.AddChildObject('link', link);
    link.ToTree;
  end;
  if authorExsit then
  begin
    author.TreeNodeShape := TreeNodeShape.AddChildObject('author', author);
    author.ToTree;
  end;
  if copyrightExsit then
  begin
    copyright.TreeNodeShape := TreeNodeShape.AddChildObject('copyright', copyright);
    copyright.ToTree;
  end;
  if boundsExsit then
  begin
    bounds.TreeNodeShape := TreeNodeShape.AddChildObject('bounds', bounds);
    bounds.ToTree;
  end;
  for I := 0 to extensionsCount - 1 do
  begin
    extensionss[I].TreeNodeShape := TreeNodeShape.AddChildObject('extensions', extensions[I]);
    extensions[I].ToTree;
  end;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if descriptionExsit then
    TreeNodeShape.AddChild('description');
  if timeExsit then
    TreeNodeShape.AddChild('time');
  if keywordsExsit then
    TreeNodeShape.AddChild('keywords');
end;

procedure TGPXMetadata.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  linkAddMenu: TMenuItem;
  authorAddMenu: TMenuItem;
  copyrightAddMenu: TMenuItem;
  boundsAddMenu: TMenuItem;
  extensionsAddMenu: TMenuItem;
  nameAddMenu: TMenuItem;
  descriptionAddMenu: TMenuItem;
  timeAddMenu: TMenuItem;
  keywordsAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXMetadataPop) and Assigned(TGPXMetadataTreeComponent) then
    begin
      TGPXMetadataPop.Clear;
      linkAddMenu := TMenuItem.Create(TGPXMetadataPop);
      linkAddMenu.Text := 'Add link';
      linkAddMenu.OnClick := AddlinkEvent;
      TGPXMetadataPop.AddObject(linkAddMenu);
      authorAddMenu := TMenuItem.Create(TGPXMetadataPop);
      authorAddMenu.Text := 'Add author';
      authorAddMenu.OnClick := AddauthorEvent;
      TGPXMetadataPop.AddObject(authorAddMenu);
      copyrightAddMenu := TMenuItem.Create(TGPXMetadataPop);
      copyrightAddMenu.Text := 'Add copyright';
      copyrightAddMenu.OnClick := AddcopyrightEvent;
      TGPXMetadataPop.AddObject(copyrightAddMenu);
      boundsAddMenu := TMenuItem.Create(TGPXMetadataPop);
      boundsAddMenu.Text := 'Add bounds';
      boundsAddMenu.OnClick := AddboundsEvent;
      TGPXMetadataPop.AddObject(boundsAddMenu);
      extensionsAddMenu := TMenuItem.Create(TGPXMetadataPop);
      extensionsAddMenu.Text := 'Add extensions';
      extensionsAddMenu.OnClick := AddextensionsEvent;
      TGPXMetadataPop.AddObject(extensionsAddMenu);
      nameAddMenu := TMenuItem.Create(TGPXMetadataPop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TGPXMetadataPop.AddObject(nameAddMenu);
      descriptionAddMenu := TMenuItem.Create(TGPXMetadataPop);
      descriptionAddMenu.Text := 'Add description';
      descriptionAddMenu.OnClick := AdddescriptionEvent;
      TGPXMetadataPop.AddObject(descriptionAddMenu);
      timeAddMenu := TMenuItem.Create(TGPXMetadataPop);
      timeAddMenu.Text := 'Add time';
      timeAddMenu.OnClick := AddtimeEvent;
      TGPXMetadataPop.AddObject(timeAddMenu);
      keywordsAddMenu := TMenuItem.Create(TGPXMetadataPop);
      keywordsAddMenu.Text := 'Add keywords';
      keywordsAddMenu.OnClick := AddkeywordsEvent;
      TGPXMetadataPop.AddObject(keywordsAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXMetadataTreeComponent.ClientToScreen(pt);
      TGPXMetadataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXMetadata.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXMetadataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(description);
  Names_Value.Add('time');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, time));
  Names_Value.Add('keywords');
  Types_Value.Add(xs_string);
  _Values_Value.Add(keywords);
  TGPXMetadataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXMetadata.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        description := _Value;
      end;
    2:
      begin
        time := StrToDateTimeDef(_Value, Now());
      end;
    3:
      begin
        keywords := _Value;
      end;
  end;
  ToTree;
end;

function TGPXMetadata.Addlink: TGPXLink;
begin;
  if not FlinkExsit then
    Flink := TGPXLink.Create(Self);
  Result := Flink;
  FlinkExsit := True;
end;

procedure TGPXMetadata.Setlink(const _Value: TGPXLink);
begin
  if FlinkExsit then
    Flink.Free;
  FlinkExsit := True;
  Flink := _Value;
  Flink.Parent := Self;
end;

procedure TGPXMetadata.linkRemove;
begin
  if FlinkExsit then
  begin
    Flink.Free;
    FlinkExsit := False;
  end;
end;

procedure TGPXMetadata.AddlinkEvent(Sender: TObject);
begin
  Addlink;
  Flink.ToTree;
end;

function TGPXMetadata.Addauthor: TGPXPerson;
begin;
  if not FauthorExsit then
    Fauthor := TGPXPerson.Create(Self);
  Result := Fauthor;
  FauthorExsit := True;
end;

procedure TGPXMetadata.Setauthor(const _Value: TGPXPerson);
begin
  if FauthorExsit then
    Fauthor.Free;
  FauthorExsit := True;
  Fauthor := _Value;
  Fauthor.Parent := Self;
end;

procedure TGPXMetadata.authorRemove;
begin
  if FauthorExsit then
  begin
    Fauthor.Free;
    FauthorExsit := False;
  end;
end;

procedure TGPXMetadata.AddauthorEvent(Sender: TObject);
begin
  Addauthor;
  Fauthor.ToTree;
end;

function TGPXMetadata.Addcopyright: TGPXCopyright;
begin;
  if not FcopyrightExsit then
    Fcopyright := TGPXCopyright.Create(Self);
  Result := Fcopyright;
  FcopyrightExsit := True;
end;

procedure TGPXMetadata.Setcopyright(const _Value: TGPXCopyright);
begin
  if FcopyrightExsit then
    Fcopyright.Free;
  FcopyrightExsit := True;
  Fcopyright := _Value;
  Fcopyright.Parent := Self;
end;

procedure TGPXMetadata.copyrightRemove;
begin
  if FcopyrightExsit then
  begin
    Fcopyright.Free;
    FcopyrightExsit := False;
  end;
end;

procedure TGPXMetadata.AddcopyrightEvent(Sender: TObject);
begin
  Addcopyright;
  Fcopyright.ToTree;
end;

function TGPXMetadata.Addbounds: TGPXBounds;
begin;
  if not FboundsExsit then
    Fbounds := TGPXBounds.Create(Self);
  Result := Fbounds;
  FboundsExsit := True;
end;

procedure TGPXMetadata.Setbounds(const _Value: TGPXBounds);
begin
  if FboundsExsit then
    Fbounds.Free;
  FboundsExsit := True;
  Fbounds := _Value;
  Fbounds.Parent := Self;
end;

procedure TGPXMetadata.boundsRemove;
begin
  if FboundsExsit then
  begin
    Fbounds.Free;
    FboundsExsit := False;
  end;
end;

procedure TGPXMetadata.AddboundsEvent(Sender: TObject);
begin
  Addbounds;
  Fbounds.ToTree;
end;

function TGPXMetadata.Addextensions: TXMLBase;
var
  extensionstmp: TXMLBase;
begin;
  extensionstmp := TXMLBase.Create(Self);
  Fextensionss.Add(extensionstmp);
  Result := extensionstmp;
end;

procedure TGPXMetadata.Setextensionss(const _Value: TList<TXMLBase>);
begin
  extensionsClear;
  Fextensionss := _Value;
end;

procedure TGPXMetadata.extensionsClear;
begin
  while Fextensionss.Count > 0 do
  begin
    Fextensionss.Items[0].Free;
    Fextensionss.Delete(0);
  end;
end;

function TGPXMetadata.extensionsCount: Integer;
begin
  Result := Fextensionss.Count;
end;

function TGPXMetadata.Getextensions(Index: Integer): TXMLBase;
begin
  Result := Fextensionss[Index];
end;

procedure TGPXMetadata.Setextensions(Index: Integer;
  const _Value: TXMLBase);
begin
  _Value.Parent := Self;
  Fextensionss[Index].Free;
  Fextensionss[Index] := _Value;
end;

procedure TGPXMetadata.Removeextensions(_Value: TXMLBase);
begin
  Fextensionss.Remove(_Value);
  _Value.Free;
end;

procedure TGPXMetadata.Deleteextensions(Index: Integer);
begin
  Fextensionss.Items[Index].Free;
  Fextensionss.Delete(Index);
end;

procedure TGPXMetadata.AddextensionsEvent(Sender: TObject);
var
  tmp: TXMLBase;
begin
  tmp := Addextensions;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('extensions', tmp);
  tmp.ToTree;
end;

function TGPXMetadata.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TGPXMetadata.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TGPXMetadata.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TGPXMetadata.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TGPXMetadata.Adddescription: String;
begin;
  Result := Fdescription;
  FdescriptionExsit := True;
end;

procedure TGPXMetadata.Setdescription(const _Value: String);
begin
  FdescriptionExsit := True;
  Fdescription := _Value;
end;

procedure TGPXMetadata.descriptionRemove;
begin
  if FdescriptionExsit then
  begin
    FdescriptionExsit := False;
  end;
end;

procedure TGPXMetadata.AdddescriptionEvent(Sender: TObject);
begin
  Adddescription;
end;

function TGPXMetadata.Addtime: TDateTime;
begin;
  Result := Ftime;
  FtimeExsit := True;
end;

procedure TGPXMetadata.Settime(const _Value: TDateTime);
begin
  FtimeExsit := True;
  Ftime := _Value;
end;

procedure TGPXMetadata.timeRemove;
begin
  if FtimeExsit then
  begin
    FtimeExsit := False;
  end;
end;

procedure TGPXMetadata.AddtimeEvent(Sender: TObject);
begin
  Addtime;
end;

function TGPXMetadata.Addkeywords: String;
begin;
  Result := Fkeywords;
  FkeywordsExsit := True;
end;

procedure TGPXMetadata.Setkeywords(const _Value: String);
begin
  FkeywordsExsit := True;
  Fkeywords := _Value;
end;

procedure TGPXMetadata.keywordsRemove;
begin
  if FkeywordsExsit then
  begin
    FkeywordsExsit := False;
  end;
end;

procedure TGPXMetadata.AddkeywordsEvent(Sender: TObject);
begin
  Addkeywords;
end;

{  Copyright}
constructor TGPXCopyright.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXCopyright.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXCopyright.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'year' then
      begin
        Fyear := nodeTmp.Text.ToInteger;
        FyearExsit := True;
      end
      else if nodeTmp.NodeName = 'license' then
      begin
        Flicense := nodeTmp.Text;
        FlicenseExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'author' then
      begin
        Fauthor := nodeTmp.Text;
        FauthorExsit := True;
      end;
    end;
  except
    raise Exception.Create('Copyright Read XML Error!' + node.Xml);
  end;
end;

function TGPXCopyright.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  yearTmp: IXMLNode;
  authorTmp: IXMLNode;
  licenseTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'copyright';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FyearExsit then
    begin
      yearTmp := doc.CreateNode('year', ntElement);
      yearTmp.NodeValue := Fyear.toString;
      node.ChildNodes.Add(yearTmp);
    end;
    if FlicenseExsit then
    begin
      licenseTmp := doc.CreateNode('license', ntElement);
      licenseTmp.NodeValue := Flicense;
      node.ChildNodes.Add(licenseTmp);
    end;
    if FauthorExsit then 
    begin
      authorTmp := doc.CreateNode('author', ntAttribute);
      authorTmp.NodeValue := Fauthor;
      node.AttributeNodes.Add(authorTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXCopyright.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if yearExsit then
    TreeNodeShape.AddChild('year');
  if authorExsit then
    TreeNodeShape.AddChild('author');
  if licenseExsit then
    TreeNodeShape.AddChild('license');
end;

procedure TGPXCopyright.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  yearAddMenu: TMenuItem;
  authorAddMenu: TMenuItem;
  licenseAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXCopyrightPop) and Assigned(TGPXCopyrightTreeComponent) then
    begin
      TGPXCopyrightPop.Clear;
      yearAddMenu := TMenuItem.Create(TGPXCopyrightPop);
      yearAddMenu.Text := 'Add year';
      yearAddMenu.OnClick := AddyearEvent;
      TGPXCopyrightPop.AddObject(yearAddMenu);
      authorAddMenu := TMenuItem.Create(TGPXCopyrightPop);
      authorAddMenu.Text := 'Add author';
      authorAddMenu.OnClick := AddauthorEvent;
      TGPXCopyrightPop.AddObject(authorAddMenu);
      licenseAddMenu := TMenuItem.Create(TGPXCopyrightPop);
      licenseAddMenu.Text := 'Add license';
      licenseAddMenu.OnClick := AddlicenseEvent;
      TGPXCopyrightPop.AddObject(licenseAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXCopyrightTreeComponent.ClientToScreen(pt);
      TGPXCopyrightPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXCopyright.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXCopyrightXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('year');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(year.toString);
  Names_Value.Add('author');
  Types_Value.Add(xs_string);
  _Values_Value.Add(author);
  Names_Value.Add('license');
  Types_Value.Add(xs_string);
  _Values_Value.Add(license);
  TGPXCopyrightXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXCopyright.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        year := _Value.ToInteger;
      end;
    1:
      begin
        author := _Value;
      end;
    2:
      begin
        license := _Value;
      end;
  end;
  ToTree;
end;

function TGPXCopyright.Addyear: Integer;
begin;
  Result := Fyear;
  FyearExsit := True;
end;

procedure TGPXCopyright.Setyear(const _Value: Integer);
begin
  FyearExsit := True;
  Fyear := _Value;
end;

procedure TGPXCopyright.yearRemove;
begin
  if FyearExsit then
  begin
    FyearExsit := False;
  end;
end;

procedure TGPXCopyright.AddyearEvent(Sender: TObject);
begin
  Addyear;
end;

function TGPXCopyright.Addauthor: String;
begin;
  Result := Fauthor;
  FauthorExsit := True;
end;

procedure TGPXCopyright.Setauthor(const _Value: String);
begin
  FauthorExsit := True;
  Fauthor := _Value;
end;

procedure TGPXCopyright.authorRemove;
begin
  if FauthorExsit then
  begin
    FauthorExsit := False;
  end;
end;

procedure TGPXCopyright.AddauthorEvent(Sender: TObject);
begin
  Addauthor;
end;

function TGPXCopyright.Addlicense: String;
begin;
  Result := Flicense;
  FlicenseExsit := True;
end;

procedure TGPXCopyright.Setlicense(const _Value: String);
begin
  FlicenseExsit := True;
  Flicense := _Value;
end;

procedure TGPXCopyright.licenseRemove;
begin
  if FlicenseExsit then
  begin
    FlicenseExsit := False;
  end;
end;

procedure TGPXCopyright.AddlicenseEvent(Sender: TObject);
begin
  Addlicense;
end;

{  Link}
constructor TGPXLink.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXLink.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXLink.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'text' then
      begin
        FText := nodeTmp.Text;
        FTextExsit := True;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        FTypeS := nodeTmp.Text;
        FTypeSExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'href' then
      begin
        FHref := nodeTmp.Text;
        FHrefExsit := True;
      end;
    end;
  except
    raise Exception.Create('Link Read XML Error!' + node.Xml);
  end;
end;

function TGPXLink.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  TextTmp: IXMLNode;
  TypeSTmp: IXMLNode;
  HrefTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'link';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FTextExsit then
    begin
      TextTmp := doc.CreateNode('text', ntElement);
      TextTmp.NodeValue := FText;
      node.ChildNodes.Add(TextTmp);
    end;
    if FTypeSExsit then
    begin
      TypeSTmp := doc.CreateNode('type', ntElement);
      TypeSTmp.NodeValue := FTypeS;
      node.ChildNodes.Add(TypeSTmp);
    end;
    if FHrefExsit then 
    begin
      HrefTmp := doc.CreateNode('href', ntAttribute);
      HrefTmp.NodeValue := FHref;
      node.AttributeNodes.Add(HrefTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXLink.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if TextExsit then
    TreeNodeShape.AddChild('Text');
  if TypeSExsit then
    TreeNodeShape.AddChild('TypeS');
  if HrefExsit then
    TreeNodeShape.AddChild('Href');
end;

procedure TGPXLink.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  TextAddMenu: TMenuItem;
  TypeSAddMenu: TMenuItem;
  HrefAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXLinkPop) and Assigned(TGPXLinkTreeComponent) then
    begin
      TGPXLinkPop.Clear;
      TextAddMenu := TMenuItem.Create(TGPXLinkPop);
      TextAddMenu.Text := 'Add Text';
      TextAddMenu.OnClick := AddTextEvent;
      TGPXLinkPop.AddObject(TextAddMenu);
      TypeSAddMenu := TMenuItem.Create(TGPXLinkPop);
      TypeSAddMenu.Text := 'Add TypeS';
      TypeSAddMenu.OnClick := AddTypeSEvent;
      TGPXLinkPop.AddObject(TypeSAddMenu);
      HrefAddMenu := TMenuItem.Create(TGPXLinkPop);
      HrefAddMenu.Text := 'Add Href';
      HrefAddMenu.OnClick := AddHrefEvent;
      TGPXLinkPop.AddObject(HrefAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXLinkTreeComponent.ClientToScreen(pt);
      TGPXLinkPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXLink.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXLinkXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Text');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Text);
  Names_Value.Add('TypeS');
  Types_Value.Add(xs_string);
  _Values_Value.Add(TypeS);
  Names_Value.Add('Href');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Href);
  TGPXLinkXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXLink.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Text := _Value;
      end;
    1:
      begin
        TypeS := _Value;
      end;
    2:
      begin
        Href := _Value;
      end;
  end;
  ToTree;
end;

function TGPXLink.AddText: String;
begin;
  Result := FText;
  FTextExsit := True;
end;

procedure TGPXLink.SetText(const _Value: String);
begin
  FTextExsit := True;
  FText := _Value;
end;

procedure TGPXLink.TextRemove;
begin
  if FTextExsit then
  begin
    FTextExsit := False;
  end;
end;

procedure TGPXLink.AddTextEvent(Sender: TObject);
begin
  AddText;
end;

function TGPXLink.AddTypeS: String;
begin;
  Result := FTypeS;
  FTypeSExsit := True;
end;

procedure TGPXLink.SetTypeS(const _Value: String);
begin
  FTypeSExsit := True;
  FTypeS := _Value;
end;

procedure TGPXLink.TypeSRemove;
begin
  if FTypeSExsit then
  begin
    FTypeSExsit := False;
  end;
end;

procedure TGPXLink.AddTypeSEvent(Sender: TObject);
begin
  AddTypeS;
end;

function TGPXLink.AddHref: String;
begin;
  Result := FHref;
  FHrefExsit := True;
end;

procedure TGPXLink.SetHref(const _Value: String);
begin
  FHrefExsit := True;
  FHref := _Value;
end;

procedure TGPXLink.HrefRemove;
begin
  if FHrefExsit then
  begin
    FHrefExsit := False;
  end;
end;

procedure TGPXLink.AddHrefEvent(Sender: TObject);
begin
  AddHref;
end;

{  Email}
constructor TGPXEmail.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXEmail.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXEmail.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'id' then
      begin
        FID := nodeTmp.Text;
        FIDExsit := True;
      end
      else if nodeTmp.NodeName = 'domain' then
      begin
        FDomain := nodeTmp.Text;
        FDomainExsit := True;
      end;
    end;
  except
    raise Exception.Create('Email Read XML Error!' + node.Xml);
  end;
end;

function TGPXEmail.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  IDTmp: IXMLNode;
  DomainTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'email';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FIDExsit then 
    begin
      IDTmp := doc.CreateNode('id', ntAttribute);
      IDTmp.NodeValue := FID;
      node.AttributeNodes.Add(IDTmp);
    end;
    if FDomainExsit then 
    begin
      DomainTmp := doc.CreateNode('domain', ntAttribute);
      DomainTmp.NodeValue := FDomain;
      node.AttributeNodes.Add(DomainTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXEmail.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if IDExsit then
    TreeNodeShape.AddChild('ID');
  if DomainExsit then
    TreeNodeShape.AddChild('Domain');
end;

procedure TGPXEmail.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  IDAddMenu: TMenuItem;
  DomainAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXEmailPop) and Assigned(TGPXEmailTreeComponent) then
    begin
      TGPXEmailPop.Clear;
      IDAddMenu := TMenuItem.Create(TGPXEmailPop);
      IDAddMenu.Text := 'Add ID';
      IDAddMenu.OnClick := AddIDEvent;
      TGPXEmailPop.AddObject(IDAddMenu);
      DomainAddMenu := TMenuItem.Create(TGPXEmailPop);
      DomainAddMenu.Text := 'Add Domain';
      DomainAddMenu.OnClick := AddDomainEvent;
      TGPXEmailPop.AddObject(DomainAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXEmailTreeComponent.ClientToScreen(pt);
      TGPXEmailPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXEmail.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXEmailXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('ID');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ID);
  Names_Value.Add('Domain');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Domain);
  TGPXEmailXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXEmail.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        ID := _Value;
      end;
    1:
      begin
        Domain := _Value;
      end;
  end;
  ToTree;
end;

function TGPXEmail.AddID: String;
begin;
  Result := FID;
  FIDExsit := True;
end;

procedure TGPXEmail.SetID(const _Value: String);
begin
  FIDExsit := True;
  FID := _Value;
end;

procedure TGPXEmail.IDRemove;
begin
  if FIDExsit then
  begin
    FIDExsit := False;
  end;
end;

procedure TGPXEmail.AddIDEvent(Sender: TObject);
begin
  AddID;
end;

function TGPXEmail.AddDomain: String;
begin;
  Result := FDomain;
  FDomainExsit := True;
end;

procedure TGPXEmail.SetDomain(const _Value: String);
begin
  FDomainExsit := True;
  FDomain := _Value;
end;

procedure TGPXEmail.DomainRemove;
begin
  if FDomainExsit then
  begin
    FDomainExsit := False;
  end;
end;

procedure TGPXEmail.AddDomainEvent(Sender: TObject);
begin
  AddDomain;
end;

{  Person}
constructor TGPXPerson.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXPerson.Destroy;
begin
  if FEmailExsit then
    FEmail.Free;
  if FLinkExsit then
    FLink.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXPerson.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'email' then
      begin
        FEmail := TGPXEmail.Create(Self);
        FEmail.FromXML(nodeTmp);
        FEmailExsit := True;
      end
      else if nodeTmp.NodeName = 'link' then
      begin
        FLink := TGPXLink.Create(Self);
        FLink.FromXML(nodeTmp);
        FLinkExsit := True;
      end
      else if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Person Read XML Error!' + node.Xml);
  end;
end;

function TGPXPerson.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'person';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FEmailExsit then
      FEmail.ToXML(node, 'email');
    if FLinkExsit then
      FLink.ToXML(node, 'link');
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXPerson.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if EmailExsit then
  begin
    Email.TreeNodeShape := TreeNodeShape.AddChildObject('Email', Email);
    Email.ToTree;
  end;
  if LinkExsit then
  begin
    Link.TreeNodeShape := TreeNodeShape.AddChildObject('Link', Link);
    Link.ToTree;
  end;
  if NameExsit then
    TreeNodeShape.AddChild('Name');
end;

procedure TGPXPerson.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  EmailAddMenu: TMenuItem;
  LinkAddMenu: TMenuItem;
  NameAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXPersonPop) and Assigned(TGPXPersonTreeComponent) then
    begin
      TGPXPersonPop.Clear;
      EmailAddMenu := TMenuItem.Create(TGPXPersonPop);
      EmailAddMenu.Text := 'Add Email';
      EmailAddMenu.OnClick := AddEmailEvent;
      TGPXPersonPop.AddObject(EmailAddMenu);
      LinkAddMenu := TMenuItem.Create(TGPXPersonPop);
      LinkAddMenu.Text := 'Add Link';
      LinkAddMenu.OnClick := AddLinkEvent;
      TGPXPersonPop.AddObject(LinkAddMenu);
      NameAddMenu := TMenuItem.Create(TGPXPersonPop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TGPXPersonPop.AddObject(NameAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXPersonTreeComponent.ClientToScreen(pt);
      TGPXPersonPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXPerson.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXPersonXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  TGPXPersonXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXPerson.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
  end;
  ToTree;
end;

function TGPXPerson.AddEmail: TGPXEmail;
begin;
  if not FEmailExsit then
    FEmail := TGPXEmail.Create(Self);
  Result := FEmail;
  FEmailExsit := True;
end;

procedure TGPXPerson.SetEmail(const _Value: TGPXEmail);
begin
  if FEmailExsit then
    FEmail.Free;
  FEmailExsit := True;
  FEmail := _Value;
  FEmail.Parent := Self;
end;

procedure TGPXPerson.EmailRemove;
begin
  if FEmailExsit then
  begin
    FEmail.Free;
    FEmailExsit := False;
  end;
end;

procedure TGPXPerson.AddEmailEvent(Sender: TObject);
begin
  AddEmail;
  FEmail.ToTree;
end;

function TGPXPerson.AddLink: TGPXLink;
begin;
  if not FLinkExsit then
    FLink := TGPXLink.Create(Self);
  Result := FLink;
  FLinkExsit := True;
end;

procedure TGPXPerson.SetLink(const _Value: TGPXLink);
begin
  if FLinkExsit then
    FLink.Free;
  FLinkExsit := True;
  FLink := _Value;
  FLink.Parent := Self;
end;

procedure TGPXPerson.LinkRemove;
begin
  if FLinkExsit then
  begin
    FLink.Free;
    FLinkExsit := False;
  end;
end;

procedure TGPXPerson.AddLinkEvent(Sender: TObject);
begin
  AddLink;
  FLink.ToTree;
end;

function TGPXPerson.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TGPXPerson.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TGPXPerson.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TGPXPerson.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

{  Point}
constructor TGPXPoint.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXPoint.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXPoint.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'ele' then
      begin
        FElevation := nodeTmp.Text;
        FElevationExsit := True;
      end
      else if nodeTmp.NodeName = 'time' then
      begin
        FTime := StrToDateTimeDef(nodeTmp.Text, Now());
        FTimeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'lat' then
      begin
        FLatitude := nodeTmp.Text;
        FLatitudeExsit := True;
      end
      else if nodeTmp.NodeName = 'lon' then
      begin
        FLongitude := nodeTmp.Text;
        FLongitudeExsit := True;
      end;
    end;
  except
    raise Exception.Create('Point Read XML Error!' + node.Xml);
  end;
end;

function TGPXPoint.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  LatitudeTmp: IXMLNode;
  LongitudeTmp: IXMLNode;
  ElevationTmp: IXMLNode;
  TimeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'pt';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FElevationExsit then
    begin
      ElevationTmp := doc.CreateNode('ele', ntElement);
      ElevationTmp.NodeValue := FElevation;
      node.ChildNodes.Add(ElevationTmp);
    end;
    if FTimeExsit then
    begin
      TimeTmp := doc.CreateNode('time', ntElement);
      TimeTmp.NodeValue := FormatDateTime(XMLDTFormat, FTime);
      node.ChildNodes.Add(TimeTmp);
    end;
    if FLatitudeExsit then 
    begin
      LatitudeTmp := doc.CreateNode('lat', ntAttribute);
      LatitudeTmp.NodeValue := FLatitude;
      node.AttributeNodes.Add(LatitudeTmp);
    end;
    if FLongitudeExsit then 
    begin
      LongitudeTmp := doc.CreateNode('lon', ntAttribute);
      LongitudeTmp.NodeValue := FLongitude;
      node.AttributeNodes.Add(LongitudeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXPoint.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if LatitudeExsit then
    TreeNodeShape.AddChild('Latitude');
  if LongitudeExsit then
    TreeNodeShape.AddChild('Longitude');
  if ElevationExsit then
    TreeNodeShape.AddChild('Elevation');
  if TimeExsit then
    TreeNodeShape.AddChild('Time');
end;

procedure TGPXPoint.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  LatitudeAddMenu: TMenuItem;
  LongitudeAddMenu: TMenuItem;
  ElevationAddMenu: TMenuItem;
  TimeAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXPointPop) and Assigned(TGPXPointTreeComponent) then
    begin
      TGPXPointPop.Clear;
      LatitudeAddMenu := TMenuItem.Create(TGPXPointPop);
      LatitudeAddMenu.Text := 'Add Latitude';
      LatitudeAddMenu.OnClick := AddLatitudeEvent;
      TGPXPointPop.AddObject(LatitudeAddMenu);
      LongitudeAddMenu := TMenuItem.Create(TGPXPointPop);
      LongitudeAddMenu.Text := 'Add Longitude';
      LongitudeAddMenu.OnClick := AddLongitudeEvent;
      TGPXPointPop.AddObject(LongitudeAddMenu);
      ElevationAddMenu := TMenuItem.Create(TGPXPointPop);
      ElevationAddMenu.Text := 'Add Elevation';
      ElevationAddMenu.OnClick := AddElevationEvent;
      TGPXPointPop.AddObject(ElevationAddMenu);
      TimeAddMenu := TMenuItem.Create(TGPXPointPop);
      TimeAddMenu.Text := 'Add Time';
      TimeAddMenu.OnClick := AddTimeEvent;
      TGPXPointPop.AddObject(TimeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXPointTreeComponent.ClientToScreen(pt);
      TGPXPointPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXPoint.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXPointXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Latitude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Latitude);
  Names_Value.Add('Longitude');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Longitude);
  Names_Value.Add('Elevation');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Elevation);
  Names_Value.Add('Time');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, Time));
  TGPXPointXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXPoint.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Latitude := _Value;
      end;
    1:
      begin
        Longitude := _Value;
      end;
    2:
      begin
        Elevation := _Value;
      end;
    3:
      begin
        Time := StrToDateTimeDef(_Value, Now());
      end;
  end;
  ToTree;
end;

function TGPXPoint.AddLatitude: String;
begin;
  Result := FLatitude;
  FLatitudeExsit := True;
end;

procedure TGPXPoint.SetLatitude(const _Value: String);
begin
  FLatitudeExsit := True;
  FLatitude := _Value;
end;

procedure TGPXPoint.LatitudeRemove;
begin
  if FLatitudeExsit then
  begin
    FLatitudeExsit := False;
  end;
end;

procedure TGPXPoint.AddLatitudeEvent(Sender: TObject);
begin
  AddLatitude;
end;

function TGPXPoint.AddLongitude: String;
begin;
  Result := FLongitude;
  FLongitudeExsit := True;
end;

procedure TGPXPoint.SetLongitude(const _Value: String);
begin
  FLongitudeExsit := True;
  FLongitude := _Value;
end;

procedure TGPXPoint.LongitudeRemove;
begin
  if FLongitudeExsit then
  begin
    FLongitudeExsit := False;
  end;
end;

procedure TGPXPoint.AddLongitudeEvent(Sender: TObject);
begin
  AddLongitude;
end;

function TGPXPoint.AddElevation: String;
begin;
  Result := FElevation;
  FElevationExsit := True;
end;

procedure TGPXPoint.SetElevation(const _Value: String);
begin
  FElevationExsit := True;
  FElevation := _Value;
end;

procedure TGPXPoint.ElevationRemove;
begin
  if FElevationExsit then
  begin
    FElevationExsit := False;
  end;
end;

procedure TGPXPoint.AddElevationEvent(Sender: TObject);
begin
  AddElevation;
end;

function TGPXPoint.AddTime: TDateTime;
begin;
  Result := FTime;
  FTimeExsit := True;
end;

procedure TGPXPoint.SetTime(const _Value: TDateTime);
begin
  FTimeExsit := True;
  FTime := _Value;
end;

procedure TGPXPoint.TimeRemove;
begin
  if FTimeExsit then
  begin
    FTimeExsit := False;
  end;
end;

procedure TGPXPoint.AddTimeEvent(Sender: TObject);
begin
  AddTime;
end;

{  Bounds}
constructor TGPXBounds.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGPXBounds.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGPXBounds.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'maxlat' then
      begin
        Fmaxlat := nodeTmp.Text.ToDouble;
        FmaxlatExsit := True;
      end
      else if nodeTmp.NodeName = 'maxlon' then
      begin
        Fmaxlon := nodeTmp.Text.ToDouble;
        FmaxlonExsit := True;
      end
      else if nodeTmp.NodeName = 'minlat' then
      begin
        Fminlat := nodeTmp.Text.ToDouble;
        FminlatExsit := True;
      end
      else if nodeTmp.NodeName = 'minlon' then
      begin
        Fminlon := nodeTmp.Text.ToDouble;
        FminlonExsit := True;
      end;
    end;
  except
    raise Exception.Create('Bounds Read XML Error!' + node.Xml);
  end;
end;

function TGPXBounds.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  maxlatTmp: IXMLNode;
  maxlonTmp: IXMLNode;
  minlatTmp: IXMLNode;
  minlonTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'bounds';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FmaxlatExsit then 
    begin
      maxlatTmp := doc.CreateNode('maxlat', ntAttribute);
      maxlatTmp.NodeValue := Fmaxlat.ToString;
      node.AttributeNodes.Add(maxlatTmp);
    end;
    if FmaxlonExsit then 
    begin
      maxlonTmp := doc.CreateNode('maxlon', ntAttribute);
      maxlonTmp.NodeValue := Fmaxlon.ToString;
      node.AttributeNodes.Add(maxlonTmp);
    end;
    if FminlatExsit then 
    begin
      minlatTmp := doc.CreateNode('minlat', ntAttribute);
      minlatTmp.NodeValue := Fminlat.ToString;
      node.AttributeNodes.Add(minlatTmp);
    end;
    if FminlonExsit then 
    begin
      minlonTmp := doc.CreateNode('minlon', ntAttribute);
      minlonTmp.NodeValue := Fminlon.ToString;
      node.AttributeNodes.Add(minlonTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGPXBounds.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if maxlatExsit then
    TreeNodeShape.AddChild('maxlat');
  if maxlonExsit then
    TreeNodeShape.AddChild('maxlon');
  if minlatExsit then
    TreeNodeShape.AddChild('minlat');
  if minlonExsit then
    TreeNodeShape.AddChild('minlon');
end;

procedure TGPXBounds.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  maxlatAddMenu: TMenuItem;
  maxlonAddMenu: TMenuItem;
  minlatAddMenu: TMenuItem;
  minlonAddMenu: TMenuItem;
begin
  ToInspector;
  GPXObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGPXBoundsPop) and Assigned(TGPXBoundsTreeComponent) then
    begin
      TGPXBoundsPop.Clear;
      maxlatAddMenu := TMenuItem.Create(TGPXBoundsPop);
      maxlatAddMenu.Text := 'Add maxlat';
      maxlatAddMenu.OnClick := AddmaxlatEvent;
      TGPXBoundsPop.AddObject(maxlatAddMenu);
      maxlonAddMenu := TMenuItem.Create(TGPXBoundsPop);
      maxlonAddMenu.Text := 'Add maxlon';
      maxlonAddMenu.OnClick := AddmaxlonEvent;
      TGPXBoundsPop.AddObject(maxlonAddMenu);
      minlatAddMenu := TMenuItem.Create(TGPXBoundsPop);
      minlatAddMenu.Text := 'Add minlat';
      minlatAddMenu.OnClick := AddminlatEvent;
      TGPXBoundsPop.AddObject(minlatAddMenu);
      minlonAddMenu := TMenuItem.Create(TGPXBoundsPop);
      minlonAddMenu.Text := 'Add minlon';
      minlonAddMenu.OnClick := AddminlonEvent;
      TGPXBoundsPop.AddObject(minlonAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGPXBoundsTreeComponent.ClientToScreen(pt);
      TGPXBoundsPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGPXBounds.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGPXBoundsXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('maxlat');
  Types_Value.Add(xs_double);
  _Values_Value.Add(maxlat.ToString);
  Names_Value.Add('maxlon');
  Types_Value.Add(xs_double);
  _Values_Value.Add(maxlon.ToString);
  Names_Value.Add('minlat');
  Types_Value.Add(xs_double);
  _Values_Value.Add(minlat.ToString);
  Names_Value.Add('minlon');
  Types_Value.Add(xs_double);
  _Values_Value.Add(minlon.ToString);
  TGPXBoundsXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGPXBounds.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        maxlat := _Value.ToDouble;
      end;
    1:
      begin
        maxlon := _Value.ToDouble;
      end;
    2:
      begin
        minlat := _Value.ToDouble;
      end;
    3:
      begin
        minlon := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TGPXBounds.Addmaxlat: Double;
begin;
  Result := Fmaxlat;
  FmaxlatExsit := True;
end;

procedure TGPXBounds.Setmaxlat(const _Value: Double);
begin
  FmaxlatExsit := True;
  Fmaxlat := _Value;
end;

procedure TGPXBounds.maxlatRemove;
begin
  if FmaxlatExsit then
  begin
    FmaxlatExsit := False;
  end;
end;

procedure TGPXBounds.AddmaxlatEvent(Sender: TObject);
begin
  Addmaxlat;
end;

function TGPXBounds.Addmaxlon: Double;
begin;
  Result := Fmaxlon;
  FmaxlonExsit := True;
end;

procedure TGPXBounds.Setmaxlon(const _Value: Double);
begin
  FmaxlonExsit := True;
  Fmaxlon := _Value;
end;

procedure TGPXBounds.maxlonRemove;
begin
  if FmaxlonExsit then
  begin
    FmaxlonExsit := False;
  end;
end;

procedure TGPXBounds.AddmaxlonEvent(Sender: TObject);
begin
  Addmaxlon;
end;

function TGPXBounds.Addminlat: Double;
begin;
  Result := Fminlat;
  FminlatExsit := True;
end;

procedure TGPXBounds.Setminlat(const _Value: Double);
begin
  FminlatExsit := True;
  Fminlat := _Value;
end;

procedure TGPXBounds.minlatRemove;
begin
  if FminlatExsit then
  begin
    FminlatExsit := False;
  end;
end;

procedure TGPXBounds.AddminlatEvent(Sender: TObject);
begin
  Addminlat;
end;

function TGPXBounds.Addminlon: Double;
begin;
  Result := Fminlon;
  FminlonExsit := True;
end;

procedure TGPXBounds.Setminlon(const _Value: Double);
begin
  FminlonExsit := True;
  Fminlon := _Value;
end;

procedure TGPXBounds.minlonRemove;
begin
  if FminlonExsit then
  begin
    FminlonExsit := False;
  end;
end;

procedure TGPXBounds.AddminlonEvent(Sender: TObject);
begin
  Addminlon;
end;



end.
