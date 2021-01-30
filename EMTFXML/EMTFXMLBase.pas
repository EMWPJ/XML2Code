unit EMTFXMLBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,
  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,
  ClientScreen, XMLInspector;


type

  TEMTF= class;
  TEMTFSite= class;
  TEMTFLocation= class;
  TEMTFProcessingInfo= class;
  TEMTFProcessingSoftware= class;
  TEMTFInstrument= class;
  TEMTFDipole= class;
  TEMTFDataTypes= class;
  TEMTFDateType= class;
  TEMTFChannels= class;
  TEMTFChannel= class;
  TEMTFData= class;
  TEMTFValue= class;
  TEMTFProvenance= class;
  TEMTFPerson= class;
  TEMTFDataQualityNotes= class;
  TEMTFUnitValue= class;
  TEMTFElectrode= class;
  TEMTFPeriodData= class;
  TEMTFTFData= class;
  TEMTFCOVData= class;
  TEMTFZ= class;
  TEMTFDeclination= class;
  TEMTFRemoteRef= class;
  TEMTFComments= class;
  TEMTFFile= class;
  TEMTFCopyright= class;
  TEMTFCitation= class;
  TEMTFStatisticalEstimates= class;
  TEMTFEstimate= class;
  TEMTFGridOrigin= class;
  TEMTFSiteCoords= class;
  TEMTFSiteLayout= class;
  TEMTFPeriodRange= class;

  TEMTF= class(TXML)
  private
    FDescription: String;
    FDescriptionExsit: Boolean;
    FProductId: String;
    FProductIdExsit: Boolean;
    FSubType: String;
    FSubTypeExsit: Boolean;
    FNotess: TList<String>;
    FTagss: TList<String>;
    FProvenance: TEMTFProvenance;
    FSite: TEMTFSite;
    FProcessingInfo: TEMTFProcessingInfo;
    FData: TEMTFData;
    FPrimaryData: TEMTFFile;
    FPrimaryDataExsit: Boolean;
    FAttachment: TEMTFFile;
    FAttachmentExsit: Boolean;
    FCopyright: TEMTFCopyright;
    FCopyrightExsit: Boolean;
    FStatisticalEstimates: TEMTFStatisticalEstimates;
    FStatisticalEstimatesExsit: Boolean;
    FDataTypes: TEMTFDataTypes;
    FDataTypesExsit: Boolean;
    FGridOrigin: TEMTFGridOrigin;
    FGridOriginExsit: Boolean;
    FSiteLayout: TEMTFSiteLayout;
    FSiteLayoutExsit: Boolean;
    FPeriodRange: TEMTFPeriodRange;
    FPeriodRangeExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetDescription(const _Value: String);
    procedure SetProductId(const _Value: String);
    procedure SetSubType(const _Value: String);
    procedure SetNotess(const _Value: TList<String>);
    function GetNotes(Index: Integer): String;
    procedure SetNotes(Index: Integer; const _Value: String);
    procedure SetTagss(const _Value: TList<String>);
    function GetTags(Index: Integer): String;
    procedure SetTags(Index: Integer; const _Value: String);
    procedure SetProvenance(const _Value: TEMTFProvenance);
    procedure SetSite(const _Value: TEMTFSite);
    procedure SetProcessingInfo(const _Value: TEMTFProcessingInfo);
    procedure SetData(const _Value: TEMTFData);
    procedure SetPrimaryData(const _Value: TEMTFFile);
    procedure SetAttachment(const _Value: TEMTFFile);
    procedure SetCopyright(const _Value: TEMTFCopyright);
    procedure SetStatisticalEstimates(const _Value: TEMTFStatisticalEstimates);
    procedure SetDataTypes(const _Value: TEMTFDataTypes);
    procedure SetGridOrigin(const _Value: TEMTFGridOrigin);
    procedure SetSiteLayout(const _Value: TEMTFSiteLayout);
    procedure SetPeriodRange(const _Value: TEMTFPeriodRange);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddProductIdEvent(Sender: TObject);
    procedure AddSubTypeEvent(Sender: TObject);
    procedure AddNotesEvent(Sender: TObject);
    procedure AddTagsEvent(Sender: TObject);
    procedure AddPrimaryDataEvent(Sender: TObject);
    procedure AddAttachmentEvent(Sender: TObject);
    procedure AddCopyrightEvent(Sender: TObject);
    procedure AddStatisticalEstimatesEvent(Sender: TObject);
    procedure AddDataTypesEvent(Sender: TObject);
    procedure AddGridOriginEvent(Sender: TObject);
    procedure AddSiteLayoutEvent(Sender: TObject);
    procedure AddPeriodRangeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddProductId: String;
    procedure ProductIdRemove;
    function AddSubType: String;
    procedure SubTypeRemove;
    function AddNotes: String;
    procedure NotesClear;
    function NotesCount: Integer;
    procedure RemoveNotes(_Value: String);
    procedure DeleteNotes(Index: Integer);
    function AddTags: String;
    procedure TagsClear;
    function TagsCount: Integer;
    procedure RemoveTags(_Value: String);
    procedure DeleteTags(Index: Integer);
    function AddPrimaryData: TEMTFFile;
    procedure PrimaryDataRemove;
    function AddAttachment: TEMTFFile;
    procedure AttachmentRemove;
    function AddCopyright: TEMTFCopyright;
    procedure CopyrightRemove;
    function AddStatisticalEstimates: TEMTFStatisticalEstimates;
    procedure StatisticalEstimatesRemove;
    function AddDataTypes: TEMTFDataTypes;
    procedure DataTypesRemove;
    function AddGridOrigin: TEMTFGridOrigin;
    procedure GridOriginRemove;
    function AddSiteLayout: TEMTFSiteLayout;
    procedure SiteLayoutRemove;
    function AddPeriodRange: TEMTFPeriodRange;
    procedure PeriodRangeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Description: String read FDescription write SetDescription;
    property ProductId: String read FProductId write SetProductId;
    property SubType: String read FSubType write SetSubType;
    property Notess: TList<String> read FNotess write SetNotess;
    property Notes[Index: Integer]: String read GetNotes write SetNotes;
    property Tagss: TList<String> read FTagss write SetTagss;
    property Tags[Index: Integer]: String read GetTags write SetTags;
    property Provenance: TEMTFProvenance read FProvenance write SetProvenance;
    property Site: TEMTFSite read FSite write SetSite;
    property ProcessingInfo: TEMTFProcessingInfo read FProcessingInfo write SetProcessingInfo;
    property Data: TEMTFData read FData write SetData;
    property PrimaryData: TEMTFFile read FPrimaryData write SetPrimaryData;
    property Attachment: TEMTFFile read FAttachment write SetAttachment;
    property Copyright: TEMTFCopyright read FCopyright write SetCopyright;
    property StatisticalEstimates: TEMTFStatisticalEstimates read FStatisticalEstimates write SetStatisticalEstimates;
    property DataTypes: TEMTFDataTypes read FDataTypes write SetDataTypes;
    property GridOrigin: TEMTFGridOrigin read FGridOrigin write SetGridOrigin;
    property SiteLayout: TEMTFSiteLayout read FSiteLayout write SetSiteLayout;
    property PeriodRange: TEMTFPeriodRange read FPeriodRange write SetPeriodRange;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property ProductIdExsit: Boolean read FProductIdExsit;
    property SubTypeExsit: Boolean read FSubTypeExsit;
    property PrimaryDataExsit: Boolean read FPrimaryDataExsit;
    property AttachmentExsit: Boolean read FAttachmentExsit;
    property CopyrightExsit: Boolean read FCopyrightExsit;
    property StatisticalEstimatesExsit: Boolean read FStatisticalEstimatesExsit;
    property DataTypesExsit: Boolean read FDataTypesExsit;
    property GridOriginExsit: Boolean read FGridOriginExsit;
    property SiteLayoutExsit: Boolean read FSiteLayoutExsit;
    property PeriodRangeExsit: Boolean read FPeriodRangeExsit;
  end;

  TEMTFSite= class(TXML)
  private
    FNumber: Integer;
    FNumberExsit: Boolean;
    FProject: String;
    FProjectExsit: Boolean;
    FSurvey: String;
    FSurveyExsit: Boolean;
    FAcquiredBy: String;
    FAcquiredByExsit: Boolean;
    FYearCollected: Integer;
    FYearCollectedExsit: Boolean;
    FId: String;
    FIdExsit: Boolean;
    FName: String;
    FNameExsit: Boolean;
    FReleaseStatus: String;
    FReleaseStatusExsit: Boolean;
    FStartTime: TDateTime;
    FStartTimeExsit: Boolean;
    FEndTime: TDateTime;
    FEndTimeExsit: Boolean;
    FRunList: String;
    FRunListExsit: Boolean;
    FLocation: TEMTFLocation;
    FLocationExsit: Boolean;
    FDataQualityNotes: TEMTFDataQualityNotes;
    FDataQualityNotesExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetNumber(const _Value: Integer);
    procedure SetProject(const _Value: String);
    procedure SetSurvey(const _Value: String);
    procedure SetAcquiredBy(const _Value: String);
    procedure SetYearCollected(const _Value: Integer);
    procedure SetId(const _Value: String);
    procedure SetName(const _Value: String);
    procedure SetReleaseStatus(const _Value: String);
    procedure SetStartTime(const _Value: TDateTime);
    procedure SetEndTime(const _Value: TDateTime);
    procedure SetRunList(const _Value: String);
    procedure SetLocation(const _Value: TEMTFLocation);
    procedure SetDataQualityNotes(const _Value: TEMTFDataQualityNotes);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNumberEvent(Sender: TObject);
    procedure AddProjectEvent(Sender: TObject);
    procedure AddSurveyEvent(Sender: TObject);
    procedure AddAcquiredByEvent(Sender: TObject);
    procedure AddYearCollectedEvent(Sender: TObject);
    procedure AddIdEvent(Sender: TObject);
    procedure AddNameEvent(Sender: TObject);
    procedure AddReleaseStatusEvent(Sender: TObject);
    procedure AddStartTimeEvent(Sender: TObject);
    procedure AddEndTimeEvent(Sender: TObject);
    procedure AddRunListEvent(Sender: TObject);
    procedure AddLocationEvent(Sender: TObject);
    procedure AddDataQualityNotesEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddNumber: Integer;
    procedure NumberRemove;
    function AddProject: String;
    procedure ProjectRemove;
    function AddSurvey: String;
    procedure SurveyRemove;
    function AddAcquiredBy: String;
    procedure AcquiredByRemove;
    function AddYearCollected: Integer;
    procedure YearCollectedRemove;
    function AddId: String;
    procedure IdRemove;
    function AddName: String;
    procedure NameRemove;
    function AddReleaseStatus: String;
    procedure ReleaseStatusRemove;
    function AddStartTime: TDateTime;
    procedure StartTimeRemove;
    function AddEndTime: TDateTime;
    procedure EndTimeRemove;
    function AddRunList: String;
    procedure RunListRemove;
    function AddLocation: TEMTFLocation;
    procedure LocationRemove;
    function AddDataQualityNotes: TEMTFDataQualityNotes;
    procedure DataQualityNotesRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Number: Integer read FNumber write SetNumber;
    property Project: String read FProject write SetProject;
    property Survey: String read FSurvey write SetSurvey;
    property AcquiredBy: String read FAcquiredBy write SetAcquiredBy;
    property YearCollected: Integer read FYearCollected write SetYearCollected;
    property Id: String read FId write SetId;
    property Name: String read FName write SetName;
    property ReleaseStatus: String read FReleaseStatus write SetReleaseStatus;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property EndTime: TDateTime read FEndTime write SetEndTime;
    property RunList: String read FRunList write SetRunList;
    property Location: TEMTFLocation read FLocation write SetLocation;
    property DataQualityNotes: TEMTFDataQualityNotes read FDataQualityNotes write SetDataQualityNotes;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NumberExsit: Boolean read FNumberExsit;
    property ProjectExsit: Boolean read FProjectExsit;
    property SurveyExsit: Boolean read FSurveyExsit;
    property AcquiredByExsit: Boolean read FAcquiredByExsit;
    property YearCollectedExsit: Boolean read FYearCollectedExsit;
    property IdExsit: Boolean read FIdExsit;
    property NameExsit: Boolean read FNameExsit;
    property ReleaseStatusExsit: Boolean read FReleaseStatusExsit;
    property StartTimeExsit: Boolean read FStartTimeExsit;
    property EndTimeExsit: Boolean read FEndTimeExsit;
    property RunListExsit: Boolean read FRunListExsit;
    property LocationExsit: Boolean read FLocationExsit;
    property DataQualityNotesExsit: Boolean read FDataQualityNotesExsit;
  end;

  TEMTFLocation= class(TXML)
  private
    FLatitude: TEMTFUnitValue;
    FLongitude: TEMTFUnitValue;
    FElevation: TEMTFUnitValue;
    FDeclination: TEMTFDeclination;
    Fdatum: String;
    FdatumExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLatitude(const _Value: TEMTFUnitValue);
    procedure SetLongitude(const _Value: TEMTFUnitValue);
    procedure SetElevation(const _Value: TEMTFUnitValue);
    procedure SetDeclination(const _Value: TEMTFDeclination);
    procedure Setdatum(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AdddatumEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Adddatum: String;
    procedure datumRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Latitude: TEMTFUnitValue read FLatitude write SetLatitude;
    property Longitude: TEMTFUnitValue read FLongitude write SetLongitude;
    property Elevation: TEMTFUnitValue read FElevation write SetElevation;
    property Declination: TEMTFDeclination read FDeclination write SetDeclination;
    property datum: String read Fdatum write Setdatum;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property datumExsit: Boolean read FdatumExsit;
  end;

  TEMTFProcessingInfo= class(TXML)
  private
    FProcessingSoftware: TEMTFProcessingSoftware;
    FProcessingSoftwareExsit: Boolean;
    FSignConvention: String;
    FSignConventionExsit: Boolean;
    FProcessedBy: String;
    FProcessedByExsit: Boolean;
    FProcessingTag: String;
    FProcessingTagExsit: Boolean;
    FRemoteRef: TEMTFRemoteRef;
    FRemoteRefExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetProcessingSoftware(const _Value: TEMTFProcessingSoftware);
    procedure SetSignConvention(const _Value: String);
    procedure SetProcessedBy(const _Value: String);
    procedure SetProcessingTag(const _Value: String);
    procedure SetRemoteRef(const _Value: TEMTFRemoteRef);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddProcessingSoftwareEvent(Sender: TObject);
    procedure AddSignConventionEvent(Sender: TObject);
    procedure AddProcessedByEvent(Sender: TObject);
    procedure AddProcessingTagEvent(Sender: TObject);
    procedure AddRemoteRefEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddProcessingSoftware: TEMTFProcessingSoftware;
    procedure ProcessingSoftwareRemove;
    function AddSignConvention: String;
    procedure SignConventionRemove;
    function AddProcessedBy: String;
    procedure ProcessedByRemove;
    function AddProcessingTag: String;
    procedure ProcessingTagRemove;
    function AddRemoteRef: TEMTFRemoteRef;
    procedure RemoteRefRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property ProcessingSoftware: TEMTFProcessingSoftware read FProcessingSoftware write SetProcessingSoftware;
    property SignConvention: String read FSignConvention write SetSignConvention;
    property ProcessedBy: String read FProcessedBy write SetProcessedBy;
    property ProcessingTag: String read FProcessingTag write SetProcessingTag;
    property RemoteRef: TEMTFRemoteRef read FRemoteRef write SetRemoteRef;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ProcessingSoftwareExsit: Boolean read FProcessingSoftwareExsit;
    property SignConventionExsit: Boolean read FSignConventionExsit;
    property ProcessedByExsit: Boolean read FProcessedByExsit;
    property ProcessingTagExsit: Boolean read FProcessingTagExsit;
    property RemoteRefExsit: Boolean read FRemoteRefExsit;
  end;

  TEMTFProcessingSoftware= class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FLastMod: String;
    FLastModExsit: Boolean;
    FAuthor: String;
    FAuthorExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetLastMod(const _Value: String);
    procedure SetAuthor(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddLastModEvent(Sender: TObject);
    procedure AddAuthorEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddLastMod: String;
    procedure LastModRemove;
    function AddAuthor: String;
    procedure AuthorRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property LastMod: String read FLastMod write SetLastMod;
    property Author: String read FAuthor write SetAuthor;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property LastModExsit: Boolean read FLastModExsit;
    property AuthorExsit: Boolean read FAuthorExsit;
  end;

  TEMTFInstrument= class(TXML)
  private
    FManufacturer: String;
    FManufacturerExsit: Boolean;
    FName: String;
    FNameExsit: Boolean;
    FId: String;
    FIdExsit: Boolean;
    FSettings: String;
    FSettingsExsit: Boolean;
    FMagentometertype: String;
    FMagentometertypeExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetManufacturer(const _Value: String);
    procedure SetName(const _Value: String);
    procedure SetId(const _Value: String);
    procedure SetSettings(const _Value: String);
    procedure SetMagentometertype(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddManufacturerEvent(Sender: TObject);
    procedure AddNameEvent(Sender: TObject);
    procedure AddIdEvent(Sender: TObject);
    procedure AddSettingsEvent(Sender: TObject);
    procedure AddMagentometertypeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddManufacturer: String;
    procedure ManufacturerRemove;
    function AddName: String;
    procedure NameRemove;
    function AddId: String;
    procedure IdRemove;
    function AddSettings: String;
    procedure SettingsRemove;
    function AddMagentometertype: String;
    procedure MagentometertypeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Manufacturer: String read FManufacturer write SetManufacturer;
    property Name: String read FName write SetName;
    property Id: String read FId write SetId;
    property Settings: String read FSettings write SetSettings;
    property Magentometertype: String read FMagentometertype write SetMagentometertype;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ManufacturerExsit: Boolean read FManufacturerExsit;
    property NameExsit: Boolean read FNameExsit;
    property IdExsit: Boolean read FIdExsit;
    property SettingsExsit: Boolean read FSettingsExsit;
    property MagentometertypeExsit: Boolean read FMagentometertypeExsit;
  end;

  TEMTFDipole= class(TXML)
  private
    Fname: String;
    FnameExsit: Boolean;
    FDipoletype: String;
    FDipoletypeExsit: Boolean;
    FManufacturer: String;
    FManufacturerExsit: Boolean;
    FLength: TEMTFUnitValue;
    FLengthExsit: Boolean;
    FAzimuth: TEMTFUnitValue;
    FAzimuthExsit: Boolean;
    FElectrodes: TList<TEMTFElectrode>;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure SetDipoletype(const _Value: String);
    procedure SetManufacturer(const _Value: String);
    procedure SetLength(const _Value: TEMTFUnitValue);
    procedure SetAzimuth(const _Value: TEMTFUnitValue);
    procedure SetElectrodes(const _Value: TList<TEMTFElectrode>);
    function GetElectrode(Index: Integer): TEMTFElectrode;
    procedure SetElectrode(Index: Integer; const _Value: TEMTFElectrode);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddnameEvent(Sender: TObject);
    procedure AddDipoletypeEvent(Sender: TObject);
    procedure AddManufacturerEvent(Sender: TObject);
    procedure AddLengthEvent(Sender: TObject);
    procedure AddAzimuthEvent(Sender: TObject);
    procedure AddElectrodeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addname: String;
    procedure nameRemove;
    function AddDipoletype: String;
    procedure DipoletypeRemove;
    function AddManufacturer: String;
    procedure ManufacturerRemove;
    function AddLength: TEMTFUnitValue;
    procedure LengthRemove;
    function AddAzimuth: TEMTFUnitValue;
    procedure AzimuthRemove;
    function AddElectrode: TEMTFElectrode;
    procedure ElectrodeClear;
    function ElectrodeCount: Integer;
    procedure RemoveElectrode(_Value: TEMTFElectrode);
    procedure DeleteElectrode(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property Dipoletype: String read FDipoletype write SetDipoletype;
    property Manufacturer: String read FManufacturer write SetManufacturer;
    property Length: TEMTFUnitValue read FLength write SetLength;
    property Azimuth: TEMTFUnitValue read FAzimuth write SetAzimuth;
    property Electrodes: TList<TEMTFElectrode> read FElectrodes write SetElectrodes;
    property Electrode[Index: Integer]: TEMTFElectrode read GetElectrode write SetElectrode;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property nameExsit: Boolean read FnameExsit;
    property DipoletypeExsit: Boolean read FDipoletypeExsit;
    property ManufacturerExsit: Boolean read FManufacturerExsit;
    property LengthExsit: Boolean read FLengthExsit;
    property AzimuthExsit: Boolean read FAzimuthExsit;
  end;

  TEMTFDataTypes= class(TXML)
  private
    FDataTypes: TList<TEMTFDateType>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetDataTypes(const _Value: TList<TEMTFDateType>);
    function GetDataType(Index: Integer): TEMTFDateType;
    procedure SetDataType(Index: Integer; const _Value: TEMTFDateType);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddDataTypeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddDataType: TEMTFDateType;
    procedure DataTypeClear;
    function DataTypeCount: Integer;
    procedure RemoveDataType(_Value: TEMTFDateType);
    procedure DeleteDataType(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property DataTypes: TList<TEMTFDateType> read FDataTypes write SetDataTypes;
    property DataType[Index: Integer]: TEMTFDateType read GetDataType write SetDataType;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFDateType= class(TXML)
  private
    Fname: String;
    FnameExsit: Boolean;
    Fztype: String;
    FztypeExsit: Boolean;
    Foutput: String;
    FoutputExsit: Boolean;
    Finput: String;
    FinputExsit: Boolean;
    Funits: String;
    FunitsExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FExternalUrl: String;
    FExternalUrlExsit: Boolean;
    FIntention: String;
    FIntentionExsit: Boolean;
    FTag: String;
    FTagExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure Setztype(const _Value: String);
    procedure Setoutput(const _Value: String);
    procedure Setinput(const _Value: String);
    procedure Setunits(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetExternalUrl(const _Value: String);
    procedure SetIntention(const _Value: String);
    procedure SetTag(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddnameEvent(Sender: TObject);
    procedure AddztypeEvent(Sender: TObject);
    procedure AddoutputEvent(Sender: TObject);
    procedure AddinputEvent(Sender: TObject);
    procedure AddunitsEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddExternalUrlEvent(Sender: TObject);
    procedure AddIntentionEvent(Sender: TObject);
    procedure AddTagEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addname: String;
    procedure nameRemove;
    function Addztype: String;
    procedure ztypeRemove;
    function Addoutput: String;
    procedure outputRemove;
    function Addinput: String;
    procedure inputRemove;
    function Addunits: String;
    procedure unitsRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddExternalUrl: String;
    procedure ExternalUrlRemove;
    function AddIntention: String;
    procedure IntentionRemove;
    function AddTag: String;
    procedure TagRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property ztype: String read Fztype write Setztype;
    property output: String read Foutput write Setoutput;
    property input: String read Finput write Setinput;
    property units: String read Funits write Setunits;
    property Description: String read FDescription write SetDescription;
    property ExternalUrl: String read FExternalUrl write SetExternalUrl;
    property Intention: String read FIntention write SetIntention;
    property Tag: String read FTag write SetTag;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property nameExsit: Boolean read FnameExsit;
    property ztypeExsit: Boolean read FztypeExsit;
    property outputExsit: Boolean read FoutputExsit;
    property inputExsit: Boolean read FinputExsit;
    property unitsExsit: Boolean read FunitsExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property ExternalUrlExsit: Boolean read FExternalUrlExsit;
    property IntentionExsit: Boolean read FIntentionExsit;
    property TagExsit: Boolean read FTagExsit;
  end;

  TEMTFChannels= class(TXML)
  private
    Fref: String;
    FrefExsit: Boolean;
    Funits: String;
    FunitsExsit: Boolean;
    FChannels: TList<TEMTFChannel>;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setref(const _Value: String);
    procedure Setunits(const _Value: String);
    procedure SetChannels(const _Value: TList<TEMTFChannel>);
    function GetChannel(Index: Integer): TEMTFChannel;
    procedure SetChannel(Index: Integer; const _Value: TEMTFChannel);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddrefEvent(Sender: TObject);
    procedure AddunitsEvent(Sender: TObject);
    procedure AddChannelEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addref: String;
    procedure refRemove;
    function Addunits: String;
    procedure unitsRemove;
    function AddChannel: TEMTFChannel;
    procedure ChannelClear;
    function ChannelCount: Integer;
    procedure RemoveChannel(_Value: TEMTFChannel);
    procedure DeleteChannel(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property ref: String read Fref write Setref;
    property units: String read Funits write Setunits;
    property Channels: TList<TEMTFChannel> read FChannels write SetChannels;
    property Channel[Index: Integer]: TEMTFChannel read GetChannel write SetChannel;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property refExsit: Boolean read FrefExsit;
    property unitsExsit: Boolean read FunitsExsit;
  end;

  TEMTFChannel= class(TXML)
  private
    Fname: String;
    Forientation: Double;
    Fx: Double;
    FxExsit: Boolean;
    Fy: Double;
    FyExsit: Boolean;
    Fz: Double;
    FzExsit: Boolean;
    Fx2: Double;
    Fx2Exsit: Boolean;
    Fy2: Double;
    Fy2Exsit: Boolean;
    Fz2: Double;
    Fz2Exsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure Setorientation(const _Value: Double);
    procedure Setx(const _Value: Double);
    procedure Sety(const _Value: Double);
    procedure Setz(const _Value: Double);
    procedure Setx2(const _Value: Double);
    procedure Sety2(const _Value: Double);
    procedure Setz2(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddxEvent(Sender: TObject);
    procedure AddyEvent(Sender: TObject);
    procedure AddzEvent(Sender: TObject);
    procedure Addx2Event(Sender: TObject);
    procedure Addy2Event(Sender: TObject);
    procedure Addz2Event(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addx: Double;
    procedure xRemove;
    function Addy: Double;
    procedure yRemove;
    function Addz: Double;
    procedure zRemove;
    function Addx2: Double;
    procedure x2Remove;
    function Addy2: Double;
    procedure y2Remove;
    function Addz2: Double;
    procedure z2Remove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property orientation: Double read Forientation write Setorientation;
    property x: Double read Fx write Setx;
    property y: Double read Fy write Sety;
    property z: Double read Fz write Setz;
    property x2: Double read Fx2 write Setx2;
    property y2: Double read Fy2 write Sety2;
    property z2: Double read Fz2 write Setz2;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property xExsit: Boolean read FxExsit;
    property yExsit: Boolean read FyExsit;
    property zExsit: Boolean read FzExsit;
    property x2Exsit: Boolean read Fx2Exsit;
    property y2Exsit: Boolean read Fy2Exsit;
    property z2Exsit: Boolean read Fz2Exsit;
  end;

  TEMTFData= class(TXML)
  private
    FPeriodDatas: TList<TEMTFPeriodData>;
    Fcount: Integer;
    FcountExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetPeriodDatas(const _Value: TList<TEMTFPeriodData>);
    function GetPeriodData(Index: Integer): TEMTFPeriodData;
    procedure SetPeriodData(Index: Integer; const _Value: TEMTFPeriodData);
    procedure Setcount(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddPeriodDataEvent(Sender: TObject);
    procedure AddcountEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddPeriodData: TEMTFPeriodData;
    procedure PeriodDataClear;
    function PeriodDataCount: Integer;
    procedure RemovePeriodData(_Value: TEMTFPeriodData);
    procedure DeletePeriodData(Index: Integer);
    function Addcount: Integer;
    procedure countRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property PeriodDatas: TList<TEMTFPeriodData> read FPeriodDatas write SetPeriodDatas;
    property PeriodData[Index: Integer]: TEMTFPeriodData read GetPeriodData write SetPeriodData;
    property count: Integer read Fcount write Setcount;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property countExsit: Boolean read FcountExsit;
  end;

  TEMTFValue= class(TXML)
  private
    Fname: String;
    FnameExsit: Boolean;
    Foutput: String;
    FoutputExsit: Boolean;
    Finput: String;
    FinputExsit: Boolean;
    Fvalue: String;
    Funits: String;
    FunitsExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure Setoutput(const _Value: String);
    procedure Setinput(const _Value: String);
    procedure Setvalue(const _Value: String);
    procedure Setunits(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddnameEvent(Sender: TObject);
    procedure AddoutputEvent(Sender: TObject);
    procedure AddinputEvent(Sender: TObject);
    procedure AddunitsEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addname: String;
    procedure nameRemove;
    function Addoutput: String;
    procedure outputRemove;
    function Addinput: String;
    procedure inputRemove;
    function Addunits: String;
    procedure unitsRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property output: String read Foutput write Setoutput;
    property input: String read Finput write Setinput;
    property value: String read Fvalue write Setvalue;
    property units: String read Funits write Setunits;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property nameExsit: Boolean read FnameExsit;
    property outputExsit: Boolean read FoutputExsit;
    property inputExsit: Boolean read FinputExsit;
    property unitsExsit: Boolean read FunitsExsit;
  end;

  TEMTFProvenance= class(TXML)
  private
    FCreateTime: TDateTime;
    FCreateTimeExsit: Boolean;
    FCreatingApplication: String;
    FCreatingApplicationExsit: Boolean;
    FCreator: TEMTFPerson;
    FCreatorExsit: Boolean;
    FSubmitter: TEMTFPerson;
    FSubmitterExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetCreateTime(const _Value: TDateTime);
    procedure SetCreatingApplication(const _Value: String);
    procedure SetCreator(const _Value: TEMTFPerson);
    procedure SetSubmitter(const _Value: TEMTFPerson);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddCreateTimeEvent(Sender: TObject);
    procedure AddCreatingApplicationEvent(Sender: TObject);
    procedure AddCreatorEvent(Sender: TObject);
    procedure AddSubmitterEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddCreateTime: TDateTime;
    procedure CreateTimeRemove;
    function AddCreatingApplication: String;
    procedure CreatingApplicationRemove;
    function AddCreator: TEMTFPerson;
    procedure CreatorRemove;
    function AddSubmitter: TEMTFPerson;
    procedure SubmitterRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property CreateTime: TDateTime read FCreateTime write SetCreateTime;
    property CreatingApplication: String read FCreatingApplication write SetCreatingApplication;
    property Creator: TEMTFPerson read FCreator write SetCreator;
    property Submitter: TEMTFPerson read FSubmitter write SetSubmitter;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property CreateTimeExsit: Boolean read FCreateTimeExsit;
    property CreatingApplicationExsit: Boolean read FCreatingApplicationExsit;
    property CreatorExsit: Boolean read FCreatorExsit;
    property SubmitterExsit: Boolean read FSubmitterExsit;
  end;

  TEMTFPerson= class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FEmail: String;
    FEmailExsit: Boolean;
    FOrg: String;
    FOrgExsit: Boolean;
    FOrgUrl: String;
    FOrgUrlExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetEmail(const _Value: String);
    procedure SetOrg(const _Value: String);
    procedure SetOrgUrl(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddEmailEvent(Sender: TObject);
    procedure AddOrgEvent(Sender: TObject);
    procedure AddOrgUrlEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddEmail: String;
    procedure EmailRemove;
    function AddOrg: String;
    procedure OrgRemove;
    function AddOrgUrl: String;
    procedure OrgUrlRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Email: String read FEmail write SetEmail;
    property Org: String read FOrg write SetOrg;
    property OrgUrl: String read FOrgUrl write SetOrgUrl;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property EmailExsit: Boolean read FEmailExsit;
    property OrgExsit: Boolean read FOrgExsit;
    property OrgUrlExsit: Boolean read FOrgUrlExsit;
  end;

  TEMTFDataQualityNotes= class(TXML)
  private
    FRating: Double;
    FRatingExsit: Boolean;
    FGoodFromPeriod: Double;
    FGoodFromPeriodExsit: Boolean;
    FGoodToPeriod: Double;
    FGoodToPeriodExsit: Boolean;
    FComments: TEMTFComments;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetRating(const _Value: Double);
    procedure SetGoodFromPeriod(const _Value: Double);
    procedure SetGoodToPeriod(const _Value: Double);
    procedure SetComments(const _Value: TEMTFComments);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddRatingEvent(Sender: TObject);
    procedure AddGoodFromPeriodEvent(Sender: TObject);
    procedure AddGoodToPeriodEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddRating: Double;
    procedure RatingRemove;
    function AddGoodFromPeriod: Double;
    procedure GoodFromPeriodRemove;
    function AddGoodToPeriod: Double;
    procedure GoodToPeriodRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Rating: Double read FRating write SetRating;
    property GoodFromPeriod: Double read FGoodFromPeriod write SetGoodFromPeriod;
    property GoodToPeriod: Double read FGoodToPeriod write SetGoodToPeriod;
    property Comments: TEMTFComments read FComments write SetComments;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property RatingExsit: Boolean read FRatingExsit;
    property GoodFromPeriodExsit: Boolean read FGoodFromPeriodExsit;
    property GoodToPeriodExsit: Boolean read FGoodToPeriodExsit;
  end;

  TEMTFUnitValue= class(TXML)
  private
    Funits: String;
    FunitsExsit: Boolean;
    FValue: Double;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setunits(const _Value: String);
    procedure SetValue(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddunitsEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addunits: String;
    procedure unitsRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property units: String read Funits write Setunits;
    property Value: Double read FValue write SetValue;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property unitsExsit: Boolean read FunitsExsit;
  end;

  TEMTFElectrode= class(TXML)
  private
    Flocation: String;
    FlocationExsit: Boolean;
    FValue: Double;
    Fnumber: String;
    FnumberExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setlocation(const _Value: String);
    procedure SetValue(const _Value: Double);
    procedure Setnumber(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddlocationEvent(Sender: TObject);
    procedure AddnumberEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addlocation: String;
    procedure locationRemove;
    function Addnumber: String;
    procedure numberRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property location: String read Flocation write Setlocation;
    property Value: Double read FValue write SetValue;
    property number: String read Fnumber write Setnumber;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property locationExsit: Boolean read FlocationExsit;
    property numberExsit: Boolean read FnumberExsit;
  end;

  TEMTFPeriodData= class(TXML)
  private
    Fvalue: Double;
    Funits: String;
    FTF: TEMTFTFData;
    FTFExsit: Boolean;
    FTFVAR: TEMTFTFData;
    FTFVARExsit: Boolean;
    FINVSIGCOV: TEMTFCOVData;
    FINVSIGCOVExsit: Boolean;
    FRESIDCOV: TEMTFCOVData;
    FRESIDCOVExsit: Boolean;
    FZ: TEMTFZ;
    FZExsit: Boolean;
    FZVAR: TEMTFZ;
    FZVARExsit: Boolean;
    FZINVSIGCOV: TEMTFZ;
    FZINVSIGCOVExsit: Boolean;
    FZRESIDCOV: TEMTFZ;
    FZRESIDCOVExsit: Boolean;
    FT: TEMTFZ;
    FTExsit: Boolean;
    FTVAR: TEMTFZ;
    FTVARExsit: Boolean;
    FTINVSIGCOV: TEMTFZ;
    FTINVSIGCOVExsit: Boolean;
    FTRESIDCOV: TEMTFZ;
    FTRESIDCOVExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setvalue(const _Value: Double);
    procedure Setunits(const _Value: String);
    procedure SetTF(const _Value: TEMTFTFData);
    procedure SetTFVAR(const _Value: TEMTFTFData);
    procedure SetINVSIGCOV(const _Value: TEMTFCOVData);
    procedure SetRESIDCOV(const _Value: TEMTFCOVData);
    procedure SetZ(const _Value: TEMTFZ);
    procedure SetZVAR(const _Value: TEMTFZ);
    procedure SetZINVSIGCOV(const _Value: TEMTFZ);
    procedure SetZRESIDCOV(const _Value: TEMTFZ);
    procedure SetT(const _Value: TEMTFZ);
    procedure SetTVAR(const _Value: TEMTFZ);
    procedure SetTINVSIGCOV(const _Value: TEMTFZ);
    procedure SetTRESIDCOV(const _Value: TEMTFZ);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddTFEvent(Sender: TObject);
    procedure AddTFVAREvent(Sender: TObject);
    procedure AddINVSIGCOVEvent(Sender: TObject);
    procedure AddRESIDCOVEvent(Sender: TObject);
    procedure AddZEvent(Sender: TObject);
    procedure AddZVAREvent(Sender: TObject);
    procedure AddZINVSIGCOVEvent(Sender: TObject);
    procedure AddZRESIDCOVEvent(Sender: TObject);
    procedure AddTEvent(Sender: TObject);
    procedure AddTVAREvent(Sender: TObject);
    procedure AddTINVSIGCOVEvent(Sender: TObject);
    procedure AddTRESIDCOVEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddTF: TEMTFTFData;
    procedure TFRemove;
    function AddTFVAR: TEMTFTFData;
    procedure TFVARRemove;
    function AddINVSIGCOV: TEMTFCOVData;
    procedure INVSIGCOVRemove;
    function AddRESIDCOV: TEMTFCOVData;
    procedure RESIDCOVRemove;
    function AddZ: TEMTFZ;
    procedure ZRemove;
    function AddZVAR: TEMTFZ;
    procedure ZVARRemove;
    function AddZINVSIGCOV: TEMTFZ;
    procedure ZINVSIGCOVRemove;
    function AddZRESIDCOV: TEMTFZ;
    procedure ZRESIDCOVRemove;
    function AddT: TEMTFZ;
    procedure TRemove;
    function AddTVAR: TEMTFZ;
    procedure TVARRemove;
    function AddTINVSIGCOV: TEMTFZ;
    procedure TINVSIGCOVRemove;
    function AddTRESIDCOV: TEMTFZ;
    procedure TRESIDCOVRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property value: Double read Fvalue write Setvalue;
    property units: String read Funits write Setunits;
    property TF: TEMTFTFData read FTF write SetTF;
    property TFVAR: TEMTFTFData read FTFVAR write SetTFVAR;
    property INVSIGCOV: TEMTFCOVData read FINVSIGCOV write SetINVSIGCOV;
    property RESIDCOV: TEMTFCOVData read FRESIDCOV write SetRESIDCOV;
    property Z: TEMTFZ read FZ write SetZ;
    property ZVAR: TEMTFZ read FZVAR write SetZVAR;
    property ZINVSIGCOV: TEMTFZ read FZINVSIGCOV write SetZINVSIGCOV;
    property ZRESIDCOV: TEMTFZ read FZRESIDCOV write SetZRESIDCOV;
    property T: TEMTFZ read FT write SetT;
    property TVAR: TEMTFZ read FTVAR write SetTVAR;
    property TINVSIGCOV: TEMTFZ read FTINVSIGCOV write SetTINVSIGCOV;
    property TRESIDCOV: TEMTFZ read FTRESIDCOV write SetTRESIDCOV;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property TFExsit: Boolean read FTFExsit;
    property TFVARExsit: Boolean read FTFVARExsit;
    property INVSIGCOVExsit: Boolean read FINVSIGCOVExsit;
    property RESIDCOVExsit: Boolean read FRESIDCOVExsit;
    property ZExsit: Boolean read FZExsit;
    property ZVARExsit: Boolean read FZVARExsit;
    property ZINVSIGCOVExsit: Boolean read FZINVSIGCOVExsit;
    property ZRESIDCOVExsit: Boolean read FZRESIDCOVExsit;
    property TExsit: Boolean read FTExsit;
    property TVARExsit: Boolean read FTVARExsit;
    property TINVSIGCOVExsit: Boolean read FTINVSIGCOVExsit;
    property TRESIDCOVExsit: Boolean read FTRESIDCOVExsit;
  end;

  TEMTFTFData= class(TXML)
  private
    Fname: String;
    FnameExsit: Boolean;
    Fcomment: String;
    FcommentExsit: Boolean;
    Fsize: Integer;
    FsizeExsit: Boolean;
    Fvalues: TList<TEMTFValue>;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure Setcomment(const _Value: String);
    procedure Setsize(const _Value: Integer);
    procedure Setvalues(const _Value: TList<TEMTFValue>);
    function Getvalue(Index: Integer): TEMTFValue;
    procedure Setvalue(Index: Integer; const _Value: TEMTFValue);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddnameEvent(Sender: TObject);
    procedure AddcommentEvent(Sender: TObject);
    procedure AddsizeEvent(Sender: TObject);
    procedure AddvalueEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addname: String;
    procedure nameRemove;
    function Addcomment: String;
    procedure commentRemove;
    function Addsize: Integer;
    procedure sizeRemove;
    function Addvalue: TEMTFValue;
    procedure valueClear;
    function valueCount: Integer;
    procedure Removevalue(_Value: TEMTFValue);
    procedure Deletevalue(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property comment: String read Fcomment write Setcomment;
    property size: Integer read Fsize write Setsize;
    property values: TList<TEMTFValue> read Fvalues write Setvalues;
    property value[Index: Integer]: TEMTFValue read Getvalue write Setvalue;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property nameExsit: Boolean read FnameExsit;
    property commentExsit: Boolean read FcommentExsit;
    property sizeExsit: Boolean read FsizeExsit;
  end;

  TEMTFCOVData= class(TXML)
  private
    Fname: String;
    FnameExsit: Boolean;
    Fcomment: String;
    FcommentExsit: Boolean;
    Fsize: Integer;
    FsizeExsit: Boolean;
    Fvalue: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setname(const _Value: String);
    procedure Setcomment(const _Value: String);
    procedure Setsize(const _Value: Integer);
    procedure Setvalue(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddnameEvent(Sender: TObject);
    procedure AddcommentEvent(Sender: TObject);
    procedure AddsizeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addname: String;
    procedure nameRemove;
    function Addcomment: String;
    procedure commentRemove;
    function Addsize: Integer;
    procedure sizeRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property name: String read Fname write Setname;
    property comment: String read Fcomment write Setcomment;
    property size: Integer read Fsize write Setsize;
    property value: String read Fvalue write Setvalue;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property nameExsit: Boolean read FnameExsit;
    property commentExsit: Boolean read FcommentExsit;
    property sizeExsit: Boolean read FsizeExsit;
  end;

  TEMTFZ= class(TXML)
  private
    FZtype: String;
    FZtypeExsit: Boolean;
    Fsize: String;
    FsizeExsit: Boolean;
    Funits: String;
    FunitsExsit: Boolean;
    Fvalues: TList<TEMTFValue>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetZtype(const _Value: String);
    procedure Setsize(const _Value: String);
    procedure Setunits(const _Value: String);
    procedure Setvalues(const _Value: TList<TEMTFValue>);
    function Getvalue(Index: Integer): TEMTFValue;
    procedure Setvalue(Index: Integer; const _Value: TEMTFValue);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddZtypeEvent(Sender: TObject);
    procedure AddsizeEvent(Sender: TObject);
    procedure AddunitsEvent(Sender: TObject);
    procedure AddvalueEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddZtype: String;
    procedure ZtypeRemove;
    function Addsize: String;
    procedure sizeRemove;
    function Addunits: String;
    procedure unitsRemove;
    function Addvalue: TEMTFValue;
    procedure valueClear;
    function valueCount: Integer;
    procedure Removevalue(_Value: TEMTFValue);
    procedure Deletevalue(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Ztype: String read FZtype write SetZtype;
    property size: String read Fsize write Setsize;
    property units: String read Funits write Setunits;
    property values: TList<TEMTFValue> read Fvalues write Setvalues;
    property value[Index: Integer]: TEMTFValue read Getvalue write Setvalue;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ZtypeExsit: Boolean read FZtypeExsit;
    property sizeExsit: Boolean read FsizeExsit;
    property unitsExsit: Boolean read FunitsExsit;
  end;

  TEMTFDeclination= class(TXML)
  private
    FValue: Double;
    FEpoch: Double;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetValue(const _Value: Double);
    procedure SetEpoch(const _Value: Double);
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
    property Value: Double read FValue write SetValue;
    property Epoch: Double read FEpoch write SetEpoch;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFRemoteRef= class(TXML)
  private
    FValue: String;
    FRTypes: TList<String>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetValue(const _Value: String);
    procedure SetRTypes(const _Value: TList<String>);
    function GetRType(Index: Integer): String;
    procedure SetRType(Index: Integer; const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddRTypeEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddRType: String;
    procedure RTypeClear;
    function RTypeCount: Integer;
    procedure RemoveRType(_Value: String);
    procedure DeleteRType(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Value: String read FValue write SetValue;
    property RTypes: TList<String> read FRTypes write SetRTypes;
    property RType[Index: Integer]: String read GetRType write SetRType;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFComments= class(TXML)
  private
    FValue: String;
    FAuthor: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetValue(const _Value: String);
    procedure SetAuthor(const _Value: String);
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
    property Value: String read FValue write SetValue;
    property Author: String read FAuthor write SetAuthor;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFFile= class(TXML)
  private
    FFilename: String;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetFilename(const _Value: String);
    procedure SetDescription(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddDescriptionEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddDescription: String;
    procedure DescriptionRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Filename: String read FFilename write SetFilename;
    property Description: String read FDescription write SetDescription;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property DescriptionExsit: Boolean read FDescriptionExsit;
  end;

  TEMTFCopyright= class(TXML)
  private
    FCitation: TEMTFCitation;
    FCitationExsit: Boolean;
    FReleaseStatus: String;
    FReleaseStatusExsit: Boolean;
    FConditionsOfUse: String;
    FConditionsOfUseExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetCitation(const _Value: TEMTFCitation);
    procedure SetReleaseStatus(const _Value: String);
    procedure SetConditionsOfUse(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddCitationEvent(Sender: TObject);
    procedure AddReleaseStatusEvent(Sender: TObject);
    procedure AddConditionsOfUseEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddCitation: TEMTFCitation;
    procedure CitationRemove;
    function AddReleaseStatus: String;
    procedure ReleaseStatusRemove;
    function AddConditionsOfUse: String;
    procedure ConditionsOfUseRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Citation: TEMTFCitation read FCitation write SetCitation;
    property ReleaseStatus: String read FReleaseStatus write SetReleaseStatus;
    property ConditionsOfUse: String read FConditionsOfUse write SetConditionsOfUse;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property CitationExsit: Boolean read FCitationExsit;
    property ReleaseStatusExsit: Boolean read FReleaseStatusExsit;
    property ConditionsOfUseExsit: Boolean read FConditionsOfUseExsit;
  end;

  TEMTFCitation= class(TXML)
  private
    FTitle: String;
    FTitleExsit: Boolean;
    FAuthors: String;
    FAuthorsExsit: Boolean;
    FYear: Integer;
    FYearExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetTitle(const _Value: String);
    procedure SetAuthors(const _Value: String);
    procedure SetYear(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddTitleEvent(Sender: TObject);
    procedure AddAuthorsEvent(Sender: TObject);
    procedure AddYearEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddTitle: String;
    procedure TitleRemove;
    function AddAuthors: String;
    procedure AuthorsRemove;
    function AddYear: Integer;
    procedure YearRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Title: String read FTitle write SetTitle;
    property Authors: String read FAuthors write SetAuthors;
    property Year: Integer read FYear write SetYear;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property TitleExsit: Boolean read FTitleExsit;
    property AuthorsExsit: Boolean read FAuthorsExsit;
    property YearExsit: Boolean read FYearExsit;
  end;

  TEMTFStatisticalEstimates= class(TXML)
  private
    FEstimates: TList<TEMTFEstimate>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetEstimates(const _Value: TList<TEMTFEstimate>);
    function GetEstimate(Index: Integer): TEMTFEstimate;
    procedure SetEstimate(Index: Integer; const _Value: TEMTFEstimate);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddEstimateEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddEstimate: TEMTFEstimate;
    procedure EstimateClear;
    function EstimateCount: Integer;
    procedure RemoveEstimate(_Value: TEMTFEstimate);
    procedure DeleteEstimate(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Estimates: TList<TEMTFEstimate> read FEstimates write SetEstimates;
    property Estimate[Index: Integer]: TEMTFEstimate read GetEstimate write SetEstimate;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFEstimate= class(TXML)
  private
    FName: String;
    FNameExsit: Boolean;
    FEType: String;
    FETypeExsit: Boolean;
    FDescription: String;
    FDescriptionExsit: Boolean;
    FExternalUrl: String;
    FExternalUrlExsit: Boolean;
    FIntention: String;
    FIntentionExsit: Boolean;
    FTag: String;
    FTagExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetEType(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetExternalUrl(const _Value: String);
    procedure SetIntention(const _Value: String);
    procedure SetTag(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddNameEvent(Sender: TObject);
    procedure AddETypeEvent(Sender: TObject);
    procedure AddDescriptionEvent(Sender: TObject);
    procedure AddExternalUrlEvent(Sender: TObject);
    procedure AddIntentionEvent(Sender: TObject);
    procedure AddTagEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddName: String;
    procedure NameRemove;
    function AddEType: String;
    procedure ETypeRemove;
    function AddDescription: String;
    procedure DescriptionRemove;
    function AddExternalUrl: String;
    procedure ExternalUrlRemove;
    function AddIntention: String;
    procedure IntentionRemove;
    function AddTag: String;
    procedure TagRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property EType: String read FEType write SetEType;
    property Description: String read FDescription write SetDescription;
    property ExternalUrl: String read FExternalUrl write SetExternalUrl;
    property Intention: String read FIntention write SetIntention;
    property Tag: String read FTag write SetTag;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property NameExsit: Boolean read FNameExsit;
    property ETypeExsit: Boolean read FETypeExsit;
    property DescriptionExsit: Boolean read FDescriptionExsit;
    property ExternalUrlExsit: Boolean read FExternalUrlExsit;
    property IntentionExsit: Boolean read FIntentionExsit;
    property TagExsit: Boolean read FTagExsit;
  end;

  TEMTFGridOrigin= class(TXML)
  private
    FLocation: TEMTFLocation;
    FLocationExsit: Boolean;
    FSiteCoords: TEMTFSiteCoords;
    FSiteCoordsExsit: Boolean;
    FName: String;
    FNameExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLocation(const _Value: TEMTFLocation);
    procedure SetSiteCoords(const _Value: TEMTFSiteCoords);
    procedure SetName(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddLocationEvent(Sender: TObject);
    procedure AddSiteCoordsEvent(Sender: TObject);
    procedure AddNameEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddLocation: TEMTFLocation;
    procedure LocationRemove;
    function AddSiteCoords: TEMTFSiteCoords;
    procedure SiteCoordsRemove;
    function AddName: String;
    procedure NameRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Location: TEMTFLocation read FLocation write SetLocation;
    property SiteCoords: TEMTFSiteCoords read FSiteCoords write SetSiteCoords;
    property Name: String read FName write SetName;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property LocationExsit: Boolean read FLocationExsit;
    property SiteCoordsExsit: Boolean read FSiteCoordsExsit;
    property NameExsit: Boolean read FNameExsit;
  end;

  TEMTFSiteCoords= class(TXML)
  private
    FStype: String;
    FStypeExsit: Boolean;
    Funits: String;
    FunitsExsit: Boolean;
    FX: Double;
    FXExsit: Boolean;
    FY: Double;
    FYExsit: Boolean;
    FZ: Double;
    FZExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetStype(const _Value: String);
    procedure Setunits(const _Value: String);
    procedure SetX(const _Value: Double);
    procedure SetY(const _Value: Double);
    procedure SetZ(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddStypeEvent(Sender: TObject);
    procedure AddunitsEvent(Sender: TObject);
    procedure AddXEvent(Sender: TObject);
    procedure AddYEvent(Sender: TObject);
    procedure AddZEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddStype: String;
    procedure StypeRemove;
    function Addunits: String;
    procedure unitsRemove;
    function AddX: Double;
    procedure XRemove;
    function AddY: Double;
    procedure YRemove;
    function AddZ: Double;
    procedure ZRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Stype: String read FStype write SetStype;
    property units: String read Funits write Setunits;
    property X: Double read FX write SetX;
    property Y: Double read FY write SetY;
    property Z: Double read FZ write SetZ;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property StypeExsit: Boolean read FStypeExsit;
    property unitsExsit: Boolean read FunitsExsit;
    property XExsit: Boolean read FXExsit;
    property YExsit: Boolean read FYExsit;
    property ZExsit: Boolean read FZExsit;
  end;

  TEMTFSiteLayout= class(TXML)
  private
    FInputChannels: TEMTFChannels;
    FOutputChannels: TEMTFChannels;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetInputChannels(const _Value: TEMTFChannels);
    procedure SetOutputChannels(const _Value: TEMTFChannels);
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
    property InputChannels: TEMTFChannels read FInputChannels write SetInputChannels;
    property OutputChannels: TEMTFChannels read FOutputChannels write SetOutputChannels;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TEMTFPeriodRange= class(TXML)
  private
    Fmin: Double;
    FminExsit: Boolean;
    Fmax: Double;
    FmaxExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure Setmin(const _Value: Double);
    procedure Setmax(const _Value: Double);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddminEvent(Sender: TObject);
    procedure AddmaxEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function Addmin: Double;
    procedure minRemove;
    function Addmax: Double;
    procedure maxRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property min: Double read Fmin write Setmin;
    property max: Double read Fmax write Setmax;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property minExsit: Boolean read FminExsit;
    property maxExsit: Boolean read FmaxExsit;
  end;


var
  TEMTFPop: TPopupMenu;
  TEMTFXMLInspector: TXMLInspector;
  TEMTFTreeComponent: TTree;
  TEMTFSitePop: TPopupMenu;
  TEMTFSiteXMLInspector: TXMLInspector;
  TEMTFSiteTreeComponent: TTree;
  TEMTFLocationPop: TPopupMenu;
  TEMTFLocationXMLInspector: TXMLInspector;
  TEMTFLocationTreeComponent: TTree;
  TEMTFProcessingInfoPop: TPopupMenu;
  TEMTFProcessingInfoXMLInspector: TXMLInspector;
  TEMTFProcessingInfoTreeComponent: TTree;
  TEMTFProcessingSoftwarePop: TPopupMenu;
  TEMTFProcessingSoftwareXMLInspector: TXMLInspector;
  TEMTFProcessingSoftwareTreeComponent: TTree;
  TEMTFInstrumentPop: TPopupMenu;
  TEMTFInstrumentXMLInspector: TXMLInspector;
  TEMTFInstrumentTreeComponent: TTree;
  TEMTFDipolePop: TPopupMenu;
  TEMTFDipoleXMLInspector: TXMLInspector;
  TEMTFDipoleTreeComponent: TTree;
  TEMTFDataTypesPop: TPopupMenu;
  TEMTFDataTypesXMLInspector: TXMLInspector;
  TEMTFDataTypesTreeComponent: TTree;
  TEMTFDateTypePop: TPopupMenu;
  TEMTFDateTypeXMLInspector: TXMLInspector;
  TEMTFDateTypeTreeComponent: TTree;
  TEMTFChannelsPop: TPopupMenu;
  TEMTFChannelsXMLInspector: TXMLInspector;
  TEMTFChannelsTreeComponent: TTree;
  TEMTFChannelPop: TPopupMenu;
  TEMTFChannelXMLInspector: TXMLInspector;
  TEMTFChannelTreeComponent: TTree;
  TEMTFDataPop: TPopupMenu;
  TEMTFDataXMLInspector: TXMLInspector;
  TEMTFDataTreeComponent: TTree;
  TEMTFValuePop: TPopupMenu;
  TEMTFValueXMLInspector: TXMLInspector;
  TEMTFValueTreeComponent: TTree;
  TEMTFProvenancePop: TPopupMenu;
  TEMTFProvenanceXMLInspector: TXMLInspector;
  TEMTFProvenanceTreeComponent: TTree;
  TEMTFPersonPop: TPopupMenu;
  TEMTFPersonXMLInspector: TXMLInspector;
  TEMTFPersonTreeComponent: TTree;
  TEMTFDataQualityNotesPop: TPopupMenu;
  TEMTFDataQualityNotesXMLInspector: TXMLInspector;
  TEMTFDataQualityNotesTreeComponent: TTree;
  TEMTFUnitValuePop: TPopupMenu;
  TEMTFUnitValueXMLInspector: TXMLInspector;
  TEMTFUnitValueTreeComponent: TTree;
  TEMTFElectrodePop: TPopupMenu;
  TEMTFElectrodeXMLInspector: TXMLInspector;
  TEMTFElectrodeTreeComponent: TTree;
  TEMTFPeriodDataPop: TPopupMenu;
  TEMTFPeriodDataXMLInspector: TXMLInspector;
  TEMTFPeriodDataTreeComponent: TTree;
  TEMTFTFDataPop: TPopupMenu;
  TEMTFTFDataXMLInspector: TXMLInspector;
  TEMTFTFDataTreeComponent: TTree;
  TEMTFCOVDataPop: TPopupMenu;
  TEMTFCOVDataXMLInspector: TXMLInspector;
  TEMTFCOVDataTreeComponent: TTree;
  TEMTFZPop: TPopupMenu;
  TEMTFZXMLInspector: TXMLInspector;
  TEMTFZTreeComponent: TTree;
  TEMTFDeclinationPop: TPopupMenu;
  TEMTFDeclinationXMLInspector: TXMLInspector;
  TEMTFDeclinationTreeComponent: TTree;
  TEMTFRemoteRefPop: TPopupMenu;
  TEMTFRemoteRefXMLInspector: TXMLInspector;
  TEMTFRemoteRefTreeComponent: TTree;
  TEMTFCommentsPop: TPopupMenu;
  TEMTFCommentsXMLInspector: TXMLInspector;
  TEMTFCommentsTreeComponent: TTree;
  TEMTFFilePop: TPopupMenu;
  TEMTFFileXMLInspector: TXMLInspector;
  TEMTFFileTreeComponent: TTree;
  TEMTFCopyrightPop: TPopupMenu;
  TEMTFCopyrightXMLInspector: TXMLInspector;
  TEMTFCopyrightTreeComponent: TTree;
  TEMTFCitationPop: TPopupMenu;
  TEMTFCitationXMLInspector: TXMLInspector;
  TEMTFCitationTreeComponent: TTree;
  TEMTFStatisticalEstimatesPop: TPopupMenu;
  TEMTFStatisticalEstimatesXMLInspector: TXMLInspector;
  TEMTFStatisticalEstimatesTreeComponent: TTree;
  TEMTFEstimatePop: TPopupMenu;
  TEMTFEstimateXMLInspector: TXMLInspector;
  TEMTFEstimateTreeComponent: TTree;
  TEMTFGridOriginPop: TPopupMenu;
  TEMTFGridOriginXMLInspector: TXMLInspector;
  TEMTFGridOriginTreeComponent: TTree;
  TEMTFSiteCoordsPop: TPopupMenu;
  TEMTFSiteCoordsXMLInspector: TXMLInspector;
  TEMTFSiteCoordsTreeComponent: TTree;
  TEMTFSiteLayoutPop: TPopupMenu;
  TEMTFSiteLayoutXMLInspector: TXMLInspector;
  TEMTFSiteLayoutTreeComponent: TTree;
  TEMTFPeriodRangePop: TPopupMenu;
  TEMTFPeriodRangeXMLInspector: TXMLInspector;
  TEMTFPeriodRangeTreeComponent: TTree;
  EMTFXMLObject: TObject;

implementation

{  EM_TF}
constructor TEMTF.Create(par: TXML = nil);
begin
  inherited Create(par);
  FNotess := TList<String>.Create;
  FTagss := TList<String>.Create;
  FProvenance := TEMTFProvenance.Create(Self);
  FSite := TEMTFSite.Create(Self);
  FProcessingInfo := TEMTFProcessingInfo.Create(Self);
  FData := TEMTFData.Create(Self);
end;

destructor TEMTF.Destroy;
begin
  FNotess.Free;
  FTagss.Free;
  FProvenance.Free;
  FSite.Free;
  FProcessingInfo.Free;
  FData.Free;
  if FPrimaryDataExsit then
    FPrimaryData.Free;
  if FAttachmentExsit then
    FAttachment.Free;
  if FCopyrightExsit then
    FCopyright.Free;
  if FStatisticalEstimatesExsit then
    FStatisticalEstimates.Free;
  if FDataTypesExsit then
    FDataTypes.Free;
  if FGridOriginExsit then
    FGridOrigin.Free;
  if FSiteLayoutExsit then
    FSiteLayout.Free;
  if FPeriodRangeExsit then
    FPeriodRange.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTF.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'ProductId' then
      begin
        FProductId := nodeTmp.Text;
        FProductIdExsit := True;
      end
      else if nodeTmp.NodeName = 'SubType' then
      begin
        FSubType := nodeTmp.Text;
        FSubTypeExsit := True;
      end
      else if nodeTmp.NodeName = 'Notes' then
      begin
        FNotess.Add(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'Tags' then
      begin
        FTagss.Add(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'Provenance' then
      begin
        FProvenance := TEMTFProvenance.Create(Self);
        FProvenance.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'Site' then
      begin
        FSite := TEMTFSite.Create(Self);
        FSite.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'ProcessingInfo' then
      begin
        FProcessingInfo := TEMTFProcessingInfo.Create(Self);
        FProcessingInfo.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'Data' then
      begin
        FData := TEMTFData.Create(Self);
        FData.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'PrimaryData' then
      begin
        FPrimaryData := TEMTFFile.Create(Self);
        FPrimaryData.FromXML(nodeTmp);
        FPrimaryDataExsit := True;
      end
      else if nodeTmp.NodeName = 'Attachment' then
      begin
        FAttachment := TEMTFFile.Create(Self);
        FAttachment.FromXML(nodeTmp);
        FAttachmentExsit := True;
      end
      else if nodeTmp.NodeName = 'Copyright' then
      begin
        FCopyright := TEMTFCopyright.Create(Self);
        FCopyright.FromXML(nodeTmp);
        FCopyrightExsit := True;
      end
      else if nodeTmp.NodeName = 'StatisticalEstimates' then
      begin
        FStatisticalEstimates := TEMTFStatisticalEstimates.Create(Self);
        FStatisticalEstimates.FromXML(nodeTmp);
        FStatisticalEstimatesExsit := True;
      end
      else if nodeTmp.NodeName = 'DataTypes' then
      begin
        FDataTypes := TEMTFDataTypes.Create(Self);
        FDataTypes.FromXML(nodeTmp);
        FDataTypesExsit := True;
      end
      else if nodeTmp.NodeName = 'GridOrigin' then
      begin
        FGridOrigin := TEMTFGridOrigin.Create(Self);
        FGridOrigin.FromXML(nodeTmp);
        FGridOriginExsit := True;
      end
      else if nodeTmp.NodeName = 'SiteLayout' then
      begin
        FSiteLayout := TEMTFSiteLayout.Create(Self);
        FSiteLayout.FromXML(nodeTmp);
        FSiteLayoutExsit := True;
      end
      else if nodeTmp.NodeName = 'PeriodRange' then
      begin
        FPeriodRange := TEMTFPeriodRange.Create(Self);
        FPeriodRange.FromXML(nodeTmp);
        FPeriodRangeExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('EM_TF Read XML Error!' + node.Xml);
  end;
end;

function TEMTF.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  DescriptionTmp: IXMLNode;
  ProductIdTmp: IXMLNode;
  SubTypeTmp: IXMLNode;
  NotesTmp: IXMLNode;
  TagsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'EM_TF';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('Description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FProductIdExsit then
    begin
      ProductIdTmp := doc.CreateNode('ProductId', ntElement);
      ProductIdTmp.NodeValue := FProductId;
      node.ChildNodes.Add(ProductIdTmp);
    end;
    if FSubTypeExsit then
    begin
      SubTypeTmp := doc.CreateNode('SubType', ntElement);
      SubTypeTmp.NodeValue := FSubType;
      node.ChildNodes.Add(SubTypeTmp);
    end;
    for I := 0 to FNotess.Count - 1 do
begin
      NotesTmp := doc.CreateNode('Notes', ntElement);
      NotesTmp.NodeValue := FNotess.Items[I];
      node.ChildNodes.Add(NotesTmp);
    end;
    for I := 0 to FTagss.Count - 1 do
begin
      TagsTmp := doc.CreateNode('Tags', ntElement);
      TagsTmp.NodeValue := FTagss.Items[I];
      node.ChildNodes.Add(TagsTmp);
    end;
    FProvenance.ToXML(node, 'Provenance');
    FSite.ToXML(node, 'Site');
    FProcessingInfo.ToXML(node, 'ProcessingInfo');
    FData.ToXML(node, 'Data');
    if FPrimaryDataExsit then
      FPrimaryData.ToXML(node, 'PrimaryData');
    if FAttachmentExsit then
      FAttachment.ToXML(node, 'Attachment');
    if FCopyrightExsit then
      FCopyright.ToXML(node, 'Copyright');
    if FStatisticalEstimatesExsit then
      FStatisticalEstimates.ToXML(node, 'StatisticalEstimates');
    if FDataTypesExsit then
      FDataTypes.ToXML(node, 'DataTypes');
    if FGridOriginExsit then
      FGridOrigin.ToXML(node, 'GridOrigin');
    if FSiteLayoutExsit then
      FSiteLayout.ToXML(node, 'SiteLayout');
    if FPeriodRangeExsit then
      FPeriodRange.ToXML(node, 'PeriodRange');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTF.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.Text.Clear;
  TreeNodeShape.Text.Add('EMTF');
  TreeNodeShape.OnClick := OnClick;
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if ProductIdExsit then
    TreeNodeShape.AddChild('ProductId');
  if SubTypeExsit then
    TreeNodeShape.AddChild('SubType');
  for I := 0 to NotesCount - 1 do
  begin
    TreeNodeShape.AddChild('Notes');
  end;
  for I := 0 to TagsCount - 1 do
  begin
    TreeNodeShape.AddChild('Tags');
  end;
  Provenance.TreeNodeShape := TreeNodeShape.AddChildObject('Provenance', Provenance);
  Provenance.ToTree;
  Site.TreeNodeShape := TreeNodeShape.AddChildObject('Site', Site);
  Site.ToTree;
  ProcessingInfo.TreeNodeShape := TreeNodeShape.AddChildObject('ProcessingInfo', ProcessingInfo);
  ProcessingInfo.ToTree;
  Data.TreeNodeShape := TreeNodeShape.AddChildObject('Data', Data);
  Data.ToTree;
  if PrimaryDataExsit then
  begin
    PrimaryData.TreeNodeShape := TreeNodeShape.AddChildObject('PrimaryData', PrimaryData);
    PrimaryData.ToTree;
  end;
  if AttachmentExsit then
  begin
    Attachment.TreeNodeShape := TreeNodeShape.AddChildObject('Attachment', Attachment);
    Attachment.ToTree;
  end;
  if CopyrightExsit then
  begin
    Copyright.TreeNodeShape := TreeNodeShape.AddChildObject('Copyright', Copyright);
    Copyright.ToTree;
  end;
  if StatisticalEstimatesExsit then
  begin
    StatisticalEstimates.TreeNodeShape := TreeNodeShape.AddChildObject('StatisticalEstimates', StatisticalEstimates);
    StatisticalEstimates.ToTree;
  end;
  if DataTypesExsit then
  begin
    DataTypes.TreeNodeShape := TreeNodeShape.AddChildObject('DataTypes', DataTypes);
    DataTypes.ToTree;
  end;
  if GridOriginExsit then
  begin
    GridOrigin.TreeNodeShape := TreeNodeShape.AddChildObject('GridOrigin', GridOrigin);
    GridOrigin.ToTree;
  end;
  if SiteLayoutExsit then
  begin
    SiteLayout.TreeNodeShape := TreeNodeShape.AddChildObject('SiteLayout', SiteLayout);
    SiteLayout.ToTree;
  end;
  if PeriodRangeExsit then
  begin
    PeriodRange.TreeNodeShape := TreeNodeShape.AddChildObject('PeriodRange', PeriodRange);
    PeriodRange.ToTree;
  end;
end;

procedure TEMTF.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  DescriptionAddMenu: TMenuItem;
  ProductIdAddMenu: TMenuItem;
  SubTypeAddMenu: TMenuItem;
  NotesAddMenu: TMenuItem;
  TagsAddMenu: TMenuItem;
  PrimaryDataAddMenu: TMenuItem;
  AttachmentAddMenu: TMenuItem;
  CopyrightAddMenu: TMenuItem;
  StatisticalEstimatesAddMenu: TMenuItem;
  DataTypesAddMenu: TMenuItem;
  GridOriginAddMenu: TMenuItem;
  SiteLayoutAddMenu: TMenuItem;
  PeriodRangeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFPop) and Assigned(TEMTFTreeComponent) then
    begin
      TEMTFPop.Clear;
      DescriptionAddMenu := TMenuItem.Create(TEMTFPop);
      DescriptionAddMenu.Text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TEMTFPop.AddObject(DescriptionAddMenu);
      ProductIdAddMenu := TMenuItem.Create(TEMTFPop);
      ProductIdAddMenu.Text := 'Add ProductId';
      ProductIdAddMenu.OnClick := AddProductIdEvent;
      TEMTFPop.AddObject(ProductIdAddMenu);
      SubTypeAddMenu := TMenuItem.Create(TEMTFPop);
      SubTypeAddMenu.Text := 'Add SubType';
      SubTypeAddMenu.OnClick := AddSubTypeEvent;
      TEMTFPop.AddObject(SubTypeAddMenu);
      NotesAddMenu := TMenuItem.Create(TEMTFPop);
      NotesAddMenu.Text := 'Add Notes';
      NotesAddMenu.OnClick := AddNotesEvent;
      TEMTFPop.AddObject(NotesAddMenu);
      TagsAddMenu := TMenuItem.Create(TEMTFPop);
      TagsAddMenu.Text := 'Add Tags';
      TagsAddMenu.OnClick := AddTagsEvent;
      TEMTFPop.AddObject(TagsAddMenu);
      PrimaryDataAddMenu := TMenuItem.Create(TEMTFPop);
      PrimaryDataAddMenu.Text := 'Add PrimaryData';
      PrimaryDataAddMenu.OnClick := AddPrimaryDataEvent;
      TEMTFPop.AddObject(PrimaryDataAddMenu);
      AttachmentAddMenu := TMenuItem.Create(TEMTFPop);
      AttachmentAddMenu.Text := 'Add Attachment';
      AttachmentAddMenu.OnClick := AddAttachmentEvent;
      TEMTFPop.AddObject(AttachmentAddMenu);
      CopyrightAddMenu := TMenuItem.Create(TEMTFPop);
      CopyrightAddMenu.Text := 'Add Copyright';
      CopyrightAddMenu.OnClick := AddCopyrightEvent;
      TEMTFPop.AddObject(CopyrightAddMenu);
      StatisticalEstimatesAddMenu := TMenuItem.Create(TEMTFPop);
      StatisticalEstimatesAddMenu.Text := 'Add StatisticalEstimates';
      StatisticalEstimatesAddMenu.OnClick := AddStatisticalEstimatesEvent;
      TEMTFPop.AddObject(StatisticalEstimatesAddMenu);
      DataTypesAddMenu := TMenuItem.Create(TEMTFPop);
      DataTypesAddMenu.Text := 'Add DataTypes';
      DataTypesAddMenu.OnClick := AddDataTypesEvent;
      TEMTFPop.AddObject(DataTypesAddMenu);
      GridOriginAddMenu := TMenuItem.Create(TEMTFPop);
      GridOriginAddMenu.Text := 'Add GridOrigin';
      GridOriginAddMenu.OnClick := AddGridOriginEvent;
      TEMTFPop.AddObject(GridOriginAddMenu);
      SiteLayoutAddMenu := TMenuItem.Create(TEMTFPop);
      SiteLayoutAddMenu.Text := 'Add SiteLayout';
      SiteLayoutAddMenu.OnClick := AddSiteLayoutEvent;
      TEMTFPop.AddObject(SiteLayoutAddMenu);
      PeriodRangeAddMenu := TMenuItem.Create(TEMTFPop);
      PeriodRangeAddMenu.Text := 'Add PeriodRange';
      PeriodRangeAddMenu.OnClick := AddPeriodRangeEvent;
      TEMTFPop.AddObject(PeriodRangeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFTreeComponent.ClientToScreen(pt);
      TEMTFPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTF.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('ProductId');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ProductId);
  Names_Value.Add('SubType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(SubType);
  TEMTFXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTF.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Description := _Value;
      end;
    1:
      begin
        ProductId := _Value;
      end;
    2:
      begin
        SubType := _Value;
      end;
  end;
  ToTree;
end;

function TEMTF.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TEMTF.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TEMTF.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TEMTF.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TEMTF.AddProductId: String;
begin;
  Result := FProductId;
  FProductIdExsit := True;
end;

procedure TEMTF.SetProductId(const _Value: String);
begin
  FProductIdExsit := True;
  FProductId := _Value;
end;

procedure TEMTF.ProductIdRemove;
begin
  if FProductIdExsit then
  begin
    FProductIdExsit := False;
  end;
end;

procedure TEMTF.AddProductIdEvent(Sender: TObject);
begin
  AddProductId;
end;

function TEMTF.AddSubType: String;
begin;
  Result := FSubType;
  FSubTypeExsit := True;
end;

procedure TEMTF.SetSubType(const _Value: String);
begin
  FSubTypeExsit := True;
  FSubType := _Value;
end;

procedure TEMTF.SubTypeRemove;
begin
  if FSubTypeExsit then
  begin
    FSubTypeExsit := False;
  end;
end;

procedure TEMTF.AddSubTypeEvent(Sender: TObject);
begin
  AddSubType;
end;

function TEMTF.AddNotes: String;
var
Notestmp: String;
begin;
  FNotess.Add(Notestmp);
  Result := Notestmp;
end;

procedure TEMTF.SetNotess(const _Value: TList<String>);
begin
  FNotess.Clear;
  FNotess := _Value;
end;

procedure TEMTF.NotesClear;
begin
  FNotess.Clear;
end;

function TEMTF.NotesCount: Integer;
begin
  Result := FNotess.Count;
end;

function TEMTF.GetNotes(Index: Integer): String;
begin
  Result := FNotess[Index];
end;

procedure TEMTF.SetNotes(Index: Integer;
  const _Value: String);
begin
  FNotess[Index] := _Value;
end;

procedure TEMTF.RemoveNotes(_Value: String);
begin
  FNotess.Remove(_Value);
end;

procedure TEMTF.DeleteNotes(Index: Integer);
begin
  FNotess.Delete(Index);
end;

procedure TEMTF.AddNotesEvent(Sender: TObject);
begin
  AddNotes;
end;

function TEMTF.AddTags: String;
var
Tagstmp: String;
begin;
  FTagss.Add(Tagstmp);
  Result := Tagstmp;
end;

procedure TEMTF.SetTagss(const _Value: TList<String>);
begin
  FTagss.Clear;
  FTagss := _Value;
end;

procedure TEMTF.TagsClear;
begin
  FTagss.Clear;
end;

function TEMTF.TagsCount: Integer;
begin
  Result := FTagss.Count;
end;

function TEMTF.GetTags(Index: Integer): String;
begin
  Result := FTagss[Index];
end;

procedure TEMTF.SetTags(Index: Integer;
  const _Value: String);
begin
  FTagss[Index] := _Value;
end;

procedure TEMTF.RemoveTags(_Value: String);
begin
  FTagss.Remove(_Value);
end;

procedure TEMTF.DeleteTags(Index: Integer);
begin
  FTagss.Delete(Index);
end;

procedure TEMTF.AddTagsEvent(Sender: TObject);
begin
  AddTags;
end;

procedure TEMTF.SetProvenance(const _Value: TEMTFProvenance);
begin
  FProvenance.Free;
  FProvenance := _Value;
  FProvenance.Parent := Self;
end;

procedure TEMTF.SetSite(const _Value: TEMTFSite);
begin
  FSite.Free;
  FSite := _Value;
  FSite.Parent := Self;
end;

procedure TEMTF.SetProcessingInfo(const _Value: TEMTFProcessingInfo);
begin
  FProcessingInfo.Free;
  FProcessingInfo := _Value;
  FProcessingInfo.Parent := Self;
end;

procedure TEMTF.SetData(const _Value: TEMTFData);
begin
  FData.Free;
  FData := _Value;
  FData.Parent := Self;
end;

function TEMTF.AddPrimaryData: TEMTFFile;
begin;
  if not FPrimaryDataExsit then
    FPrimaryData := TEMTFFile.Create(Self);
  Result := FPrimaryData;
  FPrimaryDataExsit := True;
end;

procedure TEMTF.SetPrimaryData(const _Value: TEMTFFile);
begin
  if FPrimaryDataExsit then
    FPrimaryData.Free;
  FPrimaryDataExsit := True;
  FPrimaryData := _Value;
  FPrimaryData.Parent := Self;
end;

procedure TEMTF.PrimaryDataRemove;
begin
  if FPrimaryDataExsit then
  begin
    FPrimaryData.Free;
    FPrimaryDataExsit := False;
  end;
end;

procedure TEMTF.AddPrimaryDataEvent(Sender: TObject);
begin
  AddPrimaryData;
  FPrimaryData.ToTree;
end;

function TEMTF.AddAttachment: TEMTFFile;
begin;
  if not FAttachmentExsit then
    FAttachment := TEMTFFile.Create(Self);
  Result := FAttachment;
  FAttachmentExsit := True;
end;

procedure TEMTF.SetAttachment(const _Value: TEMTFFile);
begin
  if FAttachmentExsit then
    FAttachment.Free;
  FAttachmentExsit := True;
  FAttachment := _Value;
  FAttachment.Parent := Self;
end;

procedure TEMTF.AttachmentRemove;
begin
  if FAttachmentExsit then
  begin
    FAttachment.Free;
    FAttachmentExsit := False;
  end;
end;

procedure TEMTF.AddAttachmentEvent(Sender: TObject);
begin
  AddAttachment;
  FAttachment.ToTree;
end;

function TEMTF.AddCopyright: TEMTFCopyright;
begin;
  if not FCopyrightExsit then
    FCopyright := TEMTFCopyright.Create(Self);
  Result := FCopyright;
  FCopyrightExsit := True;
end;

procedure TEMTF.SetCopyright(const _Value: TEMTFCopyright);
begin
  if FCopyrightExsit then
    FCopyright.Free;
  FCopyrightExsit := True;
  FCopyright := _Value;
  FCopyright.Parent := Self;
end;

procedure TEMTF.CopyrightRemove;
begin
  if FCopyrightExsit then
  begin
    FCopyright.Free;
    FCopyrightExsit := False;
  end;
end;

procedure TEMTF.AddCopyrightEvent(Sender: TObject);
begin
  AddCopyright;
  FCopyright.ToTree;
end;

function TEMTF.AddStatisticalEstimates: TEMTFStatisticalEstimates;
begin;
  if not FStatisticalEstimatesExsit then
    FStatisticalEstimates := TEMTFStatisticalEstimates.Create(Self);
  Result := FStatisticalEstimates;
  FStatisticalEstimatesExsit := True;
end;

procedure TEMTF.SetStatisticalEstimates(const _Value: TEMTFStatisticalEstimates);
begin
  if FStatisticalEstimatesExsit then
    FStatisticalEstimates.Free;
  FStatisticalEstimatesExsit := True;
  FStatisticalEstimates := _Value;
  FStatisticalEstimates.Parent := Self;
end;

procedure TEMTF.StatisticalEstimatesRemove;
begin
  if FStatisticalEstimatesExsit then
  begin
    FStatisticalEstimates.Free;
    FStatisticalEstimatesExsit := False;
  end;
end;

procedure TEMTF.AddStatisticalEstimatesEvent(Sender: TObject);
begin
  AddStatisticalEstimates;
  FStatisticalEstimates.ToTree;
end;

function TEMTF.AddDataTypes: TEMTFDataTypes;
begin;
  if not FDataTypesExsit then
    FDataTypes := TEMTFDataTypes.Create(Self);
  Result := FDataTypes;
  FDataTypesExsit := True;
end;

procedure TEMTF.SetDataTypes(const _Value: TEMTFDataTypes);
begin
  if FDataTypesExsit then
    FDataTypes.Free;
  FDataTypesExsit := True;
  FDataTypes := _Value;
  FDataTypes.Parent := Self;
end;

procedure TEMTF.DataTypesRemove;
begin
  if FDataTypesExsit then
  begin
    FDataTypes.Free;
    FDataTypesExsit := False;
  end;
end;

procedure TEMTF.AddDataTypesEvent(Sender: TObject);
begin
  AddDataTypes;
  FDataTypes.ToTree;
end;

function TEMTF.AddGridOrigin: TEMTFGridOrigin;
begin;
  if not FGridOriginExsit then
    FGridOrigin := TEMTFGridOrigin.Create(Self);
  Result := FGridOrigin;
  FGridOriginExsit := True;
end;

procedure TEMTF.SetGridOrigin(const _Value: TEMTFGridOrigin);
begin
  if FGridOriginExsit then
    FGridOrigin.Free;
  FGridOriginExsit := True;
  FGridOrigin := _Value;
  FGridOrigin.Parent := Self;
end;

procedure TEMTF.GridOriginRemove;
begin
  if FGridOriginExsit then
  begin
    FGridOrigin.Free;
    FGridOriginExsit := False;
  end;
end;

procedure TEMTF.AddGridOriginEvent(Sender: TObject);
begin
  AddGridOrigin;
  FGridOrigin.ToTree;
end;

function TEMTF.AddSiteLayout: TEMTFSiteLayout;
begin;
  if not FSiteLayoutExsit then
    FSiteLayout := TEMTFSiteLayout.Create(Self);
  Result := FSiteLayout;
  FSiteLayoutExsit := True;
end;

procedure TEMTF.SetSiteLayout(const _Value: TEMTFSiteLayout);
begin
  if FSiteLayoutExsit then
    FSiteLayout.Free;
  FSiteLayoutExsit := True;
  FSiteLayout := _Value;
  FSiteLayout.Parent := Self;
end;

procedure TEMTF.SiteLayoutRemove;
begin
  if FSiteLayoutExsit then
  begin
    FSiteLayout.Free;
    FSiteLayoutExsit := False;
  end;
end;

procedure TEMTF.AddSiteLayoutEvent(Sender: TObject);
begin
  AddSiteLayout;
  FSiteLayout.ToTree;
end;

function TEMTF.AddPeriodRange: TEMTFPeriodRange;
begin;
  if not FPeriodRangeExsit then
    FPeriodRange := TEMTFPeriodRange.Create(Self);
  Result := FPeriodRange;
  FPeriodRangeExsit := True;
end;

procedure TEMTF.SetPeriodRange(const _Value: TEMTFPeriodRange);
begin
  if FPeriodRangeExsit then
    FPeriodRange.Free;
  FPeriodRangeExsit := True;
  FPeriodRange := _Value;
  FPeriodRange.Parent := Self;
end;

procedure TEMTF.PeriodRangeRemove;
begin
  if FPeriodRangeExsit then
  begin
    FPeriodRange.Free;
    FPeriodRangeExsit := False;
  end;
end;

procedure TEMTF.AddPeriodRangeEvent(Sender: TObject);
begin
  AddPeriodRange;
  FPeriodRange.ToTree;
end;

{  Site}
constructor TEMTFSite.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFSite.Destroy;
begin
  if FLocationExsit then
    FLocation.Free;
  if FDataQualityNotesExsit then
    FDataQualityNotes.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFSite.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Project' then
      begin
        FProject := nodeTmp.Text;
        FProjectExsit := True;
      end
      else if nodeTmp.NodeName = 'Survey' then
      begin
        FSurvey := nodeTmp.Text;
        FSurveyExsit := True;
      end
      else if nodeTmp.NodeName = 'AcquiredBy' then
      begin
        FAcquiredBy := nodeTmp.Text;
        FAcquiredByExsit := True;
      end
      else if nodeTmp.NodeName = 'YearCollected' then
      begin
        FYearCollected := nodeTmp.Text.ToInteger;
        FYearCollectedExsit := True;
      end
      else if nodeTmp.NodeName = 'Id' then
      begin
        FId := nodeTmp.Text;
        FIdExsit := True;
      end
      else if nodeTmp.NodeName = 'Name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'ReleaseStatus' then
      begin
        FReleaseStatus := nodeTmp.Text;
        FReleaseStatusExsit := True;
      end
      else if nodeTmp.NodeName = 'Start' then
      begin
        FStartTime := StrToDateTimeDef(nodeTmp.Text, Now());
        FStartTimeExsit := True;
      end
      else if nodeTmp.NodeName = 'End' then
      begin
        FEndTime := StrToDateTimeDef(nodeTmp.Text, Now());
        FEndTimeExsit := True;
      end
      else if nodeTmp.NodeName = 'RunList' then
      begin
        FRunList := nodeTmp.Text;
        FRunListExsit := True;
      end
      else if nodeTmp.NodeName = 'Location' then
      begin
        FLocation := TEMTFLocation.Create(Self);
        FLocation.FromXML(nodeTmp);
        FLocationExsit := True;
      end
      else if nodeTmp.NodeName = 'DataQualityNotes' then
      begin
        FDataQualityNotes := TEMTFDataQualityNotes.Create(Self);
        FDataQualityNotes.FromXML(nodeTmp);
        FDataQualityNotesExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'number' then
      begin
        FNumber := nodeTmp.Text.ToInteger;
        FNumberExsit := True;
      end;
    end;
  except
    raise Exception.Create('Site Read XML Error!' + node.Xml);
  end;
end;

function TEMTFSite.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NumberTmp: IXMLNode;
  ProjectTmp: IXMLNode;
  SurveyTmp: IXMLNode;
  AcquiredByTmp: IXMLNode;
  YearCollectedTmp: IXMLNode;
  IdTmp: IXMLNode;
  NameTmp: IXMLNode;
  ReleaseStatusTmp: IXMLNode;
  StartTimeTmp: IXMLNode;
  EndTimeTmp: IXMLNode;
  RunListTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Site';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FProjectExsit then
    begin
      ProjectTmp := doc.CreateNode('Project', ntElement);
      ProjectTmp.NodeValue := FProject;
      node.ChildNodes.Add(ProjectTmp);
    end;
    if FSurveyExsit then
    begin
      SurveyTmp := doc.CreateNode('Survey', ntElement);
      SurveyTmp.NodeValue := FSurvey;
      node.ChildNodes.Add(SurveyTmp);
    end;
    if FAcquiredByExsit then
    begin
      AcquiredByTmp := doc.CreateNode('AcquiredBy', ntElement);
      AcquiredByTmp.NodeValue := FAcquiredBy;
      node.ChildNodes.Add(AcquiredByTmp);
    end;
    if FYearCollectedExsit then
    begin
      YearCollectedTmp := doc.CreateNode('YearCollected', ntElement);
      YearCollectedTmp.NodeValue := FYearCollected.toString;
      node.ChildNodes.Add(YearCollectedTmp);
    end;
    if FIdExsit then
    begin
      IdTmp := doc.CreateNode('Id', ntElement);
      IdTmp.NodeValue := FId;
      node.ChildNodes.Add(IdTmp);
    end;
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('Name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FReleaseStatusExsit then
    begin
      ReleaseStatusTmp := doc.CreateNode('ReleaseStatus', ntElement);
      ReleaseStatusTmp.NodeValue := FReleaseStatus;
      node.ChildNodes.Add(ReleaseStatusTmp);
    end;
    if FStartTimeExsit then
    begin
      StartTimeTmp := doc.CreateNode('Start', ntElement);
      StartTimeTmp.NodeValue := FormatDateTime(XMLDTFormat, FStartTime);
      node.ChildNodes.Add(StartTimeTmp);
    end;
    if FEndTimeExsit then
    begin
      EndTimeTmp := doc.CreateNode('End', ntElement);
      EndTimeTmp.NodeValue := FormatDateTime(XMLDTFormat, FEndTime);
      node.ChildNodes.Add(EndTimeTmp);
    end;
    if FRunListExsit then
    begin
      RunListTmp := doc.CreateNode('RunList', ntElement);
      RunListTmp.NodeValue := FRunList;
      node.ChildNodes.Add(RunListTmp);
    end;
    if FLocationExsit then
      FLocation.ToXML(node, 'Location');
    if FDataQualityNotesExsit then
      FDataQualityNotes.ToXML(node, 'DataQualityNotes');
    if FNumberExsit then 
    begin
      NumberTmp := doc.CreateNode('number', ntAttribute);
      NumberTmp.NodeValue := FNumber.toString;
      node.AttributeNodes.Add(NumberTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFSite.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if NumberExsit then
    TreeNodeShape.AddChild('Number');
  if ProjectExsit then
    TreeNodeShape.AddChild('Project');
  if SurveyExsit then
    TreeNodeShape.AddChild('Survey');
  if AcquiredByExsit then
    TreeNodeShape.AddChild('AcquiredBy');
  if YearCollectedExsit then
    TreeNodeShape.AddChild('YearCollected');
  if IdExsit then
    TreeNodeShape.AddChild('Id');
  if NameExsit then
    TreeNodeShape.AddChild('Name');
  if ReleaseStatusExsit then
    TreeNodeShape.AddChild('ReleaseStatus');
  if StartTimeExsit then
    TreeNodeShape.AddChild('StartTime');
  if EndTimeExsit then
    TreeNodeShape.AddChild('EndTime');
  if RunListExsit then
    TreeNodeShape.AddChild('RunList');
  if LocationExsit then
  begin
    Location.TreeNodeShape := TreeNodeShape.AddChildObject('Location', Location);
    Location.ToTree;
  end;
  if DataQualityNotesExsit then
  begin
    DataQualityNotes.TreeNodeShape := TreeNodeShape.AddChildObject('DataQualityNotes', DataQualityNotes);
    DataQualityNotes.ToTree;
  end;
end;

procedure TEMTFSite.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NumberAddMenu: TMenuItem;
  ProjectAddMenu: TMenuItem;
  SurveyAddMenu: TMenuItem;
  AcquiredByAddMenu: TMenuItem;
  YearCollectedAddMenu: TMenuItem;
  IdAddMenu: TMenuItem;
  NameAddMenu: TMenuItem;
  ReleaseStatusAddMenu: TMenuItem;
  StartTimeAddMenu: TMenuItem;
  EndTimeAddMenu: TMenuItem;
  RunListAddMenu: TMenuItem;
  LocationAddMenu: TMenuItem;
  DataQualityNotesAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFSitePop) and Assigned(TEMTFSiteTreeComponent) then
    begin
      TEMTFSitePop.Clear;
      NumberAddMenu := TMenuItem.Create(TEMTFSitePop);
      NumberAddMenu.Text := 'Add Number';
      NumberAddMenu.OnClick := AddNumberEvent;
      TEMTFSitePop.AddObject(NumberAddMenu);
      ProjectAddMenu := TMenuItem.Create(TEMTFSitePop);
      ProjectAddMenu.Text := 'Add Project';
      ProjectAddMenu.OnClick := AddProjectEvent;
      TEMTFSitePop.AddObject(ProjectAddMenu);
      SurveyAddMenu := TMenuItem.Create(TEMTFSitePop);
      SurveyAddMenu.Text := 'Add Survey';
      SurveyAddMenu.OnClick := AddSurveyEvent;
      TEMTFSitePop.AddObject(SurveyAddMenu);
      AcquiredByAddMenu := TMenuItem.Create(TEMTFSitePop);
      AcquiredByAddMenu.Text := 'Add AcquiredBy';
      AcquiredByAddMenu.OnClick := AddAcquiredByEvent;
      TEMTFSitePop.AddObject(AcquiredByAddMenu);
      YearCollectedAddMenu := TMenuItem.Create(TEMTFSitePop);
      YearCollectedAddMenu.Text := 'Add YearCollected';
      YearCollectedAddMenu.OnClick := AddYearCollectedEvent;
      TEMTFSitePop.AddObject(YearCollectedAddMenu);
      IdAddMenu := TMenuItem.Create(TEMTFSitePop);
      IdAddMenu.Text := 'Add Id';
      IdAddMenu.OnClick := AddIdEvent;
      TEMTFSitePop.AddObject(IdAddMenu);
      NameAddMenu := TMenuItem.Create(TEMTFSitePop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFSitePop.AddObject(NameAddMenu);
      ReleaseStatusAddMenu := TMenuItem.Create(TEMTFSitePop);
      ReleaseStatusAddMenu.Text := 'Add ReleaseStatus';
      ReleaseStatusAddMenu.OnClick := AddReleaseStatusEvent;
      TEMTFSitePop.AddObject(ReleaseStatusAddMenu);
      StartTimeAddMenu := TMenuItem.Create(TEMTFSitePop);
      StartTimeAddMenu.Text := 'Add StartTime';
      StartTimeAddMenu.OnClick := AddStartTimeEvent;
      TEMTFSitePop.AddObject(StartTimeAddMenu);
      EndTimeAddMenu := TMenuItem.Create(TEMTFSitePop);
      EndTimeAddMenu.Text := 'Add EndTime';
      EndTimeAddMenu.OnClick := AddEndTimeEvent;
      TEMTFSitePop.AddObject(EndTimeAddMenu);
      RunListAddMenu := TMenuItem.Create(TEMTFSitePop);
      RunListAddMenu.Text := 'Add RunList';
      RunListAddMenu.OnClick := AddRunListEvent;
      TEMTFSitePop.AddObject(RunListAddMenu);
      LocationAddMenu := TMenuItem.Create(TEMTFSitePop);
      LocationAddMenu.Text := 'Add Location';
      LocationAddMenu.OnClick := AddLocationEvent;
      TEMTFSitePop.AddObject(LocationAddMenu);
      DataQualityNotesAddMenu := TMenuItem.Create(TEMTFSitePop);
      DataQualityNotesAddMenu.Text := 'Add DataQualityNotes';
      DataQualityNotesAddMenu.OnClick := AddDataQualityNotesEvent;
      TEMTFSitePop.AddObject(DataQualityNotesAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFSiteTreeComponent.ClientToScreen(pt);
      TEMTFSitePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFSite.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFSiteXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Number');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Number.toString);
  Names_Value.Add('Project');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Project);
  Names_Value.Add('Survey');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Survey);
  Names_Value.Add('AcquiredBy');
  Types_Value.Add(xs_string);
  _Values_Value.Add(AcquiredBy);
  Names_Value.Add('YearCollected');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(YearCollected.toString);
  Names_Value.Add('Id');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Id);
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('ReleaseStatus');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ReleaseStatus);
  Names_Value.Add('StartTime');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, StartTime));
  Names_Value.Add('EndTime');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, EndTime));
  Names_Value.Add('RunList');
  Types_Value.Add(xs_string);
  _Values_Value.Add(RunList);
  TEMTFSiteXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFSite.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Number := _Value.ToInteger;
      end;
    1:
      begin
        Project := _Value;
      end;
    2:
      begin
        Survey := _Value;
      end;
    3:
      begin
        AcquiredBy := _Value;
      end;
    4:
      begin
        YearCollected := _Value.ToInteger;
      end;
    5:
      begin
        Id := _Value;
      end;
    6:
      begin
        Name := _Value;
      end;
    7:
      begin
        ReleaseStatus := _Value;
      end;
    8:
      begin
        StartTime := StrToDateTimeDef(_Value, Now());
      end;
    9:
      begin
        EndTime := StrToDateTimeDef(_Value, Now());
      end;
    10:
      begin
        RunList := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFSite.AddNumber: Integer;
begin;
  Result := FNumber;
  FNumberExsit := True;
end;

procedure TEMTFSite.SetNumber(const _Value: Integer);
begin
  FNumberExsit := True;
  FNumber := _Value;
end;

procedure TEMTFSite.NumberRemove;
begin
  if FNumberExsit then
  begin
    FNumberExsit := False;
  end;
end;

procedure TEMTFSite.AddNumberEvent(Sender: TObject);
begin
  AddNumber;
end;

function TEMTFSite.AddProject: String;
begin;
  Result := FProject;
  FProjectExsit := True;
end;

procedure TEMTFSite.SetProject(const _Value: String);
begin
  FProjectExsit := True;
  FProject := _Value;
end;

procedure TEMTFSite.ProjectRemove;
begin
  if FProjectExsit then
  begin
    FProjectExsit := False;
  end;
end;

procedure TEMTFSite.AddProjectEvent(Sender: TObject);
begin
  AddProject;
end;

function TEMTFSite.AddSurvey: String;
begin;
  Result := FSurvey;
  FSurveyExsit := True;
end;

procedure TEMTFSite.SetSurvey(const _Value: String);
begin
  FSurveyExsit := True;
  FSurvey := _Value;
end;

procedure TEMTFSite.SurveyRemove;
begin
  if FSurveyExsit then
  begin
    FSurveyExsit := False;
  end;
end;

procedure TEMTFSite.AddSurveyEvent(Sender: TObject);
begin
  AddSurvey;
end;

function TEMTFSite.AddAcquiredBy: String;
begin;
  Result := FAcquiredBy;
  FAcquiredByExsit := True;
end;

procedure TEMTFSite.SetAcquiredBy(const _Value: String);
begin
  FAcquiredByExsit := True;
  FAcquiredBy := _Value;
end;

procedure TEMTFSite.AcquiredByRemove;
begin
  if FAcquiredByExsit then
  begin
    FAcquiredByExsit := False;
  end;
end;

procedure TEMTFSite.AddAcquiredByEvent(Sender: TObject);
begin
  AddAcquiredBy;
end;

function TEMTFSite.AddYearCollected: Integer;
begin;
  Result := FYearCollected;
  FYearCollectedExsit := True;
end;

procedure TEMTFSite.SetYearCollected(const _Value: Integer);
begin
  FYearCollectedExsit := True;
  FYearCollected := _Value;
end;

procedure TEMTFSite.YearCollectedRemove;
begin
  if FYearCollectedExsit then
  begin
    FYearCollectedExsit := False;
  end;
end;

procedure TEMTFSite.AddYearCollectedEvent(Sender: TObject);
begin
  AddYearCollected;
end;

function TEMTFSite.AddId: String;
begin;
  Result := FId;
  FIdExsit := True;
end;

procedure TEMTFSite.SetId(const _Value: String);
begin
  FIdExsit := True;
  FId := _Value;
end;

procedure TEMTFSite.IdRemove;
begin
  if FIdExsit then
  begin
    FIdExsit := False;
  end;
end;

procedure TEMTFSite.AddIdEvent(Sender: TObject);
begin
  AddId;
end;

function TEMTFSite.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFSite.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFSite.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFSite.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TEMTFSite.AddReleaseStatus: String;
begin;
  Result := FReleaseStatus;
  FReleaseStatusExsit := True;
end;

procedure TEMTFSite.SetReleaseStatus(const _Value: String);
begin
  FReleaseStatusExsit := True;
  FReleaseStatus := _Value;
end;

procedure TEMTFSite.ReleaseStatusRemove;
begin
  if FReleaseStatusExsit then
  begin
    FReleaseStatusExsit := False;
  end;
end;

procedure TEMTFSite.AddReleaseStatusEvent(Sender: TObject);
begin
  AddReleaseStatus;
end;

function TEMTFSite.AddStartTime: TDateTime;
begin;
  Result := FStartTime;
  FStartTimeExsit := True;
end;

procedure TEMTFSite.SetStartTime(const _Value: TDateTime);
begin
  FStartTimeExsit := True;
  FStartTime := _Value;
end;

procedure TEMTFSite.StartTimeRemove;
begin
  if FStartTimeExsit then
  begin
    FStartTimeExsit := False;
  end;
end;

procedure TEMTFSite.AddStartTimeEvent(Sender: TObject);
begin
  AddStartTime;
end;

function TEMTFSite.AddEndTime: TDateTime;
begin;
  Result := FEndTime;
  FEndTimeExsit := True;
end;

procedure TEMTFSite.SetEndTime(const _Value: TDateTime);
begin
  FEndTimeExsit := True;
  FEndTime := _Value;
end;

procedure TEMTFSite.EndTimeRemove;
begin
  if FEndTimeExsit then
  begin
    FEndTimeExsit := False;
  end;
end;

procedure TEMTFSite.AddEndTimeEvent(Sender: TObject);
begin
  AddEndTime;
end;

function TEMTFSite.AddRunList: String;
begin;
  Result := FRunList;
  FRunListExsit := True;
end;

procedure TEMTFSite.SetRunList(const _Value: String);
begin
  FRunListExsit := True;
  FRunList := _Value;
end;

procedure TEMTFSite.RunListRemove;
begin
  if FRunListExsit then
  begin
    FRunListExsit := False;
  end;
end;

procedure TEMTFSite.AddRunListEvent(Sender: TObject);
begin
  AddRunList;
end;

function TEMTFSite.AddLocation: TEMTFLocation;
begin;
  if not FLocationExsit then
    FLocation := TEMTFLocation.Create(Self);
  Result := FLocation;
  FLocationExsit := True;
end;

procedure TEMTFSite.SetLocation(const _Value: TEMTFLocation);
begin
  if FLocationExsit then
    FLocation.Free;
  FLocationExsit := True;
  FLocation := _Value;
  FLocation.Parent := Self;
end;

procedure TEMTFSite.LocationRemove;
begin
  if FLocationExsit then
  begin
    FLocation.Free;
    FLocationExsit := False;
  end;
end;

procedure TEMTFSite.AddLocationEvent(Sender: TObject);
begin
  AddLocation;
  FLocation.ToTree;
end;

function TEMTFSite.AddDataQualityNotes: TEMTFDataQualityNotes;
begin;
  if not FDataQualityNotesExsit then
    FDataQualityNotes := TEMTFDataQualityNotes.Create(Self);
  Result := FDataQualityNotes;
  FDataQualityNotesExsit := True;
end;

procedure TEMTFSite.SetDataQualityNotes(const _Value: TEMTFDataQualityNotes);
begin
  if FDataQualityNotesExsit then
    FDataQualityNotes.Free;
  FDataQualityNotesExsit := True;
  FDataQualityNotes := _Value;
  FDataQualityNotes.Parent := Self;
end;

procedure TEMTFSite.DataQualityNotesRemove;
begin
  if FDataQualityNotesExsit then
  begin
    FDataQualityNotes.Free;
    FDataQualityNotesExsit := False;
  end;
end;

procedure TEMTFSite.AddDataQualityNotesEvent(Sender: TObject);
begin
  AddDataQualityNotes;
  FDataQualityNotes.ToTree;
end;

{  Location}
constructor TEMTFLocation.Create(par: TXML = nil);
begin
  inherited Create(par);
  FLatitude := TEMTFUnitValue.Create(Self);
  FLongitude := TEMTFUnitValue.Create(Self);
  FElevation := TEMTFUnitValue.Create(Self);
  FDeclination := TEMTFDeclination.Create(Self);
end;

destructor TEMTFLocation.Destroy;
begin
  FLatitude.Free;
  FLongitude.Free;
  FElevation.Free;
  FDeclination.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFLocation.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Latitude' then
      begin
        FLatitude := TEMTFUnitValue.Create(Self);
        FLatitude.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'Longitude' then
      begin
        FLongitude := TEMTFUnitValue.Create(Self);
        FLongitude.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'Elevation' then
      begin
        FElevation := TEMTFUnitValue.Create(Self);
        FElevation.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'Declination' then
      begin
        FDeclination := TEMTFDeclination.Create(Self);
        FDeclination.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'datum' then
      begin
        Fdatum := nodeTmp.Text;
        FdatumExsit := True;
      end;
    end;
  except
    raise Exception.Create('Location Read XML Error!' + node.Xml);
  end;
end;

function TEMTFLocation.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  datumTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Location';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FLatitude.ToXML(node, 'Latitude');
    FLongitude.ToXML(node, 'Longitude');
    FElevation.ToXML(node, 'Elevation');
    FDeclination.ToXML(node, 'Declination');
    if FdatumExsit then 
    begin
      datumTmp := doc.CreateNode('datum', ntAttribute);
      datumTmp.NodeValue := Fdatum;
      node.AttributeNodes.Add(datumTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFLocation.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  Latitude.TreeNodeShape := TreeNodeShape.AddChildObject('Latitude', Latitude);
  Latitude.ToTree;
  Longitude.TreeNodeShape := TreeNodeShape.AddChildObject('Longitude', Longitude);
  Longitude.ToTree;
  Elevation.TreeNodeShape := TreeNodeShape.AddChildObject('Elevation', Elevation);
  Elevation.ToTree;
  Declination.TreeNodeShape := TreeNodeShape.AddChildObject('Declination', Declination);
  Declination.ToTree;
  if datumExsit then
    TreeNodeShape.AddChild('datum');
end;

procedure TEMTFLocation.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  datumAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFLocationPop) and Assigned(TEMTFLocationTreeComponent) then
    begin
      TEMTFLocationPop.Clear;
      datumAddMenu := TMenuItem.Create(TEMTFLocationPop);
      datumAddMenu.Text := 'Add datum';
      datumAddMenu.OnClick := AdddatumEvent;
      TEMTFLocationPop.AddObject(datumAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFLocationTreeComponent.ClientToScreen(pt);
      TEMTFLocationPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFLocation.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFLocationXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('datum');
  Types_Value.Add(xs_string);
  _Values_Value.Add(datum);
  TEMTFLocationXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFLocation.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        datum := _Value;
      end;
  end;
  ToTree;
end;

procedure TEMTFLocation.SetLatitude(const _Value: TEMTFUnitValue);
begin
  FLatitude.Free;
  FLatitude := _Value;
  FLatitude.Parent := Self;
end;

procedure TEMTFLocation.SetLongitude(const _Value: TEMTFUnitValue);
begin
  FLongitude.Free;
  FLongitude := _Value;
  FLongitude.Parent := Self;
end;

procedure TEMTFLocation.SetElevation(const _Value: TEMTFUnitValue);
begin
  FElevation.Free;
  FElevation := _Value;
  FElevation.Parent := Self;
end;

procedure TEMTFLocation.SetDeclination(const _Value: TEMTFDeclination);
begin
  FDeclination.Free;
  FDeclination := _Value;
  FDeclination.Parent := Self;
end;

function TEMTFLocation.Adddatum: String;
begin;
  Result := Fdatum;
  FdatumExsit := True;
end;

procedure TEMTFLocation.Setdatum(const _Value: String);
begin
  FdatumExsit := True;
  Fdatum := _Value;
end;

procedure TEMTFLocation.datumRemove;
begin
  if FdatumExsit then
  begin
    FdatumExsit := False;
  end;
end;

procedure TEMTFLocation.AdddatumEvent(Sender: TObject);
begin
  Adddatum;
end;

{  ProcessingInfo}
constructor TEMTFProcessingInfo.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFProcessingInfo.Destroy;
begin
  if FProcessingSoftwareExsit then
    FProcessingSoftware.Free;
  if FRemoteRefExsit then
    FRemoteRef.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFProcessingInfo.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'ProcessingSoftware' then
      begin
        FProcessingSoftware := TEMTFProcessingSoftware.Create(Self);
        FProcessingSoftware.FromXML(nodeTmp);
        FProcessingSoftwareExsit := True;
      end
      else if nodeTmp.NodeName = 'SignConvention' then
      begin
        FSignConvention := nodeTmp.Text;
        FSignConventionExsit := True;
      end
      else if nodeTmp.NodeName = 'ProcessedBy' then
      begin
        FProcessedBy := nodeTmp.Text;
        FProcessedByExsit := True;
      end
      else if nodeTmp.NodeName = 'ProcessingTag' then
      begin
        FProcessingTag := nodeTmp.Text;
        FProcessingTagExsit := True;
      end
      else if nodeTmp.NodeName = 'RemoteRef' then
      begin
        FRemoteRef := TEMTFRemoteRef.Create(Self);
        FRemoteRef.FromXML(nodeTmp);
        FRemoteRefExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('ProcessingInfo Read XML Error!' + node.Xml);
  end;
end;

function TEMTFProcessingInfo.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  SignConventionTmp: IXMLNode;
  ProcessedByTmp: IXMLNode;
  ProcessingTagTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'ProcessingInfo';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FProcessingSoftwareExsit then
      FProcessingSoftware.ToXML(node, 'ProcessingSoftware');
    if FSignConventionExsit then
    begin
      SignConventionTmp := doc.CreateNode('SignConvention', ntElement);
      SignConventionTmp.NodeValue := FSignConvention;
      node.ChildNodes.Add(SignConventionTmp);
    end;
    if FProcessedByExsit then
    begin
      ProcessedByTmp := doc.CreateNode('ProcessedBy', ntElement);
      ProcessedByTmp.NodeValue := FProcessedBy;
      node.ChildNodes.Add(ProcessedByTmp);
    end;
    if FProcessingTagExsit then
    begin
      ProcessingTagTmp := doc.CreateNode('ProcessingTag', ntElement);
      ProcessingTagTmp.NodeValue := FProcessingTag;
      node.ChildNodes.Add(ProcessingTagTmp);
    end;
    if FRemoteRefExsit then
      FRemoteRef.ToXML(node, 'RemoteRef');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFProcessingInfo.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if ProcessingSoftwareExsit then
  begin
    ProcessingSoftware.TreeNodeShape := TreeNodeShape.AddChildObject('ProcessingSoftware', ProcessingSoftware);
    ProcessingSoftware.ToTree;
  end;
  if SignConventionExsit then
    TreeNodeShape.AddChild('SignConvention');
  if ProcessedByExsit then
    TreeNodeShape.AddChild('ProcessedBy');
  if ProcessingTagExsit then
    TreeNodeShape.AddChild('ProcessingTag');
  if RemoteRefExsit then
  begin
    RemoteRef.TreeNodeShape := TreeNodeShape.AddChildObject('RemoteRef', RemoteRef);
    RemoteRef.ToTree;
  end;
end;

procedure TEMTFProcessingInfo.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ProcessingSoftwareAddMenu: TMenuItem;
  SignConventionAddMenu: TMenuItem;
  ProcessedByAddMenu: TMenuItem;
  ProcessingTagAddMenu: TMenuItem;
  RemoteRefAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFProcessingInfoPop) and Assigned(TEMTFProcessingInfoTreeComponent) then
    begin
      TEMTFProcessingInfoPop.Clear;
      ProcessingSoftwareAddMenu := TMenuItem.Create(TEMTFProcessingInfoPop);
      ProcessingSoftwareAddMenu.Text := 'Add ProcessingSoftware';
      ProcessingSoftwareAddMenu.OnClick := AddProcessingSoftwareEvent;
      TEMTFProcessingInfoPop.AddObject(ProcessingSoftwareAddMenu);
      SignConventionAddMenu := TMenuItem.Create(TEMTFProcessingInfoPop);
      SignConventionAddMenu.Text := 'Add SignConvention';
      SignConventionAddMenu.OnClick := AddSignConventionEvent;
      TEMTFProcessingInfoPop.AddObject(SignConventionAddMenu);
      ProcessedByAddMenu := TMenuItem.Create(TEMTFProcessingInfoPop);
      ProcessedByAddMenu.Text := 'Add ProcessedBy';
      ProcessedByAddMenu.OnClick := AddProcessedByEvent;
      TEMTFProcessingInfoPop.AddObject(ProcessedByAddMenu);
      ProcessingTagAddMenu := TMenuItem.Create(TEMTFProcessingInfoPop);
      ProcessingTagAddMenu.Text := 'Add ProcessingTag';
      ProcessingTagAddMenu.OnClick := AddProcessingTagEvent;
      TEMTFProcessingInfoPop.AddObject(ProcessingTagAddMenu);
      RemoteRefAddMenu := TMenuItem.Create(TEMTFProcessingInfoPop);
      RemoteRefAddMenu.Text := 'Add RemoteRef';
      RemoteRefAddMenu.OnClick := AddRemoteRefEvent;
      TEMTFProcessingInfoPop.AddObject(RemoteRefAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFProcessingInfoTreeComponent.ClientToScreen(pt);
      TEMTFProcessingInfoPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFProcessingInfo.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFProcessingInfoXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('SignConvention');
  Types_Value.Add(xs_string);
  _Values_Value.Add(SignConvention);
  Names_Value.Add('ProcessedBy');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ProcessedBy);
  Names_Value.Add('ProcessingTag');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ProcessingTag);
  TEMTFProcessingInfoXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFProcessingInfo.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        SignConvention := _Value;
      end;
    1:
      begin
        ProcessedBy := _Value;
      end;
    2:
      begin
        ProcessingTag := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFProcessingInfo.AddProcessingSoftware: TEMTFProcessingSoftware;
begin;
  if not FProcessingSoftwareExsit then
    FProcessingSoftware := TEMTFProcessingSoftware.Create(Self);
  Result := FProcessingSoftware;
  FProcessingSoftwareExsit := True;
end;

procedure TEMTFProcessingInfo.SetProcessingSoftware(const _Value: TEMTFProcessingSoftware);
begin
  if FProcessingSoftwareExsit then
    FProcessingSoftware.Free;
  FProcessingSoftwareExsit := True;
  FProcessingSoftware := _Value;
  FProcessingSoftware.Parent := Self;
end;

procedure TEMTFProcessingInfo.ProcessingSoftwareRemove;
begin
  if FProcessingSoftwareExsit then
  begin
    FProcessingSoftware.Free;
    FProcessingSoftwareExsit := False;
  end;
end;

procedure TEMTFProcessingInfo.AddProcessingSoftwareEvent(Sender: TObject);
begin
  AddProcessingSoftware;
  FProcessingSoftware.ToTree;
end;

function TEMTFProcessingInfo.AddSignConvention: String;
begin;
  Result := FSignConvention;
  FSignConventionExsit := True;
end;

procedure TEMTFProcessingInfo.SetSignConvention(const _Value: String);
begin
  FSignConventionExsit := True;
  FSignConvention := _Value;
end;

procedure TEMTFProcessingInfo.SignConventionRemove;
begin
  if FSignConventionExsit then
  begin
    FSignConventionExsit := False;
  end;
end;

procedure TEMTFProcessingInfo.AddSignConventionEvent(Sender: TObject);
begin
  AddSignConvention;
end;

function TEMTFProcessingInfo.AddProcessedBy: String;
begin;
  Result := FProcessedBy;
  FProcessedByExsit := True;
end;

procedure TEMTFProcessingInfo.SetProcessedBy(const _Value: String);
begin
  FProcessedByExsit := True;
  FProcessedBy := _Value;
end;

procedure TEMTFProcessingInfo.ProcessedByRemove;
begin
  if FProcessedByExsit then
  begin
    FProcessedByExsit := False;
  end;
end;

procedure TEMTFProcessingInfo.AddProcessedByEvent(Sender: TObject);
begin
  AddProcessedBy;
end;

function TEMTFProcessingInfo.AddProcessingTag: String;
begin;
  Result := FProcessingTag;
  FProcessingTagExsit := True;
end;

procedure TEMTFProcessingInfo.SetProcessingTag(const _Value: String);
begin
  FProcessingTagExsit := True;
  FProcessingTag := _Value;
end;

procedure TEMTFProcessingInfo.ProcessingTagRemove;
begin
  if FProcessingTagExsit then
  begin
    FProcessingTagExsit := False;
  end;
end;

procedure TEMTFProcessingInfo.AddProcessingTagEvent(Sender: TObject);
begin
  AddProcessingTag;
end;

function TEMTFProcessingInfo.AddRemoteRef: TEMTFRemoteRef;
begin;
  if not FRemoteRefExsit then
    FRemoteRef := TEMTFRemoteRef.Create(Self);
  Result := FRemoteRef;
  FRemoteRefExsit := True;
end;

procedure TEMTFProcessingInfo.SetRemoteRef(const _Value: TEMTFRemoteRef);
begin
  if FRemoteRefExsit then
    FRemoteRef.Free;
  FRemoteRefExsit := True;
  FRemoteRef := _Value;
  FRemoteRef.Parent := Self;
end;

procedure TEMTFProcessingInfo.RemoteRefRemove;
begin
  if FRemoteRefExsit then
  begin
    FRemoteRef.Free;
    FRemoteRefExsit := False;
  end;
end;

procedure TEMTFProcessingInfo.AddRemoteRefEvent(Sender: TObject);
begin
  AddRemoteRef;
  FRemoteRef.ToTree;
end;

{  ProcessingSoftware}
constructor TEMTFProcessingSoftware.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFProcessingSoftware.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFProcessingSoftware.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'LastMod' then
      begin
        FLastMod := nodeTmp.Text;
        FLastModExsit := True;
      end
      else if nodeTmp.NodeName = 'Author' then
      begin
        FAuthor := nodeTmp.Text;
        FAuthorExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('ProcessingSoftware Read XML Error!' + node.Xml);
  end;
end;

function TEMTFProcessingSoftware.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  LastModTmp: IXMLNode;
  AuthorTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'ProcessingSoftware';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('Name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FLastModExsit then
    begin
      LastModTmp := doc.CreateNode('LastMod', ntElement);
      LastModTmp.NodeValue := FLastMod;
      node.ChildNodes.Add(LastModTmp);
    end;
    if FAuthorExsit then
    begin
      AuthorTmp := doc.CreateNode('Author', ntElement);
      AuthorTmp.NodeValue := FAuthor;
      node.ChildNodes.Add(AuthorTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFProcessingSoftware.ToTree;
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
  if LastModExsit then
    TreeNodeShape.AddChild('LastMod');
  if AuthorExsit then
    TreeNodeShape.AddChild('Author');
end;

procedure TEMTFProcessingSoftware.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  LastModAddMenu: TMenuItem;
  AuthorAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFProcessingSoftwarePop) and Assigned(TEMTFProcessingSoftwareTreeComponent) then
    begin
      TEMTFProcessingSoftwarePop.Clear;
      NameAddMenu := TMenuItem.Create(TEMTFProcessingSoftwarePop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFProcessingSoftwarePop.AddObject(NameAddMenu);
      LastModAddMenu := TMenuItem.Create(TEMTFProcessingSoftwarePop);
      LastModAddMenu.Text := 'Add LastMod';
      LastModAddMenu.OnClick := AddLastModEvent;
      TEMTFProcessingSoftwarePop.AddObject(LastModAddMenu);
      AuthorAddMenu := TMenuItem.Create(TEMTFProcessingSoftwarePop);
      AuthorAddMenu.Text := 'Add Author';
      AuthorAddMenu.OnClick := AddAuthorEvent;
      TEMTFProcessingSoftwarePop.AddObject(AuthorAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFProcessingSoftwareTreeComponent.ClientToScreen(pt);
      TEMTFProcessingSoftwarePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFProcessingSoftware.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFProcessingSoftwareXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('LastMod');
  Types_Value.Add(xs_string);
  _Values_Value.Add(LastMod);
  Names_Value.Add('Author');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Author);
  TEMTFProcessingSoftwareXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFProcessingSoftware.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        LastMod := _Value;
      end;
    2:
      begin
        Author := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFProcessingSoftware.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFProcessingSoftware.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFProcessingSoftware.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFProcessingSoftware.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TEMTFProcessingSoftware.AddLastMod: String;
begin;
  Result := FLastMod;
  FLastModExsit := True;
end;

procedure TEMTFProcessingSoftware.SetLastMod(const _Value: String);
begin
  FLastModExsit := True;
  FLastMod := _Value;
end;

procedure TEMTFProcessingSoftware.LastModRemove;
begin
  if FLastModExsit then
  begin
    FLastModExsit := False;
  end;
end;

procedure TEMTFProcessingSoftware.AddLastModEvent(Sender: TObject);
begin
  AddLastMod;
end;

function TEMTFProcessingSoftware.AddAuthor: String;
begin;
  Result := FAuthor;
  FAuthorExsit := True;
end;

procedure TEMTFProcessingSoftware.SetAuthor(const _Value: String);
begin
  FAuthorExsit := True;
  FAuthor := _Value;
end;

procedure TEMTFProcessingSoftware.AuthorRemove;
begin
  if FAuthorExsit then
  begin
    FAuthorExsit := False;
  end;
end;

procedure TEMTFProcessingSoftware.AddAuthorEvent(Sender: TObject);
begin
  AddAuthor;
end;

{  Instrument}
constructor TEMTFInstrument.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFInstrument.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFInstrument.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Manufacturer' then
      begin
        FManufacturer := nodeTmp.Text;
        FManufacturerExsit := True;
      end
      else if nodeTmp.NodeName = 'Name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'Id' then
      begin
        FId := nodeTmp.Text;
        FIdExsit := True;
      end
      else if nodeTmp.NodeName = 'Settings' then
      begin
        FSettings := nodeTmp.Text;
        FSettingsExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'type' then
      begin
        FMagentometertype := nodeTmp.Text;
        FMagentometertypeExsit := True;
      end;
    end;
  except
    raise Exception.Create('Instrument Read XML Error!' + node.Xml);
  end;
end;

function TEMTFInstrument.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ManufacturerTmp: IXMLNode;
  NameTmp: IXMLNode;
  IdTmp: IXMLNode;
  SettingsTmp: IXMLNode;
  MagentometertypeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Instrument';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FManufacturerExsit then
    begin
      ManufacturerTmp := doc.CreateNode('Manufacturer', ntElement);
      ManufacturerTmp.NodeValue := FManufacturer;
      node.ChildNodes.Add(ManufacturerTmp);
    end;
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('Name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FIdExsit then
    begin
      IdTmp := doc.CreateNode('Id', ntElement);
      IdTmp.NodeValue := FId;
      node.ChildNodes.Add(IdTmp);
    end;
    if FSettingsExsit then
    begin
      SettingsTmp := doc.CreateNode('Settings', ntElement);
      SettingsTmp.NodeValue := FSettings;
      node.ChildNodes.Add(SettingsTmp);
    end;
    if FMagentometertypeExsit then 
    begin
      MagentometertypeTmp := doc.CreateNode('type', ntAttribute);
      MagentometertypeTmp.NodeValue := FMagentometertype;
      node.AttributeNodes.Add(MagentometertypeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFInstrument.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if ManufacturerExsit then
    TreeNodeShape.AddChild('Manufacturer');
  if NameExsit then
    TreeNodeShape.AddChild('Name');
  if IdExsit then
    TreeNodeShape.AddChild('Id');
  if SettingsExsit then
    TreeNodeShape.AddChild('Settings');
  if MagentometertypeExsit then
    TreeNodeShape.AddChild('Magentometertype');
end;

procedure TEMTFInstrument.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ManufacturerAddMenu: TMenuItem;
  NameAddMenu: TMenuItem;
  IdAddMenu: TMenuItem;
  SettingsAddMenu: TMenuItem;
  MagentometertypeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFInstrumentPop) and Assigned(TEMTFInstrumentTreeComponent) then
    begin
      TEMTFInstrumentPop.Clear;
      ManufacturerAddMenu := TMenuItem.Create(TEMTFInstrumentPop);
      ManufacturerAddMenu.Text := 'Add Manufacturer';
      ManufacturerAddMenu.OnClick := AddManufacturerEvent;
      TEMTFInstrumentPop.AddObject(ManufacturerAddMenu);
      NameAddMenu := TMenuItem.Create(TEMTFInstrumentPop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFInstrumentPop.AddObject(NameAddMenu);
      IdAddMenu := TMenuItem.Create(TEMTFInstrumentPop);
      IdAddMenu.Text := 'Add Id';
      IdAddMenu.OnClick := AddIdEvent;
      TEMTFInstrumentPop.AddObject(IdAddMenu);
      SettingsAddMenu := TMenuItem.Create(TEMTFInstrumentPop);
      SettingsAddMenu.Text := 'Add Settings';
      SettingsAddMenu.OnClick := AddSettingsEvent;
      TEMTFInstrumentPop.AddObject(SettingsAddMenu);
      MagentometertypeAddMenu := TMenuItem.Create(TEMTFInstrumentPop);
      MagentometertypeAddMenu.Text := 'Add Magentometertype';
      MagentometertypeAddMenu.OnClick := AddMagentometertypeEvent;
      TEMTFInstrumentPop.AddObject(MagentometertypeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFInstrumentTreeComponent.ClientToScreen(pt);
      TEMTFInstrumentPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFInstrument.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFInstrumentXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Manufacturer');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Manufacturer);
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Id');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Id);
  Names_Value.Add('Settings');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Settings);
  Names_Value.Add('Magentometertype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Magentometertype);
  TEMTFInstrumentXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFInstrument.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Manufacturer := _Value;
      end;
    1:
      begin
        Name := _Value;
      end;
    2:
      begin
        Id := _Value;
      end;
    3:
      begin
        Settings := _Value;
      end;
    4:
      begin
        Magentometertype := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFInstrument.AddManufacturer: String;
begin;
  Result := FManufacturer;
  FManufacturerExsit := True;
end;

procedure TEMTFInstrument.SetManufacturer(const _Value: String);
begin
  FManufacturerExsit := True;
  FManufacturer := _Value;
end;

procedure TEMTFInstrument.ManufacturerRemove;
begin
  if FManufacturerExsit then
  begin
    FManufacturerExsit := False;
  end;
end;

procedure TEMTFInstrument.AddManufacturerEvent(Sender: TObject);
begin
  AddManufacturer;
end;

function TEMTFInstrument.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFInstrument.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFInstrument.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFInstrument.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TEMTFInstrument.AddId: String;
begin;
  Result := FId;
  FIdExsit := True;
end;

procedure TEMTFInstrument.SetId(const _Value: String);
begin
  FIdExsit := True;
  FId := _Value;
end;

procedure TEMTFInstrument.IdRemove;
begin
  if FIdExsit then
  begin
    FIdExsit := False;
  end;
end;

procedure TEMTFInstrument.AddIdEvent(Sender: TObject);
begin
  AddId;
end;

function TEMTFInstrument.AddSettings: String;
begin;
  Result := FSettings;
  FSettingsExsit := True;
end;

procedure TEMTFInstrument.SetSettings(const _Value: String);
begin
  FSettingsExsit := True;
  FSettings := _Value;
end;

procedure TEMTFInstrument.SettingsRemove;
begin
  if FSettingsExsit then
  begin
    FSettingsExsit := False;
  end;
end;

procedure TEMTFInstrument.AddSettingsEvent(Sender: TObject);
begin
  AddSettings;
end;

function TEMTFInstrument.AddMagentometertype: String;
begin;
  Result := FMagentometertype;
  FMagentometertypeExsit := True;
end;

procedure TEMTFInstrument.SetMagentometertype(const _Value: String);
begin
  FMagentometertypeExsit := True;
  FMagentometertype := _Value;
end;

procedure TEMTFInstrument.MagentometertypeRemove;
begin
  if FMagentometertypeExsit then
  begin
    FMagentometertypeExsit := False;
  end;
end;

procedure TEMTFInstrument.AddMagentometertypeEvent(Sender: TObject);
begin
  AddMagentometertype;
end;

{  Dipole}
constructor TEMTFDipole.Create(par: TXML = nil);
begin
  inherited Create(par);
  FElectrodes := TList<TEMTFElectrode>.Create;
end;

destructor TEMTFDipole.Destroy;
begin
  if FLengthExsit then
    FLength.Free;
  if FAzimuthExsit then
    FAzimuth.Free;
  ElectrodeClear;
  FElectrodes.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFDipole.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ElectrodeTmp: TEMTFElectrode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Manufacturer' then
      begin
        FManufacturer := nodeTmp.Text;
        FManufacturerExsit := True;
      end
      else if nodeTmp.NodeName = 'Length' then
      begin
        FLength := TEMTFUnitValue.Create(Self);
        FLength.FromXML(nodeTmp);
        FLengthExsit := True;
      end
      else if nodeTmp.NodeName = 'Azimuth' then
      begin
        FAzimuth := TEMTFUnitValue.Create(Self);
        FAzimuth.FromXML(nodeTmp);
        FAzimuthExsit := True;
      end
      else if nodeTmp.NodeName = 'Electrode' then
      begin
        ElectrodeTmp := TEMTFElectrode.Create(Self);
        ElectrodeTmp.FromXML(nodeTmp);
        FElectrodes.Add(ElectrodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        FDipoletype := nodeTmp.Text;
        FDipoletypeExsit := True;
      end;
    end;
  except
    raise Exception.Create('Dipole Read XML Error!' + node.Xml);
  end;
end;

function TEMTFDipole.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  DipoletypeTmp: IXMLNode;
  ManufacturerTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Dipole';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FManufacturerExsit then
    begin
      ManufacturerTmp := doc.CreateNode('Manufacturer', ntElement);
      ManufacturerTmp.NodeValue := FManufacturer;
      node.ChildNodes.Add(ManufacturerTmp);
    end;
    if FLengthExsit then
      FLength.ToXML(node, 'Length');
    if FAzimuthExsit then
      FAzimuth.ToXML(node, 'Azimuth');
    for I := 0 to FElectrodes.Count - 1 do
       FElectrodes.Items[I].ToXML(node, 'Electrode');
    if FnameExsit then 
    begin
      nameTmp := doc.CreateNode('name', ntAttribute);
      nameTmp.NodeValue := Fname;
      node.AttributeNodes.Add(nameTmp);
    end;
    if FDipoletypeExsit then 
    begin
      DipoletypeTmp := doc.CreateNode('type', ntAttribute);
      DipoletypeTmp.NodeValue := FDipoletype;
      node.AttributeNodes.Add(DipoletypeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFDipole.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if DipoletypeExsit then
    TreeNodeShape.AddChild('Dipoletype');
  if ManufacturerExsit then
    TreeNodeShape.AddChild('Manufacturer');
  if LengthExsit then
  begin
    Length.TreeNodeShape := TreeNodeShape.AddChildObject('Length', Length);
    Length.ToTree;
  end;
  if AzimuthExsit then
  begin
    Azimuth.TreeNodeShape := TreeNodeShape.AddChildObject('Azimuth', Azimuth);
    Azimuth.ToTree;
  end;
  for I := 0 to ElectrodeCount - 1 do
  begin
    Electrodes[I].TreeNodeShape := TreeNodeShape.AddChildObject('Electrode', Electrode[I]);
    Electrode[I].ToTree;
  end;
end;

procedure TEMTFDipole.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  nameAddMenu: TMenuItem;
  DipoletypeAddMenu: TMenuItem;
  ManufacturerAddMenu: TMenuItem;
  LengthAddMenu: TMenuItem;
  AzimuthAddMenu: TMenuItem;
  ElectrodeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDipolePop) and Assigned(TEMTFDipoleTreeComponent) then
    begin
      TEMTFDipolePop.Clear;
      nameAddMenu := TMenuItem.Create(TEMTFDipolePop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TEMTFDipolePop.AddObject(nameAddMenu);
      DipoletypeAddMenu := TMenuItem.Create(TEMTFDipolePop);
      DipoletypeAddMenu.Text := 'Add Dipoletype';
      DipoletypeAddMenu.OnClick := AddDipoletypeEvent;
      TEMTFDipolePop.AddObject(DipoletypeAddMenu);
      ManufacturerAddMenu := TMenuItem.Create(TEMTFDipolePop);
      ManufacturerAddMenu.Text := 'Add Manufacturer';
      ManufacturerAddMenu.OnClick := AddManufacturerEvent;
      TEMTFDipolePop.AddObject(ManufacturerAddMenu);
      LengthAddMenu := TMenuItem.Create(TEMTFDipolePop);
      LengthAddMenu.Text := 'Add Length';
      LengthAddMenu.OnClick := AddLengthEvent;
      TEMTFDipolePop.AddObject(LengthAddMenu);
      AzimuthAddMenu := TMenuItem.Create(TEMTFDipolePop);
      AzimuthAddMenu.Text := 'Add Azimuth';
      AzimuthAddMenu.OnClick := AddAzimuthEvent;
      TEMTFDipolePop.AddObject(AzimuthAddMenu);
      ElectrodeAddMenu := TMenuItem.Create(TEMTFDipolePop);
      ElectrodeAddMenu.Text := 'Add Electrode';
      ElectrodeAddMenu.OnClick := AddElectrodeEvent;
      TEMTFDipolePop.AddObject(ElectrodeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFDipoleTreeComponent.ClientToScreen(pt);
      TEMTFDipolePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFDipole.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDipoleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('Dipoletype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Dipoletype);
  Names_Value.Add('Manufacturer');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Manufacturer);
  TEMTFDipoleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFDipole.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        Dipoletype := _Value;
      end;
    2:
      begin
        Manufacturer := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFDipole.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TEMTFDipole.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TEMTFDipole.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TEMTFDipole.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TEMTFDipole.AddDipoletype: String;
begin;
  Result := FDipoletype;
  FDipoletypeExsit := True;
end;

procedure TEMTFDipole.SetDipoletype(const _Value: String);
begin
  FDipoletypeExsit := True;
  FDipoletype := _Value;
end;

procedure TEMTFDipole.DipoletypeRemove;
begin
  if FDipoletypeExsit then
  begin
    FDipoletypeExsit := False;
  end;
end;

procedure TEMTFDipole.AddDipoletypeEvent(Sender: TObject);
begin
  AddDipoletype;
end;

function TEMTFDipole.AddManufacturer: String;
begin;
  Result := FManufacturer;
  FManufacturerExsit := True;
end;

procedure TEMTFDipole.SetManufacturer(const _Value: String);
begin
  FManufacturerExsit := True;
  FManufacturer := _Value;
end;

procedure TEMTFDipole.ManufacturerRemove;
begin
  if FManufacturerExsit then
  begin
    FManufacturerExsit := False;
  end;
end;

procedure TEMTFDipole.AddManufacturerEvent(Sender: TObject);
begin
  AddManufacturer;
end;

function TEMTFDipole.AddLength: TEMTFUnitValue;
begin;
  if not FLengthExsit then
    FLength := TEMTFUnitValue.Create(Self);
  Result := FLength;
  FLengthExsit := True;
end;

procedure TEMTFDipole.SetLength(const _Value: TEMTFUnitValue);
begin
  if FLengthExsit then
    FLength.Free;
  FLengthExsit := True;
  FLength := _Value;
  FLength.Parent := Self;
end;

procedure TEMTFDipole.LengthRemove;
begin
  if FLengthExsit then
  begin
    FLength.Free;
    FLengthExsit := False;
  end;
end;

procedure TEMTFDipole.AddLengthEvent(Sender: TObject);
begin
  AddLength;
  FLength.ToTree;
end;

function TEMTFDipole.AddAzimuth: TEMTFUnitValue;
begin;
  if not FAzimuthExsit then
    FAzimuth := TEMTFUnitValue.Create(Self);
  Result := FAzimuth;
  FAzimuthExsit := True;
end;

procedure TEMTFDipole.SetAzimuth(const _Value: TEMTFUnitValue);
begin
  if FAzimuthExsit then
    FAzimuth.Free;
  FAzimuthExsit := True;
  FAzimuth := _Value;
  FAzimuth.Parent := Self;
end;

procedure TEMTFDipole.AzimuthRemove;
begin
  if FAzimuthExsit then
  begin
    FAzimuth.Free;
    FAzimuthExsit := False;
  end;
end;

procedure TEMTFDipole.AddAzimuthEvent(Sender: TObject);
begin
  AddAzimuth;
  FAzimuth.ToTree;
end;

function TEMTFDipole.AddElectrode: TEMTFElectrode;
var
  Electrodetmp: TEMTFElectrode;
begin;
  Electrodetmp := TEMTFElectrode.Create(Self);
  FElectrodes.Add(Electrodetmp);
  Result := Electrodetmp;
end;

procedure TEMTFDipole.SetElectrodes(const _Value: TList<TEMTFElectrode>);
begin
  ElectrodeClear;
  FElectrodes := _Value;
end;

procedure TEMTFDipole.ElectrodeClear;
begin
  while FElectrodes.Count > 0 do
  begin
    FElectrodes.Items[0].Free;
    FElectrodes.Delete(0);
  end;
end;

function TEMTFDipole.ElectrodeCount: Integer;
begin
  Result := FElectrodes.Count;
end;

function TEMTFDipole.GetElectrode(Index: Integer): TEMTFElectrode;
begin
  Result := FElectrodes[Index];
end;

procedure TEMTFDipole.SetElectrode(Index: Integer;
  const _Value: TEMTFElectrode);
begin
  _Value.Parent := Self;
  FElectrodes[Index].Free;
  FElectrodes[Index] := _Value;
end;

procedure TEMTFDipole.RemoveElectrode(_Value: TEMTFElectrode);
begin
  FElectrodes.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFDipole.DeleteElectrode(Index: Integer);
begin
  FElectrodes.Items[Index].Free;
  FElectrodes.Delete(Index);
end;

procedure TEMTFDipole.AddElectrodeEvent(Sender: TObject);
var
  tmp: TEMTFElectrode;
begin
  tmp := AddElectrode;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Electrode', tmp);
  tmp.ToTree;
end;

{  DataTypes}
constructor TEMTFDataTypes.Create(par: TXML = nil);
begin
  inherited Create(par);
  FDataTypes := TList<TEMTFDateType>.Create;
end;

destructor TEMTFDataTypes.Destroy;
begin
  DataTypeClear;
  FDataTypes.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFDataTypes.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  DataTypeTmp: TEMTFDateType;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'DataType' then
      begin
        DataTypeTmp := TEMTFDateType.Create(Self);
        DataTypeTmp.FromXML(nodeTmp);
        FDataTypes.Add(DataTypeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('DataTypes Read XML Error!' + node.Xml);
  end;
end;

function TEMTFDataTypes.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'DataTypes';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FDataTypes.Count - 1 do
       FDataTypes.Items[I].ToXML(node, 'DataType');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFDataTypes.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to DataTypeCount - 1 do
  begin
    DataTypes[I].TreeNodeShape := TreeNodeShape.AddChildObject('DataType', DataType[I]);
    DataType[I].ToTree;
  end;
end;

procedure TEMTFDataTypes.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  DataTypeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDataTypesPop) and Assigned(TEMTFDataTypesTreeComponent) then
    begin
      TEMTFDataTypesPop.Clear;
      DataTypeAddMenu := TMenuItem.Create(TEMTFDataTypesPop);
      DataTypeAddMenu.Text := 'Add DataType';
      DataTypeAddMenu.OnClick := AddDataTypeEvent;
      TEMTFDataTypesPop.AddObject(DataTypeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFDataTypesTreeComponent.ClientToScreen(pt);
      TEMTFDataTypesPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFDataTypes.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDataTypesXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TEMTFDataTypesXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFDataTypes.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

function TEMTFDataTypes.AddDataType: TEMTFDateType;
var
  DataTypetmp: TEMTFDateType;
begin;
  DataTypetmp := TEMTFDateType.Create(Self);
  FDataTypes.Add(DataTypetmp);
  Result := DataTypetmp;
end;

procedure TEMTFDataTypes.SetDataTypes(const _Value: TList<TEMTFDateType>);
begin
  DataTypeClear;
  FDataTypes := _Value;
end;

procedure TEMTFDataTypes.DataTypeClear;
begin
  while FDataTypes.Count > 0 do
  begin
    FDataTypes.Items[0].Free;
    FDataTypes.Delete(0);
  end;
end;

function TEMTFDataTypes.DataTypeCount: Integer;
begin
  Result := FDataTypes.Count;
end;

function TEMTFDataTypes.GetDataType(Index: Integer): TEMTFDateType;
begin
  Result := FDataTypes[Index];
end;

procedure TEMTFDataTypes.SetDataType(Index: Integer;
  const _Value: TEMTFDateType);
begin
  _Value.Parent := Self;
  FDataTypes[Index].Free;
  FDataTypes[Index] := _Value;
end;

procedure TEMTFDataTypes.RemoveDataType(_Value: TEMTFDateType);
begin
  FDataTypes.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFDataTypes.DeleteDataType(Index: Integer);
begin
  FDataTypes.Items[Index].Free;
  FDataTypes.Delete(Index);
end;

procedure TEMTFDataTypes.AddDataTypeEvent(Sender: TObject);
var
  tmp: TEMTFDateType;
begin
  tmp := AddDataType;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('DataType', tmp);
  tmp.ToTree;
end;

{  type}
constructor TEMTFDateType.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFDateType.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFDateType.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'ExternalUrl' then
      begin
        FExternalUrl := nodeTmp.Text;
        FExternalUrlExsit := True;
      end
      else if nodeTmp.NodeName = 'Intention' then
      begin
        FIntention := nodeTmp.Text;
        FIntentionExsit := True;
      end
      else if nodeTmp.NodeName = 'Tag' then
      begin
        FTag := nodeTmp.Text;
        FTagExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        Fztype := nodeTmp.Text;
        FztypeExsit := True;
      end
      else if nodeTmp.NodeName = 'output' then
      begin
        Foutput := nodeTmp.Text;
        FoutputExsit := True;
      end
      else if nodeTmp.NodeName = 'input' then
      begin
        Finput := nodeTmp.Text;
        FinputExsit := True;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    end;
  except
    raise Exception.Create('type Read XML Error!' + node.Xml);
  end;
end;

function TEMTFDateType.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  ztypeTmp: IXMLNode;
  outputTmp: IXMLNode;
  inputTmp: IXMLNode;
  unitsTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  ExternalUrlTmp: IXMLNode;
  IntentionTmp: IXMLNode;
  TagTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'type';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('Description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FExternalUrlExsit then
    begin
      ExternalUrlTmp := doc.CreateNode('ExternalUrl', ntElement);
      ExternalUrlTmp.NodeValue := FExternalUrl;
      node.ChildNodes.Add(ExternalUrlTmp);
    end;
    if FIntentionExsit then
    begin
      IntentionTmp := doc.CreateNode('Intention', ntElement);
      IntentionTmp.NodeValue := FIntention;
      node.ChildNodes.Add(IntentionTmp);
    end;
    if FTagExsit then
    begin
      TagTmp := doc.CreateNode('Tag', ntElement);
      TagTmp.NodeValue := FTag;
      node.ChildNodes.Add(TagTmp);
    end;
    if FnameExsit then 
    begin
      nameTmp := doc.CreateNode('name', ntAttribute);
      nameTmp.NodeValue := Fname;
      node.AttributeNodes.Add(nameTmp);
    end;
    if FztypeExsit then 
    begin
      ztypeTmp := doc.CreateNode('type', ntAttribute);
      ztypeTmp.NodeValue := Fztype;
      node.AttributeNodes.Add(ztypeTmp);
    end;
    if FoutputExsit then 
    begin
      outputTmp := doc.CreateNode('output', ntAttribute);
      outputTmp.NodeValue := Foutput;
      node.AttributeNodes.Add(outputTmp);
    end;
    if FinputExsit then 
    begin
      inputTmp := doc.CreateNode('input', ntAttribute);
      inputTmp.NodeValue := Finput;
      node.AttributeNodes.Add(inputTmp);
    end;
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFDateType.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if ztypeExsit then
    TreeNodeShape.AddChild('ztype');
  if outputExsit then
    TreeNodeShape.AddChild('output');
  if inputExsit then
    TreeNodeShape.AddChild('input');
  if unitsExsit then
    TreeNodeShape.AddChild('units');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if ExternalUrlExsit then
    TreeNodeShape.AddChild('ExternalUrl');
  if IntentionExsit then
    TreeNodeShape.AddChild('Intention');
  if TagExsit then
    TreeNodeShape.AddChild('Tag');
end;

procedure TEMTFDateType.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  nameAddMenu: TMenuItem;
  ztypeAddMenu: TMenuItem;
  outputAddMenu: TMenuItem;
  inputAddMenu: TMenuItem;
  unitsAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  ExternalUrlAddMenu: TMenuItem;
  IntentionAddMenu: TMenuItem;
  TagAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDateTypePop) and Assigned(TEMTFDateTypeTreeComponent) then
    begin
      TEMTFDateTypePop.Clear;
      nameAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TEMTFDateTypePop.AddObject(nameAddMenu);
      ztypeAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      ztypeAddMenu.Text := 'Add ztype';
      ztypeAddMenu.OnClick := AddztypeEvent;
      TEMTFDateTypePop.AddObject(ztypeAddMenu);
      outputAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      outputAddMenu.Text := 'Add output';
      outputAddMenu.OnClick := AddoutputEvent;
      TEMTFDateTypePop.AddObject(outputAddMenu);
      inputAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      inputAddMenu.Text := 'Add input';
      inputAddMenu.OnClick := AddinputEvent;
      TEMTFDateTypePop.AddObject(inputAddMenu);
      unitsAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFDateTypePop.AddObject(unitsAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      DescriptionAddMenu.Text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TEMTFDateTypePop.AddObject(DescriptionAddMenu);
      ExternalUrlAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      ExternalUrlAddMenu.Text := 'Add ExternalUrl';
      ExternalUrlAddMenu.OnClick := AddExternalUrlEvent;
      TEMTFDateTypePop.AddObject(ExternalUrlAddMenu);
      IntentionAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      IntentionAddMenu.Text := 'Add Intention';
      IntentionAddMenu.OnClick := AddIntentionEvent;
      TEMTFDateTypePop.AddObject(IntentionAddMenu);
      TagAddMenu := TMenuItem.Create(TEMTFDateTypePop);
      TagAddMenu.Text := 'Add Tag';
      TagAddMenu.OnClick := AddTagEvent;
      TEMTFDateTypePop.AddObject(TagAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFDateTypeTreeComponent.ClientToScreen(pt);
      TEMTFDateTypePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFDateType.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDateTypeXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('ztype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ztype);
  Names_Value.Add('output');
  Types_Value.Add(xs_string);
  _Values_Value.Add(output);
  Names_Value.Add('input');
  Types_Value.Add(xs_string);
  _Values_Value.Add(input);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('ExternalUrl');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ExternalUrl);
  Names_Value.Add('Intention');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Intention);
  Names_Value.Add('Tag');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Tag);
  TEMTFDateTypeXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFDateType.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        ztype := _Value;
      end;
    2:
      begin
        output := _Value;
      end;
    3:
      begin
        input := _Value;
      end;
    4:
      begin
        units := _Value;
      end;
    5:
      begin
        Description := _Value;
      end;
    6:
      begin
        ExternalUrl := _Value;
      end;
    7:
      begin
        Intention := _Value;
      end;
    8:
      begin
        Tag := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFDateType.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TEMTFDateType.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TEMTFDateType.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TEMTFDateType.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TEMTFDateType.Addztype: String;
begin;
  Result := Fztype;
  FztypeExsit := True;
end;

procedure TEMTFDateType.Setztype(const _Value: String);
begin
  FztypeExsit := True;
  Fztype := _Value;
end;

procedure TEMTFDateType.ztypeRemove;
begin
  if FztypeExsit then
  begin
    FztypeExsit := False;
  end;
end;

procedure TEMTFDateType.AddztypeEvent(Sender: TObject);
begin
  Addztype;
end;

function TEMTFDateType.Addoutput: String;
begin;
  Result := Foutput;
  FoutputExsit := True;
end;

procedure TEMTFDateType.Setoutput(const _Value: String);
begin
  FoutputExsit := True;
  Foutput := _Value;
end;

procedure TEMTFDateType.outputRemove;
begin
  if FoutputExsit then
  begin
    FoutputExsit := False;
  end;
end;

procedure TEMTFDateType.AddoutputEvent(Sender: TObject);
begin
  Addoutput;
end;

function TEMTFDateType.Addinput: String;
begin;
  Result := Finput;
  FinputExsit := True;
end;

procedure TEMTFDateType.Setinput(const _Value: String);
begin
  FinputExsit := True;
  Finput := _Value;
end;

procedure TEMTFDateType.inputRemove;
begin
  if FinputExsit then
  begin
    FinputExsit := False;
  end;
end;

procedure TEMTFDateType.AddinputEvent(Sender: TObject);
begin
  Addinput;
end;

function TEMTFDateType.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFDateType.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFDateType.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFDateType.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

function TEMTFDateType.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TEMTFDateType.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TEMTFDateType.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TEMTFDateType.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TEMTFDateType.AddExternalUrl: String;
begin;
  Result := FExternalUrl;
  FExternalUrlExsit := True;
end;

procedure TEMTFDateType.SetExternalUrl(const _Value: String);
begin
  FExternalUrlExsit := True;
  FExternalUrl := _Value;
end;

procedure TEMTFDateType.ExternalUrlRemove;
begin
  if FExternalUrlExsit then
  begin
    FExternalUrlExsit := False;
  end;
end;

procedure TEMTFDateType.AddExternalUrlEvent(Sender: TObject);
begin
  AddExternalUrl;
end;

function TEMTFDateType.AddIntention: String;
begin;
  Result := FIntention;
  FIntentionExsit := True;
end;

procedure TEMTFDateType.SetIntention(const _Value: String);
begin
  FIntentionExsit := True;
  FIntention := _Value;
end;

procedure TEMTFDateType.IntentionRemove;
begin
  if FIntentionExsit then
  begin
    FIntentionExsit := False;
  end;
end;

procedure TEMTFDateType.AddIntentionEvent(Sender: TObject);
begin
  AddIntention;
end;

function TEMTFDateType.AddTag: String;
begin;
  Result := FTag;
  FTagExsit := True;
end;

procedure TEMTFDateType.SetTag(const _Value: String);
begin
  FTagExsit := True;
  FTag := _Value;
end;

procedure TEMTFDateType.TagRemove;
begin
  if FTagExsit then
  begin
    FTagExsit := False;
  end;
end;

procedure TEMTFDateType.AddTagEvent(Sender: TObject);
begin
  AddTag;
end;

{  Channels}
constructor TEMTFChannels.Create(par: TXML = nil);
begin
  inherited Create(par);
  FChannels := TList<TEMTFChannel>.Create;
end;

destructor TEMTFChannels.Destroy;
begin
  ChannelClear;
  FChannels.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFChannels.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ChannelTmp: TEMTFChannel;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Channel' then
      begin
        ChannelTmp := TEMTFChannel.Create(Self);
        ChannelTmp.FromXML(nodeTmp);
        FChannels.Add(ChannelTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'ref' then
      begin
        Fref := nodeTmp.Text;
        FrefExsit := True;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    end;
  except
    raise Exception.Create('Channels Read XML Error!' + node.Xml);
  end;
end;

function TEMTFChannels.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  refTmp: IXMLNode;
  unitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Channels';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FChannels.Count - 1 do
       FChannels.Items[I].ToXML(node, 'Channel');
    if FrefExsit then 
    begin
      refTmp := doc.CreateNode('ref', ntAttribute);
      refTmp.NodeValue := Fref;
      node.AttributeNodes.Add(refTmp);
    end;
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFChannels.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if refExsit then
    TreeNodeShape.AddChild('ref');
  if unitsExsit then
    TreeNodeShape.AddChild('units');
  for I := 0 to ChannelCount - 1 do
  begin
    Channels[I].TreeNodeShape := TreeNodeShape.AddChildObject('Channel', Channel[I]);
    Channel[I].ToTree;
  end;
end;

procedure TEMTFChannels.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  refAddMenu: TMenuItem;
  unitsAddMenu: TMenuItem;
  ChannelAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFChannelsPop) and Assigned(TEMTFChannelsTreeComponent) then
    begin
      TEMTFChannelsPop.Clear;
      refAddMenu := TMenuItem.Create(TEMTFChannelsPop);
      refAddMenu.Text := 'Add ref';
      refAddMenu.OnClick := AddrefEvent;
      TEMTFChannelsPop.AddObject(refAddMenu);
      unitsAddMenu := TMenuItem.Create(TEMTFChannelsPop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFChannelsPop.AddObject(unitsAddMenu);
      ChannelAddMenu := TMenuItem.Create(TEMTFChannelsPop);
      ChannelAddMenu.Text := 'Add Channel';
      ChannelAddMenu.OnClick := AddChannelEvent;
      TEMTFChannelsPop.AddObject(ChannelAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFChannelsTreeComponent.ClientToScreen(pt);
      TEMTFChannelsPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFChannels.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFChannelsXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('ref');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ref);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  TEMTFChannelsXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFChannels.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        ref := _Value;
      end;
    1:
      begin
        units := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFChannels.Addref: String;
begin;
  Result := Fref;
  FrefExsit := True;
end;

procedure TEMTFChannels.Setref(const _Value: String);
begin
  FrefExsit := True;
  Fref := _Value;
end;

procedure TEMTFChannels.refRemove;
begin
  if FrefExsit then
  begin
    FrefExsit := False;
  end;
end;

procedure TEMTFChannels.AddrefEvent(Sender: TObject);
begin
  Addref;
end;

function TEMTFChannels.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFChannels.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFChannels.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFChannels.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

function TEMTFChannels.AddChannel: TEMTFChannel;
var
  Channeltmp: TEMTFChannel;
begin;
  Channeltmp := TEMTFChannel.Create(Self);
  FChannels.Add(Channeltmp);
  Result := Channeltmp;
end;

procedure TEMTFChannels.SetChannels(const _Value: TList<TEMTFChannel>);
begin
  ChannelClear;
  FChannels := _Value;
end;

procedure TEMTFChannels.ChannelClear;
begin
  while FChannels.Count > 0 do
  begin
    FChannels.Items[0].Free;
    FChannels.Delete(0);
  end;
end;

function TEMTFChannels.ChannelCount: Integer;
begin
  Result := FChannels.Count;
end;

function TEMTFChannels.GetChannel(Index: Integer): TEMTFChannel;
begin
  Result := FChannels[Index];
end;

procedure TEMTFChannels.SetChannel(Index: Integer;
  const _Value: TEMTFChannel);
begin
  _Value.Parent := Self;
  FChannels[Index].Free;
  FChannels[Index] := _Value;
end;

procedure TEMTFChannels.RemoveChannel(_Value: TEMTFChannel);
begin
  FChannels.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFChannels.DeleteChannel(Index: Integer);
begin
  FChannels.Items[Index].Free;
  FChannels.Delete(Index);
end;

procedure TEMTFChannels.AddChannelEvent(Sender: TObject);
var
  tmp: TEMTFChannel;
begin
  tmp := AddChannel;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Channel', tmp);
  tmp.ToTree;
end;

{  Channel}
constructor TEMTFChannel.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFChannel.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFChannel.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'orientation' then
      begin
        Forientation := nodeTmp.Text.ToDouble;
      end
      else if nodeTmp.NodeName = 'x' then
      begin
        Fx := nodeTmp.Text.ToDouble;
        FxExsit := True;
      end
      else if nodeTmp.NodeName = 'y' then
      begin
        Fy := nodeTmp.Text.ToDouble;
        FyExsit := True;
      end
      else if nodeTmp.NodeName = 'z' then
      begin
        Fz := nodeTmp.Text.ToDouble;
        FzExsit := True;
      end
      else if nodeTmp.NodeName = 'x2' then
      begin
        Fx2 := nodeTmp.Text.ToDouble;
        Fx2Exsit := True;
      end
      else if nodeTmp.NodeName = 'y2' then
      begin
        Fy2 := nodeTmp.Text.ToDouble;
        Fy2Exsit := True;
      end
      else if nodeTmp.NodeName = 'z2' then
      begin
        Fz2 := nodeTmp.Text.ToDouble;
        Fz2Exsit := True;
      end;
    end;
  except
    raise Exception.Create('Channel Read XML Error!' + node.Xml);
  end;
end;

function TEMTFChannel.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  orientationTmp: IXMLNode;
  xTmp: IXMLNode;
  yTmp: IXMLNode;
  zTmp: IXMLNode;
  x2Tmp: IXMLNode;
  y2Tmp: IXMLNode;
  z2Tmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Channel';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    nameTmp := doc.CreateNode('name', ntAttribute);
    nameTmp.NodeValue := Fname;
    node.AttributeNodes.Add(nameTmp);
    orientationTmp := doc.CreateNode('orientation', ntAttribute);
    orientationTmp.NodeValue := Forientation.ToString;
    node.AttributeNodes.Add(orientationTmp);
    if FxExsit then 
    begin
      xTmp := doc.CreateNode('x', ntAttribute);
      xTmp.NodeValue := Fx.ToString;
      node.AttributeNodes.Add(xTmp);
    end;
    if FyExsit then 
    begin
      yTmp := doc.CreateNode('y', ntAttribute);
      yTmp.NodeValue := Fy.ToString;
      node.AttributeNodes.Add(yTmp);
    end;
    if FzExsit then 
    begin
      zTmp := doc.CreateNode('z', ntAttribute);
      zTmp.NodeValue := Fz.ToString;
      node.AttributeNodes.Add(zTmp);
    end;
    if Fx2Exsit then 
    begin
      x2Tmp := doc.CreateNode('x2', ntAttribute);
      x2Tmp.NodeValue := Fx2.ToString;
      node.AttributeNodes.Add(x2Tmp);
    end;
    if Fy2Exsit then 
    begin
      y2Tmp := doc.CreateNode('y2', ntAttribute);
      y2Tmp.NodeValue := Fy2.ToString;
      node.AttributeNodes.Add(y2Tmp);
    end;
    if Fz2Exsit then 
    begin
      z2Tmp := doc.CreateNode('z2', ntAttribute);
      z2Tmp.NodeValue := Fz2.ToString;
      node.AttributeNodes.Add(z2Tmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFChannel.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('name');
  TreeNodeShape.AddChild('orientation');
  if xExsit then
    TreeNodeShape.AddChild('x');
  if yExsit then
    TreeNodeShape.AddChild('y');
  if zExsit then
    TreeNodeShape.AddChild('z');
  if x2Exsit then
    TreeNodeShape.AddChild('x2');
  if y2Exsit then
    TreeNodeShape.AddChild('y2');
  if z2Exsit then
    TreeNodeShape.AddChild('z2');
end;

procedure TEMTFChannel.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  xAddMenu: TMenuItem;
  yAddMenu: TMenuItem;
  zAddMenu: TMenuItem;
  x2AddMenu: TMenuItem;
  y2AddMenu: TMenuItem;
  z2AddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFChannelPop) and Assigned(TEMTFChannelTreeComponent) then
    begin
      TEMTFChannelPop.Clear;
      xAddMenu := TMenuItem.Create(TEMTFChannelPop);
      xAddMenu.Text := 'Add x';
      xAddMenu.OnClick := AddxEvent;
      TEMTFChannelPop.AddObject(xAddMenu);
      yAddMenu := TMenuItem.Create(TEMTFChannelPop);
      yAddMenu.Text := 'Add y';
      yAddMenu.OnClick := AddyEvent;
      TEMTFChannelPop.AddObject(yAddMenu);
      zAddMenu := TMenuItem.Create(TEMTFChannelPop);
      zAddMenu.Text := 'Add z';
      zAddMenu.OnClick := AddzEvent;
      TEMTFChannelPop.AddObject(zAddMenu);
      x2AddMenu := TMenuItem.Create(TEMTFChannelPop);
      x2AddMenu.Text := 'Add x2';
      x2AddMenu.OnClick := Addx2Event;
      TEMTFChannelPop.AddObject(x2AddMenu);
      y2AddMenu := TMenuItem.Create(TEMTFChannelPop);
      y2AddMenu.Text := 'Add y2';
      y2AddMenu.OnClick := Addy2Event;
      TEMTFChannelPop.AddObject(y2AddMenu);
      z2AddMenu := TMenuItem.Create(TEMTFChannelPop);
      z2AddMenu.Text := 'Add z2';
      z2AddMenu.OnClick := Addz2Event;
      TEMTFChannelPop.AddObject(z2AddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFChannelTreeComponent.ClientToScreen(pt);
      TEMTFChannelPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFChannel.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFChannelXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('orientation');
  Types_Value.Add(xs_double);
  _Values_Value.Add(orientation.ToString);
  Names_Value.Add('x');
  Types_Value.Add(xs_double);
  _Values_Value.Add(x.ToString);
  Names_Value.Add('y');
  Types_Value.Add(xs_double);
  _Values_Value.Add(y.ToString);
  Names_Value.Add('z');
  Types_Value.Add(xs_double);
  _Values_Value.Add(z.ToString);
  Names_Value.Add('x2');
  Types_Value.Add(xs_double);
  _Values_Value.Add(x2.ToString);
  Names_Value.Add('y2');
  Types_Value.Add(xs_double);
  _Values_Value.Add(y2.ToString);
  Names_Value.Add('z2');
  Types_Value.Add(xs_double);
  _Values_Value.Add(z2.ToString);
  TEMTFChannelXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFChannel.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        orientation := _Value.ToDouble;
      end;
    2:
      begin
        x := _Value.ToDouble;
      end;
    3:
      begin
        y := _Value.ToDouble;
      end;
    4:
      begin
        z := _Value.ToDouble;
      end;
    5:
      begin
        x2 := _Value.ToDouble;
      end;
    6:
      begin
        y2 := _Value.ToDouble;
      end;
    7:
      begin
        z2 := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

procedure TEMTFChannel.Setname(const _Value: String);
begin
  Fname := _Value;
end;

procedure TEMTFChannel.Setorientation(const _Value: Double);
begin
  Forientation := _Value;
end;

function TEMTFChannel.Addx: Double;
begin;
  Result := Fx;
  FxExsit := True;
end;

procedure TEMTFChannel.Setx(const _Value: Double);
begin
  FxExsit := True;
  Fx := _Value;
end;

procedure TEMTFChannel.xRemove;
begin
  if FxExsit then
  begin
    FxExsit := False;
  end;
end;

procedure TEMTFChannel.AddxEvent(Sender: TObject);
begin
  Addx;
end;

function TEMTFChannel.Addy: Double;
begin;
  Result := Fy;
  FyExsit := True;
end;

procedure TEMTFChannel.Sety(const _Value: Double);
begin
  FyExsit := True;
  Fy := _Value;
end;

procedure TEMTFChannel.yRemove;
begin
  if FyExsit then
  begin
    FyExsit := False;
  end;
end;

procedure TEMTFChannel.AddyEvent(Sender: TObject);
begin
  Addy;
end;

function TEMTFChannel.Addz: Double;
begin;
  Result := Fz;
  FzExsit := True;
end;

procedure TEMTFChannel.Setz(const _Value: Double);
begin
  FzExsit := True;
  Fz := _Value;
end;

procedure TEMTFChannel.zRemove;
begin
  if FzExsit then
  begin
    FzExsit := False;
  end;
end;

procedure TEMTFChannel.AddzEvent(Sender: TObject);
begin
  Addz;
end;

function TEMTFChannel.Addx2: Double;
begin;
  Result := Fx2;
  Fx2Exsit := True;
end;

procedure TEMTFChannel.Setx2(const _Value: Double);
begin
  Fx2Exsit := True;
  Fx2 := _Value;
end;

procedure TEMTFChannel.x2Remove;
begin
  if Fx2Exsit then
  begin
    Fx2Exsit := False;
  end;
end;

procedure TEMTFChannel.Addx2Event(Sender: TObject);
begin
  Addx2;
end;

function TEMTFChannel.Addy2: Double;
begin;
  Result := Fy2;
  Fy2Exsit := True;
end;

procedure TEMTFChannel.Sety2(const _Value: Double);
begin
  Fy2Exsit := True;
  Fy2 := _Value;
end;

procedure TEMTFChannel.y2Remove;
begin
  if Fy2Exsit then
  begin
    Fy2Exsit := False;
  end;
end;

procedure TEMTFChannel.Addy2Event(Sender: TObject);
begin
  Addy2;
end;

function TEMTFChannel.Addz2: Double;
begin;
  Result := Fz2;
  Fz2Exsit := True;
end;

procedure TEMTFChannel.Setz2(const _Value: Double);
begin
  Fz2Exsit := True;
  Fz2 := _Value;
end;

procedure TEMTFChannel.z2Remove;
begin
  if Fz2Exsit then
  begin
    Fz2Exsit := False;
  end;
end;

procedure TEMTFChannel.Addz2Event(Sender: TObject);
begin
  Addz2;
end;

{  Data}
constructor TEMTFData.Create(par: TXML = nil);
begin
  inherited Create(par);
  FPeriodDatas := TList<TEMTFPeriodData>.Create;
end;

destructor TEMTFData.Destroy;
begin
  PeriodDataClear;
  FPeriodDatas.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFData.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  PeriodDataTmp: TEMTFPeriodData;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Period' then
      begin
        PeriodDataTmp := TEMTFPeriodData.Create(Self);
        PeriodDataTmp.FromXML(nodeTmp);
        FPeriodDatas.Add(PeriodDataTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'count' then
      begin
        Fcount := nodeTmp.Text.ToInteger;
        FcountExsit := True;
      end;
    end;
  except
    raise Exception.Create('Data Read XML Error!' + node.Xml);
  end;
end;

function TEMTFData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  countTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Data';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FPeriodDatas.Count - 1 do
       FPeriodDatas.Items[I].ToXML(node, 'Period');
    if FcountExsit then 
    begin
      countTmp := doc.CreateNode('count', ntAttribute);
      countTmp.NodeValue := Fcount.toString;
      node.AttributeNodes.Add(countTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to PeriodDataCount - 1 do
  begin
    PeriodDatas[I].TreeNodeShape := TreeNodeShape.AddChildObject('PeriodData', PeriodData[I]);
    PeriodData[I].ToTree;
  end;
  if countExsit then
    TreeNodeShape.AddChild('count');
end;

procedure TEMTFData.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  PeriodDataAddMenu: TMenuItem;
  countAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDataPop) and Assigned(TEMTFDataTreeComponent) then
    begin
      TEMTFDataPop.Clear;
      PeriodDataAddMenu := TMenuItem.Create(TEMTFDataPop);
      PeriodDataAddMenu.Text := 'Add PeriodData';
      PeriodDataAddMenu.OnClick := AddPeriodDataEvent;
      TEMTFDataPop.AddObject(PeriodDataAddMenu);
      countAddMenu := TMenuItem.Create(TEMTFDataPop);
      countAddMenu.Text := 'Add count';
      countAddMenu.OnClick := AddcountEvent;
      TEMTFDataPop.AddObject(countAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFDataTreeComponent.ClientToScreen(pt);
      TEMTFDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('count');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(count.toString);
  TEMTFDataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFData.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        count := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

function TEMTFData.AddPeriodData: TEMTFPeriodData;
var
  PeriodDatatmp: TEMTFPeriodData;
begin;
  PeriodDatatmp := TEMTFPeriodData.Create(Self);
  FPeriodDatas.Add(PeriodDatatmp);
  Result := PeriodDatatmp;
end;

procedure TEMTFData.SetPeriodDatas(const _Value: TList<TEMTFPeriodData>);
begin
  PeriodDataClear;
  FPeriodDatas := _Value;
end;

procedure TEMTFData.PeriodDataClear;
begin
  while FPeriodDatas.Count > 0 do
  begin
    FPeriodDatas.Items[0].Free;
    FPeriodDatas.Delete(0);
  end;
end;

function TEMTFData.PeriodDataCount: Integer;
begin
  Result := FPeriodDatas.Count;
end;

function TEMTFData.GetPeriodData(Index: Integer): TEMTFPeriodData;
begin
  Result := FPeriodDatas[Index];
end;

procedure TEMTFData.SetPeriodData(Index: Integer;
  const _Value: TEMTFPeriodData);
begin
  _Value.Parent := Self;
  FPeriodDatas[Index].Free;
  FPeriodDatas[Index] := _Value;
end;

procedure TEMTFData.RemovePeriodData(_Value: TEMTFPeriodData);
begin
  FPeriodDatas.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFData.DeletePeriodData(Index: Integer);
begin
  FPeriodDatas.Items[Index].Free;
  FPeriodDatas.Delete(Index);
end;

procedure TEMTFData.AddPeriodDataEvent(Sender: TObject);
var
  tmp: TEMTFPeriodData;
begin
  tmp := AddPeriodData;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('PeriodData', tmp);
  tmp.ToTree;
end;

function TEMTFData.Addcount: Integer;
begin;
  Result := Fcount;
  FcountExsit := True;
end;

procedure TEMTFData.Setcount(const _Value: Integer);
begin
  FcountExsit := True;
  Fcount := _Value;
end;

procedure TEMTFData.countRemove;
begin
  if FcountExsit then
  begin
    FcountExsit := False;
  end;
end;

procedure TEMTFData.AddcountEvent(Sender: TObject);
begin
  Addcount;
end;

{  value}
constructor TEMTFValue.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFValue.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFValue.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'output' then
      begin
        Foutput := nodeTmp.Text;
        FoutputExsit := True;
      end
      else if nodeTmp.NodeName = 'input' then
      begin
        Finput := nodeTmp.Text;
        FinputExsit := True;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    Fvalue := node.Text;
    end;
  except
    raise Exception.Create('value Read XML Error!' + node.Xml);
  end;
end;

function TEMTFValue.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  outputTmp: IXMLNode;
  inputTmp: IXMLNode;
  valueTmp: IXMLNode;
  unitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'value';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FnameExsit then 
    begin
      nameTmp := doc.CreateNode('name', ntAttribute);
      nameTmp.NodeValue := Fname;
      node.AttributeNodes.Add(nameTmp);
    end;
    if FoutputExsit then 
    begin
      outputTmp := doc.CreateNode('output', ntAttribute);
      outputTmp.NodeValue := Foutput;
      node.AttributeNodes.Add(outputTmp);
    end;
    if FinputExsit then 
    begin
      inputTmp := doc.CreateNode('input', ntAttribute);
      inputTmp.NodeValue := Finput;
      node.AttributeNodes.Add(inputTmp);
    end;
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    node.NodeValue := Fvalue;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFValue.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if outputExsit then
    TreeNodeShape.AddChild('output');
  if inputExsit then
    TreeNodeShape.AddChild('input');
  TreeNodeShape.AddChild('value');
  if unitsExsit then
    TreeNodeShape.AddChild('units');
end;

procedure TEMTFValue.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  nameAddMenu: TMenuItem;
  outputAddMenu: TMenuItem;
  inputAddMenu: TMenuItem;
  unitsAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFValuePop) and Assigned(TEMTFValueTreeComponent) then
    begin
      TEMTFValuePop.Clear;
      nameAddMenu := TMenuItem.Create(TEMTFValuePop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TEMTFValuePop.AddObject(nameAddMenu);
      outputAddMenu := TMenuItem.Create(TEMTFValuePop);
      outputAddMenu.Text := 'Add output';
      outputAddMenu.OnClick := AddoutputEvent;
      TEMTFValuePop.AddObject(outputAddMenu);
      inputAddMenu := TMenuItem.Create(TEMTFValuePop);
      inputAddMenu.Text := 'Add input';
      inputAddMenu.OnClick := AddinputEvent;
      TEMTFValuePop.AddObject(inputAddMenu);
      unitsAddMenu := TMenuItem.Create(TEMTFValuePop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFValuePop.AddObject(unitsAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFValueTreeComponent.ClientToScreen(pt);
      TEMTFValuePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFValue.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFValueXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('output');
  Types_Value.Add(xs_string);
  _Values_Value.Add(output);
  Names_Value.Add('input');
  Types_Value.Add(xs_string);
  _Values_Value.Add(input);
  Names_Value.Add('value');
  Types_Value.Add(xs_string);
  _Values_Value.Add(value);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  TEMTFValueXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFValue.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        output := _Value;
      end;
    2:
      begin
        input := _Value;
      end;
    3:
      begin
        value := _Value;
      end;
    4:
      begin
        units := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFValue.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TEMTFValue.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TEMTFValue.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TEMTFValue.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TEMTFValue.Addoutput: String;
begin;
  Result := Foutput;
  FoutputExsit := True;
end;

procedure TEMTFValue.Setoutput(const _Value: String);
begin
  FoutputExsit := True;
  Foutput := _Value;
end;

procedure TEMTFValue.outputRemove;
begin
  if FoutputExsit then
  begin
    FoutputExsit := False;
  end;
end;

procedure TEMTFValue.AddoutputEvent(Sender: TObject);
begin
  Addoutput;
end;

function TEMTFValue.Addinput: String;
begin;
  Result := Finput;
  FinputExsit := True;
end;

procedure TEMTFValue.Setinput(const _Value: String);
begin
  FinputExsit := True;
  Finput := _Value;
end;

procedure TEMTFValue.inputRemove;
begin
  if FinputExsit then
  begin
    FinputExsit := False;
  end;
end;

procedure TEMTFValue.AddinputEvent(Sender: TObject);
begin
  Addinput;
end;

procedure TEMTFValue.Setvalue(const _Value: String);
begin
  Fvalue := _Value;
end;

function TEMTFValue.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFValue.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFValue.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFValue.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

{  Provenance}
constructor TEMTFProvenance.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFProvenance.Destroy;
begin
  if FCreatorExsit then
    FCreator.Free;
  if FSubmitterExsit then
    FSubmitter.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFProvenance.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'CreateTime' then
      begin
        FCreateTime := StrToDateTimeDef(nodeTmp.Text, Now());
        FCreateTimeExsit := True;
      end
      else if nodeTmp.NodeName = 'CreatingApplication' then
      begin
        FCreatingApplication := nodeTmp.Text;
        FCreatingApplicationExsit := True;
      end
      else if nodeTmp.NodeName = 'Creator' then
      begin
        FCreator := TEMTFPerson.Create(Self);
        FCreator.FromXML(nodeTmp);
        FCreatorExsit := True;
      end
      else if nodeTmp.NodeName = 'Submitter' then
      begin
        FSubmitter := TEMTFPerson.Create(Self);
        FSubmitter.FromXML(nodeTmp);
        FSubmitterExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Provenance Read XML Error!' + node.Xml);
  end;
end;

function TEMTFProvenance.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  CreateTimeTmp: IXMLNode;
  CreatingApplicationTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Provenance';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FCreateTimeExsit then
    begin
      CreateTimeTmp := doc.CreateNode('CreateTime', ntElement);
      CreateTimeTmp.NodeValue := FormatDateTime(XMLDTFormat, FCreateTime);
      node.ChildNodes.Add(CreateTimeTmp);
    end;
    if FCreatingApplicationExsit then
    begin
      CreatingApplicationTmp := doc.CreateNode('CreatingApplication', ntElement);
      CreatingApplicationTmp.NodeValue := FCreatingApplication;
      node.ChildNodes.Add(CreatingApplicationTmp);
    end;
    if FCreatorExsit then
      FCreator.ToXML(node, 'Creator');
    if FSubmitterExsit then
      FSubmitter.ToXML(node, 'Submitter');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFProvenance.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if CreateTimeExsit then
    TreeNodeShape.AddChild('CreateTime');
  if CreatingApplicationExsit then
    TreeNodeShape.AddChild('CreatingApplication');
  if CreatorExsit then
  begin
    Creator.TreeNodeShape := TreeNodeShape.AddChildObject('Creator', Creator);
    Creator.ToTree;
  end;
  if SubmitterExsit then
  begin
    Submitter.TreeNodeShape := TreeNodeShape.AddChildObject('Submitter', Submitter);
    Submitter.ToTree;
  end;
end;

procedure TEMTFProvenance.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  CreateTimeAddMenu: TMenuItem;
  CreatingApplicationAddMenu: TMenuItem;
  CreatorAddMenu: TMenuItem;
  SubmitterAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFProvenancePop) and Assigned(TEMTFProvenanceTreeComponent) then
    begin
      TEMTFProvenancePop.Clear;
      CreateTimeAddMenu := TMenuItem.Create(TEMTFProvenancePop);
      CreateTimeAddMenu.Text := 'Add CreateTime';
      CreateTimeAddMenu.OnClick := AddCreateTimeEvent;
      TEMTFProvenancePop.AddObject(CreateTimeAddMenu);
      CreatingApplicationAddMenu := TMenuItem.Create(TEMTFProvenancePop);
      CreatingApplicationAddMenu.Text := 'Add CreatingApplication';
      CreatingApplicationAddMenu.OnClick := AddCreatingApplicationEvent;
      TEMTFProvenancePop.AddObject(CreatingApplicationAddMenu);
      CreatorAddMenu := TMenuItem.Create(TEMTFProvenancePop);
      CreatorAddMenu.Text := 'Add Creator';
      CreatorAddMenu.OnClick := AddCreatorEvent;
      TEMTFProvenancePop.AddObject(CreatorAddMenu);
      SubmitterAddMenu := TMenuItem.Create(TEMTFProvenancePop);
      SubmitterAddMenu.Text := 'Add Submitter';
      SubmitterAddMenu.OnClick := AddSubmitterEvent;
      TEMTFProvenancePop.AddObject(SubmitterAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFProvenanceTreeComponent.ClientToScreen(pt);
      TEMTFProvenancePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFProvenance.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFProvenanceXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('CreateTime');
  Types_Value.Add(xs_dateTime);
  _Values_Value.Add(FormatDateTime(XMLDTFormat, CreateTime));
  Names_Value.Add('CreatingApplication');
  Types_Value.Add(xs_string);
  _Values_Value.Add(CreatingApplication);
  TEMTFProvenanceXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFProvenance.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        CreateTime := StrToDateTimeDef(_Value, Now());
      end;
    1:
      begin
        CreatingApplication := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFProvenance.AddCreateTime: TDateTime;
begin;
  Result := FCreateTime;
  FCreateTimeExsit := True;
end;

procedure TEMTFProvenance.SetCreateTime(const _Value: TDateTime);
begin
  FCreateTimeExsit := True;
  FCreateTime := _Value;
end;

procedure TEMTFProvenance.CreateTimeRemove;
begin
  if FCreateTimeExsit then
  begin
    FCreateTimeExsit := False;
  end;
end;

procedure TEMTFProvenance.AddCreateTimeEvent(Sender: TObject);
begin
  AddCreateTime;
end;

function TEMTFProvenance.AddCreatingApplication: String;
begin;
  Result := FCreatingApplication;
  FCreatingApplicationExsit := True;
end;

procedure TEMTFProvenance.SetCreatingApplication(const _Value: String);
begin
  FCreatingApplicationExsit := True;
  FCreatingApplication := _Value;
end;

procedure TEMTFProvenance.CreatingApplicationRemove;
begin
  if FCreatingApplicationExsit then
  begin
    FCreatingApplicationExsit := False;
  end;
end;

procedure TEMTFProvenance.AddCreatingApplicationEvent(Sender: TObject);
begin
  AddCreatingApplication;
end;

function TEMTFProvenance.AddCreator: TEMTFPerson;
begin;
  if not FCreatorExsit then
    FCreator := TEMTFPerson.Create(Self);
  Result := FCreator;
  FCreatorExsit := True;
end;

procedure TEMTFProvenance.SetCreator(const _Value: TEMTFPerson);
begin
  if FCreatorExsit then
    FCreator.Free;
  FCreatorExsit := True;
  FCreator := _Value;
  FCreator.Parent := Self;
end;

procedure TEMTFProvenance.CreatorRemove;
begin
  if FCreatorExsit then
  begin
    FCreator.Free;
    FCreatorExsit := False;
  end;
end;

procedure TEMTFProvenance.AddCreatorEvent(Sender: TObject);
begin
  AddCreator;
  FCreator.ToTree;
end;

function TEMTFProvenance.AddSubmitter: TEMTFPerson;
begin;
  if not FSubmitterExsit then
    FSubmitter := TEMTFPerson.Create(Self);
  Result := FSubmitter;
  FSubmitterExsit := True;
end;

procedure TEMTFProvenance.SetSubmitter(const _Value: TEMTFPerson);
begin
  if FSubmitterExsit then
    FSubmitter.Free;
  FSubmitterExsit := True;
  FSubmitter := _Value;
  FSubmitter.Parent := Self;
end;

procedure TEMTFProvenance.SubmitterRemove;
begin
  if FSubmitterExsit then
  begin
    FSubmitter.Free;
    FSubmitterExsit := False;
  end;
end;

procedure TEMTFProvenance.AddSubmitterEvent(Sender: TObject);
begin
  AddSubmitter;
  FSubmitter.ToTree;
end;

{  Person}
constructor TEMTFPerson.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFPerson.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFPerson.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'Email' then
      begin
        FEmail := nodeTmp.Text;
        FEmailExsit := True;
      end
      else if nodeTmp.NodeName = 'Org' then
      begin
        FOrg := nodeTmp.Text;
        FOrgExsit := True;
      end
      else if nodeTmp.NodeName = 'OrgUrl' then
      begin
        FOrgUrl := nodeTmp.Text;
        FOrgUrlExsit := True;
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

function TEMTFPerson.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  EmailTmp: IXMLNode;
  OrgTmp: IXMLNode;
  OrgUrlTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Person';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FNameExsit then
    begin
      NameTmp := doc.CreateNode('Name', ntElement);
      NameTmp.NodeValue := FName;
      node.ChildNodes.Add(NameTmp);
    end;
    if FEmailExsit then
    begin
      EmailTmp := doc.CreateNode('Email', ntElement);
      EmailTmp.NodeValue := FEmail;
      node.ChildNodes.Add(EmailTmp);
    end;
    if FOrgExsit then
    begin
      OrgTmp := doc.CreateNode('Org', ntElement);
      OrgTmp.NodeValue := FOrg;
      node.ChildNodes.Add(OrgTmp);
    end;
    if FOrgUrlExsit then
    begin
      OrgUrlTmp := doc.CreateNode('OrgUrl', ntElement);
      OrgUrlTmp.NodeValue := FOrgUrl;
      node.ChildNodes.Add(OrgUrlTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFPerson.ToTree;
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
  if EmailExsit then
    TreeNodeShape.AddChild('Email');
  if OrgExsit then
    TreeNodeShape.AddChild('Org');
  if OrgUrlExsit then
    TreeNodeShape.AddChild('OrgUrl');
end;

procedure TEMTFPerson.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  EmailAddMenu: TMenuItem;
  OrgAddMenu: TMenuItem;
  OrgUrlAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFPersonPop) and Assigned(TEMTFPersonTreeComponent) then
    begin
      TEMTFPersonPop.Clear;
      NameAddMenu := TMenuItem.Create(TEMTFPersonPop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFPersonPop.AddObject(NameAddMenu);
      EmailAddMenu := TMenuItem.Create(TEMTFPersonPop);
      EmailAddMenu.Text := 'Add Email';
      EmailAddMenu.OnClick := AddEmailEvent;
      TEMTFPersonPop.AddObject(EmailAddMenu);
      OrgAddMenu := TMenuItem.Create(TEMTFPersonPop);
      OrgAddMenu.Text := 'Add Org';
      OrgAddMenu.OnClick := AddOrgEvent;
      TEMTFPersonPop.AddObject(OrgAddMenu);
      OrgUrlAddMenu := TMenuItem.Create(TEMTFPersonPop);
      OrgUrlAddMenu.Text := 'Add OrgUrl';
      OrgUrlAddMenu.OnClick := AddOrgUrlEvent;
      TEMTFPersonPop.AddObject(OrgUrlAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFPersonTreeComponent.ClientToScreen(pt);
      TEMTFPersonPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFPerson.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFPersonXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Email');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Email);
  Names_Value.Add('Org');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Org);
  Names_Value.Add('OrgUrl');
  Types_Value.Add(xs_string);
  _Values_Value.Add(OrgUrl);
  TEMTFPersonXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFPerson.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Email := _Value;
      end;
    2:
      begin
        Org := _Value;
      end;
    3:
      begin
        OrgUrl := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFPerson.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFPerson.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFPerson.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFPerson.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TEMTFPerson.AddEmail: String;
begin;
  Result := FEmail;
  FEmailExsit := True;
end;

procedure TEMTFPerson.SetEmail(const _Value: String);
begin
  FEmailExsit := True;
  FEmail := _Value;
end;

procedure TEMTFPerson.EmailRemove;
begin
  if FEmailExsit then
  begin
    FEmailExsit := False;
  end;
end;

procedure TEMTFPerson.AddEmailEvent(Sender: TObject);
begin
  AddEmail;
end;

function TEMTFPerson.AddOrg: String;
begin;
  Result := FOrg;
  FOrgExsit := True;
end;

procedure TEMTFPerson.SetOrg(const _Value: String);
begin
  FOrgExsit := True;
  FOrg := _Value;
end;

procedure TEMTFPerson.OrgRemove;
begin
  if FOrgExsit then
  begin
    FOrgExsit := False;
  end;
end;

procedure TEMTFPerson.AddOrgEvent(Sender: TObject);
begin
  AddOrg;
end;

function TEMTFPerson.AddOrgUrl: String;
begin;
  Result := FOrgUrl;
  FOrgUrlExsit := True;
end;

procedure TEMTFPerson.SetOrgUrl(const _Value: String);
begin
  FOrgUrlExsit := True;
  FOrgUrl := _Value;
end;

procedure TEMTFPerson.OrgUrlRemove;
begin
  if FOrgUrlExsit then
  begin
    FOrgUrlExsit := False;
  end;
end;

procedure TEMTFPerson.AddOrgUrlEvent(Sender: TObject);
begin
  AddOrgUrl;
end;

{  DataQualityNotes}
constructor TEMTFDataQualityNotes.Create(par: TXML = nil);
begin
  inherited Create(par);
  FComments := TEMTFComments.Create(Self);
end;

destructor TEMTFDataQualityNotes.Destroy;
begin
  FComments.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFDataQualityNotes.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Rating' then
      begin
        FRating := nodeTmp.Text.ToDouble;
        FRatingExsit := True;
      end
      else if nodeTmp.NodeName = 'GoodFromPeriod' then
      begin
        FGoodFromPeriod := nodeTmp.Text.ToDouble;
        FGoodFromPeriodExsit := True;
      end
      else if nodeTmp.NodeName = 'GoodToPeriod' then
      begin
        FGoodToPeriod := nodeTmp.Text.ToDouble;
        FGoodToPeriodExsit := True;
      end
      else if nodeTmp.NodeName = 'Comments' then
      begin
        FComments := TEMTFComments.Create(Self);
        FComments.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('DataQualityNotes Read XML Error!' + node.Xml);
  end;
end;

function TEMTFDataQualityNotes.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  RatingTmp: IXMLNode;
  GoodFromPeriodTmp: IXMLNode;
  GoodToPeriodTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'DataQualityNotes';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FRatingExsit then
    begin
      RatingTmp := doc.CreateNode('Rating', ntElement);
      RatingTmp.NodeValue := FRating.ToString;
      node.ChildNodes.Add(RatingTmp);
    end;
    if FGoodFromPeriodExsit then
    begin
      GoodFromPeriodTmp := doc.CreateNode('GoodFromPeriod', ntElement);
      GoodFromPeriodTmp.NodeValue := FGoodFromPeriod.ToString;
      node.ChildNodes.Add(GoodFromPeriodTmp);
    end;
    if FGoodToPeriodExsit then
    begin
      GoodToPeriodTmp := doc.CreateNode('GoodToPeriod', ntElement);
      GoodToPeriodTmp.NodeValue := FGoodToPeriod.ToString;
      node.ChildNodes.Add(GoodToPeriodTmp);
    end;
    FComments.ToXML(node, 'Comments');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFDataQualityNotes.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if RatingExsit then
    TreeNodeShape.AddChild('Rating');
  if GoodFromPeriodExsit then
    TreeNodeShape.AddChild('GoodFromPeriod');
  if GoodToPeriodExsit then
    TreeNodeShape.AddChild('GoodToPeriod');
  Comments.TreeNodeShape := TreeNodeShape.AddChildObject('Comments', Comments);
  Comments.ToTree;
end;

procedure TEMTFDataQualityNotes.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  RatingAddMenu: TMenuItem;
  GoodFromPeriodAddMenu: TMenuItem;
  GoodToPeriodAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDataQualityNotesPop) and Assigned(TEMTFDataQualityNotesTreeComponent) then
    begin
      TEMTFDataQualityNotesPop.Clear;
      RatingAddMenu := TMenuItem.Create(TEMTFDataQualityNotesPop);
      RatingAddMenu.Text := 'Add Rating';
      RatingAddMenu.OnClick := AddRatingEvent;
      TEMTFDataQualityNotesPop.AddObject(RatingAddMenu);
      GoodFromPeriodAddMenu := TMenuItem.Create(TEMTFDataQualityNotesPop);
      GoodFromPeriodAddMenu.Text := 'Add GoodFromPeriod';
      GoodFromPeriodAddMenu.OnClick := AddGoodFromPeriodEvent;
      TEMTFDataQualityNotesPop.AddObject(GoodFromPeriodAddMenu);
      GoodToPeriodAddMenu := TMenuItem.Create(TEMTFDataQualityNotesPop);
      GoodToPeriodAddMenu.Text := 'Add GoodToPeriod';
      GoodToPeriodAddMenu.OnClick := AddGoodToPeriodEvent;
      TEMTFDataQualityNotesPop.AddObject(GoodToPeriodAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFDataQualityNotesTreeComponent.ClientToScreen(pt);
      TEMTFDataQualityNotesPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFDataQualityNotes.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDataQualityNotesXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Rating');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Rating.ToString);
  Names_Value.Add('GoodFromPeriod');
  Types_Value.Add(xs_double);
  _Values_Value.Add(GoodFromPeriod.ToString);
  Names_Value.Add('GoodToPeriod');
  Types_Value.Add(xs_double);
  _Values_Value.Add(GoodToPeriod.ToString);
  TEMTFDataQualityNotesXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFDataQualityNotes.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Rating := _Value.ToDouble;
      end;
    1:
      begin
        GoodFromPeriod := _Value.ToDouble;
      end;
    2:
      begin
        GoodToPeriod := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TEMTFDataQualityNotes.AddRating: Double;
begin;
  Result := FRating;
  FRatingExsit := True;
end;

procedure TEMTFDataQualityNotes.SetRating(const _Value: Double);
begin
  FRatingExsit := True;
  FRating := _Value;
end;

procedure TEMTFDataQualityNotes.RatingRemove;
begin
  if FRatingExsit then
  begin
    FRatingExsit := False;
  end;
end;

procedure TEMTFDataQualityNotes.AddRatingEvent(Sender: TObject);
begin
  AddRating;
end;

function TEMTFDataQualityNotes.AddGoodFromPeriod: Double;
begin;
  Result := FGoodFromPeriod;
  FGoodFromPeriodExsit := True;
end;

procedure TEMTFDataQualityNotes.SetGoodFromPeriod(const _Value: Double);
begin
  FGoodFromPeriodExsit := True;
  FGoodFromPeriod := _Value;
end;

procedure TEMTFDataQualityNotes.GoodFromPeriodRemove;
begin
  if FGoodFromPeriodExsit then
  begin
    FGoodFromPeriodExsit := False;
  end;
end;

procedure TEMTFDataQualityNotes.AddGoodFromPeriodEvent(Sender: TObject);
begin
  AddGoodFromPeriod;
end;

function TEMTFDataQualityNotes.AddGoodToPeriod: Double;
begin;
  Result := FGoodToPeriod;
  FGoodToPeriodExsit := True;
end;

procedure TEMTFDataQualityNotes.SetGoodToPeriod(const _Value: Double);
begin
  FGoodToPeriodExsit := True;
  FGoodToPeriod := _Value;
end;

procedure TEMTFDataQualityNotes.GoodToPeriodRemove;
begin
  if FGoodToPeriodExsit then
  begin
    FGoodToPeriodExsit := False;
  end;
end;

procedure TEMTFDataQualityNotes.AddGoodToPeriodEvent(Sender: TObject);
begin
  AddGoodToPeriod;
end;

procedure TEMTFDataQualityNotes.SetComments(const _Value: TEMTFComments);
begin
  FComments.Free;
  FComments := _Value;
  FComments.Parent := Self;
end;

{  UnitValue}
constructor TEMTFUnitValue.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFUnitValue.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFUnitValue.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    FValue := node.Text.ToDouble;
    end;
  except
    raise Exception.Create('UnitValue Read XML Error!' + node.Xml);
  end;
end;

function TEMTFUnitValue.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  unitsTmp: IXMLNode;
  ValueTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'UnitValue';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    node.NodeValue := FValue.ToString;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFUnitValue.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if unitsExsit then
    TreeNodeShape.AddChild('units');
  TreeNodeShape.AddChild('Value');
end;

procedure TEMTFUnitValue.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  unitsAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFUnitValuePop) and Assigned(TEMTFUnitValueTreeComponent) then
    begin
      TEMTFUnitValuePop.Clear;
      unitsAddMenu := TMenuItem.Create(TEMTFUnitValuePop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFUnitValuePop.AddObject(unitsAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFUnitValueTreeComponent.ClientToScreen(pt);
      TEMTFUnitValuePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFUnitValue.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFUnitValueXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  Names_Value.Add('Value');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Value.ToString);
  TEMTFUnitValueXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFUnitValue.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        units := _Value;
      end;
    1:
      begin
        Value := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TEMTFUnitValue.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFUnitValue.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFUnitValue.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFUnitValue.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

procedure TEMTFUnitValue.SetValue(const _Value: Double);
begin
  FValue := _Value;
end;

{  Electrode}
constructor TEMTFElectrode.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFElectrode.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFElectrode.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'location' then
      begin
        Flocation := nodeTmp.Text;
        FlocationExsit := True;
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        Fnumber := nodeTmp.Text;
        FnumberExsit := True;
      end;
    FValue := node.Text.ToDouble;
    end;
  except
    raise Exception.Create('Electrode Read XML Error!' + node.Xml);
  end;
end;

function TEMTFElectrode.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  locationTmp: IXMLNode;
  ValueTmp: IXMLNode;
  numberTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Electrode';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FlocationExsit then 
    begin
      locationTmp := doc.CreateNode('location', ntAttribute);
      locationTmp.NodeValue := Flocation;
      node.AttributeNodes.Add(locationTmp);
    end;
    if FnumberExsit then 
    begin
      numberTmp := doc.CreateNode('number', ntAttribute);
      numberTmp.NodeValue := Fnumber;
      node.AttributeNodes.Add(numberTmp);
    end;
    node.NodeValue := FValue.ToString;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFElectrode.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if locationExsit then
    TreeNodeShape.AddChild('location');
  TreeNodeShape.AddChild('Value');
  if numberExsit then
    TreeNodeShape.AddChild('number');
end;

procedure TEMTFElectrode.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  locationAddMenu: TMenuItem;
  numberAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFElectrodePop) and Assigned(TEMTFElectrodeTreeComponent) then
    begin
      TEMTFElectrodePop.Clear;
      locationAddMenu := TMenuItem.Create(TEMTFElectrodePop);
      locationAddMenu.Text := 'Add location';
      locationAddMenu.OnClick := AddlocationEvent;
      TEMTFElectrodePop.AddObject(locationAddMenu);
      numberAddMenu := TMenuItem.Create(TEMTFElectrodePop);
      numberAddMenu.Text := 'Add number';
      numberAddMenu.OnClick := AddnumberEvent;
      TEMTFElectrodePop.AddObject(numberAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFElectrodeTreeComponent.ClientToScreen(pt);
      TEMTFElectrodePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFElectrode.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFElectrodeXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('location');
  Types_Value.Add(xs_string);
  _Values_Value.Add(location);
  Names_Value.Add('Value');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Value.ToString);
  Names_Value.Add('number');
  Types_Value.Add(xs_string);
  _Values_Value.Add(number);
  TEMTFElectrodeXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFElectrode.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        location := _Value;
      end;
    1:
      begin
        Value := _Value.ToDouble;
      end;
    2:
      begin
        number := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFElectrode.Addlocation: String;
begin;
  Result := Flocation;
  FlocationExsit := True;
end;

procedure TEMTFElectrode.Setlocation(const _Value: String);
begin
  FlocationExsit := True;
  Flocation := _Value;
end;

procedure TEMTFElectrode.locationRemove;
begin
  if FlocationExsit then
  begin
    FlocationExsit := False;
  end;
end;

procedure TEMTFElectrode.AddlocationEvent(Sender: TObject);
begin
  Addlocation;
end;

procedure TEMTFElectrode.SetValue(const _Value: Double);
begin
  FValue := _Value;
end;

function TEMTFElectrode.Addnumber: String;
begin;
  Result := Fnumber;
  FnumberExsit := True;
end;

procedure TEMTFElectrode.Setnumber(const _Value: String);
begin
  FnumberExsit := True;
  Fnumber := _Value;
end;

procedure TEMTFElectrode.numberRemove;
begin
  if FnumberExsit then
  begin
    FnumberExsit := False;
  end;
end;

procedure TEMTFElectrode.AddnumberEvent(Sender: TObject);
begin
  Addnumber;
end;

{  Period}
constructor TEMTFPeriodData.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFPeriodData.Destroy;
begin
  if FTFExsit then
    FTF.Free;
  if FTFVARExsit then
    FTFVAR.Free;
  if FINVSIGCOVExsit then
    FINVSIGCOV.Free;
  if FRESIDCOVExsit then
    FRESIDCOV.Free;
  if FZExsit then
    FZ.Free;
  if FZVARExsit then
    FZVAR.Free;
  if FZINVSIGCOVExsit then
    FZINVSIGCOV.Free;
  if FZRESIDCOVExsit then
    FZRESIDCOV.Free;
  if FTExsit then
    FT.Free;
  if FTVARExsit then
    FTVAR.Free;
  if FTINVSIGCOVExsit then
    FTINVSIGCOV.Free;
  if FTRESIDCOVExsit then
    FTRESIDCOV.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFPeriodData.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'TF' then
      begin
        FTF := TEMTFTFData.Create(Self);
        FTF.FromXML(nodeTmp);
        FTFExsit := True;
      end
      else if nodeTmp.NodeName = 'TFVAR' then
      begin
        FTFVAR := TEMTFTFData.Create(Self);
        FTFVAR.FromXML(nodeTmp);
        FTFVARExsit := True;
      end
      else if nodeTmp.NodeName = 'INVSIGCOV' then
      begin
        FINVSIGCOV := TEMTFCOVData.Create(Self);
        FINVSIGCOV.FromXML(nodeTmp);
        FINVSIGCOVExsit := True;
      end
      else if nodeTmp.NodeName = 'RESIDCOV' then
      begin
        FRESIDCOV := TEMTFCOVData.Create(Self);
        FRESIDCOV.FromXML(nodeTmp);
        FRESIDCOVExsit := True;
      end
      else if nodeTmp.NodeName = 'Z' then
      begin
        FZ := TEMTFZ.Create(Self);
        FZ.FromXML(nodeTmp);
        FZExsit := True;
      end
      else if nodeTmp.NodeName = 'Z.VAR' then
      begin
        FZVAR := TEMTFZ.Create(Self);
        FZVAR.FromXML(nodeTmp);
        FZVARExsit := True;
      end
      else if nodeTmp.NodeName = 'Z.INVSIGCOV' then
      begin
        FZINVSIGCOV := TEMTFZ.Create(Self);
        FZINVSIGCOV.FromXML(nodeTmp);
        FZINVSIGCOVExsit := True;
      end
      else if nodeTmp.NodeName = 'Z.RESIDCOV' then
      begin
        FZRESIDCOV := TEMTFZ.Create(Self);
        FZRESIDCOV.FromXML(nodeTmp);
        FZRESIDCOVExsit := True;
      end
      else if nodeTmp.NodeName = 'T' then
      begin
        FT := TEMTFZ.Create(Self);
        FT.FromXML(nodeTmp);
        FTExsit := True;
      end
      else if nodeTmp.NodeName = 'T.VAR' then
      begin
        FTVAR := TEMTFZ.Create(Self);
        FTVAR.FromXML(nodeTmp);
        FTVARExsit := True;
      end
      else if nodeTmp.NodeName = 'T.INVSIGCOV' then
      begin
        FTINVSIGCOV := TEMTFZ.Create(Self);
        FTINVSIGCOV.FromXML(nodeTmp);
        FTINVSIGCOVExsit := True;
      end
      else if nodeTmp.NodeName = 'T.RESIDCOV' then
      begin
        FTRESIDCOV := TEMTFZ.Create(Self);
        FTRESIDCOV.FromXML(nodeTmp);
        FTRESIDCOVExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'value' then
      begin
        Fvalue := nodeTmp.Text.ToDouble;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Period Read XML Error!' + node.Xml);
  end;
end;

function TEMTFPeriodData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  valueTmp: IXMLNode;
  unitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Period';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FTFExsit then
      FTF.ToXML(node, 'TF');
    if FTFVARExsit then
      FTFVAR.ToXML(node, 'TFVAR');
    if FINVSIGCOVExsit then
      FINVSIGCOV.ToXML(node, 'INVSIGCOV');
    if FRESIDCOVExsit then
      FRESIDCOV.ToXML(node, 'RESIDCOV');
    if FZExsit then
      FZ.ToXML(node, 'Z');
    if FZVARExsit then
      FZVAR.ToXML(node, 'Z.VAR');
    if FZINVSIGCOVExsit then
      FZINVSIGCOV.ToXML(node, 'Z.INVSIGCOV');
    if FZRESIDCOVExsit then
      FZRESIDCOV.ToXML(node, 'Z.RESIDCOV');
    if FTExsit then
      FT.ToXML(node, 'T');
    if FTVARExsit then
      FTVAR.ToXML(node, 'T.VAR');
    if FTINVSIGCOVExsit then
      FTINVSIGCOV.ToXML(node, 'T.INVSIGCOV');
    if FTRESIDCOVExsit then
      FTRESIDCOV.ToXML(node, 'T.RESIDCOV');
    valueTmp := doc.CreateNode('value', ntAttribute);
    valueTmp.NodeValue := Fvalue.ToString;
    node.AttributeNodes.Add(valueTmp);
    unitsTmp := doc.CreateNode('units', ntAttribute);
    unitsTmp.NodeValue := Funits;
    node.AttributeNodes.Add(unitsTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFPeriodData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('value');
  TreeNodeShape.AddChild('units');
  if TFExsit then
  begin
    TF.TreeNodeShape := TreeNodeShape.AddChildObject('TF', TF);
    TF.ToTree;
  end;
  if TFVARExsit then
  begin
    TFVAR.TreeNodeShape := TreeNodeShape.AddChildObject('TFVAR', TFVAR);
    TFVAR.ToTree;
  end;
  if INVSIGCOVExsit then
  begin
    INVSIGCOV.TreeNodeShape := TreeNodeShape.AddChildObject('INVSIGCOV', INVSIGCOV);
    INVSIGCOV.ToTree;
  end;
  if RESIDCOVExsit then
  begin
    RESIDCOV.TreeNodeShape := TreeNodeShape.AddChildObject('RESIDCOV', RESIDCOV);
    RESIDCOV.ToTree;
  end;
  if ZExsit then
  begin
    Z.TreeNodeShape := TreeNodeShape.AddChildObject('Z', Z);
    Z.ToTree;
  end;
  if ZVARExsit then
  begin
    ZVAR.TreeNodeShape := TreeNodeShape.AddChildObject('ZVAR', ZVAR);
    ZVAR.ToTree;
  end;
  if ZINVSIGCOVExsit then
  begin
    ZINVSIGCOV.TreeNodeShape := TreeNodeShape.AddChildObject('ZINVSIGCOV', ZINVSIGCOV);
    ZINVSIGCOV.ToTree;
  end;
  if ZRESIDCOVExsit then
  begin
    ZRESIDCOV.TreeNodeShape := TreeNodeShape.AddChildObject('ZRESIDCOV', ZRESIDCOV);
    ZRESIDCOV.ToTree;
  end;
  if TExsit then
  begin
    T.TreeNodeShape := TreeNodeShape.AddChildObject('T', T);
    T.ToTree;
  end;
  if TVARExsit then
  begin
    TVAR.TreeNodeShape := TreeNodeShape.AddChildObject('TVAR', TVAR);
    TVAR.ToTree;
  end;
  if TINVSIGCOVExsit then
  begin
    TINVSIGCOV.TreeNodeShape := TreeNodeShape.AddChildObject('TINVSIGCOV', TINVSIGCOV);
    TINVSIGCOV.ToTree;
  end;
  if TRESIDCOVExsit then
  begin
    TRESIDCOV.TreeNodeShape := TreeNodeShape.AddChildObject('TRESIDCOV', TRESIDCOV);
    TRESIDCOV.ToTree;
  end;
end;

procedure TEMTFPeriodData.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  TFAddMenu: TMenuItem;
  TFVARAddMenu: TMenuItem;
  INVSIGCOVAddMenu: TMenuItem;
  RESIDCOVAddMenu: TMenuItem;
  ZAddMenu: TMenuItem;
  ZVARAddMenu: TMenuItem;
  ZINVSIGCOVAddMenu: TMenuItem;
  ZRESIDCOVAddMenu: TMenuItem;
  TAddMenu: TMenuItem;
  TVARAddMenu: TMenuItem;
  TINVSIGCOVAddMenu: TMenuItem;
  TRESIDCOVAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFPeriodDataPop) and Assigned(TEMTFPeriodDataTreeComponent) then
    begin
      TEMTFPeriodDataPop.Clear;
      TFAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TFAddMenu.Text := 'Add TF';
      TFAddMenu.OnClick := AddTFEvent;
      TEMTFPeriodDataPop.AddObject(TFAddMenu);
      TFVARAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TFVARAddMenu.Text := 'Add TFVAR';
      TFVARAddMenu.OnClick := AddTFVAREvent;
      TEMTFPeriodDataPop.AddObject(TFVARAddMenu);
      INVSIGCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      INVSIGCOVAddMenu.Text := 'Add INVSIGCOV';
      INVSIGCOVAddMenu.OnClick := AddINVSIGCOVEvent;
      TEMTFPeriodDataPop.AddObject(INVSIGCOVAddMenu);
      RESIDCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      RESIDCOVAddMenu.Text := 'Add RESIDCOV';
      RESIDCOVAddMenu.OnClick := AddRESIDCOVEvent;
      TEMTFPeriodDataPop.AddObject(RESIDCOVAddMenu);
      ZAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      ZAddMenu.Text := 'Add Z';
      ZAddMenu.OnClick := AddZEvent;
      TEMTFPeriodDataPop.AddObject(ZAddMenu);
      ZVARAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      ZVARAddMenu.Text := 'Add ZVAR';
      ZVARAddMenu.OnClick := AddZVAREvent;
      TEMTFPeriodDataPop.AddObject(ZVARAddMenu);
      ZINVSIGCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      ZINVSIGCOVAddMenu.Text := 'Add ZINVSIGCOV';
      ZINVSIGCOVAddMenu.OnClick := AddZINVSIGCOVEvent;
      TEMTFPeriodDataPop.AddObject(ZINVSIGCOVAddMenu);
      ZRESIDCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      ZRESIDCOVAddMenu.Text := 'Add ZRESIDCOV';
      ZRESIDCOVAddMenu.OnClick := AddZRESIDCOVEvent;
      TEMTFPeriodDataPop.AddObject(ZRESIDCOVAddMenu);
      TAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TAddMenu.Text := 'Add T';
      TAddMenu.OnClick := AddTEvent;
      TEMTFPeriodDataPop.AddObject(TAddMenu);
      TVARAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TVARAddMenu.Text := 'Add TVAR';
      TVARAddMenu.OnClick := AddTVAREvent;
      TEMTFPeriodDataPop.AddObject(TVARAddMenu);
      TINVSIGCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TINVSIGCOVAddMenu.Text := 'Add TINVSIGCOV';
      TINVSIGCOVAddMenu.OnClick := AddTINVSIGCOVEvent;
      TEMTFPeriodDataPop.AddObject(TINVSIGCOVAddMenu);
      TRESIDCOVAddMenu := TMenuItem.Create(TEMTFPeriodDataPop);
      TRESIDCOVAddMenu.Text := 'Add TRESIDCOV';
      TRESIDCOVAddMenu.OnClick := AddTRESIDCOVEvent;
      TEMTFPeriodDataPop.AddObject(TRESIDCOVAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFPeriodDataTreeComponent.ClientToScreen(pt);
      TEMTFPeriodDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFPeriodData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFPeriodDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('value');
  Types_Value.Add(xs_double);
  _Values_Value.Add(value.ToString);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  TEMTFPeriodDataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFPeriodData.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        value := _Value.ToDouble;
      end;
    1:
      begin
        units := _Value;
      end;
  end;
  ToTree;
end;

procedure TEMTFPeriodData.Setvalue(const _Value: Double);
begin
  Fvalue := _Value;
end;

procedure TEMTFPeriodData.Setunits(const _Value: String);
begin
  Funits := _Value;
end;

function TEMTFPeriodData.AddTF: TEMTFTFData;
begin;
  if not FTFExsit then
    FTF := TEMTFTFData.Create(Self);
  Result := FTF;
  FTFExsit := True;
end;

procedure TEMTFPeriodData.SetTF(const _Value: TEMTFTFData);
begin
  if FTFExsit then
    FTF.Free;
  FTFExsit := True;
  FTF := _Value;
  FTF.Parent := Self;
end;

procedure TEMTFPeriodData.TFRemove;
begin
  if FTFExsit then
  begin
    FTF.Free;
    FTFExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTFEvent(Sender: TObject);
begin
  AddTF;
  FTF.ToTree;
end;

function TEMTFPeriodData.AddTFVAR: TEMTFTFData;
begin;
  if not FTFVARExsit then
    FTFVAR := TEMTFTFData.Create(Self);
  Result := FTFVAR;
  FTFVARExsit := True;
end;

procedure TEMTFPeriodData.SetTFVAR(const _Value: TEMTFTFData);
begin
  if FTFVARExsit then
    FTFVAR.Free;
  FTFVARExsit := True;
  FTFVAR := _Value;
  FTFVAR.Parent := Self;
end;

procedure TEMTFPeriodData.TFVARRemove;
begin
  if FTFVARExsit then
  begin
    FTFVAR.Free;
    FTFVARExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTFVAREvent(Sender: TObject);
begin
  AddTFVAR;
  FTFVAR.ToTree;
end;

function TEMTFPeriodData.AddINVSIGCOV: TEMTFCOVData;
begin;
  if not FINVSIGCOVExsit then
    FINVSIGCOV := TEMTFCOVData.Create(Self);
  Result := FINVSIGCOV;
  FINVSIGCOVExsit := True;
end;

procedure TEMTFPeriodData.SetINVSIGCOV(const _Value: TEMTFCOVData);
begin
  if FINVSIGCOVExsit then
    FINVSIGCOV.Free;
  FINVSIGCOVExsit := True;
  FINVSIGCOV := _Value;
  FINVSIGCOV.Parent := Self;
end;

procedure TEMTFPeriodData.INVSIGCOVRemove;
begin
  if FINVSIGCOVExsit then
  begin
    FINVSIGCOV.Free;
    FINVSIGCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddINVSIGCOVEvent(Sender: TObject);
begin
  AddINVSIGCOV;
  FINVSIGCOV.ToTree;
end;

function TEMTFPeriodData.AddRESIDCOV: TEMTFCOVData;
begin;
  if not FRESIDCOVExsit then
    FRESIDCOV := TEMTFCOVData.Create(Self);
  Result := FRESIDCOV;
  FRESIDCOVExsit := True;
end;

procedure TEMTFPeriodData.SetRESIDCOV(const _Value: TEMTFCOVData);
begin
  if FRESIDCOVExsit then
    FRESIDCOV.Free;
  FRESIDCOVExsit := True;
  FRESIDCOV := _Value;
  FRESIDCOV.Parent := Self;
end;

procedure TEMTFPeriodData.RESIDCOVRemove;
begin
  if FRESIDCOVExsit then
  begin
    FRESIDCOV.Free;
    FRESIDCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddRESIDCOVEvent(Sender: TObject);
begin
  AddRESIDCOV;
  FRESIDCOV.ToTree;
end;

function TEMTFPeriodData.AddZ: TEMTFZ;
begin;
  if not FZExsit then
    FZ := TEMTFZ.Create(Self);
  Result := FZ;
  FZExsit := True;
end;

procedure TEMTFPeriodData.SetZ(const _Value: TEMTFZ);
begin
  if FZExsit then
    FZ.Free;
  FZExsit := True;
  FZ := _Value;
  FZ.Parent := Self;
end;

procedure TEMTFPeriodData.ZRemove;
begin
  if FZExsit then
  begin
    FZ.Free;
    FZExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddZEvent(Sender: TObject);
begin
  AddZ;
  FZ.ToTree;
end;

function TEMTFPeriodData.AddZVAR: TEMTFZ;
begin;
  if not FZVARExsit then
    FZVAR := TEMTFZ.Create(Self);
  Result := FZVAR;
  FZVARExsit := True;
end;

procedure TEMTFPeriodData.SetZVAR(const _Value: TEMTFZ);
begin
  if FZVARExsit then
    FZVAR.Free;
  FZVARExsit := True;
  FZVAR := _Value;
  FZVAR.Parent := Self;
end;

procedure TEMTFPeriodData.ZVARRemove;
begin
  if FZVARExsit then
  begin
    FZVAR.Free;
    FZVARExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddZVAREvent(Sender: TObject);
begin
  AddZVAR;
  FZVAR.ToTree;
end;

function TEMTFPeriodData.AddZINVSIGCOV: TEMTFZ;
begin;
  if not FZINVSIGCOVExsit then
    FZINVSIGCOV := TEMTFZ.Create(Self);
  Result := FZINVSIGCOV;
  FZINVSIGCOVExsit := True;
end;

procedure TEMTFPeriodData.SetZINVSIGCOV(const _Value: TEMTFZ);
begin
  if FZINVSIGCOVExsit then
    FZINVSIGCOV.Free;
  FZINVSIGCOVExsit := True;
  FZINVSIGCOV := _Value;
  FZINVSIGCOV.Parent := Self;
end;

procedure TEMTFPeriodData.ZINVSIGCOVRemove;
begin
  if FZINVSIGCOVExsit then
  begin
    FZINVSIGCOV.Free;
    FZINVSIGCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddZINVSIGCOVEvent(Sender: TObject);
begin
  AddZINVSIGCOV;
  FZINVSIGCOV.ToTree;
end;

function TEMTFPeriodData.AddZRESIDCOV: TEMTFZ;
begin;
  if not FZRESIDCOVExsit then
    FZRESIDCOV := TEMTFZ.Create(Self);
  Result := FZRESIDCOV;
  FZRESIDCOVExsit := True;
end;

procedure TEMTFPeriodData.SetZRESIDCOV(const _Value: TEMTFZ);
begin
  if FZRESIDCOVExsit then
    FZRESIDCOV.Free;
  FZRESIDCOVExsit := True;
  FZRESIDCOV := _Value;
  FZRESIDCOV.Parent := Self;
end;

procedure TEMTFPeriodData.ZRESIDCOVRemove;
begin
  if FZRESIDCOVExsit then
  begin
    FZRESIDCOV.Free;
    FZRESIDCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddZRESIDCOVEvent(Sender: TObject);
begin
  AddZRESIDCOV;
  FZRESIDCOV.ToTree;
end;

function TEMTFPeriodData.AddT: TEMTFZ;
begin;
  if not FTExsit then
    FT := TEMTFZ.Create(Self);
  Result := FT;
  FTExsit := True;
end;

procedure TEMTFPeriodData.SetT(const _Value: TEMTFZ);
begin
  if FTExsit then
    FT.Free;
  FTExsit := True;
  FT := _Value;
  FT.Parent := Self;
end;

procedure TEMTFPeriodData.TRemove;
begin
  if FTExsit then
  begin
    FT.Free;
    FTExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTEvent(Sender: TObject);
begin
  AddT;
  FT.ToTree;
end;

function TEMTFPeriodData.AddTVAR: TEMTFZ;
begin;
  if not FTVARExsit then
    FTVAR := TEMTFZ.Create(Self);
  Result := FTVAR;
  FTVARExsit := True;
end;

procedure TEMTFPeriodData.SetTVAR(const _Value: TEMTFZ);
begin
  if FTVARExsit then
    FTVAR.Free;
  FTVARExsit := True;
  FTVAR := _Value;
  FTVAR.Parent := Self;
end;

procedure TEMTFPeriodData.TVARRemove;
begin
  if FTVARExsit then
  begin
    FTVAR.Free;
    FTVARExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTVAREvent(Sender: TObject);
begin
  AddTVAR;
  FTVAR.ToTree;
end;

function TEMTFPeriodData.AddTINVSIGCOV: TEMTFZ;
begin;
  if not FTINVSIGCOVExsit then
    FTINVSIGCOV := TEMTFZ.Create(Self);
  Result := FTINVSIGCOV;
  FTINVSIGCOVExsit := True;
end;

procedure TEMTFPeriodData.SetTINVSIGCOV(const _Value: TEMTFZ);
begin
  if FTINVSIGCOVExsit then
    FTINVSIGCOV.Free;
  FTINVSIGCOVExsit := True;
  FTINVSIGCOV := _Value;
  FTINVSIGCOV.Parent := Self;
end;

procedure TEMTFPeriodData.TINVSIGCOVRemove;
begin
  if FTINVSIGCOVExsit then
  begin
    FTINVSIGCOV.Free;
    FTINVSIGCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTINVSIGCOVEvent(Sender: TObject);
begin
  AddTINVSIGCOV;
  FTINVSIGCOV.ToTree;
end;

function TEMTFPeriodData.AddTRESIDCOV: TEMTFZ;
begin;
  if not FTRESIDCOVExsit then
    FTRESIDCOV := TEMTFZ.Create(Self);
  Result := FTRESIDCOV;
  FTRESIDCOVExsit := True;
end;

procedure TEMTFPeriodData.SetTRESIDCOV(const _Value: TEMTFZ);
begin
  if FTRESIDCOVExsit then
    FTRESIDCOV.Free;
  FTRESIDCOVExsit := True;
  FTRESIDCOV := _Value;
  FTRESIDCOV.Parent := Self;
end;

procedure TEMTFPeriodData.TRESIDCOVRemove;
begin
  if FTRESIDCOVExsit then
  begin
    FTRESIDCOV.Free;
    FTRESIDCOVExsit := False;
  end;
end;

procedure TEMTFPeriodData.AddTRESIDCOVEvent(Sender: TObject);
begin
  AddTRESIDCOV;
  FTRESIDCOV.ToTree;
end;

{  TFData}
constructor TEMTFTFData.Create(par: TXML = nil);
begin
  inherited Create(par);
  Fvalues := TList<TEMTFValue>.Create;
end;

destructor TEMTFTFData.Destroy;
begin
  valueClear;
  Fvalues.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFTFData.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  valueTmp: TEMTFValue;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'comment' then
      begin
        Fcomment := nodeTmp.Text;
        FcommentExsit := True;
      end
      else if nodeTmp.NodeName = 'size' then
      begin
        Fsize := nodeTmp.Text.ToInteger;
        FsizeExsit := True;
      end
      else if nodeTmp.NodeName = 'value' then
      begin
        valueTmp := TEMTFValue.Create(Self);
        valueTmp.FromXML(nodeTmp);
        Fvalues.Add(valueTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('TFData Read XML Error!' + node.Xml);
  end;
end;

function TEMTFTFData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  commentTmp: IXMLNode;
  sizeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'TFData';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FnameExsit then
    begin
      nameTmp := doc.CreateNode('name', ntElement);
      nameTmp.NodeValue := Fname;
      node.ChildNodes.Add(nameTmp);
    end;
    if FcommentExsit then
    begin
      commentTmp := doc.CreateNode('comment', ntElement);
      commentTmp.NodeValue := Fcomment;
      node.ChildNodes.Add(commentTmp);
    end;
    if FsizeExsit then
    begin
      sizeTmp := doc.CreateNode('size', ntElement);
      sizeTmp.NodeValue := Fsize.toString;
      node.ChildNodes.Add(sizeTmp);
    end;
    for I := 0 to Fvalues.Count - 1 do
       Fvalues.Items[I].ToXML(node, 'value');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFTFData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if commentExsit then
    TreeNodeShape.AddChild('comment');
  if sizeExsit then
    TreeNodeShape.AddChild('size');
  for I := 0 to valueCount - 1 do
  begin
    values[I].TreeNodeShape := TreeNodeShape.AddChildObject('value', value[I]);
    value[I].ToTree;
  end;
end;

procedure TEMTFTFData.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  nameAddMenu: TMenuItem;
  commentAddMenu: TMenuItem;
  sizeAddMenu: TMenuItem;
  valueAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFTFDataPop) and Assigned(TEMTFTFDataTreeComponent) then
    begin
      TEMTFTFDataPop.Clear;
      nameAddMenu := TMenuItem.Create(TEMTFTFDataPop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TEMTFTFDataPop.AddObject(nameAddMenu);
      commentAddMenu := TMenuItem.Create(TEMTFTFDataPop);
      commentAddMenu.Text := 'Add comment';
      commentAddMenu.OnClick := AddcommentEvent;
      TEMTFTFDataPop.AddObject(commentAddMenu);
      sizeAddMenu := TMenuItem.Create(TEMTFTFDataPop);
      sizeAddMenu.Text := 'Add size';
      sizeAddMenu.OnClick := AddsizeEvent;
      TEMTFTFDataPop.AddObject(sizeAddMenu);
      valueAddMenu := TMenuItem.Create(TEMTFTFDataPop);
      valueAddMenu.Text := 'Add value';
      valueAddMenu.OnClick := AddvalueEvent;
      TEMTFTFDataPop.AddObject(valueAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFTFDataTreeComponent.ClientToScreen(pt);
      TEMTFTFDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFTFData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFTFDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('comment');
  Types_Value.Add(xs_string);
  _Values_Value.Add(comment);
  Names_Value.Add('size');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(size.toString);
  TEMTFTFDataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFTFData.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        comment := _Value;
      end;
    2:
      begin
        size := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

function TEMTFTFData.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TEMTFTFData.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TEMTFTFData.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TEMTFTFData.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TEMTFTFData.Addcomment: String;
begin;
  Result := Fcomment;
  FcommentExsit := True;
end;

procedure TEMTFTFData.Setcomment(const _Value: String);
begin
  FcommentExsit := True;
  Fcomment := _Value;
end;

procedure TEMTFTFData.commentRemove;
begin
  if FcommentExsit then
  begin
    FcommentExsit := False;
  end;
end;

procedure TEMTFTFData.AddcommentEvent(Sender: TObject);
begin
  Addcomment;
end;

function TEMTFTFData.Addsize: Integer;
begin;
  Result := Fsize;
  FsizeExsit := True;
end;

procedure TEMTFTFData.Setsize(const _Value: Integer);
begin
  FsizeExsit := True;
  Fsize := _Value;
end;

procedure TEMTFTFData.sizeRemove;
begin
  if FsizeExsit then
  begin
    FsizeExsit := False;
  end;
end;

procedure TEMTFTFData.AddsizeEvent(Sender: TObject);
begin
  Addsize;
end;

function TEMTFTFData.Addvalue: TEMTFValue;
var
  valuetmp: TEMTFValue;
begin;
  valuetmp := TEMTFValue.Create(Self);
  Fvalues.Add(valuetmp);
  Result := valuetmp;
end;

procedure TEMTFTFData.Setvalues(const _Value: TList<TEMTFValue>);
begin
  valueClear;
  Fvalues := _Value;
end;

procedure TEMTFTFData.valueClear;
begin
  while Fvalues.Count > 0 do
  begin
    Fvalues.Items[0].Free;
    Fvalues.Delete(0);
  end;
end;

function TEMTFTFData.valueCount: Integer;
begin
  Result := Fvalues.Count;
end;

function TEMTFTFData.Getvalue(Index: Integer): TEMTFValue;
begin
  Result := Fvalues[Index];
end;

procedure TEMTFTFData.Setvalue(Index: Integer;
  const _Value: TEMTFValue);
begin
  _Value.Parent := Self;
  Fvalues[Index].Free;
  Fvalues[Index] := _Value;
end;

procedure TEMTFTFData.Removevalue(_Value: TEMTFValue);
begin
  Fvalues.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFTFData.Deletevalue(Index: Integer);
begin
  Fvalues.Items[Index].Free;
  Fvalues.Delete(Index);
end;

procedure TEMTFTFData.AddvalueEvent(Sender: TObject);
var
  tmp: TEMTFValue;
begin
  tmp := Addvalue;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('value', tmp);
  tmp.ToTree;
end;

{  Cov}
constructor TEMTFCOVData.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFCOVData.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFCOVData.FromXML(node: IXMLNode);
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
        Fname := nodeTmp.Text;
        FnameExsit := True;
      end
      else if nodeTmp.NodeName = 'comment' then
      begin
        Fcomment := nodeTmp.Text;
        FcommentExsit := True;
      end
      else if nodeTmp.NodeName = 'size' then
      begin
        Fsize := nodeTmp.Text.ToInteger;
        FsizeExsit := True;
      end
      else if nodeTmp.NodeName = 'value' then
      begin
        Fvalue := nodeTmp.Text;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Cov Read XML Error!' + node.Xml);
  end;
end;

function TEMTFCOVData.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  nameTmp: IXMLNode;
  commentTmp: IXMLNode;
  sizeTmp: IXMLNode;
  valueTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Cov';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FnameExsit then
    begin
      nameTmp := doc.CreateNode('name', ntElement);
      nameTmp.NodeValue := Fname;
      node.ChildNodes.Add(nameTmp);
    end;
    if FcommentExsit then
    begin
      commentTmp := doc.CreateNode('comment', ntElement);
      commentTmp.NodeValue := Fcomment;
      node.ChildNodes.Add(commentTmp);
    end;
    if FsizeExsit then
    begin
      sizeTmp := doc.CreateNode('size', ntElement);
      sizeTmp.NodeValue := Fsize.toString;
      node.ChildNodes.Add(sizeTmp);
    end;
    valueTmp := doc.CreateNode('value', ntElement);
    valueTmp.NodeValue := Fvalue;
    node.ChildNodes.Add(valueTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFCOVData.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if nameExsit then
    TreeNodeShape.AddChild('name');
  if commentExsit then
    TreeNodeShape.AddChild('comment');
  if sizeExsit then
    TreeNodeShape.AddChild('size');
  TreeNodeShape.AddChild('value');
end;

procedure TEMTFCOVData.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  nameAddMenu: TMenuItem;
  commentAddMenu: TMenuItem;
  sizeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFCOVDataPop) and Assigned(TEMTFCOVDataTreeComponent) then
    begin
      TEMTFCOVDataPop.Clear;
      nameAddMenu := TMenuItem.Create(TEMTFCOVDataPop);
      nameAddMenu.Text := 'Add name';
      nameAddMenu.OnClick := AddnameEvent;
      TEMTFCOVDataPop.AddObject(nameAddMenu);
      commentAddMenu := TMenuItem.Create(TEMTFCOVDataPop);
      commentAddMenu.Text := 'Add comment';
      commentAddMenu.OnClick := AddcommentEvent;
      TEMTFCOVDataPop.AddObject(commentAddMenu);
      sizeAddMenu := TMenuItem.Create(TEMTFCOVDataPop);
      sizeAddMenu.Text := 'Add size';
      sizeAddMenu.OnClick := AddsizeEvent;
      TEMTFCOVDataPop.AddObject(sizeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFCOVDataTreeComponent.ClientToScreen(pt);
      TEMTFCOVDataPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFCOVData.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFCOVDataXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(name);
  Names_Value.Add('comment');
  Types_Value.Add(xs_string);
  _Values_Value.Add(comment);
  Names_Value.Add('size');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(size.toString);
  Names_Value.Add('value');
  Types_Value.Add(xs_string);
  _Values_Value.Add(value);
  TEMTFCOVDataXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFCOVData.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        name := _Value;
      end;
    1:
      begin
        comment := _Value;
      end;
    2:
      begin
        size := _Value.ToInteger;
      end;
    3:
      begin
        value := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFCOVData.Addname: String;
begin;
  Result := Fname;
  FnameExsit := True;
end;

procedure TEMTFCOVData.Setname(const _Value: String);
begin
  FnameExsit := True;
  Fname := _Value;
end;

procedure TEMTFCOVData.nameRemove;
begin
  if FnameExsit then
  begin
    FnameExsit := False;
  end;
end;

procedure TEMTFCOVData.AddnameEvent(Sender: TObject);
begin
  Addname;
end;

function TEMTFCOVData.Addcomment: String;
begin;
  Result := Fcomment;
  FcommentExsit := True;
end;

procedure TEMTFCOVData.Setcomment(const _Value: String);
begin
  FcommentExsit := True;
  Fcomment := _Value;
end;

procedure TEMTFCOVData.commentRemove;
begin
  if FcommentExsit then
  begin
    FcommentExsit := False;
  end;
end;

procedure TEMTFCOVData.AddcommentEvent(Sender: TObject);
begin
  Addcomment;
end;

function TEMTFCOVData.Addsize: Integer;
begin;
  Result := Fsize;
  FsizeExsit := True;
end;

procedure TEMTFCOVData.Setsize(const _Value: Integer);
begin
  FsizeExsit := True;
  Fsize := _Value;
end;

procedure TEMTFCOVData.sizeRemove;
begin
  if FsizeExsit then
  begin
    FsizeExsit := False;
  end;
end;

procedure TEMTFCOVData.AddsizeEvent(Sender: TObject);
begin
  Addsize;
end;

procedure TEMTFCOVData.Setvalue(const _Value: String);
begin
  Fvalue := _Value;
end;

{  Z}
constructor TEMTFZ.Create(par: TXML = nil);
begin
  inherited Create(par);
  Fvalues := TList<TEMTFValue>.Create;
end;

destructor TEMTFZ.Destroy;
begin
  valueClear;
  Fvalues.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFZ.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  valueTmp: TEMTFValue;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'value' then
      begin
        valueTmp := TEMTFValue.Create(Self);
        valueTmp.FromXML(nodeTmp);
        Fvalues.Add(valueTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'type' then
      begin
        FZtype := nodeTmp.Text;
        FZtypeExsit := True;
      end
      else if nodeTmp.NodeName = 'size' then
      begin
        Fsize := nodeTmp.Text;
        FsizeExsit := True;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    end;
  except
    raise Exception.Create('Z Read XML Error!' + node.Xml);
  end;
end;

function TEMTFZ.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ZtypeTmp: IXMLNode;
  sizeTmp: IXMLNode;
  unitsTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Z';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to Fvalues.Count - 1 do
       Fvalues.Items[I].ToXML(node, 'value');
    if FZtypeExsit then 
    begin
      ZtypeTmp := doc.CreateNode('type', ntAttribute);
      ZtypeTmp.NodeValue := FZtype;
      node.AttributeNodes.Add(ZtypeTmp);
    end;
    if FsizeExsit then 
    begin
      sizeTmp := doc.CreateNode('size', ntAttribute);
      sizeTmp.NodeValue := Fsize;
      node.AttributeNodes.Add(sizeTmp);
    end;
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFZ.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if ZtypeExsit then
    TreeNodeShape.AddChild('Ztype');
  if sizeExsit then
    TreeNodeShape.AddChild('size');
  if unitsExsit then
    TreeNodeShape.AddChild('units');
  for I := 0 to valueCount - 1 do
  begin
    values[I].TreeNodeShape := TreeNodeShape.AddChildObject('value', value[I]);
    value[I].ToTree;
  end;
end;

procedure TEMTFZ.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ZtypeAddMenu: TMenuItem;
  sizeAddMenu: TMenuItem;
  unitsAddMenu: TMenuItem;
  valueAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFZPop) and Assigned(TEMTFZTreeComponent) then
    begin
      TEMTFZPop.Clear;
      ZtypeAddMenu := TMenuItem.Create(TEMTFZPop);
      ZtypeAddMenu.Text := 'Add Ztype';
      ZtypeAddMenu.OnClick := AddZtypeEvent;
      TEMTFZPop.AddObject(ZtypeAddMenu);
      sizeAddMenu := TMenuItem.Create(TEMTFZPop);
      sizeAddMenu.Text := 'Add size';
      sizeAddMenu.OnClick := AddsizeEvent;
      TEMTFZPop.AddObject(sizeAddMenu);
      unitsAddMenu := TMenuItem.Create(TEMTFZPop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFZPop.AddObject(unitsAddMenu);
      valueAddMenu := TMenuItem.Create(TEMTFZPop);
      valueAddMenu.Text := 'Add value';
      valueAddMenu.OnClick := AddvalueEvent;
      TEMTFZPop.AddObject(valueAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFZTreeComponent.ClientToScreen(pt);
      TEMTFZPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFZ.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFZXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Ztype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Ztype);
  Names_Value.Add('size');
  Types_Value.Add(xs_string);
  _Values_Value.Add(size);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  TEMTFZXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFZ.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Ztype := _Value;
      end;
    1:
      begin
        size := _Value;
      end;
    2:
      begin
        units := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFZ.AddZtype: String;
begin;
  Result := FZtype;
  FZtypeExsit := True;
end;

procedure TEMTFZ.SetZtype(const _Value: String);
begin
  FZtypeExsit := True;
  FZtype := _Value;
end;

procedure TEMTFZ.ZtypeRemove;
begin
  if FZtypeExsit then
  begin
    FZtypeExsit := False;
  end;
end;

procedure TEMTFZ.AddZtypeEvent(Sender: TObject);
begin
  AddZtype;
end;

function TEMTFZ.Addsize: String;
begin;
  Result := Fsize;
  FsizeExsit := True;
end;

procedure TEMTFZ.Setsize(const _Value: String);
begin
  FsizeExsit := True;
  Fsize := _Value;
end;

procedure TEMTFZ.sizeRemove;
begin
  if FsizeExsit then
  begin
    FsizeExsit := False;
  end;
end;

procedure TEMTFZ.AddsizeEvent(Sender: TObject);
begin
  Addsize;
end;

function TEMTFZ.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFZ.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFZ.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFZ.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

function TEMTFZ.Addvalue: TEMTFValue;
var
  valuetmp: TEMTFValue;
begin;
  valuetmp := TEMTFValue.Create(Self);
  Fvalues.Add(valuetmp);
  Result := valuetmp;
end;

procedure TEMTFZ.Setvalues(const _Value: TList<TEMTFValue>);
begin
  valueClear;
  Fvalues := _Value;
end;

procedure TEMTFZ.valueClear;
begin
  while Fvalues.Count > 0 do
  begin
    Fvalues.Items[0].Free;
    Fvalues.Delete(0);
  end;
end;

function TEMTFZ.valueCount: Integer;
begin
  Result := Fvalues.Count;
end;

function TEMTFZ.Getvalue(Index: Integer): TEMTFValue;
begin
  Result := Fvalues[Index];
end;

procedure TEMTFZ.Setvalue(Index: Integer;
  const _Value: TEMTFValue);
begin
  _Value.Parent := Self;
  Fvalues[Index].Free;
  Fvalues[Index] := _Value;
end;

procedure TEMTFZ.Removevalue(_Value: TEMTFValue);
begin
  Fvalues.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFZ.Deletevalue(Index: Integer);
begin
  Fvalues.Items[Index].Free;
  Fvalues.Delete(Index);
end;

procedure TEMTFZ.AddvalueEvent(Sender: TObject);
var
  tmp: TEMTFValue;
begin
  tmp := Addvalue;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('value', tmp);
  tmp.ToTree;
end;

{  Declination}
constructor TEMTFDeclination.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFDeclination.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFDeclination.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'epoch' then
      begin
        FEpoch := nodeTmp.Text.ToDouble;
      end;
    FValue := node.Text.ToDouble;
    end;
  except
    raise Exception.Create('Declination Read XML Error!' + node.Xml);
  end;
end;

function TEMTFDeclination.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ValueTmp: IXMLNode;
  EpochTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Declination';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    EpochTmp := doc.CreateNode('epoch', ntAttribute);
    EpochTmp.NodeValue := FEpoch.ToString;
    node.AttributeNodes.Add(EpochTmp);
    node.NodeValue := FValue.ToString;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFDeclination.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Value');
  TreeNodeShape.AddChild('Epoch');
end;

procedure TEMTFDeclination.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFDeclinationPop) and Assigned(TEMTFDeclinationTreeComponent) then
    begin
      TEMTFDeclinationPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TEMTFDeclinationTreeComponent.ClientToScreen(pt);
      TEMTFDeclinationPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFDeclination.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFDeclinationXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Value');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Value.ToString);
  Names_Value.Add('Epoch');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Epoch.ToString);
  TEMTFDeclinationXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFDeclination.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Value := _Value.ToDouble;
      end;
    1:
      begin
        Epoch := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

procedure TEMTFDeclination.SetValue(const _Value: Double);
begin
  FValue := _Value;
end;

procedure TEMTFDeclination.SetEpoch(const _Value: Double);
begin
  FEpoch := _Value;
end;

{  RemoteRef}
constructor TEMTFRemoteRef.Create(par: TXML = nil);
begin
  inherited Create(par);
  FRTypes := TList<String>.Create;
end;

destructor TEMTFRemoteRef.Destroy;
begin
  FRTypes.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFRemoteRef.FromXML(node: IXMLNode);
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
;
    FValue := node.Text;
    end;
  except
    raise Exception.Create('RemoteRef Read XML Error!' + node.Xml);
  end;
end;

function TEMTFRemoteRef.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ValueTmp: IXMLNode;
  RTypeTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'RemoteRef';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    node.NodeValue := FValue;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFRemoteRef.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Value');
  for I := 0 to RTypeCount - 1 do
  begin
    TreeNodeShape.AddChild('RType');
  end;
end;

procedure TEMTFRemoteRef.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  RTypeAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFRemoteRefPop) and Assigned(TEMTFRemoteRefTreeComponent) then
    begin
      TEMTFRemoteRefPop.Clear;
      RTypeAddMenu := TMenuItem.Create(TEMTFRemoteRefPop);
      RTypeAddMenu.Text := 'Add RType';
      RTypeAddMenu.OnClick := AddRTypeEvent;
      TEMTFRemoteRefPop.AddObject(RTypeAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFRemoteRefTreeComponent.ClientToScreen(pt);
      TEMTFRemoteRefPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFRemoteRef.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFRemoteRefXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Value');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Value);
  TEMTFRemoteRefXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFRemoteRef.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Value := _Value;
      end;
  end;
  ToTree;
end;

procedure TEMTFRemoteRef.SetValue(const _Value: String);
begin
  FValue := _Value;
end;

function TEMTFRemoteRef.AddRType: String;
var
RTypetmp: String;
begin;
  FRTypes.Add(RTypetmp);
  Result := RTypetmp;
end;

procedure TEMTFRemoteRef.SetRTypes(const _Value: TList<String>);
begin
  FRTypes.Clear;
  FRTypes := _Value;
end;

procedure TEMTFRemoteRef.RTypeClear;
begin
  FRTypes.Clear;
end;

function TEMTFRemoteRef.RTypeCount: Integer;
begin
  Result := FRTypes.Count;
end;

function TEMTFRemoteRef.GetRType(Index: Integer): String;
begin
  Result := FRTypes[Index];
end;

procedure TEMTFRemoteRef.SetRType(Index: Integer;
  const _Value: String);
begin
  FRTypes[Index] := _Value;
end;

procedure TEMTFRemoteRef.RemoveRType(_Value: String);
begin
  FRTypes.Remove(_Value);
end;

procedure TEMTFRemoteRef.DeleteRType(Index: Integer);
begin
  FRTypes.Delete(Index);
end;

procedure TEMTFRemoteRef.AddRTypeEvent(Sender: TObject);
begin
  AddRType;
end;

{  Comments}
constructor TEMTFComments.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFComments.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFComments.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'author' then
      begin
        FAuthor := nodeTmp.Text;
      end;
    FValue := node.Text;
    end;
  except
    raise Exception.Create('Comments Read XML Error!' + node.Xml);
  end;
end;

function TEMTFComments.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ValueTmp: IXMLNode;
  AuthorTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Comments';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    AuthorTmp := doc.CreateNode('author', ntAttribute);
    AuthorTmp.NodeValue := FAuthor;
    node.AttributeNodes.Add(AuthorTmp);
    node.NodeValue := FValue;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFComments.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Value');
  TreeNodeShape.AddChild('Author');
end;

procedure TEMTFComments.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFCommentsPop) and Assigned(TEMTFCommentsTreeComponent) then
    begin
      TEMTFCommentsPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TEMTFCommentsTreeComponent.ClientToScreen(pt);
      TEMTFCommentsPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFComments.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFCommentsXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Value');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Value);
  Names_Value.Add('Author');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Author);
  TEMTFCommentsXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFComments.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Value := _Value;
      end;
    1:
      begin
        Author := _Value;
      end;
  end;
  ToTree;
end;

procedure TEMTFComments.SetValue(const _Value: String);
begin
  FValue := _Value;
end;

procedure TEMTFComments.SetAuthor(const _Value: String);
begin
  FAuthor := _Value;
end;

{  EMTFFile}
constructor TEMTFFile.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFFile.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFFile.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Filename' then
      begin
        FFilename := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
        FDescriptionExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('EMTFFile Read XML Error!' + node.Xml);
  end;
end;

function TEMTFFile.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  FilenameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'EMTFFile';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FilenameTmp := doc.CreateNode('Filename', ntElement);
    FilenameTmp.NodeValue := FFilename;
    node.ChildNodes.Add(FilenameTmp);
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('Description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFFile.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Filename');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
end;

procedure TEMTFFile.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  DescriptionAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFFilePop) and Assigned(TEMTFFileTreeComponent) then
    begin
      TEMTFFilePop.Clear;
      DescriptionAddMenu := TMenuItem.Create(TEMTFFilePop);
      DescriptionAddMenu.Text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TEMTFFilePop.AddObject(DescriptionAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFFileTreeComponent.ClientToScreen(pt);
      TEMTFFilePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFFile.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFFileXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Filename');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Filename);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TEMTFFileXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFFile.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Filename := _Value;
      end;
    1:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

procedure TEMTFFile.SetFilename(const _Value: String);
begin
  FFilename := _Value;
end;

function TEMTFFile.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TEMTFFile.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TEMTFFile.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TEMTFFile.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

{  Copyright}
constructor TEMTFCopyright.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFCopyright.Destroy;
begin
  if FCitationExsit then
    FCitation.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFCopyright.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Citation' then
      begin
        FCitation := TEMTFCitation.Create(Self);
        FCitation.FromXML(nodeTmp);
        FCitationExsit := True;
      end
      else if nodeTmp.NodeName = 'ReleaseStatus' then
      begin
        FReleaseStatus := nodeTmp.Text;
        FReleaseStatusExsit := True;
      end
      else if nodeTmp.NodeName = 'ConditionsOfUse' then
      begin
        FConditionsOfUse := nodeTmp.Text;
        FConditionsOfUseExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Copyright Read XML Error!' + node.Xml);
  end;
end;

function TEMTFCopyright.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ReleaseStatusTmp: IXMLNode;
  ConditionsOfUseTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Copyright';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FCitationExsit then
      FCitation.ToXML(node, 'Citation');
    if FReleaseStatusExsit then
    begin
      ReleaseStatusTmp := doc.CreateNode('ReleaseStatus', ntElement);
      ReleaseStatusTmp.NodeValue := FReleaseStatus;
      node.ChildNodes.Add(ReleaseStatusTmp);
    end;
    if FConditionsOfUseExsit then
    begin
      ConditionsOfUseTmp := doc.CreateNode('ConditionsOfUse', ntElement);
      ConditionsOfUseTmp.NodeValue := FConditionsOfUse;
      node.ChildNodes.Add(ConditionsOfUseTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFCopyright.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if CitationExsit then
  begin
    Citation.TreeNodeShape := TreeNodeShape.AddChildObject('Citation', Citation);
    Citation.ToTree;
  end;
  if ReleaseStatusExsit then
    TreeNodeShape.AddChild('ReleaseStatus');
  if ConditionsOfUseExsit then
    TreeNodeShape.AddChild('ConditionsOfUse');
end;

procedure TEMTFCopyright.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  CitationAddMenu: TMenuItem;
  ReleaseStatusAddMenu: TMenuItem;
  ConditionsOfUseAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFCopyrightPop) and Assigned(TEMTFCopyrightTreeComponent) then
    begin
      TEMTFCopyrightPop.Clear;
      CitationAddMenu := TMenuItem.Create(TEMTFCopyrightPop);
      CitationAddMenu.Text := 'Add Citation';
      CitationAddMenu.OnClick := AddCitationEvent;
      TEMTFCopyrightPop.AddObject(CitationAddMenu);
      ReleaseStatusAddMenu := TMenuItem.Create(TEMTFCopyrightPop);
      ReleaseStatusAddMenu.Text := 'Add ReleaseStatus';
      ReleaseStatusAddMenu.OnClick := AddReleaseStatusEvent;
      TEMTFCopyrightPop.AddObject(ReleaseStatusAddMenu);
      ConditionsOfUseAddMenu := TMenuItem.Create(TEMTFCopyrightPop);
      ConditionsOfUseAddMenu.Text := 'Add ConditionsOfUse';
      ConditionsOfUseAddMenu.OnClick := AddConditionsOfUseEvent;
      TEMTFCopyrightPop.AddObject(ConditionsOfUseAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFCopyrightTreeComponent.ClientToScreen(pt);
      TEMTFCopyrightPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFCopyright.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFCopyrightXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('ReleaseStatus');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ReleaseStatus);
  Names_Value.Add('ConditionsOfUse');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ConditionsOfUse);
  TEMTFCopyrightXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFCopyright.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        ReleaseStatus := _Value;
      end;
    1:
      begin
        ConditionsOfUse := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFCopyright.AddCitation: TEMTFCitation;
begin;
  if not FCitationExsit then
    FCitation := TEMTFCitation.Create(Self);
  Result := FCitation;
  FCitationExsit := True;
end;

procedure TEMTFCopyright.SetCitation(const _Value: TEMTFCitation);
begin
  if FCitationExsit then
    FCitation.Free;
  FCitationExsit := True;
  FCitation := _Value;
  FCitation.Parent := Self;
end;

procedure TEMTFCopyright.CitationRemove;
begin
  if FCitationExsit then
  begin
    FCitation.Free;
    FCitationExsit := False;
  end;
end;

procedure TEMTFCopyright.AddCitationEvent(Sender: TObject);
begin
  AddCitation;
  FCitation.ToTree;
end;

function TEMTFCopyright.AddReleaseStatus: String;
begin;
  Result := FReleaseStatus;
  FReleaseStatusExsit := True;
end;

procedure TEMTFCopyright.SetReleaseStatus(const _Value: String);
begin
  FReleaseStatusExsit := True;
  FReleaseStatus := _Value;
end;

procedure TEMTFCopyright.ReleaseStatusRemove;
begin
  if FReleaseStatusExsit then
  begin
    FReleaseStatusExsit := False;
  end;
end;

procedure TEMTFCopyright.AddReleaseStatusEvent(Sender: TObject);
begin
  AddReleaseStatus;
end;

function TEMTFCopyright.AddConditionsOfUse: String;
begin;
  Result := FConditionsOfUse;
  FConditionsOfUseExsit := True;
end;

procedure TEMTFCopyright.SetConditionsOfUse(const _Value: String);
begin
  FConditionsOfUseExsit := True;
  FConditionsOfUse := _Value;
end;

procedure TEMTFCopyright.ConditionsOfUseRemove;
begin
  if FConditionsOfUseExsit then
  begin
    FConditionsOfUseExsit := False;
  end;
end;

procedure TEMTFCopyright.AddConditionsOfUseEvent(Sender: TObject);
begin
  AddConditionsOfUse;
end;

{  Citation}
constructor TEMTFCitation.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFCitation.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFCitation.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Title' then
      begin
        FTitle := nodeTmp.Text;
        FTitleExsit := True;
      end
      else if nodeTmp.NodeName = 'Authors' then
      begin
        FAuthors := nodeTmp.Text;
        FAuthorsExsit := True;
      end
      else if nodeTmp.NodeName = 'Year' then
      begin
        FYear := nodeTmp.Text.ToInteger;
        FYearExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Citation Read XML Error!' + node.Xml);
  end;
end;

function TEMTFCitation.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  TitleTmp: IXMLNode;
  AuthorsTmp: IXMLNode;
  YearTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Citation';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FTitleExsit then
    begin
      TitleTmp := doc.CreateNode('Title', ntElement);
      TitleTmp.NodeValue := FTitle;
      node.ChildNodes.Add(TitleTmp);
    end;
    if FAuthorsExsit then
    begin
      AuthorsTmp := doc.CreateNode('Authors', ntElement);
      AuthorsTmp.NodeValue := FAuthors;
      node.ChildNodes.Add(AuthorsTmp);
    end;
    if FYearExsit then
    begin
      YearTmp := doc.CreateNode('Year', ntElement);
      YearTmp.NodeValue := FYear.toString;
      node.ChildNodes.Add(YearTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFCitation.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if TitleExsit then
    TreeNodeShape.AddChild('Title');
  if AuthorsExsit then
    TreeNodeShape.AddChild('Authors');
  if YearExsit then
    TreeNodeShape.AddChild('Year');
end;

procedure TEMTFCitation.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  TitleAddMenu: TMenuItem;
  AuthorsAddMenu: TMenuItem;
  YearAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFCitationPop) and Assigned(TEMTFCitationTreeComponent) then
    begin
      TEMTFCitationPop.Clear;
      TitleAddMenu := TMenuItem.Create(TEMTFCitationPop);
      TitleAddMenu.Text := 'Add Title';
      TitleAddMenu.OnClick := AddTitleEvent;
      TEMTFCitationPop.AddObject(TitleAddMenu);
      AuthorsAddMenu := TMenuItem.Create(TEMTFCitationPop);
      AuthorsAddMenu.Text := 'Add Authors';
      AuthorsAddMenu.OnClick := AddAuthorsEvent;
      TEMTFCitationPop.AddObject(AuthorsAddMenu);
      YearAddMenu := TMenuItem.Create(TEMTFCitationPop);
      YearAddMenu.Text := 'Add Year';
      YearAddMenu.OnClick := AddYearEvent;
      TEMTFCitationPop.AddObject(YearAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFCitationTreeComponent.ClientToScreen(pt);
      TEMTFCitationPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFCitation.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFCitationXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Title');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Title);
  Names_Value.Add('Authors');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Authors);
  Names_Value.Add('Year');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Year.toString);
  TEMTFCitationXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFCitation.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Title := _Value;
      end;
    1:
      begin
        Authors := _Value;
      end;
    2:
      begin
        Year := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

function TEMTFCitation.AddTitle: String;
begin;
  Result := FTitle;
  FTitleExsit := True;
end;

procedure TEMTFCitation.SetTitle(const _Value: String);
begin
  FTitleExsit := True;
  FTitle := _Value;
end;

procedure TEMTFCitation.TitleRemove;
begin
  if FTitleExsit then
  begin
    FTitleExsit := False;
  end;
end;

procedure TEMTFCitation.AddTitleEvent(Sender: TObject);
begin
  AddTitle;
end;

function TEMTFCitation.AddAuthors: String;
begin;
  Result := FAuthors;
  FAuthorsExsit := True;
end;

procedure TEMTFCitation.SetAuthors(const _Value: String);
begin
  FAuthorsExsit := True;
  FAuthors := _Value;
end;

procedure TEMTFCitation.AuthorsRemove;
begin
  if FAuthorsExsit then
  begin
    FAuthorsExsit := False;
  end;
end;

procedure TEMTFCitation.AddAuthorsEvent(Sender: TObject);
begin
  AddAuthors;
end;

function TEMTFCitation.AddYear: Integer;
begin;
  Result := FYear;
  FYearExsit := True;
end;

procedure TEMTFCitation.SetYear(const _Value: Integer);
begin
  FYearExsit := True;
  FYear := _Value;
end;

procedure TEMTFCitation.YearRemove;
begin
  if FYearExsit then
  begin
    FYearExsit := False;
  end;
end;

procedure TEMTFCitation.AddYearEvent(Sender: TObject);
begin
  AddYear;
end;

{  StatisticalEstimates}
constructor TEMTFStatisticalEstimates.Create(par: TXML = nil);
begin
  inherited Create(par);
  FEstimates := TList<TEMTFEstimate>.Create;
end;

destructor TEMTFStatisticalEstimates.Destroy;
begin
  EstimateClear;
  FEstimates.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFStatisticalEstimates.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  EstimateTmp: TEMTFEstimate;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Estimate' then
      begin
        EstimateTmp := TEMTFEstimate.Create(Self);
        EstimateTmp.FromXML(nodeTmp);
        FEstimates.Add(EstimateTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('StatisticalEstimates Read XML Error!' + node.Xml);
  end;
end;

function TEMTFStatisticalEstimates.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'StatisticalEstimates';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FEstimates.Count - 1 do
       FEstimates.Items[I].ToXML(node, 'Estimate');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFStatisticalEstimates.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to EstimateCount - 1 do
  begin
    Estimates[I].TreeNodeShape := TreeNodeShape.AddChildObject('Estimate', Estimate[I]);
    Estimate[I].ToTree;
  end;
end;

procedure TEMTFStatisticalEstimates.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  EstimateAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFStatisticalEstimatesPop) and Assigned(TEMTFStatisticalEstimatesTreeComponent) then
    begin
      TEMTFStatisticalEstimatesPop.Clear;
      EstimateAddMenu := TMenuItem.Create(TEMTFStatisticalEstimatesPop);
      EstimateAddMenu.Text := 'Add Estimate';
      EstimateAddMenu.OnClick := AddEstimateEvent;
      TEMTFStatisticalEstimatesPop.AddObject(EstimateAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFStatisticalEstimatesTreeComponent.ClientToScreen(pt);
      TEMTFStatisticalEstimatesPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFStatisticalEstimates.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFStatisticalEstimatesXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TEMTFStatisticalEstimatesXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFStatisticalEstimates.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

function TEMTFStatisticalEstimates.AddEstimate: TEMTFEstimate;
var
  Estimatetmp: TEMTFEstimate;
begin;
  Estimatetmp := TEMTFEstimate.Create(Self);
  FEstimates.Add(Estimatetmp);
  Result := Estimatetmp;
end;

procedure TEMTFStatisticalEstimates.SetEstimates(const _Value: TList<TEMTFEstimate>);
begin
  EstimateClear;
  FEstimates := _Value;
end;

procedure TEMTFStatisticalEstimates.EstimateClear;
begin
  while FEstimates.Count > 0 do
  begin
    FEstimates.Items[0].Free;
    FEstimates.Delete(0);
  end;
end;

function TEMTFStatisticalEstimates.EstimateCount: Integer;
begin
  Result := FEstimates.Count;
end;

function TEMTFStatisticalEstimates.GetEstimate(Index: Integer): TEMTFEstimate;
begin
  Result := FEstimates[Index];
end;

procedure TEMTFStatisticalEstimates.SetEstimate(Index: Integer;
  const _Value: TEMTFEstimate);
begin
  _Value.Parent := Self;
  FEstimates[Index].Free;
  FEstimates[Index] := _Value;
end;

procedure TEMTFStatisticalEstimates.RemoveEstimate(_Value: TEMTFEstimate);
begin
  FEstimates.Remove(_Value);
  _Value.Free;
end;

procedure TEMTFStatisticalEstimates.DeleteEstimate(Index: Integer);
begin
  FEstimates.Items[Index].Free;
  FEstimates.Delete(Index);
end;

procedure TEMTFStatisticalEstimates.AddEstimateEvent(Sender: TObject);
var
  tmp: TEMTFEstimate;
begin
  tmp := AddEstimate;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Estimate', tmp);
  tmp.ToTree;
end;

{  Estimate}
constructor TEMTFEstimate.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFEstimate.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFEstimate.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
        FDescriptionExsit := True;
      end
      else if nodeTmp.NodeName = 'ExternalUrl' then
      begin
        FExternalUrl := nodeTmp.Text;
        FExternalUrlExsit := True;
      end
      else if nodeTmp.NodeName = 'Intention' then
      begin
        FIntention := nodeTmp.Text;
        FIntentionExsit := True;
      end
      else if nodeTmp.NodeName = 'Tag' then
      begin
        FTag := nodeTmp.Text;
        FTagExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        FEType := nodeTmp.Text;
        FETypeExsit := True;
      end;
    end;
  except
    raise Exception.Create('Estimate Read XML Error!' + node.Xml);
  end;
end;

function TEMTFEstimate.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  ETypeTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  ExternalUrlTmp: IXMLNode;
  IntentionTmp: IXMLNode;
  TagTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Estimate';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FDescriptionExsit then
    begin
      DescriptionTmp := doc.CreateNode('Description', ntElement);
      DescriptionTmp.NodeValue := FDescription;
      node.ChildNodes.Add(DescriptionTmp);
    end;
    if FExternalUrlExsit then
    begin
      ExternalUrlTmp := doc.CreateNode('ExternalUrl', ntElement);
      ExternalUrlTmp.NodeValue := FExternalUrl;
      node.ChildNodes.Add(ExternalUrlTmp);
    end;
    if FIntentionExsit then
    begin
      IntentionTmp := doc.CreateNode('Intention', ntElement);
      IntentionTmp.NodeValue := FIntention;
      node.ChildNodes.Add(IntentionTmp);
    end;
    if FTagExsit then
    begin
      TagTmp := doc.CreateNode('Tag', ntElement);
      TagTmp.NodeValue := FTag;
      node.ChildNodes.Add(TagTmp);
    end;
    if FNameExsit then 
    begin
      NameTmp := doc.CreateNode('name', ntAttribute);
      NameTmp.NodeValue := FName;
      node.AttributeNodes.Add(NameTmp);
    end;
    if FETypeExsit then 
    begin
      ETypeTmp := doc.CreateNode('type', ntAttribute);
      ETypeTmp.NodeValue := FEType;
      node.AttributeNodes.Add(ETypeTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFEstimate.ToTree;
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
  if ETypeExsit then
    TreeNodeShape.AddChild('EType');
  if DescriptionExsit then
    TreeNodeShape.AddChild('Description');
  if ExternalUrlExsit then
    TreeNodeShape.AddChild('ExternalUrl');
  if IntentionExsit then
    TreeNodeShape.AddChild('Intention');
  if TagExsit then
    TreeNodeShape.AddChild('Tag');
end;

procedure TEMTFEstimate.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  NameAddMenu: TMenuItem;
  ETypeAddMenu: TMenuItem;
  DescriptionAddMenu: TMenuItem;
  ExternalUrlAddMenu: TMenuItem;
  IntentionAddMenu: TMenuItem;
  TagAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFEstimatePop) and Assigned(TEMTFEstimateTreeComponent) then
    begin
      TEMTFEstimatePop.Clear;
      NameAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFEstimatePop.AddObject(NameAddMenu);
      ETypeAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      ETypeAddMenu.Text := 'Add EType';
      ETypeAddMenu.OnClick := AddETypeEvent;
      TEMTFEstimatePop.AddObject(ETypeAddMenu);
      DescriptionAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      DescriptionAddMenu.Text := 'Add Description';
      DescriptionAddMenu.OnClick := AddDescriptionEvent;
      TEMTFEstimatePop.AddObject(DescriptionAddMenu);
      ExternalUrlAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      ExternalUrlAddMenu.Text := 'Add ExternalUrl';
      ExternalUrlAddMenu.OnClick := AddExternalUrlEvent;
      TEMTFEstimatePop.AddObject(ExternalUrlAddMenu);
      IntentionAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      IntentionAddMenu.Text := 'Add Intention';
      IntentionAddMenu.OnClick := AddIntentionEvent;
      TEMTFEstimatePop.AddObject(IntentionAddMenu);
      TagAddMenu := TMenuItem.Create(TEMTFEstimatePop);
      TagAddMenu.Text := 'Add Tag';
      TagAddMenu.OnClick := AddTagEvent;
      TEMTFEstimatePop.AddObject(TagAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFEstimateTreeComponent.ClientToScreen(pt);
      TEMTFEstimatePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFEstimate.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFEstimateXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('EType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(EType);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('ExternalUrl');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ExternalUrl);
  Names_Value.Add('Intention');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Intention);
  Names_Value.Add('Tag');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Tag);
  TEMTFEstimateXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFEstimate.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        EType := _Value;
      end;
    2:
      begin
        Description := _Value;
      end;
    3:
      begin
        ExternalUrl := _Value;
      end;
    4:
      begin
        Intention := _Value;
      end;
    5:
      begin
        Tag := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFEstimate.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFEstimate.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFEstimate.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFEstimate.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

function TEMTFEstimate.AddEType: String;
begin;
  Result := FEType;
  FETypeExsit := True;
end;

procedure TEMTFEstimate.SetEType(const _Value: String);
begin
  FETypeExsit := True;
  FEType := _Value;
end;

procedure TEMTFEstimate.ETypeRemove;
begin
  if FETypeExsit then
  begin
    FETypeExsit := False;
  end;
end;

procedure TEMTFEstimate.AddETypeEvent(Sender: TObject);
begin
  AddEType;
end;

function TEMTFEstimate.AddDescription: String;
begin;
  Result := FDescription;
  FDescriptionExsit := True;
end;

procedure TEMTFEstimate.SetDescription(const _Value: String);
begin
  FDescriptionExsit := True;
  FDescription := _Value;
end;

procedure TEMTFEstimate.DescriptionRemove;
begin
  if FDescriptionExsit then
  begin
    FDescriptionExsit := False;
  end;
end;

procedure TEMTFEstimate.AddDescriptionEvent(Sender: TObject);
begin
  AddDescription;
end;

function TEMTFEstimate.AddExternalUrl: String;
begin;
  Result := FExternalUrl;
  FExternalUrlExsit := True;
end;

procedure TEMTFEstimate.SetExternalUrl(const _Value: String);
begin
  FExternalUrlExsit := True;
  FExternalUrl := _Value;
end;

procedure TEMTFEstimate.ExternalUrlRemove;
begin
  if FExternalUrlExsit then
  begin
    FExternalUrlExsit := False;
  end;
end;

procedure TEMTFEstimate.AddExternalUrlEvent(Sender: TObject);
begin
  AddExternalUrl;
end;

function TEMTFEstimate.AddIntention: String;
begin;
  Result := FIntention;
  FIntentionExsit := True;
end;

procedure TEMTFEstimate.SetIntention(const _Value: String);
begin
  FIntentionExsit := True;
  FIntention := _Value;
end;

procedure TEMTFEstimate.IntentionRemove;
begin
  if FIntentionExsit then
  begin
    FIntentionExsit := False;
  end;
end;

procedure TEMTFEstimate.AddIntentionEvent(Sender: TObject);
begin
  AddIntention;
end;

function TEMTFEstimate.AddTag: String;
begin;
  Result := FTag;
  FTagExsit := True;
end;

procedure TEMTFEstimate.SetTag(const _Value: String);
begin
  FTagExsit := True;
  FTag := _Value;
end;

procedure TEMTFEstimate.TagRemove;
begin
  if FTagExsit then
  begin
    FTagExsit := False;
  end;
end;

procedure TEMTFEstimate.AddTagEvent(Sender: TObject);
begin
  AddTag;
end;

{  GridOrigin}
constructor TEMTFGridOrigin.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFGridOrigin.Destroy;
begin
  if FLocationExsit then
    FLocation.Free;
  if FSiteCoordsExsit then
    FSiteCoords.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFGridOrigin.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Location' then
      begin
        FLocation := TEMTFLocation.Create(Self);
        FLocation.FromXML(nodeTmp);
        FLocationExsit := True;
      end
      else if nodeTmp.NodeName = 'SiteCoords' then
      begin
        FSiteCoords := TEMTFSiteCoords.Create(Self);
        FSiteCoords.FromXML(nodeTmp);
        FSiteCoordsExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
        FNameExsit := True;
      end;
    end;
  except
    raise Exception.Create('GridOrigin Read XML Error!' + node.Xml);
  end;
end;

function TEMTFGridOrigin.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'GridOrigin';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FLocationExsit then
      FLocation.ToXML(node, 'Location');
    if FSiteCoordsExsit then
      FSiteCoords.ToXML(node, 'SiteCoords');
    if FNameExsit then 
    begin
      NameTmp := doc.CreateNode('name', ntAttribute);
      NameTmp.NodeValue := FName;
      node.AttributeNodes.Add(NameTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFGridOrigin.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if LocationExsit then
  begin
    Location.TreeNodeShape := TreeNodeShape.AddChildObject('Location', Location);
    Location.ToTree;
  end;
  if SiteCoordsExsit then
  begin
    SiteCoords.TreeNodeShape := TreeNodeShape.AddChildObject('SiteCoords', SiteCoords);
    SiteCoords.ToTree;
  end;
  if NameExsit then
    TreeNodeShape.AddChild('Name');
end;

procedure TEMTFGridOrigin.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  LocationAddMenu: TMenuItem;
  SiteCoordsAddMenu: TMenuItem;
  NameAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFGridOriginPop) and Assigned(TEMTFGridOriginTreeComponent) then
    begin
      TEMTFGridOriginPop.Clear;
      LocationAddMenu := TMenuItem.Create(TEMTFGridOriginPop);
      LocationAddMenu.Text := 'Add Location';
      LocationAddMenu.OnClick := AddLocationEvent;
      TEMTFGridOriginPop.AddObject(LocationAddMenu);
      SiteCoordsAddMenu := TMenuItem.Create(TEMTFGridOriginPop);
      SiteCoordsAddMenu.Text := 'Add SiteCoords';
      SiteCoordsAddMenu.OnClick := AddSiteCoordsEvent;
      TEMTFGridOriginPop.AddObject(SiteCoordsAddMenu);
      NameAddMenu := TMenuItem.Create(TEMTFGridOriginPop);
      NameAddMenu.Text := 'Add Name';
      NameAddMenu.OnClick := AddNameEvent;
      TEMTFGridOriginPop.AddObject(NameAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFGridOriginTreeComponent.ClientToScreen(pt);
      TEMTFGridOriginPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFGridOrigin.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFGridOriginXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  TEMTFGridOriginXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFGridOrigin.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
  end;
  ToTree;
end;

function TEMTFGridOrigin.AddLocation: TEMTFLocation;
begin;
  if not FLocationExsit then
    FLocation := TEMTFLocation.Create(Self);
  Result := FLocation;
  FLocationExsit := True;
end;

procedure TEMTFGridOrigin.SetLocation(const _Value: TEMTFLocation);
begin
  if FLocationExsit then
    FLocation.Free;
  FLocationExsit := True;
  FLocation := _Value;
  FLocation.Parent := Self;
end;

procedure TEMTFGridOrigin.LocationRemove;
begin
  if FLocationExsit then
  begin
    FLocation.Free;
    FLocationExsit := False;
  end;
end;

procedure TEMTFGridOrigin.AddLocationEvent(Sender: TObject);
begin
  AddLocation;
  FLocation.ToTree;
end;

function TEMTFGridOrigin.AddSiteCoords: TEMTFSiteCoords;
begin;
  if not FSiteCoordsExsit then
    FSiteCoords := TEMTFSiteCoords.Create(Self);
  Result := FSiteCoords;
  FSiteCoordsExsit := True;
end;

procedure TEMTFGridOrigin.SetSiteCoords(const _Value: TEMTFSiteCoords);
begin
  if FSiteCoordsExsit then
    FSiteCoords.Free;
  FSiteCoordsExsit := True;
  FSiteCoords := _Value;
  FSiteCoords.Parent := Self;
end;

procedure TEMTFGridOrigin.SiteCoordsRemove;
begin
  if FSiteCoordsExsit then
  begin
    FSiteCoords.Free;
    FSiteCoordsExsit := False;
  end;
end;

procedure TEMTFGridOrigin.AddSiteCoordsEvent(Sender: TObject);
begin
  AddSiteCoords;
  FSiteCoords.ToTree;
end;

function TEMTFGridOrigin.AddName: String;
begin;
  Result := FName;
  FNameExsit := True;
end;

procedure TEMTFGridOrigin.SetName(const _Value: String);
begin
  FNameExsit := True;
  FName := _Value;
end;

procedure TEMTFGridOrigin.NameRemove;
begin
  if FNameExsit then
  begin
    FNameExsit := False;
  end;
end;

procedure TEMTFGridOrigin.AddNameEvent(Sender: TObject);
begin
  AddName;
end;

{  SiteCoords}
constructor TEMTFSiteCoords.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFSiteCoords.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFSiteCoords.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'X' then
      begin
        FX := nodeTmp.Text.ToDouble;
        FXExsit := True;
      end
      else if nodeTmp.NodeName = 'Y' then
      begin
        FY := nodeTmp.Text.ToDouble;
        FYExsit := True;
      end
      else if nodeTmp.NodeName = 'Z' then
      begin
        FZ := nodeTmp.Text.ToDouble;
        FZExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'type' then
      begin
        FStype := nodeTmp.Text;
        FStypeExsit := True;
      end
      else if nodeTmp.NodeName = 'units' then
      begin
        Funits := nodeTmp.Text;
        FunitsExsit := True;
      end;
    end;
  except
    raise Exception.Create('SiteCoords Read XML Error!' + node.Xml);
  end;
end;

function TEMTFSiteCoords.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  StypeTmp: IXMLNode;
  unitsTmp: IXMLNode;
  XTmp: IXMLNode;
  YTmp: IXMLNode;
  ZTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'SiteCoords';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FXExsit then
    begin
      XTmp := doc.CreateNode('X', ntElement);
      XTmp.NodeValue := FX.ToString;
      node.ChildNodes.Add(XTmp);
    end;
    if FYExsit then
    begin
      YTmp := doc.CreateNode('Y', ntElement);
      YTmp.NodeValue := FY.ToString;
      node.ChildNodes.Add(YTmp);
    end;
    if FZExsit then
    begin
      ZTmp := doc.CreateNode('Z', ntElement);
      ZTmp.NodeValue := FZ.ToString;
      node.ChildNodes.Add(ZTmp);
    end;
    if FStypeExsit then 
    begin
      StypeTmp := doc.CreateNode('type', ntAttribute);
      StypeTmp.NodeValue := FStype;
      node.AttributeNodes.Add(StypeTmp);
    end;
    if FunitsExsit then 
    begin
      unitsTmp := doc.CreateNode('units', ntAttribute);
      unitsTmp.NodeValue := Funits;
      node.AttributeNodes.Add(unitsTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFSiteCoords.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if StypeExsit then
    TreeNodeShape.AddChild('Stype');
  if unitsExsit then
    TreeNodeShape.AddChild('units');
  if XExsit then
    TreeNodeShape.AddChild('X');
  if YExsit then
    TreeNodeShape.AddChild('Y');
  if ZExsit then
    TreeNodeShape.AddChild('Z');
end;

procedure TEMTFSiteCoords.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  StypeAddMenu: TMenuItem;
  unitsAddMenu: TMenuItem;
  XAddMenu: TMenuItem;
  YAddMenu: TMenuItem;
  ZAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFSiteCoordsPop) and Assigned(TEMTFSiteCoordsTreeComponent) then
    begin
      TEMTFSiteCoordsPop.Clear;
      StypeAddMenu := TMenuItem.Create(TEMTFSiteCoordsPop);
      StypeAddMenu.Text := 'Add Stype';
      StypeAddMenu.OnClick := AddStypeEvent;
      TEMTFSiteCoordsPop.AddObject(StypeAddMenu);
      unitsAddMenu := TMenuItem.Create(TEMTFSiteCoordsPop);
      unitsAddMenu.Text := 'Add units';
      unitsAddMenu.OnClick := AddunitsEvent;
      TEMTFSiteCoordsPop.AddObject(unitsAddMenu);
      XAddMenu := TMenuItem.Create(TEMTFSiteCoordsPop);
      XAddMenu.Text := 'Add X';
      XAddMenu.OnClick := AddXEvent;
      TEMTFSiteCoordsPop.AddObject(XAddMenu);
      YAddMenu := TMenuItem.Create(TEMTFSiteCoordsPop);
      YAddMenu.Text := 'Add Y';
      YAddMenu.OnClick := AddYEvent;
      TEMTFSiteCoordsPop.AddObject(YAddMenu);
      ZAddMenu := TMenuItem.Create(TEMTFSiteCoordsPop);
      ZAddMenu.Text := 'Add Z';
      ZAddMenu.OnClick := AddZEvent;
      TEMTFSiteCoordsPop.AddObject(ZAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFSiteCoordsTreeComponent.ClientToScreen(pt);
      TEMTFSiteCoordsPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFSiteCoords.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFSiteCoordsXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Stype');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Stype);
  Names_Value.Add('units');
  Types_Value.Add(xs_string);
  _Values_Value.Add(units);
  Names_Value.Add('X');
  Types_Value.Add(xs_double);
  _Values_Value.Add(X.ToString);
  Names_Value.Add('Y');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Y.ToString);
  Names_Value.Add('Z');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Z.ToString);
  TEMTFSiteCoordsXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFSiteCoords.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Stype := _Value;
      end;
    1:
      begin
        units := _Value;
      end;
    2:
      begin
        X := _Value.ToDouble;
      end;
    3:
      begin
        Y := _Value.ToDouble;
      end;
    4:
      begin
        Z := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TEMTFSiteCoords.AddStype: String;
begin;
  Result := FStype;
  FStypeExsit := True;
end;

procedure TEMTFSiteCoords.SetStype(const _Value: String);
begin
  FStypeExsit := True;
  FStype := _Value;
end;

procedure TEMTFSiteCoords.StypeRemove;
begin
  if FStypeExsit then
  begin
    FStypeExsit := False;
  end;
end;

procedure TEMTFSiteCoords.AddStypeEvent(Sender: TObject);
begin
  AddStype;
end;

function TEMTFSiteCoords.Addunits: String;
begin;
  Result := Funits;
  FunitsExsit := True;
end;

procedure TEMTFSiteCoords.Setunits(const _Value: String);
begin
  FunitsExsit := True;
  Funits := _Value;
end;

procedure TEMTFSiteCoords.unitsRemove;
begin
  if FunitsExsit then
  begin
    FunitsExsit := False;
  end;
end;

procedure TEMTFSiteCoords.AddunitsEvent(Sender: TObject);
begin
  Addunits;
end;

function TEMTFSiteCoords.AddX: Double;
begin;
  Result := FX;
  FXExsit := True;
end;

procedure TEMTFSiteCoords.SetX(const _Value: Double);
begin
  FXExsit := True;
  FX := _Value;
end;

procedure TEMTFSiteCoords.XRemove;
begin
  if FXExsit then
  begin
    FXExsit := False;
  end;
end;

procedure TEMTFSiteCoords.AddXEvent(Sender: TObject);
begin
  AddX;
end;

function TEMTFSiteCoords.AddY: Double;
begin;
  Result := FY;
  FYExsit := True;
end;

procedure TEMTFSiteCoords.SetY(const _Value: Double);
begin
  FYExsit := True;
  FY := _Value;
end;

procedure TEMTFSiteCoords.YRemove;
begin
  if FYExsit then
  begin
    FYExsit := False;
  end;
end;

procedure TEMTFSiteCoords.AddYEvent(Sender: TObject);
begin
  AddY;
end;

function TEMTFSiteCoords.AddZ: Double;
begin;
  Result := FZ;
  FZExsit := True;
end;

procedure TEMTFSiteCoords.SetZ(const _Value: Double);
begin
  FZExsit := True;
  FZ := _Value;
end;

procedure TEMTFSiteCoords.ZRemove;
begin
  if FZExsit then
  begin
    FZExsit := False;
  end;
end;

procedure TEMTFSiteCoords.AddZEvent(Sender: TObject);
begin
  AddZ;
end;

{  SiteLayout}
constructor TEMTFSiteLayout.Create(par: TXML = nil);
begin
  inherited Create(par);
  FInputChannels := TEMTFChannels.Create(Self);
  FOutputChannels := TEMTFChannels.Create(Self);
end;

destructor TEMTFSiteLayout.Destroy;
begin
  FInputChannels.Free;
  FOutputChannels.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFSiteLayout.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'InputChannels' then
      begin
        FInputChannels := TEMTFChannels.Create(Self);
        FInputChannels.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'OutputChannels' then
      begin
        FOutputChannels := TEMTFChannels.Create(Self);
        FOutputChannels.FromXML(nodeTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('SiteLayout Read XML Error!' + node.Xml);
  end;
end;

function TEMTFSiteLayout.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'SiteLayout';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    FInputChannels.ToXML(node, 'InputChannels');
    FOutputChannels.ToXML(node, 'OutputChannels');
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFSiteLayout.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  InputChannels.TreeNodeShape := TreeNodeShape.AddChildObject('InputChannels', InputChannels);
  InputChannels.ToTree;
  OutputChannels.TreeNodeShape := TreeNodeShape.AddChildObject('OutputChannels', OutputChannels);
  OutputChannels.ToTree;
end;

procedure TEMTFSiteLayout.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFSiteLayoutPop) and Assigned(TEMTFSiteLayoutTreeComponent) then
    begin
      TEMTFSiteLayoutPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TEMTFSiteLayoutTreeComponent.ClientToScreen(pt);
      TEMTFSiteLayoutPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFSiteLayout.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFSiteLayoutXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TEMTFSiteLayoutXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFSiteLayout.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

procedure TEMTFSiteLayout.SetInputChannels(const _Value: TEMTFChannels);
begin
  FInputChannels.Free;
  FInputChannels := _Value;
  FInputChannels.Parent := Self;
end;

procedure TEMTFSiteLayout.SetOutputChannels(const _Value: TEMTFChannels);
begin
  FOutputChannels.Free;
  FOutputChannels := _Value;
  FOutputChannels.Parent := Self;
end;

{  PeriodRange}
constructor TEMTFPeriodRange.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TEMTFPeriodRange.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TEMTFPeriodRange.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'min' then
      begin
        Fmin := nodeTmp.Text.ToDouble;
        FminExsit := True;
      end
      else if nodeTmp.NodeName = 'max' then
      begin
        Fmax := nodeTmp.Text.ToDouble;
        FmaxExsit := True;
      end;
    end;
  except
    raise Exception.Create('PeriodRange Read XML Error!' + node.Xml);
  end;
end;

function TEMTFPeriodRange.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  minTmp: IXMLNode;
  maxTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'PeriodRange';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    if FminExsit then 
    begin
      minTmp := doc.CreateNode('min', ntAttribute);
      minTmp.NodeValue := Fmin.ToString;
      node.AttributeNodes.Add(minTmp);
    end;
    if FmaxExsit then 
    begin
      maxTmp := doc.CreateNode('max', ntAttribute);
      maxTmp.NodeValue := Fmax.ToString;
      node.AttributeNodes.Add(maxTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TEMTFPeriodRange.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  if minExsit then
    TreeNodeShape.AddChild('min');
  if maxExsit then
    TreeNodeShape.AddChild('max');
end;

procedure TEMTFPeriodRange.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  minAddMenu: TMenuItem;
  maxAddMenu: TMenuItem;
begin
  ToInspector;
  EMTFXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TEMTFPeriodRangePop) and Assigned(TEMTFPeriodRangeTreeComponent) then
    begin
      TEMTFPeriodRangePop.Clear;
      minAddMenu := TMenuItem.Create(TEMTFPeriodRangePop);
      minAddMenu.Text := 'Add min';
      minAddMenu.OnClick := AddminEvent;
      TEMTFPeriodRangePop.AddObject(minAddMenu);
      maxAddMenu := TMenuItem.Create(TEMTFPeriodRangePop);
      maxAddMenu.Text := 'Add max';
      maxAddMenu.OnClick := AddmaxEvent;
      TEMTFPeriodRangePop.AddObject(maxAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TEMTFPeriodRangeTreeComponent.ClientToScreen(pt);
      TEMTFPeriodRangePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TEMTFPeriodRange.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TEMTFPeriodRangeXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('min');
  Types_Value.Add(xs_double);
  _Values_Value.Add(min.ToString);
  Names_Value.Add('max');
  Types_Value.Add(xs_double);
  _Values_Value.Add(max.ToString);
  TEMTFPeriodRangeXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TEMTFPeriodRange.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        min := _Value.ToDouble;
      end;
    1:
      begin
        max := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

function TEMTFPeriodRange.Addmin: Double;
begin;
  Result := Fmin;
  FminExsit := True;
end;

procedure TEMTFPeriodRange.Setmin(const _Value: Double);
begin
  FminExsit := True;
  Fmin := _Value;
end;

procedure TEMTFPeriodRange.minRemove;
begin
  if FminExsit then
  begin
    FminExsit := False;
  end;
end;

procedure TEMTFPeriodRange.AddminEvent(Sender: TObject);
begin
  Addmin;
end;

function TEMTFPeriodRange.Addmax: Double;
begin;
  Result := Fmax;
  FmaxExsit := True;
end;

procedure TEMTFPeriodRange.Setmax(const _Value: Double);
begin
  FmaxExsit := True;
  Fmax := _Value;
end;

procedure TEMTFPeriodRange.maxRemove;
begin
  if FmaxExsit then
  begin
    FmaxExsit := False;
  end;
end;

procedure TEMTFPeriodRange.AddmaxEvent(Sender: TObject);
begin
  Addmax;
end;



end.
