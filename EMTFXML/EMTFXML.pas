unit EMTFXML;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections,
  XMLCore, XMLLeafTypes, RunExeWait, FileHash,
  ClientScreen, FMXTee.Tree, FMX.Menus,
  EMTFXMLBase;

type

  TEMTFHelper = class helper for TEMTF
  private
  protected
  public
    procedure LoadEDI(fname: String);
  end;

  TEMTFSiteHelper = class helper for TEMTFSite
  private
  protected
  public
  end;

  TEMTFLocationHelper = class helper for TEMTFLocation
  private
  protected
  public
  end;

  TEMTFProcessingInfoHelper = class helper for TEMTFProcessingInfo
  private
  protected
  public
  end;

  TEMTFProcessingSoftwareHelper = class helper for TEMTFProcessingSoftware
  private
  protected
  public
  end;

  TEMTFInstrumentHelper = class helper for TEMTFInstrument
  private
  protected
  public
  end;

  TEMTFDipoleHelper = class helper for TEMTFDipole
  private
  protected
  public
  end;

  TEMTFDataTypesHelper = class helper for TEMTFDataTypes
  private
  protected
  public
  end;

  TEMTFDateTypeHelper = class helper for TEMTFDateType
  private
  protected
  public
  end;

  TEMTFChannelsHelper = class helper for TEMTFChannels
  private
  protected
  public
  end;

  TEMTFChannelHelper = class helper for TEMTFChannel
  private
  protected
  public
  end;

  TEMTFDataHelper = class helper for TEMTFData
  private
  protected
  public
  end;

  TEMTFValueHelper = class helper for TEMTFValue
  private
  protected
  public
  end;

  TEMTFProvenanceHelper = class helper for TEMTFProvenance
  private
  protected
  public
  end;

  TEMTFPersonHelper = class helper for TEMTFPerson
  private
  protected
  public
  end;

  TEMTFDataQualityNotesHelper = class helper for TEMTFDataQualityNotes
  private
  protected
  public
  end;

  TEMTFUnitValueHelper = class helper for TEMTFUnitValue
  private
  protected
  public
  end;

  TEMTFElectrodeHelper = class helper for TEMTFElectrode
  private
  protected
  public
  end;

  TEMTFPeriodDataHelper = class helper for TEMTFPeriodData
  private
  protected
  public
  end;

  TEMTFTFDataHelper = class helper for TEMTFTFData
  private
  protected
  public
  end;

  TEMTFCOVDataHelper = class helper for TEMTFCOVData
  private
  protected
  public
  end;

  TEMTFZHelper = class helper for TEMTFZ
  private
  protected
  public
  end;

  TEMTFDeclinationHelper = class helper for TEMTFDeclination
  private
  protected
  public
  end;

  TEMTFRemoteRefHelper = class helper for TEMTFRemoteRef
  private
  protected
  public
  end;

  TEMTFCommentsHelper = class helper for TEMTFComments
  private
  protected
  public
  end;

  TEMTFFileHelper = class helper for TEMTFFile
  private
  protected
  public
  end;

  TEMTFCopyrightHelper = class helper for TEMTFCopyright
  private
  protected
  public
  end;

  TEMTFCitationHelper = class helper for TEMTFCitation
  private
  protected
  public
  end;

  TEMTFStatisticalEstimatesHelper = class helper for TEMTFStatisticalEstimates
  private
  protected
  public
  end;

  TEMTFEstimateHelper = class helper for TEMTFEstimate
  private
  protected
  public
  end;

  TEMTFGridOriginHelper = class helper for TEMTFGridOrigin
  private
  protected
  public
  end;

  TEMTFSiteCoordsHelper = class helper for TEMTFSiteCoords
  private
  protected
  public
  end;

  TEMTFSiteLayoutHelper = class helper for TEMTFSiteLayout
  private
  protected
  public
  end;

  TEMTFPeriodRangeHelper = class helper for TEMTFPeriodRange
  private
  protected
  public
  end;

implementation

{ EM_TF }
procedure TEMTFHelper.LoadEDI(fname: String);
var
  f1, f2: tfilestream;
  fhs: String;
  Name: String;
begin
  if FileExists(fname) then
  begin
    name := ExtractFileNameNoExt(fname);
    f1 := tfilestream.Create(fname, fmopenread);
    fhs := GetFileStreamHash(f1);
    f1.Position := 0;
    try
      f2 := tfilestream.Create('./' + name + '.edi', fmopenwrite or fmcreate);
      try
        f2.CopyFrom(f1, f1.size);
      finally
        f2.Free;
      end;
    finally
      f1.Free;
    end;
  end;
  WinExecAndWait32('./edi2xml ' + name + '.edi ' + name + '.xml', False);
  Load('./' + name + '.xml');
  DeleteFile('./' + name + '.xml');
  DeleteFile('./' + name + '.edi');
end;
{ Site }
{ Location }
{ ProcessingInfo }
{ ProcessingSoftware }
{ Instrument }
{ Dipole }
{ DataTypes }
{ type }
{ Channels }
{ Channel }
{ Data }
{ value }
{ Provenance }
{ Person }
{ DataQualityNotes }
{ UnitValue }
{ Electrode }
{ Period }
{ TFData }
{ Cov }
{ Z }
{ Declination }
{ RemoteRef }
{ Comments }
{ EMTFFile }
{ Copyright }
{ Citation }
{ StatisticalEstimates }
{ Estimate }
{ GridOrigin }
{ SiteCoords }
{ SiteLayout }
{ PeriodRange }

end.
