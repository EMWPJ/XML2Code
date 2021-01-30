unit EMTFXMLFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,
  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,
  EMTFXML, EMTFXMLBase;

type
  TFrameEMTFXML = class(TFrame)
    EMTFXMLTree: TXMLTree;
    EMTFXMLIns: TXMLInspector;
    SplitterBottom: TSplitter;
    EMTFXMLMenuBar: TMenuBar;
    NewEMTFXMLButton: TMenuItem;
    OpenEMTFXMLButton: TMenuItem;
    SaveEMTFXMLButton: TMenuItem;
    OpenDialogEMTFXML: TOpenDialog;
    SaveDialogEMTFXML: TSaveDialog;
    procedure NewEMTFXML(Sender: TObject);
    procedure OpenEMTFXML(Sender: TObject);
    procedure SaveEMTFXML(Sender: TObject);
  private
    FEMTFXMLObj: TEMTF;

    FTEMTFPop: TPopupMenu;
    FTEMTFSitePop: TPopupMenu;
    FTEMTFLocationPop: TPopupMenu;
    FTEMTFProcessingInfoPop: TPopupMenu;
    FTEMTFProcessingSoftwarePop: TPopupMenu;
    FTEMTFInstrumentPop: TPopupMenu;
    FTEMTFDipolePop: TPopupMenu;
    FTEMTFDataTypesPop: TPopupMenu;
    FTEMTFDateTypePop: TPopupMenu;
    FTEMTFChannelsPop: TPopupMenu;
    FTEMTFChannelPop: TPopupMenu;
    FTEMTFDataPop: TPopupMenu;
    FTEMTFValuePop: TPopupMenu;
    FTEMTFProvenancePop: TPopupMenu;
    FTEMTFPersonPop: TPopupMenu;
    FTEMTFDataQualityNotesPop: TPopupMenu;
    FTEMTFUnitValuePop: TPopupMenu;
    FTEMTFElectrodePop: TPopupMenu;
    FTEMTFPeriodDataPop: TPopupMenu;
    FTEMTFTFDataPop: TPopupMenu;
    FTEMTFCOVDataPop: TPopupMenu;
    FTEMTFZPop: TPopupMenu;
    FTEMTFDeclinationPop: TPopupMenu;
    FTEMTFRemoteRefPop: TPopupMenu;
    FTEMTFCommentsPop: TPopupMenu;
    FTEMTFFilePop: TPopupMenu;
    FTEMTFCopyrightPop: TPopupMenu;
    FTEMTFCitationPop: TPopupMenu;
    FTEMTFStatisticalEstimatesPop: TPopupMenu;
    FTEMTFEstimatePop: TPopupMenu;
    FTEMTFGridOriginPop: TPopupMenu;
    FTEMTFSiteCoordsPop: TPopupMenu;
    FTEMTFSiteLayoutPop: TPopupMenu;
    FTEMTFPeriodRangePop: TPopupMenu;

    EMTFXMLTreeRoot: TTreeNodeShape;

  public
    function GetEMTFXMLObj: TEMTF;
    procedure SetEMTFXMLObj(const _Value: TEMTF);
    procedure FrameInit;
    property EMTFXMLObj: TEMTF read GetEMTFXMLObj write SetEMTFXMLObj;

  end;

implementation

{$R *.fmx}

procedure TFrameEMTFXML.FrameInit;
begin
  FTEMTFPop := TPopupMenu.Create(Self);
  FTEMTFPop.Parent := Self;
  TEMTFPop := FTEMTFPop;

  FTEMTFSitePop := TPopupMenu.Create(Self);
  FTEMTFSitePop.Parent := Self;
  TEMTFSitePop := FTEMTFSitePop;

  FTEMTFLocationPop := TPopupMenu.Create(Self);
  FTEMTFLocationPop.Parent := Self;
  TEMTFLocationPop := FTEMTFLocationPop;

  FTEMTFProcessingInfoPop := TPopupMenu.Create(Self);
  FTEMTFProcessingInfoPop.Parent := Self;
  TEMTFProcessingInfoPop := FTEMTFProcessingInfoPop;

  FTEMTFProcessingSoftwarePop := TPopupMenu.Create(Self);
  FTEMTFProcessingSoftwarePop.Parent := Self;
  TEMTFProcessingSoftwarePop := FTEMTFProcessingSoftwarePop;

  FTEMTFInstrumentPop := TPopupMenu.Create(Self);
  FTEMTFInstrumentPop.Parent := Self;
  TEMTFInstrumentPop := FTEMTFInstrumentPop;

  FTEMTFDipolePop := TPopupMenu.Create(Self);
  FTEMTFDipolePop.Parent := Self;
  TEMTFDipolePop := FTEMTFDipolePop;

  FTEMTFDataTypesPop := TPopupMenu.Create(Self);
  FTEMTFDataTypesPop.Parent := Self;
  TEMTFDataTypesPop := FTEMTFDataTypesPop;

  FTEMTFDateTypePop := TPopupMenu.Create(Self);
  FTEMTFDateTypePop.Parent := Self;
  TEMTFDateTypePop := FTEMTFDateTypePop;

  FTEMTFChannelsPop := TPopupMenu.Create(Self);
  FTEMTFChannelsPop.Parent := Self;
  TEMTFChannelsPop := FTEMTFChannelsPop;

  FTEMTFChannelPop := TPopupMenu.Create(Self);
  FTEMTFChannelPop.Parent := Self;
  TEMTFChannelPop := FTEMTFChannelPop;

  FTEMTFDataPop := TPopupMenu.Create(Self);
  FTEMTFDataPop.Parent := Self;
  TEMTFDataPop := FTEMTFDataPop;

  FTEMTFValuePop := TPopupMenu.Create(Self);
  FTEMTFValuePop.Parent := Self;
  TEMTFValuePop := FTEMTFValuePop;

  FTEMTFProvenancePop := TPopupMenu.Create(Self);
  FTEMTFProvenancePop.Parent := Self;
  TEMTFProvenancePop := FTEMTFProvenancePop;

  FTEMTFPersonPop := TPopupMenu.Create(Self);
  FTEMTFPersonPop.Parent := Self;
  TEMTFPersonPop := FTEMTFPersonPop;

  FTEMTFDataQualityNotesPop := TPopupMenu.Create(Self);
  FTEMTFDataQualityNotesPop.Parent := Self;
  TEMTFDataQualityNotesPop := FTEMTFDataQualityNotesPop;

  FTEMTFUnitValuePop := TPopupMenu.Create(Self);
  FTEMTFUnitValuePop.Parent := Self;
  TEMTFUnitValuePop := FTEMTFUnitValuePop;

  FTEMTFElectrodePop := TPopupMenu.Create(Self);
  FTEMTFElectrodePop.Parent := Self;
  TEMTFElectrodePop := FTEMTFElectrodePop;

  FTEMTFPeriodDataPop := TPopupMenu.Create(Self);
  FTEMTFPeriodDataPop.Parent := Self;
  TEMTFPeriodDataPop := FTEMTFPeriodDataPop;

  FTEMTFTFDataPop := TPopupMenu.Create(Self);
  FTEMTFTFDataPop.Parent := Self;
  TEMTFTFDataPop := FTEMTFTFDataPop;

  FTEMTFCOVDataPop := TPopupMenu.Create(Self);
  FTEMTFCOVDataPop.Parent := Self;
  TEMTFCOVDataPop := FTEMTFCOVDataPop;

  FTEMTFZPop := TPopupMenu.Create(Self);
  FTEMTFZPop.Parent := Self;
  TEMTFZPop := FTEMTFZPop;

  FTEMTFDeclinationPop := TPopupMenu.Create(Self);
  FTEMTFDeclinationPop.Parent := Self;
  TEMTFDeclinationPop := FTEMTFDeclinationPop;

  FTEMTFRemoteRefPop := TPopupMenu.Create(Self);
  FTEMTFRemoteRefPop.Parent := Self;
  TEMTFRemoteRefPop := FTEMTFRemoteRefPop;

  FTEMTFCommentsPop := TPopupMenu.Create(Self);
  FTEMTFCommentsPop.Parent := Self;
  TEMTFCommentsPop := FTEMTFCommentsPop;

  FTEMTFFilePop := TPopupMenu.Create(Self);
  FTEMTFFilePop.Parent := Self;
  TEMTFFilePop := FTEMTFFilePop;

  FTEMTFCopyrightPop := TPopupMenu.Create(Self);
  FTEMTFCopyrightPop.Parent := Self;
  TEMTFCopyrightPop := FTEMTFCopyrightPop;

  FTEMTFCitationPop := TPopupMenu.Create(Self);
  FTEMTFCitationPop.Parent := Self;
  TEMTFCitationPop := FTEMTFCitationPop;

  FTEMTFStatisticalEstimatesPop := TPopupMenu.Create(Self);
  FTEMTFStatisticalEstimatesPop.Parent := Self;
  TEMTFStatisticalEstimatesPop := FTEMTFStatisticalEstimatesPop;

  FTEMTFEstimatePop := TPopupMenu.Create(Self);
  FTEMTFEstimatePop.Parent := Self;
  TEMTFEstimatePop := FTEMTFEstimatePop;

  FTEMTFGridOriginPop := TPopupMenu.Create(Self);
  FTEMTFGridOriginPop.Parent := Self;
  TEMTFGridOriginPop := FTEMTFGridOriginPop;

  FTEMTFSiteCoordsPop := TPopupMenu.Create(Self);
  FTEMTFSiteCoordsPop.Parent := Self;
  TEMTFSiteCoordsPop := FTEMTFSiteCoordsPop;

  FTEMTFSiteLayoutPop := TPopupMenu.Create(Self);
  FTEMTFSiteLayoutPop.Parent := Self;
  TEMTFSiteLayoutPop := FTEMTFSiteLayoutPop;

  FTEMTFPeriodRangePop := TPopupMenu.Create(Self);
  FTEMTFPeriodRangePop.Parent := Self;
  TEMTFPeriodRangePop := FTEMTFPeriodRangePop;

  if Not Assigned(FEMTFXMLObj) then
    FEMTFXMLObj := TEMTF.Create(nil);
  if Not Assigned(EMTFXMLTreeRoot) then
    EMTFXMLTreeRoot := EMTFXMLTree.AddRoot('EMTFXML');
  FEMTFXMLObj.TreeNodeShape := EMTFXMLTreeRoot;
  FEMTFXMLObj.ToTree;

  TEMTFXMLInspector := EMTFXMLIns;
  TEMTFTreeComponent := EMTFXMLTree;
  TEMTFSiteXMLInspector := EMTFXMLIns;
  TEMTFSiteTreeComponent := EMTFXMLTree;
  TEMTFLocationXMLInspector := EMTFXMLIns;
  TEMTFLocationTreeComponent := EMTFXMLTree;
  TEMTFProcessingInfoXMLInspector := EMTFXMLIns;
  TEMTFProcessingInfoTreeComponent := EMTFXMLTree;
  TEMTFProcessingSoftwareXMLInspector := EMTFXMLIns;
  TEMTFProcessingSoftwareTreeComponent := EMTFXMLTree;
  TEMTFInstrumentXMLInspector := EMTFXMLIns;
  TEMTFInstrumentTreeComponent := EMTFXMLTree;
  TEMTFDipoleXMLInspector := EMTFXMLIns;
  TEMTFDipoleTreeComponent := EMTFXMLTree;
  TEMTFDataTypesXMLInspector := EMTFXMLIns;
  TEMTFDataTypesTreeComponent := EMTFXMLTree;
  TEMTFDateTypeXMLInspector := EMTFXMLIns;
  TEMTFDateTypeTreeComponent := EMTFXMLTree;
  TEMTFChannelsXMLInspector := EMTFXMLIns;
  TEMTFChannelsTreeComponent := EMTFXMLTree;
  TEMTFChannelXMLInspector := EMTFXMLIns;
  TEMTFChannelTreeComponent := EMTFXMLTree;
  TEMTFDataXMLInspector := EMTFXMLIns;
  TEMTFDataTreeComponent := EMTFXMLTree;
  TEMTFValueXMLInspector := EMTFXMLIns;
  TEMTFValueTreeComponent := EMTFXMLTree;
  TEMTFProvenanceXMLInspector := EMTFXMLIns;
  TEMTFProvenanceTreeComponent := EMTFXMLTree;
  TEMTFPersonXMLInspector := EMTFXMLIns;
  TEMTFPersonTreeComponent := EMTFXMLTree;
  TEMTFDataQualityNotesXMLInspector := EMTFXMLIns;
  TEMTFDataQualityNotesTreeComponent := EMTFXMLTree;
  TEMTFUnitValueXMLInspector := EMTFXMLIns;
  TEMTFUnitValueTreeComponent := EMTFXMLTree;
  TEMTFElectrodeXMLInspector := EMTFXMLIns;
  TEMTFElectrodeTreeComponent := EMTFXMLTree;
  TEMTFPeriodDataXMLInspector := EMTFXMLIns;
  TEMTFPeriodDataTreeComponent := EMTFXMLTree;
  TEMTFTFDataXMLInspector := EMTFXMLIns;
  TEMTFTFDataTreeComponent := EMTFXMLTree;
  TEMTFCOVDataXMLInspector := EMTFXMLIns;
  TEMTFCOVDataTreeComponent := EMTFXMLTree;
  TEMTFZXMLInspector := EMTFXMLIns;
  TEMTFZTreeComponent := EMTFXMLTree;
  TEMTFDeclinationXMLInspector := EMTFXMLIns;
  TEMTFDeclinationTreeComponent := EMTFXMLTree;
  TEMTFRemoteRefXMLInspector := EMTFXMLIns;
  TEMTFRemoteRefTreeComponent := EMTFXMLTree;
  TEMTFCommentsXMLInspector := EMTFXMLIns;
  TEMTFCommentsTreeComponent := EMTFXMLTree;
  TEMTFFileXMLInspector := EMTFXMLIns;
  TEMTFFileTreeComponent := EMTFXMLTree;
  TEMTFCopyrightXMLInspector := EMTFXMLIns;
  TEMTFCopyrightTreeComponent := EMTFXMLTree;
  TEMTFCitationXMLInspector := EMTFXMLIns;
  TEMTFCitationTreeComponent := EMTFXMLTree;
  TEMTFStatisticalEstimatesXMLInspector := EMTFXMLIns;
  TEMTFStatisticalEstimatesTreeComponent := EMTFXMLTree;
  TEMTFEstimateXMLInspector := EMTFXMLIns;
  TEMTFEstimateTreeComponent := EMTFXMLTree;
  TEMTFGridOriginXMLInspector := EMTFXMLIns;
  TEMTFGridOriginTreeComponent := EMTFXMLTree;
  TEMTFSiteCoordsXMLInspector := EMTFXMLIns;
  TEMTFSiteCoordsTreeComponent := EMTFXMLTree;
  TEMTFSiteLayoutXMLInspector := EMTFXMLIns;
  TEMTFSiteLayoutTreeComponent := EMTFXMLTree;
  TEMTFPeriodRangeXMLInspector := EMTFXMLIns;
  TEMTFPeriodRangeTreeComponent := EMTFXMLTree;
end;

function TFrameEMTFXML.GetEMTFXMLObj: TEMTF;
begin
  Result := FEMTFXMLObj;
end;

procedure TFrameEMTFXML.NewEMTFXML(Sender: TObject);
begin
  FEMTFXMLObj.TreeNodeShape := nil;
  FEMTFXMLObj.Free;
  FEMTFXMLObj := TEMTF.Create(nil);
  FEMTFXMLObj.TreeNodeShape := EMTFXMLTreeRoot;
  FEMTFXMLObj.ToTree;
end;

procedure TFrameEMTFXML.OpenEMTFXML(Sender: TObject);
begin
  if OpenDialogEMTFXML.Execute then
  begin
    FEMTFXMLObj.TreeNodeShape := nil;
    FEMTFXMLObj.Free;
    FEMTFXMLObj := TEMTF.Create(nil);
    FEMTFXMLObj.Load(OpenDialogEMTFXML.FileName);
    FEMTFXMLObj.TreeNodeShape := EMTFXMLTreeRoot;
    FEMTFXMLObj.ToTree;
  end;
end;

procedure TFrameEMTFXML.SaveEMTFXML(Sender: TObject);
begin
  if SaveDialogEMTFXML.Execute then
  begin
    FEMTFXMLObj.Save(SaveDialogEMTFXML.FileName);
  end;
end;

procedure TFrameEMTFXML.SetEMTFXMLObj(const _Value: TEMTF);
begin
  if Not Assigned(_Value) then
    Exit;
  FEMTFXMLObj.TreeNodeShape := nil;
  FEMTFXMLObj.Free;
  FEMTFXMLObj := _Value;
end;

end.


