unit XMLCore;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes,
  FMXTee.Tree, FMX.Dialogs;

type

  XMLInspectorEvent = procedure(index: Integer; value: String) of object;

  TXML = class;
  TXMLBase = class;
  TXMLFile = class;
  TXMLClass = class of TXML;

  TXML = class abstract(TObject)
  private
    FParent: TXML;
    FOnSetEvent: XMLInspectorEvent;
    procedure SetParent(const value: TXML);
  protected
    procedure FromXML(node: IXMLNode); virtual; abstract;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; virtual; abstract;
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    procedure Load(const fileName: string);
    procedure Save(const fileName: string);
    function Xml: string;
    procedure SetXMLProperty(index: Integer; value: String); virtual;
    property OnSetEvent: XMLInspectorEvent read FOnSetEvent write FOnSetEvent;
    property Parent: TXML read FParent write SetParent;
  end;

  TXMLBase = class(TXML)
  private
    FTreeNodeShape: TTreeNodeShape;
    FXMLNode: IXMLNode;
    procedure SetXMLNode(const value: IXMLNode);
  protected
  public
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure ToTree; virtual;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property XMLNode: IXMLNode read FXMLNode write SetXMLNode;
    destructor Destroy; override;
    procedure SetXMLProperty(index: Integer; value: String); override;
  end;

  TXMLFile = class(TXML)
  private
    FFileName: String;
    FName: string;
    FDescription: string;
    FTypeName: string;
    FTreeNodeShape: TTreeNodeShape;
    FSUBXML: TXML;
    procedure SetFileName(const value: String);
    procedure SetName(const value: string);
    procedure SetDescription(const value: string);
    procedure SetTypeName(const value: string);
    procedure SetSUBXML(const value: TXML);
    property fileName: String read FFileName write SetFileName;
    property Name: string read FName write SetName;
    property Description: string read FDescription write SetDescription;
    property TypeName: string read FTypeName write SetTypeName;
    property SUBXML: TXML read FSUBXML write SetSUBXML;
  protected
  public
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure ToTree; virtual;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    destructor Destroy; override;
    procedure SetXMLProperty(index: Integer; value: String); override;
  end;

var
  XML_FilePath: string;

implementation

constructor TXML.Create(par: TXML = nil);
begin
  FParent := par;
end;

destructor TXML.Destroy;
begin
  inherited;
end;

procedure TXML.Load(const fileName: string);
var
  doc: IXMLDocument;
  node: IXMLNode;
begin
  doc := LoadXMLDocument(fileName);
  node := doc.DocumentElement;
  FromXML(node);
end;

procedure TXML.Save(const fileName: string);
var
  doc: IXMLDocument;
begin
  doc := NewXMLDocument();
  doc.Encoding := 'UTF-8';
  doc.NodeIndentStr := '  ';
  doc.Options := [doNodeAutoIndent];
  ToXML(doc.node);
  doc.SaveToFile(fileName);
  XML_FilePath := ExtractFilePath(fileName);
end;

procedure TXML.SetParent(const value: TXML);
begin
  FParent := value;
end;

procedure TXML.SetXMLProperty(index: Integer; value: String);
begin
  //
end;

function TXML.Xml: string;
var
  doc: IXMLDocument;
  node: IXMLNode;
begin
  doc := NewXMLDocument();
  node := doc.DocumentElement;
  ToXML(node);
  Result := node.Xml;
end;

destructor TXMLBase.Destroy;
begin
  FXMLNode := nil;
  inherited;
end;

procedure TXMLBase.FromXML(node: IXMLNode);
begin
  FXMLNode := node;
end;

procedure TXMLBase.SetXMLNode(const value: IXMLNode);
begin
  FXMLNode := value;
end;

procedure TXMLBase.SetXMLProperty(index: Integer; value: String);
begin
  inherited;
end;

procedure TXMLBase.ToTree;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
end;

function TXMLBase.ToXML(par: IXMLNode; pt: string): IXMLNode;
begin
  par.ChildNodes.Add(FXMLNode);
end;

destructor TXMLFile.Destroy;
begin
  inherited;
end;

procedure TXMLFile.FromXML(node: IXMLNode);
var
  doc: IXMLDocument;
  I: Integer;
  nodeTmp: IXMLNode;
  xclass: TXMLClass;
begin
  for I := 0 to node.ChildNodes.Count - 1 do
  begin
    nodeTmp := node.ChildNodes.Get(I);
    if nodeTmp.NodeName = 'Name' then
    begin
      FName := nodeTmp.text;
    end
    else if nodeTmp.NodeName = 'Description' then
    begin
      FDescription := nodeTmp.text;
    end
    else if nodeTmp.NodeName = 'File' then
    begin
      FFileName := nodeTmp.text;
    end
    else if nodeTmp.NodeName = 'XMLType' then
    begin
      FTypeName := nodeTmp.text;
    end;
  end;
  if FTypeName = '' then
  begin

  end
  else if FTypeName = '' then
  begin

  end
  else if FTypeName = '' then
  begin

  end
  else
  begin
    xclass := TXMLBase;
  end;
  if FileExists(FFileName) then
  begin
    if Not Assigned(SUBXML) then
      SUBXML := xclass.Create();
    SUBXML.Load(XML_FilePath + FFileName);
  end;
end;

procedure TXMLFile.SetFileName(const value: String);
begin
  FFileName := value;
end;

procedure TXMLFile.SetName(const value: string);
begin
  FName := value;
end;

procedure TXMLFile.SetDescription(const value: string);
begin
  FDescription := value;
end;

procedure TXMLFile.SetTypeName(const value: string);
begin
  FTypeName := value;
end;

procedure TXMLFile.SetSUBXML(const value: TXML);
begin
  FSUBXML := value;
end;

procedure TXMLFile.SetXMLProperty(index: Integer; value: String);
begin
  inherited;
  //
end;

procedure TXMLFile.ToTree;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
end;

function TXMLFile.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  FileNameTmp: IXMLNode;
  TypeNameTmp: IXMLNode;
begin
  doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'XMLFile';
  node := doc.CreateNode(pt);
  par.ChildNodes.Add(node);
  NameTmp := doc.CreateNode('Name', ntElement);
  NameTmp.NodeValue := FName;
  node.ChildNodes.Add(NameTmp);
  DescriptionTmp := doc.CreateNode('Description', ntElement);
  DescriptionTmp.NodeValue := FDescription;
  node.ChildNodes.Add(DescriptionTmp);
  FileNameTmp := doc.CreateNode('FileName', ntElement);
  FileNameTmp.NodeValue := FFileName;
  node.ChildNodes.Add(FileNameTmp);
  TypeNameTmp := doc.CreateNode('TypeName', ntElement);
  TypeNameTmp.NodeValue := FTypeName;
  node.ChildNodes.Add(TypeNameTmp);
  if Assigned(FSUBXML) then
    FSUBXML.Save(XML_FilePath + FFileName);
end;

end.
