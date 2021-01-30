unit GeoXMLBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,
  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,
  ClientScreen, XMLInspector;


type

  TGeoXML= class;
  TGeoFolder= class;
  TGeoElement= class;
  TGeoPoint= class;
  TGeoLine= class;
  TGeoLines= class;
  TGeoPolygon= class;
  TGeoStyle= class;
  TGeoPointStyle= class;
  TGeoLineStyle= class;
  TGeoPolygonStyle= class;

  TGeoXML= class(TXML)
  private
    FName: String;
    FDescription: String;
    FFolders: TList<TGeoFolder>;
    FStyles: TList<TGeoStyle>;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetFolders(const _Value: TList<TGeoFolder>);
    function GetFolder(Index: Integer): TGeoFolder;
    procedure SetFolder(Index: Integer; const _Value: TGeoFolder);
    procedure SetStyles(const _Value: TList<TGeoStyle>);
    function GetStyle(Index: Integer): TGeoStyle;
    procedure SetStyle(Index: Integer; const _Value: TGeoStyle);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddFolderEvent(Sender: TObject);
    procedure AddStyleEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddFolder: TGeoFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TGeoFolder);
    procedure DeleteFolder(Index: Integer);
    function AddStyle: TGeoStyle;
    procedure StyleClear;
    function StyleCount: Integer;
    procedure RemoveStyle(_Value: TGeoStyle);
    procedure DeleteStyle(Index: Integer);
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property Folders: TList<TGeoFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TGeoFolder read GetFolder write SetFolder;
    property Styles: TList<TGeoStyle> read FStyles write SetStyles;
    property Style[Index: Integer]: TGeoStyle read GetStyle write SetStyle;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoFolder= class(TXML)
  private
    FName: String;
    FDescription: String;
    FFolders: TList<TGeoFolder>;
    FElements: TList<TGeoElement>;
    FStyle: Integer;
    FStyleExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetFolders(const _Value: TList<TGeoFolder>);
    function GetFolder(Index: Integer): TGeoFolder;
    procedure SetFolder(Index: Integer; const _Value: TGeoFolder);
    procedure SetElements(const _Value: TList<TGeoElement>);
    function GetElement(Index: Integer): TGeoElement;
    procedure SetElement(Index: Integer; const _Value: TGeoElement);
    procedure SetStyle(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddFolderEvent(Sender: TObject);
    procedure AddPointEvent(Sender: TObject);
    procedure AddLineEvent(Sender: TObject);
    procedure AddLinesEvent(Sender: TObject);
    procedure AddPolygonEvent(Sender: TObject);
    procedure AddStyleEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddFolder: TGeoFolder;
    procedure FolderClear;
    function FolderCount: Integer;
    procedure RemoveFolder(_Value: TGeoFolder);
    procedure DeleteFolder(Index: Integer);
    function AddPoint: TGeoPoint;
    function AddLine: TGeoLine;
    function AddLines: TGeoLines;
    function AddPolygon: TGeoPolygon;
    procedure ElementClear;
    function ElementCount: Integer;
    procedure RemoveElement(_Value: TGeoElement);
    procedure DeleteElement(Index: Integer);
    function AddStyle: Integer;
    procedure StyleRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property Folders: TList<TGeoFolder> read FFolders write SetFolders;
    property Folder[Index: Integer]: TGeoFolder read GetFolder write SetFolder;
    property Elements: TList<TGeoElement> read FElements write SetElements;
    property Element[Index: Integer]: TGeoElement read GetElement write SetElement;
    property Style: Integer read FStyle write SetStyle;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property StyleExsit: Boolean read FStyleExsit;
  end;

  TGeoElement= class abstract(TXML)
  private
    FName: String;
    FDescription: String;
    FVisible: Boolean;
    FPosition: ArrayCoordinates;
    FStyle: Integer;
    FStyleExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetDescription(const _Value: String);
    procedure SetVisible(const _Value: Boolean);
    procedure SetPosition(const _Value: ArrayCoordinates);
    procedure SetStyle(const _Value: Integer);
  protected
    procedure FromXML(node: IXMLNode); virtual;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; virtual;
    function AppendToXML(node: IXMLNode; pt: string = ''): IXMLNode;
    procedure AddStyleEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddStyle: Integer;
    procedure StyleRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property Description: String read FDescription write SetDescription;
    property Visible: Boolean read FVisible write SetVisible;
    property Position: ArrayCoordinates read FPosition write SetPosition;
    property Style: Integer read FStyle write SetStyle;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property StyleExsit: Boolean read FStyleExsit;
  end;

  TGeoPoint= class(TGeoElement)
  private
    FTreeNodeShape: TTreeNodeShape;
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
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoLine= class(TGeoElement)
  private
    FTreeNodeShape: TTreeNodeShape;
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
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoLines= class(TGeoElement)
  private
    FClosed: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetClosed(const _Value: Boolean);
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
    property Closed: Boolean read FClosed write SetClosed;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoPolygon= class(TGeoElement)
  private
    FFilled: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetFilled(const _Value: Boolean);
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
    property Filled: Boolean read FFilled write SetFilled;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoStyle= class abstract(TXML)
  private
    FName: String;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
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
    property Name: String read FName write SetName;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoPointStyle= class(TGeoStyle)
  private
    FColor: Integer;
    FSize: Double;
    FPointStyle: Integer;
    FBorderStyle: Integer;
    FBorderColor: TAlphaColor;
    FBorderWidth: Double;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetColor(const _Value: Integer);
    procedure SetSize(const _Value: Double);
    procedure SetPointStyle(const _Value: Integer);
    procedure SetBorderStyle(const _Value: Integer);
    procedure SetBorderColor(const _Value: TAlphaColor);
    procedure SetBorderWidth(const _Value: Double);
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
    property Color: Integer read FColor write SetColor;
    property Size: Double read FSize write SetSize;
    property PointStyle: Integer read FPointStyle write SetPointStyle;
    property BorderStyle: Integer read FBorderStyle write SetBorderStyle;
    property BorderColor: TAlphaColor read FBorderColor write SetBorderColor;
    property BorderWidth: Double read FBorderWidth write SetBorderWidth;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoLineStyle= class(TGeoStyle)
  private
    FPointStyle: TGeoPointStyle;
    FLineColor: TAlphaColor;
    FLineWidth: Double;
    FLineStyle: Integer;
    FLineEndStyle: Integer;
    FLineJoinStyle: Integer;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetPointStyle(const _Value: TGeoPointStyle);
    procedure SetLineColor(const _Value: TAlphaColor);
    procedure SetLineWidth(const _Value: Double);
    procedure SetLineStyle(const _Value: Integer);
    procedure SetLineEndStyle(const _Value: Integer);
    procedure SetLineJoinStyle(const _Value: Integer);
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
    property PointStyle: TGeoPointStyle read FPointStyle write SetPointStyle;
    property LineColor: TAlphaColor read FLineColor write SetLineColor;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property LineStyle: Integer read FLineStyle write SetLineStyle;
    property LineEndStyle: Integer read FLineEndStyle write SetLineEndStyle;
    property LineJoinStyle: Integer read FLineJoinStyle write SetLineJoinStyle;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;

  TGeoPolygonStyle= class(TGeoStyle)
  private
    FLineStyle: TGeoLineStyle;
    FFillColor: TAlphaColor;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetLineStyle(const _Value: TGeoLineStyle);
    procedure SetFillColor(const _Value: TAlphaColor);
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
    property LineStyle: TGeoLineStyle read FLineStyle write SetLineStyle;
    property FillColor: TAlphaColor read FFillColor write SetFillColor;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
  end;


var
  TGeoXMLPop: TPopupMenu;
  TGeoXMLXMLInspector: TXMLInspector;
  TGeoXMLTreeComponent: TTree;
  TGeoFolderPop: TPopupMenu;
  TGeoFolderXMLInspector: TXMLInspector;
  TGeoFolderTreeComponent: TTree;
  TGeoElementPop: TPopupMenu;
  TGeoElementXMLInspector: TXMLInspector;
  TGeoElementTreeComponent: TTree;
  TGeoPointPop: TPopupMenu;
  TGeoPointXMLInspector: TXMLInspector;
  TGeoPointTreeComponent: TTree;
  TGeoLinePop: TPopupMenu;
  TGeoLineXMLInspector: TXMLInspector;
  TGeoLineTreeComponent: TTree;
  TGeoLinesPop: TPopupMenu;
  TGeoLinesXMLInspector: TXMLInspector;
  TGeoLinesTreeComponent: TTree;
  TGeoPolygonPop: TPopupMenu;
  TGeoPolygonXMLInspector: TXMLInspector;
  TGeoPolygonTreeComponent: TTree;
  TGeoStylePop: TPopupMenu;
  TGeoStyleXMLInspector: TXMLInspector;
  TGeoStyleTreeComponent: TTree;
  TGeoPointStylePop: TPopupMenu;
  TGeoPointStyleXMLInspector: TXMLInspector;
  TGeoPointStyleTreeComponent: TTree;
  TGeoLineStylePop: TPopupMenu;
  TGeoLineStyleXMLInspector: TXMLInspector;
  TGeoLineStyleTreeComponent: TTree;
  TGeoPolygonStylePop: TPopupMenu;
  TGeoPolygonStyleXMLInspector: TXMLInspector;
  TGeoPolygonStyleTreeComponent: TTree;
  GeoXMLObject: TObject;

implementation

{  GeoXML}
constructor TGeoXML.Create(par: TXML = nil);
begin
  inherited Create(par);
  FFolders := TList<TGeoFolder>.Create;
  FStyles := TList<TGeoStyle>.Create;
end;

destructor TGeoXML.Destroy;
begin
  FolderClear;
  FFolders.Free;
  StyleClear;
  FStyles.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoXML.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  FolderTmp: TGeoFolder;
  StyleTmp: TGeoStyle;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TGeoFolder.Create(Self);
        FolderTmp.FromXML(nodeTmp);
        FFolders.Add(FolderTmp);
      end
      else if nodeTmp.NodeName = 'Style' then
      begin
        StyleTmp := TGeoStyle.Create(Self);
        StyleTmp.FromXML(nodeTmp);
        FStyles.Add(StyleTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('GeoXML Read XML Error!' + node.Xml);
  end;
end;

function TGeoXML.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'GeoXML';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    DescriptionTmp := doc.CreateNode('Description', ntElement);
    DescriptionTmp.NodeValue := FDescription;
    node.ChildNodes.Add(DescriptionTmp);
    for I := 0 to FFolders.Count - 1 do
       FFolders.Items[I].ToXML(node, 'Folder');
    for I := 0 to FStyles.Count - 1 do
       FStyles.Items[I].ToXML(node, 'Style');
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoXML.ToTree;
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
  TreeNodeShape.AddChild('Description');
  for I := 0 to FolderCount - 1 do
  begin
    Folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder', Folder[I]);
    Folder[I].ToTree;
  end;
  for I := 0 to StyleCount - 1 do
  begin
    Styles[I].TreeNodeShape := TreeNodeShape.AddChildObject('Style', Style[I]);
    Style[I].ToTree;
  end;
end;

procedure TGeoXML.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  FolderAddMenu: TMenuItem;
  StyleAddMenu: TMenuItem;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoXMLPop) and Assigned(TGeoXMLTreeComponent) then
    begin
      TGeoXMLPop.Clear;
      FolderAddMenu := TMenuItem.Create(TGeoXMLPop);
      FolderAddMenu.Text := 'Add Folder';
      FolderAddMenu.OnClick := AddFolderEvent;
      TGeoXMLPop.AddObject(FolderAddMenu);
      StyleAddMenu := TMenuItem.Create(TGeoXMLPop);
      StyleAddMenu.Text := 'Add Style';
      StyleAddMenu.OnClick := AddStyleEvent;
      TGeoXMLPop.AddObject(StyleAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGeoXMLTreeComponent.ClientToScreen(pt);
      TGeoXMLPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoXML.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoXMLXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  TGeoXMLXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoXML.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Description := _Value;
      end;
  end;
  ToTree;
end;

procedure TGeoXML.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TGeoXML.SetDescription(const _Value: String);
begin
  FDescription := _Value;
end;

function TGeoXML.AddFolder: TGeoFolder;
var
  Foldertmp: TGeoFolder;
begin;
  Foldertmp := TGeoFolder.Create(Self);
  FFolders.Add(Foldertmp);
  Result := Foldertmp;
end;

procedure TGeoXML.SetFolders(const _Value: TList<TGeoFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

procedure TGeoXML.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TGeoXML.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

function TGeoXML.GetFolder(Index: Integer): TGeoFolder;
begin
  Result := FFolders[Index];
end;

procedure TGeoXML.SetFolder(Index: Integer;
  const _Value: TGeoFolder);
begin
  _Value.Parent := Self;
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TGeoXML.RemoveFolder(_Value: TGeoFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TGeoXML.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

procedure TGeoXML.AddFolderEvent(Sender: TObject);
var
  tmp: TGeoFolder;
begin
  tmp := AddFolder;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Folder', tmp);
  tmp.ToTree;
end;

function TGeoXML.AddStyle: TGeoStyle;
var
  Styletmp: TGeoStyle;
begin;
  Styletmp := TGeoStyle.Create(Self);
  FStyles.Add(Styletmp);
  Result := Styletmp;
end;

procedure TGeoXML.SetStyles(const _Value: TList<TGeoStyle>);
begin
  StyleClear;
  FStyles := _Value;
end;

procedure TGeoXML.StyleClear;
begin
  while FStyles.Count > 0 do
  begin
    FStyles.Items[0].Free;
    FStyles.Delete(0);
  end;
end;

function TGeoXML.StyleCount: Integer;
begin
  Result := FStyles.Count;
end;

function TGeoXML.GetStyle(Index: Integer): TGeoStyle;
begin
  Result := FStyles[Index];
end;

procedure TGeoXML.SetStyle(Index: Integer;
  const _Value: TGeoStyle);
begin
  _Value.Parent := Self;
  FStyles[Index].Free;
  FStyles[Index] := _Value;
end;

procedure TGeoXML.RemoveStyle(_Value: TGeoStyle);
begin
  FStyles.Remove(_Value);
  _Value.Free;
end;

procedure TGeoXML.DeleteStyle(Index: Integer);
begin
  FStyles.Items[Index].Free;
  FStyles.Delete(Index);
end;

procedure TGeoXML.AddStyleEvent(Sender: TObject);
var
  tmp: TGeoStyle;
begin
  tmp := AddStyle;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Style', tmp);
  tmp.ToTree;
end;

{  Folder}
constructor TGeoFolder.Create(par: TXML = nil);
begin
  inherited Create(par);
  FFolders := TList<TGeoFolder>.Create;
  FElements := TList<TGeoElement>.Create;
end;

destructor TGeoFolder.Destroy;
begin
  FolderClear;
  FFolders.Free;
  ElementClear;
  FElements.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoFolder.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  FolderTmp: TGeoFolder;
  ElementTmp: TGeoElement;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Description' then
      begin
        FDescription := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'Folder' then
      begin
        FolderTmp := TGeoFolder.Create(Self);
        FolderTmp.FromXML(nodeTmp);
        FFolders.Add(FolderTmp);
      end
      else if nodeTmp.NodeName = 'Point' then
      begin
        ElementTmp := TGeoPoint.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'Line' then
      begin
        ElementTmp := TGeoLine.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'Lines' then
      begin
        ElementTmp := TGeoLines.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'Polygon' then
      begin
        ElementTmp := TGeoPolygon.Create(Self);
        ElementTmp.FromXML(nodeTmp);
        FElements.Add(ElementTmp);
      end
      else if nodeTmp.NodeName = 'Style' then
      begin
        FStyle := nodeTmp.Text.ToInteger;
        FStyleExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Folder Read XML Error!' + node.Xml);
  end;
end;

function TGeoFolder.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  StyleTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Folder';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    DescriptionTmp := doc.CreateNode('Description', ntElement);
    DescriptionTmp.NodeValue := FDescription;
    node.ChildNodes.Add(DescriptionTmp);
    for I := 0 to FFolders.Count - 1 do
       FFolders.Items[I].ToXML(node, 'Folder');
    for I := 0 to FElements.Count - 1 do
       FElements.Items[I].ToXML(node, '#Optional');
    if FStyleExsit then
    begin
      StyleTmp := doc.CreateNode('Style', ntElement);
      StyleTmp.NodeValue := FStyle.toString;
      node.ChildNodes.Add(StyleTmp);
    end;
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoFolder.ToTree;
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
  TreeNodeShape.AddChild('Description');
  for I := 0 to FolderCount - 1 do
  begin
    Folders[I].TreeNodeShape := TreeNodeShape.AddChildObject('Folder', Folder[I]);
    Folder[I].ToTree;
  end;
  for I := 0 to ElementCount - 1 do
  begin
    Elements[I].TreeNodeShape := TreeNodeShape.AddChildObject('Element', Element[I]);
    Element[I].ToTree;
  end;
  if StyleExsit then
    TreeNodeShape.AddChild('Style');
end;

procedure TGeoFolder.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  FolderAddMenu: TMenuItem;
  ElementAddMenu: TMenuItem;
  PointAddMenu: TMenuItem;
  LineAddMenu: TMenuItem;
  LinesAddMenu: TMenuItem;
  PolygonAddMenu: TMenuItem;
  StyleAddMenu: TMenuItem;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoFolderPop) and Assigned(TGeoFolderTreeComponent) then
    begin
      TGeoFolderPop.Clear;
      FolderAddMenu := TMenuItem.Create(TGeoFolderPop);
      FolderAddMenu.Text := 'Add Folder';
      FolderAddMenu.OnClick := AddFolderEvent;
      TGeoFolderPop.AddObject(FolderAddMenu);
      ElementAddMenu := TMenuItem.Create(TGeoFolderPop);
      ElementAddMenu.Text := 'Add Element';
      TGeoFolderPop.AddObject(ElementAddMenu);
      PointAddMenu := TMenuItem.Create(ElementAddMenu);
      PointAddMenu.Text := 'Point';
      PointAddMenu.OnClick := AddPointEvent;
      ElementAddMenu.AddObject(PointAddMenu);
      LineAddMenu := TMenuItem.Create(ElementAddMenu);
      LineAddMenu.Text := 'Line';
      LineAddMenu.OnClick := AddLineEvent;
      ElementAddMenu.AddObject(LineAddMenu);
      LinesAddMenu := TMenuItem.Create(ElementAddMenu);
      LinesAddMenu.Text := 'Lines';
      LinesAddMenu.OnClick := AddLinesEvent;
      ElementAddMenu.AddObject(LinesAddMenu);
      PolygonAddMenu := TMenuItem.Create(ElementAddMenu);
      PolygonAddMenu.Text := 'Polygon';
      PolygonAddMenu.OnClick := AddPolygonEvent;
      ElementAddMenu.AddObject(PolygonAddMenu);
      StyleAddMenu := TMenuItem.Create(TGeoFolderPop);
      StyleAddMenu.Text := 'Add Style';
      StyleAddMenu.OnClick := AddStyleEvent;
      TGeoFolderPop.AddObject(StyleAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGeoFolderTreeComponent.ClientToScreen(pt);
      TGeoFolderPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoFolder.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoFolderXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('Style');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Style.toString);
  TGeoFolderXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoFolder.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Description := _Value;
      end;
    2:
      begin
        Style := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

procedure TGeoFolder.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TGeoFolder.SetDescription(const _Value: String);
begin
  FDescription := _Value;
end;

function TGeoFolder.AddFolder: TGeoFolder;
var
  Foldertmp: TGeoFolder;
begin;
  Foldertmp := TGeoFolder.Create(Self);
  FFolders.Add(Foldertmp);
  Result := Foldertmp;
end;

procedure TGeoFolder.SetFolders(const _Value: TList<TGeoFolder>);
begin
  FolderClear;
  FFolders := _Value;
end;

procedure TGeoFolder.FolderClear;
begin
  while FFolders.Count > 0 do
  begin
    FFolders.Items[0].Free;
    FFolders.Delete(0);
  end;
end;

function TGeoFolder.FolderCount: Integer;
begin
  Result := FFolders.Count;
end;

function TGeoFolder.GetFolder(Index: Integer): TGeoFolder;
begin
  Result := FFolders[Index];
end;

procedure TGeoFolder.SetFolder(Index: Integer;
  const _Value: TGeoFolder);
begin
  _Value.Parent := Self;
  FFolders[Index].Free;
  FFolders[Index] := _Value;
end;

procedure TGeoFolder.RemoveFolder(_Value: TGeoFolder);
begin
  FFolders.Remove(_Value);
  _Value.Free;
end;

procedure TGeoFolder.DeleteFolder(Index: Integer);
begin
  FFolders.Items[Index].Free;
  FFolders.Delete(Index);
end;

procedure TGeoFolder.AddFolderEvent(Sender: TObject);
var
  tmp: TGeoFolder;
begin
  tmp := AddFolder;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Folder', tmp);
  tmp.ToTree;
end;

function TGeoFolder.AddPoint: TGeoPoint;
var
  Pointtmp: TGeoPoint;
begin;
  Pointtmp := TGeoPoint.Create(Self);
  FElements.Add(Pointtmp);
  Result := Pointtmp;
end;

function TGeoFolder.AddLine: TGeoLine;
var
  Linetmp: TGeoLine;
begin;
  Linetmp := TGeoLine.Create(Self);
  FElements.Add(Linetmp);
  Result := Linetmp;
end;

function TGeoFolder.AddLines: TGeoLines;
var
  Linestmp: TGeoLines;
begin;
  Linestmp := TGeoLines.Create(Self);
  FElements.Add(Linestmp);
  Result := Linestmp;
end;

function TGeoFolder.AddPolygon: TGeoPolygon;
var
  Polygontmp: TGeoPolygon;
begin;
  Polygontmp := TGeoPolygon.Create(Self);
  FElements.Add(Polygontmp);
  Result := Polygontmp;
end;

procedure TGeoFolder.SetElements(const _Value: TList<TGeoElement>);
begin
  ElementClear;
  FElements := _Value;
end;

procedure TGeoFolder.ElementClear;
begin
  while FElements.Count > 0 do
  begin
    FElements.Items[0].Free;
    FElements.Delete(0);
  end;
end;

function TGeoFolder.ElementCount: Integer;
begin
  Result := FElements.Count;
end;

function TGeoFolder.GetElement(Index: Integer): TGeoElement;
begin
  Result := FElements[Index];
end;

procedure TGeoFolder.SetElement(Index: Integer;
  const _Value: TGeoElement);
begin
  _Value.Parent := Self;
  FElements[Index].Free;
  FElements[Index] := _Value;
end;

procedure TGeoFolder.RemoveElement(_Value: TGeoElement);
begin
  FElements.Remove(_Value);
  _Value.Free;
end;

procedure TGeoFolder.DeleteElement(Index: Integer);
begin
  FElements.Items[Index].Free;
  FElements.Delete(Index);
end;

procedure TGeoFolder.AddPointEvent(Sender: TObject);
var
  tmp: TGeoPoint;
begin
  tmp := AddPoint;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Point', tmp);
  tmp.ToTree;
end;

procedure TGeoFolder.AddLineEvent(Sender: TObject);
var
  tmp: TGeoLine;
begin
  tmp := AddLine;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Line', tmp);
  tmp.ToTree;
end;

procedure TGeoFolder.AddLinesEvent(Sender: TObject);
var
  tmp: TGeoLines;
begin
  tmp := AddLines;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Lines', tmp);
  tmp.ToTree;
end;

procedure TGeoFolder.AddPolygonEvent(Sender: TObject);
var
  tmp: TGeoPolygon;
begin
  tmp := AddPolygon;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Polygon', tmp);
  tmp.ToTree;
end;

function TGeoFolder.AddStyle: Integer;
begin;
  Result := FStyle;
  FStyleExsit := True;
end;

procedure TGeoFolder.SetStyle(const _Value: Integer);
begin
  FStyleExsit := True;
  FStyle := _Value;
end;

procedure TGeoFolder.StyleRemove;
begin
  if FStyleExsit then
  begin
    FStyleExsit := False;
  end;
end;

procedure TGeoFolder.AddStyleEvent(Sender: TObject);
begin
  AddStyle;
end;

{  Element}
constructor TGeoElement.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoElement.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoElement.FromXML(node: IXMLNode);
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
      end
      else if nodeTmp.NodeName = 'Visible' then
      begin
        FVisible := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'Position' then
      begin
        FPosition := StringToCoordinates(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'Style' then
      begin
        FStyle := nodeTmp.Text.ToInteger;
        FStyleExsit := True;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Element Read XML Error!' + node.Xml);
  end;
end;

function TGeoElement.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  VisibleTmp: IXMLNode;
  PositionTmp: IXMLNode;
  StyleTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Element';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    DescriptionTmp := doc.CreateNode('Description', ntElement);
    DescriptionTmp.NodeValue := FDescription;
    node.ChildNodes.Add(DescriptionTmp);
    VisibleTmp := doc.CreateNode('Visible', ntElement);
    VisibleTmp.NodeValue := Boolean2String(FVisible);
    node.ChildNodes.Add(VisibleTmp);
    PositionTmp := doc.CreateNode('Position', ntElement);
    PositionTmp.NodeValue := CoordinatesToString(FPosition);
    node.ChildNodes.Add(PositionTmp);
    if FStyleExsit then
    begin
      StyleTmp := doc.CreateNode('Style', ntElement);
      StyleTmp.NodeValue := FStyle.toString;
      node.ChildNodes.Add(StyleTmp);
    end;
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

function TGeoElement.AppendToXML(node: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  NameTmp: IXMLNode;
  DescriptionTmp: IXMLNode;
  VisibleTmp: IXMLNode;
  PositionTmp: IXMLNode;
  StyleTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := node.OwnerDocument;
    DescriptionTmp := doc.CreateNode('Description', ntElement);
    DescriptionTmp.NodeValue := FDescription;
    node.ChildNodes.Add(DescriptionTmp);
    VisibleTmp := doc.CreateNode('Visible', ntElement);
    VisibleTmp.NodeValue := Boolean2String(FVisible);
    node.ChildNodes.Add(VisibleTmp);
    PositionTmp := doc.CreateNode('Position', ntElement);
    PositionTmp.NodeValue := CoordinatesToString(FPosition);
    node.ChildNodes.Add(PositionTmp);
    if FStyleExsit then
    begin
      StyleTmp := doc.CreateNode('Style', ntElement);
      StyleTmp.NodeValue := FStyle.toString;
      node.ChildNodes.Add(StyleTmp);
    end;
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoElement.ToTree;
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
  TreeNodeShape.AddChild('Description');
  TreeNodeShape.AddChild('Visible');
  TreeNodeShape.AddChild('Position');
  if StyleExsit then
    TreeNodeShape.AddChild('Style');
end;

procedure TGeoElement.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  StyleAddMenu: TMenuItem;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoElementPop) and Assigned(TGeoElementTreeComponent) then
    begin
      TGeoElementPop.Clear;
      StyleAddMenu := TMenuItem.Create(TGeoElementPop);
      StyleAddMenu.Text := 'Add Style';
      StyleAddMenu.OnClick := AddStyleEvent;
      TGeoElementPop.AddObject(StyleAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TGeoElementTreeComponent.ClientToScreen(pt);
      TGeoElementPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoElement.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoElementXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Description');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Description);
  Names_Value.Add('Visible');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Visible));
  Names_Value.Add('Position');
  Types_Value.Add(xml_Coordinate);
  _Values_Value.Add(CoordinatesToString(Position));
  Names_Value.Add('Style');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Style.toString);
  TGeoElementXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoElement.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Description := _Value;
      end;
    2:
      begin
        Visible := String2Boolean(_Value);
      end;
    3:
      begin
        Position := StringToCoordinates(_Value);
      end;
    4:
      begin
        Style := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

procedure TGeoElement.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TGeoElement.SetDescription(const _Value: String);
begin
  FDescription := _Value;
end;

procedure TGeoElement.SetVisible(const _Value: Boolean);
begin
  FVisible := _Value;
end;

procedure TGeoElement.SetPosition(const _Value: ArrayCoordinates);
begin
  FPosition := _Value;
end;

function TGeoElement.AddStyle: Integer;
begin;
  Result := FStyle;
  FStyleExsit := True;
end;

procedure TGeoElement.SetStyle(const _Value: Integer);
begin
  FStyleExsit := True;
  FStyle := _Value;
end;

procedure TGeoElement.StyleRemove;
begin
  if FStyleExsit then
  begin
    FStyleExsit := False;
  end;
end;

procedure TGeoElement.AddStyleEvent(Sender: TObject);
begin
  AddStyle;
end;

{  Point}
constructor TGeoPoint.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoPoint.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoPoint.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Point Read XML Error!' + node.Xml);
  end;
end;

function TGeoPoint.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Point';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoPoint.ToTree;
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

procedure TGeoPoint.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoPointPop) and Assigned(TGeoPointTreeComponent) then
    begin
      TGeoPointPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoPointTreeComponent.ClientToScreen(pt);
      TGeoPointPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoPoint.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoPointXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TGeoPointXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoPoint.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

{  Line}
constructor TGeoLine.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoLine.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoLine.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Line Read XML Error!' + node.Xml);
  end;
end;

function TGeoLine.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Line';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoLine.ToTree;
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

procedure TGeoLine.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoLinePop) and Assigned(TGeoLineTreeComponent) then
    begin
      TGeoLinePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoLineTreeComponent.ClientToScreen(pt);
      TGeoLinePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoLine.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoLineXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  TGeoLineXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoLine.SetXMLProperty(Index: Integer; _Value: String);
begin
end;

{  Lines}
constructor TGeoLines.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoLines.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoLines.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Closed' then
      begin
        FClosed := String2Boolean(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Lines Read XML Error!' + node.Xml);
  end;
end;

function TGeoLines.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ClosedTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Lines';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    ClosedTmp := doc.CreateNode('Closed', ntElement);
    ClosedTmp.NodeValue := Boolean2String(FClosed);
    node.ChildNodes.Add(ClosedTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoLines.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Closed');
end;

procedure TGeoLines.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoLinesPop) and Assigned(TGeoLinesTreeComponent) then
    begin
      TGeoLinesPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoLinesTreeComponent.ClientToScreen(pt);
      TGeoLinesPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoLines.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoLinesXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Closed');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Closed));
  TGeoLinesXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoLines.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Closed := String2Boolean(_Value);
      end;
  end;
  ToTree;
end;

procedure TGeoLines.SetClosed(const _Value: Boolean);
begin
  FClosed := _Value;
end;

{  Polygon}
constructor TGeoPolygon.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoPolygon.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoPolygon.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Filled' then
      begin
        FFilled := String2Boolean(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('Polygon Read XML Error!' + node.Xml);
  end;
end;

function TGeoPolygon.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  FilledTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Polygon';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    FilledTmp := doc.CreateNode('Filled', ntElement);
    FilledTmp.NodeValue := Boolean2String(FFilled);
    node.ChildNodes.Add(FilledTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoPolygon.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Filled');
end;

procedure TGeoPolygon.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoPolygonPop) and Assigned(TGeoPolygonTreeComponent) then
    begin
      TGeoPolygonPop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoPolygonTreeComponent.ClientToScreen(pt);
      TGeoPolygonPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoPolygon.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoPolygonXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Filled');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Filled));
  TGeoPolygonXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoPolygon.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Filled := String2Boolean(_Value);
      end;
  end;
  ToTree;
end;

procedure TGeoPolygon.SetFilled(const _Value: Boolean);
begin
  FFilled := _Value;
end;

{  Style}
constructor TGeoStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoStyle.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoStyle.FromXML(node: IXMLNode);
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
        FName := nodeTmp.Text;
      end;
    end;
  except
    raise Exception.Create('Style Read XML Error!' + node.Xml);
  end;
end;

function TGeoStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'Style';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

function TGeoStyle.AppendToXML(node: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  NameTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := node.OwnerDocument;
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoStyle.ToTree;
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
end;

procedure TGeoStyle.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoStylePop) and Assigned(TGeoStyleTreeComponent) then
    begin
      TGeoStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoStyleTreeComponent.ClientToScreen(pt);
      TGeoStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  TGeoStyleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
  end;
  ToTree;
end;

procedure TGeoStyle.SetName(const _Value: String);
begin
  FName := _Value;
end;

{  PointStyle}
constructor TGeoPointStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TGeoPointStyle.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoPointStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Color' then
      begin
        FColor := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'Size' then
      begin
        FSize := nodeTmp.Text.ToDouble;
      end
      else if nodeTmp.NodeName = 'PointStyle' then
      begin
        FPointStyle := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'BorderStyle' then
      begin
        FBorderStyle := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'BorderColor' then
      begin
        FBorderColor := String2Hex(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'BorderWidth' then
      begin
        FBorderWidth := nodeTmp.Text.ToDouble;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('PointStyle Read XML Error!' + node.Xml);
  end;
end;

function TGeoPointStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  ColorTmp: IXMLNode;
  SizeTmp: IXMLNode;
  PointStyleTmp: IXMLNode;
  BorderStyleTmp: IXMLNode;
  BorderColorTmp: IXMLNode;
  BorderWidthTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'PointStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    ColorTmp := doc.CreateNode('Color', ntElement);
    ColorTmp.NodeValue := FColor.toString;
    node.ChildNodes.Add(ColorTmp);
    SizeTmp := doc.CreateNode('Size', ntElement);
    SizeTmp.NodeValue := FSize.ToString;
    node.ChildNodes.Add(SizeTmp);
    PointStyleTmp := doc.CreateNode('PointStyle', ntElement);
    PointStyleTmp.NodeValue := FPointStyle.toString;
    node.ChildNodes.Add(PointStyleTmp);
    BorderStyleTmp := doc.CreateNode('BorderStyle', ntElement);
    BorderStyleTmp.NodeValue := FBorderStyle.toString;
    node.ChildNodes.Add(BorderStyleTmp);
    BorderColorTmp := doc.CreateNode('BorderColor', ntElement);
    BorderColorTmp.NodeValue := Hex2String(FBorderColor);
    node.ChildNodes.Add(BorderColorTmp);
    BorderWidthTmp := doc.CreateNode('BorderWidth', ntElement);
    BorderWidthTmp.NodeValue := FBorderWidth.ToString;
    node.ChildNodes.Add(BorderWidthTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoPointStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.AddChild('Color');
  TreeNodeShape.AddChild('Size');
  TreeNodeShape.AddChild('PointStyle');
  TreeNodeShape.AddChild('BorderStyle');
  TreeNodeShape.AddChild('BorderColor');
  TreeNodeShape.AddChild('BorderWidth');
end;

procedure TGeoPointStyle.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoPointStylePop) and Assigned(TGeoPointStyleTreeComponent) then
    begin
      TGeoPointStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoPointStyleTreeComponent.ClientToScreen(pt);
      TGeoPointStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoPointStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoPointStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Color');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Color.toString);
  Names_Value.Add('Size');
  Types_Value.Add(xs_double);
  _Values_Value.Add(Size.ToString);
  Names_Value.Add('PointStyle');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(PointStyle.toString);
  Names_Value.Add('BorderStyle');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(BorderStyle.toString);
  Names_Value.Add('BorderColor');
  Types_Value.Add(xml_Color);
  _Values_Value.Add(Hex2String(BorderColor));
  Names_Value.Add('BorderWidth');
  Types_Value.Add(xs_double);
  _Values_Value.Add(BorderWidth.ToString);
  TGeoPointStyleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoPointStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Color := _Value.ToInteger;
      end;
    1:
      begin
        Size := _Value.ToDouble;
      end;
    2:
      begin
        PointStyle := _Value.ToInteger;
      end;
    3:
      begin
        BorderStyle := _Value.ToInteger;
      end;
    4:
      begin
        BorderColor := String2Hex(_Value);
      end;
    5:
      begin
        BorderWidth := _Value.ToDouble;
      end;
  end;
  ToTree;
end;

procedure TGeoPointStyle.SetColor(const _Value: Integer);
begin
  FColor := _Value;
end;

procedure TGeoPointStyle.SetSize(const _Value: Double);
begin
  FSize := _Value;
end;

procedure TGeoPointStyle.SetPointStyle(const _Value: Integer);
begin
  FPointStyle := _Value;
end;

procedure TGeoPointStyle.SetBorderStyle(const _Value: Integer);
begin
  FBorderStyle := _Value;
end;

procedure TGeoPointStyle.SetBorderColor(const _Value: TAlphaColor);
begin
  FBorderColor := _Value;
end;

procedure TGeoPointStyle.SetBorderWidth(const _Value: Double);
begin
  FBorderWidth := _Value;
end;

{  LineStyle}
constructor TGeoLineStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
  FPointStyle := TGeoPointStyle.Create(Self);
end;

destructor TGeoLineStyle.Destroy;
begin
  FPointStyle.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoLineStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'PointStyle' then
      begin
        FPointStyle := TGeoPointStyle.Create(Self);
        FPointStyle.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'LineColor' then
      begin
        FLineColor := String2Hex(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'LineWidth' then
      begin
        FLineWidth := nodeTmp.Text.ToDouble;
      end
      else if nodeTmp.NodeName = 'LineStyle' then
      begin
        FLineStyle := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'LineEndStyle' then
      begin
        FLineEndStyle := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'LineJoinStyle' then
      begin
        FLineJoinStyle := nodeTmp.Text.ToInteger;
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('LineStyle Read XML Error!' + node.Xml);
  end;
end;

function TGeoLineStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  LineColorTmp: IXMLNode;
  LineWidthTmp: IXMLNode;
  LineStyleTmp: IXMLNode;
  LineEndStyleTmp: IXMLNode;
  LineJoinStyleTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'LineStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    FPointStyle.ToXML(node, 'PointStyle');
    LineColorTmp := doc.CreateNode('LineColor', ntElement);
    LineColorTmp.NodeValue := Hex2String(FLineColor);
    node.ChildNodes.Add(LineColorTmp);
    LineWidthTmp := doc.CreateNode('LineWidth', ntElement);
    LineWidthTmp.NodeValue := FLineWidth.ToString;
    node.ChildNodes.Add(LineWidthTmp);
    LineStyleTmp := doc.CreateNode('LineStyle', ntElement);
    LineStyleTmp.NodeValue := FLineStyle.toString;
    node.ChildNodes.Add(LineStyleTmp);
    LineEndStyleTmp := doc.CreateNode('LineEndStyle', ntElement);
    LineEndStyleTmp.NodeValue := FLineEndStyle.toString;
    node.ChildNodes.Add(LineEndStyleTmp);
    LineJoinStyleTmp := doc.CreateNode('LineJoinStyle', ntElement);
    LineJoinStyleTmp.NodeValue := FLineJoinStyle.toString;
    node.ChildNodes.Add(LineJoinStyleTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoLineStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  PointStyle.TreeNodeShape := TreeNodeShape.AddChildObject('PointStyle', PointStyle);
  PointStyle.ToTree;
  TreeNodeShape.AddChild('LineColor');
  TreeNodeShape.AddChild('LineWidth');
  TreeNodeShape.AddChild('LineStyle');
  TreeNodeShape.AddChild('LineEndStyle');
  TreeNodeShape.AddChild('LineJoinStyle');
end;

procedure TGeoLineStyle.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoLineStylePop) and Assigned(TGeoLineStyleTreeComponent) then
    begin
      TGeoLineStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoLineStyleTreeComponent.ClientToScreen(pt);
      TGeoLineStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoLineStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoLineStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('LineColor');
  Types_Value.Add(xml_Color);
  _Values_Value.Add(Hex2String(LineColor));
  Names_Value.Add('LineWidth');
  Types_Value.Add(xs_double);
  _Values_Value.Add(LineWidth.ToString);
  Names_Value.Add('LineStyle');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(LineStyle.toString);
  Names_Value.Add('LineEndStyle');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(LineEndStyle.toString);
  Names_Value.Add('LineJoinStyle');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(LineJoinStyle.toString);
  TGeoLineStyleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoLineStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        LineColor := String2Hex(_Value);
      end;
    1:
      begin
        LineWidth := _Value.ToDouble;
      end;
    2:
      begin
        LineStyle := _Value.ToInteger;
      end;
    3:
      begin
        LineEndStyle := _Value.ToInteger;
      end;
    4:
      begin
        LineJoinStyle := _Value.ToInteger;
      end;
  end;
  ToTree;
end;

procedure TGeoLineStyle.SetPointStyle(const _Value: TGeoPointStyle);
begin
  FPointStyle.Free;
  FPointStyle := _Value;
  FPointStyle.Parent := Self;
end;

procedure TGeoLineStyle.SetLineColor(const _Value: TAlphaColor);
begin
  FLineColor := _Value;
end;

procedure TGeoLineStyle.SetLineWidth(const _Value: Double);
begin
  FLineWidth := _Value;
end;

procedure TGeoLineStyle.SetLineStyle(const _Value: Integer);
begin
  FLineStyle := _Value;
end;

procedure TGeoLineStyle.SetLineEndStyle(const _Value: Integer);
begin
  FLineEndStyle := _Value;
end;

procedure TGeoLineStyle.SetLineJoinStyle(const _Value: Integer);
begin
  FLineJoinStyle := _Value;
end;

{  PolygonStyle}
constructor TGeoPolygonStyle.Create(par: TXML = nil);
begin
  inherited Create(par);
  FLineStyle := TGeoLineStyle.Create(Self);
end;

destructor TGeoPolygonStyle.Destroy;
begin
  FLineStyle.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TGeoPolygonStyle.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
begin
  try
    inherited FromXML(node);
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'LineStyle' then
      begin
        FLineStyle := TGeoLineStyle.Create(Self);
        FLineStyle.FromXML(nodeTmp);
      end
      else if nodeTmp.NodeName = 'FillColor' then
      begin
        FFillColor := String2Hex(nodeTmp.Text);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
;
    end;
  except
    raise Exception.Create('PolygonStyle Read XML Error!' + node.Xml);
  end;
end;

function TGeoPolygonStyle.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  FillColorTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
  if (pt = '') or (pt[1] = '#') then
    pt := 'PolygonStyle';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    inherited AppendToXML(node);
    FLineStyle.ToXML(node, 'LineStyle');
    FillColorTmp := doc.CreateNode('FillColor', ntElement);
    FillColorTmp.NodeValue := Hex2String(FFillColor);
    node.ChildNodes.Add(FillColorTmp);
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TGeoPolygonStyle.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  LineStyle.TreeNodeShape := TreeNodeShape.AddChildObject('LineStyle', LineStyle);
  LineStyle.ToTree;
  TreeNodeShape.AddChild('FillColor');
end;

procedure TGeoPolygonStyle.OnClick(Sender: TTreeNodeShape;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
begin
  ToInspector;
  GeoXMLObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TGeoPolygonStylePop) and Assigned(TGeoPolygonStyleTreeComponent) then
    begin
      TGeoPolygonStylePop.Clear;
      pt := TPointF.Create(X, Y);
      pt := TGeoPolygonStyleTreeComponent.ClientToScreen(pt);
      TGeoPolygonStylePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TGeoPolygonStyle.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TGeoPolygonStyleXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('FillColor');
  Types_Value.Add(xml_Color);
  _Values_Value.Add(Hex2String(FillColor));
  TGeoPolygonStyleXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TGeoPolygonStyle.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        FillColor := String2Hex(_Value);
      end;
  end;
  ToTree;
end;

procedure TGeoPolygonStyle.SetLineStyle(const _Value: TGeoLineStyle);
begin
  FLineStyle.Free;
  FLineStyle := _Value;
  FLineStyle.Parent := Self;
end;

procedure TGeoPolygonStyle.SetFillColor(const _Value: TAlphaColor);
begin
  FFillColor := _Value;
end;



end.
