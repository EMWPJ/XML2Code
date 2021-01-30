unit XML2CodeBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,
  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,
  ClientScreen, XMLInspector;

type

  TXML2Code = class;
  TXML2CodeClass = class;
  TXML2CodeChild = class;

  TXML2Code = class(TXML)
  private
    FXMLClasss: TList<TXML2CodeClass>;
    FName: String;
    FUsing: String1D;
    FUsingExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetXMLClasss(const _Value: TList<TXML2CodeClass>);
    function GetXMLClass(Index: Integer): TXML2CodeClass;
    procedure SetXMLClass(Index: Integer; const _Value: TXML2CodeClass);
    procedure SetName(const _Value: String);
    procedure SetUsing(const _Value: String1D);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddXMLClassEvent(Sender: TObject);
    procedure AddUsingEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddXMLClass: TXML2CodeClass;
    procedure XMLClassClear;
    function XMLClassCount: Integer;
    procedure RemoveXMLClass(_Value: TXML2CodeClass);
    procedure DeleteXMLClass(Index: Integer);
    function AddUsing: String1D;
    procedure UsingRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property XMLClasss: TList<TXML2CodeClass> read FXMLClasss
      write SetXMLClasss;
    property XMLClass[Index: Integer]: TXML2CodeClass read GetXMLClass
      write SetXMLClass;
    property Name: String read FName write SetName;
    property Using: String1D read FUsing write SetUsing;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property UsingExsit: Boolean read FUsingExsit;
  end;

  TXML2CodeClass = class(TXML)
  private
    FChilds: TList<TXML2CodeChild>;
    FDataType: String;
    FRoot: Boolean;
    FName: String;
    FParentClass: String;
    FParentClassExsit: Boolean;
    FChildClass: String1D;
    FChildClassExsit: Boolean;
    FisAbstract: Boolean;
    FisAbstractExsit: Boolean;
    FPath: String;
    FPathExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetChilds(const _Value: TList<TXML2CodeChild>);
    function GetChild(Index: Integer): TXML2CodeChild;
    procedure SetChild(Index: Integer; const _Value: TXML2CodeChild);
    procedure SetDataType(const _Value: String);
    procedure SetRoot(const _Value: Boolean);
    procedure SetName(const _Value: String);
    procedure SetParentClass(const _Value: String);
    procedure SetChildClass(const _Value: String1D);
    procedure SetisAbstract(const _Value: Boolean);
    procedure SetPath(const _Value: String);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddChildEvent(Sender: TObject);
    procedure AddParentClassEvent(Sender: TObject);
    procedure AddChildClassEvent(Sender: TObject);
    procedure AddisAbstractEvent(Sender: TObject);
    procedure AddPathEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddChild: TXML2CodeChild;
    procedure ChildClear;
    function ChildCount: Integer;
    procedure RemoveChild(_Value: TXML2CodeChild);
    procedure DeleteChild(Index: Integer);
    function AddParentClass: String;
    procedure ParentClassRemove;
    function AddChildClass: String1D;
    procedure ChildClassRemove;
    function AddisAbstract: Boolean;
    procedure isAbstractRemove;
    function AddPath: String;
    procedure PathRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Childs: TList<TXML2CodeChild> read FChilds write SetChilds;
    property Child[Index: Integer]: TXML2CodeChild read GetChild write SetChild;
    property DataType: String read FDataType write SetDataType;
    property Root: Boolean read FRoot write SetRoot;
    property Name: String read FName write SetName;
    property ParentClass: String read FParentClass write SetParentClass;
    property ChildClass: String1D read FChildClass write SetChildClass;
    property isAbstract: Boolean read FisAbstract write SetisAbstract;
    property Path: String read FPath write SetPath;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property ParentClassExsit: Boolean read FParentClassExsit;
    property ChildClassExsit: Boolean read FChildClassExsit;
    property isAbstractExsit: Boolean read FisAbstractExsit;
    property PathExsit: Boolean read FPathExsit;
  end;

  TXML2CodeChild = class(TXML)
  private
    FName: String;
    FDataType: String;
    FPath: String;
    FNumber: Integer;
    FLeaf: Boolean;
    FVisual: Boolean;
    FisVirtual: Boolean;
    FisVirtualExsit: Boolean;
    FTreeNodeShape: TTreeNodeShape;
    procedure SetName(const _Value: String);
    procedure SetDataType(const _Value: String);
    procedure SetPath(const _Value: String);
    procedure SetNumber(const _Value: Integer);
    procedure SetLeaf(const _Value: Boolean);
    procedure SetVisual(const _Value: Boolean);
    procedure SetisVirtual(const _Value: Boolean);
  protected
    procedure FromXML(node: IXMLNode); override;
    function ToXML(par: IXMLNode; pt: string = ''): IXMLNode; override;
    procedure AddisVirtualEvent(Sender: TObject);
  public
    constructor Create(par: TXML = nil);
    destructor Destroy; override;
    function AddisVirtual: Boolean;
    procedure isVirtualRemove;
    procedure ToTree; virtual;
    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); virtual;
    procedure SetXMLProperty(Index: Integer; _Value: String); override;
    procedure ToInspector;
    property Name: String read FName write SetName;
    property DataType: String read FDataType write SetDataType;
    property Path: String read FPath write SetPath;
    property Number: Integer read FNumber write SetNumber;
    property Leaf: Boolean read FLeaf write SetLeaf;
    property Visual: Boolean read FVisual write SetVisual;
    property isVirtual: Boolean read FisVirtual write SetisVirtual;
    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape
      write FTreeNodeShape;
    property isVirtualExsit: Boolean read FisVirtualExsit;
  end;

var
  TXML2CodePop: TPopupMenu;
  TXML2CodeXMLInspector: TXMLInspector;
  TXML2CodeTreeComponent: TTree;
  TXML2CodeClassPop: TPopupMenu;
  TXML2CodeClassXMLInspector: TXMLInspector;
  TXML2CodeClassTreeComponent: TTree;
  TXML2CodeChildPop: TPopupMenu;
  TXML2CodeChildXMLInspector: TXMLInspector;
  TXML2CodeChildTreeComponent: TTree;
  XML2CodeObject: TObject;

implementation

{ XML2Code }
constructor TXML2Code.Create(par: TXML = nil);
begin
  inherited Create(par);
  FXMLClasss := TList<TXML2CodeClass>.Create;
end;

destructor TXML2Code.Destroy;
begin
  XMLClassClear;
  FXMLClasss.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TXML2Code.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  XMLClassTmp: TXML2CodeClass;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'XMLClass' then
      begin
        XMLClassTmp := TXML2CodeClass.Create(Self);
        XMLClassTmp.FromXML(nodeTmp);
        FXMLClasss.Add(XMLClassTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'using' then
      begin
        FUsing := String2String1D(nodeTmp.Text);
        FUsingExsit := True;
      end;
    end;
  except
    raise Exception.Create('XML2Code Read XML Error!' + node.Xml);
  end;
end;

function TXML2Code.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  UsingTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'XML2Code';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FXMLClasss.Count - 1 do
      FXMLClasss.Items[I].ToXML(node, 'XMLClass');
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    if FUsingExsit then
    begin
      UsingTmp := doc.CreateNode('using', ntAttribute);
      UsingTmp.NodeValue := String1D2String(FUsing);
      node.AttributeNodes.Add(UsingTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TXML2Code.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to XMLClassCount - 1 do
  begin
    XMLClasss[I].TreeNodeShape := TreeNodeShape.AddChildObject('XMLClass',
      XMLClass[I]);
    XMLClass[I].ToTree;
  end;
  TreeNodeShape.AddChild('Name');
  if UsingExsit then
    TreeNodeShape.AddChild('Using');
end;

procedure TXML2Code.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  XMLClassAddMenu: TMenuItem;
  UsingAddMenu: TMenuItem;
begin
  ToInspector;
  XML2CodeObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TXML2CodePop) and Assigned(TXML2CodeTreeComponent) then
    begin
      TXML2CodePop.Clear;
      XMLClassAddMenu := TMenuItem.Create(TXML2CodePop);
      XMLClassAddMenu.Text := 'Add XMLClass';
      XMLClassAddMenu.OnClick := AddXMLClassEvent;
      TXML2CodePop.AddObject(XMLClassAddMenu);
      UsingAddMenu := TMenuItem.Create(TXML2CodePop);
      UsingAddMenu.Text := 'Add Using';
      UsingAddMenu.OnClick := AddUsingEvent;
      TXML2CodePop.AddObject(UsingAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TXML2CodeTreeComponent.ClientToScreen(pt);
      TXML2CodePop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TXML2Code.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TXML2CodeXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('Using');
  Types_Value.Add(xml_Integer1D);
  _Values_Value.Add(String1D2String(Using));
  TXML2CodeXMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);
end;

procedure TXML2Code.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        Using := String2String1D(_Value);
      end;
  end;
  ToTree;
end;

function TXML2Code.AddXMLClass: TXML2CodeClass;
var
  XMLClassTmp: TXML2CodeClass;
begin;
  XMLClassTmp := TXML2CodeClass.Create(Self);
  FXMLClasss.Add(XMLClassTmp);
  Result := XMLClassTmp;
end;

procedure TXML2Code.SetXMLClasss(const _Value: TList<TXML2CodeClass>);
begin
  XMLClassClear;
  FXMLClasss := _Value;
end;

procedure TXML2Code.XMLClassClear;
begin
  while FXMLClasss.Count > 0 do
  begin
    FXMLClasss.Items[0].Free;
    FXMLClasss.Delete(0);
  end;
end;

function TXML2Code.XMLClassCount: Integer;
begin
  Result := FXMLClasss.Count;
end;

function TXML2Code.GetXMLClass(Index: Integer): TXML2CodeClass;
begin
  Result := FXMLClasss[Index];
end;

procedure TXML2Code.SetXMLClass(Index: Integer; const _Value: TXML2CodeClass);
begin
  _Value.Parent := Self;
  FXMLClasss[Index].Free;
  FXMLClasss[Index] := _Value;
end;

procedure TXML2Code.RemoveXMLClass(_Value: TXML2CodeClass);
begin
  FXMLClasss.Remove(_Value);
  _Value.Free;
end;

procedure TXML2Code.DeleteXMLClass(Index: Integer);
begin
  FXMLClasss.Items[Index].Free;
  FXMLClasss.Delete(Index);
end;

procedure TXML2Code.AddXMLClassEvent(Sender: TObject);
var
  tmp: TXML2CodeClass;
begin
  tmp := AddXMLClass;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('XMLClass', tmp);
  tmp.ToTree;
end;

procedure TXML2Code.SetName(const _Value: String);
begin
  FName := _Value;
end;

function TXML2Code.AddUsing: String1D;
begin;
  Result := FUsing;
  FUsingExsit := True;
end;

procedure TXML2Code.SetUsing(const _Value: String1D);
begin
  FUsingExsit := True;
  FUsing := _Value;
end;

procedure TXML2Code.UsingRemove;
begin
  if FUsingExsit then
  begin
    FUsingExsit := False;
  end;
end;

procedure TXML2Code.AddUsingEvent(Sender: TObject);
begin
  AddUsing;
end;

{ Class }
constructor TXML2CodeClass.Create(par: TXML = nil);
begin
  inherited Create(par);
  FChilds := TList<TXML2CodeChild>.Create;
end;

destructor TXML2CodeClass.Destroy;
begin
  ChildClear;
  FChilds.Free;
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TXML2CodeClass.FromXML(node: IXMLNode);
var
  I: Integer;
  nodeTmp: IXMLNode;
  ChildTmp: TXML2CodeChild;
begin
  try
    for I := 0 to node.ChildNodes.Count - 1 do
    begin
      nodeTmp := node.ChildNodes.Get(I);
      if nodeTmp.NodeName = 'Child' then
      begin
        ChildTmp := TXML2CodeChild.Create(Self);
        ChildTmp.FromXML(nodeTmp);
        FChilds.Add(ChildTmp);
      end;
    end;
    for I := 0 to node.AttributeNodes.Count - 1 do
    begin
      nodeTmp := node.AttributeNodes.Get(I);
      if nodeTmp.NodeName = 'type' then
      begin
        FDataType := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'Root' then
      begin
        FRoot := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'parentClass' then
      begin
        FParentClass := nodeTmp.Text;
        FParentClassExsit := True;
      end
      else if nodeTmp.NodeName = 'childClass' then
      begin
        FChildClass := String2String1D(nodeTmp.Text);
        FChildClassExsit := True;
      end
      else if nodeTmp.NodeName = 'abstract' then
      begin
        FisAbstract := String2Boolean(nodeTmp.Text);
        FisAbstractExsit := True;
      end
      else if nodeTmp.NodeName = 'path' then
      begin
        FPath := nodeTmp.Text;
        FPathExsit := True;
      end;
    end;
  except
    raise Exception.Create('Class Read XML Error!' + node.Xml);
  end;
end;

function TXML2CodeClass.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  DataTypeTmp: IXMLNode;
  RootTmp: IXMLNode;
  NameTmp: IXMLNode;
  ParentClassTmp: IXMLNode;
  ChildClassTmp: IXMLNode;
  isAbstractTmp: IXMLNode;
  PathTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Class';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    for I := 0 to FChilds.Count - 1 do
      FChilds.Items[I].ToXML(node, 'Child');
    DataTypeTmp := doc.CreateNode('type', ntAttribute);
    DataTypeTmp.NodeValue := FDataType;
    node.AttributeNodes.Add(DataTypeTmp);
    RootTmp := doc.CreateNode('Root', ntAttribute);
    RootTmp.NodeValue := Boolean2String(FRoot);
    node.AttributeNodes.Add(RootTmp);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    if FParentClassExsit then
    begin
      ParentClassTmp := doc.CreateNode('parentClass', ntAttribute);
      ParentClassTmp.NodeValue := FParentClass;
      node.AttributeNodes.Add(ParentClassTmp);
    end;
    if FChildClassExsit then
    begin
      ChildClassTmp := doc.CreateNode('childClass', ntAttribute);
      ChildClassTmp.NodeValue := String1D2String(FChildClass);
      node.AttributeNodes.Add(ChildClassTmp);
    end;
    if FisAbstractExsit then
    begin
      isAbstractTmp := doc.CreateNode('abstract', ntAttribute);
      isAbstractTmp.NodeValue := Boolean2String(FisAbstract);
      node.AttributeNodes.Add(isAbstractTmp);
    end;
    if FPathExsit then
    begin
      PathTmp := doc.CreateNode('path', ntAttribute);
      PathTmp.NodeValue := FPath;
      node.AttributeNodes.Add(PathTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TXML2CodeClass.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  for I := 0 to ChildCount - 1 do
  begin
    Childs[I].TreeNodeShape := TreeNodeShape.AddChildObject('Child', Child[I]);
    Child[I].ToTree;
  end;
  TreeNodeShape.AddChild('DataType');
  TreeNodeShape.AddChild('Root');
  TreeNodeShape.AddChild('Name');
  if ParentClassExsit then
    TreeNodeShape.AddChild('ParentClass');
  if ChildClassExsit then
    TreeNodeShape.AddChild('ChildClass');
  if isAbstractExsit then
    TreeNodeShape.AddChild('isAbstract');
  if PathExsit then
    TreeNodeShape.AddChild('Path');
end;

procedure TXML2CodeClass.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  ChildAddMenu: TMenuItem;
  ParentClassAddMenu: TMenuItem;
  ChildClassAddMenu: TMenuItem;
  isAbstractAddMenu: TMenuItem;
  PathAddMenu: TMenuItem;
begin
  ToInspector;
  XML2CodeObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TXML2CodeClassPop) and Assigned(TXML2CodeClassTreeComponent)
    then
    begin
      TXML2CodeClassPop.Clear;
      ChildAddMenu := TMenuItem.Create(TXML2CodeClassPop);
      ChildAddMenu.Text := 'Add Child';
      ChildAddMenu.OnClick := AddChildEvent;
      TXML2CodeClassPop.AddObject(ChildAddMenu);
      ParentClassAddMenu := TMenuItem.Create(TXML2CodeClassPop);
      ParentClassAddMenu.Text := 'Add ParentClass';
      ParentClassAddMenu.OnClick := AddParentClassEvent;
      TXML2CodeClassPop.AddObject(ParentClassAddMenu);
      ChildClassAddMenu := TMenuItem.Create(TXML2CodeClassPop);
      ChildClassAddMenu.Text := 'Add ChildClass';
      ChildClassAddMenu.OnClick := AddChildClassEvent;
      TXML2CodeClassPop.AddObject(ChildClassAddMenu);
      isAbstractAddMenu := TMenuItem.Create(TXML2CodeClassPop);
      isAbstractAddMenu.Text := 'Add isAbstract';
      isAbstractAddMenu.OnClick := AddisAbstractEvent;
      TXML2CodeClassPop.AddObject(isAbstractAddMenu);
      PathAddMenu := TMenuItem.Create(TXML2CodeClassPop);
      PathAddMenu.Text := 'Add Path';
      PathAddMenu.OnClick := AddPathEvent;
      TXML2CodeClassPop.AddObject(PathAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TXML2CodeClassTreeComponent.ClientToScreen(pt);
      TXML2CodeClassPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TXML2CodeClass.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TXML2CodeClassXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('DataType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(DataType);
  Names_Value.Add('Root');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Root));
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('ParentClass');
  Types_Value.Add(xs_string);
  _Values_Value.Add(ParentClass);
  Names_Value.Add('ChildClass');
  Types_Value.Add(xml_Integer1D);
  _Values_Value.Add(String1D2String(ChildClass));
  Names_Value.Add('isAbstract');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(isAbstract));
  Names_Value.Add('Path');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Path);
  TXML2CodeClassXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TXML2CodeClass.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        DataType := _Value;
      end;
    1:
      begin
        Root := String2Boolean(_Value);
      end;
    2:
      begin
        Name := _Value;
      end;
    3:
      begin
        ParentClass := _Value;
      end;
    4:
      begin
        ChildClass := String2String1D(_Value);
      end;
    5:
      begin
        isAbstract := String2Boolean(_Value);
      end;
    6:
      begin
        Path := _Value;
      end;
  end;
  ToTree;
end;

function TXML2CodeClass.AddChild: TXML2CodeChild;
var
  ChildTmp: TXML2CodeChild;
begin;
  ChildTmp := TXML2CodeChild.Create(Self);
  FChilds.Add(ChildTmp);
  Result := ChildTmp;
end;

procedure TXML2CodeClass.SetChilds(const _Value: TList<TXML2CodeChild>);
begin
  ChildClear;
  FChilds := _Value;
end;

procedure TXML2CodeClass.ChildClear;
begin
  while FChilds.Count > 0 do
  begin
    FChilds.Items[0].Free;
    FChilds.Delete(0);
  end;
end;

function TXML2CodeClass.ChildCount: Integer;
begin
  Result := FChilds.Count;
end;

function TXML2CodeClass.GetChild(Index: Integer): TXML2CodeChild;
begin
  Result := FChilds[Index];
end;

procedure TXML2CodeClass.SetChild(Index: Integer; const _Value: TXML2CodeChild);
begin
  _Value.Parent := Self;
  FChilds[Index].Free;
  FChilds[Index] := _Value;
end;

procedure TXML2CodeClass.RemoveChild(_Value: TXML2CodeChild);
begin
  FChilds.Remove(_Value);
  _Value.Free;
end;

procedure TXML2CodeClass.DeleteChild(Index: Integer);
begin
  FChilds.Items[Index].Free;
  FChilds.Delete(Index);
end;

procedure TXML2CodeClass.AddChildEvent(Sender: TObject);
var
  tmp: TXML2CodeChild;
begin
  tmp := AddChild;
  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject('Child', tmp);
  tmp.ToTree;
end;

procedure TXML2CodeClass.SetDataType(const _Value: String);
begin
  FDataType := _Value;
end;

procedure TXML2CodeClass.SetRoot(const _Value: Boolean);
begin
  FRoot := _Value;
end;

procedure TXML2CodeClass.SetName(const _Value: String);
begin
  FName := _Value;
end;

function TXML2CodeClass.AddParentClass: String;
begin;
  Result := FParentClass;
  FParentClassExsit := True;
end;

procedure TXML2CodeClass.SetParentClass(const _Value: String);
begin
  FParentClassExsit := True;
  FParentClass := _Value;
end;

procedure TXML2CodeClass.ParentClassRemove;
begin
  if FParentClassExsit then
  begin
    FParentClassExsit := False;
  end;
end;

procedure TXML2CodeClass.AddParentClassEvent(Sender: TObject);
begin
  AddParentClass;
end;

function TXML2CodeClass.AddChildClass: String1D;
begin;
  Result := FChildClass;
  FChildClassExsit := True;
end;

procedure TXML2CodeClass.SetChildClass(const _Value: String1D);
begin
  FChildClassExsit := True;
  FChildClass := _Value;
end;

procedure TXML2CodeClass.ChildClassRemove;
begin
  if FChildClassExsit then
  begin
    FChildClassExsit := False;
  end;
end;

procedure TXML2CodeClass.AddChildClassEvent(Sender: TObject);
begin
  AddChildClass;
end;

function TXML2CodeClass.AddisAbstract: Boolean;
begin;
  Result := FisAbstract;
  FisAbstractExsit := True;
end;

procedure TXML2CodeClass.SetisAbstract(const _Value: Boolean);
begin
  FisAbstractExsit := True;
  FisAbstract := _Value;
end;

procedure TXML2CodeClass.isAbstractRemove;
begin
  if FisAbstractExsit then
  begin
    FisAbstractExsit := False;
  end;
end;

procedure TXML2CodeClass.AddisAbstractEvent(Sender: TObject);
begin
  AddisAbstract;
end;

function TXML2CodeClass.AddPath: String;
begin;
  Result := FPath;
  FPathExsit := True;
end;

procedure TXML2CodeClass.SetPath(const _Value: String);
begin
  FPathExsit := True;
  FPath := _Value;
end;

procedure TXML2CodeClass.PathRemove;
begin
  if FPathExsit then
  begin
    FPathExsit := False;
  end;
end;

procedure TXML2CodeClass.AddPathEvent(Sender: TObject);
begin
  AddPath;
end;

{ Child }
constructor TXML2CodeChild.Create(par: TXML = nil);
begin
  inherited Create(par);
end;

destructor TXML2CodeChild.Destroy;
begin
  if Assigned(FTreeNodeShape) then
    FTreeNodeShape.Free;
  inherited;
end;

procedure TXML2CodeChild.FromXML(node: IXMLNode);
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
      if nodeTmp.NodeName = 'name' then
      begin
        FName := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'type' then
      begin
        FDataType := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'path' then
      begin
        FPath := nodeTmp.Text;
      end
      else if nodeTmp.NodeName = 'number' then
      begin
        FNumber := nodeTmp.Text.ToInteger;
      end
      else if nodeTmp.NodeName = 'leaf' then
      begin
        FLeaf := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'visual' then
      begin
        FVisual := String2Boolean(nodeTmp.Text);
      end
      else if nodeTmp.NodeName = 'virtual' then
      begin
        FisVirtual := String2Boolean(nodeTmp.Text);
        FisVirtualExsit := True;
      end;
    end;
  except
    raise Exception.Create('Child Read XML Error!' + node.Xml);
  end;
end;

function TXML2CodeChild.ToXML(par: IXMLNode; pt: string): IXMLNode;
var
  doc: IXMLDocument;
  node: IXMLNode;
  NameTmp: IXMLNode;
  DataTypeTmp: IXMLNode;
  PathTmp: IXMLNode;
  NumberTmp: IXMLNode;
  LeafTmp: IXMLNode;
  VisualTmp: IXMLNode;
  isVirtualTmp: IXMLNode;
  I: Integer;
begin
  try
    doc := par.OwnerDocument;
    if (pt = '') or (pt[1] = '#') then
      pt := 'Child';
    node := doc.CreateNode(pt);
    par.ChildNodes.Add(node);
    NameTmp := doc.CreateNode('name', ntAttribute);
    NameTmp.NodeValue := FName;
    node.AttributeNodes.Add(NameTmp);
    DataTypeTmp := doc.CreateNode('type', ntAttribute);
    DataTypeTmp.NodeValue := FDataType;
    node.AttributeNodes.Add(DataTypeTmp);
    PathTmp := doc.CreateNode('path', ntAttribute);
    PathTmp.NodeValue := FPath;
    node.AttributeNodes.Add(PathTmp);
    NumberTmp := doc.CreateNode('number', ntAttribute);
    NumberTmp.NodeValue := FNumber.toString;
    node.AttributeNodes.Add(NumberTmp);
    LeafTmp := doc.CreateNode('leaf', ntAttribute);
    LeafTmp.NodeValue := Boolean2String(FLeaf);
    node.AttributeNodes.Add(LeafTmp);
    VisualTmp := doc.CreateNode('visual', ntAttribute);
    VisualTmp.NodeValue := Boolean2String(FVisual);
    node.AttributeNodes.Add(VisualTmp);
    if FisVirtualExsit then
    begin
      isVirtualTmp := doc.CreateNode('virtual', ntAttribute);
      isVirtualTmp.NodeValue := Boolean2String(FisVirtual);
      node.AttributeNodes.Add(isVirtualTmp);
    end;
    Result := node;
  except
    raise Exception.Create('XML2Code Write XML Error!');
  end;
end;

procedure TXML2CodeChild.ToTree;
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
  TreeNodeShape.AddChild('DataType');
  TreeNodeShape.AddChild('Path');
  TreeNodeShape.AddChild('Number');
  TreeNodeShape.AddChild('Leaf');
  TreeNodeShape.AddChild('Visual');
  if isVirtualExsit then
    TreeNodeShape.AddChild('isVirtual');
end;

procedure TXML2CodeChild.OnClick(Sender: TTreeNodeShape; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPointF;
  isVirtualAddMenu: TMenuItem;
begin
  ToInspector;
  XML2CodeObject := Self;
  if Button = TMouseButton.mbRight then
  begin
    if Assigned(TXML2CodeChildPop) and Assigned(TXML2CodeChildTreeComponent)
    then
    begin
      TXML2CodeChildPop.Clear;
      isVirtualAddMenu := TMenuItem.Create(TXML2CodeChildPop);
      isVirtualAddMenu.Text := 'Add isVirtual';
      isVirtualAddMenu.OnClick := AddisVirtualEvent;
      TXML2CodeChildPop.AddObject(isVirtualAddMenu);
      pt := TPointF.Create(X, Y);
      pt := TXML2CodeChildTreeComponent.ClientToScreen(pt);
      TXML2CodeChildPop.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TXML2CodeChild.ToInspector;
var
  Names_Value: TStringList;
  Types_Value: TList<XMLTypes>;
  _Values_Value: TStringList;
begin
  if not Assigned(TXML2CodeChildXMLInspector) then
    Exit;
  Names_Value := TStringList.Create;
  Types_Value := TList<XMLTypes>.Create;
  _Values_Value := TStringList.Create;
  Names_Value.Add('Name');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Name);
  Names_Value.Add('DataType');
  Types_Value.Add(xs_string);
  _Values_Value.Add(DataType);
  Names_Value.Add('Path');
  Types_Value.Add(xs_string);
  _Values_Value.Add(Path);
  Names_Value.Add('Number');
  Types_Value.Add(xs_integer);
  _Values_Value.Add(Number.toString);
  Names_Value.Add('Leaf');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Leaf));
  Names_Value.Add('Visual');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(Visual));
  Names_Value.Add('isVirtual');
  Types_Value.Add(xs_boolean);
  _Values_Value.Add(Boolean2String(isVirtual));
  TXML2CodeChildXMLInspector.SetData(Names_Value, _Values_Value,
    Types_Value, Self);
end;

procedure TXML2CodeChild.SetXMLProperty(Index: Integer; _Value: String);
begin
  case index of
    0:
      begin
        Name := _Value;
      end;
    1:
      begin
        DataType := _Value;
      end;
    2:
      begin
        Path := _Value;
      end;
    3:
      begin
        Number := _Value.ToInteger;
      end;
    4:
      begin
        Leaf := String2Boolean(_Value);
      end;
    5:
      begin
        Visual := String2Boolean(_Value);
      end;
    6:
      begin
        isVirtual := String2Boolean(_Value);
      end;
  end;
  ToTree;
end;

procedure TXML2CodeChild.SetName(const _Value: String);
begin
  FName := _Value;
end;

procedure TXML2CodeChild.SetDataType(const _Value: String);
begin
  FDataType := _Value;
end;

procedure TXML2CodeChild.SetPath(const _Value: String);
begin
  FPath := _Value;
end;

procedure TXML2CodeChild.SetNumber(const _Value: Integer);
begin
  FNumber := _Value;
end;

procedure TXML2CodeChild.SetLeaf(const _Value: Boolean);
begin
  FLeaf := _Value;
end;

procedure TXML2CodeChild.SetVisual(const _Value: Boolean);
begin
  FVisual := _Value;
end;

function TXML2CodeChild.AddisVirtual: Boolean;
begin;
  Result := FisVirtual;
  FisVirtualExsit := True;
end;

procedure TXML2CodeChild.SetisVirtual(const _Value: Boolean);
begin
  FisVirtualExsit := True;
  FisVirtual := _Value;
end;

procedure TXML2CodeChild.isVirtualRemove;
begin
  if FisVirtualExsit then
  begin
    FisVirtualExsit := False;
  end;
end;

procedure TXML2CodeChild.AddisVirtualEvent(Sender: TObject);
begin
  AddisVirtual;
end;

end.
