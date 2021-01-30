unit XML2Code;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, XMLCore, Xml.XMLDoc, Xml.XMLIntf,
  System.Generics.Collections, XML2CodeBase, XMLLeafTypes, FMX.Dialogs;

type

  TXML2CodeHelper = class helper for TXML2Code
  private
    function ClassOf(const dt: String): TXML2CodeClass;
    function BaseUse: string;
    function BaseTypes: string;
    function BaseImplements: string;
    function BaseStatements: string;
    function BaseVars: string;
    function Use: string;
    function Types: string;
    function Implements: string;
    function Statements: string;
    function ExportFrameFMX(rName: string): string;
    function ExportFramePas(rName, rType: string): string;
  public
    procedure ExportBaseCode(filePath: string);
    procedure ExportCode(filePath: string);

    procedure ExportFrame(filePath: string; rootName: string = '';
      rootType: string = '');
    procedure ToTree; overload;
    procedure Load(const fileName: string); overload;
  end;

  TXML2CodeClassHelper = class helper for TXML2CodeClass
  private
    function ParentX2C: TXML2Code;
    function BaseImplements: string;
    function BaseStatements: string;
    function BaseProtectedStatements: string;
    function BasePublicStatements: string;
    function BasePrivateStatements: string;
    function BaseConstructorStatements: string;
    function BaseConstructorImplements: string;
    function BaseDestructorStatements: string;
    function BaseDestructorImplements: string;
    function BaseVars: string;
    function PrivateStatements: string;
    function ProtectedStatements: string;
    function PublicStatements: string;
    function Implements: string;
    function Statements: string;
    function FromXMLImplement: string;
    function FromXMLStatement: string;
    function ToXMLImplement: string;
    function ToXMLStatement: string;
    function AppendToXMLImplement: string;
    function AppendToXMLStatement: string;

    function ToTreeImplement: string;
    function ToTreeStatement: string;
    function OnClickImplement: string;
    function OnClickStatement: string;
    function DeleteSelfEventStatement: string;
    function DeleteSelfEventImplement: string;
    function DeleteSelfMenuStatement: string;
    function DeleteSelfMenuImplement: string;
    function TreeNodeShapeProperty: string;

    function SetXMLPropertyStatement: string;
    function SetXMLPropertyImplement: string;
    function ToInspectorStatement: string;
    function ToInspectorImplement: string;

  public
    procedure ToTree; overload;
  end;

  TXML2CodeChildHelper = class helper for TXML2CodeChild
  private
    function ChildClass: TList<TXML2CodeClass>;
    function ChildFieldStatement: string;
    function ChildAddImplement: string;
    function ChildAddStatement: string;
    function ChildCountImplement: string;
    function ChildCountStatement: string;
    function ChildPropertyStatement: string;
    function ChildSetImplement: string;
    function ChildSetStatement: string;
    function ChildRemoveImplement: string;
    function ChildRemoveStatement: string;
    function ChildClearImplement: string;
    function ChildClearStatement: string;
    function ChildDeleteImplement: string;
    function ChildDeleteStatement: string;
    function ChildIndexPropertyStatement: string;
    function ChildGetIndexImplement: string;
    function ChildGetIndexStatement: string;
    function ChildSetIndexImplement: string;
    function ChildSetIndexStatement: string;
    function ChildExsitProperty: string;

    function ChildAddEventImplement: string;
    function ChildAddEventStatement: string;
    function ChildAddEventMenuImplement: string;
    function ChildAddEventMenuStatement: string;
    function ChildToTree: string;

    function ChildPrivateStatements: string;
    function ChildPublicStatements: string;
    function ChildProtectedStatements: string;
    function ChildBaseImplements: string;

    function LeafFieldStatement: string;
    function LeafAddImplement: string;
    function LeafAddStatement: string;
    function LeafClearImplement: string;
    function LeafClearStatement: string;
    function LeafCountImplement: string;
    function LeafCountStatement: string;
    function LeafPropertyStatement: string;
    function LeafSetImplement: string;
    function LeafSetStatement: string;
    function LeafRemoveImplement: string;
    function LeafRemoveStatement: string;
    function LeafDeleteImplement: string;
    function LeafDeleteStatement: string;
    function LeafIndexPropertyStatement: string;
    function LeafGetIndexImplement: string;
    function LeafGetIndexStatement: string;
    function LeafSetIndexImplement: string;
    function LeafSetIndexStatement: string;
    function LeafExsitProperty: string;

    function LeafAddEventImplement: string;
    function LeafAddEventStatement: string;
    function LeafAddEventMenuImplement: string;
    function LeafAddEventMenuStatement: string;
    function LeafToTree: string;

    function LeafPrivateStatements: string;
    function LeafProtectedStatements: string;
    function LeafPublicStatements: string;
    function LeafBaseImplements: string;

    function LeafToInspectorImplement: string;
    function LeafSetXMLPropertyImplement(var Index: Integer): string;
    function LeafType: string;
    function ParentClass: TXML2CodeClass;
    function ConvertStr: string;
    function ConvertXML(_Value: string = 'nodeTmp.Text'): string;
    function NodeType: TNodeType;

    function ReadAttribute: string;
    function ReadChild: string;
    function ReadText: string;

    function WriteAttribute: string;
    function WriteChild: string;
    function WriteText: string;
  public
    procedure ToTree; overload;
  end;

var
  VisualCode: Boolean;

implementation

{ TXML2CodeHelper }
function TXML2CodeHelper.BaseImplements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].BaseImplements;
  end;
  Result := Result + #13#10;
end;

function TXML2CodeHelper.BaseStatements: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].BaseStatements;
  end;
end;

function TXML2CodeHelper.BaseTypes: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '  ' + XMLClass[I].DataType + '= class;'#13#10;
  end;
end;

function TXML2CodeHelper.BaseUse: string;
var
  I, count: Integer;
begin
  if VisualCode then
    Result := 'uses'#13#10 +
      '  System.SysUtils, System.Types, System.UITypes, System.Classes,'#13#10 +
      '  System.Variants, FMX.Types, System.Generics.Collections, FMX.Dialogs,'#13#10
      + '  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, FMXTee.Tree, FMX.Menus,'#13#10
      + '  ClientScreen, XMLInspector'
  else
    Result := 'uses'#13#10 +
      '  System.SysUtils, System.Types, System.UITypes, System.Classes,'#13#10 +
      '  System.Variants, FMX.Types, System.Generics.Collections,'#13#10 +
      '  XMLCore, Xml.XMLDoc, Xml.XMLIntf, XMLLeafTypes, XMLInspector';
  if Self.UsingExsit then
  begin
    count := Length(Self.Using);
    for I := 0 to count - 1 do
    begin
      Result := Result + ', ' + Self.Using[I];
    end;
  end;
  Result := Result + ';'#13#10#13#10;
end;

function TXML2CodeHelper.BaseVars: string;
var
  I: Integer;
begin
  Result := '';
  if VisualCode then
  begin
    Result := Result + 'var'#13#10;
    for I := 0 to XMLClassCount - 1 do
    begin
      Result := Result + XMLClass[I].BaseVars;
    end;
    Result := Result + '  ' + Name + 'Object: TObject;'#13#10;
  end;
end;

function TXML2CodeHelper.ClassOf(const dt: String): TXML2CodeClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to XMLClassCount - 1 do
  begin
    if XMLClass[I].DataType = dt then
    begin
      Result := XMLClass[I];
      Exit;
    end;
  end;
end;

procedure TXML2CodeHelper.ExportBaseCode(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + 'Base;'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.BaseUse);
  strs.Add(Self.BaseTypes);
  strs.Add(Self.BaseStatements);
  strs.Add(Self.BaseVars);
  strs.Add(Self.BaseImplements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Self.Name + 'Base.pas');
  strs.Free;
end;

procedure TXML2CodeHelper.ExportCode(filePath: string);
var
  strs: TStringList;
begin
  strs := TStringList.Create;
  strs.Add('unit ' + Name + ';'#13#10 + ''#13#10 + 'interface'#13#10);
  strs.Add(Self.Use);
  strs.Add(Self.Types);
  strs.Add(Self.Statements);
  strs.Add(Self.Implements);
  strs.Add('end.');
  strs.SaveToFile(filePath + '\' + Name + '.pas');
  strs.Free;
end;

procedure TXML2CodeHelper.ExportFrame(filePath: string; rootName: string = '';
  rootType: string = '');
var
  strs: TStringList;
  rName, rType: string;
begin
  rName := rootName;
  rType := rootType;
  if rName.IsEmpty then
    rName := Name;
  if rType.IsEmpty then
    rType := XMLClass[0].DataType;
  strs := TStringList.Create;
  strs.Add(Self.ExportFrameFMX(rName));
  strs.SaveToFile(filePath + '\' + rName + 'Frame.fmx');
  strs.Clear;
  strs.Add(Self.ExportFramePas(rName, rType));
  strs.SaveToFile(filePath + '\' + rName + 'Frame.pas');
  strs.Free;
end;

function TXML2CodeHelper.ExportFrameFMX(rName: string): string;
begin

  Result := 'object Frame' + rName + ': TFrame' + rName + ''#13#10 + // 0
    '  Size.Width = 281.000000000000000000'#13#10 + // 1
    '  Size.Height = 462.000000000000000000'#13#10 + // 2
    '  Size.PlatformDefault = False'#13#10 + // 3
    '  object SplitterBottom: TSplitter'#13#10 + // 4
    '    Align = Bottom'#13#10 + // 5
    '    Cursor = crVSplit'#13#10 + // 6
    '    MinSize = 20.000000000000000000'#13#10 + // 7
    '    Position.Y = 325.000000000000000000'#13#10 + // 8
    '    Size.Width = 281.000000000000000000'#13#10 + // 9
    '    Size.Height = 3.000000000000000000'#13#10 + // 10
    '    Size.PlatformDefault = False'#13#10 + // 11
    '  end'#13#10 + // 12
    '  object ' + rName + 'MenuBar: TMenuBar'#13#10 + // 13
    '    Size.Width = 281.000000000000000000'#13#10 + // 14
    '    Size.Height = 24.000000000000000000'#13#10 + // 15
    '    Size.PlatformDefault = False'#13#10 + // 16
    '    TabOrder = 2'#13#10 + // 17
    '    object New' + rName + 'Button: TMenuItem'#13#10 + // 18
    '      Text = ''New ' + rName + ''''#13#10 + // 19
    '      OnClick = New' + rName + ''#13#10 + // 20
    '    end'#13#10 + // 21
    '    object Open' + rName + 'Button: TMenuItem'#13#10 + // 22
    '      Text = ''Open ' + rName + ''''#13#10 + // 23
    '      OnClick = Open' + rName + ''#13#10 + // 24
    '    end'#13#10 + // 25
    '    object Save' + rName + 'Button: TMenuItem'#13#10 + // 26
    '      Text = ''Save ' + rName + ''''#13#10 + // 27
    '      OnClick = Save' + rName + ''#13#10 + // 28
    '    end'#13#10 + // 29
    '  end'#13#10 + // 30
    '  object OpenDialog' + rName + ': TOpenDialog'#13#10 + // 31
    '    Filter = ''' + rName + '|*.' + rName + ''''#13#10 + // 32
    '    Left = 16'#13#10 + // 33
    '    Top = 96'#13#10 + // 34
    '  end'#13#10 + // 35
    '  object SaveDialog' + rName + ': TSaveDialog'#13#10 + // 36
    '    Filter = ''' + rName + '|*.' + rName + ''''#13#10 + // 37
    '    Left = 104'#13#10 + // 38
    '    Top = 96'#13#10 + // 39
    '  end'#13#10 + // 40
    '  object ' + rName + 'Ins: TXMLInspector'#13#10 + // 41
    '    Align = Bottom'#13#10 + // 42
    '    Position.Y = 328.000000000000000000'#13#10 + // 43
    '    Size.Width = 281.000000000000000000'#13#10 + // 44
    '    Size.Height = 134.000000000000000000'#13#10 + // 45
    '    Size.PlatformDefault = False'#13#10 + // 46
    '    TabOrder = 5'#13#10 + // 47
    '    Viewport.Width = 281.000000000000000000'#13#10 + // 48
    '    Viewport.Height = 134.000000000000000000'#13#10 + // 49
    '  end'#13#10 + // 50
    '  object ' + rName + 'Tree: TXMLTree'#13#10 + // 51
    '    BufferedDisplay = False'#13#10 + // 52
    '    CrossBox.Border.Fill.Color = claGray'#13#10 + // 53
    '    Grid.BigPen.Fill.Color = claGray'#13#10 + // 54
    '    Grid.Pen.Fill.Color = claSilver'#13#10 + // 55
    '    HotTrack.Font.StyleExt = {04040000000000000004000000}'#13#10 + // 56
    '    Page.Border.Fill.Color = claDarkgray'#13#10 + // 57
    '    Selected.Border.Fill.Color = claYellow'#13#10 + // 58
    '    Zoom.Brush.Kind = None'#13#10 + // 59
    '    Align = Client'#13#10 + // 60
    '    TabOrder = 6'#13#10 + // 61
    '  end'#13#10 + // 62
    'end'#13#10;
end;

function TXML2CodeHelper.ExportFramePas(rName, rType: string): string;
var
  I: Integer;
begin
  Result := 'unit ' + rName + 'Frame;'#13#10 + // 0
    ''#13#10 + // 1
    'interface'#13#10 + // 2
    ''#13#10 + // 3
    'uses'#13#10 + // 4
    '  System.SysUtils, System.Types, System.UITypes, System.Classes,'#13#10 +
  // 5
    '  System.Variants, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,'#13#10
    + // 6
    '  FMX.Dialogs, FMX.StdCtrls, FMX.Menus, FMX.Controls.Presentation,'#13#10 +
  // 7
    '  FMXTee.Procs, FMXTee.Tree, XMLCore, XMLLeafTypes, XMLInspector, XMLTree,'#13#10
    +
  // 8
    '  ' + Name + ', ' + Name + 'Base;'#13#10 + // 9
    ''#13#10 + // 10
    'type'#13#10 + // 11
    '  TFrame' + rName + ' = class(TFrame)'#13#10 + // 12
    '    ' + rName + 'Tree: TXMLTree;'#13#10 + // 13
    '    ' + rName + 'Ins: TXMLInspector;'#13#10 + // 13
    '    SplitterBottom: TSplitter;'#13#10 + // 15
    '    ' + rName + 'MenuBar: TMenuBar;'#13#10 + // 17
  // '    ' + rName + 'Menu: TMainMenu;'#13#10 + // 16
  // '    FileMenu: TMenuItem;'#13#10 + // 18
  // '    New' + rName + 'Menu: TMenuItem;'#13#10 + // 19
  // '    Open' + rName + 'Menu: TMenuItem;'#13#10 + // 20
  // '    Save' + rName + 'Menu: TMenuItem;'#13#10 + // 21
    '    New' + rName + 'Button: TMenuItem;'#13#10 + // 22
    '    Open' + rName + 'Button: TMenuItem;'#13#10 + // 23
    '    Save' + rName + 'Button: TMenuItem;'#13#10 + // 24
    '    OpenDialog' + rName + ': TOpenDialog;'#13#10 + // 25
    '    SaveDialog' + rName + ': TSaveDialog;'#13#10 + // 26
    '    procedure New' + rName + '(Sender: TObject);'#13#10 + // 27
    '    procedure Open' + rName + '(Sender: TObject);'#13#10 + // 28
    '    procedure Save' + rName + '(Sender: TObject);'#13#10 + // 29
    '  private'#13#10 + // 30
    '    F' + rName + 'Obj: ' + rType + ';'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '    F' + XMLClass[I].DataType +
      'Pop: TPopupMenu;'#13#10;
  end;
  Result := Result + ''#13#10 + // 57
    '    ' + rName + 'TreeRoot: TTreeNodeShape;'#13#10 + // 58
    ''#13#10 + // 60
    '  public'#13#10 + // 61
    '    function Get' + rName + 'Obj: ' + rType + ';'#13#10 + // 62
    '    procedure Set' + rName + 'Obj(const _Value: ' + rType + ');'#13#10 +
  // 63
    '    procedure FrameInit;'#13#10 + // 64
    '    procedure FrameFinal;'#13#10 + // 64
    '    property ' + rName + 'Obj: ' + rType + ' read Get' + rName +
    'Obj write Set' + rName + 'Obj;'#13#10 + // 65
    ''#13#10 + // 66
    '  end;'#13#10 + // 67
    ''#13#10 + // 68
    'implementation'#13#10 + // 69
    ''#13#10 + // 70
    '{$R *.fmx}'#13#10 + // 71
    ''#13#10 + // 72
    'procedure TFrame' + rName + '.FrameInit;'#13#10 + // 73
    'begin'#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '  F' + XMLClass[I].DataType +
      'Pop := TPopupMenu.Create(Self);'#13#10 + '  F' + XMLClass[I].DataType +
      'Pop.Parent := Self;'#13#10 + '  ' + XMLClass[I].DataType + 'Pop := F' +
      XMLClass[I].DataType + 'Pop;'#13#10#13#10;
  end;
  Result := Result + '  if Not Assigned(F' + rName + 'Obj) then'#13#10 + // 151
    '    F' + rName + 'Obj := ' + rType + '.Create(nil);'#13#10 + // 152
    '  if Not Assigned(' + rName + 'TreeRoot) then'#13#10 + // 153
    '    ' + rName + 'TreeRoot := ' + rName + 'Tree.AddRoot(''' + rName +
    ''');'#13#10 +
  // 154
    '  F' + rName + 'Obj.TreeNodeShape := ' + rName + 'TreeRoot;'#13#10 + // 155
    '  F' + rName + 'Obj.ToTree;'#13#10 + // 156
    ''#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + '  ' + XMLClass[I].DataType + 'XMLInspector := ' + rName
      + 'Ins;'#13#10;
    Result := Result + '  ' + XMLClass[I].DataType + 'TreeComponent := ' + rName
      + 'Tree;'#13#10;
  end;
  Result := Result + 'end;'#13#10 + // 162
    ''#13#10 + // 163
    'procedure TFrame' + rName + '.FrameFinal;'#13#10 + // 73
    'begin'#13#10 + '    F' + rName + 'Obj.Free;'#13#10 + 'end;'#13#10#13#10;
  Result := Result + 'function TFrame' + rName + '.Get' + rName + 'Obj: ' + rType + ';'#13#10 +
  // 164
    'begin'#13#10 + // 165
    '  Result := F' + rName + 'Obj;'#13#10 + // 166
    'end;'#13#10 + // 167
    ''#13#10 + // 168
    'procedure TFrame' + rName + '.New' + rName + '(Sender: TObject);'#13#10 +
  // 169
    'begin'#13#10 + // 170
    '  F' + rName + 'Obj.TreeNodeShape := nil;'#13#10 + // 171
    '  F' + rName + 'Obj.Free;'#13#10 + // 172
    '  F' + rName + 'Obj := ' + rType + '.Create(nil);'#13#10 + // 173
    '  F' + rName + 'Obj.TreeNodeShape := ' + rName + 'TreeRoot;'#13#10 + // 174
    '  F' + rName + 'Obj.ToTree;'#13#10 + // 175
    'end;'#13#10 + // 176
    ''#13#10 + // 177
    'procedure TFrame' + rName + '.Open' + rName + '(Sender: TObject);'#13#10 +
  // 178
    'begin'#13#10 + // 179
    '  if OpenDialog' + rName + '.Execute then'#13#10 + // 180
    '  begin'#13#10 + // 181
    '    F' + rName + 'Obj.TreeNodeShape := nil;'#13#10 + // 182
    '    F' + rName + 'Obj.Free;'#13#10 + // 183
    '    F' + rName + 'Obj := ' + rType + '.Create(nil);'#13#10 + // 184
    '    F' + rName + 'Obj.Load(OpenDialog' + rName + '.FileName);'#13#10 +
  // 185
    '    F' + rName + 'Obj.TreeNodeShape := ' + rName + 'TreeRoot;'#13#10 +
  // 186
    '    F' + rName + 'Obj.ToTree;'#13#10 + // 187
    '  end;'#13#10 + // 188
    'end;'#13#10 + // 189
    ''#13#10 + // 190
    'procedure TFrame' + rName + '.Save' + rName + '(Sender: TObject);'#13#10 +
  // 191
    'begin'#13#10 + // 192
    '  if SaveDialog' + rName + '.Execute then'#13#10 + // 193
    '  begin'#13#10 + // 194
    '    F' + rName + 'Obj.Save(SaveDialog' + rName + '.FileName);'#13#10 +
  // 195
    '  end;'#13#10 + // 196
    'end;'#13#10 + // 197
    ''#13#10 + // 198
    'procedure TFrame' + rName + '.Set' + rName + 'Obj(const _Value: ' + rType +
    ');'#13#10 + // 199
    'begin'#13#10 + // 200
    '  if Not Assigned(_Value) then'#13#10 + // 201
    '    Exit;'#13#10 + // 202
    '  F' + rName + 'Obj.TreeNodeShape := nil;'#13#10 + // 203
    '  F' + rName + 'Obj.Free;'#13#10 + // 204
    '  F' + rName + 'Obj := _Value;'#13#10 + // 205
    'end;'#13#10 + // 206
    ''#13#10 + // 207
    'end.'#13#10 + // 208
    ''#13#10;
end;

function TXML2CodeHelper.Implements: string;
var
  I: Integer;
begin
  Result := 'implementation'#13#10#13#10;
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].Implements;
  end;
  Result := Result + #13#10;
end;

procedure TXML2CodeHelper.Load(const fileName: string);
var
  I, J: Integer;
begin
  inherited Load(fileName);
  for I := 0 to XMLClassCount - 1 do
  begin
    if XMLClass[I].Path = '' then
    begin
      XMLClass[I].AddPath;
      XMLClass[I].Path := XMLClass[I].Name;
    end;
    if not XMLClass[I].isAbstractExsit then
    begin
      XMLClass[I].AddisAbstract;
      XMLClass[I].isAbstract := False;
    end;
    for J := 0 to XMLClass[I].ChildCount - 1 do
    begin
      if not XMLClass[I].Child[J].isVirtualExsit then
      begin
        XMLClass[I].Child[J].AddisVirtual;
        XMLClass[I].Child[J].isVirtual := False;
      end;
    end;
  end;
end;

function TXML2CodeHelper.Statements: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to XMLClassCount - 1 do
  begin
    Result := Result + XMLClass[I].Statements + #13#10;
  end;
end;

procedure TXML2CodeHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.Text.Clear;
  TreeNodeShape.Text.Add('Code : ' + Name);
  if UsingExsit then
    TreeNodeShape.Text.Add('Using : ' + String1D2String(Using));
  for I := 0 to XMLClassCount - 1 do
  begin
    XMLClasss[I].TreeNodeShape := TreeNodeShape.AddChildObject('XMLClass',
      XMLClass[I]);
    XMLClass[I].ToTree;
  end;
end;

function TXML2CodeHelper.Types: string;
var
  I: Integer;
begin
  Result := 'type'#13#10#13#10;
end;

function TXML2CodeHelper.Use: string;
var
  I, count: Integer;
begin
  Result := 'uses'#13#10 +
    '  System.SysUtils, System.Types, System.UITypes, System.Classes,'#13#10 +
    '  System.Variants, FMX.Types, System.Generics.Collections,'#13#10 +
    '  XMLCore, XMLLeafTypes, '#13#10 +
    '  ClientScreen, FMXTee.Tree, FMX.Menus,'#13#10 + Name + 'Base';
  if Self.UsingExsit then
  begin
    count := Length(Self.Using);
    for I := 0 to count - 1 do
    begin
      Result := Result + ', ' + Self.Using[I];
    end;
  end;
  Result := Result + ';'#13#10#13#10;
end;

{ TXML2CodeClassHelper }
function TXML2CodeClassHelper.BaseConstructorImplements: string;
var
  I: Integer;
begin
  Result := 'constructor ' + DataType + '.Create(par: TXML = nil);'#13#10;
  Result := Result + 'begin'#13#10;
  Result := Result + '  inherited Create(par);'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          begin

          end;
        1:
          begin
          end;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's := TList<' +
              Child[I].LeafType + '>.Create;'#13#10;
          end;
      end;
    end
    else
    begin
      case Child[I].Number of
        0:
          begin

          end;
        1:
          begin
            Result := Result + '  F' + Child[I].Name + ' := ' +
              Child[I].DataType + '.Create(Self);'#13#10;
          end;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's := TList<' +
              Child[I].DataType + '>.Create;'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.BaseConstructorStatements: string;
begin
  Result := '    constructor Create(par: TXML = nil);'#13#10;
end;

function TXML2CodeClassHelper.BaseDestructorImplements: string;
var
  I: Integer;
begin
  Result := 'destructor ' + DataType + '.Destroy;'#13#10;
  Result := Result + 'begin'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          ;
        1:
          ;
        2:
          begin
            Result := Result + '  F' + Child[I].Name + 's.Free;'#13#10;
          end;
      end;
    end
    else
    begin
      case Child[I].Number of
        0:
          begin
            Result := Result + '  if F' + Child[I].Name + 'Exsit then'#13#10 +
              '    F' + Child[I].Name + '.Free;'#13#10;
          end;
        1:
          begin
            Result := Result + '  F' + Child[I].Name + '.Free;'#13#10;
          end;
        2:
          begin
            Result := Result + '  ' + Child[I].Name + 'Clear;'#13#10;
            Result := Result + '  F' + Child[I].Name + 's.Free;'#13#10;
          end;
      end;
    end;
  end;
  if VisualCode then
  begin
    Result := Result + '  if Assigned(FTreeNodeShape) then'#13#10 +
      '    FTreeNodeShape.Free;'#13#10;
  end;
  Result := Result + '  inherited;'#13#10;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.BaseDestructorStatements: string;
begin
  Result := '    destructor Destroy; override;'#13#10;
end;

function TXML2CodeClassHelper.BaseImplements: string;
var
  I: Integer;
begin
  Result := '{  ' + Name + '}'#13#10;
  Result := Result + Self.BaseConstructorImplements;
  Result := Result + Self.BaseDestructorImplements;
  Result := Result + Self.FromXMLImplement;
  Result := Result + Self.ToXMLImplement;
  Result := Result + Self.AppendToXMLImplement;
  if VisualCode then
  begin
    Result := Result + Self.ToTreeImplement;
    Result := Result + Self.OnClickImplement;
    Result := Result + Self.ToInspectorImplement;
    Result := Result + Self.SetXMLPropertyImplement;
    Result := Result + Self.DeleteSelfEventImplement;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafBaseImplements;
    end
    else
    begin
      Result := Result + Child[I].ChildBaseImplements;
    end;
  end;
end;

function TXML2CodeClassHelper.BasePrivateStatements: string;
var
  I: Integer;
begin
  Result := '  private'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafFieldStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildFieldStatement;
    end;
  end;
  if VisualCode then
    Result := Result + '    FTreeNodeShape: TTreeNodeShape;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafSetStatement;
      Result := Result + Child[I].LeafGetIndexStatement;
      Result := Result + Child[I].LeafSetIndexStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildSetStatement;
      Result := Result + Child[I].ChildGetIndexStatement;
      Result := Result + Child[I].ChildSetIndexStatement;
    end;
  end;
  if VisualCode then
    Result := Result + Self.DeleteSelfEventStatement;
end;

function TXML2CodeClassHelper.BaseProtectedStatements: string;
var
  I: Integer;
begin
  Result := '  protected'#13#10;
  Result := Result + Self.FromXMLStatement;
  Result := Result + Self.ToXMLStatement;
  Result := Result + Self.AppendToXMLStatement;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      if VisualCode then
      begin
        Result := Result + Child[I].LeafAddEventStatement;
      end;
    end
    else
    begin
      if VisualCode then
      begin
        Result := Result + Child[I].ChildAddEventStatement;
      end;
    end;
  end;
end;

function TXML2CodeClassHelper.BasePublicStatements: string;
var
  I: Integer;
begin
  Result := '  public'#13#10;
  Result := Result + Self.BaseConstructorStatements;
  Result := Result + Self.BaseDestructorStatements;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafPublicStatements;
    end
    else
    begin
      Result := Result + Child[I].ChildPublicStatements;
    end;
  end;
  if VisualCode then
  begin
    Result := Result + Self.ToTreeStatement;
    Result := Result + Self.OnClickStatement;
    Result := Result + Self.SetXMLPropertyStatement;
    Result := Result + Self.ToInspectorStatement;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafPropertyStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildPropertyStatement;
    end;
  end;
  if VisualCode then
  begin
    Result := Result + Self.TreeNodeShapeProperty;
    for I := 0 to ChildCount - 1 do
    begin
      if Child[I].Leaf then
      begin
        Result := Result + Child[I].LeafExsitProperty;
      end
      else
      begin
        Result := Result + Child[I].ChildExsitProperty;
      end;
    end;
  end;
end;

function TXML2CodeClassHelper.BaseStatements: string;
var
  absclass: string;
begin
  absclass := '= class(';
  if Self.isAbstract then
  begin
    absclass := '= class abstract(';
  end;
  if not Self.ParentClassExsit then
  begin
    Result := '  ' + DataType + absclass + 'TXML)'#13#10;
  end
  else
  begin
    if Self.ParentClass.IsEmpty then
    begin
      Result := '  ' + DataType + absclass + 'TXML)'#13#10;
    end
    else
    begin
      Result := '  ' + DataType + absclass + Self.ParentClass + ')'#13#10;
    end;
  end;
  Result := Result + Self.BasePrivateStatements;
  Result := Result + Self.BaseProtectedStatements;
  Result := Result + Self.BasePublicStatements;
  Result := Result + '  end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.BaseVars: string;
begin
  Result := '';
  if VisualCode then
  begin
    Result := Result + '  ' + DataType + 'Pop: TPopupMenu;'#13#10;
    Result := Result + '  ' + DataType + 'XMLInspector: TXMLInspector;'#13#10;
    Result := Result + '  ' + DataType + 'TreeComponent: TTree;'#13#10;
  end;
end;

function TXML2CodeClassHelper.DeleteSelfEventImplement: string;
begin

end;

function TXML2CodeClassHelper.DeleteSelfEventStatement: string;
begin

end;

function TXML2CodeClassHelper.DeleteSelfMenuImplement: string;
begin

end;

function TXML2CodeClassHelper.DeleteSelfMenuStatement: string;
begin

end;

function TXML2CodeClassHelper.FromXMLImplement: string;
var
  I, J: Integer;
  str: string;
begin
  Result := 'procedure ' + DataType + '.FromXML(node: IXMLNode);'#13#10 +
    'var'#13#10 + '  I: Integer;'#13#10 + '  nodeTmp: IXMLNode;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if not Child[I].Leaf then
    begin
      case Child[I].Number of
        0:
          ;
        1:
          ;
        2:
          begin
            Result := Result + '  ' + Child[I].Name + 'Tmp: ' +
              Child[I].DataType + ';'#13#10;
          end;
      end;
    end;
  end;
  Result := Result + 'begin'#13#10 + '  try'#13#10;
  if Self.ParentClassExsit then
    if not Self.ParentClass.IsEmpty then
      Result := Result + '    inherited FromXML(node);'#13#10;
  Result := Result + '    for I := 0 to node.ChildNodes.Count - 1 do'#13#10 +
    '    begin'#13#10 + '      nodeTmp := node.ChildNodes.Get(I);'#13#10;
  str := '';
  for I := 0 to ChildCount - 1 do
  begin
    str := str + Child[I].ReadChild;
  end;
  str := str.Remove(6, 5);
  str := str.Remove(str.Length - 2, 2);
  str := str + ';'#13#10;
  Result := Result + str;
  Result := Result + '    end;'#13#10 +
    '    for I := 0 to node.AttributeNodes.Count - 1 do'#13#10 +
    '    begin'#13#10 + '      nodeTmp := node.AttributeNodes.Get(I);'#13#10;
  str := '';
  for I := 0 to ChildCount - 1 do
  begin
    str := str + Child[I].ReadAttribute;
  end;
  str := str.Remove(6, 5);
  str := str.Remove(str.Length - 2, 2);
  str := str + ';'#13#10;
  Result := Result + str;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].ReadText;
  end;
  Result := Result + '    end;'#13#10 + '  except'#13#10 +
    '    raise Exception.Create(''' + Name +
    ' Read XML Error!'' + node.Xml);'#13#10 + '  end;'#13#10 +
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.FromXMLStatement: string;
begin
  Result := '    procedure FromXML(node: IXMLNode); override;'#13#10;
  if isAbstract then
    Result := '    procedure FromXML(node: IXMLNode); virtual;'#13#10;
end;

function TXML2CodeClassHelper.Implements: string;
var
  I: Integer;
begin
  Result := '{  ' + Name + '}'#13#10;
end;

function TXML2CodeClassHelper.OnClickImplement: string;
var
  I: Integer;
begin
  Result := 'procedure ' + DataType + '.OnClick(Sender: TTreeNodeShape;'#13#10 +
    '  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);'#13#10 +
    'var'#13#10 + '  pt: TPointF;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafAddEventMenuStatement;
    end
    else
    begin
      Result := Result + Child[I].ChildAddEventMenuStatement;
    end;
  end;
  Result := Result + 'begin'#13#10 + '  ToInspector;'#13#10 + '  ' +
    TXML2Code(Parent).Name + 'Object := Self;'#13#10 +
    '  if Button = TMouseButton.mbRight then'#13#10 + '  begin'#13#10 +
    '    if Assigned(' + DataType + 'Pop) and Assigned(' + DataType +
    'TreeComponent) then'#13#10 + '    begin'#13#10 + '      ' + DataType +
    'Pop.Clear;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafAddEventMenuImplement;
    end
    else
    begin
      Result := Result + Child[I].ChildAddEventMenuImplement;
    end;
  end;
  Result := Result + '      pt := TPointF.Create(X, Y);'#13#10 + '      pt := '
    + DataType + 'TreeComponent.ClientToScreen(pt);'#13#10 + '      ' + DataType
    + 'Pop.Popup(pt.X, pt.Y);'#13#10 + '    end;'#13#10 + '  end;'#13#10 +
    'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.OnClickStatement: string;
begin
  Result := '    procedure OnClick(Sender: TTreeNodeShape; Button: TMouseButton;'#13#10
    + '      Shift: TShiftState; X, Y: Integer); virtual;'#13#10;
end;

function TXML2CodeClassHelper.ParentX2C: TXML2Code;
begin
  Result := TXML2Code(Self.Parent);
end;

function TXML2CodeClassHelper.PrivateStatements: string;
begin
  Result := '  private'#13#10;
end;

function TXML2CodeClassHelper.ProtectedStatements: string;
begin
  Result := '  protected'#13#10;
end;

function TXML2CodeClassHelper.PublicStatements: string;
begin
  Result := '  public'#13#10;
end;

function TXML2CodeClassHelper.ToInspectorImplement: string;
var
  I: Integer;
begin
  Result := '';
  if VisualCode then
  begin
    Result := 'procedure ' + DataType + '.ToInspector;'#13#10 + 'var'#13#10 +
      '  Names_Value: TStringList;'#13#10 +
      '  Types_Value: TList<XMLTypes>;'#13#10 +
      '  _Values_Value: TStringList;'#13#10 + 'begin'#13#10 +
      '  if not Assigned(' + DataType + 'XMLInspector) then'#13#10 +
      '    Exit;'#13#10 + '  Names_Value := TStringList.Create;'#13#10 +
      '  Types_Value := TList<XMLTypes>.Create;'#13#10 +
      '  _Values_Value := TStringList.Create;'#13#10;
    for I := 0 to ChildCount - 1 do
    begin
      if Child[I].Leaf then
      begin
        Result := Result + Child[I].LeafToInspectorImplement;
      end;
    end;
    Result := Result + '  ' + DataType +
      'XMLInspector.SetData(Names_Value, _Values_Value, Types_Value, Self);'#13#10
      + 'end;'#13#10#13#10;
  end;
end;

function TXML2CodeClassHelper.ToInspectorStatement: string;
begin
  Result := '';
  if VisualCode then
    Result := '    procedure ToInspector;'#13#10;
end;

function TXML2CodeClassHelper.SetXMLPropertyImplement: string;
var
  Index, I: Integer;
begin
  Result := '';
  if VisualCode then
  begin
    Result := Result + 'procedure ' + DataType +
      '.SetXMLProperty(Index: Integer; _Value: String);'#13#10 + 'begin'#13#10 +
      '  case index of'#13#10;
    Index := 0;
    for I := 0 to ChildCount - 1 do
    begin
      if Child[I].Leaf then
      begin
        Result := Result + TXML2CodeChild(Child[I])
          .LeafSetXMLPropertyImplement(Index);
      end;
    end;
    Result := Result + '  end;'#13#10 + '  ToTree;'#13#10 + 'end;'#13#10#13#10;
    if Index = 0 then
    begin
      Result := 'procedure ' + DataType +
        '.SetXMLProperty(Index: Integer; _Value: String);'#13#10 + 'begin'#13#10
        + 'end;'#13#10#13#10;
    end;
  end;
end;

function TXML2CodeClassHelper.SetXMLPropertyStatement: string;
begin
  Result := '';
  if VisualCode then
    Result := '    procedure SetXMLProperty(Index: Integer; _Value: String); override;'#13#10;
end;

function TXML2CodeClassHelper.Statements: string;
begin
  Result := '  ' + DataType + 'Helper = class helper for ' + DataType + #13#10;
  Result := Result + Self.PrivateStatements;
  Result := Result + Self.ProtectedStatements;
  Result := Result + Self.PublicStatements;
  Result := Result + '  end;'#13#10;
end;

procedure TXML2CodeClassHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;
  TreeNodeShape.Text.Clear;;
  TreeNodeShape.Text.Add('Name : ' + Name);
  TreeNodeShape.Text.Add('DataType : ' + DataType);
  TreeNodeShape.Text.Add('Root : ' + Root.ToString);
  if ParentClassExsit then
    TreeNodeShape.Text.Add('ParentClass : ' + ParentClass);
  if ChildClassExsit then
    TreeNodeShape.Text.Add('ChildClass : ' + String1D2String(ChildClass));
  for I := 0 to ChildCount - 1 do
  begin
    Childs[I].TreeNodeShape := TreeNodeShape.AddChildObject('Child', Child[I]);
    Child[I].ToTree;
  end;
end;

function TXML2CodeClassHelper.ToTreeImplement: string;
var
  I: Integer;
begin
  Result := 'procedure ' + DataType + '.ToTree;'#13#10 + // 0
    'var'#13#10 + // 1
    '  I: Integer;'#13#10 + // 2
    'begin'#13#10 + // 3
    '  if not Assigned(TreeNodeShape) then'#13#10 + // 4
    '  begin'#13#10 + // 5
    '    Exit;'#13#10 + // 6
    '  end;'#13#10 + // 7
    '  TreeNodeShape.Clear;'#13#10 + // 8
  // '  TreeNodeShape.Text.Clear;'#13#10 + // 9
  // '  TreeNodeShape.Text.Add(''' + Name + ''');'#13#10 + // 10
    '  TreeNodeShape.OnClick := OnClick;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + Child[I].LeafToTree;
    end
    else
    begin
      Result := Result + Child[I].ChildToTree;
    end;
  end;
  Result := Result + 'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.ToTreeStatement: string;
begin
  Result := '    procedure ToTree; virtual;'#13#10;
end;

function TXML2CodeClassHelper.ToXMLImplement: string;
var
  I, J: Integer;
begin
  Result := 'function ' + DataType +
    '.ToXML(par: IXMLNode; pt: string): IXMLNode;'#13#10 + 'var'#13#10 +
    '  doc: IXMLDocument;'#13#10 + '  node: IXMLNode;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + '  ' + Child[I].Name + 'Tmp: IXMLNode;'#13#10;
    end;
  end;
  Result := Result + '  I: Integer;'#13#10 + 'begin'#13#10 + '  try'#13#10 +
    '    doc := par.OwnerDocument;'#13#10;
  Result := Result + '  if (pt = '''') or (pt[1] = ''#'') then'#13#10 +
    '    pt := ''' + Path + ''';'#13#10;
  Result := Result + '    node := doc.CreateNode(pt);'#13#10 +
    '    par.ChildNodes.Add(node);'#13#10;
  if Self.ParentClassExsit then
    if not Self.ParentClass.IsEmpty then
      Result := Result + '    inherited AppendToXML(node);'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteChild;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteAttribute;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteText;
  end;
  Result := Result + '    Result := node;'#13#10 + '  except'#13#10 +
    '    raise Exception.Create(''XML2Code Write XML Error!'');'#13#10 +
    '  end;'#13#10 + 'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.AppendToXMLImplement: string;
var
  I, J: Integer;
begin
  Result := '';
  if not ChildClassExsit then
    Exit;
  if Length(ChildClass) < 0 then
    Exit;
  Result := 'function ' + DataType +
    '.AppendToXML(node: IXMLNode; pt: string): IXMLNode;'#13#10 + 'var'#13#10 +
    '  doc: IXMLDocument;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    if Child[I].Leaf then
    begin
      Result := Result + '  ' + Child[I].Name + 'Tmp: IXMLNode;'#13#10;
    end;
  end;
  Result := Result + '  I: Integer;'#13#10 + 'begin'#13#10 + '  try'#13#10 +
    '    doc := node.OwnerDocument;'#13#10;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteChild;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteAttribute;
  end;
  for I := 0 to ChildCount - 1 do
  begin
    Result := Result + Child[I].WriteText;
  end;
  Result := Result + '    Result := node;'#13#10 + '  except'#13#10 +
    '    raise Exception.Create(''XML2Code Write XML Error!'');'#13#10 +
    '  end;'#13#10 + 'end;'#13#10#13#10;
end;

function TXML2CodeClassHelper.ToXMLStatement: string;
begin
  Result := '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; override;'#13#10;
  if isAbstract then
    Result := '    function ToXML(par: IXMLNode; pt: string = ''''): IXMLNode; virtual;'#13#10;
end;

function TXML2CodeClassHelper.AppendToXMLStatement: string;
begin
  Result := '';
  if ChildClassExsit then
    if Length(ChildClass) > 0 then
      Result := '    function AppendToXML(node: IXMLNode; pt: string = ''''): IXMLNode;'#13#10;
end;

function TXML2CodeClassHelper.TreeNodeShapeProperty: string;
begin
  Result := '    property TreeNodeShape: TTreeNodeShape read FTreeNodeShape'#13#10
    + '      write FTreeNodeShape;'#13#10;
end;

function TXML2CodeChildHelper.ChildAddEventImplement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + 'procedure ' + ParentClass.DataType + '.Add' +
              childclasses[I].Name + 'Event(Sender: TObject);'#13#10 +
              'begin'#13#10 + '  Add' + childclasses[I].Name + ';'#13#10 + '  F'
              + Name + '.ToTree;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name +
            ';'#13#10 + '  F' + Name + '.ToTree;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + 'procedure ' + ParentClass.DataType + '.Add' +
              childclasses[I].Name + 'Event(Sender: TObject);'#13#10 +
              'var'#13#10 + '  tmp: ' + childclasses[I].DataType + ';'#13#10 +
              'begin'#13#10 + '  tmp := Add' + childclasses[I].Name + ';'#13#10
              + '  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject(''' +
              childclasses[I].Name + ''', tmp);'#13#10 + '  tmp.ToTree;'#13#10 +
              'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
            'Event(Sender: TObject);'#13#10 + 'var'#13#10 + '  tmp: ' + DataType
            + ';'#13#10 + 'begin'#13#10 + '  tmp := Add' + Name + ';'#13#10 +
            '  tmp.TreeNodeShape := FTreeNodeShape.AddChildObject(''' + Name +
            ''', tmp);'#13#10 + '  tmp.ToTree;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '    procedure Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure Add' + Name +
            'Event(Sender: TObject);'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '    procedure Add' + childclasses[I].Name +
              'Event(Sender: TObject);'#13#10;
          end;
        end
        else
        begin
          Result := '    procedure Add' + Name +
            'Event(Sender: TObject);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventMenuImplement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
            ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
            'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' +
            ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '      ' + childclasses[I].Name +
              'AddMenu := TMenuItem.Create(' + Name + 'AddMenu);'#13#10 +
              '      ' + childclasses[I].Name + 'AddMenu.Text := ''' +
              childclasses[I].Name + ''';'#13#10 + '      ' + childclasses[I]
              .Name + 'AddMenu.OnClick := Add' + childclasses[I].Name +
              'Event;'#13#10 + '      ' + Name + 'AddMenu.AddObject(' +
              childclasses[I].Name + 'AddMenu);'#13#10;
          end;
        end
        else
        begin
          Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
            ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
            'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' + Name +
            'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + '      ' +
            ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
            ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
            'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' +
            ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '      ' + childclasses[I].Name +
              'AddMenu := TMenuItem.Create(' + Name + 'AddMenu);'#13#10 +
              '      ' + childclasses[I].Name + 'AddMenu.Text := ''' +
              childclasses[I].Name + ''';'#13#10 + '      ' + childclasses[I]
              .Name + 'AddMenu.OnClick := Add' + childclasses[I].Name +
              'Event;'#13#10 + '      ' + Name + 'AddMenu.AddObject(' +
              childclasses[I].Name + 'AddMenu);'#13#10;

          end;
        end
        else
        begin
          Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
            ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
            'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' + Name +
            'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + '      ' +
            ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddEventMenuStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
          childclasses := Self.ChildClass;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '  ' + childclasses[I].Name +
              'AddMenu: TMenuItem;'#13#10;
          end;
        end
        else
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
          for I := 0 to childclasses.count - 1 do
          begin
            Result := Result + '  ' + childclasses[I].Name +
              'AddMenu: TMenuItem;'#13#10;
          end;
        end
        else
        begin
          Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddImplement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + 'function ' + ParentClass.DataType + '.Add' +
              childclasses[I].Name + ': ' + childclasses[I].DataType + ';'#13#10
              + 'begin;'#13#10 + '  if not F' + Name + 'Exsit then'#13#10 +
              '  F' + Name + '.Free;'#13#10 + '  F' + Name + ' := ' +
              childclasses[I].DataType + '.Create(Self);'#13#10 + '  Result := '
              + childclasses[I].DataType + '(F' + Name + ');'#13#10 + '  F' +
              Name + 'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' +
            DataType + ';'#13#10 + 'begin;'#13#10 + '  if not F' + Name +
            'Exsit then'#13#10 + '    F' + Name + ' := ' + DataType +
            '.Create(Self);'#13#10 + '  Result := F' + Name + ';'#13#10 + '  F'
            + Name + 'Exsit := True;'#13#10 + 'end;'#13#10#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + 'function ' + ParentClass.DataType + '.Add' +
              childclasses[I].Name + ': ' + childclasses[I].DataType + ';'#13#10
              + 'var'#13#10 + '  ' + childclasses[I].Name + 'tmp: ' +
              childclasses[I].DataType + ';'#13#10 + 'begin;'#13#10 + '  ' +
              childclasses[I].Name + 'tmp := ' + childclasses[I].DataType +
              '.Create(Self);'#13#10 + '  F' + Name + 's.Add(' + childclasses[I]
              .Name + 'tmp);'#13#10 + '  Result := ' + childclasses[I].Name +
              'tmp;'#13#10 + 'end;'#13#10#13#10;
          end;
        end
        else
        begin
          Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' +
            DataType + ';'#13#10 + 'var'#13#10 + '  ' + Name + 'tmp: ' +
            DataType + ';'#13#10 + 'begin;'#13#10 + '  ' + Name + 'tmp := ' +
            DataType + '.Create(Self);'#13#10 + '  F' + Name + 's.Add(' + Name +
            'tmp);'#13#10 + '  Result := ' + Name + 'tmp;'#13#10 +
            'end;'#13#10#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildAddStatement: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case Number of
    0:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + '    function Add' + childclasses[I].Name + ': '
              + childclasses[I].DataType + ';'#13#10;
          end;
        end
        else
        begin
          Result := '    function Add' + Name + ': ' + DataType + ';'#13#10;
        end;
      end;
    1:
      ;
    2:
      begin
        if isVirtual then
        begin
          childclasses := Self.ChildClass;
          for I := 0 to ChildClass.count - 1 do
          begin
            Result := Result + '    function Add' + childclasses[I].Name + ': '
              + childclasses[I].DataType + ';'#13#10;
          end;
        end
        else
        begin
          Result := '    function Add' + Name + ': ' + DataType + ';'#13#10;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildBaseImplements: string;
begin
  Result := '';
  Result := Result + ChildAddImplement;
  Result := Result + ChildSetImplement;
  Result := Result + ChildClearImplement;
  Result := Result + ChildCountImplement;
  Result := Result + ChildGetIndexImplement;
  Result := Result + ChildSetIndexImplement;
  Result := Result + ChildRemoveImplement;
  Result := Result + ChildDeleteImplement;
  if VisualCode then
  begin
    Result := Result + ChildAddEventImplement;
  end;
end;

function TXML2CodeChildHelper.ChildClass: TList<TXML2CodeClass>;
var
  x2c: TXML2Code;
  x2cclass, tmp: TXML2CodeClass;
  I: Integer;
begin
  x2c := Self.ParentClass.ParentX2C;
  x2cclass := x2c.ClassOf(DataType);
  if x2cclass = nil then
  begin
    Result := nil;
    Exit;
  end;
  if not x2cclass.ChildClassExsit then
  begin
    Result := nil;
    Exit;
  end;
  Result := TList<TXML2CodeClass>.Create;
  for I := 0 to Length(x2cclass.ChildClass) - 1 do
  begin
    tmp := x2c.ClassOf(x2cclass.ChildClass[I]);
    if tmp = nil then
      Continue;
    Result.Add(tmp);
  end;
  if Result.count = 0 then
  begin
    Result.Free;
    Result := nil;
  end;
end;

function TXML2CodeChildHelper.ChildClearImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name +
          'Clear;'#13#10 + 'begin'#13#10 + '  while F' + Name +
          's.Count > 0 do'#13#10 + '  begin'#13#10 + '    F' + Name +
          's.Items[0].Free;'#13#10 + '    F' + Name + 's.Delete(0);'#13#10 +
          '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildClearStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure ' + Name + 'Clear;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildCountImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.' + Name +
          'Count: Integer;'#13#10 + 'begin'#13#10 + '  Result := F' + Name +
          's.Count;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildCountStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function ' + Name + 'Count: Integer;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildDeleteImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Delete' + Name +
          '(Index: Integer);'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Items[Index].Free;'#13#10 + '  F' + Name + 's.Delete(Index);'#13#10
          + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildDeleteStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Delete' + Name + '(Index: Integer);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name +
          'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildHelper.ChildFieldStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    F' + Name + ': ' + DataType + ';'#13#10 + '    F' + Name
          + 'Exsit: Boolean;'#13#10;
      end;
    1:
      begin
        Result := '    F' + Name + ': ' + DataType + ';'#13#10;
      end;
    2:
      begin
        Result := '    F' + Name + 's: TList<' + DataType + '>;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildGetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Get' + Name +
          '(Index: Integer): ' + DataType + ';'#13#10 + 'begin'#13#10 +
          '  Result := F' + Name + 's[Index];'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildGetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function Get' + Name + '(Index: Integer): ' + DataType +
          ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildIndexPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    property ' + Name + '[Index: Integer]: ' + DataType +
          ' read Get' + Name + ' write Set' + Name + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildFieldStatement;
  Result := Result + Self.ChildSetStatement;
  Result := Result + Self.ChildGetIndexStatement;
  Result := Result + Self.ChildSetIndexStatement;
end;

function TXML2CodeChildHelper.ChildPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + ': ' + DataType + ' read F' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
    1:
      begin
        Result := '    property ' + Name + ': ' + DataType + ' read F' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
    2:
      begin
        Result := '    property ' + Name + 's: TList<' + DataType + '> read F' +
          Name + 's write Set' + Name + 's;'#13#10;
        Result := Result + Self.ChildIndexPropertyStatement;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildHelper.ChildPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.ChildAddStatement;
  Result := Result + Self.ChildClearStatement;
  Result := Result + Self.ChildCountStatement;
  Result := Result + Self.ChildRemoveStatement;
  Result := Result + Self.ChildDeleteStatement;
end;

function TXML2CodeChildHelper.ChildRemoveImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name +
          'Remove;'#13#10 + 'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10
          + '  begin'#13#10 + '    F' + Name + '.Free;'#13#10 + '    F' + Name +
          'Exsit := False;'#13#10 + '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Remove' + Name +
          '(_Value: ' + DataType + ');'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Remove(_Value);'#13#10 + '  _Value.Free;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildRemoveStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure ' + Name + 'Remove;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Remove' + Name + '(_Value: ' + DataType +
          ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(const _Value: ' + DataType + ');'#13#10 + 'begin'#13#10 + '  if F' +
          Name + 'Exsit then'#13#10 + '    F' + Name + '.Free;'#13#10 + '  F' +
          Name + 'Exsit := True;'#13#10 + '  F' + Name + ' := _Value;'#13#10 +
          '  F' + Name + '.Parent := Self;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(const _Value: ' + DataType + ');'#13#10 + 'begin'#13#10 + '  F' +
          Name + '.Free;'#13#10 + '  F' + Name + ' := _Value;'#13#10 + '  F' +
          Name + '.Parent := Self;'#13#10 + 'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          's(const _Value: TList<' + DataType + '>);'#13#10 + 'begin'#13#10 +
          '  ' + Name + 'Clear;'#13#10 + '  F' + Name + 's := _Value;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + DataType +
          ');'#13#10;
      end;
    1:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + DataType +
          ');'#13#10;
      end;
    2:
      begin
        Result := '    procedure Set' + Name + 's(const _Value: TList<' +
          DataType + '>);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildToTree: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  if ' + Name + 'Exsit then'#13#10 + '  begin'#13#10 + '    '
          + Name + '.TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name +
          ''', ' + Name + ');'#13#10 + '    ' + Name + '.ToTree;'#13#10 +
          '  end;'#13#10;
      end;
    1:
      begin
        Result := '  ' + Name +
          '.TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name + ''', ' +
          Name + ');'#13#10 + '  ' + Name + '.ToTree;'#13#10;
      end;
    2:
      begin
        Result := '  for I := 0 to ' + Name + 'Count - 1 do'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    ' + Name +
          's[I].TreeNodeShape := TreeNodeShape.AddChildObject(''' + Name +
          ''', ' + Name + '[I]);'#13#10 + '    ' + Name + '[I].ToTree;'#13#10 +
          '  end;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(Index: Integer;'#13#10 + '  const _Value: ' + DataType + ');'#13#10
          + 'begin'#13#10 + '  _Value.Parent := Self;'#13#10 + '  F' + Name +
          's[Index].Free;'#13#10 + '  F' + Name + 's[Index] := _Value;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ChildSetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Set' + Name + '(Index: Integer; const _Value: '
          + DataType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.ConvertStr: string;
begin
  Result := '';
  case Number of
    0:
      Result := XMLConvertStr(String2XMLType(DataType), 'F' + Name);
    1:
      Result := XMLConvertStr(String2XMLType(DataType), 'F' + Name);
    2:
      Result := XMLConvertStr(String2XMLType(DataType),
        'F' + Name + 's.Items[I]');
  end;
end;

function TXML2CodeChildHelper.ConvertXML
  (_Value: string = 'nodeTmp.Text'): string;
begin
  Result := StrConvertXML(String2XMLType(DataType), _Value);
end;

function TXML2CodeChildHelper.LeafAddEventImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
          'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name +
          ';'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Add' + Name +
          'Event(Sender: TObject);'#13#10 + 'begin'#13#10 + '  Add' + Name +
          ';'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddEventStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Add' + Name + 'Event(Sender: TObject);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddEventMenuImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
          ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
          'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' + Name +
          'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + '      ' +
          ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '      ' + Name + 'AddMenu := TMenuItem.Create(' +
          ParentClass.DataType + 'Pop);'#13#10 + '      ' + Name +
          'AddMenu.Text := ''Add ' + Name + ''';'#13#10 + '      ' + Name +
          'AddMenu.OnClick := Add' + Name + 'Event;'#13#10 + '      ' +
          ParentClass.DataType + 'Pop.AddObject(' + Name + 'AddMenu);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddEventMenuStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '  ' + Name + 'AddMenu: TMenuItem;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' +
          LeafType + ';'#13#10 + 'begin;'#13#10 + '  Result := F' + Name +
          ';'#13#10 + '  F' + Name + 'Exsit := True;'#13#10 +
          'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Add' + Name + ': ' +
          LeafType + ';'#13#10 + 'var'#13#10 + Name + 'tmp: ' + LeafType +
          ';'#13#10 + 'begin;'#13#10 + '  F' + Name + 's.Add(' + Name +
          'tmp);'#13#10 + '  Result := ' + Name + 'tmp;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafAddStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    function Add' + Name + ': ' + LeafType + ';'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    function Add' + Name + ': ' + LeafType + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafBaseImplements: string;
begin
  Result := '';
  Result := Result + LeafAddImplement;
  Result := Result + LeafSetImplement;
  Result := Result + LeafClearImplement;
  Result := Result + LeafCountImplement;
  Result := Result + LeafGetIndexImplement;
  Result := Result + LeafSetIndexImplement;
  Result := Result + LeafRemoveImplement;
  Result := Result + LeafDeleteImplement;
  if VisualCode then
  begin
    Result := Result + LeafAddEventImplement;
  end;
end;

function TXML2CodeChildHelper.LeafClearImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name +
          'Clear;'#13#10 + 'begin'#13#10 + '  F' + Name + 's.Clear;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafClearStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure ' + Name + 'Clear;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafCountImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.' + Name +
          'Count: Integer;'#13#10 + 'begin'#13#10 + '  Result := F' + Name +
          's.Count;'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafCountStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function ' + Name + 'Count: Integer;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafDeleteImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Delete' + Name +
          '(Index: Integer);'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Delete(Index);'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafDeleteStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Delete' + Name + '(Index: Integer);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafExsitProperty: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + 'Exsit: Boolean read F' + Name +
          'Exsit;'#13#10;
      end;
    1:
      ;
    2:
      ;
  end;
end;

function TXML2CodeChildHelper.LeafFieldStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    F' + Name + ': ' + LeafType + ';'#13#10 + '    F' + Name
          + 'Exsit: Boolean;'#13#10;
      end;
    1:
      begin
        Result := '    F' + Name + ': ' + LeafType + ';'#13#10;
      end;
    2:
      begin
        Result := '    F' + Name + 's: TList<' + LeafType + '>;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafGetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'function ' + ParentClass.DataType + '.Get' + Name +
          '(Index: Integer): ' + LeafType + ';'#13#10 + 'begin'#13#10 +
          '  Result := F' + Name + 's[Index];'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafGetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    function Get' + Name + '(Index: Integer): ' + LeafType +
          ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafIndexPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    property ' + Name + '[Index: Integer]: ' + LeafType +
          ' read Get' + Name + ' write Set' + Name + ';'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafPrivateStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafFieldStatement;
  Result := Result + Self.LeafSetStatement;
  Result := Result + Self.LeafGetIndexStatement;
  Result := Result + Self.LeafSetIndexStatement;
end;

function TXML2CodeChildHelper.LeafPropertyStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    property ' + Name + ': ' + LeafType + ' read F' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
    1:
      begin
        Result := '    property ' + Name + ': ' + LeafType + ' read F' + Name +
          ' write Set' + Name + ';'#13#10;
      end;
    2:
      begin
        Result := '    property ' + Name + 's: TList<' + LeafType + '> read F' +
          Name + 's write Set' + Name + 's;'#13#10;
        Result := Result + Self.LeafIndexPropertyStatement;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafProtectedStatements: string;
begin
  Result := '';
end;

function TXML2CodeChildHelper.LeafPublicStatements: string;
begin
  Result := '';
  Result := Result + Self.LeafAddStatement;
  Result := Result + Self.LeafClearStatement;
  Result := Result + Self.LeafCountStatement;
  Result := Result + Self.LeafRemoveStatement;
  Result := Result + Self.LeafDeleteStatement;
end;

function TXML2CodeChildHelper.LeafRemoveImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.' + Name +
          'Remove;'#13#10 + 'begin'#13#10 + '  if F' + Name + 'Exsit then'#13#10
          + '  begin'#13#10 + '    F' + Name + 'Exsit := False;'#13#10 +
          '  end;'#13#10 + 'end;'#13#10#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Remove' + Name +
          '(_Value: ' + LeafType + ');'#13#10 + 'begin'#13#10 + '  F' + Name +
          's.Remove(_Value);'#13#10 + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafRemoveStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure ' + Name + 'Remove;'#13#10;
      end;
    1:
      ;
    2:
      begin
        Result := '    procedure Remove' + Name + '(_Value: ' + LeafType +
          ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetImplement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(const _Value: ' + LeafType + ');'#13#10 + 'begin'#13#10 + '  F' +
          Name + 'Exsit := True;'#13#10 + '  F' + Name + ' := _Value;'#13#10 +
          'end;'#13#10#13#10;
      end;
    1:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(const _Value: ' + LeafType + ');'#13#10 + 'begin'#13#10 + '  F' +
          Name + ' := _Value;'#13#10 + 'end;'#13#10#13#10;
      end;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          's(const _Value: TList<' + LeafType + '>);'#13#10 + 'begin'#13#10 +
          '  F' + Name + 's.Clear;'#13#10 + '  F' + Name + 's := _Value;'#13#10
          + 'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetStatement: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + LeafType +
          ');'#13#10;
      end;
    1:
      begin
        Result := '    procedure Set' + Name + '(const _Value: ' + LeafType +
          ');'#13#10;
      end;
    2:
      begin
        Result := '    procedure Set' + Name + 's(const _Value: TList<' +
          LeafType + '>);'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetXMLPropertyImplement
  (var Index: Integer): string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '    ' + Index.ToString + ':'#13#10 +
            '      begin'#13#10 + '        ' + Name + ' := ' +
            StrConvertXML(String2XMLType(DataType), '_Value') + ';'#13#10 +
            '      end;'#13#10;
          Inc(Index);
        end;
      1:
        begin
          Result := Result + '    ' + Index.ToString + ':'#13#10 +
            '      begin'#13#10 + '        ' + Name + ' := ' +
            StrConvertXML(String2XMLType(DataType), '_Value') + ';'#13#10 +
            '      end;'#13#10;
          Inc(Index);
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildHelper.LeafSetIndexImplement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := 'procedure ' + ParentClass.DataType + '.Set' + Name +
          '(Index: Integer;'#13#10 + '  const _Value: ' + LeafType + ');'#13#10
          + 'begin'#13#10 + '  F' + Name + 's[Index] := _Value;'#13#10 +
          'end;'#13#10#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafSetIndexStatement: string;
begin
  Result := '';
  case Number of
    0:
      ;
    1:
      ;
    2:
      begin
        Result := '    procedure Set' + Name + '(Index: Integer; const _Value: '
          + LeafType + ');'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafToInspectorImplement: string;
begin
  Result := '';
  if Visual then
  begin
    case Number of
      0:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 +
            '  Types_Value.Add(' + XMLTypeStrings[String2XMLType(DataType)] +
            ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), Name) + ');'#13#10;
        end;
      1:
        begin
          Result := Result + '  Names_Value.Add(''' + Name + ''');'#13#10 +
            '  Types_Value.Add(' + XMLTypeStrings[String2XMLType(DataType)] +
            ');'#13#10 + '  _Values_Value.Add(' +
            XMLConvertStr(String2XMLType(DataType), Name) + ');'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

function TXML2CodeChildHelper.LeafToTree: string;
begin
  Result := '';
  case Number of
    0:
      begin
        Result := '  if ' + Name + 'Exsit then'#13#10 +
          '    TreeNodeShape.AddChild(''' + Name + ''');'#13#10;
      end;
    1:
      begin
        Result := '  TreeNodeShape.AddChild(''' + Name + ''');'#13#10;
      end;
    2:
      begin
        Result := '  for I := 0 to ' + Name + 'Count - 1 do'#13#10 + // 0
          '  begin'#13#10 + // 1
          '    TreeNodeShape.AddChild(''' + Name + ''');'#13#10 +
          '  end;'#13#10;
      end;
  end;
end;

function TXML2CodeChildHelper.LeafType: string;
begin
  Result := XMLType2PascalType(String2XMLType(DataType));
end;

function TXML2CodeChildHelper.NodeType: TNodeType;
begin
  if Path = '' then
  begin
    Result := ntText;
  end
  else if Path[1] = '~' then
  begin
    Result := ntCData;
  end
  else if Path[1] = '@' then
  begin
    Result := ntAttribute;
  end
  else
  begin
    Result := ntElement;
  end;
end;

function TXML2CodeChildHelper.ParentClass: TXML2CodeClass;
begin
  Result := TXML2CodeClass(Self.Parent);
end;

function TXML2CodeChildHelper.ReadAttribute: string;
begin
  Result := '';
  if NodeType = ntAttribute then
  begin
    case Number of
      0:
        Result := '      else if nodeTmp.NodeName = ''' + Path.Remove(0, 1) +
          ''' then'#13#10 + '      begin'#13#10 + '        F' + Name + ' := ' +
          Self.ConvertXML + ';'#13#10 + '        F' + Name +
          'Exsit := True;'#13#10 + '      end'#13#10;
      1:
        Result := '      else if nodeTmp.NodeName = ''' + Path.Remove(0, 1) +
          ''' then'#13#10 + '      begin'#13#10 + '        F' + Name + ' := ' +
          Self.ConvertXML + ';'#13#10 + '      end'#13#10;
      2:
        ;
    end;
  end;
end;

function TXML2CodeChildHelper.ReadChild: string;
var
  childclasses: TList<TXML2CodeClass>;
  I: Integer;
begin
  Result := '';
  case NodeType of
    ntElement:
      begin
        if Leaf then
        begin
          case Number of
            0:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path +
                  ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                  ' := ' + Self.ConvertXML + ';'#13#10 + '        F' + Name +
                  'Exsit := True;'#13#10 + '      end'#13#10;
              end;
            1:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path +
                  ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                  ' := ' + Self.ConvertXML + ';'#13#10 + '      end'#13#10;
              end;
            2:
              begin
                Result := '      else if nodeTmp.NodeName = ''' + Path +
                  ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                  's.Add(' + Self.ConvertXML + ');'#13#10 + '      end'#13#10;
              end;
          end;
        end
        else
        begin
          case Number of
            0:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + '      else if nodeTmp.NodeName = ''' +
                      childclasses[I].Path + ''' then'#13#10 +
                      '      begin'#13#10 + '        F' + Name + ' := ' +
                      childclasses[I].DataType + '.Create(Self);'#13#10 +
                      '        F' + Name + '.FromXML(nodeTmp);'#13#10 +
                      '        F' + Name + 'Exsit := True;'#13#10 +
                      '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path +
                    ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                    ' := ' + DataType + '.Create(Self);'#13#10 + '        F' +
                    Name + '.FromXML(nodeTmp);'#13#10 + '        F' + Name +
                    'Exsit := True;'#13#10 + '      end'#13#10;
                end;
              end;

            1:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + '      else if nodeTmp.NodeName = ''' +
                      childclasses[I].Path + ''' then'#13#10 +
                      '      begin'#13#10 + '        F' + Name + ' := ' +
                      childclasses[I].DataType + '.Create(Self);'#13#10 +
                      '        F' + Name + '.FromXML(nodeTmp);'#13#10 +
                      '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path +
                    ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                    ' := ' + DataType + '.Create(Self);'#13#10 + '        F' +
                    Name + '.FromXML(nodeTmp);'#13#10 + '      end'#13#10;
                end;
              end;
            2:
              begin
                if isVirtual then
                begin
                  childclasses := Self.ChildClass;
                  for I := 0 to childclasses.count - 1 do
                  begin
                    Result := Result + '      else if nodeTmp.NodeName = ''' +
                      childclasses[I].Path + ''' then'#13#10 +
                      '      begin'#13#10 + '        ' + Name + 'Tmp := ' +
                      childclasses[I].DataType + '.Create(Self);'#13#10 +
                      '        ' + Name + 'Tmp.FromXML(nodeTmp);'#13#10 +
                      '        F' + Name + 's.Add(' + Name + 'Tmp);'#13#10 +
                      '      end'#13#10;
                  end;
                  childclasses.Free;
                end
                else
                begin
                  Result := '      else if nodeTmp.NodeName = ''' + Path +
                    ''' then'#13#10 + '      begin'#13#10 + '        ' + Name +
                    'Tmp := ' + DataType + '.Create(Self);'#13#10 + '        ' +
                    Name + 'Tmp.FromXML(nodeTmp);'#13#10 + '        F' + Name +
                    's.Add(' + Name + 'Tmp);'#13#10 + '      end'#13#10;
                end;
              end;
          end;
        end;
      end;
    ntCData:
      begin
        case Number of
          0:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path +
                ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                ' := ' + Self.ConvertXML + ';'#13#10 + '      end'#13#10;
            end;
          1:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path +
                ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                ' := ' + Self.ConvertXML + ';'#13#10 + '      end'#13#10;
            end;
          2:
            begin
              Result := '      else if nodeTmp.NodeName = ''' + Path +
                ''' then'#13#10 + '      begin'#13#10 + '        F' + Name +
                's.Add(' + ConvertXML + ');'#13#10 + '      end'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.ReadText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    F' + Name + ' := ' + Self.ConvertXML('node.Text') +
            ';'#13#10;
        end;
      1:
        begin
          Result := '    F' + Name + ' := ' + Self.ConvertXML('node.Text') +
            ';'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

procedure TXML2CodeChildHelper.ToTree;
var
  I: Integer;
begin
  if not Assigned(TreeNodeShape) then
  begin
    Exit;
  end;
  TreeNodeShape.Clear;
  TreeNodeShape.OnClick := OnClick;

  TreeNodeShape.Text.Clear;;
  TreeNodeShape.Text.Add('Name : ' + Name);
  TreeNodeShape.Text.Add('DataType : ' + DataType);
  TreeNodeShape.Text.Add('Path' + Path);
  TreeNodeShape.Text.Add('Number' + Number.ToString);
  TreeNodeShape.Text.Add('Leaf' + Leaf.ToString);
  TreeNodeShape.Text.Add('Visual' + Visual.ToString);
end;

function TXML2CodeChildHelper.WriteAttribute: string;
begin
  Result := '';
  if NodeType = ntAttribute then
  begin
    case Number of
      0:
        Result := '    if F' + Name + 'Exsit then '#13#10 + '    begin'#13#10 +
          '      ' + Name + 'Tmp := doc.CreateNode(''' + Path.Remove(0, 1) +
          ''', ntAttribute);'#13#10 + '      ' + Name + 'Tmp.NodeValue := ' +
          ConvertStr + ';'#13#10 + '      node.AttributeNodes.Add(' + Name +
          'Tmp);'#13#10 + '    end;'#13#10;
      1:
        Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + Path.Remove(0, 1)
          + ''', ntAttribute);'#13#10 + '    ' + Name + 'Tmp.NodeValue := ' +
          ConvertStr + ';'#13#10 + '    node.AttributeNodes.Add(' + Name +
          'Tmp);'#13#10;
      2:
        ;
    end;
  end;
end;

function TXML2CodeChildHelper.WriteChild: string;
begin
  Result := '';
  case NodeType of
    ntElement:
      begin
        if Leaf then
        begin
          case Number of
            0:
              begin
                Result := '    if F' + Name + 'Exsit then'#13#10 +
                  '    begin'#13#10 + '      ' + Name +
                  'Tmp := doc.CreateNode(''' + Path + ''', ntElement);'#13#10 +
                  '      ' + Name + 'Tmp.NodeValue := ' + ConvertStr + ';'#13#10
                  + '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 +
                  '    end;'#13#10;
              end;
            1:
              begin
                Result := '    ' + Name + 'Tmp := doc.CreateNode(''' + Path +
                  ''', ntElement);'#13#10 + '    ' + Name + 'Tmp.NodeValue := '
                  + ConvertStr + ';'#13#10 + '    node.ChildNodes.Add(' + Name +
                  'Tmp);'#13#10;
              end;
            2:
              begin
                Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10
                  + 'begin'#13#10 + '      ' + Name + 'Tmp := doc.CreateNode('''
                  + Path + ''', ntElement);'#13#10 + '      ' + Name +
                  'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                  '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 +
                  '    end;'#13#10;
              end;
          end;
        end
        else
        begin
          case Number of
            0:
              begin
                Result := '    if F' + Name + 'Exsit then'#13#10 + '      F' +
                  Name + '.ToXML(node, ''' + Path + ''');'#13#10;
              end;
            1:
              begin
                Result := '    F' + Name + '.ToXML(node, ''' + Path +
                  ''');'#13#10;
              end;
            2:
              begin
                Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10
                  + '       F' + Name + 's.Items[I].ToXML(node, ''' + Path +
                  ''');'#13#10;
              end;
          end;
        end;
      end;
    ntCData:
      begin
        case Number of
          0:
            begin
              Result := '    if F' + Name + 'Exsit then'#13#10 + 'begin'#13#10 +
                '      ' + Name + 'Tmp := doc.CreateNode(''' + Path.Remove(1, 1)
                + ''', ntCData);'#13#10 + '      ' + Name + 'Tmp.NodeValue := '
                + ConvertStr + ';'#13#10 + '      node.ChildNodes.Add(' + Name +
                'Tmp);'#13#10 + '    end;'#13#10;
            end;
          1:
            begin
              Result := '    ' + Name + 'Tmp := doc.CreateNode(''' +
                Path.Remove(1, 1) + ''', ntCData);'#13#10 + '    ' + Name +
                'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '    node.ChildNodes.Add(' + Name + 'Tmp);'#13#10;
            end;
          2:
            begin
              Result := '    for I := 0 to F' + Name + 's.Count - 1 do'#13#10 +
                'begin'#13#10 + '      ' + Name + 'Tmp := doc.CreateNode(''' +
                Path.Remove(1, 1) + ''', ntCData);'#13#10 + '      ' + Name +
                'Tmp.NodeValue := ' + ConvertStr + ';'#13#10 +
                '      node.ChildNodes.Add(' + Name + 'Tmp);'#13#10 +
                '    end;'#13#10;
            end;
        end;
      end;
  end;
end;

function TXML2CodeChildHelper.WriteText: string;
begin
  Result := '';
  if NodeType = ntText then
  begin
    case Number of
      0:
        begin
          Result := '    if F' + Name + 'Exsit then'#13#10 + '    begin'#13#10 +
            '      node.NodeValue := ' + ConvertStr + ';'#13#10 +
            '    end;'#13#10;
        end;
      1:
        begin
          Result := '    node.NodeValue := ' + ConvertStr + ';'#13#10;
        end;
      2:
        begin
        end;
    end;
  end;
end;

initialization

VisualCode := True;

end.
