unit XMLInspector;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Controls.Presentation,
  FMX.ScrollBox, XMLLeafTypes, XMLCore, FMX.Layouts, FMX.ListBox,
  FMX.Edit, FMX.Colors, FMX.SpinBox, FMX.EditBox, FMX.NumberBox,
  FMX.DateTimeCtrls, LinesEditorForm, TextEditorForm;

type
  TXMLInspector = class;
  TXMLEditorButton = class;

  TXMLInspector = class(TVertScrollBox)
  private
    FNames: TStringList;
    FOnSetEvent: XMLInspectorEvent;
    FTypes: TList<XMLTypes>;
    FValues: TStringList;
    FTarget: TXML;
    procedure SetNames(const value: TStringList);
    procedure SetTypes(const value: TList<XMLTypes>);
    procedure SetValues(const value: TStringList);
    procedure SetTarget(const value: TXML);
    procedure AddIndex(const Index: Integer);
    procedure AddLabel(const Index: Integer);
    procedure AddEdit(const Index: Integer);
    procedure AddCheckBox(const Index: Integer);
    procedure AddComboBox(const Index: Integer);
    procedure AddDateEdit(const Index: Integer);
    procedure AddTimeEdit(const Index: Integer);
    procedure AddDateTimeEdit(const Index: Integer);
    procedure AddNumberBox(const Index: Integer);
    procedure AddSpinBox(const Index: Integer);
    procedure AddColorComboBox(const Index: Integer);
    procedure AddXMLEditorButton(const Index: Integer; const xtype: XMLTypes);
  public
    procedure Change(Sender: TObject);
    procedure SetData(NamesValue, ValuesValue: TStringList;
      TypesValue: TList<XMLTypes>; Sender: TXML);
    procedure SetXMLProperty(Index: Integer; value: String); virtual;
    property Types: TList<XMLTypes> read FTypes write SetTypes;
    property Target: TXML read FTarget write SetTarget;
  published
    property Names: TStringList read FNames write SetNames;
    property Values: TStringList read FValues write SetValues;
    property OnSetEvent: XMLInspectorEvent read FOnSetEvent write FOnSetEvent;
  end;

  TXMLEditorButton = class(TButton)
  private
    FOnChange: TNotifyEvent;
    FXValue: string;
    FXType: XMLTypes;
    procedure SetXValue(const value: string);
    procedure SetXType(const value: XMLTypes);
  public
    procedure ShowEditor(Sender: TObject);
    procedure SetValue(str: string);
    property XValue: string read FXValue write SetXValue;
    property xtype: XMLTypes read FXType write SetXType;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XML2Code', [TXMLInspector]);
end;

procedure TXMLInspector.AddCheckBox(const Index: Integer);
var
  tmp: TCheckBox;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  tmp := TCheckBox.Create(pn);
  tmp.Parent := pn;
  tmp.Height := 20;
  tmp.Align := TAlignLayout.Top;
  tmp.TextAlign := TTextAlign.Center;
  tmp.Text := FNames[Index];
  tmp.IsChecked := String2Boolean(FValues[Index]);
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddColorComboBox(const Index: Integer);
var
  tmp: TColorComboBox;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TColorComboBox.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddComboBox(const Index: Integer);
var
  tmp: TComboBox;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TComboBox.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddDateEdit(const Index: Integer);
var
  tmp: TDateEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TDateEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Date := StrToDateDef(FValues[Index], Now(), XMLDateFormat);
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddDateTimeEdit(const Index: Integer);
var
  tmp: TDateEdit;
  tmpt: TTimeEdit;
  lb: TLabel;
  pn: TPanel;
  dt: TDateTime;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  dt := StrToDateTimeDef(FValues[Index], Now(), XMLDateTimeFormat);
  tmp := TDateEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Left;
  tmp.Date := dt;
  tmp.OnChange := Change;
  tmpt := TTimeEdit.Create(pn);
  tmpt.Parent := pn;
  tmpt.Align := TAlignLayout.Client;
  tmpt.Time := dt;
  tmpt.TimeFormatKind := TDTFormatKind.Long;
  tmpt.OnChange := Change;
end;

procedure TXMLInspector.AddEdit(const Index: Integer);
var
  tmp: TEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddIndex(const Index: Integer);
begin
  case FTypes[Index] of
    xs_ENTITIES:
      begin
        AddEdit(Index);
      end;
    xs_ENTITY:
      begin
        AddEdit(Index);
      end;
    xs_ID:
      begin
        AddEdit(Index);
      end;
    xs_IDREF:
      begin
        AddEdit(Index);
      end;
    xs_IDREFSlanguage:
      begin
        AddEdit(Index);
      end;
    xs_Name:
      begin
        AddEdit(Index);
      end;
    xs_NCName:
      begin
        AddEdit(Index);
      end;
    xs_NMTOKEN:
      begin
        AddEdit(Index);
      end;
    xs_NMTOKENS:
      begin
        AddEdit(Index);
      end;
    xs_normalizedString:
      begin
        AddEdit(Index);
      end;
    xs_QName:
      begin
        AddEdit(Index);
      end;
    xs_string:
      begin
        AddEdit(Index);
      end;
    xs_token:
      begin
        AddEdit(Index);
      end;
    xs_date:
      begin
        AddDateEdit(Index);
      end;
    xs_time:
      begin
        AddTimeEdit(Index);
      end;
    xs_dateTime:
      begin
        AddDateTimeEdit(Index);
      end;
    xs_duration:
      begin
        AddTimeEdit(Index);
      end;
    xs_byte:
      begin
        AddEdit(Index);
      end;
    xs_decimal:
      begin
        AddEdit(Index);
      end;
    xs_int:
      begin
        AddEdit(Index);
      end;
    xs_integer:
      begin
        AddEdit(Index);
      end;
    xs_long:
      begin
        AddEdit(Index);
      end;
    xs_negativeInteger:
      begin
        AddEdit(Index);
      end;
    xs_nonNegativeInteger:
      begin
        AddEdit(Index);
      end;
    xs_nonPositiveInteger:
      begin
        AddEdit(Index);
      end;
    xs_positiveInteger:
      begin
        AddNumberBox(Index);
      end;
    xs_short:
      begin
        AddEdit(Index);
      end;
    xs_unsignedLong:
      begin
        AddEdit(Index);
      end;
    xs_unsignedInt:
      begin
        AddEdit(Index);
      end;
    xs_unsignedShort:
      begin
        AddEdit(Index);
      end;
    xs_unsignedByte:
      begin
        AddEdit(Index);
      end;
    xs_anyURI:
      begin
        AddEdit(Index);
      end;
    xs_base64Binary:
      begin
        AddEdit(Index);
      end;
    xs_boolean:
      begin
        AddCheckBox(Index);
      end;
    xs_double:
      begin
        AddEdit(Index);
      end;
    xs_float:
      begin
        AddEdit(Index);
      end;
    xs_hexBinary:
      begin
        AddEdit(Index);
      end;
    xs_NOTATION:
      begin
        AddEdit(Index);
      end;
    xs_Class:
      begin
        AddEdit(Index);
      end;
    xml_Complex:
      begin
        AddEdit(Index);
      end;
    xml_Coordinate:
      begin
        AddEdit(Index);
      end;
    xml_ArrayCoordinates:
      begin
        AddXMLEditorButton(Index, xml_ArrayCoordinates);
      end;
    xml_Double1D:
      begin
        AddXMLEditorButton(Index, xml_Double1D);
      end;
    xml_Integer1D:
      begin
        AddXMLEditorButton(Index, xml_Integer1D);
      end;
    xml_Double2D:
      begin
        AddXMLEditorButton(Index, xml_Double2D);
      end;
    xml_Integer2D:
      begin
        AddXMLEditorButton(Index, xml_Integer2D);
      end;
    xml_Byte1D:
      begin
        AddXMLEditorButton(Index, xml_Byte1D);
      end;
    xml_Boolean1D:
      begin
        AddXMLEditorButton(Index, xml_Boolean1D);
      end;
    xml_Byte2D:
      begin
        AddXMLEditorButton(Index, xml_Byte2D);
      end;
    xml_Boolean2D:
      begin
        AddXMLEditorButton(Index, xml_Boolean2D);
      end;
    W_Point2I:
      begin
        AddEdit(Index);
      end;
    W_Point2D:
      begin
        AddEdit(Index);
      end;
    W_Point3I:
      begin
        AddEdit(Index);
      end;
    W_Point3D:
      begin
        AddEdit(Index);
      end;
    W_Point2Is:
      begin
        AddEdit(Index);
      end;
    W_Point2Ds:
      begin
        AddEdit(Index);
      end;
    W_Point3Is:
      begin
        AddEdit(Index);
      end;
    W_Point3Ds:
      begin
        AddEdit(Index);
      end;
    xml_Pointer:
      begin
        AddEdit(Index);
      end;
  end;
end;

procedure TXMLInspector.AddLabel(const Index: Integer);
var
  tmp: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  tmp := TLabel.Create(pn);
  tmp.Parent := pn;
  tmp.Height := 20;
  tmp.Align := TAlignLayout.Top;
  tmp.TextAlign := TTextAlign.Center;
  tmp.Text := FNames[Index];
end;

procedure TXMLInspector.AddNumberBox(const Index: Integer);
var
  tmp: TNumberBox;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TNumberBox.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddSpinBox(const Index: Integer);
var
  tmp: TSpinBox;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TSpinBox.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Text := FValues[Index];
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddTimeEdit(const Index: Integer);
var
  tmp: TTimeEdit;
  lb: TLabel;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  lb := TLabel.Create(pn);
  lb.Parent := pn;
  lb.Text := FNames[Index];
  lb.Align := TAlignLayout.Left;
  lb.TextSettings.HorzAlign := TTextAlign.Center;
  tmp := TTimeEdit.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Time := StrToTimeDef(FValues[Index], Now(), XMLTimeFormat);
  tmp.TimeFormatKind := TDTFormatKind.Long;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.AddXMLEditorButton(const Index: Integer;
  const xtype: XMLTypes);
var
  tmp: TXMLEditorButton;
  pn: TPanel;
begin
  pn := TPanel.Create(Self);
  pn.Parent := Self;
  pn.Height := 20;
  pn.Align := TAlignLayout.Top;
  tmp := TXMLEditorButton.Create(pn);
  tmp.Parent := pn;
  tmp.Align := TAlignLayout.Client;
  tmp.Text := FNames[Index];
  tmp.XValue := FValues[Index];
  tmp.xtype := xtype;
  tmp.OnClick := tmp.ShowEditor;
  tmp.OnChange := Change;
end;

procedure TXMLInspector.Change(Sender: TObject);
var
  Index: Integer;
begin
  Index := Self.Children.Items[1].Children.IndexOf(TFmxObject(Sender).Parent);
  case FTypes[Index] of
    xs_ENTITIES:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_ENTITY:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_ID:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_IDREF:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_IDREFSlanguage:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_Name:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_NCName:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_NMTOKEN:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_NMTOKENS:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_normalizedString:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_QName:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_string:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_token:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_date:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_time:
      begin
        FValues[Index] := FormatDateTime('hh:mm:ss', TTimeEdit(Sender).Time);
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_dateTime:
      begin
        FValues[Index] := FormatDateTime('yyyy-mm-dd', TDateEdit(Sender).Date);
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_duration:
      begin
        FValues[Index] := FormatDateTime('yyyy-mm-dd',
          TDateEdit(TFmxObject(Sender).Parent.Children.Items[0]).Date) + 'T' +
          FormatDateTime('hh:mm:ss',
          TTimeEdit(TFmxObject(Sender).Parent.Children.Items[1]).Time);
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_byte:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_decimal:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_int:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_integer:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_long:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_negativeInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_nonNegativeInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_nonPositiveInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_positiveInteger:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_short:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_unsignedLong:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_unsignedInt:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_unsignedShort:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_unsignedByte:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_anyURI:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_base64Binary:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_boolean:
      begin
        FValues[Index] := Boolean2String(TCheckBox(Sender).IsChecked);
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_double:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_float:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_hexBinary:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_NOTATION:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xs_Class:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Complex:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_ArrayCoordinates:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Double1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Integer1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Double2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Integer2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Byte1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Boolean1D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Byte2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Boolean2D:
      begin
        FValues[Index] := TXMLEditorButton(Sender).XValue;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point2I:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point2D:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point3I:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point3D:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point2Is:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point2Ds:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point3Is:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    W_Point3Ds:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
    xml_Pointer:
      begin
        FValues[Index] := TEdit(Sender).Text;
        OnSetEvent(Index, FValues[Index]);
      end;
  end;
end;

procedure TXMLInspector.SetData(NamesValue, ValuesValue: TStringList;
  TypesValue: TList<XMLTypes>; Sender: TXML);
var
  I, count: Integer;
begin
  if Assigned(FNames) then
    FNames.Free;
  if Assigned(FTypes) then
    FTypes.Free;
  if Assigned(FValues) then
    FValues.Free;
  FNames := NamesValue;
  FValues := ValuesValue;
  FTypes := TypesValue;
  FTarget := Sender;
  Self.FOnSetEvent := Sender.SetXMLProperty;
  Sender.OnSetEvent := Self.SetXMLProperty;
  while Self.Children.Items[1].ChildrenCount > 0 do
  begin
    if Assigned(Self.Children.Items[1].Children.Items[0]) then
      Self.Children.Items[1].Children.Items[0].Free;
  end;
  count := FNames.count;
  for I := 0 to count - 1 do
  begin
    AddIndex(I);
  end;
end;

procedure TXMLInspector.SetNames(const value: TStringList);
begin
  FNames := value;
end;

procedure TXMLInspector.SetXMLProperty(Index: Integer; value: String);
begin
  FValues[index] := value;
end;

procedure TXMLInspector.SetTypes(const value: TList<XMLTypes>);
begin
  FTypes := value;
end;

procedure TXMLInspector.SetValues(const value: TStringList);
begin
  FValues := value;
end;

procedure TXMLInspector.SetTarget(const value: TXML);
begin
  FTarget := value;
end;

procedure TXMLEditorButton.SetXValue(const value: string);
begin
  FXValue := value;
end;

procedure TXMLEditorButton.SetValue(str: string);
begin
  FXValue := str;
  OnChange(Self);
end;

procedure TXMLEditorButton.SetXType(const value: XMLTypes);
begin
  FXType := value;
end;

procedure TXMLEditorButton.ShowEditor(Sender: TObject);
begin
  case FXType of
    xs_ENTITIES:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_ENTITY:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_ID:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_IDREF:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_IDREFSlanguage:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_Name:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_NCName:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_NMTOKEN:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_NMTOKENS:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_normalizedString:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_QName:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_string:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_token:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_date:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_time:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_dateTime:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_duration:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_byte:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_decimal:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_int:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_integer:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_long:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_negativeInteger:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_nonNegativeInteger:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_nonPositiveInteger:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_positiveInteger:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_short:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_unsignedLong:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_unsignedInt:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_unsignedShort:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_unsignedByte:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_anyURI:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_base64Binary:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_boolean:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_double:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_float:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_hexBinary:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_NOTATION:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xs_Class:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Complex:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_ArrayCoordinates:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Double1D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Integer1D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Double2D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Integer2D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Byte1D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Boolean1D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Byte2D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Boolean2D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point2I:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point2D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point3I:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point3D:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point2Is:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point2Ds:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point3Is:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    W_Point3Ds:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
    xml_Pointer:
      begin
        if Assigned(FormTextEditor) then
        begin
          FormTextEditor.TextEditor.SetString(Self.FXValue);
          FormTextEditor.TextEditor.onSetString := Self.SetValue;
          FormTextEditor.ShowModal;
        end;
      end;
  else
    begin
      if Assigned(FormTextEditor) then
      begin
        FormTextEditor.TextEditor.SetString(Self.FXValue);
        FormTextEditor.TextEditor.onSetString := Self.SetValue;
        FormTextEditor.ShowModal;
      end;
    end;
  end;
end;

end.
