unit ListViewFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Menus, FMX.ListView, System.Generics.Collections, System.Generics.Defaults,
  XMLLeafTypes, XMLEvent;

type
  TFrameListView = class(TFrame)
    List: TListView;
    ListPop: TPopupMenu;
    ClearMenu: TMenuItem;
    RemoveUnCheckedMenu: TMenuItem;
    RemoveCheckedMenu: TMenuItem;
    procedure ClearMenuClick(Sender: TObject);
    procedure ListItemClickEx(const Sender: TObject; ItemIndex: Integer; const
      LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
    procedure RemoveCheckedMenuClick(Sender: TObject);
    procedure RemoveUnCheckedMenuClick(Sender: TObject);
  private
    FItems: TList<TObject>;
    FNames: TStrings;
    FOnRemoveItem: TNotifyEvent;
    FOnClickItem: TBooleanEvent;
    FOnClickIndex: TIndexBooleanEvent;
    FOnRemoveIndex: TIntegerEvent;
    procedure UpdateList;
  public
    procedure SetListData(Names: String1D; objects: TList<TObject> = nil);
    property Items: TList<TObject> read FItems;
    property Names: TStrings read FNames;
  published
    property OnRemoveItem: TNotifyEvent read FOnRemoveItem write FOnRemoveItem;
    property OnClickItem: TBooleanEvent read FOnClickItem write FOnClickItem;
    property OnClickIndex: TIndexBooleanEvent read FOnClickIndex write
      FOnClickIndex;
    property OnRemoveIndex: TIntegerEvent read FOnRemoveIndex write FOnRemoveIndex;
  end;

implementation

{$R *.fmx}


procedure TFrameListView.ClearMenuClick(Sender: TObject);
var
  I, Count: Integer;
begin
  Count := FItems.Count;
  for I := Count - 1 downto 0 do
  begin
    if List.Items.Checked[I] then
    begin
      FOnRemoveIndex(FItems[I], I);
    end;
  end;
  FItems.Clear;
  FNames.Clear;
end;

procedure TFrameListView.ListItemClickEx(const Sender: TObject; ItemIndex:
  Integer; const LocalClickPos: TPointF; const ItemObject: TListItemDrawable);
begin
  FOnClickIndex(ItemIndex, List.Items.Checked[ItemIndex]);
end;

procedure TFrameListView.RemoveCheckedMenuClick(Sender: TObject);
var
  I, Count: Integer;
begin
  Count := FItems.Count;
  for I := Count - 1 downto 0 do
  begin
    if List.Items.Checked[I] then
    begin
      FItems.Delete(I);
      FNames.Delete(I);
      FOnRemoveIndex(FItems[I], I);
    end;
  end;
end;

procedure TFrameListView.RemoveUnCheckedMenuClick(Sender: TObject);
var
  I, Count: Integer;
begin
  Count := FItems.Count;
  for I := Count - 1 downto 0 do
  begin
    if not List.Items.Checked[I] then
    begin
      FItems.Delete(I);
      FNames.Delete(I);
      FOnRemoveIndex(FItems[I], I);
    end;
  end;
end;

procedure TFrameListView.SetListData(Names: String1D; objects: TList<TObject> = nil);
var
  I, Count: Integer;
begin
  if not Assigned(FItems) then
    FItems := TList<TObject>.Create;
  if not Assigned(FNames) then
    FNames := TStringList.Create;
  FItems.Clear;
  FNames.Clear;
  Count := Length(Names);
  for I := 0 to Count - 1 do
  begin
    FNames.Add(Names[I]);
  end;
  if Assigned(objects) then
  begin
    for I := 0 to Count - 1 do
    begin
      FItems.Add(objects[I]);
    end;
  end
  else
  begin
    for I := 0 to Count - 1 do
    begin
      FItems.Add(nil);
    end;
  end;
  UpdateList;
end;

procedure TFrameListView.UpdateList;
var
  I: Integer;
  vitem: TListViewItem;
begin
  List.BeginUpdate;
  List.Items.Clear;
  List.AllowSelection := True;
  List.EditMode := True;
  for I := 0 to FItems.Count - 1 do
  begin
    vitem := List.Items.Add;
    vitem.Text := FNames[I];
    vitem.Checked := True;
  end;
  List.EndUpdate;
end;

end.
