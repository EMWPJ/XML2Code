unit LinesEditorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Menus, FMX.Controls.Presentation, XMLLeafTypes;

type

  TFrameLinesEditor = class(TFrame)
    ListBox: TListBox;
    PopupMenuLines: TPopupMenu;
    LoadMenu: TMenuItem;
    SaveMenu: TMenuItem;
    OpenDialogLines: TOpenDialog;
    SaveDialogLines: TSaveDialog;
    ClearMenu: TMenuItem;
    procedure ClearMenuClick(Sender: TObject);
    procedure LoadFile(Sender: TObject; fileName: string);
    procedure LoadMenuClick(Sender: TObject);
    procedure SaveFile(Sender: TObject; fileName: string);
    procedure SaveMenuClick(Sender: TObject);
  private
    FonSetString: StringEvent;
    FonSetStrings: StringsEvent;
  public
    procedure SetStrings(Sender: TObject; strs: TStrings);
  published
    property onSetString: StringEvent read FonSetString write FonSetString;
    property onSetStrings: StringsEvent read FonSetStrings write FonSetStrings;
  end;

implementation

{$R *.fmx}
{ TFrameLinesEditor }

procedure TFrameLinesEditor.ClearMenuClick(Sender: TObject);
begin
  ListBox.Items.Clear;
end;

procedure TFrameLinesEditor.LoadFile(Sender: TObject; fileName: string);
begin
  ListBox.Items.Clear;
  ListBox.Items.LoadFromFile(fileName);
end;

procedure TFrameLinesEditor.LoadMenuClick(Sender: TObject);
begin
  if OpenDialogLines.Execute then
    LoadFile(Sender, OpenDialogLines.fileName);
end;

procedure TFrameLinesEditor.SaveFile(Sender: TObject; fileName: string);
begin
  ListBox.Items.SaveToFile(fileName);
end;

procedure TFrameLinesEditor.SaveMenuClick(Sender: TObject);
begin
  if SaveDialogLines.Execute then
    SaveFile(Sender, SaveDialogLines.fileName);
end;

procedure TFrameLinesEditor.SetStrings(Sender: TObject; strs: TStrings);
var
  I: Integer;
begin
  ListBox.Items.Clear;
  for I := 0 to strs.Count - 1 do
  begin
    ListBox.Items.Add(strs[I]);
  end;
end;

end.
