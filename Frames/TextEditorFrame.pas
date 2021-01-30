unit TextEditorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Menus, FMX.Controls.Presentation, XMLLeafTypes,
  FMX.ScrollBox, FMX.Memo;

type

  TFrameTextEditor = class(TFrame)
    PopupMenuLines: TPopupMenu;
    LoadMenu: TMenuItem;
    SaveMenu: TMenuItem;
    OpenDialogText: TOpenDialog;
    SaveDialogText: TSaveDialog;
    MemoText: TMemo;
    procedure LoadFile(Sender: TObject; fileName: string);
    procedure LoadMenuClick(Sender: TObject);
    procedure SaveFile(Sender: TObject; fileName: string);
    procedure SaveMenuClick(Sender: TObject);
  private
    FonSetString: StringEvent;
  public
    procedure SetString(str: string);
  published
    property onSetString: StringEvent read FonSetString write FonSetString;
  end;

implementation

{$R *.fmx}
{ TFrameTextEditor }


procedure TFrameTextEditor.LoadFile(Sender: TObject; fileName: string);
begin
  MemoText.Lines.Clear;
  MemoText.Lines.LoadFromFile(fileName);
end;

procedure TFrameTextEditor.LoadMenuClick(Sender: TObject);
begin
  if OpenDialogText.Execute then
    LoadFile(Sender, OpenDialogText.fileName);
end;

procedure TFrameTextEditor.SaveFile(Sender: TObject; fileName: string);
begin
  MemoText.Lines.SaveToFile(fileName);
end;

procedure TFrameTextEditor.SaveMenuClick(Sender: TObject);
begin
  if SaveDialogText.Execute then
    SaveFile(Sender, SaveDialogText.fileName);
end;

procedure TFrameTextEditor.SetString(str: string);
var
  I: Integer;
begin
  MemoText.WordWrap := True;
  MemoText.Text := str;
end;

end.

