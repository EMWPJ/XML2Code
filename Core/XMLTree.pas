unit XMLTree;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.Controls.Presentation,
  FMX.ScrollBox, XMLLeafTypes, XMLCore, FMX.Layouts, FMX.ListBox,
  FMX.Edit, FMX.Colors, FMX.SpinBox, FMX.EditBox, FMX.NumberBox,
  FMX.DateTimeCtrls, FMXTee.Tree, FMXTee.Procs;

type

  TXMLTree = class;

  TXMLTree = class(TTree)

  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('XML2Code', [TXMLTree]);
end;

constructor TXMLTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.AllowPanning := TPanningMode.pmVertical;
  Self.SingleSelection := False;
  Self.Selected.ShiftState := [ssCtrl];
  Self.ScrollMouseButton := TMouseButton.mbLeft;
  Self.WheelNavigation := TTreeWheelNavigation.wnZoom;
  Self.Zoom.MouseWheel := TMouseWheelStyle.pmwNormal;
  Self.Zoom.MouseButton := TMouseButton.mbMiddle;
end;

end.
