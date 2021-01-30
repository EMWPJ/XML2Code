unit KMLForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, KMLFrame;

type
  TFormKML = class(TForm)
    KMLFrame: TFrameKML;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormKML: TFormKML;

implementation

{$R *.fmx}

procedure TFormKML.FormCreate(Sender: TObject);
begin
  KMLFrame.FrameInit;
end;

end.
