unit ClientScreen;

interface

uses
  System.SysUtils, FMX.Controls, FMX.Types, System.Types;

type
  TScreenClientHelper = class helper for TControl
    function ClientToScreen(const pt: TPointF): TPointF; inline;
    function ScreenToClient(const pt: TPointF): TPointF; inline;
  end;

implementation

{ TScreenClientHelper }

function TScreenClientHelper.ClientToScreen(const pt: TPointF): TPointF;
begin
  Result := Scene.LocalToScreen(LocalToAbsolute(pt));
end;

function TScreenClientHelper.ScreenToClient(const pt: TPointF): TPointF;
begin
  Result := AbsoluteToLocal(Scene.ScreenToLocal(pt));
end;

end.
