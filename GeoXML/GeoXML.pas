unit GeoXML;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections,
  XMLCore, XMLLeafTypes,
  ClientScreen, FMXTee.Tree, FMX.Menus,
  GeoXMLBase;

type

  TGeoXMLHelper = class helper for TGeoXML
  private
  protected
  public
    procedure GetAllElement(var eles: TList<TGeoElement>);
  end;

  TGeoFolderHelper = class helper for TGeoFolder
  private
  protected
  public
    procedure GetAllElement(var eles: TList<TGeoElement>);
  end;

  TGeoElementHelper = class helper for TGeoElement
  private
  protected
  public
  end;

  TGeoPointHelper = class helper for TGeoPoint
  private
  protected
  public
  end;

  TGeoLineHelper = class helper for TGeoLine
  private
  protected
  public
  end;

  TGeoLinesHelper = class helper for TGeoLines
  private
  protected
  public
  end;

  TGeoPolygonHelper = class helper for TGeoPolygon
  private
  protected
  public
  end;

  TGeoStyleHelper = class helper for TGeoStyle
  private
  protected
  public
  end;

  TGeoPointStyleHelper = class helper for TGeoPointStyle
  private
  protected
  public
  end;

  TGeoLineStyleHelper = class helper for TGeoLineStyle
  private
  protected
  public
  end;

  TGeoPolygonStyleHelper = class helper for TGeoPolygonStyle
  private
  protected
  public
  end;

implementation

{ TGeoXMLHelper }

procedure TGeoXMLHelper.GetAllElement(var eles: TList<TGeoElement>);
var
  I: Integer;
begin
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllElement(eles);
  end;
end;

{ TGeoFolderHelper }

procedure TGeoFolderHelper.GetAllElement(var eles: TList<TGeoElement>);
var
  I: Integer;
begin
  for I := 0 to ElementCount - 1 do
  begin
    eles.Add(Element[I]);
  end;
  for I := 0 to FolderCount - 1 do
  begin
    Folder[I].GetAllElement(eles);
  end;
end;

end.
