unit BaseMapFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, XMLLeafTypes, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  WChart, FMXTee.Editor.EditorPanel, FMX.Menus, FMXTee.Engine, FMXTee.Procs,
  FMXTee.Chart, FMXTee.Series.Surface, FMXTee.Series.Map, FMXTee.Series.World,
  KML, KMLBase, FMXTee.Series, GeoXML, GeoXMLBase;

type
  TFrameBaseMapChart = class(TFrameWChart)
    SeriesBaseMap: TMapSeries;
    procedure ChartAfterDraw(Sender: TObject);
    procedure ChartBeforeDrawSeries(Sender: TObject);
  private
    { Private declarations }
    basemapkml: TKML;
    FGeo: TGeoXML;
    procedure SetGeo(const Value: TGeoXML);

  public
    procedure FramInit;
    procedure LoadBaseMapFormKML(const fileName: String);
    procedure PaintGeoXML(Geo: TGeoXML);
    property Geo: TGeoXML read FGeo write SetGeo;
    { Public declarations }
  end;

var
  FrameBaseMapChart: TFrameBaseMapChart;

implementation

{$R *.fmx}

procedure TFrameBaseMapChart.ChartAfterDraw(Sender: TObject);
var
  I, J, count: Integer;
  eles: TList<TGeoElement>;
  x, y: Single;
  poly: Array of TPointF;
begin
  inherited;
  if not Assigned(Geo) then
    Exit;
  eles := TList<TGeoElement>.Create;
  Geo.GetAllElement(eles);
  for I := 0 to eles.count - 1 do
  begin
    if not eles[I].Visible then
      Continue;
    if eles[I] is TGeoPoint then
    begin
      x := GetX(TGeoPoint(eles[I]).Position[0].Longitude);
      y := GetY(TGeoPoint(eles[I]).Position[0].Latitude);
      Chart.Canvas.Ellipse(x - 5, y - 5, y - 5, y + 5);
    end
    else if eles[I] is TGeoLine then
    begin
      SetLength(poly, Length(TGeoLine(eles[I]).Position));
      for J := 0 to Length(TGeoLine(eles[I]).Position) - 1 do
      begin
        poly[J].x := GetX(TGeoLine(eles[I]).Position[J].Longitude);
        poly[J].y := GetY(TGeoLine(eles[I]).Position[J].Latitude);
      end;
      Chart.Canvas.Polyline(poly);
    end
    else if eles[I] is TGeoLines then
    begin
      SetLength(poly, Length(TGeoLine(eles[I]).Position) + 1);
      for J := 0 to Length(TGeoLine(eles[I]).Position) - 1 do
      begin
        poly[J].x := GetX(TGeoLine(eles[I]).Position[J].Longitude);
        poly[J].y := GetY(TGeoLine(eles[I]).Position[J].Latitude);
      end;
      poly[Length(TGeoLine(eles[I]).Position)].x := poly[0].x;
      poly[Length(TGeoLine(eles[I]).Position)].y := poly[0].y;
      Chart.Canvas.Polyline(poly);
    end
    else if eles[I] is TGeoPolygon then
    begin
      SetLength(poly, Length(TGeoPolygon(eles[I]).Position));
      for J := 0 to Length(TGeoPolygon(eles[I]).Position) - 1 do
      begin
        poly[J].x := GetX(TGeoPolygon(eles[I]).Position[J].Longitude);
        poly[J].y := GetY(TGeoPolygon(eles[I]).Position[J].Latitude);
      end;
      Chart.Canvas.Polygon(poly);
    end;
  end;
  eles.Free;
end;

procedure TFrameBaseMapChart.ChartBeforeDrawSeries(Sender: TObject);
var
  I, J, count: Integer;
  eles: TList<TGeoElement>;
  x, y: Single;
  poly: Array of TPointF;
begin
  inherited;
  // if not Assigned(Geo) then
  // Exit;
  // eles := TList<TGeoElement>.Create;
  // Geo.GetAllElement(eles);
  // for I := 0 to eles.count - 1 do
  // begin
  // if not eles[I].Visible then
  // Continue;
  // if eles[I] is TGeoPoint then
  // begin
  // x := GetX(TGeoPoint(eles[I]).Position[0].Longitude);
  // y := GetY(TGeoPoint(eles[I]).Position[0].Latitude);
  // Chart.Canvas.Ellipse(x - 5, y - 5, y - 5, y + 5);
  // end
  // else if eles[I] is TGeoLine then
  // begin
  // SetLength(poly, Length(TGeoLine(eles[I]).Position));
  // for J := 0 to Length(TGeoLine(eles[I]).Position) - 1 do
  // begin
  // poly[J].x := GetX(TGeoLine(eles[I]).Position[J].Longitude);
  // poly[J].y := GetY(TGeoLine(eles[I]).Position[J].Latitude);
  // end;
  // Chart.Canvas.Polyline(poly);
  // end
  // else if eles[I] is TGeoLines then
  // begin
  // SetLength(poly, Length(TGeoLine(eles[I]).Position) + 1);
  // for J := 0 to Length(TGeoLine(eles[I]).Position) - 1 do
  // begin
  // poly[J].x := GetX(TGeoLine(eles[I]).Position[J].Longitude);
  // poly[J].y := GetY(TGeoLine(eles[I]).Position[J].Latitude);
  // end;
  // poly[Length(TGeoLine(eles[I]).Position)].x := poly[0].x;
  // poly[Length(TGeoLine(eles[I]).Position)].y := poly[0].y;
  // Chart.Canvas.Polyline(poly);
  // end
  // else if eles[I] is TGeoPolygon then
  // begin
  // SetLength(poly, Length(TGeoPolygon(eles[I]).Position));
  // for J := 0 to Length(TGeoPolygon(eles[I]).Position) - 1 do
  // begin
  // poly[J].x := GetX(TGeoPolygon(eles[I]).Position[J].Longitude);
  // poly[J].y := GetY(TGeoPolygon(eles[I]).Position[J].Latitude);
  // end;
  // Chart.Canvas.Polygon(poly);
  // end;
  // end;
  // eles.Free;
end;

procedure TFrameBaseMapChart.FramInit;
begin
  //
end;

procedure TFrameBaseMapChart.LoadBaseMapFormKML(const fileName: String);
var
  folder: TKMLFolder;
  place: TKMLPlacemark;
  Polygon: TKMLPolygon;
  Boundary: TKMLBoundary;
  coors: ArrayCoordinates;
  teep: TTeePolygon;
  I, J, K, M: Integer;
begin
  SeriesBaseMap.Shapes.Clear;
  if Not Assigned(basemapkml) then
    basemapkml := TKML.Create(nil);
  basemapkml.Load(fileName);
  for I := 0 to basemapkml.Document.FolderCount - 1 do
  begin
    folder := basemapkml.Document.folder[I];
    for J := 0 to folder.PlacemarkCount - 1 do
    begin
      place := folder.Placemark[J];
      if place.Geometry is TKMLPolygon then
      begin
        Polygon := TKMLPolygon(place.Geometry);
        for K := 0 to Polygon.OuterBoundaryIsCount - 1 do
        begin
          Boundary := Polygon.OuterBoundaryIs[K];
          coors := Boundary.LinearRing.Coordinates;
          teep := SeriesBaseMap.Shapes.Add;
          for M := 0 to Length(coors) - 1 do
          begin
            teep.AddXY(coors[M].Longitude, coors[M].Latitude);
          end;
        end;
        for K := 0 to Polygon.InnerBoundaryIsCount - 1 do
        begin
          Boundary := Polygon.InnerBoundaryIs[K];
          coors := Boundary.LinearRing.Coordinates;
          teep := SeriesBaseMap.Shapes.Add;
          for M := 0 to Length(coors) - 1 do
          begin
            teep.AddXY(coors[M].Longitude, coors[M].Latitude);
          end;
        end;
      end;
    end;
  end;
end;

procedure TFrameBaseMapChart.PaintGeoXML(Geo: TGeoXML);
var
  geoGroup: TSeriesGroup;
  I, J, count: Integer;
  eles: TList<TGeoElement>;
  ps: TPointSeries;
  ls: TFastLineSeries;
  mp: TMapSeries;
  pl: TTeePolygon;
  pointx, pointy: Double1D;
begin
  geoGroup := Chart.SeriesGroups.Items[1];
  count := geoGroup.Series.count;
  for I := count - 1 downto 0 do
  begin
    geoGroup.Series[I].Free;
  end;
  eles := TList<TGeoElement>.Create;
  Geo.GetAllElement(eles);
  ps := TPointSeries.Create(Chart);
  ps.ParentChart := Chart;
  ps.ColorEachPoint := True;
  ps.Pointer.Size := 5;
  ps.Pointer.Style := TSeriesPointerStyle.psCircle;
  geoGroup.Add(ps);
  mp := TMapSeries.Create(Chart);
  mp.ParentChart := Chart;
  mp.Pen.Color := TAlphaColorRec.Red;
  geoGroup.Add(mp);
  for I := 0 to eles.count - 1 do
  begin
    if not eles[I].Visible then
      Continue;
    if eles[I] is TGeoPoint then
    begin
      ps.AddXY(TGeoPoint(eles[I]).Position[0].Longitude,
        TGeoPoint(eles[I]).Position[0].Latitude, TGeoPoint(eles[I]).Name);
    end
    else if eles[I] is TGeoLine then
    begin
      ls := TFastLineSeries.Create(Chart);
      ls.ParentChart := Chart;
      ls.Title := TGeoLine(eles[I]).Name;
      for J := 0 to Length(TGeoLine(eles[I]).Position) - 1 do
      begin
        ls.AddXY(TGeoLine(eles[I]).Position[J].Longitude,
          TGeoLine(eles[I]).Position[J].Latitude);
      end;
      geoGroup.Add(ls);
    end
    else if eles[I] is TGeoLines then
    begin
      pl := mp.Shapes.Add;
      pl.Text := TGeoLines(eles[I]).Name;
      for J := 0 to Length(TGeoLines(eles[I]).Position) - 1 do
      begin
        pl.AddXY(TGeoLines(eles[I]).Position[J].Longitude,
          TGeoLines(eles[I]).Position[J].Latitude);
      end;
      pl.Closed := TGeoLines(eles[I]).Closed;
    end
    else if eles[I] is TGeoPolygon then
    begin
      pl := mp.Shapes.Add;
      pl.Text := TGeoPolygon(eles[I]).Name;
      for J := 0 to Length(TGeoPolygon(eles[I]).Position) - 1 do
      begin
        pl.AddXY(TGeoPolygon(eles[I]).Position[J].Longitude,
          TGeoPolygon(eles[I]).Position[J].Latitude);
      end;
    end;
  end;
  eles.Free;
  Chart.Repaint;
end;

procedure TFrameBaseMapChart.SetGeo(const Value: TGeoXML);
begin
  FGeo := Value;
end;

end.
