unit WChart;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Math,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMXTee.Engine, FMXTee.Procs, FMXTee.Chart, FMX.Menus,
  FMXTee.Editor.EditorPanel, FMXTee.Tools, FMXTee.Series,
  FMXTee.Series.Surface, MaxMinForm;

type
  TFrameWChart = class(TFrame)
    Chart: TChart;
    ChartEditor: TChartEditor;
    PopChart: TPopupMenu;
    ChartEditorMenu: TMenuItem;
    AutoAxisMenu: TMenuItem;
    ZoomXMenu: TMenuItem;
    ZoomYMenu: TMenuItem;
    AutoXAxisMenu: TMenuItem;
    AutoYAxisMenu: TMenuItem;
    AddTempLineMenu: TMenuItem;
    AutoZAxisMenu: TMenuItem;
    RestoreAxisMenu: TMenuItem;
    RecordAxisMenu: TMenuItem;
    XLogMenu: TMenuItem;
    YLogMenu: TMenuItem;
    ExportToClipBoardMenu: TMenuItem;
    procedure AddTempLineMenuClick(Sender: TObject);
    procedure ChartEditorShow(Sender: TObject);
    procedure AutoAxis(Sender: TObject);
    procedure AutoXAxis(Sender: TObject);
    procedure AutoYAxis(Sender: TObject);
    procedure AutoZAxis(Sender: TObject);
    procedure ChartClickAxis(Sender: TCustomChart; Axis: TChartAxis; Button:
      TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ExportToClipBoardMenuClick(Sender: TObject);
    procedure RecordAxisMenuClick(Sender: TObject);
    procedure RestoreAxisMenuClick(Sender: TObject);
    procedure ZoomXMenuClick(Sender: TObject);
    procedure ZoomYMenuClick(Sender: TObject);
    procedure SymmetryYAxis(Sender: TObject);
    procedure XLogMenuClick(Sender: TObject);
    procedure YLogMenuClick(Sender: TObject);
  private
    FXLog: Boolean;
    FYLog: Boolean;
    FXMax: Double;
    FYMax: Double;
    FXMin: Double;
    FYMin: Double;
    procedure SetXLog(const Value: Boolean);
    procedure SetYLog(const Value: Boolean);
    procedure SetXMax(const Value: Double);
    procedure SetYMax(const Value: Double);
    procedure SetXMin(const Value: Double);
    procedure SetYMin(const Value: Double);
    { Private declarations }
  public
    function GetChartX(X: Double): Double;
    function GetChartY(Y: Double): Double;
    function GetChartPosition(point: TPoint): TPoint;
    function GetX(X: Double): Double;
    function GetY(Y: Double): Double;
    function GetPosition(point: TPoint): TPoint;
    property XLog: Boolean read FXLog write SetXLog;
    property YLog: Boolean read FYLog write SetYLog;
    property XMax: Double read FXMax write SetXMax;
    property YMax: Double read FYMax write SetYMax;
    property XMin: Double read FXMin write SetXMin;
    property YMin: Double read FYMin write SetYMin;
  end;

implementation

{$R *.fmx}


procedure TFrameWChart.AddTempLineMenuClick(Sender: TObject);
var
  I, count: Integer;
  strs: TStringList;
  dia: TOpenDialog;
  ser: TLineSeries;
  xystr: TArray<string>;
begin
  strs := TStringList.Create;
  dia := TOpenDialog.Create(Self);
  if dia.Execute then
  begin
    strs.LoadFromFile(dia.FileName);
    count := strs.count;
    ser := TLineSeries.Create(Chart);
    for I := 0 to count - 1 do
    begin
      xystr := strs[I].Split([' ', #9, #10, #13]);
      if Length(xystr) < 2 then
        Continue;
      ser.AddXY(xystr[0].ToDouble, xystr[1].ToDouble);
    end;
    Chart.AddSeries(ser);
  end;
  dia.Free;
end;

procedure TFrameWChart.AutoAxis(Sender: TObject);
begin
  Chart.BottomAxis.Automatic := True;
  Chart.LeftAxis.Automatic := True;
  Chart.DepthAxis.Automatic := True;
end;

procedure TFrameWChart.AutoXAxis(Sender: TObject);
begin
  Chart.BottomAxis.Automatic := True;
end;

procedure TFrameWChart.AutoYAxis(Sender: TObject);
begin
  Chart.LeftAxis.Automatic := True;
end;

procedure TFrameWChart.AutoZAxis(Sender: TObject);
begin
  Chart.DepthAxis.Automatic := True;
end;

procedure TFrameWChart.ChartClickAxis(Sender: TCustomChart; Axis: TChartAxis;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min, max: Double;
begin
  min := Axis.Minimum;
  max := Axis.Maximum;
  FormMaxMIn.SetMaxMin(min, max);
  if Axis = Chart.BottomAxis then
  begin
    FXMin := min;
    FXMax := max;
  end
  else if Axis = Chart.LeftAxis then
  begin
    FYMin := min;
    FYMax := max;
  end;
  Axis.SetMinMax(min, max);
end;

procedure TFrameWChart.ChartEditorShow(Sender: TObject);
begin
  ChartEditor.Execute;
end;

procedure TFrameWChart.ExportToClipBoardMenuClick(Sender: TObject);
begin
  Chart.CopyToClipboardBitmap;
end;

function TFrameWChart.GetChartPosition(point: TPoint): TPoint;
begin
  Result.X := Trunc(GetChartX(point.X));
  Result.Y := Trunc(GetChartY(point.Y));
end;

function TFrameWChart.GetChartX(X: Double): Double;
var
  logbase: Double;
begin
  if Chart.Axes.Bottom.Logarithmic then
  begin
    logbase := Chart.Axes.Bottom.LogarithmicBase;
    Result := Power(logbase, (logN(logbase, Chart.BottomAxis.Maximum) - //
      logN(logbase, Chart.BottomAxis.Minimum)) //
      / (Chart.ChartRect.Right - Chart.ChartRect.Left) //
      * (X - Chart.ChartRect.Left) //
      + logN(logbase, Chart.BottomAxis.Minimum));
  end
  else
    Result := (Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum) //
      / (Chart.ChartRect.Right - Chart.ChartRect.Left) //
      * (X - Chart.ChartRect.Left) //
      + Chart.BottomAxis.Minimum;
end;

function TFrameWChart.GetChartY(Y: Double): Double;
var
  logbase: Double;
begin
  if Chart.Axes.Left.Logarithmic then
  begin
    logbase := Chart.Axes.Left.LogarithmicBase;
    Result := Power(logbase, logN(logbase, Chart.LeftAxis.Maximum) //
      - (logN(logbase, Chart.LeftAxis.Maximum) - //
      logN(logbase, Chart.LeftAxis.Minimum)) //
      / (Chart.ChartRect.Bottom - Chart.ChartRect.Top) //
      * (Y - Chart.ChartRect.Top));
  end
  else
    Result := Chart.LeftAxis.Maximum //
      - (Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum) //
      / (Chart.ChartRect.Bottom - Chart.ChartRect.Top) //
      * (Y - Chart.ChartRect.Top);
end;

function TFrameWChart.GetPosition(point: TPoint): TPoint;
begin
  Result.X := Round(GetX(point.X));
  Result.Y := Round(GetX(point.Y));
end;

function TFrameWChart.GetX(X: Double): Double;
var
  logbase: Double;
begin
  try
    if Chart.Axes.Bottom.Logarithmic then
    begin
      logbase := Chart.Axes.Bottom.LogarithmicBase;
      Result := (logN(logbase, X) - logN(logbase, Chart.BottomAxis.Minimum)) //
        / (logN(logbase, Chart.BottomAxis.Maximum) - //
        logN(logbase, Chart.BottomAxis.Minimum)) //
        * (Chart.ChartRect.Right - Chart.ChartRect.Left) //
        + Chart.ChartRect.Left;
    end
    else
      Result := (X - Chart.BottomAxis.Minimum) //
        / (Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum) //
        * (Chart.ChartRect.Right - Chart.ChartRect.Left) //
        + Chart.ChartRect.Left;
  except
    raise Exception.Create('GetX Error'#13#10 + 'X - Chart.BottomAxis.Minimum: '
      //
      + FloatToStr(X - Chart.BottomAxis.Minimum) + #13#10 //
      + 'Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum: ' //
      + FloatToStr(Chart.BottomAxis.Maximum - Chart.BottomAxis.Minimum) + #13#10
      //
      + 'Chart.ChartRect.Right - Chart.ChartRect.Left: ' //
      + FloatToStr(Chart.ChartRect.Right - Chart.ChartRect.Left) + #13#10 + //
      'Chart.ChartRect.Left: ' //
      + FloatToStr(Chart.ChartRect.Left) + #13#10);
  end;
end;

function TFrameWChart.GetY(Y: Double): Double;
var
  logbase: Double;
begin
  try
    if Chart.Axes.Left.Logarithmic then
    begin
      logbase := Chart.Axes.Left.LogarithmicBase;
      Result := (logN(logbase, Chart.LeftAxis.Maximum) - logN(logbase, Y)) //
        / (logN(logbase, Chart.LeftAxis.Maximum) - //
        logN(logbase, Chart.LeftAxis.Minimum)) //
        * (Chart.ChartRect.Bottom - Chart.ChartRect.Top) //
        + Chart.ChartRect.Top;
    end
    else
      Result := (Chart.LeftAxis.Maximum - Y) //
        / (Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum) //
        * (Chart.ChartRect.Bottom - Chart.ChartRect.Top) //
        + Chart.ChartRect.Top;
  except
    raise Exception.Create('GetX Error'#13#10 + 'Chart.LeftAxis.Maximum - Y: '
      //
      + FloatToStr(Chart.LeftAxis.Maximum - Y) + #13#10 //
      + 'Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum: ' //
      + FloatToStr(Chart.LeftAxis.Maximum - Chart.LeftAxis.Minimum) + #13#10 //
      + 'Chart.ChartRect.Bottom - Chart.ChartRect.Top: ' //
      + FloatToStr(Chart.ChartRect.Bottom - Chart.ChartRect.Top) + #13#10 + //
      'Chart.ChartRect.Top: ' //
      + FloatToStr(Chart.ChartRect.Top) + #13#10);
  end;
end;

procedure TFrameWChart.RecordAxisMenuClick(Sender: TObject);
begin
  FXMin := Chart.BottomAxis.Minimum;
  FXMax := Chart.BottomAxis.Maximum;
  FYMin := Chart.LeftAxis.Minimum;
  FYMax := Chart.LeftAxis.Maximum;
end;

procedure TFrameWChart.RestoreAxisMenuClick(Sender: TObject);
begin
  Chart.BottomAxis.SetMinMax(FXMin, FXMax);
  Chart.LeftAxis.SetMinMax(FYMin, FYMax);
end;

procedure TFrameWChart.SetXLog(const Value: Boolean);
begin
  FXLog := Value;
  Chart.BottomAxis.Logarithmic := FXLog;
end;

procedure TFrameWChart.SetYLog(const Value: Boolean);
begin
  FYLog := Value;
  Chart.LeftAxis.Logarithmic := FYLog;
end;

procedure TFrameWChart.SetXMax(const Value: Double);
begin
  FXMax := Value;
  Chart.BottomAxis.Maximum := FXMax;
end;

procedure TFrameWChart.SetYMax(const Value: Double);
begin
  FYMax := Value;
  Chart.LeftAxis.Maximum := FYMax;
end;

procedure TFrameWChart.SetXMin(const Value: Double);
begin
  FXMin := Value;
  Chart.BottomAxis.Minimum := FXMin;
end;

procedure TFrameWChart.SetYMin(const Value: Double);
begin
  FYMin := Value;
  Chart.LeftAxis.Minimum := FYMin;
end;

procedure TFrameWChart.SymmetryYAxis(Sender: TObject);
var
  max, min: Double;
begin
  max := Chart.MaxYValue(Chart.LeftAxis);
  min := Chart.MinYValue(Chart.LeftAxis);
  if Abs(max) > Abs(min) then
    max := Abs(max)
  else
    max := Abs(min);
  Chart.LeftAxis.SetMinMax(-max, max);
end;

procedure TFrameWChart.XLogMenuClick(Sender: TObject);
begin
  FXLog := Chart.BottomAxis.Logarithmic;
  FXLog := not FXLog;
  Chart.BottomAxis.Logarithmic := FXLog;
end;

procedure TFrameWChart.YLogMenuClick(Sender: TObject);
begin
  FYLog := Chart.LeftAxis.Logarithmic;
  FYLog := not FYLog;
  Chart.LeftAxis.Logarithmic := FYLog;
end;

procedure TFrameWChart.ZoomXMenuClick(Sender: TObject);
begin
  ZoomXMenu.IsChecked := not ZoomXMenu.IsChecked;
  Chart.Zoom.Allow := True;
  if ZoomXMenu.IsChecked and ZoomYMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdBoth
  else if ZoomXMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdHorizontal
  else if ZoomYMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdVertical
  else
    Chart.Zoom.Allow := False;
end;

procedure TFrameWChart.ZoomYMenuClick(Sender: TObject);
begin
  ZoomYMenu.IsChecked := not ZoomYMenu.IsChecked;
  Chart.Zoom.Allow := True;
  if ZoomXMenu.IsChecked and ZoomYMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdBoth
  else if ZoomXMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdHorizontal
  else if ZoomYMenu.IsChecked then
    Chart.Zoom.Direction := TTeeZoomDirection.tzdVertical
  else
    Chart.Zoom.Allow := False;
end;

end.
