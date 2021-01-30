unit XMLEvent;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, System.Generics.Collections, XMLLeafTypes;

type

  TDoubleEvent = procedure(Sender: TObject; value: Double) of object;
  TDouble1DEvent = procedure(Sender: TObject; value: Double1D) of object;
  TDouble2DEvent = procedure(Sender: TObject; value: Double2D) of object;
  TDouble3DEvent = procedure(Sender: TObject; value: Double3D) of object;

  TIntegerEvent = procedure(Sender: TObject; value: Integer) of object;
  TInteger1DEvent = procedure(Sender: TObject; value: Integer1D) of object;
  TInteger2DEvent = procedure(Sender: TObject; value: Integer2D) of object;
  TInteger3DEvent = procedure(Sender: TObject; value: Integer3D) of object;

  TIndexBooleanEvent = procedure(index: Integer; value: Boolean) of object;
  TBooleanEvent = procedure(Sender: TObject; value: Boolean) of object;
  TBoolean1DEvent = procedure(Sender: TObject; value: Boolean1D) of object;
  TBoolean2DEvent = procedure(Sender: TObject; value: Boolean2D) of object;
  TBoolean3DEvent = procedure(Sender: TObject; value: Boolean3D) of object;

implementation

end.
