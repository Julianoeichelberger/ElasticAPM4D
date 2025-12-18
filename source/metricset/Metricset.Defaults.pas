unit Metricset.Defaults;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Metricset;

type
  TMemory = Record
    _Physical: double;
    _Virtual: double;
    _PageFile: double;
    _load: double;
  End;

  TMetricSetDefaults = class(TMetricSets)
  public
    constructor Create; override;
    class function Get: string;
  end;

function MemoryUsage: TMemory;

implementation

{$IFDEF MSWINDOWS}

uses
  Winapi.ActiveX, System.Win.ComObj, System.Variants, Winapi.TLHelp32, Winapi.psAPI, Winapi.Windows;
{$ENDIF}


function MemoryUsage: TMemory;
var
  GlobalMemory: TMemoryStatus;
begin
  GlobalMemoryStatus(GlobalMemory);
  with GlobalMemory do
  begin
    Result._Physical := (dwTotalPhys - dwAvailPhys) * 100 / dwTotalPhys;
    Result._Virtual := (dwTotalVirtual - dwAvailVirtual) * 100 / dwTotalVirtual;
    Result._PageFile := (dwTotalPageFile - dwAvailPageFile) * 100 / dwTotalPageFile;
    Result._load := (dwLength - dwMemoryLoad) * 100 / dwLength;
  end;
end;

// function HardDiskSpace: double;
// const
// WbemComputer = 'localhost';
// wbemFlagForwardOnly = $00000020;
// var
// FSWbemLocator: OLEVariant;
// FWMIService: OLEVariant;
// FWbemObjectSet: OLEVariant;
// FWbemObject: OLEVariant;
// oEnum: IEnumvariant;
// iValue: LongWord;
// begin
// FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
// FWMIService := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', '', '');
// FWbemObjectSet := FWMIService.ExecQuery(Format('SELECT * FROM Win32_LogicalDisk Where Caption=%s',
// [QuotedStr('c')]), 'WQL', wbemFlagForwardOnly);
// oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
// if oEnum.Next(1, FWbemObject, iValue) = 0 then
// begin
// Result := (FWbemObject.FreeSpace / 100 * FWbemObject.Size) / 100;
// FWbemObject := Unassigned;
// end;
// end;

{ TMetricSetDefaults }

constructor TMetricSetDefaults.Create;
var
  Memery: TMemory;
begin
  inherited;
  Memery := MemoryUsage;
  Samples.AddPercentageGauge('HardDisk', Memery._Physical);
  Samples.AddPercentageGauge('Ram', Memery._load);
end;

class function TMetricSetDefaults.Get: string;
var
  Metricset: TMetricSetDefaults;
begin
  Metricset := TMetricSetDefaults.Create;
  try
    Result := Metricset.ToJsonString;
  finally
    Metricset.Free;
  end;
end;

end.
