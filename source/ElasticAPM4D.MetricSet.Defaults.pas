unit ElasticAPM4D.MetricSet.Defaults;

interface

type
  TDiskInfo = Record
    Usage: Int64;
    FreeSpace: Int64;
  End;

  TElasticAPM4DMetricsetDefaults = class
{$IFDEF MSWINDOWS}
    class function MemoryUsage: Int64;
    class function DiskInfo: TDiskInfo;
{$ENDIF}
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  ActiveX, ComObj, Variants,
  TLHelp32, psAPI, Winapi.Windows, System.SysUtils;
{$ENDIF}

{ TElasticAPM4DMetricsetDefaults }

{$IFDEF MSWINDOWS}
class function TElasticAPM4DMetricsetDefaults.MemoryUsage: Int64;
var
  pmc: PPROCESS_MEMORY_COUNTERS;
  cb: Integer;
begin
  Result := 0;
  cb := SizeOf(TProcessMemoryCounters);
  GetMem(pmc, cb);
  pmc^.cb := cb;
  if GetProcessMemoryInfo(GetCurrentProcess(), pmc, cb) then
    Result := longint(pmc^.WorkingSetSize);
  FreeMem(pmc);
end;

class function TElasticAPM4DMetricsetDefaults.DiskInfo: TDiskInfo;
const
  WbemUser = '';
  WbemPassword = '';
  WbemComputer = 'localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator: OLEVariant;
  FWMIService: OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject: OLEVariant;
  oEnum: IEnumvariant;
  iValue: LongWord;
begin;
  Result.FreeSpace := 0;
  Result.Usage := 0;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', WbemUser, WbemPassword);
  FWbemObjectSet := FWMIService.ExecQuery(Format('SELECT * FROM Win32_LogicalDisk Where Caption=%s',
    [QuotedStr('c')]), 'WQL', wbemFlagForwardOnly);
  oEnum := IUnknown(FWbemObjectSet._NewEnum) as IEnumvariant;
  if oEnum.Next(1, FWbemObject, iValue) = 0 then
  begin
    Result.FreeSpace := FWbemObject.FreeSpace;
    Result.Usage := FWbemObject.Size - Result.FreeSpace;
    FWbemObject := Unassigned;
  end;
end;
{$ENDIF}

end.
