{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Metricset.Defaults;

interface

uses
  System.SysUtils, System.Classes, Apm4D.Metricset;

type
  TMemory = Record
    _Physical: UInt64;
    _Virtual: UInt64;
    _PageFile: UInt64;
    _load: double;
  End;

  TCpuUsage = Record
    SystemCpuPct: double; // System CPU usage [0,1]
    ProcessCpuPct: double; // Process CPU usage [0,1]
  End;

  TMetricSetDefaults = class(TMetricSets)
  public
    constructor Create; override;
    class function Get: string;
  end;

function MemoryUsage: TMemory;
function GetCpuUsage: TCpuUsage;

implementation

{$IFDEF MSWINDOWS}


uses
  System.Win.ComObj, System.Variants, Winapi.psAPI, Winapi.Windows, Apm4D.Metricset.Samples;
{$ENDIF}


function MemoryUsage: TMemory;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(TMemoryStatusEx);
  GlobalMemoryStatusEx(MemStatus);
  with MemStatus do
  begin
    // Store actual byte values, not percentages
    // Use MEMORYSTATUSEX instead of MEMORYSTATUS to support > 4GB
    Result._Physical := ullTotalPhys; // Total physical memory in bytes (UInt64)
    Result._Virtual := ullAvailPhys; // Available physical memory in bytes (UInt64)
    Result._PageFile := ullTotalPageFile; // Total page file in bytes (UInt64)
    Result._load := dwMemoryLoad; // Memory load percentage (0-100)
  end;
end;

var
  LastSystemIdleTime, LastSystemKernelTime, LastSystemUserTime: TFileTime;
  LastProcessKernelTime, LastProcessUserTime: TFileTime;
  LastSampleTime: TDateTime;
  FirstSample: Boolean = True;

function GetCpuUsage: TCpuUsage;
var
  SystemIdleTime, SystemKernelTime, SystemUserTime: TFileTime;
  ProcessCreationTime, ProcessExitTime, ProcessKernelTime, ProcessUserTime: TFileTime;
  SystemIdleDiff, SystemKernelDiff, SystemUserDiff, SystemTotalDiff: Int64;
  ProcessKernelDiff, ProcessUserDiff, ProcessTotalDiff: Int64;
  CurrentTime: TDateTime;
  TimeDiff: Double;
begin
  Result.SystemCpuPct := 0;
  Result.ProcessCpuPct := 0;

  CurrentTime := Now;

  // Get system times
  if not GetSystemTimes(SystemIdleTime, SystemKernelTime, SystemUserTime) then
    Exit;

  // Get process times
  if not GetProcessTimes(GetCurrentProcess, ProcessCreationTime, ProcessExitTime,
    ProcessKernelTime, ProcessUserTime) then
    Exit;

  // First sample - just store values
  if FirstSample then
  begin
    LastSystemIdleTime := SystemIdleTime;
    LastSystemKernelTime := SystemKernelTime;
    LastSystemUserTime := SystemUserTime;
    LastProcessKernelTime := ProcessKernelTime;
    LastProcessUserTime := ProcessUserTime;
    LastSampleTime := CurrentTime;
    FirstSample := False;
    Exit;
  end;

  TimeDiff := (CurrentTime - LastSampleTime) * 24 * 3600; // seconds

  // Avoid division by zero or too short intervals
  if TimeDiff < 0.1 then
    Exit;

  // Calculate system CPU usage
  SystemIdleDiff := Int64(SystemIdleTime) - Int64(LastSystemIdleTime);
  SystemKernelDiff := Int64(SystemKernelTime) - Int64(LastSystemKernelTime);
  SystemUserDiff := Int64(SystemUserTime) - Int64(LastSystemUserTime);
  SystemTotalDiff := SystemKernelDiff + SystemUserDiff;

  if SystemTotalDiff > 0 then
    Result.SystemCpuPct := (SystemTotalDiff - SystemIdleDiff) / SystemTotalDiff;

  // Calculate process CPU usage
  ProcessKernelDiff := Int64(ProcessKernelTime) - Int64(LastProcessKernelTime);
  ProcessUserDiff := Int64(ProcessUserTime) - Int64(LastProcessUserTime);
  ProcessTotalDiff := ProcessKernelDiff + ProcessUserDiff;

  // Convert 100-nanosecond intervals to percentage of time elapsed
  if TimeDiff > 0 then
    Result.ProcessCpuPct := (ProcessTotalDiff / 10000000.0) / TimeDiff;

  // Clamp values to [0,1] range
  if Result.SystemCpuPct < 0 then
    Result.SystemCpuPct := 0;
  if Result.SystemCpuPct > 1 then
    Result.SystemCpuPct := 1;
  if Result.ProcessCpuPct < 0 then
    Result.ProcessCpuPct := 0;
  if Result.ProcessCpuPct > 1 then
    Result.ProcessCpuPct := 1;

  // Store current values for next calculation
  LastSystemIdleTime := SystemIdleTime;
  LastSystemKernelTime := SystemKernelTime;
  LastSystemUserTime := SystemUserTime;
  LastProcessKernelTime := ProcessKernelTime;
  LastProcessUserTime := ProcessUserTime;
  LastSampleTime := CurrentTime;
end;

{ TMetricSetDefaults }

constructor TMetricSetDefaults.Create;
var
  Memery: TMemory;
  CpuUsage: TCpuUsage;
begin
  inherited;
  Memery := MemoryUsage;
  CpuUsage := GetCpuUsage;

  // Elastic APM official metric names - must be in BYTES as per specification:
  // https://github.com/elastic/apm/blob/main/specs/agents/metrics.md
  // • system.memory.total: total usable memory on the system, in bytes
  // • system.memory.actual.free: total available memory on the system, in bytes
  Samples.AddBytesGauge('system.memory.total', Memery._Physical);
  Samples.AddBytesGauge('system.memory.actual.free', Int64(Memery._Virtual));

  // CPU metrics (in range [0,1] representing 0-100%)
  // • system.cpu.total.norm.pct: system CPU usage since last report
  // • system.process.cpu.total.norm.pct: process CPU usage since last report
  // Only add CPU metrics if they have valid values (not first sample)
  if (CpuUsage.SystemCpuPct > 0) or (CpuUsage.ProcessCpuPct > 0) then
  begin
    Samples.AddDecimalGauge('system.cpu.total.norm.pct', CpuUsage.SystemCpuPct);
    Samples.AddDecimalGauge('system.process.cpu.total.norm.pct', CpuUsage.ProcessCpuPct);
  end;
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
