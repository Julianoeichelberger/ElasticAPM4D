unit ElasticAPM4D.System;

interface

type
  TElasticAPM4DSystem = class
  private
    FArchitecture: String;
    FHostname: String;
    FPlatform: String;
    function GetHostName: string;
    function GetWindowsArquitecture: string;
  public
    constructor Create;

    property Architecture: String read FArchitecture;
    property Hostname: String read FHostname;
    property &Platform: String read FPlatform;
  end;

implementation

Uses JclSysInfo, Windows, SysUtils;

{ TElasticAPM4DSystem }

function TElasticAPM4DSystem.GetHostName: string;
var
  buffer: array [0 .. MAX_COMPUTERNAME_LENGTH + 1] of Char;
  Size: Cardinal;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  Windows.GetComputerName(@buffer, Size);
  Result := StrPas(buffer);
end;

function TElasticAPM4DSystem.GetWindowsArquitecture: string;
begin
  Result := 'x86';
  if IsWindows64 then
    Result := 'x64';
end;

constructor TElasticAPM4DSystem.Create;
begin
  FArchitecture := GetWindowsArquitecture;
  FHostname := GetHostName;
  FPlatform := GetWindowsVersionString;
end;

end.
