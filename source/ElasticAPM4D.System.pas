unit ElasticAPM4D.System;

interface

type
  TSystem = class
  private
    FArchitecture: String;
    FHostname: String;
    FPlatform: String;
    function GetHostNameInOS: string;
    function GetWindowsArquitecture: string;
    function GetPlatform: String;
  public
    constructor Create;

    property Architecture: String read FArchitecture;
    property Hostname: String read FHostname;
    property &Platform: String read FPlatform;
  end;

implementation

Uses
  // JclSysInfo,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
{$IFDEF UNIX} unix, {$ENDIF}
  System.SysUtils;

{ TElasticAPM4DSystem }

function TSystem.GetHostNameInOS: string;
{$IFDEF MSWINDOWS}
var
  l: DWORD;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := unix.GetHostName;
{$ENDIF}
{$IFDEF MSWINDOWS}
  l := 255;
  SetLength(Result, l);
  GetComputerName(PChar(Result), l);
  SetLength(Result, l);
{$ENDIF}
end;

function TSystem.GetPlatform: String;
begin
  // Result := GetOSVersionString
end;

function TSystem.GetWindowsArquitecture: string;
begin
  Result := '';
{$IFDEF MSWINDOWS}
  Result := 'x86';
  // if IsWindows64 then
  // Result := 'x64';
{$ENDIF}
end;

constructor TSystem.Create;
begin
  FArchitecture := GetWindowsArquitecture;
  FHostname := GetHostNameInOS;
  FPlatform := GetPlatform;
end;

end.
