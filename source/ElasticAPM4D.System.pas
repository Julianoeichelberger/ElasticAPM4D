unit ElasticAPM4D.System;

interface

type
  TSystem = class
  private
    FArchitecture: String;
    FHostname: String;
    FPlatform: String;
    function GetHostNameInOS: string;
  public
    constructor Create;

    property Architecture: String read FArchitecture;
    property Hostname: String read FHostname;
    property &Platform: String read FPlatform;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Windows, {$ENDIF} {$IFDEF UNIX} unix, {$ENDIF} System.SysUtils;

{ TSystem }

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

constructor TSystem.Create;
const
  ARQHITECTURE: array [TOSVersion.TArchitecture] of string = ('IntelX86', 'IntelX64', 'ARM32', 'ARM64');
begin
  FArchitecture := ARQHITECTURE[TOSVersion.Architecture];
  FHostname := GetHostNameInOS;
  FPlatform := TOSVersion.ToString;
end;

end.
