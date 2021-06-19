unit Metadata.System;

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
  end;

implementation

uses
{$IFDEF MSWINDOWS} TLHelp32, psAPI, Winapi.Windows, Vcl.Forms, {$ENDIF}
{$IFDEF UNIX} Unix, {$ENDIF}
  System.SysUtils;

{ TSystem }

constructor TSystem.Create;
const
  ARQHITECTURE: array [TOSVersion.TArchitecture] of string = ('IntelX86', 'IntelX64', 'ARM32', 'ARM64');
begin
  FArchitecture := ARQHITECTURE[TOSVersion.Architecture];
  FHostname := GetHostNameInOS;
  FPlatform := TOSVersion.ToString;
end;

function TSystem.GetHostNameInOS: string;
{$IFDEF MSWINDOWS}
var
  l: DWORD;
{$ENDIF}
begin
{$IFDEF UNIX}
  Result := Unix.GetHostName;
{$ENDIF}
{$IFDEF MSWINDOWS}
  l := 255;
  SetLength(Result, l);
  GetComputerName(PChar(Result), l);
  SetLength(Result, l);
{$ENDIF}
end;

end.
