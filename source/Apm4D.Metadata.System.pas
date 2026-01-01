{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.System;

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

uses
{$IFDEF MSWINDOWS} Winapi.psAPI, Winapi.Windows, Vcl.Forms, {$ENDIF}
{$IFDEF UNIX} Posix.Unistd, {$ENDIF}
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
{$IFDEF UNIX}
var
  HostName: array [0 .. 255] of AnsiChar;
{$ENDIF}
begin
{$IFDEF UNIX}
  if gethostname(HostName, SizeOf(HostName)) = 0 then
    Result := string(HostName)
  else
    Result := 'unknown';
{$ENDIF}
{$IFDEF MSWINDOWS}
  l := 255;
  SetLength(Result, l);
  GetComputerName(PChar(Result), l);
  SetLength(Result, l);
{$ENDIF}
end;

end.
