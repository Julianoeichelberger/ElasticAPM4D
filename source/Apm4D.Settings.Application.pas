unit Apm4D.Settings.Application;

interface

type 
  /// <summary>
  /// Application settings 
  /// </summary>
  TApplicationSettings = class
  strict private 
    FName: string;
    FVersion: string;
    FEnvironment: string; 
  public
    constructor Create; 
    property Name: string read FName write FName;
    property Version: string read FVersion write FVersion;
    property Environment: string read FEnvironment write FEnvironment;
  end;


implementation

Uses
{$IFDEF MSWINDOWS} Winapi.Windows, Vcl.Forms, System.Win.ComObj, {$ENDIF}
System.IOUtils, System.SysUtils, System.DateUtils, System.Variants;

{ TApplicationSettings }

function GetAppVersion: string;
{$IFDEF MSWINDOWS}
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
{$ENDIF}
begin  
  Result := '0.0.0';
{$IFDEF MSWINDOWS}
    Exe := ParamStr(0);
    Size := GetFileVersionInfoSize(PChar(Exe), Handle);
    if Size = 0 then 
       Exit; 

    SetLength(Buffer, Size);
    
    if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) or not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
       RaiseLastOSError;
    
    Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi, LongRec(FixedPtr.dwFileVersionMS).Lo, LongRec(FixedPtr.dwFileVersionLS).Hi, LongRec(FixedPtr.dwFileVersionLS).Lo]);
{$ENDIF} 
end;

constructor TApplicationSettings.Create;
begin
  FName := TPath.GetFileNameWithoutExtension(Application.ExeName);
  FVersion := GetAppVersion;
  FEnvironment := 'staging';
end;

end.