unit ElasticAPM4D.Config;

interface

uses
  System.SyncObjs;

type
  TIgnoreUnitsStackTrace = (iustVCL, iustSystem, iustWinapi, iustREST, iustData, iustFireDAC, iustWeb, iustVCLTee, iustXml,
    iustDatasnap);

  TIgnoreUnitsStackTraceSet = set of TIgnoreUnitsStackTrace;

  TConfig = class
  strict private
    class var FDatabase: string;
    class var FDatabaseUser: string;
    class var FDatabaseInstance: string;
    class var FAppName: string;
    class var FAppVersion: string;
    class var FUrlElasticAPM: string;
    class var FIsActive: Boolean;
    class var FUserId: string;
    class var FUserName: string;
    class var FUserMail: string;
    class var FUpdateTime: Integer;
    class var FMaxJsonPerThread: Integer;
    class var FIgnoreUnitsStackTraceSet: TIgnoreUnitsStackTraceSet;
    class var FIgnoreUnitsWasChanged: Boolean;
    class var FOutputFileDir: string;
  private
    class var FSession: TCriticalSection;
  public
    class function GetAppName: string; static;
    class function GetAppVersion: string; static;
    class function GetDatabase: string;
    class function GetDatabaseUser: string; static;
    class function GetDatabaseInstance: string; static;
    class function GetActive: Boolean; static;
    class function GetUrlElasticAPM: string; static;
    class function GetUserId: string; static;
    class function GetUserMail: string; static;
    class function GetUserName: string; static;
    class function GetUpdateTime: Integer; static;
    class function GetMaxJsonPerThread: Integer; static;
    class function GetIgnoreUnitsStackTraceSet: TIgnoreUnitsStackTraceSet; static;
    class function GetLogOutputFilePath: string; static;

    class procedure SetAppName(const Value: string); static;
    class procedure SetAppVersion(const Value: string); static;
    class procedure SetDatabase(const Value: string);
    class procedure SetDatabaseUser(const Value: string); static;
    class procedure SetDatabaseInstance(const Value: string); static;
    class procedure SetActive(const Value: Boolean); static;
    class procedure SetUrlElasticAPM(const Value: string); static;
    class procedure SetUserId(const Value: string); static;
    class procedure SetUserMail(const Value: string); static;
    class procedure SetUserName(const Value: string); static;
    class procedure SetUpdateTime(const Value: Integer); static;
    class procedure SetMaxJsonPerThread(const Value: Integer); static;
    class procedure SetIgnoreUnitsStackTraceSet(const AIgnoreUnits: TIgnoreUnitsStackTraceSet); static;
    class procedure SetLogOutputFilePath(const Value: string); static;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Windows, Vcl.Forms, {$ENDIF} System.IOUtils, System.SysUtils, System.DateUtils;

{ TConfig }

class function TConfig.GetAppName: string;
begin
  FSession.Enter;
  try
{$IFDEF MSWINDOWS}
    if FAppName.IsEmpty then
      FAppName := TPath.GetFileNameWithoutExtension(Application.ExeName);
{$ENDIF}
    Result := FAppName;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetAppVersion: string;
{$IFDEF MSWINDOWS}
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
{$ENDIF}
begin
  FSession.Enter;
  try
    if FAppVersion.IsEmpty then
    begin
{$IFDEF MSWINDOWS}
      Exe := ParamStr(0);
      Size := GetFileVersionInfoSize(PChar(Exe), Handle);
      if Size = 0 then
      begin
        FAppVersion := '0';
        Exit;
      end;
      SetLength(Buffer, Size);
      if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) or not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
        RaiseLastOSError;
      FAppVersion := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi, LongRec(FixedPtr.dwFileVersionMS).Lo,
        LongRec(FixedPtr.dwFileVersionLS).Hi, LongRec(FixedPtr.dwFileVersionLS).Lo]);
{$ENDIF}
    end;
    Result := FAppVersion;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetDatabase: string;
begin
  FSession.Enter;
  try
    Result := FDatabase;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetDatabaseInstance: string;
begin
  FSession.Enter;
  try
    Result := FDatabaseInstance;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetDatabaseUser: string;
begin
  FSession.Enter;
  try
    Result := FDatabaseUser;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetIgnoreUnitsStackTraceSet: TIgnoreUnitsStackTraceSet;
begin
  FSession.Enter;
  try
    if not FIgnoreUnitsWasChanged then
      Exit([]);
    Result := FIgnoreUnitsStackTraceSet;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetLogOutputFilePath: string;
begin
  FSession.Enter;
  try
    Result := FOutputFileDir;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetActive: Boolean;
begin
  FSession.Enter;
  try
    Result := FIsActive;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetMaxJsonPerThread: Integer;
begin
  FSession.Enter;
  try
    if FMaxJsonPerThread = 0 then
      FMaxJsonPerThread := 60;
    Result := FMaxJsonPerThread;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetUpdateTime: Integer;
begin
  FSession.Enter;
  try
    if FUpdateTime = 0 then
      FUpdateTime := 60000;
    Result := FUpdateTime;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetUrlElasticAPM: string;
begin
  FSession.Enter;
  try
    if FUrlElasticAPM.IsEmpty then
      FUrlElasticAPM := 'http://127.0.0.1:8200/intake/v2/events';
    Result := FUrlElasticAPM;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetUserId: string;
begin
  FSession.Enter;
  try
    Result := FUserId;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetUserMail: string;
begin
  FSession.Enter;
  try
    Result := FUserMail;
  finally
    FSession.Release;
  end;
end;

class function TConfig.GetUserName: string;
begin
  FSession.Enter;
  try
    Result := FUserName;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetAppName(const Value: string);
begin
  FSession.Enter;
  try
    FAppName := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetAppVersion(const Value: string);
begin
  FSession.Enter;
  try
    FAppVersion := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetDatabase(const Value: string);
begin
  FSession.Enter;
  try
    FDatabase := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetDatabaseInstance(const Value: string);
begin
  FSession.Enter;
  try
    FDatabaseInstance := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetDatabaseUser(const Value: string);
begin
  FSession.Enter;
  try
    FDatabaseUser := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetIgnoreUnitsStackTraceSet(const AIgnoreUnits: TIgnoreUnitsStackTraceSet);
begin
  FSession.Enter;
  try
    FIgnoreUnitsStackTraceSet := AIgnoreUnits;
    FIgnoreUnitsWasChanged := True;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetLogOutputFilePath(const Value: string);
begin
  FSession.Enter;
  try
    FOutputFileDir := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetActive(const Value: Boolean);
begin
  FSession.Enter;
  try
    FIsActive := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetMaxJsonPerThread(const Value: Integer);
begin
  FSession.Enter;
  try
    if Value >= 5 then
      FMaxJsonPerThread := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetUpdateTime(const Value: Integer);
begin
  FSession.Enter;
  try
    if Value >= 5000 then
      FUpdateTime := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetUrlElasticAPM(const Value: string);
begin
  FSession.Enter;
  try
    FUrlElasticAPM := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetUserId(const Value: string);
begin
  FSession.Enter;
  try
    FUserId := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetUserMail(const Value: string);
begin
  FSession.Enter;
  try
    FUserMail := Value;
  finally
    FSession.Release;
  end;
end;

class procedure TConfig.SetUserName(const Value: string);
begin
  FSession.Enter;
  try
    FUserName := Value;
  finally
    FSession.Release;
  end;
end;

initialization

TConfig.FSession := TCriticalSection.Create;
TConfig.SetActive(True);

finalization

TConfig.SetActive(False);
TConfig.FSession.Free;

end.
