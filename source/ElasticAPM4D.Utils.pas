unit ElasticAPM4D.Utils;

interface

Uses
  System.SysUtils, System.IniFiles;

type
  EElasticAPM4DException = Exception;

  TOutcome = (success, failure, unknown);

  TConfig = class
  private
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
  public
    class function GetAppName: string; static;
    class function GetAppVersion: string; static;
    class function GetDatabase: string;
    class function GetDatabaseUser: string; static;
    class function GetDatabaseInstance: string; static;
    class function GetIsActive: Boolean; static;
    class function GetUrlElasticAPM: string; static;
    class function GetUserId: string; static;
    class function GetUserMail: string; static;
    class function GetUserName: string; static;

    class procedure SetAppName(const Value: string); static;
    class procedure SetAppVersion(const Value: string); static;
    class procedure SetDatabase(const Value: string);
    class procedure SetDatabaseUser(const Value: string); static;
    class procedure SetDatabaseInstance(const Value: string); static;
    class procedure SetIsActive(const Value: Boolean); static;
    class procedure SetUrlElasticAPM(const Value: string); static;
    class procedure SetUserId(const Value: string); static;
    class procedure SetUserMail(const Value: string); static;
    class procedure SetUserName(const Value: string); static;
  end;

  TUUid = class
  private
    class procedure RemoveChars(var AStr: string);
  public
    class function Get64b: string;
    class function Get128b: string;
  end;

  TTimestampEpoch = class
    class function Get(ADate: TDatetime): Int64;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Windows, Vcl.Forms, {$ENDIF} System.IOUtils, System.DateUtils;

{ TUUid }

class function TUUid.Get64b: string;
begin
  Result := Copy(Get128b, 1, 16);
end;

class procedure TUUid.RemoveChars(var AStr: string);
begin
  AStr := AStr.Replace('-', '', [rfReplaceAll]);
  AStr := AStr.Replace('{', '');
  AStr := AStr.Replace('}', '');
end;

class function TUUid.Get128b: string;
var
  Uid: TGuid;
  nResult: HResult;
begin
  Result := '';
  nResult := CreateGuid(Uid);
  if nResult = S_OK then
  begin
    Result := GuidToString(Uid).ToLower;
    RemoveChars(Result);
    Exit;
  end;
end;

{ TTimestampEpoch }

class function TTimestampEpoch.Get(ADate: TDatetime): Int64;
begin
  Result := StrToInt64(FormatFloat('0', DateTimeToUnix(ADate, False)) + FormatDateTime('zzz', ADate) + '000');
end;

{ TConfig }

class function TConfig.GetAppName: string;
begin
{$IFDEF MSWINDOWS}
  if FAppName.IsEmpty then
    FAppName := TPath.GetFileNameWithoutExtension(Application.ExeName);
{$ENDIF}
  Result := FAppName;
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
    if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) or
      not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
      RaiseLastOSError;
    FAppVersion := Format('%d.%d.%d.%d',
      [LongRec(FixedPtr.dwFileVersionMS).Hi, LongRec(FixedPtr.dwFileVersionMS).Lo,
      LongRec(FixedPtr.dwFileVersionLS).Hi, LongRec(FixedPtr.dwFileVersionLS).Lo]);
{$ENDIF}
  end;
  Result := FAppVersion;
end;

class function TConfig.GetDatabase: string;
begin
  Result := FDatabase;
end;

class function TConfig.GetDatabaseInstance: string;
begin
  Result := FDatabaseInstance;
end;

class function TConfig.GetDatabaseUser: string;
begin
  Result := FDatabaseUser;
end;

class function TConfig.GetIsActive: Boolean;
begin
  Result := FIsActive;
end;

class function TConfig.GetUrlElasticAPM: string;
begin
  if FUrlElasticAPM.IsEmpty then
    FUrlElasticAPM := 'http://127.0.0.1:8200/intake/v2/events';
  Result := FUrlElasticAPM;
end;

class function TConfig.GetUserId: string;
begin
  Result := FUserId;
end;

class function TConfig.GetUserMail: string;
begin
  Result := FUserMail;
end;

class function TConfig.GetUserName: string;
begin
  Result := FUserName;
end;

class procedure TConfig.SetAppName(const Value: string);
begin
  FAppName := Value;
end;

class procedure TConfig.SetAppVersion(const Value: string);
begin
  FAppVersion := Value;
end;

class procedure TConfig.SetDatabase(const Value: string);
begin
  FDatabase := Value;
end;

class procedure TConfig.SetDatabaseInstance(const Value: string);
begin
  FDatabaseInstance := Value;
end;

class procedure TConfig.SetDatabaseUser(const Value: string);
begin
  FDatabaseUser := Value;
end;

class procedure TConfig.SetIsActive(const Value: Boolean);
begin
  FIsActive := Value;
end;

class procedure TConfig.SetUrlElasticAPM(const Value: string);
begin
  FUrlElasticAPM := Value;
end;

class procedure TConfig.SetUserId(const Value: string);
begin
  FUserId := Value;
end;

class procedure TConfig.SetUserMail(const Value: string);
begin
  FUserMail := Value;
end;

class procedure TConfig.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

initialization

TConfig.SetIsActive(True);

finalization

TConfig.SetIsActive(False);

end.
