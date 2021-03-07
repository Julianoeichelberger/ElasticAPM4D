unit ElasticAPM4D.Utils;

interface

Uses
  System.SysUtils, System.IniFiles;

type
  EElasticAPM4DException = Exception;

  TConfig = class
  strict private
  type
    TProperties = Record
    private
      FEnabled: Boolean;
      FServiceName: string;
      FDatabase: string;
      FURL: string;
      FAppVersion: string;
    public
      property Enabled: Boolean read FEnabled write FEnabled;
      property URL: string read FURL write FURL;
      property ServiceName: string read FServiceName write FServiceName;
      property AppVersion: string read FAppVersion write FAppVersion;
      property Database: string read FDatabase write FDatabase;
    end;
  strict private
    class var FFile: TIniFile;
    class var FConfigs: TProperties;
    class function GetFileName: string;
  public
    class procedure InitializeFile;
    class procedure RealeseFile;

    class function Enabled: Boolean;
    class function URL: string;
    class function ServiceName: string;
    class function Database: string;
    class function AppVersion: string;
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
  System.DateUtils, System.IOUtils;

{ TConfig }

class function TConfig.AppVersion: string;
begin
  Result := FConfigs.AppVersion;
end;

class function TConfig.Database: string;
begin
  Result := FConfigs.Database;
end;

class function TConfig.Enabled: Boolean;
begin
  Result := FConfigs.Enabled;
end;

class function TConfig.ServiceName: string;
begin
  Result := FConfigs.ServiceName;
end;

class function TConfig.URL: string;
begin
  Result := FConfigs.URL;
end;

class function TConfig.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TDirectory.GetCurrentDirectory) + 'ElasticAPM4D.ini';
end;

class procedure TConfig.InitializeFile;
begin
  if GetFileName.Contains('Windows') then
    Exit;
  FFile := TIniFile.Create(GetFileName);

  if not FFile.ValueExists('apm', 'enabled') then
    FFile.WriteString('apm', 'enabled', 'False');

  FConfigs.Enabled := FFile.ReadString('apm', 'enabled', 'False').ToBoolean;
  if not FConfigs.Enabled then
    Exit;

  if not FFile.ValueExists('apm', 'url') then
    FFile.WriteString('apm', 'url', 'http://127.0.0.1:8200/intake/v2/events');

  FConfigs.URL := FFile.ReadString('apm', 'url', '');

  if not FFile.ValueExists('service', 'name') then
    FFile.WriteString('service', 'name', '');

  FConfigs.ServiceName := FFile.ReadString('service', 'name', '');

  if not FFile.ValueExists('service', 'database') then
    FFile.WriteString('service', 'database', '');

  FConfigs.ServiceName := FFile.ReadString('service', 'database', '');

  if not FFile.ValueExists('service', 'appversion') then
    FFile.WriteString('service', 'appversion', '');

  FConfigs.ServiceName := FFile.ReadString('service', 'appversion', '');
end;

class procedure TConfig.RealeseFile;
begin
  if Assigned(FFile) then
    FFile.Free;
end;

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

end.
