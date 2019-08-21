unit ElasticAPM4D.Config;

interface

Uses
  IniFiles;

type
  TElasticAPM4DConfigProperties = Record
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

  TElasticAPM4DConfig = class
  strict private
    class var FFile: TIniFile;
    class var FConfigs: TElasticAPM4DConfigProperties;
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

implementation

Uses
  SysUtils,
  IOUtils;

{ TElasticAPM4DConfig }

class function TElasticAPM4DConfig.AppVersion: string;
begin
  Result := FConfigs.AppVersion;
end;

class function TElasticAPM4DConfig.Database: string;
begin
  Result := FConfigs.Database;
end;

class function TElasticAPM4DConfig.Enabled: Boolean;
begin
  Result := FConfigs.Enabled;
end;

class function TElasticAPM4DConfig.ServiceName: string;
begin
  Result := FConfigs.ServiceName;
end;

class function TElasticAPM4DConfig.URL: string;
begin
  Result := FConfigs.URL;
end;

class function TElasticAPM4DConfig.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TDirectory.GetCurrentDirectory) + 'ElasticAPM4D.ini';
end;

class procedure TElasticAPM4DConfig.InitializeFile;
begin
  FFile := TIniFile.Create(GetFileName);

  if not FFile.ValueExists('apm', 'enabled') then
    FFile.WriteString('apm', 'enabled', 'False');

  FConfigs.Enabled := FFile.ReadString('apm', 'enabled', 'False').ToBoolean;
  if not FConfigs.Enabled then
    exit;

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

class procedure TElasticAPM4DConfig.RealeseFile;
begin
  FFile.Free;
end;

end.
