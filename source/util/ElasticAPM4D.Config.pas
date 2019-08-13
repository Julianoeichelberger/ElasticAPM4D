unit ElasticAPM4D.Config;

interface

Uses
  IniFiles;

type
  TElasticAPM4DConfig = class
  strict private
    class var FFile: TIniFile;
    class function GetFileName: string;
  public
    class procedure InitializeFile;
    class procedure RealeseFile;

    class function Enabled: Boolean;
    class function URL: string;
    class function ServiceName: string;

  end;

implementation

Uses
  SysUtils,
  IOUtils;

{ TElasticAPM4DConfig }

class function TElasticAPM4DConfig.Enabled: Boolean;
begin
  if not FFile.ValueExists('main', 'enabled') then
    FFile.WriteString('main', 'enabled', 'False');

  Result := FFile.ReadBool('main', 'enabled', False);
end;

class function TElasticAPM4DConfig.GetFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(TDirectory.GetCurrentDirectory) + 'ElasticAPM4D.ini';
end;

class procedure TElasticAPM4DConfig.InitializeFile;
begin
  FFile := TIniFile.Create(GetFileName);
end;

class procedure TElasticAPM4DConfig.RealeseFile;
begin
  FFile.Free;
end;

class function TElasticAPM4DConfig.ServiceName: string;
begin
  if not FFile.ValueExists('main', 'service') then
    FFile.WriteString('main', 'service', '');

  Result := FFile.ReadString('main', 'url', '');
end;

class function TElasticAPM4DConfig.URL: string;
begin
  if not FFile.ValueExists('main', 'url') then
    FFile.WriteString('main', 'url', 'http://127.0.0.1:8200/intake/v2/events');

  Result := FFile.ReadString('main', 'url', '');
end;

initialization

TElasticAPM4DConfig.InitializeFile;

finalization

TElasticAPM4DConfig.RealeseFile;

end.
