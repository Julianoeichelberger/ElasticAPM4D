{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Settings;

interface

uses
  System.SyncObjs,
  System.Generics.Collections,
{$IFDEF MSWINDOWS}
  Apm4D.Interceptors.Base,
{$ENDIF}
  Apm4D.Settings.Database,
  Apm4D.Settings.User,
  Apm4D.Settings.Application,
  Apm4D.Settings.Elastic,
  Apm4D.Settings.Log;

type
  /// <summary>
  /// It's a singleton class. You can configure global application settings.
  /// </summary>
  TApm4DSettings = class
  private
    class var FLock: TCriticalSection;
    class var FIsActive: Boolean;
    class var FDatabase: TDatabaseSettings;
    class var FUser: TUserSettings;
    class var FApplication: TApplicationSettings;
    class var FElastic: TElasticSettings;
    class var FLog: TLogSettings;
{$IFDEF MSWINDOWS}
    class var FInterceptors: TList<TApm4DInterceptorClass>;
{$ENDIF}
  public
    class function Database: TDatabaseSettings; static;
    class function User: TUserSettings; static;
    class function Application: TApplicationSettings; static;
    class function Elastic: TElasticSettings; static;
    class function Log: TLogSettings; static;

    class procedure Activate;
    class procedure Deactivate;
    class function IsActive: boolean;

    class procedure ReleaseInstance;

{$IFDEF MSWINDOWS}
    class procedure RegisterInterceptor(AClass: TApm4DInterceptorClass);
    class procedure UnregisterInterceptor(AClass: TApm4DInterceptorClass);
    class function GetInterceptors: TList<TApm4DInterceptorClass>;
{$ENDIF}
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Vcl.Forms, System.Win.ComObj, {$ENDIF}
  System.SysUtils, System.DateUtils, System.Variants;

{ TApm4DSettings }

class function TApm4DSettings.Database: TDatabaseSettings;
begin
  FLock.Enter;
  try
    if FDatabase = nil then
      FDatabase := TDatabaseSettings.Create;
    Result := FDatabase;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.User: TUserSettings;
begin
  FLock.Enter;
  try
    if FUser = nil then
      FUser := TUserSettings.Create;
    Result := FUser;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.Application: TApplicationSettings;
begin
  FLock.Enter;
  try
    if FApplication = nil then
      FApplication := TApplicationSettings.Create;
    Result := FApplication;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.Elastic: TElasticSettings;
begin
  FLock.Enter;
  try
    if FElastic = nil then
      FElastic := TElasticSettings.Create;
    Result := FElastic;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.Log: TLogSettings;
begin
  FLock.Enter;
  try
    if FLog = nil then
      FLog := TLogSettings.Create;
    Result := FLog;
  finally
    FLock.Leave;
  end;
end;

class procedure TApm4DSettings.ReleaseInstance;
begin
  FLock.Enter;
  try
    if FLog <> nil then
      FreeAndNil(FLog);
    if FElastic <> nil then
      FreeAndNil(FElastic);
    if FApplication <> nil then
      FreeAndNil(FApplication);
    if FUser <> nil then
      FreeAndNil(FUser);
    if FDatabase <> nil then
      FreeAndNil(FDatabase);
{$IFDEF MSWINDOWS}
    if FInterceptors <> nil then
      FreeAndNil(FInterceptors);
{$ENDIF}
  finally
    FLock.Leave;
  end;
end;

class procedure TApm4DSettings.Activate;
begin
  FLock.Enter;
  try
    FIsActive := True;
  finally
    FLock.Leave;
  end;
end;

class procedure TApm4DSettings.Deactivate;
begin
  FLock.Enter;
  try
    FIsActive := False;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.IsActive: boolean;
begin
  FLock.Enter;
  try
    Result := FIsActive;
  finally
    FLock.Leave;
  end;
end;

{$IFDEF MSWINDOWS}


class procedure TApm4DSettings.RegisterInterceptor(AClass: TApm4DInterceptorClass);
begin
  FLock.Enter;
  try
    if not assigned(FInterceptors) then
      FInterceptors := TList<TApm4DInterceptorClass>.Create;

    if not FInterceptors.Contains(AClass) then
      FInterceptors.Add(AClass);
  finally
    FLock.Leave;
  end;
end;

class procedure TApm4DSettings.UnregisterInterceptor(AClass: TApm4DInterceptorClass);
begin
  FLock.Enter;
  try
    if not assigned(FInterceptors) then
      FInterceptors := TList<TApm4DInterceptorClass>.Create;

    FInterceptors.Remove(AClass);
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.GetInterceptors: TList<TApm4DInterceptorClass>;
begin
  FLock.Enter;
  try
    if not assigned(FInterceptors) then
      FInterceptors := TList<TApm4DInterceptorClass>.Create;

    Result := FInterceptors;
  finally
    FLock.Leave;
  end;
end;

{$ENDIF}

initialization

TApm4DSettings.FLock := TCriticalSection.Create;

finalization

TApm4DSettings.ReleaseInstance;
TApm4DSettings.FLock.Free;

end.
