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
  Apm4D.Interceptor,
  Apm4D.Interceptor.DataSet,
  Apm4D.Interceptor.Actions,
  Apm4D.Interceptor.OnClick,
  Apm4D.Interceptor.RESTRequest,
{$ENDIF}
  Apm4D.Metricset.Base,
  Apm4D.Share.Stacktrace,
  Apm4D.Settings.Database,
  Apm4D.Settings.User,
  Apm4D.Settings.Application,
  Apm4D.Settings.Elastic,
  Apm4D.Settings.Log;

type
{$IFDEF MSWINDOWS}
  TApm4DInterceptOnClick = Apm4D.Interceptor.OnClick.TApm4DInterceptOnClick;
  TApm4DInterceptActions = Apm4D.Interceptor.Actions.TApm4DInterceptActions;
  TApm4DInterceptDataSet = Apm4D.Interceptor.DataSet.TApm4DInterceptDataSet;
  TApm4DInterceptRESTRequest = Apm4D.Interceptor.RESTRequest.TApm4DInterceptRESTRequest;
{$ENDIF}
  TApm4DMetricsetClass = Apm4D.Metricset.Base.TApm4DMetricsetClass; 
  
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
    class var FStackTracer: TStackTracerClass;
{$IFDEF MSWINDOWS}
    class var FInterceptors: TDictionary<TApm4DInterceptorClass, TArray<TClass>>;
{$ENDIF}
    class var FMetricsets: TList<TApm4DMetricsetClass>;
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
    class procedure RegisterInterceptor(AInterceptor: TApm4DInterceptorClass; AClasses: TArray<TClass>);
    class function GetInterceptors: TDictionary<TApm4DInterceptorClass, TArray<TClass>>;
{$ENDIF}
    class procedure AddStackTracer(AStackTracer: TStackTracerClass);
    class function CreateStackTracer: TStackTracer;
    /// <summary>
    /// Register a custom metricset class.
    /// </summary>
    class procedure RegisterMetricset(AMetricsetClass: TApm4DMetricsetClass);
    
    /// <summary>
    /// Get all registered metricsets.
    /// </summary>
    class function GetMetricsets: TList<TApm4DMetricsetClass>;
    
    /// <summary>
    /// Clear all registered metricsets.
    /// </summary>
    class procedure ClearMetricsets;
  end;

implementation

Uses
{$IFDEF MSWINDOWS} Vcl.Forms, {$ENDIF}
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

class procedure TApm4DSettings.AddStackTracer(AStackTracer: TStackTracerClass);
begin
  FLock.Enter;
  try
    FStackTracer := AStackTracer;
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.CreateStackTracer: TStackTracer;
begin
  FLock.Enter;
  try
    Result := nil;
    if Assigned(FStackTracer) then
      Result := FStackTracer.Create; 
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
    if FMetricsets <> nil then
      FreeAndNil(FMetricsets);
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


class procedure TApm4DSettings.RegisterInterceptor(AInterceptor: TApm4DInterceptorClass; AClasses: TArray<TClass>);
begin
  FLock.Enter;
  try
    if not assigned(FInterceptors) then
      FInterceptors := TDictionary < TApm4DInterceptorClass, TArray < TClass >>.Create;
    FInterceptors.AddOrSetValue(AInterceptor, AClasses);
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.GetInterceptors: TDictionary<TApm4DInterceptorClass, TArray<TClass>>;
begin
  FLock.Enter;
  try
    if not assigned(FInterceptors) then
      FInterceptors := TDictionary < TApm4DInterceptorClass, TArray < TClass >>.Create;

    Result := FInterceptors;
  finally
    FLock.Leave;
  end;
end;

{$ENDIF}

class procedure TApm4DSettings.RegisterMetricset(AMetricsetClass: TApm4DMetricsetClass);
begin
  FLock.Enter;
  try
    if not Assigned(FMetricsets) then
      FMetricsets := TList<TApm4DMetricsetClass>.Create;
    
    // Avoid duplicates
    if not FMetricsets.Contains(AMetricsetClass) then
      FMetricsets.Add(AMetricsetClass);
  finally
    FLock.Leave;
  end;
end;

class function TApm4DSettings.GetMetricsets: TList<TApm4DMetricsetClass>;
begin
  FLock.Enter;
  try
    if not Assigned(FMetricsets) then
      FMetricsets := TList<TApm4DMetricsetClass>.Create;
    
    Result := FMetricsets;
  finally
    FLock.Leave;
  end;
end;

class procedure TApm4DSettings.ClearMetricsets;
begin
  FLock.Enter;
  try
    if Assigned(FMetricsets) then
      FMetricsets.Clear;
  finally
    FLock.Leave;
  end;
end;

initialization

TApm4DSettings.FLock := TCriticalSection.Create;

finalization

TApm4DSettings.ReleaseInstance;
TApm4DSettings.FLock.Free;

end.
