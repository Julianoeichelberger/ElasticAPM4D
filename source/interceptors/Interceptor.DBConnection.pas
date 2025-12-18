unit Interceptor.DBConnection;

interface

uses
  System.Classes, Data.DB, Interceptor.Base, System.Rtti;

type
  TAPMConnectionDBInterceptor<T: TCustomConnection> = class(TAPMInterceptor<T>)
  private
    FConnection: TCustomConnection;    
    FOnRestored: TNotifyEvent;
    FOnLost: TNotifyEvent;
    FOnAfterRollback: TNotifyEvent;
    FOnAfterDisconnect: TNotifyEvent;    
    procedure SetEventIfExists(const AEventName: string; var AOrigin: TNotifyEvent; AReplace: TNotifyEvent); 
  protected
    procedure DoRestored(Sender: TObject); virtual;
    procedure DoLost(Sender: TObject); virtual;
    procedure DoAfterRollback(Sender: TObject); virtual;
    procedure DoAfterDisconnect(Sender: TObject); virtual; 
  public 
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); override; 
    function GetInterceptedType: TClass; override;
  end;

implementation

Uses
  ElasticAPM4D, SysUtils;

function TAPMConnectionDBInterceptor<T>.GetInterceptedType: TClass;
begin 
  Result := T;
end;

procedure TAPMConnectionDBInterceptor<T>.Load(AOwner: TComponent; AInterceptControl: TComponent);
begin
  FOwner := AOwner;
  FControl := AInterceptControl as T;
  FConnection := FControl as TCustomConnection;

  FOnAfterDisconnect := FConnection.AfterDisconnect;
  FConnection.AfterDisconnect := DoAfterDisconnect;

  SetEventIfExists('OnRestored', FOnRestored, DoRestored);
  SetEventIfExists('OnLost', FOnLost, DoLost);
  SetEventIfExists('OnAfterRollback', FOnAfterRollback, DoAfterRollback); 
end;

procedure TAPMConnectionDBInterceptor<T>.DoRestored(Sender: TObject);
begin
  if Assigned(FOnRestored) then
    FOnRestored(Sender);
   TApm.LogWarning('Connection restored');
end;

procedure TAPMConnectionDBInterceptor<T>.DoLost(Sender: TObject);
begin
  if Assigned(FOnLost) then
    FOnLost(Sender);
   TApm.LogWarning('Connection lost');
end;

procedure TAPMConnectionDBInterceptor<T>.DoAfterRollback(Sender: TObject);
begin
  if Assigned(FOnAfterRollback) then
    FOnAfterRollback(Sender);
  TApm.LogWarning('Transaction rolled back');
end;

procedure TAPMConnectionDBInterceptor<T>.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FOnAfterDisconnect) then
    FOnAfterDisconnect(Sender);
  TApm.LogWarning('Connection disconnected');
end;

procedure TAPMConnectionDBInterceptor<T>.SetEventIfExists(const AEventName: string; var AOrigin: TNotifyEvent; AReplace: TNotifyEvent);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  EventValue: TValue;
  MethodPtr: TMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(FConnection.ClassType);
    if Assigned(RttiType) then
    begin
      RttiProperty := RttiType.GetProperty(AEventName);
      if Assigned(RttiProperty) and (RttiProperty.PropertyType.TypeKind = tkMethod) then
      begin
        try
          EventValue := RttiProperty.GetValue(FConnection);
          EventValue.ExtractRawData(@MethodPtr);
          AOrigin := TNotifyEvent(MethodPtr);
          RttiProperty.SetValue(FConnection, TValue.From<TNotifyEvent>(AReplace));
        except
          // ignore errors
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end; 


end.
