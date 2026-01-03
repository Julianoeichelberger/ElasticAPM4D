{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor.DBConnection;

interface

uses
  System.Classes, Data.DB, Apm4D.Interceptor, System.Rtti;

type
  TApm4DInterceptDBConnection = class(TApm4DInterceptor)
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
    constructor Create(AOwner, AInterceptControl: TComponent); override;
    class function IsCompatible(AComponent: TComponent): Boolean; override;
  end;

implementation

uses
  Apm4D;

class function TApm4DInterceptDBConnection.IsCompatible(AComponent: TComponent): Boolean;
begin
  Result := AComponent is TCustomConnection;
end;

constructor TApm4DInterceptDBConnection.Create(AOwner, AInterceptControl: TComponent);
begin
  inherited;
  
  FConnection := FControl as TCustomConnection;

  FOnAfterDisconnect := FConnection.AfterDisconnect;
  FConnection.AfterDisconnect := DoAfterDisconnect;

  SetEventIfExists('OnRestored', FOnRestored, DoRestored);
  SetEventIfExists('OnLost', FOnLost, DoLost);
  SetEventIfExists('OnAfterRollback', FOnAfterRollback, DoAfterRollback);
end;

procedure TApm4DInterceptDBConnection.DoRestored(Sender: TObject);
begin
  if Assigned(FOnRestored) then
    FOnRestored(Sender);
end;

procedure TApm4DInterceptDBConnection.DoLost(Sender: TObject);
begin
  if Assigned(FOnLost) then
    FOnLost(Sender);
end;

procedure TApm4DInterceptDBConnection.DoAfterRollback(Sender: TObject);
begin
  if Assigned(FOnAfterRollback) then
    FOnAfterRollback(Sender);
end;

procedure TApm4DInterceptDBConnection.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FOnAfterDisconnect) then
    FOnAfterDisconnect(Sender);
end;

procedure TApm4DInterceptDBConnection.SetEventIfExists(const AEventName: string; var AOrigin: TNotifyEvent;
  AReplace: TNotifyEvent);
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
