{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Interceptors.DataSet;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptors.Base, System.Rtti, System.TypInfo;

type
  TAPMDataSetInterceptor = class(TApm4DInterceptor<TDataSet>)
  private
    FEvents: TDictionary<string, TDataSetNotifyEvent>;
    procedure SetEventIfExists(const AEventName: string; AReplace: TDataSetNotifyEvent);
    function GetEvents: TDictionary<string, TDataSetNotifyEvent>;
    function GetOwnerName: string;
    procedure ExecuteOriginEvent(AName: string; DataSet: TDataSet);
  protected
    procedure DoAfterOpen(DataSet: TDataSet); virtual;
    procedure DoAfterDelete(DataSet: TDataSet); virtual;
    procedure DoAfterExecute(DataSet: TDataSet); virtual;
    procedure DoAfterInsert(DataSet: TDataSet); virtual;
    procedure DoAfterEdit(DataSet: TDataSet); virtual;

    procedure DoBeforeOpen(DataSet: TDataSet); virtual;
    procedure DoBeforeDelete(DataSet: TDataSet); virtual;
    procedure DoBeforeExecute(DataSet: TDataSet); virtual;
    procedure DoBeforeInsert(DataSet: TDataSet); virtual;
    procedure DoBeforeEdit(DataSet: TDataSet); virtual;

    property Events: TDictionary<string, TDataSetNotifyEvent> read GetEvents;
  public
    destructor Destroy; override;

    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); override;

    function GetInterceptedType: TClass; override;
  end;

implementation

uses
  Apm4D, SysUtils;

destructor TAPMDataSetInterceptor.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TAPMDataSetInterceptor.GetEvents: TDictionary<string, TDataSetNotifyEvent>;
begin
  if not assigned(FEvents) then
    FEvents := TDictionary<string, TDataSetNotifyEvent>.Create;
  Result := FEvents;
end;

function TAPMDataSetInterceptor.GetInterceptedType: TClass;
begin
  Result := TDataSet;
end;

function TAPMDataSetInterceptor.GetOwnerName: string;
var
  Context: TRttiContext;
begin
  // Result := 'Unknowd';
  if Assigned(FOwner) and (FOwner.Name <> '') then
    exit(FOwner.Name);

  Result := Context.GetType(FOwner.ClassType).Name;
end;

procedure TAPMDataSetInterceptor.SetEventIfExists(const AEventName: string; AReplace: TDataSetNotifyEvent);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  OriginalEvent: TDataSetNotifyEvent;
  Value: TValue;
  Method: TMethod;
  ReplaceMethod: TMethod;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(FControl.ClassInfo);
    if Assigned(RttiType) then
    begin
      RttiProperty := RttiType.GetProperty(AEventName);
      if Assigned(RttiProperty) and (RttiProperty.PropertyType.TypeKind = tkMethod) then
      begin
        try
          OriginalEvent := nil;
          Value := RttiProperty.GetValue(FControl);
          if not Value.IsEmpty then
          begin
            Method := Value.AsType<TMethod>;
            OriginalEvent := TDataSetNotifyEvent(Method);
          end;
          Events.Add(AEventName, OriginalEvent);

          ReplaceMethod.Code := TMethod(AReplace).Code;
          ReplaceMethod.Data := TMethod(AReplace).Data;
          TValue.Make(@ReplaceMethod, RttiProperty.PropertyType.Handle, Value);
          RttiProperty.SetValue(FControl, Value);
        except
          // ignore errors
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

procedure TAPMDataSetInterceptor.Load(AOwner: TComponent; AInterceptControl: TComponent);
begin
  FOwner := AOwner;
  FControl := AInterceptControl as TDataSet;

  Events.Add('AfterOpen', FControl.AfterOpen);
  Events.Add('BeforeOpen', FControl.BeforeOpen);
  Events.Add('AfterDelete', FControl.AfterDelete);
  Events.Add('BeforeDelete', FControl.BeforeDelete);
  Events.Add('AfterInsert', FControl.AfterInsert);
  Events.Add('BeforeInsert', FControl.BeforeInsert);
  Events.Add('AfterEdit', FControl.AfterEdit);
  Events.Add('BeforeEdit', FControl.BeforeEdit);

  FControl.AfterOpen := DoAfterOpen;
  FControl.BeforeOpen := DoBeforeOpen;
  FControl.AfterDelete := DoAfterDelete;
  FControl.BeforeDelete := DoBeforeDelete;
  FControl.AfterInsert := DoAfterInsert;
  FControl.BeforeInsert := DoBeforeInsert;
  FControl.AfterEdit := DoAfterEdit;
  FControl.BeforeEdit := DoBeforeEdit;

  SetEventIfExists('AfterExecute', DoAfterExecute);
  SetEventIfExists('BeforeExecute', DoBeforeExecute);
end;

procedure TAPMDataSetInterceptor.ExecuteOriginEvent(AName: string; DataSet: TDataSet);
var
  OriginalEvent: TDataSetNotifyEvent;
begin
  if Events.TryGetValue(AName, OriginalEvent) and assigned(OriginalEvent) then
    OriginalEvent(DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeOpen(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName + '.Open', 'database');

  ExecuteOriginEvent('BeforeOpen', DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeDelete(DataSet: TDataSet); 
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName + '.Delete', 'database');

  ExecuteOriginEvent('BeforeDelete', DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeExecute(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName + '.Execute', 'database');
  ExecuteOriginEvent('BeforeExecute', DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeInsert(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName + '.Insert', 'database');

  ExecuteOriginEvent('BeforeInsert', DataSet); 
end;

procedure TAPMDataSetInterceptor.DoBeforeEdit(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName + '.Edit', 'database');
  ExecuteOriginEvent('BeforeEdit', DataSet);
end;

procedure TAPMDataSetInterceptor.DoAfterOpen(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterOpen', DataSet); 
  TApm4D.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterDelete(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterDelete', DataSet); 
  TApm4D.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterExecute(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterExecute', DataSet);
  TApm4D.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterInsert(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterInsert', DataSet);
  TApm4D.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterEdit(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterEdit', DataSet);
  TApm4D.EndSpan;
end;

end.
