{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor.DataSet;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptor, System.Rtti, System.TypInfo;

type
  TApm4DInterceptDataSet = class(TApm4DInterceptor)
  private
    FEvents: TDictionary<string, TDataSetNotifyEvent>;
    procedure SetEventIfExists(const AEventName: string; AReplace: TDataSetNotifyEvent);
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
  public
    constructor Create(AOwner, AInterceptControl: TComponent); override;
    destructor Destroy; override;

    class function IsCompatible(AComponent: TComponent): Boolean; override;
  end;

implementation

uses
  Apm4D, SysUtils;

class function TApm4DInterceptDataSet.IsCompatible(AComponent: TComponent): Boolean;
begin
  Result := AComponent is TDataSet;
end;

constructor TApm4DInterceptDataSet.Create(AOwner, AInterceptControl: TComponent);
begin
  inherited;
  FEvents := TDictionary<string, TDataSetNotifyEvent>.Create;

  FEvents.Add('AfterOpen', TDataSet(FControl).AfterOpen);
  FEvents.Add('BeforeOpen', TDataSet(FControl).BeforeOpen);
  FEvents.Add('AfterDelete', TDataSet(FControl).AfterDelete);
  FEvents.Add('BeforeDelete', TDataSet(FControl).BeforeDelete);
  FEvents.Add('AfterInsert', TDataSet(FControl).AfterInsert);
  FEvents.Add('BeforeInsert', TDataSet(FControl).BeforeInsert);
  FEvents.Add('AfterEdit', TDataSet(FControl).AfterEdit);
  FEvents.Add('BeforeEdit', TDataSet(FControl).BeforeEdit);

  TDataSet(FControl).AfterOpen := DoAfterOpen;
  TDataSet(FControl).BeforeOpen := DoBeforeOpen;
  TDataSet(FControl).AfterDelete := DoAfterDelete;
  TDataSet(FControl).BeforeDelete := DoBeforeDelete;
  TDataSet(FControl).AfterInsert := DoAfterInsert;
  TDataSet(FControl).BeforeInsert := DoBeforeInsert;
  TDataSet(FControl).AfterEdit := DoAfterEdit;
  TDataSet(FControl).BeforeEdit := DoBeforeEdit;

  SetEventIfExists('AfterExecute', DoAfterExecute);
  SetEventIfExists('BeforeExecute', DoBeforeExecute);
end;

destructor TApm4DInterceptDataSet.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TApm4DInterceptDataSet.GetOwnerName: string;
var
  Context: TRttiContext;
begin
  // Result := 'Unknowd';
  if Assigned(FOwner) and (FOwner.Name <> '') then
    exit(FOwner.Name);

  Result := Context.GetType(FOwner.ClassType).Name;
end;

procedure TApm4DInterceptDataSet.SetEventIfExists(const AEventName: string; AReplace: TDataSetNotifyEvent);
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
          FEvents.Add(AEventName, OriginalEvent);

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

procedure TApm4DInterceptDataSet.ExecuteOriginEvent(AName: string; DataSet: TDataSet);
var
  OriginalEvent: TDataSetNotifyEvent;
begin
  if FEvents.TryGetValue(AName, OriginalEvent) and assigned(OriginalEvent) then
    OriginalEvent(DataSet);
end;

procedure TApm4DInterceptDataSet.DoBeforeOpen(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName(FControl) + '.Open', 'db');

  ExecuteOriginEvent('BeforeOpen', DataSet);
end;

procedure TApm4DInterceptDataSet.DoBeforeDelete(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName(FControl) + '.Delete', 'db');

  ExecuteOriginEvent('BeforeDelete', DataSet);
end;

procedure TApm4DInterceptDataSet.DoBeforeExecute(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName(FControl) + '.Execute', 'db');
  ExecuteOriginEvent('BeforeExecute', DataSet);
end;

procedure TApm4DInterceptDataSet.DoBeforeInsert(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName(FControl) + '.Insert', 'db');

  ExecuteOriginEvent('BeforeInsert', DataSet);
end;

procedure TApm4DInterceptDataSet.DoBeforeEdit(DataSet: TDataSet);
begin
  if TApm4D.ExistsTransaction and not TApm4D.IsPaused then
    TApm4D.StartSpan(GetOwnerName + '.' + GetControlName(FControl) + '.Edit', 'db');
  ExecuteOriginEvent('BeforeEdit', DataSet);
end;

procedure TApm4DInterceptDataSet.DoAfterOpen(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterOpen', DataSet);
  TApm4D.EndSpan;
end;

procedure TApm4DInterceptDataSet.DoAfterDelete(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterDelete', DataSet);
  TApm4D.EndSpan;
end;

procedure TApm4DInterceptDataSet.DoAfterExecute(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterExecute', DataSet);
  TApm4D.EndSpan;
end;

procedure TApm4DInterceptDataSet.DoAfterInsert(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterInsert', DataSet);
  TApm4D.EndSpan;
end;

procedure TApm4DInterceptDataSet.DoAfterEdit(DataSet: TDataSet);
begin
  ExecuteOriginEvent('AfterEdit', DataSet);
  TApm4D.EndSpan;
end;

end.
