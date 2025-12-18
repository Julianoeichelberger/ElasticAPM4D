unit Interceptor.DataSet;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Interceptor.Base, System.Rtti, System.TypInfo;

type
  TAPMDataSetInterceptor = class(TAPMInterceptor<TDataSet>)
  private
    FEvents: TDictionary<string, TDataSetNotifyEvent>;
    FOnAfterExecute: TDataSetNotifyEvent;
    FOnBeforeExecute: TDataSetNotifyEvent;
    function GetSQLStatement: string;
    procedure SetEventIfExists(const AEventName: string; AOrigin, AReplace: TDataSetNotifyEvent);
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
    constructor Create;
    destructor Destroy; override;
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); override;
    function GetInterceptedType: TClass; override;
  end;

implementation

Uses
  ElasticAPM4D, SysUtils;

destructor TAPMDataSetInterceptor.Destroy;
begin
  FEvents.Free;
  inherited;
end;

constructor TAPMDataSetInterceptor.Create;
begin
  FEvents := TDictionary<string, TDataSetNotifyEvent>.Create;
end;

function TAPMDataSetInterceptor.GetInterceptedType: TClass;
begin
  Result := TDataSet;
end;

function TAPMDataSetInterceptor.GetSQLStatement: string;
const
  // Propriedades comuns que contêm SQL em diferentes implementações de TDataSet
  SQL_PROPERTIES: array [0 .. 4] of string = ('SQL', 'CommandText', 'SelectSQL', 'Query', 'SelectCommand');
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  PropValue: TValue;
  PropName: string;
  SQLText: TStrings;
begin
  Result := '';
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(FControl.ClassType);
    if not Assigned(RttiType) then
      Exit;

    // Tenta encontrar a propriedade SQL
    for PropName in SQL_PROPERTIES do
    begin
      RttiProperty := RttiType.GetProperty(PropName);
      if Assigned(RttiProperty) then
      begin
        PropValue := RttiProperty.GetValue(FControl);

        // Se for TStrings (como SQL.Text)
        if PropValue.IsType<TStrings> then
        begin
          SQLText := PropValue.AsObject as TStrings;
          if Assigned(SQLText) and (SQLText.Count > 0) then
          begin
            Result := String(SQLText.Text).Trim;
            Break;
          end;
        end
        // Se for string direta (como CommandText)
        else if PropValue.IsType<string> then
        begin
          Result := PropValue.AsString.Trim;
          if Result <> '' then
            Break;
        end;
      end;
    end;
  finally
    RttiContext.Free;
  end;
end;

procedure TAPMDataSetInterceptor.SetEventIfExists(const AEventName: string; AOrigin, AReplace: TDataSetNotifyEvent);
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
begin
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(FControl.ClassType);
    if Assigned(RttiType) then
    begin
      RttiProperty := RttiType.GetProperty(AEventName);
      if Assigned(RttiProperty) and (RttiProperty.PropertyType.TypeKind = tkMethod) then
      begin
        try
          RttiProperty.GetValue(FControl).ExtractRawData(@AOrigin);
          RttiProperty.SetValue(FControl, TValue.From<TDataSetNotifyEvent>(AReplace));
          FEvents.Add(AEventName, AOrigin);
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

  FEvents.Add('AfterOpen', FControl.AfterOpen);
  FEvents.Add('BeforeOpen', FControl.BeforeOpen);
  FEvents.Add('AfterDelete', FControl.AfterDelete);
  FEvents.Add('BeforeDelete', FControl.BeforeDelete);
  FEvents.Add('AfterInsert', FControl.AfterInsert);
  FEvents.Add('BeforeInsert', FControl.BeforeInsert);
  FEvents.Add('AfterEdit', FControl.AfterEdit);
  FEvents.Add('BeforeEdit', FControl.BeforeEdit);

  FControl.AfterOpen := DoAfterOpen;
  FControl.BeforeOpen := DoBeforeOpen;
  FControl.AfterDelete := DoAfterDelete;
  FControl.BeforeDelete := DoBeforeDelete;
  FControl.AfterInsert := DoAfterInsert;
  FControl.BeforeInsert := DoBeforeInsert;
  FControl.AfterEdit := DoAfterEdit;
  FControl.BeforeEdit := DoBeforeEdit;

  SetEventIfExists('AfterExecute', FOnAfterExecute, DoAfterExecute);
  SetEventIfExists('BeforeExecute', FOnBeforeExecute, DoBeforeExecute);
end;

procedure TAPMDataSetInterceptor.DoBeforeOpen(DataSet: TDataSet);
var
  SQLStatement: string;
begin
  if TApm.ExistsTransaction and not TApm.isPaused then
  begin
    SQLStatement := GetSQLStatement;
    if SQLStatement <> '' then
      TApm.StartSpanSQL(FOwner.Name + '.' + GetControlName + '.Open', SQLStatement)
    else
      TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Open', 'database');
  end;
  if FEvents.ContainsKey('BeforeOpen') then
    FEvents['BeforeOpen'](DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeDelete(DataSet: TDataSet);
begin
  if TApm.ExistsTransaction and not TApm.isPaused then
    TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Delete', 'database');
  if FEvents.ContainsKey('BeforeDelete') then
    FEvents['BeforeDelete'](DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeExecute(DataSet: TDataSet);
begin
  if TApm.ExistsTransaction and not TApm.isPaused then
    TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Execute', 'database');
  if FEvents.ContainsKey('BeforeExecute') then
    FEvents['BeforeExecute'](DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeInsert(DataSet: TDataSet);
begin
  if TApm.ExistsTransaction and not TApm.isPaused then
    TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Insert', 'database');

  if FEvents.ContainsKey('BeforeInsert') then
    FEvents['BeforeInsert'](DataSet);
end;

procedure TAPMDataSetInterceptor.DoBeforeEdit(DataSet: TDataSet);
begin
  if TApm.ExistsTransaction and not TApm.isPaused then
    TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Edit', 'database');
  if FEvents.ContainsKey('BeforeEdit') then
    FEvents['BeforeEdit'](DataSet);
end;

procedure TAPMDataSetInterceptor.DoAfterOpen(DataSet: TDataSet);
begin
  if FEvents.ContainsKey('AfterOpen') then
    FEvents['AfterOpen'](DataSet);
  TApm.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterDelete(DataSet: TDataSet);
begin
  if FEvents.ContainsKey('AfterDelete') then
    FEvents['AfterDelete'](DataSet);
  TApm.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterExecute(DataSet: TDataSet);
begin
  if FEvents.ContainsKey('AfterExecute') then
    FEvents['AfterExecute'](DataSet);
  TApm.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterInsert(DataSet: TDataSet);
begin
  if FEvents.ContainsKey('AfterInsert') then
    FEvents['AfterInsert'](DataSet);
  TApm.EndSpan;
end;

procedure TAPMDataSetInterceptor.DoAfterEdit(DataSet: TDataSet);
begin
  if FEvents.ContainsKey('AfterEdit') then
    FEvents['AfterEdit'](DataSet);
  TApm.EndSpan;
end;

end.
