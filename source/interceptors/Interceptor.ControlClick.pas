unit Interceptor.ControlClick;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Interceptor.Base;

type
  TAPMControlClickInterceptor<T: class> = class(TAPMInterceptor<T>)
  private
    FOnClick: TNotifyEvent;
    FExecuting: Boolean;
  protected
    procedure DoOnClick(Sender: TObject); virtual;
  public
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); override;
    function GetInterceptedType: TClass; override;
  end;

  TControlAccess = class(TControl);

implementation

Uses
  ElasticAPM4D;

function TAPMControlClickInterceptor<T>.GetInterceptedType: TClass;
begin
  Result := T;
end;

procedure TAPMControlClickInterceptor<T>.Load(AOwner: TComponent; AInterceptControl: TComponent);
begin
  FOwner := AOwner;
  FControl := AInterceptControl as T;
  FExecuting := False;

  FOnClick := TControlAccess(AInterceptControl).OnClick;
  TControlAccess(AInterceptControl).OnClick := DoOnClick;
end;

procedure TAPMControlClickInterceptor<T>.DoOnClick(Sender: TObject);
var
  IsSpan: Boolean;
begin
  // Proteção contra re-entrada (stack overflow)
  if FExecuting then
    exit;

  FExecuting := True;
  try
    if TApm.IsPaused then
    begin
      if Assigned(FOnClick) then
        FOnClick(Sender);
      Exit;
    end;

    IsSpan := TApm.ExistsTransaction;
    if IsSpan then
      TApm.StartSpan(FOwner.Name + '.' + GetControlName + '.Click', 'UI.Click')
    else
      TApm.StartTransaction(FOwner.Name + '.' + GetControlName + '.Click', 'UI.Click');
    try
      if Assigned(FOnClick) then
        FOnClick(Sender);
    finally
      if IsSpan then
        TApm.EndSpan
      else
        TApm.EndTransaction;
    end;
  finally
    FExecuting := False;
  end;
end;

end.
