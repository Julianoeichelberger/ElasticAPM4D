unit Apm4D.Interceptor.Actions;

interface

uses
  System.Variants, Generics.Collections, Vcl.Controls, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptor;

type
  TApm4DInterceptActions = class(TApm4DInterceptor)
  private
    FOnExecute: TNotifyEvent;
    FExecuting: Boolean;
  protected
    procedure DoOnExecute(Sender: TObject); virtual;
  public
    constructor Create(AOwner, AInterceptControl: TComponent); override;
    class function IsCompatible(AComponent: TComponent): Boolean; override;
  end;

implementation

uses
  Apm4D;

{ TApm4DInterceptActions }

class function TApm4DInterceptActions.IsCompatible(AComponent: TComponent): Boolean;
begin
  Result := AComponent is TAction;
end;

constructor TApm4DInterceptActions.Create(AOwner, AInterceptControl: TComponent);
begin
  inherited;
  FExecuting := False;

  FOnExecute := TAction(AInterceptControl).OnExecute;
  TAction(AInterceptControl).OnExecute := DoOnExecute;
end;

procedure TApm4DInterceptActions.DoOnExecute(Sender: TObject);
var
  IsSpan: Boolean;
begin
  // to avoid stack overflow
  if FExecuting then
    exit;

  FExecuting := True;
  try
    if TApm4D.IsPaused then
    begin
      if Assigned(FOnExecute) then
        FOnExecute(Sender);
      Exit;
    end;

    IsSpan := TApm4D.ExistsTransaction;
    if IsSpan then
      TApm4D.StartSpan(FOwner.Name + '.' + GetControlName(FControl) + '.Execute', 'UI')
    else
      TApm4D.StartTransaction(FOwner.Name + '.' + GetControlName(FControl) + '.Execute', 'UI');
    try
      if Assigned(FOnExecute) then
        FOnExecute(Sender);
    finally
      if IsSpan then
        TApm4D.EndSpan
      else
        TApm4D.EndTransaction;
    end;
  finally
    FExecuting := False;
  end;
end;

end.

