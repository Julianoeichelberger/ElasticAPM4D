{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor.OnClick;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptor, Vcl.Menus, Vcl.StdCtrls;

type
  TApm4DInterceptOnClick = class(TApm4DInterceptor)
  private
    FOnClick: TNotifyEvent;
    FExecuting: Boolean;
  protected
    procedure DoOnClick(Sender: TObject); virtual;
  public
    constructor Create(AOwner, AInterceptControl: TComponent); override;
    class function IsCompatible(AComponent: TComponent): Boolean; override;
  end;

  TControlAccess = class(TControl);

implementation

uses
  Apm4D;

{ TApm4DInterceptOnClick }

class function TApm4DInterceptOnClick.IsCompatible(AComponent: TComponent): Boolean;
begin
  Result := AComponent is TControl;
end;

constructor TApm4DInterceptOnClick.Create(AOwner, AInterceptControl: TComponent);
begin
  inherited;
  FExecuting := False;

  FOnClick := TControlAccess(AInterceptControl).OnClick;
  TControlAccess(AInterceptControl).OnClick := DoOnClick;
end;

procedure TApm4DInterceptOnClick.DoOnClick(Sender: TObject);
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
      if Assigned(FOnClick) then
        FOnClick(Sender);
      Exit;
    end;

    IsSpan := TApm4D.ExistsTransaction;
    if IsSpan then
      TApm4D.StartSpan(FOwner.Name + '.' + GetControlName(FControl) + '.Click', 'UI.Click')
    else
      TApm4D.StartTransaction(FOwner.Name + '.' + GetControlName(FControl) + '.Click', 'UI.Click');
    try
      if Assigned(FOnClick) then
        FOnClick(Sender);
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
