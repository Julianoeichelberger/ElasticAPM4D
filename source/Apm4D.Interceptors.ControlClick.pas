{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Interceptors.ControlClick;

interface

uses 
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptors.Base;

type
  TAPMControlClickInterceptor<T: class> = class(TApm4DInterceptor<T>)
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

uses 
  Apm4D;

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
  // Prote��o contra re-entrada (stack overflow)
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
      TApm4D.StartSpan(FOwner.Name + '.' + GetControlName + '.Click', 'UI.Click')
    else
      TApm4D.StartTransaction(FOwner.Name + '.' + GetControlName + '.Click', 'UI.Click');
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
