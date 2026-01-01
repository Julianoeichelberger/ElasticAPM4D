{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Interceptors.Base;

interface

uses
  System.Variants, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, System.Rtti, System.StrUtils;

type
  IApm4DInterceptor = Interface
    ['{5824BF04-8F7B-4721-866E-2D982A8180F6}']
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent);
    function GetInterceptedType: TClass;
  End;

  // Base non-generic class to allow class references
  TApm4DInterceptorBase = class(TInterfacedObject, IApm4DInterceptor)
  public
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); virtual;
    function GetInterceptedType: TClass; virtual;
  end;

  TApm4DInterceptorClass = class of TApm4DInterceptorBase;

  TApm4DInterceptor<T: class> = class(TApm4DInterceptorBase)
  protected
    FControl: T;
    FOwner: TComponent;
    function HasProperty(AObject: TComponent; const APropertyName: string): Boolean;
    function GetControlName: string; virtual;
  public
  end;

implementation

procedure TApm4DInterceptorBase.Load(AOwner: TComponent; AInterceptControl: TComponent);
begin
  // empty
end;

function TApm4DInterceptorBase.GetInterceptedType: TClass;
begin
  Result := nil;
end;

function TApm4DInterceptor<T>.HasProperty(AObject: TComponent; const APropertyName: string): Boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
begin
  Result := False;
  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(AObject.ClassType);
    if Assigned(RttiType) then
    begin
      RttiProperty := RttiType.GetProperty(APropertyName);
      Result := Assigned(RttiProperty);
    end;
  finally
    RttiContext.Free;
  end;
end;

function TApm4DInterceptor<T>.GetControlName: string;
begin
  Result := IfThen(TComponent(FControl).Name <> '', TComponent(FControl).Name, FControl.ClassName);
end;

end.
