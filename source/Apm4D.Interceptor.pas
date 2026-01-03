{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor;

interface

uses
  System.Variants, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, System.Rtti, System.StrUtils;

type
  IApm4DInterceptor = Interface
    ['{5824BF04-8F7B-4721-866E-2D982A8180F6}']
  End;

  TApm4DInterceptor = class(TInterfacedObject, IApm4DInterceptor)
  protected
    FOwner: TComponent;
    FControl: TComponent;
    function HasProperty(AObject: TComponent; const APropertyName: string): Boolean;
    function GetControlName(AControl: TComponent): string; virtual;
  public
    constructor Create(AOwner, AInterceptControl: TComponent); virtual;
    class function IsCompatible(AComponent: TComponent): Boolean; virtual;
  end;

  TApm4DInterceptorClass = class of TApm4DInterceptor;

implementation

constructor TApm4DInterceptor.Create(AOwner, AInterceptControl: TComponent);
begin
  FOwner := AOwner;
  FControl := AInterceptControl;
end;

class function TApm4DInterceptor.IsCompatible(AComponent: TComponent): Boolean;
begin
  // Default implementation - override in descendants for specific type checks
  Result := True;
end;

function TApm4DInterceptor.HasProperty(AObject: TComponent; const APropertyName: string): Boolean;
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

function TApm4DInterceptor.GetControlName(AControl: TComponent): string;
begin
  Result := IfThen(AControl.Name <> '', AControl.Name, AControl.ClassName);
end;

end.
