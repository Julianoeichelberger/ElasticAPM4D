unit Interceptor.Base;

interface

uses
  System.Variants, System.Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, System.Rtti, System.StrUtils, System.TypInfo;

type
  IAPMInterceptor = Interface
    ['{5824BF04-8F7B-4721-866E-2D982A8180F6}']
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); 
    function GetInterceptedType: TClass;
  End;

  // Base non-generic class to allow class references
  TAPMInterceptorBase = class(TInterfacedObject, IAPMInterceptor)
  public
    procedure Load(AOwner: TComponent; AInterceptControl: TComponent); virtual;  
    function GetInterceptedType: TClass; virtual;  
  end;

  TAPMInterceptorClass = class of TAPMInterceptorBase;

  TAPMInterceptor<T: class> = class(TAPMInterceptorBase)
  protected 
    FControl: T;
    FOwner: TComponent; 
    function HasProperty(AObject: TComponent; const APropertyName: string): Boolean;   
    function GetControlName: string; virtual;
  public 
  end;
 
  
implementation

procedure TAPMInterceptorBase.Load(AOwner: TComponent; AInterceptControl: TComponent);
begin
  // empty
end;

function TAPMInterceptorBase.GetInterceptedType: TClass;
begin
  Result := nil;
end;


function TAPMInterceptor<T>.HasProperty(AObject: TComponent; const APropertyName: string): Boolean;
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

function TAPMInterceptor<T>.GetControlName: string;
begin 
  Result := IfThen(TComponent(FControl).Name <> '', TComponent(FControl).Name, FControl.ClassName);
end;



end.