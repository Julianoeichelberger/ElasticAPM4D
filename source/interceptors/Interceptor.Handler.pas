unit Interceptor.Handler;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Interceptor.Base;

type
  IAPMInterceptorHandler = Interface
    ['{D4C6E3B2-1F4E-4D3A-8F1E-3C6F4D8B9A2E}']
    procedure Inject(AParent: TComponent);
    procedure RegisterInterceptor(AInterceptor: TAPMInterceptorClass);
    procedure UnregisterInterceptor(AInterceptor: TAPMInterceptorClass);
  End;

  TAPMInterceptorHandler = class(TInterfacedObject, IAPMInterceptorHandler)
  private
    FInterceptors: TList<TAPMInterceptorClass>;
    FControlsIntercepted: TDictionary<TComponent, TAPMInterceptorBase>;
    procedure InternalIntercept(AControl: TComponent; AOwner: TComponent; AInterceptor: TAPMInterceptorClass);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inject(AParent: TComponent);

    procedure RegisterInterceptor(AInterceptor: TAPMInterceptorClass);
    procedure UnregisterInterceptor(AInterceptor: TAPMInterceptorClass);
  end;

implementation

Uses
  ElasticAPM4D.Config;

constructor TAPMInterceptorHandler.Create;
begin
  FInterceptors := TList<TAPMInterceptorClass>.Create;
  FControlsIntercepted := TDictionary<TComponent, TAPMInterceptorBase>.Create;

  FInterceptors.AddRange(TConfig.Interceptors.ToArray);
end;

destructor TAPMInterceptorHandler.Destroy;
begin
  FInterceptors.Free;
  FControlsIntercepted.Free;
  inherited;
end;

procedure TAPMInterceptorHandler.RegisterInterceptor(AInterceptor: TAPMInterceptorClass);
begin
  FInterceptors.Add(AInterceptor);
end;

procedure TAPMInterceptorHandler.UnregisterInterceptor(AInterceptor: TAPMInterceptorClass);
begin
  FInterceptors.Remove(AInterceptor);
end;

procedure TAPMInterceptorHandler.InternalIntercept(AControl: TComponent; AOwner: TComponent; AInterceptor: TAPMInterceptorClass);
begin
  if FControlsIntercepted.ContainsKey(AControl) then
    Exit;

  var Interceptor := AInterceptor.Create;
  if AControl.InheritsFrom(Interceptor.GetInterceptedType) then
  begin
    FControlsIntercepted.Add(AControl, Interceptor);
    Interceptor.Load(AOwner, AControl);
  end;

  if AControl is TWinControl then
  begin
    for var I := 0 to TWinControl(AControl).ControlCount - 1 do
    begin
      var
      ChildControl := TWinControl(AControl).Controls[I];
      for var InterceptorClass in FInterceptors.ToArray do
        InternalIntercept(ChildControl, AOwner, InterceptorClass);
    end;
  end;
end;

procedure TAPMInterceptorHandler.Inject(AParent: TComponent);
begin
  for var InterceptorClass in FInterceptors.ToArray do
    InternalIntercept(AParent, AParent, InterceptorClass);
end;

end.
