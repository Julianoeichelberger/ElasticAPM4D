{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptors.Handler;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptors.Base;

type
  IApm4DInterceptorHandler = Interface
    ['{D4C6E3B2-1F4E-4D3A-8F1E-3C6F4D8B9A2E}']
    procedure Inject(AParent: TComponent);
    procedure RegisterInterceptor(AInterceptor: TApm4DInterceptorClass);
    procedure UnregisterInterceptor(AInterceptor: TApm4DInterceptorClass);
  End;

  TApm4DInterceptorHandler = class(TInterfacedObject, IApm4DInterceptorHandler)
  private
    FInterceptors: TList<TApm4DInterceptorClass>;
    FControlsIntercepted: TDictionary<TComponent, TApm4DInterceptorBase>;
    procedure InternalIntercept(AControl: TComponent; AOwner: TComponent; AInterceptor: TApm4DInterceptorClass);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inject(AParent: TComponent);

    procedure RegisterInterceptor(AInterceptor: TApm4DInterceptorClass);
    procedure UnregisterInterceptor(AInterceptor: TApm4DInterceptorClass);
  end;

implementation

uses
  Apm4D.Settings;

constructor TApm4DInterceptorHandler.Create;
begin
  FInterceptors := TList<TApm4DInterceptorClass>.Create;
  FControlsIntercepted := TDictionary<TComponent, TApm4DInterceptorBase>.Create;

  FInterceptors.AddRange(TApm4DSettings.GetInterceptors.ToArray);
end;

destructor TApm4DInterceptorHandler.Destroy;
begin
  FInterceptors.Free;
  FControlsIntercepted.Free;
  inherited;
end;

procedure TApm4DInterceptorHandler.RegisterInterceptor(AInterceptor: TApm4DInterceptorClass);
begin
  FInterceptors.Add(AInterceptor);
end;

procedure TApm4DInterceptorHandler.UnregisterInterceptor(AInterceptor: TApm4DInterceptorClass);
begin
  FInterceptors.Remove(AInterceptor);
end;

procedure TApm4DInterceptorHandler.InternalIntercept(AControl: TComponent; AOwner: TComponent;
  AInterceptor: TApm4DInterceptorClass);
begin
  if FControlsIntercepted.ContainsKey(AControl) then
    Exit;

  var
  Interceptor := AInterceptor.Create;
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
    for var I := 0 to TWinControl(AControl).ComponentCount - 1 do
    begin
      var
      ChildControl := TWinControl(AControl).Components[I];
      for var InterceptorClass in FInterceptors.ToArray do
        InternalIntercept(ChildControl, AOwner, InterceptorClass);
    end;
  end
  else if AControl is TDataModule then
  begin
    for var I := 0 to TDataModule(AControl).ComponentCount - 1 do
    begin
      var
      ChildControl := TDataModule(AControl).Components[I];
      for var InterceptorClass in FInterceptors.ToArray do
        InternalIntercept(ChildControl, AOwner, InterceptorClass);
    end;
  end;
end;

procedure TApm4DInterceptorHandler.Inject(AParent: TComponent);
begin
  for var InterceptorClass in FInterceptors.ToArray do
    InternalIntercept(AParent, AParent, InterceptorClass);
end;

end.
