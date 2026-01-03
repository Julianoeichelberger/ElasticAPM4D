{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Interceptor.Handler;

interface

uses
  System.Variants, Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList,
  System.Classes, Data.DB, Apm4D.Interceptor;

type
  IApm4DInterceptorHandler = Interface
    ['{D4C6E3B2-1F4E-4D3A-8F1E-3C6F4D8B9A2E}']
    function Inject: IApm4DInterceptorHandler;
    function IgnoreInheritedClasses: IApm4DInterceptorHandler;
    function AddDefaultsInterceptors: IApm4DInterceptorHandler;
    function AddInterceptor(AInterceptor: TApm4DInterceptorClass; AClasses: TArray<TClass>): IApm4DInterceptorHandler;
    procedure Disconnect;
  End;

  TInterceptorFinder = class
  private
    FClasses: TDictionary<TClass, TArray<TApm4DInterceptorClass>>;
    FInterceptors: TArray<IApm4DInterceptor>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AClass: TClass; AInterceptor: TApm4DInterceptorClass);
    procedure Build(AOwner, AControl: TComponent; AUseInheritedClasses: boolean);
  end;

  TApm4DInterceptorHandler= class;

  TComponentHelper = class(TComponent)
  private
    FHandler: TApm4DInterceptorHandler;
    FMonitoredComponent: TComponent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; AHandler: TApm4DInterceptorHandler); reintroduce;
  end;

  TApm4DInterceptorHandler = class(TInterfacedObject, IApm4DInterceptorHandler)
  private
    FOwner: TComponent;
    FFinder: TInterceptorFinder;
    FUseInheritedClasses: Boolean;
    FHelper: TComponentHelper;
    function IgnoreInheritedClasses: IApm4DInterceptorHandler;
    function AddDefaultsInterceptors: IApm4DInterceptorHandler;
    function AddInterceptor(AInterceptor: TApm4DInterceptorClass; AClasses: TArray<TClass>): IApm4DInterceptorHandler;
    function Inject: IApm4DInterceptorHandler;
    procedure Disconnect;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

  TApm4DInterceptorBuilder = class
    class function CreateDefault(AOwner: TComponent): IApm4DInterceptorHandler;
    class function New(AOwner: TComponent): IApm4DInterceptorHandler;
  end;

implementation

uses
  Apm4D.Settings;

{ TComponentHelper }

constructor TComponentHelper.Create(AOwner: TComponent; AHandler: TApm4DInterceptorHandler);
begin
  inherited Create(nil); // No Owner to avoid double-free
  FHandler := AHandler;
  FMonitoredComponent := AOwner;
  if Assigned(AOwner) then
    AOwner.FreeNotification(Self);
end;

procedure TComponentHelper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMonitoredComponent) and Assigned(FHandler) then
  begin
    // Don't call Disconnect here, just invalidate references
    // to avoid accessing components during destruction
    FHandler.FOwner := nil;
    FMonitoredComponent := nil;
    FHandler := nil;
  end;
end;

{ TInterceptorFinder }

procedure TInterceptorFinder.Add(const AClass: TClass; AInterceptor: TApm4DInterceptorClass);
var
  Interceptors: TArray<TApm4DInterceptorClass>;
begin
  if not FClasses.TryGetValue(AClass, Interceptors) then
  begin
    FClasses.Add(AClass, [AInterceptor]);
    exit;
  end;
  FClasses.AddOrSetValue(AClass, Interceptors + [AInterceptor])
end;

constructor TInterceptorFinder.Create;
begin
  FClasses := TDictionary < TClass, TArray < TApm4DInterceptorClass >>.Create;
end;

destructor TInterceptorFinder.Destroy;
begin
  // Clear interceptors array before destroying to avoid invalid pointer operations
  SetLength(FInterceptors, 0);
  FClasses.Free;
  inherited;
end;

procedure TInterceptorFinder.Build(AOwner, AControl: TComponent; AUseInheritedClasses: boolean);

  function MakeIt(AClasses: TArray<TApm4DInterceptorClass>): TArray<IApm4DInterceptor>;
  begin
    Result := [];
    for var Clas in AClasses do
    begin
      // Only create interceptor if component is compatible
      if Clas.IsCompatible(AControl) then
        Result := Result + [Clas.Create(AOwner, AControl)];
    end;
  end;

var
  Interceptors: TArray<TApm4DInterceptorClass>;
  NewInterceptors: TArray<IApm4DInterceptor>;
begin
  if FClasses.TryGetValue(AControl.ClassType, Interceptors) then
  begin
    NewInterceptors := MakeIt(Interceptors);
    // Accumulate interceptors instead of replacing
    FInterceptors := FInterceptors + NewInterceptors;
    exit;
  end;
  if AUseInheritedClasses then
  begin
    for var Clas in FClasses.Keys do
    begin
      if AControl.InheritsFrom(Clas) then
      begin
        NewInterceptors := MakeIt(FClasses.Items[Clas]);
        // Accumulate interceptors instead of replacing
        FInterceptors := FInterceptors + NewInterceptors;
        Break;
      end;
    end;
  end;
end;

{ TApm4DInterceptorHandler }

function TApm4DInterceptorHandler.AddDefaultsInterceptors: IApm4DInterceptorHandler;
var
  Defs: TDictionary<TApm4DInterceptorClass, TArray<TClass>>;
  Interceptor: TApm4DInterceptorClass;
begin
  Defs := TApm4DSettings.GetInterceptors;
  if Assigned(Defs) then
  begin
    for Interceptor in Defs.Keys do
      AddInterceptor(Interceptor, Defs.Items[Interceptor]);
  end;
  result := Self;
end;

function TApm4DInterceptorHandler.AddInterceptor(AInterceptor: TApm4DInterceptorClass;
  AClasses: TArray<TClass>): IApm4DInterceptorHandler;
begin
  if Assigned(FFinder) then
  begin
    for var Clas in AClasses do
      FFinder.Add(Clas, AInterceptor);
  end;
  result := Self;
end;

constructor TApm4DInterceptorHandler.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  Assert(FOwner <> nil, 'TApm4DInterceptorHandler: Param "AOwner" can not be nil');
  FFinder := TInterceptorFinder.Create;
  FUseInheritedClasses := true;
  FHelper := TComponentHelper.Create(FOwner, Self);
end;

destructor TApm4DInterceptorHandler.Destroy;
begin
  Disconnect;
  if Assigned(FHelper) then
    FHelper.Free;
  FFinder.Free;
  inherited;
end;

procedure TApm4DInterceptorHandler.Disconnect;
begin
  FOwner := nil; // Invalidate owner reference first
  if Assigned(FFinder) then
  begin
    SetLength(FFinder.FInterceptors, 0);
    FFinder.FClasses.Clear;
  end;
end;

function TApm4DInterceptorHandler.IgnoreInheritedClasses: IApm4DInterceptorHandler;
begin
  FUseInheritedClasses := false;
  result := Self;
end;

function TApm4DInterceptorHandler.Inject: IApm4DInterceptorHandler;
var
  i: Integer;
begin
  if not Assigned(FFinder) or not Assigned(FOwner) then
    Exit(Self);

  FFinder.Build(FOwner, FOwner, FUseInheritedClasses);
  for i := 0 to FOwner.ComponentCount - 1 do
  begin
    if Assigned(FOwner.Components[i]) then
      FFinder.Build(FOwner, FOwner.Components[i], FUseInheritedClasses);
  end;
  result := Self;
end;

{ TApm4DInterceptorBuilder }

class function TApm4DInterceptorBuilder.CreateDefault(AOwner: TComponent): IApm4DInterceptorHandler;
begin
  Result :=
    TApm4DInterceptorBuilder
    .New(AOwner)
    .AddDefaultsInterceptors
    .Inject;
end;

class function TApm4DInterceptorBuilder.New(AOwner: TComponent): IApm4DInterceptorHandler;
begin
  Result := TApm4DInterceptorHandler.Create(AOwner);
end;

end.
