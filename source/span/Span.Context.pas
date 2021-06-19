unit Span.Context;

interface

uses
  Share.Service,
  Share.Context.Message,
  Span.Context.Http,
  Span.Context.Db,
  Span.Context.Destination;

type
  TSpanContextHttp = Span.Context.Http.TSpanContextHttp;
  TContextMessage = Share.Context.Message.TContextMessage;
  TSpanContextDB = Span.Context.Db.TSpanContextDB;
  TService = Share.Service.TService;

  // <summary>
  // Context holds arbitrary contextual information for the event.
  // </summary>
  TSpanContext = class
  private
    FService: TService;
    FHttp: TSpanContextHttp;
    FDb: TSpanContextDB;
    FMessage: TContextMessage;
    FDestination: TSpanContextDestination;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function CreateHttp(const AHttpMethod: string): TSpanContextHttp;
    function CreateMessage(const AName, ABody: string): TContextMessage;
    function CreateDestination: TSpanContextDestination;

    property Http: TSpanContextHttp read FHttp;
    property Service: TService read FService;
    property Db: TSpanContextDB read FDb;
  end;

implementation

{ TSpanContext }

constructor TSpanContext.Create;
begin
  FService := TService.Create;
  FDb := TSpanContextDB.Create;
end;

function TSpanContext.CreateDestination: TSpanContextDestination;
begin
  if not Assigned(FDestination) then
    FDestination := TSpanContextDestination.Create;
  Result := FDestination;
end;

function TSpanContext.CreateHttp(const AHttpMethod: string): TSpanContextHttp;
begin
  if not Assigned(FHttp) then
    FHttp := TSpanContextHttp.Create(AHttpMethod);
  Result := FHttp;
end;

function TSpanContext.CreateMessage(const AName, ABody: string): TContextMessage;
begin
  if not Assigned(FMessage) then
    FMessage := TContextMessage.Create(AName, ABody);
  Result := FMessage;
end;

destructor TSpanContext.Destroy;
begin
  FService.Free;
  FDb.Free;
  if Assigned(FHttp) then
    FHttp.Free;
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDestination) then
    FDestination.Free;
  inherited;
end;

end.
