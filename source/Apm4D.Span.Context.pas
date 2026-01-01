{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Span.Context;

interface

uses 
  Apm4D.Share.Service,
  Apm4D.Share.Context.Message,
  Apm4D.Span.Context.Http,
  Apm4D.Span.Context.Db,
  Apm4D.Span.Context.Destination;

type
  TSpanContextHttp = Apm4D.Span.Context.Http.TSpanContextHttp;
  TContextMessage = Apm4D.Share.Context.Message.TContextMessage;
  TSpanContextDB = Apm4D.Span.Context.Db.TSpanContextDB;
  TService = Apm4D.Share.Service.TService;

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
    function CreateDb: TSpanContextDB;

    property Http: TSpanContextHttp read FHttp;
    property Service: TService read FService;
    property Db: TSpanContextDB read FDb;
    property &Message: TContextMessage read FMessage;
    property Destination: TSpanContextDestination read FDestination;
  end;

implementation

{ TSpanContext }

constructor TSpanContext.Create;
begin
  FService := TService.Create;
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

function TSpanContext.CreateDb: TSpanContextDB;
begin 
  if not Assigned(FDb) then
    FDb := TSpanContextDB.Create;
  Result := FDb;
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
  if Assigned(FDb) then
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
