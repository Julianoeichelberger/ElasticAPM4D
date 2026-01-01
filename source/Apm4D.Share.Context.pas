{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Context;

interface

uses 
  Apm4D.Share.User,
  Apm4D.Share.Response,
  Apm4D.Share.Context.Request,
  Apm4D.Share.Context.Message,
  Apm4D.Share.Context.Page,
  Apm4D.Share.Service;

type
  // <summary>
  // Context holds arbitrary contextual information for the event.
  // </summary>
  TContext = class
  private
    FPage: TContextPage;
    FResponse: TResponse;
    FRequest: TContextRequest;
    FService: TService;
    FUser: TUser;
    FMessage: TContextMessage;
    FCustom: TObject;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPage(const AReferer, AUrl: string);
    procedure AddRequest(const AHttpMethod: string);
    procedure AddResponse(const AStatusCode: Integer);
    procedure AddMessage(const AName, ABody: string);

    property User: TUser read FUser;
    property Service: TService read FService;
    property Request: TContextRequest read FRequest;
    property Response: TResponse read FResponse;
    property Message: TContextMessage read FMessage;

    // <summary>
    // Custom can contain additional Apm4D.Metadata to be stored with the event. The format is unspecified and can be deeply
    // nested objects. The information will not be indexed or searchable in Elasticsearch.
    // </summary>
    property Custom: TObject read FCustom write FCustom;
  end;

implementation

{ TContext }

procedure TContext.AddMessage(const AName, ABody: string);
begin
  if not Assigned(FMessage) then
    FMessage := TContextMessage.Create(AName, ABody);
end;

procedure TContext.AddPage(const AReferer, AUrl: string);
begin
  if Assigned(FPage) then
    exit;

  FPage := TContextPage.Create;
  FPage.Referer := AReferer;
  FPage.Url := AUrl;
end;

procedure TContext.AddRequest(const AHttpMethod: string);
begin
  if not Assigned(FRequest) then
    FRequest := TContextRequest.Create(AHttpMethod);
end;

procedure TContext.AddResponse(const AStatusCode: Integer);
begin
  if not Assigned(FResponse) then
    FResponse := TResponse.Create(AStatusCode);
end;

constructor TContext.Create;
begin
  FService := TService.Create;
  FUser := TUser.Create;
end;

destructor TContext.Destroy;
begin
  FService.Free;
  FUser.Free;
  if Assigned(FPage) then
    FPage.Free;
  if Assigned(FResponse) then
    FResponse.Free;
  if Assigned(FRequest) then
    FRequest.Free;
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FCustom) then
    FCustom.Free;
  inherited;
end;

end.
