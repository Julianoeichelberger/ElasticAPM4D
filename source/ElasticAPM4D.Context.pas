unit ElasticAPM4D.Context;

interface

uses
  System.SysUtils, ElasticAPM4D.User, ElasticAPM4D.Request, ElasticAPM4D.Service, ElasticAPM4D.Message;

type
  // <summary>
  // Page holds information related to the current page and page referers. It is only sent from RUM agents.
  // </summary>
  TPage = class
  private
    FReferer: String;
    FUrl: String;
  public
    property Referer: String read FReferer write FReferer;
    property Url: String read FUrl write FUrl;
  end;

  // <summary>
  // Response describes the HTTP response information in case the event was created as a result of an HTTP request
  // </summary>
  TResponse = class
  private
    FFinished: Boolean;
    FHeaders_sent: Boolean;
    FStatus_code: Integer;
    Fheaders: TObject;
  public
    property Finished: Boolean read FFinished write FFinished;
    property Headers_sent: Boolean read FHeaders_sent write FHeaders_sent;
    property Status_code: Integer read FStatus_code write FStatus_code;
    property Headers: TObject read Fheaders write Fheaders;
  end;

  // <summary>
  // Context holds arbitrary contextual information for the event.
  // </summary>
  TContext = class
  private
    FPage: TPage;
    FResponse: TResponse;
    FRequest: TRequest;
    FService: TService;
    FUser: TUser;
    FMessage: TMessage;
    FCustom: TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property User: TUser read FUser write FUser;
    property Service: TService read FService write FService;
    property Request: TRequest read FRequest write FRequest;
    property Page: TPage read FPage write FPage;
    property Response: TResponse read FResponse write FResponse;
    property Message: TMessage read FMessage write FMessage;

    // <summary>
    // Custom can contain additional metadata to be stored with the event. The format is unspecified and can be deeply
    // nested objects. The information will not be indexed or searchable in Elasticsearch.
    // </summary>
    property Custom: TObject read FCustom write FCustom;
  end;

implementation

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
