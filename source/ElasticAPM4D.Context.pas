unit ElasticAPM4D.Context;

interface

uses
{$IFDEF dmvcframework}
  MVCFramework.RESTClient,
  MVCFramework,
{$ENDIF}
  System.SysUtils,
  IdHTTP,
  ElasticAPM4D.User,
  ElasticAPM4D.Request,
  ElasticAPM4D.Service;

type
  TContextPage = class
  private
    FReferer: String;
    FUrl: String;
  public
    property referer: String read FReferer write FReferer;
    property url: String read FUrl write FUrl;
  end;

  TContextResponse = class
  private
    FFinished: Boolean;
    FHeaders_sent: Boolean;
    FStatus_code: Integer;
    Fheaders: TObject;
  public
    property finished: Boolean read FFinished write FFinished;
    property headers_sent: Boolean read FHeaders_sent write FHeaders_sent;
    property status_code: Integer read FStatus_code write FStatus_code;
    property headers: TObject read Fheaders write Fheaders;
  end;

  TContext = class
  private
    FPage: TContextPage;
    FResponse: TContextResponse;
    FRequest: TRequest;
    FService: TService;
    FUser: TUser;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AutoCreatePage(AIdHTTP: TIdCustomHTTP);
    procedure AutoCreateResponse(AIdHTTP: TIdCustomHTTP);
    procedure AutoCreateRequest(AIdHTTP: TIdCustomHTTP);

{$IFDEF dmvcframework}
    procedure AutoConfigureContext(const AResponse: IRESTResponse); overload;
    procedure AutoConfigureContext(const AContext: TWebContext); overload;
{$ENDIF}
    property User: TUser read FUser write FUser;
    property Service: TService read FService write FService;
    property Request: TRequest read FRequest write FRequest;
    property Page: TContextPage read FPage write FPage;
    property Response: TContextResponse read FResponse write FResponse;
  end;

implementation

{ TElasticAPM4DContext }

procedure TContext.AutoCreatePage(AIdHTTP: TIdCustomHTTP);
begin
  FPage := TContextPage.Create;
  FPage.referer := AIdHTTP.Request.referer;
  FPage.url := AIdHTTP.Request.url;
end;

procedure TContext.AutoCreateRequest(AIdHTTP: TIdCustomHTTP);
begin
  FRequest := TRequest.Create(AIdHTTP);
end;

procedure TContext.AutoCreateResponse(AIdHTTP: TIdCustomHTTP);
begin
  FResponse := TContextResponse.Create;
  FResponse.finished := AIdHTTP.ResponseCode < 300;
  FResponse.headers_sent := AIdHTTP.Response.CustomHeaders.Count > 0;
  FResponse.status_code := AIdHTTP.ResponseCode;
end;

{$IFDEF dmvcframework}

procedure TContext.AutoConfigureContext(const AResponse: IRESTResponse);
begin
  FResponse := TContextResponse.Create;
  FResponse.status_code := AResponse.ResponseCode;
  FResponse.finished := AResponse.ResponseCode < 300;
  FResponse.headers_sent := AResponse.headers.Count > 0;

  FRequest := TRequest.Create;
  FRequest.body := AResponse.BodyAsString;
end;

procedure TContext.AutoConfigureContext(const AContext: TWebContext);
begin
  FResponse := TContextResponse.Create;
  FResponse.status_code := AContext.Response.StatusCode;
  FResponse.finished := AContext.Response.StatusCode < 300;
  FResponse.headers_sent := AContext.Response.CustomHeaders.Count > 0;

  FRequest := TRequest.Create;
  FRequest.body := AContext.Request.body;
  FRequest.method := AContext.Request.HTTPMethodAsString;

  if not AContext.LoggedUser.username.IsEmpty then
  begin
    if not Assigned(FUser) then
      FUser := TUser.Create;
    FUser.username := AContext.LoggedUser.username;
  end;
end;

{$ENDIF}

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
  inherited;
end;

end.
