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
  TElasticAPM4DContextPage = class
  private
    FReferer: String;
    FUrl: String;
  public
    property referer: String read FReferer write FReferer;
    property url: String read FUrl write FUrl;
  end;

  TElasticAPM4DContextResponse = class
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

  TElasticAPM4DContext = class
  private
    FPage: TElasticAPM4DContextPage;
    FResponse: TElasticAPM4DContextResponse;
    FRequest: TElasticAPM4DRequest;
    FService: TElasticAPM4DService;
    FUser: TElasticAPM4DUser;
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
    property User: TElasticAPM4DUser read FUser write FUser;
    property Service: TElasticAPM4DService read FService write FService;
    property Request: TElasticAPM4DRequest read FRequest write FRequest;
    property Page: TElasticAPM4DContextPage read FPage write FPage;
    property Response: TElasticAPM4DContextResponse read FResponse write FResponse;
  end;

implementation

{ TElasticAPM4DContext }

procedure TElasticAPM4DContext.AutoCreatePage(AIdHTTP: TIdCustomHTTP);
begin
  FPage := TElasticAPM4DContextPage.Create;
  FPage.referer := AIdHTTP.Request.referer;
  FPage.url := AIdHTTP.Request.url;
end;

procedure TElasticAPM4DContext.AutoCreateRequest(AIdHTTP: TIdCustomHTTP);
begin
  FRequest := TElasticAPM4DRequest.Create(AIdHTTP);
end;

procedure TElasticAPM4DContext.AutoCreateResponse(AIdHTTP: TIdCustomHTTP);
begin
  FResponse := TElasticAPM4DContextResponse.Create;
  FResponse.finished := AIdHTTP.ResponseCode < 300;
  FResponse.headers_sent := AIdHTTP.Response.CustomHeaders.Count > 0;
  FResponse.status_code := AIdHTTP.ResponseCode;
end;

{$IFDEF dmvcframework}

procedure TElasticAPM4DContext.AutoConfigureContext(const AResponse: IRESTResponse);
begin
  FResponse := TElasticAPM4DContextResponse.Create;
  FResponse.status_code := AResponse.ResponseCode;
  FResponse.finished := AResponse.ResponseCode < 300;
  FResponse.headers_sent := AResponse.headers.Count > 0;

  FRequest := TElasticAPM4DRequest.Create;
  FRequest.body := AResponse.BodyAsString;
end;

procedure TElasticAPM4DContext.AutoConfigureContext(const AContext: TWebContext);
begin
  FResponse := TElasticAPM4DContextResponse.Create;
  FResponse.status_code := AContext.Response.StatusCode;
  FResponse.finished := AContext.Response.StatusCode < 300;
  FResponse.headers_sent := AContext.Response.CustomHeaders.Count > 0;

  FRequest := TElasticAPM4DRequest.Create;
  FRequest.body := AContext.Request.body;
  FRequest.method := AContext.Request.HTTPMethodAsString;

  if not AContext.LoggedUser.username.IsEmpty then
  begin
    if not Assigned(FUser) then
      FUser := TElasticAPM4DUser.Create;
    FUser.username := AContext.LoggedUser.username;
  end;
end;

{$ENDIF}

constructor TElasticAPM4DContext.Create;
begin
  FService := TElasticAPM4DService.Create;
  FUser := TElasticAPM4DUser.Create;
end;

destructor TElasticAPM4DContext.Destroy;
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
