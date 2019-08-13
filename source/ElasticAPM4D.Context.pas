unit ElasticAPM4D.Context;

interface

uses
  SysUtils,
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
    Fheaders: string;
  public
    property finished: Boolean read FFinished write FFinished;
    property headers_sent: Boolean read FHeaders_sent write FHeaders_sent;
    property status_code: Integer read FStatus_code write FStatus_code;
    property headers: string read Fheaders write Fheaders;
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
