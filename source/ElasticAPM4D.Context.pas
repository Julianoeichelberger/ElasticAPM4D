unit ElasticAPM4D.Context;

interface

uses
  System.SysUtils, ElasticAPM4D.User, ElasticAPM4D.Request, ElasticAPM4D.Service;

type
  TPage = class
  private
    FReferer: String;
    FUrl: String;
  public
    property Referer: String read FReferer write FReferer;
    property Url: String read FUrl write FUrl;
  end;

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

  TContext = class
  private
    FPage: TPage;
    FResponse: TResponse;
    FRequest: TRequest;
    FService: TService;
    FUser: TUser;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property User: TUser read FUser write FUser;
    property Service: TService read FService write FService;
    property Request: TRequest read FRequest write FRequest;
    property Page: TPage read FPage write FPage;
    property Response: TResponse read FResponse write FResponse;
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
  inherited;
end;

end.
