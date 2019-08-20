unit ElasticAPM4D.Request;

interface

Uses
  IdHTTP;

type
  TElasticAPM4DRequestSocket = class
  private
    FEncrypted: Boolean;
    FRemote_address: String;
  public
    property encrypted: Boolean read FEncrypted write FEncrypted;
    property remote_address: String read FRemote_address write FRemote_address;
  end;

  TElasticAPM4DRequestURL = class
  private
    FFull: String;
    FHash: String;
    FHostname: String;
    FPathname: String;
    FPort: Integer;
    FProtocol: String;
    FRaw: String;
    FSearch: String;
  public
    property full: String read FFull write FFull;
    property hash: String read FHash write FHash;
    property hostname: String read FHostname write FHostname;
    property pathname: String read FPathname write FPathname;
    property port: Integer read FPort write FPort;
    property protocol: String read FProtocol write FProtocol;
    property raw: String read FRaw write FRaw;
    property search: String read FSearch write FSearch;
  end;

  TElasticAPM4DRequest = class
  private
    FFreeCookies: Boolean;
    FBody: String;
    Fcookies: TObject;
    Fheaders: string;
    FHttp_version: String;
    FMethod: String;
    FSocket: TElasticAPM4DRequestSocket;
    FUrl: TElasticAPM4DRequestURL;
  public
    constructor Create(const AFreeCookies: Boolean = True); overload;
    constructor Create(AIdHTTP: TIdCustomHTTP); overload;
    destructor Destroy; override;

    property body: String read FBody write FBody;
    property cookies: TObject read Fcookies write Fcookies;
    property headers: string read Fheaders write Fheaders;
    property http_version: String read FHttp_version write FHttp_version;
    property method: String read FMethod write FMethod;
    property socket: TElasticAPM4DRequestSocket read FSocket;
    property url: TElasticAPM4DRequestURL read FUrl;
  end;

implementation

Uses
  SysUtils;

{ TElasticAPM4DRequest }

constructor TElasticAPM4DRequest.Create(const AFreeCookies: Boolean);
begin
  FFreeCookies := AFreeCookies;
  FSocket := TElasticAPM4DRequestSocket.Create;
  FUrl := TElasticAPM4DRequestURL.Create;
end;

constructor TElasticAPM4DRequest.Create(AIdHTTP: TIdCustomHTTP);
var
  I: Integer;
begin
  Create;
  FMethod := AIdHTTP.Request.method;
  FUrl.protocol := AIdHTTP.Response.ResponseText;
  FUrl.hostname := AIdHTTP.Request.Host;
  FUrl.full := AIdHTTP.Request.url;

  for I := 0 to pred(AIdHTTP.Request.CustomHeaders.Count) do
    Fheaders := Fheaders + ',' + AIdHTTP.Request.CustomHeaders.Strings[I];

  if not Fheaders.isEmpty then
    Fheaders := Fheaders.Remove(1, 1);
end;

destructor TElasticAPM4DRequest.Destroy;
begin
  FUrl.Free;
  FSocket.Free;
  if Assigned(Fcookies) and FFreeCookies then
    Fcookies.Free;
  inherited;
end;

end.
