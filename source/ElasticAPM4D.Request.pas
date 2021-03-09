unit ElasticAPM4D.Request;

interface

type
  TSocket = class
  private
    FEncrypted: Boolean;
    FRemote_address: string;
  public
    property Encrypted: Boolean read FEncrypted write FEncrypted;
    property Remote_address: string read FRemote_address write FRemote_address;
  end;

  TURL = class
  private
    FFull: string;
    FHash: string;
    FHostname: string;
    FPathname: string;
    FPort: Integer;
    FProtocol: string;
    FRaw: string;
    FSearch: string;
  public
    property Full: string read FFull write FFull;
    property Hash: string read FHash write FHash;
    property Hostname: string read FHostname write FHostname;
    property Pathname: string read FPathname write FPathname;
    property Port: Integer read FPort write FPort;
    property Protocol: string read FProtocol write FProtocol;
    property Raw: string read FRaw write FRaw;
    property Search: string read FSearch write FSearch;
  end;

  TRequest = class
  private
    FBody: string;
    Fcookies: TObject;
    Fheaders: string;
    FHttp_version: String;
    FMethod: String;
    FSocket: TSocket;
    FUrl: TURL;
  public
    constructor Create;
    destructor Destroy; override;

    property Body: string read FBody write FBody;
    property Cookies: TObject read Fcookies write Fcookies;
    property Headers: string read Fheaders write Fheaders;
    property Http_version: string read FHttp_version write FHttp_version;
    property Method: string read FMethod write FMethod;
    property Socket: TSocket read FSocket;
    property Url: TURL read FUrl;
  end;

implementation

Uses
  System.SysUtils;

{ TRequest }

constructor TRequest.Create;
begin
  FSocket := TSocket.Create;
  FUrl := TURL.Create;
end;

destructor TRequest.Destroy;
begin
  FUrl.Free;
  FSocket.Free;
  if Assigned(Fcookies) then
    Fcookies.Free;
  inherited;
end;

end.
