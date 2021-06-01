unit ElasticAPM4D.Request;

interface

type
  // <summary>
  // Socket holds information related to the recorded request,
  // such as whether or not data were encrypted and the remote address.
  // </summary>
  TSocket = class
  private
    FEncrypted: Boolean;
    FRemote_address: string;
  public
    property Encrypted: Boolean read FEncrypted write FEncrypted;
    property Remote_address: string read FRemote_address write FRemote_address;
  end;

  // <summary>
  // URL holds information sucha as the raw URL, scheme, host and path.
  // </summary>
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
    // <summary> Full, possibly agent-assembled URL of the request, e.g. https://example.com:443/search?q=elasticsearch#top. </summary>
    property Full: string read FFull write FFull;

    // <summary> Hash of the request URL, e.g. 'top' </summary>
    property Hash: string read FHash write FHash;

    // <summary> Hostname information of the request, e.g. 'example.com'.\ </summary>
    property Hostname: string read FHostname write FHostname;

    // <summary> Path of the request, e.g. '/search' </summary>
    property Pathname: string read FPathname write FPathname;

    // <summary> Port of the request, e.g. '443'. Can be sent as string or int. </summary>
    property Port: Integer read FPort write FPort;

    // <summary> Protocol information for the recorded request, e.g. 'https:' </summary>
    property Protocol: string read FProtocol write FProtocol;

    // <summary>
    // Raw unparsed URL of the HTTP request line, e.g https://example.com:443/search?q=elasticsearch. This URL may be
    // absolute or relative. For more details, see https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2.
    // </summary>
    property Raw: string read FRaw write FRaw;

    // <summary> Search contains the query string information of the request. It is expected to have values delimited by ampersands. </summary>
    property Search: string read FSearch write FSearch;
  end;

  // <summary>
  // Request describes the HTTP request information in case the event was created as a result of an HTTP request.
  // </summary>
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
    // <summary>
    // Body only contais the request bod, not the query string information.
    // It can either be a dictionary (for standard HTTP requests) or a raw request body.
    // </summary>
    property Body: string read FBody write FBody;

    // <summary>
    // Cookies used by the request, parsed as key-value objects.
    // </summary>
    property Cookies: TObject read Fcookies write Fcookies;

    // <summary>
    // Headers includes any HTTP headers sent by the requester. Cookies will be taken by headers if supplied.
    // </summary>
    property Headers: string read Fheaders write Fheaders;

    // <summary>
    // HTTPVersion holds information about the used HTTP version.
    // </summary>
    property Http_version: string read FHttp_version write FHttp_version;

    // <summary>
    // Method holds information about the method of the HTTP request.
    // </summary>
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
