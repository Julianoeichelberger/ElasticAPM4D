unit Share.Context.Request.Url;

interface

type
  // <summary>
  // URL holds information sucha as the raw URL, scheme, host and path.
  // </summary>
  TContextRequestURL = class
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

implementation

end.
