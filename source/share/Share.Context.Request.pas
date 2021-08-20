unit Share.Context.Request;

interface

uses
  Share.Context.Request.Socket, Share.Context.Request.Url;

type
  // <summary>
  // Request describes the HTTP request information in case the event was created as a result of an HTTP request.
  // </summary>
  TContextRequest = class
  private
    FBody: string;
    Fcookies: TObject;
    Fheaders: string;
    FHttp_version: String;
    FMethod: String;
    FSocket: TContextRequestSocket;
    FUrl: TContextRequestURL;
  public
    constructor Create(const AHttpMethod: string);
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

    property Method: string read FMethod;

    property Socket: TContextRequestSocket read FSocket;
    property Url: TContextRequestURL read FUrl;
  end;

implementation

{ TRequest }

constructor TContextRequest.Create(const AHttpMethod: string);
begin
  FUrl := TContextRequestURL.Create;
  FSocket := TContextRequestSocket.Create;
  FHttp_version := 'HTTP/1.1';
  FMethod := AHttpMethod;
end;

destructor TContextRequest.Destroy;
begin
  FUrl.Free;
  FSocket.Free;
  if Assigned(Fcookies) then
    Fcookies.Free;
  inherited;
end;

end.
