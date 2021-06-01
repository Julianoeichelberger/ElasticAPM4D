unit ElasticAPM4D.Helpers;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, ElasticAPM4D.Transaction, ElasticAPM4D.Span;

function StartTransaction(AIdHTTP: TIdHTTP; const AType, AName: string): TTransaction;
procedure EndTransaction(AIdHTTP: TIdHTTP);
function StartSpan(AName: string): TSpan;
procedure EndSpan(AIdHTTP: TIdHTTP);
{$IFDEF dmvcframework}
class function StartTransaction(AActionName: string; AContext: TWebContext): TTransaction; overload;
class procedure EndTransaction(const ARESTClient: MVCFramework.RESTClient.TRESTClient; const AResponse: IRESTResponse;
  const AHttpMethod: string); overload;
class procedure EndTransaction(const AContext: TWebContext); overload;
{$ENDIF}

implementation

Uses
  ElasticAPM4D, ElasticAPM4D.Context, ElasticAPM4D.Request;

procedure IndyHttpURL(var AIdHTTP: TIdHTTP);
begin

end;


function StartTransaction(AIdHTTP: TIdHTTP; const AType, AName: string): TTransaction;
begin
  if not TElasticAPM4D.ExistsTransaction then
    Result := TElasticAPM4D.StartTransaction(AName, 'Request')
  else
    Result := TElasticAPM4D.Transaction;

  Result.Context.Request.url.Hostname := AIdHTTP.url.Host;
  Result.Context.Request.url.Full := AIdHTTP.url.GetFullURI;
  Result.Context.Request.url.Protocol := AIdHTTP.url.Protocol;
  Result.Context.Request.url.Pathname := AIdHTTP.url.Path;
  Result.Context.Request.url.port := StrToIntDef(AIdHTTP.url.port, 0);
  Result.Context.Request.url.Search := AIdHTTP.url.Params;
  Result.Context.Request.url.Raw := AIdHTTP.url.Document;
  Result.Context.Request.Method := AIdHTTP.Request.Method;
end;

procedure EndTransaction(AIdHTTP: TIdHTTP);
begin
  if not TElasticAPM4D.ExistsTransaction then
    exit;

  TElasticAPM4D.Transaction.Context.Response := TResponse.Create;
  TElasticAPM4D.Transaction.Context.Response.finished := True;
  TElasticAPM4D.Transaction.Context.Response.headers_sent := AIdHTTP.Request.CustomHeaders.Count > 0;
  TElasticAPM4D.Transaction.Context.Response.status_code := AIdHTTP.ResponseCode;

  TElasticAPM4D.EndTransaction;
end;

function StartSpan(AName: string): TSpan;
begin
  Result := TElasticAPM4D.StartSpan(AName, 'Request');
end;

procedure EndSpan(AIdHTTP: TIdHTTP);
begin
  TElasticAPM4D.Span.Context.http := THttp.Create;
  TElasticAPM4D.Span.Context.http.Method := AIdHTTP.Request.Method;
  TElasticAPM4D.Span.Context.http.status_code := AIdHTTP.ResponseCode;
  TElasticAPM4D.Span.Context.http.url := AIdHTTP.url.URI;
  TElasticAPM4D.EndSpan;
end;

{$IFDEF dmvcframework}


class function TElasticAPM4D.StartTransaction(AActionName: string; AContext: TWebContext): TTransaction;
begin
  Result := StartCustomTransaction('DMVCFramework', AActionName);
  FPackage.Header := AContext.Request.Headers[HeaderKey];
end;

class procedure TElasticAPM4D.EndTransaction(const ARESTClient: MVCFramework.RESTClient.TRESTClient;
  const AResponse: IRESTResponse; const AHttpMethod: string);
var
  LError: TError;
begin
  // TElasticAPM4D.Transaction.Context.AutoConfigureContext(AResponse);
  TElasticAPM4D.Transaction.Context.Request.url.Full := ARESTClient.url;
  TElasticAPM4D.Transaction.Context.Request.Method := AHttpMethod;
  if AResponse.HasError then
  begin
    LError := GetError;

    LError.Exception.Code := AResponse.Error.HTTPError.ToString;
    LError.Exception.&Type := AResponse.Error.ExceptionClassname;
    LError.Exception.Message := AResponse.Error.ExceptionMessage;

    AddError(LError);
  end;
  EndTransaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AContext: TWebContext);
begin
  // CurrentTransaction.Context.AutoConfigureContext(AContext);
  EndTransaction;
end;

{$ENDIF}

//
// { TIdHTTP }
//
// procedure TIdHTTP.DoRequest(const AMethod: TIdHTTPMethod; AURL: string;
// ASource, AResponseContent: TStream; AIgnoreReplies: array of Int16);
// var
// HasTransaction: Boolean;
// LName: string;
// begin
// HasTransaction := TElasticAPM4D.ExistsTransaction;
// LName := AURL;
// if not HasTransaction then
// begin
// TElasticAPM4D.StartTransaction('Indy', AURL);
// LName := 'DoRequest';
// end;
// // TElasticAPM4D.StartSpan(Self, LName);
// try
// Try
// inherited;
// except
// // on E: EIdHTTPProtocolException do
// // begin
// // TElasticAPM4D.AddError(Self, E);
// // raise;
// // end;
// on E: Exception do
// begin
// TElasticAPM4D.AddError(E);
// raise;
// end;
// end;
// Finally
// // TElasticAPM4D.EndSpan(Self);
// if not HasTransaction then
// TElasticAPM4D.EndTransaction;
// End;
// end;
//
// { TContext }
//
// constructor TContext.Create(AIdHTTP: TIdHTTP);
// var
// I: Integer;
// begin
// inherited Create;
// FIdHTTP := AIdHTTP;
//
// Page := TPage.Create;
// Page.referer := AIdHTTP.Request.referer;
// Page.url := AIdHTTP.Request.url;
//
// Request := TRequest.Create;
//
// Request.Method := AIdHTTP.Request.Method;
// Request.Http_version := AIdHTTP.Version;
// Request.Socket.encrypted := Assigned(AIdHTTP.Socket);
//
// Request.url.Hostname := AIdHTTP.url.Host;
// Request.url.full := AIdHTTP.url.GetFullURI;
// Request.url.protocol := AIdHTTP.url.protocol;
// Request.url.pathname := AIdHTTP.url.Path;
// Request.url.port := StrToIntDef(AIdHTTP.url.port, 0);
// Request.url.search := AIdHTTP.url.Params;
// Request.url.raw := AIdHTTP.url.Document;
//
// for I := 0 to pred(AIdHTTP.Request.CustomHeaders.Count) do
// Request.headers := Request.headers + ', ' + AIdHTTP.Request.CustomHeaders.Strings[I];
//
// if not Request.headers.isEmpty then
// Request.headers := Request.headers.Remove(1, 1);
// end;
//
// procedure TContext.FillResponse;
// begin
// Response := TResponse.Create;
// Response.finished := True;
// Response.headers_sent := FIdHTTP.Response.CustomHeaders.Count > 0;
// Response.status_code := FIdHTTP.ResponseCode;
// end;

end.
