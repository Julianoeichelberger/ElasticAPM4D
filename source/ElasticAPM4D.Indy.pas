unit ElasticAPM4D.Indy;

interface

uses
  IdHTTP, ElasticAPM4D.Request, ElasticAPM4D.Transaction, ElasticAPM4D;

function StartTransaction(AIdHTTP: TIdHTTP; const AName: string; const ATraceId: string = ''): TTransaction;
procedure EndTransaction(AIdHTTP: TIdHTTP);
function StartSpan(AName: string): TSpan;
procedure EndSpan(AIdHTTP: TIdHTTP);

implementation

uses
  ElasticAPM4D.Context, ElasticAPM4D.Span, SysUtils;

function StartTransaction(AIdHTTP: TIdHTTP; const AName: string; const ATraceId: string): TTransaction;
begin
  Result := TElasticAPM4D.StartTransaction(AName, 'Request', ATraceId);
  Result.Context := ElasticAPM4D.Context.TContext.Create;
  Result.Context.Request := TRequest.Create;
  Result.Context.Request.url.Hostname := AIdHTTP.url.Host;
  Result.Context.Request.url.Protocol := AIdHTTP.url.Protocol;
  Result.Context.Request.url.Pathname := AIdHTTP.url.Path;
  Result.Context.Request.url.port := StrToIntDef(AIdHTTP.url.port, 0);
  Result.Context.Request.url.Search := AIdHTTP.url.Params;
  Result.Context.Request.url.Raw := AIdHTTP.url.Document;
end;

procedure EndTransaction(AIdHTTP: TIdHTTP);
begin
  if not TElasticAPM4D.ExistsTransaction then
    exit;

  TElasticAPM4D.Transaction.Context.Request.Method := AIdHTTP.Request.Method;
  TElasticAPM4D.Transaction.Context.Response := ElasticAPM4D.Context.TResponse.Create;
  TElasticAPM4D.Transaction.Context.Response.finished := True;
  TElasticAPM4D.Transaction.Context.Response.headers_sent := AIdHTTP.Request.CustomHeaders.Count > 0;
  TElasticAPM4D.Transaction.Context.Response.status_code := AIdHTTP.ResponseCode;
  TElasticAPM4D.Transaction.Result := 'HTTP ' + AIdHTTP.ResponseCode.ToString;
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

end.
