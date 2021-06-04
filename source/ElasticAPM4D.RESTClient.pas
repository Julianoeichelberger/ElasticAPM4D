unit ElasticAPM4D.RESTClient;

interface

uses
  REST.Client,
  REST.Types,
  ElasticAPM4D.Transaction,
  ElasticAPM4D;

function StartTransaction(ARequest: TRESTRequest; const AName: string = ''; const ATraceId: string = ''): TTransaction;
procedure EndTransaction(AResponse: TRESTResponse);
function StartSpan(ARequest: TRESTRequest; const AName: string = ''): TSpan;
procedure EndSpan(AResponse: TRESTResponse);

implementation

uses
  ElasticAPM4D.Context, ElasticAPM4D.Request, ElasticAPM4D.Span, StrUtils, SysUtils;

function GetName(ARequest: TRESTRequest; const AName: string): string;
begin
  Result := IfThen(AName.IsEmpty, ARequest.Resource, AName);
  if Result.IsEmpty then
    Result := ARequest.Client.Name;
  if Result.IsEmpty then
    Result := 'RESTRequest';
end;

function StartTransaction(ARequest: TRESTRequest; const AName: string = ''; const ATraceId: string = ''): TTransaction;
begin
  Result := TElasticAPM4D.StartTransaction(GetName(ARequest, AName), 'Request', ATraceId);
  Result.Context := ElasticAPM4D.Context.TContext.Create;
  Result.Context.Request := TRequest.Create;
  Result.Context.Request.Url.Full := ARequest.GetFullRequestURL(True);
  Result.Context.Request.Url.Pathname := ARequest.ResourceSuffix + ARequest.Resource;
  Result.Context.Request.Url.Protocol := 'HTTP';
  Result.Context.Request.Method := RESTRequestMethodToString(ARequest.Method);
end;

procedure EndTransaction(AResponse: TRESTResponse);
begin
  TElasticAPM4D.Transaction.Context.Response := ElasticAPM4D.Context.TResponse.Create;
  TElasticAPM4D.Transaction.Context.Response.Status_code := AResponse.StatusCode;
  TElasticAPM4D.Transaction.Context.Response.Headers_sent := AResponse.Headers.Count > 0;
  TElasticAPM4D.Transaction.Result := 'HTTP ' + AResponse.StatusCode.ToString;
  TElasticAPM4D.EndTransaction;
end;

function StartSpan(ARequest: TRESTRequest; const AName: string): TSpan;
begin
  Result := TElasticAPM4D.StartSpan(GetName(ARequest, AName), 'Request');
  Result.Context.http := THttp.Create;
  Result.Context.http.Url := ARequest.Client.BaseURL;
  Result.Context.http.Method := RESTRequestMethodToString(ARequest.Method);
end;

procedure EndSpan(AResponse: TRESTResponse);
begin
  TElasticAPM4D.Span.Context.http.Status_code := AResponse.StatusCode;
  TElasticAPM4D.EndSpan;
end;

end.
