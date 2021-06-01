unit ElasticAPM4D.Indy;

interface

uses
  IdHTTP, ElasticAPM4D.Request, ElasticAPM4D.Transaction;

function StartTransaction(AIdHTTP: TIdHTTP; const AType, AName: string): TTransaction;

implementation

uses
  SysUtils;

function StartTransaction(AIdHTTP: TIdHTTP; const AType, AName: string): TTransaction;
begin
  Result.Context.Request.url.Hostname := AIdHTTP.url.Host;
  Result.Context.Request.url.Full := AIdHTTP.url.GetFullURI;
  Result.Context.Request.url.Protocol := AIdHTTP.url.Protocol;
  Result.Context.Request.url.Pathname := AIdHTTP.url.Path;
  Result.Context.Request.url.port := StrToIntDef(AIdHTTP.url.port, 0);
  Result.Context.Request.url.Search := AIdHTTP.url.Params;
  Result.Context.Request.url.Raw := AIdHTTP.url.Document;
  Result.Context.Request.Method := AIdHTTP.Request.Method;
end;

end.
