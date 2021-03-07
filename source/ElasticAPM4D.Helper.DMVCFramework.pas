unit ElasticAPM4D.Helper.DMVCFramework;

interface

uses
  ElasticAPM4D;

type
  TElasticAPM4D = class helper for ElasticAPM4D.TElasticAPM4D

{$IFDEF dmvcframework}
    class function StartTransaction(AActionName: string; AContext: TWebContext): TTransaction; overload;
    class procedure EndTransaction(const ARESTClient: MVCFramework.RESTClient.TRESTClient; const AResponse: IRESTResponse;
      const AHttpMethod: string); overload;
    class procedure EndTransaction(const AContext: TWebContext); overload;
{$ENDIF}
  end;

implementation


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
  CurrentTransaction.Context.AutoConfigureContext(AResponse);
  CurrentTransaction.Context.Request.url.full := ARESTClient.url;
  CurrentTransaction.Context.Request.Method := AHttpMethod;
  if AResponse.HasError then
  begin
    LError := GetError;

    LError.Exception.code := AResponse.Error.HTTPError.ToString;
    LError.Exception.&type := AResponse.Error.ExceptionClassname;
    LError.Exception.message := AResponse.Error.ExceptionMessage;

    AddError(LError);
    EndTransaction('Error');
  end
  else
    EndTransaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AContext: TWebContext);
begin
  CurrentTransaction.Context.AutoConfigureContext(AContext);
  EndTransaction;
end;

{$ENDIF}

end.
