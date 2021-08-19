{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{*******************************************************}
unit ElasticAPM4D;

interface

uses
  System.Rtti, System.SysUtils, System.classes, System.Generics.Collections, IdHTTP,
  Span, Transaction, Metadata, Error, Share.Types, REST.Client,
  ElasticAPM4D.DataController, ElasticAPM4D.Config;

type
  TTransaction = Transaction.TTransaction;
  TSpan = Span.TSpan;
  TOutcome = Share.Types.TOutcome;
  TConfig = ElasticAPM4D.Config.TConfig;

  TApm = class
  strict protected
    class threadvar FData: TDataController;
    class function GetErrorInstance: TError; static;
  public const
    HeaderKey = 'elastic-apm-traceparent';
  public
    class function HeaderValue: string; static;

    class function StartTransaction(const AName: string;
      const AType: string = 'Undefined'; const ATraceId: string = ''): TTransaction; static;
    class function StartTransactionRequest(const AResource: string; const AMethod: string = 'GET';
      const ATraceId: string = ''): TTransaction; overload; static;
    class function StartTransactionRequest(const ARequest: TRESTRequest): TTransaction; overload; static;

    class function ExistsTransaction: Boolean; static;
    class function Transaction: TTransaction; static;
    class procedure EndTransaction(const AOutcome: TOutcome = success); overload; static;
    class procedure EndTransaction(const AResponse: TCustomRESTResponse); overload; static;

    class function StartSpan(const AName: string; const AType: string = 'Method'): TSpan; static;
    class function StartSpanSQL(const AName, ASQL: string): TSpan; static;
    class function StartSpanRequest(const AResource: string; const AMethod: string): TSpan; static;
    class function Span: TSpan; static;
    class procedure EndSpan; overload; static;
    class procedure EndSpan(const StatusCode: Integer); overload; static;

    class procedure AddError(AError: TError); overload; static;
    class procedure AddError(E: Exception); overload; static;
    class procedure AddError(E: EIdHTTPProtocolException); overload; static;
    class procedure AddError(AResponse: TCustomRESTResponse); overload; static;
  end;

implementation

uses
  Share.Context;

{ TElasticAPM4D }

class function TApm.HeaderValue: string;
begin
  if not Assigned(FData) then
    Exit('');

  Result := FData.Header;
end;

class function TApm.StartTransaction(const AName, AType, ATraceId: string): TTransaction;
begin
  if ExistsTransaction then
    EndTransaction(unknown);

  FData := TDataController.Create;
  FData.Transaction.Start(AName, AType);

  if not ATraceId.IsEmpty then
    FData.Transaction.trace_id := ATraceId;

  Result := FData.Transaction;
end;

class function TApm.StartTransactionRequest(const ARequest: TRESTRequest): TTransaction;
var
  Name: string;
begin
  Name := ARequest.Resource;
  if Name.IsEmpty then
    Name := ARequest.Client.BaseURL;
  Result := StartTransactionRequest(Name);
end;

class function TApm.StartTransactionRequest(const AResource, AMethod, ATraceId: string): TTransaction;
begin
  Result := StartTransaction(AResource, 'Request', ATraceId);
  Result.AddContextRequest(AMethod);
end;

class function TApm.Transaction: TTransaction;
begin
  if not Assigned(FData) then
    raise ETransactionNotFound.Create('Transaction not found');

  Result := FData.Transaction;
end;

class procedure TApm.EndTransaction(const AOutcome: TOutcome);
begin
  if not Assigned(FData) then
    Exit;

  if (AOutcome = success) and (FData.ErrorList.Count > 0) then
    FData.Transaction.SetOutcome(failure);
  try
    FData.Transaction.ToEnd(AOutcome);
    FData.ToQueue;
  finally
    FreeAndNil(FData);
  end;
end;

class procedure TApm.EndTransaction(const AResponse: TCustomRESTResponse);
begin
  if not Assigned(FData) then
    Exit;

  FData.Transaction.Context.AddResponse(AResponse.StatusCode);
  FData.Transaction.Result := AResponse.StatusCode.ToString + ' ' + AResponse.StatusText;
  try
    if AResponse.StatusCode >= 500 then
      FData.Transaction.ToEnd(failure)
    else
      FData.Transaction.ToEnd;
    FData.ToQueue;
  finally
    FreeAndNil(FData);
  end;
end;

class function TApm.ExistsTransaction: Boolean;
begin
  Result := Assigned(FData);
end;

class function TApm.StartSpan(const AName, AType: string): TSpan;
begin
  Result := FData.StartSpan(AName, AType);
end;

class function TApm.StartSpanSQL(const AName, ASQL: string): TSpan;
begin
  Result := StartSpan(AName, 'Sql');
  Result.Context.db.AddSQL(ASQL);
end;

class function TApm.StartSpanRequest(const AResource, AMethod: string): TSpan;
begin
  Result := StartSpan(AResource, 'Request');
  Result.Context.CreateHttp(AMethod);
end;

class function TApm.Span: TSpan;
begin
  Result := FData.CurrentSpan;
end;

class procedure TApm.EndSpan;
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  FData.EndSpan;
end;

class procedure TApm.EndSpan(const StatusCode: Integer);
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  FData.CurrentSpan.Context.Http.AddStatusCode(StatusCode);
  FData.Transaction.Result := StatusCode.ToString;
  FData.EndSpan;
end;

class function TApm.GetErrorInstance: TError;
begin
  if FData.SpanIsOpened then
    Result := TError.Create(Span.trace_id, Span.transaction_id, Span.id)
  else
    Result := TError.Create(Transaction.trace_id, Transaction.id, Transaction.id);
end;

class procedure TApm.AddError(E: Exception);
var
  Error: TError;
begin
  if not Assigned(FData) then
    Exit;

  Error := GetErrorInstance;

  Error.Exception.&type := E.ClassName;
  Error.Exception.message := E.message;
  FData.Transaction.Result := E.message;

  AddError(Error);
end;

class procedure TApm.AddError(AError: TError);
begin
  if not Assigned(FData) then
    Exit;

  FData.ErrorList.Add(AError);
end;

class procedure TApm.AddError(AResponse: TCustomRESTResponse);
var
  Error: TError;
begin
  if not Assigned(FData) then
    Exit;

  Error := GetErrorInstance;
  Error.Exception.Code := AResponse.StatusCode.ToString;
  Error.Exception.message := AResponse.ErrorMessage;
  Error.Exception.&type := AResponse.StatusText;
  Error.Context.AddResponse(AResponse.StatusCode);
  FData.Transaction.Result := AResponse.StatusCode.ToString + ' ' + AResponse.StatusText;

  AddError(Error);
end;

class procedure TApm.AddError(E: EIdHTTPProtocolException);
var
  Error: TError;
begin
  if not Assigned(FData) then
    Exit;

  Error := GetErrorInstance;

  Error.Exception.Code := E.ErrorCode.ToString;
  Error.Exception.message := E.ErrorMessage;
  Error.Exception.&type := E.ClassName;
  Error.Context.AddResponse(E.ErrorCode);
  FData.Transaction.Result := Error.Exception.Code + ' ' + E.ErrorMessage;

  AddError(Error);
end;

end.
