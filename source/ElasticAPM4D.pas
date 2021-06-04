unit ElasticAPM4D;

interface

uses
  System.Rtti, System.SysUtils, System.classes, System.Generics.Collections,
  ElasticAPM4D.Resources, ElasticAPM4D.Error, ElasticAPM4D.Span, ElasticAPM4D.Transaction,
  ElasticAPM4D.Utils, ElasticAPM4D.Package, IdHTTP;

type
  TTransaction = ElasticAPM4D.Transaction.TTransaction;
  TSpan = ElasticAPM4D.Span.TSpan;
  TOutcome = ElasticAPM4D.Utils.TOutcome;
  TConfig = ElasticAPM4D.Utils.TConfig;

  TElasticAPM4D = class
  strict private
    class threadvar FPackage: TPackage;
  protected
    class function GetError: TError;
  public
    class function HeaderKey: string;
    class function HeaderValue: string;

    class function StartTransaction(const AName: string;
      const AType: string = 'Undefined'; const ATraceId: string = ''): TTransaction;
    class function ExistsTransaction: Boolean;
    class function Transaction: TTransaction;
    class procedure EndTransaction(const AOutcome: TOutcome = success); overload;

    class function StartSpan(const AName: string; const AType: string = 'Method'): TSpan; overload;
    class function StartSpanSQL(const AName, ASQL: string): TSpan; overload;
    class function Span: TSpan;
    class procedure EndSpan; overload;

    class procedure AddError(AError: TError); overload;
    class procedure AddError(E: Exception); overload;
    class procedure AddError(E: EIdHTTPProtocolException); overload;
  end;

implementation

{ TElasticAPM4D }

class function TElasticAPM4D.HeaderValue: string;
begin
  if not Assigned(FPackage) then
    Exit('');

  Result := FPackage.Header;
end;

class function TElasticAPM4D.HeaderKey: string;
begin
  Result := sHEADER_KEY;
end;

class function TElasticAPM4D.StartTransaction(const AName, AType, ATraceId: string): TTransaction;
begin
  if ExistsTransaction then
    EndTransaction(unknown);

  FPackage := TPackage.Create;
  FPackage.Transaction.Start(AType, AName);

  if not ATraceId.IsEmpty then
    FPackage.Transaction.trace_id := ATraceId;

  Result := FPackage.Transaction;
end;

class function TElasticAPM4D.StartSpanSQL(const AName, ASQL: string): TSpan;
begin
  Result := StartSpan(AName, 'Sql');
  Result.Context.db.statement := ASQL;
end;

class function TElasticAPM4D.Transaction: TTransaction;
begin
  if not Assigned(FPackage) then
    raise EElasticAPM4DException.Create(sTransactionNotFount);

  Result := FPackage.Transaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AOutcome: TOutcome);
begin
  if not Assigned(FPackage) then
    Exit;

  if (AOutcome = success) and (FPackage.ErrorList.Count > 0) then
    FPackage.Transaction.SetOutcome(failure);
  try
    FPackage.Transaction.&End(AOutcome);
    FPackage.ToSend;
  finally
    FreeAndNil(FPackage);
  end;
end;

class function TElasticAPM4D.ExistsTransaction: Boolean;
begin
  Result := Assigned(FPackage);
end;

class function TElasticAPM4D.StartSpan(const AName, AType: string): TSpan;
begin
  if FPackage.SpanIsOpened then
    Result := TSpan.Create(Span.trace_id, Span.transaction_id, Span.id)
  else
    Result := TSpan.Create(Transaction.trace_id, Transaction.id, Transaction.id);
  Result.Start(AName, AType);

  Result.Context.db.instance := TConfig.GetDatabaseInstance;
  Result.Context.db.&type := TConfig.GetDatabase;
  Result.Context.db.User := TConfig.GetDatabaseUser;

  FPackage.SpanList.Add(Result);
  FPackage.OpenSpanStack.Add(Result);
  Transaction.span_count.Inc;
end;

class function TElasticAPM4D.Span: TSpan;
begin
  Result := FPackage.CurrentSpan;
end;

class procedure TElasticAPM4D.EndSpan;
begin
  if not ExistsTransaction then
    Exit;

  Span.&End;
  FPackage.OpenSpanStack.Delete(Pred(FPackage.OpenSpanStack.Count));
  FPackage.Transaction.span_count.Dec;
end;

class function TElasticAPM4D.GetError: TError;
begin
  if FPackage.SpanIsOpened then
    Result := TError.Create(Span.trace_id, Span.transaction_id, Span.id)
  else
    Result := TError.Create(Transaction.trace_id, Transaction.id, Transaction.id);
end;

class procedure TElasticAPM4D.AddError(E: Exception);
var
  Error: TError;
begin
  if not Assigned(FPackage) then
    Exit;

  Error := GetError;

  Error.Exception.&type := E.ClassName;
  Error.Exception.message := E.message;

  FPackage.ErrorList.Add(Error)
end;

class procedure TElasticAPM4D.AddError(AError: TError);
begin
  if not Assigned(FPackage) then
    Exit;

  FPackage.ErrorList.Add(AError);
end;

class procedure TElasticAPM4D.AddError(E: EIdHTTPProtocolException);
var
  Error: TError;
begin
  if not Assigned(FPackage) then
    Exit;

  Error := GetError;

  Error.Exception.Code := E.ErrorCode.ToString;
  Error.Exception.message := E.ErrorMessage;
  Error.Exception.&type := E.ClassName;

  FPackage.ErrorList.Add(Error)
end;

end.
