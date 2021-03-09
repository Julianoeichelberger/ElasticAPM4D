unit ElasticAPM4D;

interface

uses
{$IFDEF dmvcframework}
  MVCFramework.RESTClient,
  MVCFramework,
{$ENDIF}
  System.Rtti, System.SysUtils, System.classes, System.Generics.Collections,
  ElasticAPM4D.Resources, ElasticAPM4D.Error, ElasticAPM4D.Span, ElasticAPM4D.Transaction,
  ElasticAPM4D.User, ElasticAPM4D.Package;

type
  TTransaction = ElasticAPM4D.Transaction.TTransaction;

  TElasticAPM4D = class
  strict private
    class threadvar FPackage: TPackage;
  private
    class procedure Finalize;
  protected
    class var FUser: TUser;
    class var FDataBase: TDB;
    class function GetError: TError;
  public
    class procedure AddUser(AUserId, AUsername: string; AUserMail: string = ''); overload;
    class procedure AddDataBase(ADbType, ADbUser: string);

    class function HeaderKey: string;
    class function HeaderValue: string;

    class function StartTransaction(const AType, AName: string; const ATraceId: string = ''): TTransaction;
    class function ExistsTransaction: Boolean;
    class function CurrentTransaction: TTransaction;
    class procedure EndTransaction(const AResult: string = sDEFAULT_RESULT); overload;

    class function StartCustomSpan(const AName: string; const AType: string = 'Method'): TSpan; overload;
    class function StartSpan(const AName, ASQL: string): TSpan; overload;
    class function CurrentSpan: TSpan;
    class procedure EndSpan; overload;

    class procedure AddError(AError: TError); overload;
    class procedure AddError(E: Exception); overload;
  end;

implementation

{ TElasticAPM4D }

uses
  ElasticAPM4D.Utils, ElasticAPM4D.Context;

class procedure TElasticAPM4D.AddUser(AUserId, AUsername, AUserMail: string);
begin
  if not Assigned(FUser) then
    FUser := TUser.Create;
  FUser.username := AUsername;
  FUser.id := AUserId;
  FUser.email := AUserMail;
end;

class procedure TElasticAPM4D.AddDataBase(ADbType, ADbUser: string);
begin
  if not Assigned(FDataBase) then
    FDataBase := TDB.Create;

  FDataBase.&type := ADbType;
  FDataBase.User := ADbUser;
end;

class procedure TElasticAPM4D.Finalize;
begin
  if Assigned(FUser) then
    FUser.Free;
  if Assigned(FDataBase) then
    FDataBase.Free;
end;

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

class function TElasticAPM4D.StartTransaction(const AType, AName, ATraceId: string): TTransaction;
begin
  if ExistsTransaction then
    EndTransaction('UnFineshed');

  FPackage := TPackage.Create;
  FPackage.Transaction.Start(AType, AName);

  if Assigned(FUser) then
  begin
    FPackage.Transaction.Context.User.id := FUser.id;
    FPackage.Transaction.Context.User.username := FUser.username;
    FPackage.Transaction.Context.User.email := FUser.email;
  end;

  if not ATraceId.IsEmpty then
    FPackage.Transaction.trace_id := ATraceId;

  Result := FPackage.Transaction;
end;

class function TElasticAPM4D.StartSpan(const AName, ASQL: string): TSpan;
begin
  Result := StartCustomSpan(AName, 'Sql');
  Result.Context.db.statement := ASQL;
end;

class function TElasticAPM4D.CurrentTransaction: TTransaction;
begin
  if not Assigned(FPackage) then
    raise EElasticAPM4DException.Create(sTransactionNotFount);

  Result := FPackage.Transaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AResult: string);
begin
  if not Assigned(FPackage) then
    Exit;

  FPackage.Transaction.Result := AResult;
  if (AResult = sDEFAULT_RESULT) and (FPackage.ErrorList.Count > 0) then
    FPackage.Transaction.Result := 'Failure';
  try
    FPackage.Transaction.&End;
    FPackage.ToSend;
  finally
    FreeAndNil(FPackage);
  end;
end;

class function TElasticAPM4D.ExistsTransaction: Boolean;
begin
  Result := Assigned(FPackage);
end;

class function TElasticAPM4D.StartCustomSpan(const AName, AType: string): TSpan;
begin
  if FPackage.SpanIsOpen then
    Result := TSpan.Create(CurrentSpan.trace_id, CurrentSpan.transaction_id, CurrentSpan.id)
  else
    Result := TSpan.Create(CurrentTransaction.trace_id, CurrentTransaction.id, CurrentTransaction.id);
  Result.Start;
  Result.name := AName;
  Result.&type := AType;
  if Assigned(FDataBase) then
  begin
    Result.Context.db.instance := FDataBase.instance;
    Result.Context.db.link := FDataBase.link;
    Result.Context.db.statement := FDataBase.statement;
    Result.Context.db.&type := FDataBase.&type;
    Result.Context.db.User := FDataBase.User;
  end;
  FPackage.SpanList.Add(Result);
  FPackage.OpenSpanStack.Add(Result);
  CurrentTransaction.span_count.Inc;
end;

// class function TElasticAPM4D.StartSpan(const AIdHttp: TIdCustomHTTP; const AName: string): TSpan;
// begin
// Result := StartCustomSpan(AName, 'Request');
// AIdHttp.Request.CustomHeaders.AddValue(HeaderKey, HeaderValue);
// end;

class function TElasticAPM4D.CurrentSpan: TSpan;
begin
  Result := FPackage.CurrentSpan;
end;

// class procedure TElasticAPM4D.EndSpan(const AIdHttp: TIdCustomHTTP);
// begin
// if not ExistsTransaction then
// exit;
//
// if not FPackage.SpanIsOpen then
// exit;
//
// CurrentSpan.action := AIdHttp.Request.Method;
// CurrentSpan.Context.AutoCreateHttp(AIdHttp);
// EndSpan;
// end;

class procedure TElasticAPM4D.EndSpan;
begin
  if not ExistsTransaction then
    Exit;

  CurrentSpan.&End;
  FPackage.OpenSpanStack.Delete(Pred(FPackage.OpenSpanStack.Count));
  FPackage.Transaction.span_count.Dec;
end;

class function TElasticAPM4D.GetError: TError;
begin
  if FPackage.SpanIsOpen then
    Result := TError.Create(CurrentSpan.trace_id, CurrentSpan.transaction_id, CurrentSpan.id)
  else
    Result := TError.Create(CurrentTransaction.trace_id, CurrentTransaction.id, CurrentTransaction.id);
end;

class procedure TElasticAPM4D.AddError(E: Exception);
var
  LError: TError;
begin
  if not Assigned(FPackage) then
    Exit;

  LError := GetError;

  LError.Exception.&type := E.ClassName;
  LError.Exception.message := E.message;

  FPackage.ErrorList.Add(LError)
end;

class procedure TElasticAPM4D.AddError(AError: TError);
begin
  if not Assigned(FPackage) then
    Exit;

  FPackage.ErrorList.Add(AError);
end;

// class procedure TElasticAPM4D.AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException);
// var
// LError: TError;
// begin
// if not Assigned(FPackage) then
// exit;
//
// LError := GetError;
//
// // LError.AutoConfigureError(AIdHttp);
//
// LError.Exception.code := E.ErrorCode.ToString;
// LError.Exception.&type := E.ClassName;
// LError.Exception.message := E.message;
//
// FPackage.ErrorList.Add(LError);
// end;

initialization

// TElasticAPM4DConfig.InitializeFile;

finalization

TElasticAPM4D.Finalize;

end.
