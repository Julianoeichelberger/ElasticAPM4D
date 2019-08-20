unit ElasticAPM4D;

interface

uses
{$IFDEF dmvcframework}
  MVCFramework.RESTClient,
  MVCFramework,
{$ENDIF}
  IdHTTP,
  System.SysUtils,
  System.classes,
  Generics.Collections,
  ElasticAPM4D.Error,
  ElasticAPM4D.Span,
  ElasticAPM4D.Transaction,
  ElasticAPM4D.User,
  ElasticAPM4D.SendPackage;

type
  TElasticAPM4D = class
  strict private
    class threadvar FPackage: TElasticAPM4DSendPackage;
    class var FUser: TElasticAPM4DUser;
    class var FDataBase: TElasticAPM4DSpanContextDB;
  private
    class procedure Finalize;
    class function GetError: TElasticAPM4DError;
  public
    class procedure AddUser(AUserId, AUsername: string; AUserMail: string = ''); overload;
    class procedure AddDataBase(ADbType, ADbUser: string);

    class function Header: string;

    class function StartCustomTransaction(const AType, AName, AHeader: string): TElasticAPM4DTransaction;
    class function StarTransaction(const AName, AHeader: string): TElasticAPM4DTransaction; overload;
    class function StarTransaction(const AIdHttp: TIdCustomHTTP; const AName: string)
      : TElasticAPM4DTransaction; overload;

    class function CurrentTransaction: TElasticAPM4DTransaction;
    class procedure EndTransaction(const AResult: string = 'Sucess'); overload;

    class function StartCustomSpan(const AName: string; const AType: string = 'Method')
      : TElasticAPM4DSpan; overload;
    class function StartSpan(const AIdHttp: TIdCustomHTTP; const AName: string): TElasticAPM4DSpan; overload;
    class function StartSpan(const AName, ASQL: string): TElasticAPM4DSpan; overload;

    class function CurrentSpan: TElasticAPM4DSpan;
    class procedure EndSpan; overload;
    class procedure EndSpan(const AIdHttp: TIdCustomHTTP); overload;

    class procedure AddError(AError: TElasticAPM4DError); overload;
    class procedure AddError(E: Exception); overload;
    class procedure AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException); overload;
    class function StarTransaction(const AName: string): TElasticAPM4DTransaction; overload;
  end;

implementation

{ TElasticAPM4D }

uses
  ElasticAPM4D.Config;

class procedure TElasticAPM4D.AddUser(AUserId, AUsername, AUserMail: string);
begin
  if not Assigned(FUser) then
    FUser := TElasticAPM4DUser.Create;
  FUser.username := AUsername;
  FUser.id := AUserId;
  FUser.email := AUserMail;
end;

class procedure TElasticAPM4D.AddDataBase(ADbType, ADbUser: string);
begin
  if not Assigned(FDataBase) then
    FDataBase := TElasticAPM4DSpanContextDB.Create;

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

class function TElasticAPM4D.Header: string;
begin
  if not Assigned(FPackage) then
    raise EElasticAPM4DException.Create('Transaction not found');

  Result := FPackage.Header;
end;

class function TElasticAPM4D.StartCustomTransaction(const AType, AName, AHeader: string)
  : TElasticAPM4DTransaction;
begin
  if Assigned(FPackage) then
    raise EElasticAPM4DException.Create('Duplicate active transactions');

  FPackage := TElasticAPM4DSendPackage.Create;
  FPackage.Transaction.Start(AType, AName);

  FPackage.Transaction.Context.User.id := FUser.id;
  FPackage.Transaction.Context.User.username := FUser.username;
  FPackage.Transaction.Context.User.email := FUser.email;

  FPackage.Header := AHeader;

  Result := FPackage.Transaction;
end;

class function TElasticAPM4D.StarTransaction(const AName, AHeader: string): TElasticAPM4DTransaction;
begin
  Result := StartCustomTransaction('Request', AName, AHeader);
end;

class function TElasticAPM4D.StarTransaction(const AIdHttp: TIdCustomHTTP; const AName: string)
  : TElasticAPM4DTransaction;
begin
  Result := StartCustomTransaction('Request', AName, '');
  Result.Context.AutoCreatePage(AIdHttp);
  Result.Context.AutoCreateResponse(AIdHttp);
  Result.Context.AutoCreateRequest(AIdHttp);
end;

class function TElasticAPM4D.StarTransaction(const AName: string): TElasticAPM4DTransaction;
begin
  Result := StartCustomTransaction('Client', AName, '');
end;

class function TElasticAPM4D.StartSpan(const AName, ASQL: string): TElasticAPM4DSpan;
begin
  Result := StartCustomSpan(AName, 'Sql');
  Result.Context.db.statement := ASQL;
end;

class function TElasticAPM4D.CurrentTransaction: TElasticAPM4DTransaction;
begin
  if not Assigned(FPackage) then
    raise EElasticAPM4DException.Create('Current transaction not found');

  Result := FPackage.Transaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AResult: string);
begin
  if not Assigned(FPackage) then
    exit;

  FPackage.Transaction.Result := AResult;
  FPackage.Transaction.&End;

  FPackage.Send;
  FreeAndNil(FPackage);
end;

class function TElasticAPM4D.StartCustomSpan(const AName, AType: string): TElasticAPM4DSpan;
begin
  if FPackage.SpanIsOpen then
    Result := TElasticAPM4DSpan.Create(CurrentSpan)
  else
    Result := TElasticAPM4DSpan.Create(CurrentTransaction);
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

class function TElasticAPM4D.StartSpan(const AIdHttp: TIdCustomHTTP; const AName: string): TElasticAPM4DSpan;
begin
  Result := StartCustomSpan(AName, 'Request');
  AIdHttp.Request.CustomHeaders.AddValue('elastic-apm-traceparent', Header);
end;

class function TElasticAPM4D.CurrentSpan: TElasticAPM4DSpan;
begin
  Result := FPackage.CurrentSpan;
end;

class procedure TElasticAPM4D.EndSpan(const AIdHttp: TIdCustomHTTP);
begin
  if not FPackage.SpanIsOpen then
    exit;
  CurrentSpan.Context.http := TElasticAPM4DSpanContextHttp.Create;
  CurrentSpan.Context.http.method := AIdHttp.Request.method;
  CurrentSpan.Context.http.url := AIdHttp.Request.url;
  CurrentSpan.Context.http.status_code := AIdHttp.ResponseCode;
  EndSpan;
end;

class procedure TElasticAPM4D.EndSpan;
begin
  if not FPackage.SpanIsOpen then
    exit;

  CurrentSpan.&End;
  FPackage.OpenSpanStack.Delete(Pred(FPackage.OpenSpanStack.Count));
  FPackage.Transaction.span_count.Dec;
end;

class function TElasticAPM4D.GetError: TElasticAPM4DError;
begin
  if FPackage.SpanIsOpen then
    Result := TElasticAPM4DError.Create(CurrentSpan)
  else
    Result := TElasticAPM4DError.Create(CurrentTransaction);
end;

class procedure TElasticAPM4D.AddError(E: Exception);
var
  LError: TElasticAPM4DError;
begin
  LError := GetError;

  LError.Exception.&type := E.ClassName;
  LError.Exception.message := E.message;

  FPackage.ErrorList.Add(LError)
end;

class procedure TElasticAPM4D.AddError(AError: TElasticAPM4DError);
begin
  FPackage.ErrorList.Add(AError)
end;

class procedure TElasticAPM4D.AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException);
var
  LError: TElasticAPM4DError;
begin
  LError := GetError;

  LError.Exception.code := E.ErrorCode.ToString;
  LError.Exception.&type := E.ClassName;
  LError.Exception.message := E.message;

  LError.Context.AutoCreatePage(AIdHttp);
  LError.Context.AutoCreateResponse(AIdHttp);
  LError.Context.AutoCreateRequest(AIdHttp);

  FPackage.ErrorList.Add(LError);
end;

initialization

TElasticAPM4DConfig.InitializeFile;

finalization

TElasticAPM4D.Finalize;
TElasticAPM4DConfig.RealeseFile;

end.
