unit ElasticAPM4D;

interface

uses
  IdHTTP,
  System.SysUtils,
  System.classes,
  Generics.Collections,
  ElasticAPM4D.Error,
  ElasticAPM4D.Metadata,
  ElasticAPM4D.Span,
  ElasticAPM4D.Transaction,
  ElasticAPM4D.User;

type
  EElasticAPM4DException = Exception;

  TElasticAPM4D = class
  strict private
    class var FEnabled: Boolean;
    class var FMetadata: TElasticAPM4DMetadata;
    class var FTransaction: TElasticAPM4DTransaction;
    class var FSpanList: TObjectList<TElasticAPM4DSpan>;
    class var FErrorList: TObjectList<TElasticAPM4DError>;
    class var FOpenSpanStack: TList;
    class var FUser: TElasticAPM4DUser;
    class var FDataBase: TElasticAPM4DSpanContextDB;
    class var FVersion: string;

    class procedure SendJson;
    class function SpanIsOpen: Boolean;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure AddUser(AUser: TElasticAPM4DUser);
    class procedure AddDataBase(ADataBase: TElasticAPM4DSpanContextDB);
    class procedure AddVersion(const AVersion: string);

    class function StarTransaction(const AType, AName: string): TElasticAPM4DTransaction; overload;
    class function StarTransaction(const AIdHttp: TIdCustomHTTP; const AName: string;
      const AType: string = 'Request'): TElasticAPM4DTransaction; overload;

    class function CurrentTransaction: TElasticAPM4DTransaction;
    class procedure EndTransaction(const AResult: string = 'Sucess');

    class function StartSpan(const AName: string; const AType: string = 'Method'): TElasticAPM4DSpan;
    class function CurrentSpan: TElasticAPM4DSpan;
    class procedure EndSpan;

    class procedure AddError(AError: TElasticAPM4DError); overload;
    class procedure AddError(E: Exception); overload;
    class procedure AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException); overload;
  end;

implementation

{ TElasticAPM4D }

uses
  ElasticAPM4D.ndJson,
  ElasticAPM4D.SendThread,
  ElasticAPM4D.Config;

class procedure TElasticAPM4D.AddUser(AUser: TElasticAPM4DUser);
begin
  FUser := AUser;
end;

class procedure TElasticAPM4D.AddVersion(const AVersion: string);
begin
  FVersion := AVersion;
end;

class constructor TElasticAPM4D.Create;
begin
  FEnabled := TElasticAPM4DConfig.Enabled;
  FOpenSpanStack := TList.Create;
  FVersion := '';
end;

class destructor TElasticAPM4D.Destroy;
begin
  FOpenSpanStack.Free;
  if Assigned(FUser) then
    FUser.Free;
end;

class function TElasticAPM4D.StarTransaction(const AType, AName: string): TElasticAPM4DTransaction;
begin
  if Assigned(FTransaction) then
    raise EElasticAPM4DException.Create('Duplicate active transactions');

  FMetadata := TElasticAPM4DMetadata.Create;

  Result := TElasticAPM4DTransaction.Create;
  Result.Start(AType, AName);
  if Assigned(FUser) then
  begin
    Result.Context.User.id := FUser.id;
    Result.Context.User.username := FUser.username;
    Result.Context.User.email := FUser.email;
  end;
  FTransaction := Result;
end;

class function TElasticAPM4D.StarTransaction(const AIdHttp: TIdCustomHTTP; const AName, AType: string)
  : TElasticAPM4DTransaction;
begin
  Result := StarTransaction(AType, AName);
  // sHEADER
end;

class function TElasticAPM4D.CurrentTransaction: TElasticAPM4DTransaction;
begin
  if not Assigned(FTransaction) then
    raise EElasticAPM4DException.Create('Current transaction not found');

  Result := FTransaction;
end;

class procedure TElasticAPM4D.EndTransaction(const AResult: string);
begin
  if not Assigned(FTransaction) then
    exit;

  FTransaction.Result := AResult;
  FTransaction.&End;

  SendJson;

  FTransaction.Free;
  FMetadata.Free;
end;

class function TElasticAPM4D.StartSpan(const AName, AType: string): TElasticAPM4DSpan;
begin
  if not Assigned(FSpanList) then
    FSpanList := TObjectList<TElasticAPM4DSpan>.Create;

  if SpanIsOpen then
    Result := TElasticAPM4DSpan.Create(CurrentSpan)
  else
    Result := TElasticAPM4DSpan.Create(CurrentTransaction);
  Result.Start;
  Result.name := AName;
  Result.&type := AType;
  FSpanList.Add(Result);
  FOpenSpanStack.Add(Result);
  FTransaction.span_count.Inc;
end;

class function TElasticAPM4D.CurrentSpan: TElasticAPM4DSpan;
begin
  if not Assigned(FSpanList) or not SpanIsOpen then
    raise EElasticAPM4DException.Create('Current span not found');

  Result := FOpenSpanStack.Items[Pred(FOpenSpanStack.Count)];
end;

class procedure TElasticAPM4D.EndSpan;
begin
  if not Assigned(FSpanList) then
    exit;

  CurrentSpan.&End;
  FOpenSpanStack.Delete(Pred(FOpenSpanStack.Count));
  FTransaction.span_count.Dec;
end;

class procedure TElasticAPM4D.AddError(E: Exception);
var
  LError: TElasticAPM4DError;
begin
  if SpanIsOpen then
    LError := TElasticAPM4DError.Create(CurrentSpan)
  else
    LError := TElasticAPM4DError.Create(CurrentTransaction);

  if not Assigned(FErrorList) then
    FErrorList := TObjectList<TElasticAPM4DError>.Create;

  LError.Exception.&type := E.ClassName;
  LError.Exception.message := E.message;

  FErrorList.Add(LError)
end;

class procedure TElasticAPM4D.AddError(AError: TElasticAPM4DError);
begin
  if not Assigned(FErrorList) then
    FErrorList := TObjectList<TElasticAPM4DError>.Create;

  FErrorList.Add(AError)
end;

class procedure TElasticAPM4D.AddDataBase(ADataBase: TElasticAPM4DSpanContextDB);
begin
  FDataBase := ADataBase;
end;

class procedure TElasticAPM4D.AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException);
var
  LError: TElasticAPM4DError;
begin
  if SpanIsOpen then
    LError := TElasticAPM4DError.Create(CurrentSpan)
  else
    LError := TElasticAPM4DError.Create(CurrentTransaction);

  if not Assigned(FErrorList) then
    FErrorList := TObjectList<TElasticAPM4DError>.Create;

  LError.Exception.code := E.ErrorCode.ToString;

  LError.Context.AutoCreatePage(AIdHttp);
  LError.Context.AutoCreateResponse(AIdHttp);
  LError.Context.AutoCreateRequest(AIdHttp);

  FErrorList.Add(LError);
end;

class procedure TElasticAPM4D.SendJson;
var
  LndJson: TElasticAPM4DndJson;
  LThread: TElasticAPM4DSendThread;
begin
  if not FEnabled then
    exit;
  LndJson := TElasticAPM4DndJson.Create;
  try
    LndJson.Add(FMetadata);
    LndJson.Add(FTransaction);
    LndJson.Add(FSpanList);
    LndJson.Add(FErrorList);

    LThread := TElasticAPM4DSendThread.Create(TElasticAPM4DConfig.URL);
    LThread.Send('', LndJson.Get);
  finally
    LndJson.Free;
  end;
end;

class function TElasticAPM4D.SpanIsOpen: Boolean;
begin
  Result := FOpenSpanStack.Count > 0;
end;

end.
