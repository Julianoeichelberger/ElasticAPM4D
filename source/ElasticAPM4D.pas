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
    class threadvar FMetadata: TElasticAPM4DMetadata;
    class threadvar FTransaction: TElasticAPM4DTransaction;
    class threadvar FSpanList: TObjectList<TElasticAPM4DSpan>;
    class threadvar FErrorList: TObjectList<TElasticAPM4DError>;
    class threadvar FOpenSpanStack: TList;
    class threadvar FUser: TElasticAPM4DUser;
    class var FDataBase: TElasticAPM4DSpanContextDB;
    class var FVersion: string;
    class var FEnabled: Boolean;

    class procedure SendJson;
    class function SpanIsOpen: Boolean;
    class procedure KeepCreateLists;
    class procedure KeepFreeLists;
  private
    class procedure Initialize;
    class procedure Finalize;
  public
    class procedure AddUser(AUser: TElasticAPM4DUser); overload;
    class procedure AddUser(AUsername: string); overload;
    class procedure AddDataBase(ADataBase: TElasticAPM4DSpanContextDB);
    class procedure AddVersion(const AVersion: string);

    class function GetHeader: string;

    class function StartCustomTransaction(const AType, AName, AHeader: string): TElasticAPM4DTransaction;
    class function StarTransaction(const AName: string): TElasticAPM4DTransaction; overload;
    class function StarTransaction(const AName, AHeader: string): TElasticAPM4DTransaction; overload;
    class function StarTransaction(const AIdHttp: TIdCustomHTTP; const AName: string)
      : TElasticAPM4DTransaction; overload;

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
  ElasticAPM4D.Config,
  ElasticAPM4D.Resources;

class procedure TElasticAPM4D.AddUser(AUser: TElasticAPM4DUser);
begin
  FUser := AUser;
end;

class procedure TElasticAPM4D.AddUser(AUsername: string);
begin
  if not Assigned(FUser) then
    FUser := TElasticAPM4DUser.Create;
  FUser.username := AUsername;
end;

class procedure TElasticAPM4D.AddVersion(const AVersion: string);
begin
  FVersion := AVersion;
end;

class procedure TElasticAPM4D.AddDataBase(ADataBase: TElasticAPM4DSpanContextDB);
begin
  FDataBase := ADataBase;
end;

class procedure TElasticAPM4D.Initialize;
begin
  FEnabled := TElasticAPM4DConfig.Enabled;
  FVersion := '';
end;

class procedure TElasticAPM4D.KeepCreateLists;
begin
  if not Assigned(FSpanList) then
  begin
    FSpanList := TObjectList<TElasticAPM4DSpan>.Create;
    FOpenSpanStack := TList.Create;
    FErrorList := TObjectList<TElasticAPM4DError>.Create;
  end;
end;

class procedure TElasticAPM4D.KeepFreeLists;
begin
  if Assigned(FSpanList) then
    FreeAndNil(FSpanList);
  if Assigned(FErrorList) then
    FreeAndNil(FErrorList);
  if Assigned(FOpenSpanStack) then
    FOpenSpanStack.Free;
end;

class procedure TElasticAPM4D.Finalize;
begin
  if Assigned(FUser) then
    FUser.Free;
  if Assigned(FDataBase) then
    FDataBase.Free;
end;

class function TElasticAPM4D.GetHeader: string;
begin
  if SpanIsOpen then
    Result := Format(sHEADER, [CurrentTransaction.trace_id, CurrentSpan.id])
  else
    Result := Format(sHEADER, [CurrentTransaction.trace_id, CurrentTransaction.id]);
end;

class function TElasticAPM4D.StartCustomTransaction(const AType, AName, AHeader: string)
  : TElasticAPM4DTransaction;
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

class function TElasticAPM4D.StarTransaction(const AName: string): TElasticAPM4DTransaction;
begin
  Result := StartCustomTransaction('Client', AName, '');
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
  KeepFreeLists;
end;

class function TElasticAPM4D.StartSpan(const AName, AType: string): TElasticAPM4DSpan;
begin
  KeepCreateLists;
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
  if not SpanIsOpen then
    exit;

  CurrentSpan.&End;
  FOpenSpanStack.Delete(Pred(FOpenSpanStack.Count));
  FTransaction.span_count.Dec;
end;

class procedure TElasticAPM4D.AddError(E: Exception);
var
  LError: TElasticAPM4DError;
begin
  KeepCreateLists;
  if SpanIsOpen then
    LError := TElasticAPM4DError.Create(CurrentSpan)
  else
    LError := TElasticAPM4DError.Create(CurrentTransaction);

  LError.Exception.&type := E.ClassName;
  LError.Exception.message := E.message;

  FErrorList.Add(LError)
end;

class procedure TElasticAPM4D.AddError(AError: TElasticAPM4DError);
begin
  KeepCreateLists;
  FErrorList.Add(AError)
end;

class procedure TElasticAPM4D.AddError(AIdHttp: TIdCustomHTTP; E: EIdHTTPProtocolException);
var
  LError: TElasticAPM4DError;
begin
  KeepCreateLists;

  if SpanIsOpen then
    LError := TElasticAPM4DError.Create(CurrentSpan)
  else
    LError := TElasticAPM4DError.Create(CurrentTransaction);

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

initialization

TElasticAPM4DConfig.InitializeFile;
TElasticAPM4D.Initialize;

finalization

TElasticAPM4D.Finalize;
TElasticAPM4DConfig.RealeseFile;

end.
