{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit ElasticAPM4D;

interface

uses
  System.Rtti, System.SysUtils, System.classes, System.Generics.Collections, IdHTTP,
  Span, Transaction, Metadata, Error, Share.Types, REST.Client,
  ElasticAPM4D.DataController, ElasticAPM4D.Config, Elastic.Log;

type
  TTransaction = Transaction.TTransaction;
  TSpan = Span.TSpan;
  TOutcome = Share.Types.TOutcome;
  TConfig = ElasticAPM4D.Config.TConfig;
  TLogLevel = ElasticAPM4D.Config.TLogLevel;

  /// <summary>
  /// The main class of the ElasticAPM4D. This class guarantees that there will only be one open transaction per thread.
  /// It's thread safe.
  /// </summary>
  TApm = class
  strict protected
    class threadvar FData: TDataController;
    class function GetErrorInstance: TError; static;
    class procedure InternalSendLog(const AMessage: string; ALevel: TLogLevel;
      const ALoggerName: string); static;
  public const
    HeaderKey = 'elastic-apm-traceparent';
  public
    class function HeaderValue: string; static;

    /// <summary>
    /// Method to start a customized transaction.
    /// Params:
    /// AName    -> Define the name of the transaction (of course).
    /// AType    -> It's a way to categorize the transaction (grouping)
    /// ATraceId -> Only needed if you are using continuous tracking.
    /// </summary>
    class function StartTransaction(const AName: string;
      const AType: string = 'Undefined'; const ATraceId: string = ''): TTransaction; static;

    /// <summary>
    /// Method to start a Http Request transaction.
    /// Params:
    /// AResource -> Will be used as a transaction name.
    /// AMethod   -> HTTP Method
    /// ATraceId  -> Only needed if you are using continuous tracking.
    /// </summary>
    class function StartTransactionRequest(const AResource: string; const AMethod: string = 'GET';
      const ATraceId: string = ''): TTransaction; overload; static;

    /// <summary>
    /// Method to start a Http Request transaction using TRESTRequest (Delphi native).
    /// Params:
    /// ARequest -> The TRESTRequest delphi object already configured.
    /// </summary>
    class function StartTransactionRequest(const ARequest: TRESTRequest): TTransaction; overload; static;

    /// <summary>
    /// Method to verify if a transaction is already started.
    /// </summary>
    class function ExistsTransaction: Boolean; static;

    /// <summary>
    /// Method to return the current transaction. If not found, it will return a exception.
    /// </summary>
    class function Transaction: TTransaction; static;

    /// <summary>
    /// Method to finalize the current transaction.
    /// Params:
    /// AOutcome -> The state of the end of the transaction
    /// </summary>
    class procedure EndTransaction(const AOutcome: TOutcome = success); overload; static;

    /// <summary>
    /// Method to finalize the http request transaction.
    /// Params:
    /// AResponse -> The response of the transaction
    /// </summary>
    class procedure EndTransaction(const AResponse: TCustomRESTResponse); overload; static;

    /// <summary>
    /// Method to start a span (It's a sub transaction)
    /// Params:
    /// AName -> The span name.
    /// AType -> It's a type/category of the transaction
    /// </summary>
    class function StartSpan(const AName: string; const AType: string = 'Method'): TSpan; static;

    class procedure SetSQLToCurrentSpan(const ASQL: string);

    /// <summary>
    /// Method to start a span, specific for SQL executions. (It's a sub transaction)
    /// Params:
    /// AName -> The span name.
    /// ASQL  -> The SQL command.
    /// </summary>
    class function StartSpanSQL(const AName, ASQL: string): TSpan; static;

    /// <summary>
    /// Method to start a span, specific for Http requests executions. (It's a sub transaction)
    /// Params:
    /// AResource -> The http uri resource
    /// AMethod   -> The http method
    /// </summary>
    class function StartSpanRequest(const AResource: string; const AMethod: string): TSpan; static;

    /// <summary>
    /// Method to return the current span. If not found, it will return null.
    /// </summary>
    class function Span: TSpan; static;

    /// <summary>
    /// Method to end the current span.
    /// </summary>
    class procedure EndSpan; overload; static;
    /// <summary>
    /// Method to end the current http request span.
    /// Params:
    /// StatusCode -> The http status code response.
    /// </summary>
    class procedure EndSpan(const StatusCode: Integer); overload; static;
    /// <summary>
    /// Method to pause the current transaction and span.
    /// </summary>
    class procedure Pause; overload; static;
    /// <summary>
    /// Method to unpause the current transaction and span.
    /// </summary>
    class procedure UnPause; overload; static;
    /// <summary>
    /// Method to check if the current transaction/span is paused.
    /// </summary>
    class function IsPaused: Boolean; static;

    /// <summary>
    /// Method to add a customized error in a transaction. It must have a transaction open
    /// Params:
    /// AError -> Elastic APM Error object
    /// </summary>
    class procedure AddError(AError: TError); overload; static;

    /// <summary>
    /// Method to add a exception in a transaction. It must have a transaction open
    /// Params:
    /// E -> Delphi Exception
    /// </summary>
    class procedure AddError(E: Exception); overload; static;

    /// <summary>
    /// Method to add a http request exception in a transaction. It must have a transaction open
    /// Params:
    /// E -> Http Delphi Exception
    /// </summary>
    class procedure AddError(E: EIdHTTPProtocolException); overload; static;

    /// <summary>
    /// Method to add a TRESTClient exception in a http request transaction. It must have a transaction open
    /// Params:
    /// AResponse -> TRESTResponse object
    /// </summary>
    class procedure AddError(AResponse: TCustomRESTResponse); overload; static;

    /// <summary>
    /// Send a TRACE level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogTrace(const AMessage: string; const ALoggerName: string = ''); static;

    /// <summary>
    /// Send a DEBUG level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogDebug(const AMessage: string; const ALoggerName: string = ''); static;

    /// <summary>
    /// Send an INFO level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogInfo(const AMessage: string; const ALoggerName: string = ''); static;

    /// <summary>
    /// Send a WARNING level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogWarning(const AMessage: string; const ALoggerName: string = ''); static;

    /// <summary>
    /// Send an ERROR level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogError(const AMessage: string; const ALoggerName: string = ''); static;

    /// <summary>
    /// Send a FATAL level log message correlated with the current transaction/span.
    /// Params:
    /// AMessage -> The log message
    /// ALoggerName -> Optional logger name for categorization
    /// </summary>
    class procedure LogFatal(const AMessage: string; const ALoggerName: string = ''); static;
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
  Result := StartTransaction(AResource, 'HttpRequest', ATraceId);
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
  Result := StartSpan(AName, 'database');
  Result.Context.db.AddSQL(ASQL);
end;

class function TApm.StartSpanRequest(const AResource, AMethod: string): TSpan;
begin
  Result := StartSpan(AResource, 'Request');
  Result.Context.CreateHttp(AMethod);
end;

class procedure TApm.SetSQLToCurrentSpan(const ASQL: string);
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  FData.CurrentSpan.Context.db.AddSQL(ASQL);
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

class procedure TApm.Pause;
begin
  if not ExistsTransaction or Transaction.IsPaused then
    exit;

  Transaction.Pause;
  if Span <> nil then
    Span.Pause;
end;

class procedure TApm.UnPause;
begin
  if not ExistsTransaction or not Transaction.IsPaused then
    exit;

  Transaction.UnPause;
  if Span <> nil then
    Span.UnPause;
end;

class function TApm.IsPaused: Boolean;
begin
  Result := ExistsTransaction and Transaction.IsPaused;
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
  FData.Transaction.SetOutcome(failure);
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
  if Error.Exception.message.IsEmpty then
    Error.Exception.message := E.Message;
  Error.Exception.&type := E.ClassName;
  Error.Context.AddResponse(E.ErrorCode);
  FData.Transaction.Result := Error.Exception.Code + ' ' + E.ErrorMessage;

  AddError(Error);
end;

class procedure TApm.InternalSendLog(const AMessage: string; ALevel: TLogLevel;
  const ALoggerName: string);
var
  LogEntry: TAPMLog;
  TraceId, TransactionId, SpanId: string;
  ConfigLevel: TLogLevel;
begin
  // Verifica se logs estão habilitados
  if not TConfig.GetLogEnabled then
    Exit;

  // Verifica o nível de log configurado
  ConfigLevel := TConfig.GetLogLevel;
  if ALevel < ConfigLevel then
    Exit;

  // Obtém IDs de correlação se houver transação ativa
  if Assigned(FData) then
  begin
    TraceId := FData.Transaction.Trace_id;
    TransactionId := FData.Transaction.id;
    if FData.SpanIsOpened then
      SpanId := FData.CurrentSpan.id
    else
      SpanId := '';
  end
  else
  begin
    TraceId := '';
    TransactionId := '';
    SpanId := '';
  end;

  LogEntry := TAPMLog.Create(TraceId, TransactionId, SpanId);
  try
    LogEntry.SetMessage(AMessage, ALevel);

    if not ALoggerName.IsEmpty then
      LogEntry.SetLogger(ALoggerName);

    if Assigned(FData) then
      FData.LogList.Add(LogEntry)
    else
      LogEntry.Free; // Se não houver transação, descarta o log
  except
    LogEntry.Free;
    raise;
  end;
end;

class procedure TApm.LogTrace(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llTrace, ALoggerName);
end;

class procedure TApm.LogDebug(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llDebug, ALoggerName);
end;

class procedure TApm.LogInfo(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llInfo, ALoggerName);
end;

class procedure TApm.LogWarning(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llWarning, ALoggerName);
end;

class procedure TApm.LogError(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llError, ALoggerName);
end;

class procedure TApm.LogFatal(const AMessage: string; const ALoggerName: string);
begin
  InternalSendLog(AMessage, llFatal, ALoggerName);
end;

end.
