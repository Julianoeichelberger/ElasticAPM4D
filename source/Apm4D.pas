{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D;

interface

uses
  System.Rtti, System.SysUtils, System.classes, System.Generics.Collections, IdHTTP,
  Apm4D.Span, Apm4D.Transaction, Apm4D.Error, Apm4D.Share.Types, REST.Client,
  Apm4D.DataController, Apm4D.Settings, Apm4D.Settings.Log, Apm4D.Log, Apm4D.Interceptor.Handler;

type
  TTransaction = Apm4D.Transaction.TTransaction;
  TSpan = Apm4D.Span.TSpan;
  TOutcome = Apm4D.Share.Types.TOutcome;
  TApm4DSettings = Apm4D.Settings.TApm4DSettings;
  TLogLevel = Apm4D.Settings.Log.TLogLevel;
  IApm4DInterceptorHandler = Apm4D.Interceptor.Handler.IApm4DInterceptorHandler;
  TApm4DInterceptorBuilder = Apm4D.Interceptor.Handler.TApm4DInterceptorBuilder;

  /// <summary>
  /// The main class of the Apm4D.
  /// This class guarantees that there will only be one open Transaction per thread.
  /// It's thread safe.
  /// </summary>
  TApm4D = class
  strict protected
    class threadvar FData: TDataController;
    class function GetErrorInstance: TError; static;
  public const
    HeaderKey = 'elastic-apm-traceparent';
  public
    class function HeaderValue: string; static;

    /// <summary>
    /// Method to start a customized Transaction.
    /// Params:
    /// AName    -> Define the name of the Transaction (of course).
    /// AType    -> It's a way to categorize the Transaction (grouping)
    /// ATraceId -> Only needed if you are using continuous tracking.
    /// </summary>
    class function StartTransaction(const AName: string;
      const AType: string = 'Undefined'; const ATraceId: string = ''): TTransaction; static;

    /// <summary>
    /// Method to start a Http Request Transaction.
    /// Params:
    /// AResource -> Will be used as a Transaction name.
    /// AMethod   -> HTTP Method
    /// ATraceId  -> Only needed if you are using continuous tracking.
    /// </summary>
    class function StartTransactionRequest(const AResource: string; const AMethod: string = 'GET';
      const ATraceId: string = ''): TTransaction; overload; static;

    /// <summary>
    /// Method to start a Http Request Transaction using TRESTRequest (Delphi native).
    /// Params:
    /// ARequest -> The TRESTRequest delphi object already configured.
    /// </summary>
    class function StartTransactionRequest(const ARequest: TRESTRequest): TTransaction; overload; static;

    /// <summary>
    /// Method to verify if a Transaction is already started.
    /// </summary>
    class function ExistsTransaction: Boolean; static;

    /// <summary>
    /// Method to return the current Transaction. If not found, it will return a exception.
    /// </summary>
    class function Transaction: TTransaction; static;

    /// <summary>
    /// Method to finalize the current Transaction.
    /// Params:
    /// AOutcome -> The state of the end of the Transaction
    /// </summary>
    class procedure EndTransaction(const AOutcome: TOutcome = success); overload; static;

    /// <summary>
    /// Method to finalize the http request Transaction.
    /// Params:
    /// AResponse -> The response of the Transaction
    /// </summary>
    class procedure EndTransaction(const AResponse: TCustomRESTResponse); overload; static;

    /// <summary>
    /// Method to start a Span (It's a sub Transaction)
    /// Params:
    /// AName -> The Span name.
    /// AType -> It's a type/category of the Transaction
    /// </summary>
    class function StartSpan(const AName: string; const AType: string = 'Method'): TSpan; static;

    /// <summary>
    /// Method to set the SQL Command to the Span
    /// Params:
    /// ASQL  -> The SQL command.
    /// </summary>
    class procedure SetSQLToCurrentSpan(const ASQL: string);

    /// <summary>
    /// Method to start a Span, specific for SQL executions. (It's a sub Transaction)
    /// Params:
    /// AName -> The Span name.
    /// ADatabase  -> Database engine. Ex: mssql, mysql, etc
    /// </summary>
    class function StartSpanDb(const AName, ADatabase: string): TSpan; static;

    /// <summary>
    /// Method to start a Span, specific for Http requests executions. (It's a sub Transaction)
    /// Params:
    /// AResource -> The http uri resource
    /// AMethod   -> The http method
    /// </summary>
    class function StartSpanRequest(const AResource: string; const AMethod: string): TSpan; static;

    /// <summary>
    /// Method to return the current Span. If not found, it will return null.
    /// </summary>
    class function Span: TSpan; static;

    /// <summary>
    /// Method to end the current Span.
    /// </summary>
    class procedure EndSpan; overload; static;
    /// <summary>
    /// Method to end the current http request Span.
    /// Params:
    /// StatusCode -> The http status code response.
    /// </summary>
    class procedure EndSpan(const StatusCode: Integer); overload; static;
    /// <summary>
    /// Method to pause the current Transaction and Apm4D.Span.
    /// </summary>
    class procedure Pause; overload; static;
    /// <summary>
    /// Method to unpause the current Transaction and Apm4D.Span.
    /// </summary>
    class procedure UnPause; overload; static;
    /// <summary>
    /// Method to check if the current Transaction/Span is paused.
    /// </summary>
    class function IsPaused: Boolean; static;

    /// <summary>
    /// Method to add a customized Error in a Transaction. It must have a Transaction open
    /// Params:
    /// AError -> Elastic APM Error object
    /// </summary>
    class procedure AddError(AError: TError); overload; static;

    /// <summary>
    /// Method to add a exception in a Transaction. It must have a Transaction open
    /// Params:
    /// E -> Delphi Exception
    /// </summary>
    class procedure AddError(E: Exception); overload; static;

    /// <summary>
    /// Method to add a http request exception in a Transaction. It must have a Transaction open
    /// Params:
    /// E -> Http Delphi Exception
    /// </summary>
    class procedure AddError(E: EIdHTTPProtocolException); overload; static;

    /// <summary>
    /// Method to add a TRESTClient exception in a http request Transaction. It must have a Transaction open
    /// Params:
    /// AResponse -> TRESTResponse object
    /// </summary>
    class procedure AddError(AResponse: TCustomRESTResponse); overload; static;
  end;

implementation

uses
  Apm4D.Share.Context;

{ TElasticAPM4D }

class function TApm4D.HeaderValue: string;
begin
  if not Assigned(FData) then
    Exit('');

  Result := FData.Header;
end;

class function TApm4D.StartTransaction(const AName, AType, ATraceId: string): TTransaction;
begin
  if ExistsTransaction then
    EndTransaction(unknown);

  FData := TDataController.Create;
  FData.Transaction.Start(AName, AType);

  if not ATraceId.IsEmpty then
    FData.Transaction.trace_id := ATraceId;

  Result := FData.Transaction;
end;

class function TApm4D.StartTransactionRequest(const ARequest: TRESTRequest): TTransaction;
var
  Name: string;
begin
  Name := ARequest.Resource;
  if Name.IsEmpty then
    Name := ARequest.Client.BaseURL;
  Result := StartTransactionRequest(Name);
end;

class function TApm4D.StartTransactionRequest(const AResource, AMethod, ATraceId: string): TTransaction;
begin
  Result := StartTransaction(AResource, 'HttpRequest', ATraceId);
  Result.AddContextRequest(AMethod);
end;

class function TApm4D.Transaction: TTransaction;
begin
  if not Assigned(FData) then
    raise ETransactionNotFound.Create('Apm4D.Transaction not found');

  Result := FData.Transaction;
end;

class procedure TApm4D.EndTransaction(const AOutcome: TOutcome);
begin
  if not Assigned(FData) then
    Exit;

  if (FData.ErrorList.Count > 0) then
    FData.Transaction.SetOutcome(failure);
  try
    FData.Transaction.ToEnd(AOutcome);
    FData.ToQueue;
  finally
    FreeAndNil(FData);
  end;
end;

class procedure TApm4D.EndTransaction(const AResponse: TCustomRESTResponse);
begin
  if not Assigned(FData) then
    Exit;

  FData.Transaction.Context.AddResponse(AResponse.StatusCode);
  FData.Transaction.Result := AResponse.StatusCode.ToString + ' ' + AResponse.StatusText;
  try
    if AResponse.StatusCode >= 500 then
      FData.Transaction.ToEnd(failure)
    else
      FData.Transaction.ToEnd(success);
    FData.ToQueue;
  finally
    FreeAndNil(FData);
  end;
end;

class function TApm4D.ExistsTransaction: Boolean;
begin
  Result := Assigned(FData);
end;

class function TApm4D.StartSpan(const AName, AType: string): TSpan;
begin
  Result := FData.StartSpan(AName, AType);
end;

class function TApm4D.StartSpanDb(const AName, ADatabase: string): TSpan;
begin
  Result := StartSpan(AName, 'db');
  Result.Subtype := ADatabase;
end;

class function TApm4D.StartSpanRequest(const AResource, AMethod: string): TSpan;
begin
  Result := StartSpan(AResource, 'Request');
  Result.Context.CreateHttp(AMethod);
end;

class procedure TApm4D.SetSQLToCurrentSpan(const ASQL: string);
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  if IsPaused then
    exit;

  FData.CurrentSpan.Context.CreateDb.AddSQL(ASQL);
end;

class function TApm4D.Span: TSpan;
begin
  Result := FData.CurrentSpan;
end;

class procedure TApm4D.EndSpan;
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  FData.EndSpan;
end;

class procedure TApm4D.EndSpan(const StatusCode: Integer);
begin
  if not ExistsTransaction then
    Exit;

  if not FData.SpanIsOpened then
    Exit;

  FData.CurrentSpan.Context.Http.AddStatusCode(StatusCode);
  FData.Transaction.Result := StatusCode.ToString;
  FData.EndSpan;
end;

class function TApm4D.GetErrorInstance: TError;
begin
  if FData.SpanIsOpened then
    Result := TError.Create(Span.trace_id, Span.transaction_id, Span.id)
  else
    Result := TError.Create(Transaction.trace_id, Transaction.id, Transaction.id);
end;

class procedure TApm4D.Pause;
begin
  if not ExistsTransaction or Transaction.IsPaused then
    exit;

  Transaction.Pause;
  FData.PauseAllOpenedSpans;
end;

class procedure TApm4D.UnPause;
begin
  if not ExistsTransaction or not Transaction.IsPaused then
    exit;

  Transaction.UnPause;
  if Span <> nil then
    Span.UnPause;
end;

class function TApm4D.IsPaused: Boolean;
begin
  Result := ExistsTransaction and Transaction.IsPaused;
end;

class procedure TApm4D.AddError(E: Exception);
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

class procedure TApm4D.AddError(AError: TError);
begin
  if not Assigned(FData) then
    Exit;
  FData.Transaction.SetOutcome(failure);
  FData.ErrorList.Add(AError);
end;

class procedure TApm4D.AddError(AResponse: TCustomRESTResponse);
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

class procedure TApm4D.AddError(E: EIdHTTPProtocolException);
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


end.
