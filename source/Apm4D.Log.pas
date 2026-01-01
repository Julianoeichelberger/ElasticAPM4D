{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Log;

interface

uses
  System.SysUtils, System.JSON, Apm4D.Share.TimestampEpoch, Apm4D.Settings, Apm4D.Settings.Log;

type
  TAPMLog = class
  private
    Ftimestamp: Int64;
    Fmessage: string;
    Flevel: string;
    Ftrace_id: string;
    Ftransaction_id: string;
    Fspan_id: string;
    Fservice_name: string;
    Fservice_version: string;
    Fevent_dataset: string;
    Felog_logger: string;
    function GetLevelString(ALevel: TLogLevel): string;
  public
    constructor Create(const ATraceId, ATransactionId: string; const ASpanId: string = '');

    procedure SetMessage(const AMessage: string; ALevel: TLogLevel = llInfo);
    procedure SetServiceInfo(const AName, AVersion: string);
    procedure SetLogger(const ALoggerName: string);

    function ToJsonString: string;

    property trace_id: string read Ftrace_id write Ftrace_id;
    property transaction_id: string read Ftransaction_id write Ftransaction_id;
    property span_id: string read Fspan_id write Fspan_id;
    property timestamp: Int64 read Ftimestamp;
    property level: string read Flevel;
    property message: string read Fmessage;
  end;

implementation

uses
  REST.Json;

{ TAPMLog }

constructor TAPMLog.Create(const ATraceId, ATransactionId: string; const ASpanId: string = '');
begin
  Ftimestamp := TTimestampEpoch.Get(Now);
  Ftrace_id := ATraceId;
  Ftransaction_id := ATransactionId;
  Fspan_id := ASpanId;
  Fservice_name := TApm4DSettings.Application.Name;
  Fservice_version := TApm4DSettings.Application.Version;
  Fevent_dataset := 'app.logs';
  Flevel := 'info';
end;

function TAPMLog.GetLevelString(ALevel: TLogLevel): string;
begin
  case ALevel of
    llTrace:
      Result := 'trace';
    llDebug:
      Result := 'debug';
    llInfo:
      Result := 'info';
    llWarning:
      Result := 'warning';
    llError:
      Result := 'Error';
    llCritical:
      Result := 'critical';
  else
    Result := 'info';
  end;
end;

procedure TAPMLog.SetMessage(const AMessage: string; ALevel: TLogLevel);
begin
  Fmessage := AMessage;
  Flevel := GetLevelString(ALevel);
end;

procedure TAPMLog.SetServiceInfo(const AName, AVersion: string);
begin
  Fservice_name := AName;
  Fservice_version := AVersion;
end;

procedure TAPMLog.SetLogger(const ALoggerName: string);
begin
  Felog_logger := ALoggerName;
end;

function TAPMLog.ToJsonString: string;
var
  Json: TJSONObject;
  ServiceObj: TJSONObject;
  LogObj: TJSONObject;
  TraceObj: TJSONObject;
  TransactionObj: TJSONObject;
  SpanObj: TJSONObject;
  EventObj: TJSONObject;
begin
  Json := TJSONObject.Create;
  try
    // Timestamp
    Json.AddPair('@timestamp', TJSONNumber.Create(Ftimestamp));

    // Message
    Json.AddPair('message', Fmessage);

    // Service
    ServiceObj := TJSONObject.Create;
    ServiceObj.AddPair('name', Fservice_name);
    if not Fservice_version.IsEmpty then
      ServiceObj.AddPair('version', Fservice_version);
    Json.AddPair('service', ServiceObj);

    // Log level
    LogObj := TJSONObject.Create;
    LogObj.AddPair('level', Flevel);
    if not Felog_logger.IsEmpty then
    begin
      LogObj.AddPair('logger', Felog_logger);
    end;
    Json.AddPair('log', LogObj);

    // Trace correlation
    if not Ftrace_id.IsEmpty then
    begin
      TraceObj := TJSONObject.Create;
      TraceObj.AddPair('id', Ftrace_id);
      Json.AddPair('trace', TraceObj);
    end;

    // Apm4D.Transaction correlation
    if not Ftransaction_id.IsEmpty then
    begin
      TransactionObj := TJSONObject.Create;
      TransactionObj.AddPair('id', Ftransaction_id);
      Json.AddPair('transaction', TransactionObj);
    end;

    // Apm4D.Span correlation
    if not Fspan_id.IsEmpty then
    begin
      SpanObj := TJSONObject.Create;
      SpanObj.AddPair('id', Fspan_id);
      Json.AddPair('span', SpanObj);
    end;

    // Event dataset
    if not Fevent_dataset.IsEmpty then
    begin
      EventObj := TJSONObject.Create;
      EventObj.AddPair('dataset', Fevent_dataset);
      Json.AddPair('event', EventObj);
    end;

    Result := Json.ToString;
  finally
    Json.Free;
  end;
end;

end.
