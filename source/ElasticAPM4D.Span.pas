unit ElasticAPM4D.Span;

interface

Uses
  System.SysUtils,
  ElasticAPM4D.Destination,
  ElasticAPM4D.Service,
  ElasticAPM4D.Stacktrace,
  ElasticAPM4D.Message;

type
  // <summary>
  // Database contains contextual data for database spans
  // </summary>
  TDB = class
  private
    FInstance: string;
    FStatement: string;
    FType: string;
    FUser: string;
    Flink: string;
    Frows_affected: Integer;
  public
    // <summary>
    // Instance name of the database.
    // </summary>
    property Instance: string read FInstance write FInstance;
    // <summary>
    // Link to the database server.
    // </summary>
    property Link: string read Flink write Flink;
    // <summary>
    // Statement of the recorded database event, e.g. query.
    // </summary>
    property Statement: string read FStatement write FStatement;
    // <summary>
    // Type of the recorded database event., e.g. sql, cassandra, hbase, redis.
    // </summary>
    property &type: string read FType write FType;
    // <summary>
    // User is the username with which the database is accessed.
    // </summary>
    property User: string read FUser write FUser;
    // <summary>
    // RowsAffected shows the number of rows affected by the statement
    // </summary>
    property rows_affected: Integer read Frows_affected write Frows_affected;
  end;

  // <summary>
  // Response describes the HTTP response information in case the event was created as a result of an HTTP request.
  // </summary>
  TResponse = class
  private
    FDecoded_body_size: Integer;
    Fencoded_body_size: Integer;
    FStatus_code: Integer;
    Ftransfer_size: Integer;
  public
    // <summary>
    // DecodedBodySize holds the size of the decoded payload
    // </summary>
    property Decoded_body_size: Integer read FDecoded_body_size write FDecoded_body_size;
    // <summary>
    // EncodedBodySize holds the size of the encoded payload.
    // </summary>
    property encoded_body_size: Integer read Fencoded_body_size write Fencoded_body_size;
    // <summary>
    // StatusCode sent in the http response.
    // </summary>
    property status_code: Integer read FStatus_code write FStatus_code;
    // <summary>
    // TransferSize holds the total size of the payload.
    // </summary>
    property transfer_size: Integer read Ftransfer_size write Ftransfer_size;
  end;

  // <summary>
  // HTTP contains contextual information when the span concerns an HTTP request
  // </summary>
  THttp = class
  private
    FMethod: string;
    FStatus_code: Integer;
    FUrl: String;
    Fresponse: TResponse;
  public
    destructor Destroy; override;

    property method: string read FMethod write FMethod;
    property status_code: Integer read FStatus_code write FStatus_code;
    // <summary>
    // URL is the raw url of the correlating HTTP request
    // </summary>
    property url: String read FUrl write FUrl;
    property response: TResponse read Fresponse write Fresponse;
  end;

  // <summary>
  // Context holds arbitrary contextual information for the event.
  // </summary>
  TContext = class
  private
    FService: TService;
    FHttp: THttp;
    FDb: TDB;
    FMessage: TMessage;
    FDestination: TDestination;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Service: TService read FService write FService;
    property db: TDB read FDb write FDb;
    property http: THttp read FHttp write FHttp;
    property Message: TMessage read FMessage write FMessage;
    property Destination: TDestination read FDestination write FDestination;
  end;

  TSpan = class
  private
    FStartDate: TDateTime;
    FAction: string;
    FContext: TContext;
    FDuration: Int64;
    FId: string;
    FName: string;
    FParent_id: string;
    FStacktrace: TArray<TStacktrace>;
    FSubtype: string;
    FSync: Boolean;
    FTrace_id: string;
    FTransaction_id: string;
    FType: string;
    Ftimestamp: Int64;
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    procedure Start(const AName: string; const AType: string = 'Undefined');
    procedure &End;

    property Id: string read FId;
    property Transaction_id: string read FTransaction_id;
    property Name: string read FName;
    property &type: string read FType;
    property Parent_id: string read FParent_id;
    property Trace_id: string read FTrace_id;
    property Subtype: string read FSubtype write FSubtype;
    property Action: string read FAction write FAction;
    property Duration: Int64 read FDuration;
    property Context: TContext read FContext write FContext;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property Sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
  end;

implementation

Uses
  System.DateUtils, REST.Json, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

{ TSpanContext }

constructor TContext.Create;
begin
  FService := TService.Create;
  FDb := TDB.Create;
end;

destructor TContext.Destroy;
begin
  FService.Free;
  FDb.Free;
  if Assigned(FHttp) then
    FHttp.Free;
  if Assigned(FMessage) then
    FMessage.Free;
  if Assigned(FDestination) then
    FDestination.Free;
  inherited;
end;

{ TSpan }

constructor TSpan.Create(const ATraceId, ATransactionId, AParentId: string);
begin
  FId := TUUid.Get64b;
  FTrace_id := ATraceId;
  FTransaction_id := ATransactionId;
  FParent_id := AParentId;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FContext := TContext.Create;
end;

destructor TSpan.Destroy;
var
  LStack: TStacktrace;
begin
  for LStack in FStacktrace do
    LStack.Free;
  FreeAndNil(FContext);
  inherited;
end;

procedure TSpan.&End;
begin
  FDuration := MilliSecondsBetween(now, FStartDate);
end;

procedure TSpan.Start(const AName, AType: string);
begin
  FStartDate := now;
  Ftimestamp := TTimestampEpoch.Get(FStartDate);
  FName := AName;
  FType := AType;
end;

function TSpan.ToJsonString: string;
begin
  Result := format(sSpanJsonId, [TJson.ObjectToJsonString(self, [joDateIsUTC, joIgnoreEmptyStrings])]);
end;

{ THttp }

destructor THttp.Destroy;
begin
  if Assigned(Fresponse) then
    Fresponse.Free;
  inherited;
end;

end.
