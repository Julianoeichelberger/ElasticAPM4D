unit ElasticAPM4D.Span;

interface

Uses
  System.SysUtils,
  ElasticAPM4D.Service,
  ElasticAPM4D.StacktraceFrame,
  ElasticAPM4D.Transaction;

type
  TElasticAPM4DSpanContextDB = class
  private
    FInstance: String;
    FStatement: String;
    FType: String;
    FUser: String;
    Flink: string;
  public
    property instance: String read FInstance write FInstance;
    property link: string read Flink write Flink;
    property statement: String read FStatement write FStatement;
    property &type: String read FType write FType;
    property User: String read FUser write FUser;
  end;

  TElasticAPM4DSpanContextHttp = class
  private
    FMethod: String;
    FStatus_code: Integer;
    FUrl: String;
  public
    property method: String read FMethod write FMethod;
    property status_code: Integer read FStatus_code write FStatus_code;
    property url: String read FUrl write FUrl;
  end;

  TElasticAPM4DSpanService = class
  private
    FAgent: TElasticAPM4DServiceAgent;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Agent: TElasticAPM4DServiceAgent read FAgent;
    property name: String read FName;
  end;

  TElasticAPM4DSpanContext = class
  private
    FService: TElasticAPM4DSpanService;
    FHttp: TElasticAPM4DSpanContextHttp;
    FDb: TElasticAPM4DSpanContextDB;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Service: TElasticAPM4DSpanService read FService write FService;
    property db: TElasticAPM4DSpanContextDB read FDb write FDb;
    property http: TElasticAPM4DSpanContextHttp read FHttp write FHttp;
  end;

  TElasticAPM4DSpan = class
  private
    FStartDate: TDateTime;
    FAction: String;
    FContext: TElasticAPM4DSpanContext;
    FDuration: Int64;
    FId: String;
    FName: String;
    FParent_id: String;
    FStacktrace: TArray<TElasticAPM4DStacktrace>;
    FSubtype: String;
    FSync: Boolean;
    FTrace_id: String;
    FTransaction_id: String;
    FType: String;
    Ftimestamp: Int64;
  public
    constructor Create(AParent: TElasticAPM4DSpan); overload;
    constructor Create(AParent: TElasticAPM4DTransaction); overload;
    destructor Destroy; override;

    function ToJsonString: string;

    procedure Start;
    procedure &End;

    property id: String read FId;
    property transaction_id: String read FTransaction_id;
    property name: String read FName write FName;
    property &type: String read FType write FType;
    property parent_id: String read FParent_id;
    property trace_id: String read FTrace_id;
    property subtype: String read FSubtype write FSubtype;
    property action: String read FAction write FAction;
    property duration: Int64 read FDuration write FDuration;
    property Context: TElasticAPM4DSpanContext read FContext;
    property Stacktrace: TArray<TElasticAPM4DStacktrace> read FStacktrace write FStacktrace;
    property sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
  end;

implementation

Uses
  DateUtils,
  REST.Json,
  ElasticAPM4D.TimestampEpoch,
  ElasticAPM4D.Uuid;

{ TElasticAPM4DSpanService }

constructor TElasticAPM4DSpanService.Create;
begin
  FAgent := TElasticAPM4DServiceAgent.Create;
end;

destructor TElasticAPM4DSpanService.Destroy;
begin
  FAgent.Free;
  inherited;
end;

{ TElasticAPM4DSpanContext }

constructor TElasticAPM4DSpanContext.Create;
begin
  FService := TElasticAPM4DSpanService.Create;
  FDb := TElasticAPM4DSpanContextDB.Create;
end;

destructor TElasticAPM4DSpanContext.Destroy;
begin
  FService.Free;
  FDb.Free;
  if Assigned(FHttp) then
    FHttp.Free;
  inherited;
end;

{ TElasticAPM4DSpan }

constructor TElasticAPM4DSpan.Create(AParent: TElasticAPM4DSpan);
begin
  FId := TElasticAPM4DUUid.GetUUid64b;
  FTrace_id := AParent.trace_id;
  FTransaction_id := AParent.transaction_id;
  FParent_id := AParent.id;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FContext := TElasticAPM4DSpanContext.Create;
  Ftimestamp := AParent.Timestamp; // Test
end;

constructor TElasticAPM4DSpan.Create(AParent: TElasticAPM4DTransaction);
begin
  FId := TElasticAPM4DUUid.GetUUid64b;
  FTrace_id := AParent.trace_id;
  FTransaction_id := AParent.id;
  FParent_id := AParent.id;
  FAction := '';
  FSubtype := '';
  FSync := False;
  FContext := TElasticAPM4DSpanContext.Create;
  Ftimestamp := AParent.Timestamp; // Test
end;

destructor TElasticAPM4DSpan.Destroy;
var
  LStack: TElasticAPM4DStacktrace;
begin
  for LStack in FStacktrace do
    LStack.Free;
  FreeAndNil(FContext);
  inherited;
end;

procedure TElasticAPM4DSpan.&End;
begin
  FDuration := MilliSecondsBetween(now, FStartDate);
end;

procedure TElasticAPM4DSpan.Start;
begin
  FStartDate := now;
  Ftimestamp := TElasticAPM4DTimestampEpoch.Get(FStartDate);
end;

function TElasticAPM4DSpan.ToJsonString: string;
begin
  Result := format('{"span": %s}', [TJson.ObjectToJsonString(self, [joDateIsUTC, joIgnoreEmptyStrings])]);
end;

end.
