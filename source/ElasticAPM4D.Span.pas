unit ElasticAPM4D.Span;

interface

Uses
  System.SysUtils, ElasticAPM4D.Service, ElasticAPM4D.Stacktrace;

type
  TDB = class
  private
    FInstance: String;
    FStatement: String;
    FType: String;
    FUser: String;
    Flink: string;
  public
    property Instance: String read FInstance write FInstance;
    property Link: string read Flink write Flink;
    property Statement: String read FStatement write FStatement;
    property &type: String read FType write FType;
    property User: String read FUser write FUser;
  end;

  THttp = class
  private
    FMethod: String;
    FStatus_code: Integer;
    FUrl: String;
  public
    property method: String read FMethod write FMethod;
    property status_code: Integer read FStatus_code write FStatus_code;
    property url: String read FUrl write FUrl;
  end;

  TService = class
  private
    FAgent: TAgent;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Agent: TAgent read FAgent;
    property name: String read FName;
  end;

  TContext = class
  private
    FService: TService;
    FHttp: THttp;
    FDb: TDB;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Service: TService read FService write FService;
    property db: TDB read FDb write FDb;
    property http: THttp read FHttp write FHttp;
  end;

  TSpan = class
  private
    FStartDate: TDateTime;
    FAction: String;
    FContext: TContext;
    FDuration: Int64;
    FId: String;
    FName: String;
    FParent_id: String;
    FStacktrace: TArray<TStacktrace>;
    FSubtype: String;
    FSync: Boolean;
    FTrace_id: String;
    FTransaction_id: String;
    FType: String;
    Ftimestamp: Int64;
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    procedure Start;
    procedure &End;

    property Id: String read FId;
    property Transaction_id: String read FTransaction_id;
    property Name: String read FName write FName;
    property &type: String read FType write FType;
    property Parent_id: String read FParent_id;
    property Trace_id: String read FTrace_id;
    property Subtype: String read FSubtype write FSubtype;
    property Action: String read FAction write FAction;
    property Duration: Int64 read FDuration write FDuration;
    property Context: TContext read FContext write FContext;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property Sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
  end;

implementation

Uses
  System.DateUtils, REST.Json, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

{ TSpanService }

constructor TService.Create;
begin
  FAgent := TAgent.Create;
end;

destructor TService.Destroy;
begin
  FAgent.Free;
  inherited;
end;

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

procedure TSpan.Start;
begin
  FStartDate := now;
  Ftimestamp := TTimestampEpoch.Get(FStartDate);
end;

function TSpan.ToJsonString: string;
begin
  Result := format(sSpanJsonId, [TJson.ObjectToJsonString(self, [joDateIsUTC, joIgnoreEmptyStrings])]);
end;

end.

