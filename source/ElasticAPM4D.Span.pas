unit ElasticAPM4D.Span;

interface

Uses
  IdHttp, System.SysUtils, ElasticAPM4D.Service, ElasticAPM4D.Stacktrace, ElasticAPM4D.Transaction;

type
  TSpanContextDB = class
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

  TSpanContextHttp = class
  private
    FMethod: String;
    FStatus_code: Integer;
    FUrl: String;
  public
    property method: String read FMethod write FMethod;
    property status_code: Integer read FStatus_code write FStatus_code;
    property url: String read FUrl write FUrl;
  end;

  TSpanService = class
  private
    FAgent: TServiceAgent;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Agent: TServiceAgent read FAgent;
    property name: String read FName;
  end;

  TSpanContext = class
  private
    FService: TSpanService;
    FHttp: TSpanContextHttp;
    FDb: TSpanContextDB;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AutoCreateHttp(AIdHttp: TIdCustomHTTP);

    property Service: TSpanService read FService write FService;
    property db: TSpanContextDB read FDb write FDb;
    property http: TSpanContextHttp read FHttp write FHttp;
  end;

  TSpan = class
  private
    FStartDate: TDateTime;
    FAction: String;
    FContext: TSpanContext;
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
    constructor Create(AParent: TSpan); overload;
    constructor Create(AParent: TTransaction); overload;
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
    property Context: TSpanContext read FContext;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
  end;

implementation

Uses
  System.DateUtils, REST.Json, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

{ TSpanService }

constructor TSpanService.Create;
begin
  FAgent := TServiceAgent.Create;
end;

destructor TSpanService.Destroy;
begin
  FAgent.Free;
  inherited;
end;

{ TSpanContext }

procedure TSpanContext.AutoCreateHttp(AIdHttp: TIdCustomHTTP);
begin
  FHttp := TSpanContextHttp.Create;
  FHttp.method := AIdHttp.Request.method;
  FHttp.url := AIdHttp.Request.url;
  FHttp.status_code := AIdHttp.ResponseCode;
end;

constructor TSpanContext.Create;
begin
  FService := TSpanService.Create;
  FDb := TSpanContextDB.Create;
end;

destructor TSpanContext.Destroy;
begin
  FService.Free;
  FDb.Free;
  if Assigned(FHttp) then
    FHttp.Free;
  inherited;
end;

{ TSpan }

constructor TSpan.Create(AParent: TSpan);
begin
  FId := TUUid.Get64b;
  FTrace_id := AParent.trace_id;
  FTransaction_id := AParent.transaction_id;
  FParent_id := AParent.id;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FContext := TSpanContext.Create;
end;

constructor TSpan.Create(AParent: TTransaction);
begin
  FId := TUUid.Get64b;
  FTrace_id := AParent.trace_id;
  FTransaction_id := AParent.id;
  FParent_id := AParent.id;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FContext := TSpanContext.Create;
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
