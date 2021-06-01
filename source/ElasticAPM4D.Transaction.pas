unit ElasticAPM4D.Transaction;

interface

uses
  System.Classes, System.SysUtils, ElasticAPM4D.Utils, ElasticAPM4D.Context, ElasticAPM4D.Experience;

type
  // <summary>
  // SpanCount counts correlated spans.
  // </summary>
  TSpanCount = class
  private
    FDropped: Integer;
    FStarted: Integer;
  public
    constructor Create;

    procedure Inc;
    procedure Dec;
    procedure Reset;

    property Dropped: Integer read FDropped;
    property Started: Integer read FStarted;
  end;

  // <summary>
  // Session holds optional transaction session information for RUM
  // </summary>
  TSession = class
  private
    Fid: string;
  public
    constructor Create(const AId: string);
    // <summary>
    // ID holds a session ID for grouping a set of related transactions.
    // </summary>
    property id: string read Fid;
  end;

  TTransaction = class
  private
    FStartDate: TDateTime;
    Fid: string;
    Ftrace_id: string;
    Fname: string;
    Ftype: string;
    Fresult: string;
    Fduration: Int64;
    Fcontext: TContext;
    Fspan_count: TSpanCount;
    Fsampled: boolean;
    Fparent_id: string;
    Ftimestamp: Int64;
    Fexperience: TExperience;
    Foutcome: string;
    Fsession: TSession;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const AName: string; const AType: string = 'Undefined');
    procedure &End(const AOutcome: TOutcome = success);

    function ToJsonString: string;

    // <summary>
    // Outcome of the transaction with a limited set of permitted values, describing the success or failure of the transaction
    // from the service's perspective. It is used for calculating error rates for incoming requests.
    // Permitted values: success, failure, unknown
    // </summary>
    procedure SetOutcome(const AOutcome: TOutcome);

    procedure AddSession(const ASessionId: string);

    property id: string read Fid;
    property Trace_id: string read Ftrace_id write Ftrace_id;
    property Parent_id: string read Fparent_id write Fparent_id;
    property Name: string read Fname;

    // <summary>
    // Type expresses the transaction's type as keyword that has specific relevance
    // within the service's domain, eg: 'request', 'backgroundjob'
    // </summary>
    property &type: string read Ftype;
    property Span_count: TSpanCount read Fspan_count;
    property Context: TContext read Fcontext write Fcontext;
    property Duration: Int64 read Fduration write Fduration;

    // <summary>
    // Result of the transaction. For HTTP-related transactions,  should be the status code formatted like 'HTTP 2xx
    // </summary>
    property &result: string read Fresult write Fresult;

    property Sampled: boolean read Fsampled write Fsampled;
    property Timestamp: Int64 read Ftimestamp;
    property Experience: TExperience read Fexperience write Fexperience;
  end;

implementation

Uses
  System.DateUtils, System.Rtti, REST.Json, ElasticAPM4D.Resources;

{ TSpanCount }

constructor TSpanCount.Create;
begin
  Reset;
end;

procedure TSpanCount.Dec;
begin
  FDropped := FDropped - 1;
end;

procedure TSpanCount.Inc;
begin
  FStarted := FStarted + 1;
  FDropped := FDropped + 1;
end;

procedure TSpanCount.Reset;
begin
  FDropped := 0;
  FStarted := 0;
end;
{ TSession }

constructor TSession.Create(const AId: string);
begin
  Fid := AId;
end;

{ TTransaction }

procedure TTransaction.AddSession(const ASessionId: string);
begin
  Fsession := TSession.Create(ASessionId);
end;

constructor TTransaction.Create;
begin
  Fspan_count := TSpanCount.Create;
  Fid := TUUid.Get64b;
  Ftrace_id := TUUid.Get128b;
  Fsampled := true;
end;

destructor TTransaction.Destroy;
begin
  if Assigned(Fcontext) then
    Fcontext.Free;
  if Assigned(Fexperience) then
    Fexperience.Free;
  if Assigned(Fsession) then
    Fsession.Free;
  Fspan_count.Free;
  inherited;
end;

procedure TTransaction.&End(const AOutcome: TOutcome);
begin
  if Foutcome.IsEmpty then
    SetOutcome(AOutcome);
  Fduration := MilliSecondsBetween(now, FStartDate);
end;

procedure TTransaction.SetOutcome(const AOutcome: TOutcome);
begin
  Foutcome := TRttiEnumerationType.GetName<TOutcome>(AOutcome);
end;

procedure TTransaction.Start(const AName, AType: string);
begin
  FStartDate := now;
  Ftimestamp := TTimestampEpoch.Get(FStartDate);
  Ftype := AType;
  Fname := AName;
end;

function TTransaction.ToJsonString: string;
begin
  Result := format(sTransactionJsonId, [TJson.ObjectToJsonString(Self, [joIgnoreEmptyStrings])]);
end;

end.
