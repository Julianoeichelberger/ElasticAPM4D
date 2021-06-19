unit Transaction;

interface

uses
  Share.Service,
  Share.Context,
  Share.Types,
  Transaction.Experience,
  Transaction.SpanCount,
  Transaction.Session;

type
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
    Fspan_count: TTransactionSpanCount;
    Fsampled: boolean;
    Fparent_id: string;
    Ftimestamp: Int64;
    Fexperience: TTransactionExperience;
    Foutcome: string;
    Fsession: TTransactionSession;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const AName: string; const AType: string = 'Undefined');

    procedure ToEnd(const AOutcome: TOutcome = success);

    function ToJsonString: string;

    // <summary>
    // Outcome of the transaction with a limited set of permitted values, describing the success or failure of the transaction
    // from the service's perspective. It is used for calculating error rates for incoming requests.
    // Permitted values: success, failure, unknown
    // </summary>
    procedure SetOutcome(const AOutcome: TOutcome);

    procedure AddSession(const AId: string);
    procedure AddContextRequest(const AHttpMethod: string);

    property id: string read Fid;
    property Trace_id: string read Ftrace_id write Ftrace_id;
    property Parent_id: string read Fparent_id write Fparent_id;

    // <summary>
    // Type expresses the transaction's type as keyword that has specific relevance
    // within the service's domain, eg: 'request', 'backgroundjob'
    // </summary>
    property &type: string read Ftype;
    property Span_count: TTransactionSpanCount read Fspan_count;
    property Context: TContext read Fcontext write Fcontext;
    property Duration: Int64 read Fduration write Fduration;

    // <summary>
    // Result of the transaction. For HTTP-related transactions,  should be the status code formatted like 'HTTP 2xx
    // </summary>
    property &result: string read Fresult write Fresult;

    property Sampled: boolean read Fsampled write Fsampled;
    property Timestamp: Int64 read Ftimestamp;
    property Experience: TTransactionExperience read Fexperience write Fexperience;
  end;

implementation

uses
  SysUtils, DateUtils, REST.Json, Rtti, Share.Uuid, Share.TimestampEpoch;

{ TTransaction }

procedure TTransaction.AddContextRequest(const AHttpMethod: string);
begin
  Fcontext := TContext.Create;
  Fcontext.AddRequest(AHttpMethod);
end;

procedure TTransaction.AddSession(const AId: string);
begin
  Fsession := TTransactionSession.Create(AId);
end;

constructor TTransaction.Create;
begin
  Fspan_count := TTransactionSpanCount.Create;
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

procedure TTransaction.ToEnd(const AOutcome: TOutcome);
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
  Result := format('{"transaction": %s}', [TJson.ObjectToJsonString(Self, [joIgnoreEmptyStrings])]);
end;

end.
