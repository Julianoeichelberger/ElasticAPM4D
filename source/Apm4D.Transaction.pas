{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Transaction;

interface

uses

  Apm4D.Share.Context,
  Apm4D.Share.Types,
  Apm4D.Transaction.Experience,
  Apm4D.Transaction.SpanCount,
  Apm4D.Transaction.Session,
  Apm4D.Serializer;

type
  TTransaction = class
  private
    FStartDate: TDateTime;
    FPauseStartDate: TDateTime;
    FPausedDuration: Int64;
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
    FIsPaused: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const AName: string; const AType: string = 'Undefined');

    procedure Pause;
    function IsPaused: Boolean;
    function GetPausedDuration: Int64;
    function GetPauseStartDate: TDateTime;
    procedure UnPause;

    procedure ToEnd(const AOutcome: TOutcome = success);

    function ToJsonString: string;

    // <summary>
    // Outcome of the Transaction with a limited set of permitted values, describing the success or failure of the Apm4D.Transaction
    // from the service's perspective. It is used for calculating Error rates for incoming requests.
    // Permitted values: success, failure, unknown
    // </summary>
    procedure SetOutcome(const AOutcome: TOutcome);

    procedure AddSession(const AId: string);
    procedure AddContextRequest(const AHttpMethod: string);

    property id: string read Fid;
    property Trace_id: string read Ftrace_id write Ftrace_id;
    property Parent_id: string read Fparent_id write Fparent_id;
    property Name: string read Fname;

    // <summary>
    // Type expresses the Transaction's type as keyword that has specific relevance
    // within the service's domain, eg: 'request', 'backgroundjob'
    // </summary>
    property &type: string read Ftype;
    property Span_count: TTransactionSpanCount read Fspan_count;
    property Context: TContext read Fcontext write Fcontext;
    property Duration: Int64 read Fduration write Fduration;

    // <summary>
    // Result of the Transaction. For HTTP-related transactions,  should be the status code formatted like 'HTTP 2xx
    // </summary>
    property &result: string read Fresult write Fresult;
    property Sampled: boolean read Fsampled write Fsampled;
    property Timestamp: Int64 read Ftimestamp;
    property Experience: TTransactionExperience read Fexperience write Fexperience;
    property Outcome: string read Foutcome;
    property Session: TTransactionSession read Fsession;
  end;

implementation

uses
  SysUtils, DateUtils, Rtti, Apm4D.Share.Uuid, Apm4D.Share.TimestampEpoch;

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
  FDuration := 0;
  FIsPaused := false;
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

function TTransaction.GetPausedDuration: Int64;
begin
  Result := FPausedDuration;
end;

function TTransaction.GetPauseStartDate: TDateTime;
begin
  Result := FPauseStartDate;
end;

procedure TTransaction.Pause;
begin
  if not FIsPaused then
  begin
    FPauseStartDate := now;
    FIsPaused := true;
  end;
end;

procedure TTransaction.UnPause;
begin
  if FIsPaused then
  begin
    FPausedDuration := FPausedDuration + MilliSecondsBetween(now, FPauseStartDate);
    FIsPaused := false;
  end;
end;

function TTransaction.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

procedure TTransaction.ToEnd(const AOutcome: TOutcome);
begin
  if FIsPaused then
    UnPause;
  if Foutcome.IsEmpty then
    SetOutcome(AOutcome);
  
  // Set default result based on outcome
  if Fresult.IsEmpty then
  begin
    if Foutcome = 'success' then
      Fresult := 'success'
    else if Foutcome = 'failure' then
      Fresult := 'error'
    else
      Fresult := 'unknown';
  end;

  Fduration := MilliSecondsBetween(now, FStartDate) - FPausedDuration;
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
  FPausedDuration := 0;
end;

function TTransaction.ToJsonString: string;
begin
  Result := TApm4DSerializer.ToJSON(Self, 'transaction');
end;

end.
