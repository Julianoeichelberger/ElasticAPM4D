{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Span;

interface

uses
  Apm4D.Share.Stacktrace,
  Apm4D.Span.Context,
  Apm4D.Serializer;

type
  TSpan = class
  private
    FIsPaused: Boolean;
    FStartDate: TDateTime;
    FPauseStartDate: TDateTime;
    FPausedDuration: Int64;
    FAction: string;
    FContext: TSpanContext;
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
    Foutcome: string;
  public
    constructor Create(const ATraceId, ATransactionId, AParentId: string);
    destructor Destroy; override;

    function ToJsonString: string;

    procedure Start(const AName: string; const AType: string = 'Undefined';
      const ATransactionPausedDuration: Int64 = 0);
    procedure StartRequest(const AName, AHttpMethod: string; const ATransactionPausedDuration: Int64 = 0);

    procedure Pause;
    procedure UnPause;
    function IsPaused: Boolean;

    procedure ToEnd;
    procedure ToEndRequest(const AHttpCode: Integer);

    property Id: string read FId;
    property Transaction_id: string read FTransaction_id;
    property Trace_id: string read FTrace_id;
    property Parent_id: string read FParent_id;
    property Name: string read FName;
    property &Type: string read FType;
    property Subtype: string read FSubtype write FSubtype;
    property Action: string read FAction write FAction;
    property Context: TSpanContext read FContext;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property Sync: Boolean read FSync write FSync default true;
    property Timestamp: Int64 read Ftimestamp;
    property Duration: Int64 read FDuration;
    property Outcome: string read Foutcome write Foutcome;
  end;

implementation

{ TSpan }

uses
  Apm4D.Share.Stacktrace.Jcl, System.SysUtils, System.DateUtils, Apm4D.Share.Uuid, Apm4D.Share.TimestampEpoch;

constructor TSpan.Create(const ATraceId, ATransactionId, AParentId: string);
begin
  FId := TUUid.Get64b;
  FTrace_id := ATraceId;
  FTransaction_id := ATransactionId;
  FParent_id := AParentId;
  FAction := '';
  FSubtype := '';
  FSync := true;
  FDuration := 0;
  FContext := TSpanContext.Create;
  FIsPaused := false;
end;

destructor TSpan.Destroy;
var
  Stacktrace: TStacktrace;
begin
  for Stacktrace in FStacktrace do
    Stacktrace.Free;
  FreeAndNil(FContext);
  inherited;
end;

procedure TSpan.Pause;
begin
  if not FIsPaused then
  begin
    FPauseStartDate := now;
    FIsPaused := true;
  end;
end;

function TSpan.IsPaused: Boolean;
begin
  Result := FIsPaused;
end;

procedure TSpan.UnPause;
begin
  if FIsPaused then
  begin
    FPausedDuration := FPausedDuration + MilliSecondsBetween(now, FPauseStartDate);
    FIsPaused := false;
  end;
end;

procedure TSpan.ToEnd;
begin
  if FIsPaused then
    UnPause;
  FStacktrace := TStacktraceJCL.Get;
  FDuration := MilliSecondsBetween(now, FStartDate) - FPausedDuration;
  // Set default outcome if not already set
  if Foutcome.IsEmpty then
    Foutcome := 'success';
end;

procedure TSpan.ToEndRequest(const AHttpCode: Integer);
begin
  FContext.Http.AddStatusCode(AHttpCode);
  ToEnd;
end;

procedure TSpan.Start(const AName, AType: string; const ATransactionPausedDuration: Int64);
begin
  FStartDate := now;
  Ftimestamp := TTimestampEpoch.Get(FStartDate) - (ATransactionPausedDuration * 1000);
  FName := AName;
  FType := AType;
  FPausedDuration := 0;
  // Auto-set action for database spans
  if (AType = 'database') or (AType = 'db') then
  begin
    if FAction.IsEmpty then
      FAction := 'query';
  end;
end;

procedure TSpan.StartRequest(const AName, AHttpMethod: string; const ATransactionPausedDuration: Int64 = 0);
begin
  Start(AName, 'HttpRequest', ATransactionPausedDuration);
  FContext.CreateHttp(AHttpMethod);
end;

function TSpan.ToJsonString: string;
begin
  Result := TApm4DSerializer.ToJSON(Self, 'span');
end;

end.
