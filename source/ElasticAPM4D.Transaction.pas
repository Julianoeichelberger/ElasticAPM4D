unit ElasticAPM4D.Transaction;

interface

uses
  System.Classes, System.SysUtils, ElasticAPM4D.Context;

type
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

  TTransaction = class
  private
    FStartDate: TDateTime;
    Fid: string;
    Ftrace_id: string;
    Fname: string;
    Ftype: string;
    Fresult: string;
    Fduration: int64;
    Fcontext: TContext;
    Fspan_count: TSpanCount;
    Fsampled: boolean;
    Fparent_id: string;
    Ftimestamp: int64;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(const AType, AName: string);
    procedure &End;

    function ToJsonString: string;

    property Id: string read Fid;
    property Trace_id: string read Ftrace_id write Ftrace_id;
    property Parent_id: string read Fparent_id write Fparent_id;
    property Name: string read Fname write Fname;
    property &type: string read Ftype;
    property Span_count: TSpanCount read Fspan_count;
    property Context: TContext read Fcontext write Fcontext;
    property Duration: int64 read Fduration write Fduration;
    property &result: string read Fresult write Fresult;
    property Sampled: boolean read Fsampled write Fsampled;
    property Timestamp: int64 read Ftimestamp write Ftimestamp;
  end;

implementation

Uses
  System.DateUtils, REST.Json, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

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

{ TTransaction }

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
  Fspan_count.Free;
  inherited;
end;

procedure TTransaction.&End;
begin
  Fduration := MilliSecondsBetween(now, FStartDate);
end;

procedure TTransaction.Start(const AType, AName: string);
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
