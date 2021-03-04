unit ElasticAPM4D.Transaction;

interface

uses
  System.Classes,
  System.SysUtils,
  ElasticAPM4D.Context;

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

    property dropped: Integer read FDropped;
    property started: Integer read FStarted;
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

    procedure Start(AType, AName: string);
    procedure &End;

    function toJsonString: string;

    property id: string read Fid;
    property trace_id: string read Ftrace_id write Ftrace_id;
    property parent_id: string read Fparent_id write Fparent_id;
    property name: string read Fname write Fname;
    property &type: string read Ftype;
    property span_count: TSpanCount read Fspan_count;
    property Context: TContext read Fcontext;
    property duration: int64 read Fduration write Fduration;
    property &result: string read Fresult write Fresult;
    property sampled: boolean read Fsampled write Fsampled;
    property Timestamp: int64 read Ftimestamp write Ftimestamp;
  end;

implementation

Uses
  System.DateUtils,
  REST.Json,
  ElasticAPM4D.UUid,
  ElasticAPM4D.TimestampEpoch,
  ElasticAPM4D.Resources;

{ TElasticAPM4DSpanCount }

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
  Fcontext := TContext.Create;
  Fspan_count := TSpanCount.Create;
end;

destructor TTransaction.Destroy;
begin
  Fcontext.Free;
  Fspan_count.Free;
  inherited;
end;

procedure TTransaction.Start(AType, AName: string);
begin
  FStartDate := now;
  Fid := TElasticAPM4DUUid.GetUUid64b;
  Ftrace_id := TElasticAPM4DUUid.GetUUid128b;
  Ftimestamp := TTimestampEpoch.Get(FStartDate);
  Fsampled := true;
  Ftype := AType;
  Fname := AName;
end;

procedure TTransaction.&End;
begin
  Fduration := MilliSecondsBetween(now, FStartDate);
end;

function TTransaction.toJsonString: string;
begin
  Result := format(sTransactionJsonId, [TJson.ObjectToJsonString(Self, [joIgnoreEmptyStrings])]);
end;

end.
