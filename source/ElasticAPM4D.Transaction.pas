unit ElasticAPM4D.Transaction;

interface

uses
  System.Classes,
  System.SysUtils,
  ElasticAPM4D.Context;

type
  TElasticAPM4DSpanCount = class
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

  TElasticAPM4DTransaction = class
  private
    Fid: string;
    Ftrace_id: string;
    Fname: string;
    Ftype: string;
    Fresult: string;
    Fduration: int64;
    Fcontext: TElasticAPM4DContext;
    Fspan_count: TElasticAPM4DSpanCount;
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
    property span_count: TElasticAPM4DSpanCount read Fspan_count;
    property Context: TElasticAPM4DContext read Fcontext;
    property duration: int64 read Fduration write Fduration;
    property &result: string read Fresult write Fresult;
    property sampled: boolean read Fsampled write Fsampled;
    property Timestamp: int64 read Ftimestamp write Ftimestamp;
  end;

implementation

Uses
  REST.Json,
  ElasticAPM4D.UUid,
  ElasticAPM4D.TimestampEpoch;

{ TElasticAPM4DSpanCount }

constructor TElasticAPM4DSpanCount.Create;
begin
  Reset;
end;

procedure TElasticAPM4DSpanCount.Dec;
begin
  FDropped := FDropped - 1;
end;

procedure TElasticAPM4DSpanCount.Inc;
begin
  FStarted := FStarted + 1;
  FDropped := FDropped + 1;
end;

procedure TElasticAPM4DSpanCount.Reset;
begin
  FDropped := 0;
  FStarted := 0;
end;

{ TElasticAPM4DTransaction }

constructor TElasticAPM4DTransaction.Create;
begin
  Fcontext := TElasticAPM4DContext.Create;
  Fspan_count := TElasticAPM4DSpanCount.Create;
end;

destructor TElasticAPM4DTransaction.Destroy;
begin
  Fcontext.Free;
  Fspan_count.Free;
  inherited;
end;

procedure TElasticAPM4DTransaction.Start(AType, AName: string);
begin
  Fid := TElasticAPM4DUUid.GetUUid64b;
  Ftrace_id := TElasticAPM4DUUid.GetUUid128b;
  Ftimestamp := TElasticAPM4DTimestampEpoch.Get;
  Fsampled := true;
  Ftype := AType;
  Fname := AName;
end;

procedure TElasticAPM4DTransaction.&End;
begin
  Fduration := TElasticAPM4DTimestampEpoch.Get - Ftimestamp;
end;

function TElasticAPM4DTransaction.toJsonString: string;
begin
  result := TJson.ObjectToJsonString(self, [joIgnoreEmptyStrings]);
  result := format('{"transaction": %s}}', [result]);
end;

end.
