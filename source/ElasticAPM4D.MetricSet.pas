unit ElasticAPM4D.MetricSet;

interface

type
  TElasticAPM4DSamples = class
  private
    Fvalue: Double;
  public
    property value: Double read Fvalue write Fvalue;
  end;

  TElasticAPM4DMetricSet<T: TElasticAPM4DSamples, constructor> = class
  private
    Fsamples: T;
    Ftimestamp: Int64;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ToJsonString: string;

    property Timestamp: Int64 read Ftimestamp write Ftimestamp;
    property Samples: T read Fsamples write Fsamples;
  end;

implementation

Uses
  System.SysUtils,
  Rest.Json,
  ElasticAPM4D.TimestampEpoch,
  ElasticAPM4D.Resources;

{ TElasticAPM4DMetricSet<T> }

constructor TElasticAPM4DMetricSet<T>.Create;
begin
  Fsamples := T.Create;
  Ftimestamp := TElasticAPM4DTimestampEpoch.Get(now);
end;

destructor TElasticAPM4DMetricSet<T>.Destroy;
begin
  Fsamples.Free;
  inherited;
end;

function TElasticAPM4DMetricSet<T>.ToJsonString: string;
begin
  result := format(smetricSetJsonId, [TJson.ObjectToJsonString(self)]);
end;

end.
