unit ElasticAPM4D.MetricSet;

interface

type
  TSamples = class
  private
    Fvalue: Double;
  public
    property value: Double read Fvalue write Fvalue;
  end;

  TMetricSet<T: TSamples, constructor> = class
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
  System.SysUtils, Rest.Json, ElasticAPM4D.Utils, ElasticAPM4D.Resources;

{ TMetricSet<T> }

constructor TMetricSet<T>.Create;
begin
  Fsamples := T.Create;
  Ftimestamp := TTimestampEpoch.Get(now);
end;

destructor TMetricSet<T>.Destroy;
begin
  Fsamples.Free;
  inherited;
end;

function TMetricSet<T>.ToJsonString: string;
begin
  result := format(smetricSetJsonId, [TJson.ObjectToJsonString(self)]);
end;

end.
