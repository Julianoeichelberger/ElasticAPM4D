unit Metricset;

interface

uses
  Metricset.Samples;

type
  TMetricSetType = (gauge, counter, histogram);

  TMetricSetUnit = (msuUnknown, msuPercent, msuByte, msuNanos, msuMicros,
    msuMicrossecunds, msuSecunds, msuMinuts, msuHours, msuDays);

  // Samples hold application metrics collected from the agent
  TMetricSets = class
  private
    FSamples: TMetricsetSamples;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Samples: TMetricsetSamples read FSamples;

    function ToJsonString: string;
  end;

implementation

Uses
  System.SysUtils, Rtti, Rest.Json, Share.TimestampEpoch;

{ TMetricSet }

constructor TMetricSets.Create;
begin
  FSamples := TMetricsetSamples.Create;
end;

destructor TMetricSets.Destroy;
begin
  FSamples.Free;
  inherited;
end;

function TMetricSets.ToJsonString: string;
const
  JSON_STR = '{"metricset": {"timestamp": %d, %s}}';
begin
  Result := format(JSON_STR, [TTimestampEpoch.Get(now), FSamples.ToJsonString]);
end;

end.
