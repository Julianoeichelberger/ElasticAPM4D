{ ******************************************************* }
{ }
{ Delphi Elastic Apm Agent }
{ }
{ Developed by Juliano Eichelberger }
{ }
{ ******************************************************* }
unit Apm4D.Metricset;

interface

uses
  Apm4D.Metricset.Samples;

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

uses
  System.SysUtils, Apm4D.Share.TimestampEpoch;

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
  JSON_STR = '{"metricset":{"timestamp":%d,%s}}';
begin
  // Metricset inherits service information from metadata sent at stream start
  // We don't need to include service here as it would be redundant
  Result := format(JSON_STR, [TTimestampEpoch.Get(now), FSamples.ToJsonString]);
end;

end.
