unit Metricset.Samples;

interface

uses
  Classes, Rtti, SysUtils, StrUtils, REST.Json;

type
  TSampleType = (gauge, counter, histogram);

  TSampleUnit = (
    msuUnknown, msuPercent, msuByte, msuNanos, msuMicros, msuMicrossecunds, msuSecunds, msuMinuts, msuHours, msuDays);

  TMetricsetSamples = class
  private
    FList: TStringList;
    function UnitToString(const AUnit: TSampleUnit): string;
    function TypeToString(const AType: TSampleType): string;
    function FormatName(const AName: string; const AUnit: TSampleUnit; const AType: TSampleType): string;
  public
    constructor Create;
    destructor Destroy; override;

    // Type holds an optional metric type: gauge, counter, or histogram. If Type is unknown, it will be ignored.
    // Unit holds an optional unit for the metric.
    // - \"percent\" (value is in the range [0,1])
    // - \"byte\"
    // - a time unit: \"nanos\", \"micros\", \"ms\", \"s\", \"m\", \"h\", \"d\"
    // If Unit is unknown, it will be ignored.

    procedure AddPercentageGauge(const AName: string; const AValue: Currency);
    procedure AddHistogram(const AName: string; const AUnit: TSampleUnit; const AValues: TArray<Currency>);
    procedure AddCustom(const AName: string; const AUnit: TSampleUnit; const AType: TSampleType; const AValue: Currency);

    function ToJsonString: string;
  end;

function FormatCurr(const AValue: Currency): string;

implementation

function FormatCurr(const AValue: Currency): string;
begin
  Result := StringReplace(FormatFloat('0.00', AValue), ',', '.', []);
end;

{ TMetricsetSamples }

procedure TMetricsetSamples.AddCustom(const AName: string;
  const AUnit: TSampleUnit; const AType: TSampleType; const AValue: Currency);
begin
  FList.Add(Format('"%s":{"value": %s}', [FormatName(AName, AUnit, AType), FormatCurr(AValue)]));
end;

procedure TMetricsetSamples.AddHistogram(const AName: string; const AUnit: TSampleUnit; const AValues: TArray<Currency>);

  function ArrayToString: string;
  var
    Value: Currency;
  begin
    Result := '';
    for Value in AValues do
      Result := Result + IfThen(not Result.IsEmpty, ',') + FormatCurr(Value);
  end;

begin
  FList.Add(Format('"%s":{"values": [%s]}', [FormatName(AName, AUnit, histogram), ArrayToString]));
end;

procedure TMetricsetSamples.AddPercentageGauge(const AName: string; const AValue: Currency);
begin
  AddCustom(AName, msuPercent, gauge, AValue);
end;

constructor TMetricsetSamples.Create;
begin
  FList := TStringList.Create;
  FList.Delimiter := ',';
  FList.QuoteChar := ' ';
  FList.StrictDelimiter := true;
end;

destructor TMetricsetSamples.Destroy;
begin
  FList.Free;
  inherited;
end;

function TMetricsetSamples.FormatName(const AName: string; const AUnit: TSampleUnit; const AType: TSampleType): string;
begin
  Result := Format('%s%s.%s', [IfThen(not AName.IsEmpty, AName + '.'), UnitToString(AUnit), TypeToString(AType)]);
end;

function TMetricsetSamples.ToJsonString: string;
begin
  Result := Format('"samples": {%s}', [FList.DelimitedText]);
end;

function TMetricsetSamples.TypeToString(const AType: TSampleType): string;
const
  TYPE_STR: array [TSampleType] of string = ('gauge', 'counter', 'histogram');
begin
  Result := TYPE_STR[AType];
end;

function TMetricsetSamples.UnitToString(const AUnit: TSampleUnit): string;
const
  UNIT_STR: array [TSampleUnit] of string = (
    'unknown', 'percent', 'byte', 'nanos', 'micros', 'ms', 's', 'm', 'h', 'd');
begin
  Result := UNIT_STR[AUnit];
end;

end.
