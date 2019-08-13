unit ElasticAPM4D.TimestampEpoch;

interface

type
  TElasticAPM4DTimestampEpoch = class
    class function Now: Int64;
  end;

implementation

Uses SysUtils, DateUtils;

{ TElasticAPM4DTimestampEpoch }

class function TElasticAPM4DTimestampEpoch.Now: Int64;
var
  LDate: TDateTime;
begin
  LDate := Now;
  Result := StrToInt64(FormatFloat('0', DateTimeToUnix(LDate, False)) + FormatDateTime('zzz', LDate) + '000');
end;

end.
