unit ElasticAPM4D.TimestampEpoch;

interface

type
  TElasticAPM4DTimestampEpoch = class
    class function Get(ADate: TDatetime): Int64;
  end;

implementation

Uses
  System.SysUtils,
  System.DateUtils;

{ TElasticAPM4DTimestampEpoch }

class function TElasticAPM4DTimestampEpoch.Get(ADate: TDatetime): Int64;
begin
  Result := StrToInt64(FormatFloat('0', DateTimeToUnix(ADate, False)) + FormatDateTime('zzz', ADate) + '000');
end;

end.
