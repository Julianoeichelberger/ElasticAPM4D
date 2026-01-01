{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.TimestampEpoch;

interface

type
  TTimestampEpoch = class
    class function Get(ADate: TDatetime): Int64;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TTimestampEpoch }

class function TTimestampEpoch.Get(ADate: TDatetime): Int64;
var
  LValueUTC: TDateTime;
  LEpoch: TDateTime;
begin
  LValueUTC := TTimeZone.Local.ToUniversalTime(ADate);
  LEpoch := UnixDateDelta;
  Result := MilliSecondsBetween(LValueUTC, LEpoch) * 1000;
end;

end.
