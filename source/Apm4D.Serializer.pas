{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Serializer;

interface

Uses
  System.Classes, System.SysUtils, REST.Json, System.JSON;

type
  TApm4DSerializer = class
  public
    class function ToJSON(AObject: TObject; APropertyName: string): string;
  end;

implementation

{ TApm4DSerializer }

class function TApm4DSerializer.ToJSON(AObject: TObject; APropertyName: string): string;
begin
  Result := TJSON.ObjectToJsonString(AObject, [
    joIgnoreEmptyStrings, joIgnoreEmptyArrays, joSerialPublicProps, joIndentCaseLower]);
  Result := format('{"%s": %s}', [APropertyName, Result]);
end;

end.
