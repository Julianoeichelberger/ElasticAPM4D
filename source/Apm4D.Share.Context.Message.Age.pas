{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Context.Message.Age;

interface

type
  // <summary>
  // Age of the message. If the monitored messaging framework provides a timestamp for the message, agents may use it.
  // Otherwise, the sending agent can add a timestamp in milliseconds since the Unix epoch to the message's Apm4D.Metadata to
  // be retrieved by the receiving agent. If a timestamp is not available, agents should omit this field.
  // </summary>
  TContextMessageAge = class
  private
    Fms: Integer;
  public
    constructor Create(const AMilliseconds: Integer);
  end;

implementation

{ TContextMessageAge }

constructor TContextMessageAge.Create(const AMilliseconds: Integer);
begin
  Fms := AMilliseconds;
end;

end.
