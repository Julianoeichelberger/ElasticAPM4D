{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Service.Agent;

interface

type
  // <summary>
  // Agent holds information about the APM agent capturing the event - Apm4D
  // </summary>
  TServiceAgent = class
  private
    FName: String;
    FVersion: String;
    // phemeralID is a free format ID used for metrics correlation by agents
    Fephemeral_id: string;
  public
    constructor Create;

    property Name: String read FName;
    property Version: String read FVersion;
    property Ephemeral_id: string read Fephemeral_id write Fephemeral_id;
  end;

implementation

{ TServiceAgent }

constructor TServiceAgent.Create;
begin
  FVersion := '3.0.0';
  FName := 'Apm4D';
end;

end.
