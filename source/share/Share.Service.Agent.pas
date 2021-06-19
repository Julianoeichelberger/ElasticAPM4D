unit Share.Service.Agent;

interface

type
  // <summary>
  // Agent holds information about the APM agent capturing the event - ElasticAPM4D
  // </summary>
  TServiceAgent = class
  private
    FName: String;
    FVersion: String;
    // phemeralID is a free format ID used for metrics correlation by agents
    Fephemeral_id: string;
  public
    constructor Create;
    property Ephemeral_id: string read Fephemeral_id write Fephemeral_id;
  end;

implementation

{ TServiceAgent }

constructor TServiceAgent.Create;
begin
  FVersion := '2.0.0';
  FName := 'ElasticAPM4D';
end;

end.
