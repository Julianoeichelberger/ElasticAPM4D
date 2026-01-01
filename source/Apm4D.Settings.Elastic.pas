unit Apm4D.Settings.Elastic;

interface

type 
  TElasticSettings = class
  strict private 
    FUrl: string;
    FSecret: string;
    FUpdateTime: Integer;
    FMaxJsonPerThread: Integer; 
  public
    constructor Create;
    
    property Url: string read FUrl write FUrl;
    property Secret: string read FSecret write FSecret;
    property UpdateTime: Integer read FUpdateTime write FUpdateTime;
    property MaxJsonPerThread: Integer read FMaxJsonPerThread write FMaxJsonPerThread;
  end;

  implementation

  { TElasticSettings }

constructor TElasticSettings.Create;
begin
  inherited; 
  FUrl := 'http://127.0.0.1:8200/intake/v2/events';
  FSecret := '';
  FUpdateTime := 60000;
  FMaxJsonPerThread := 60;
end;

  end.