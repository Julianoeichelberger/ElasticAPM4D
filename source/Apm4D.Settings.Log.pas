unit Apm4D.Settings.Log;

interface

type
  TLogLevel = (llTrace, llDebug, llInfo, llWarning, llError, llCritical);

  TLogSettings = class
  strict private 
    FEnabled: Boolean;
    FLevel: TLogLevel;
    FOutputFileDir: string; 
  public
    constructor Create; 

    property Enabled: Boolean read FEnabled write FEnabled;
    property Level: TLogLevel read FLevel write FLevel;
    property OutputFileDir: string read FOutputFileDir write FOutputFileDir;
  end;

implementation

{ TLogSettings }

constructor TLogSettings.Create;
begin
  inherited; 
  FEnabled := True;
  FLevel := llInfo;
  FOutputFileDir := '';
end;

end.