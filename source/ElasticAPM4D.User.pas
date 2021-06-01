unit ElasticAPM4D.User;

interface

type
  // User holds information about the correlated user for this event. If user data are provided here, all user related
  // information from metadata is ignored, otherwise the metadata's user information will be stored with the event.
  TUser = class
  private
    FEmail: String;
    FId: String;
    FUsername: String;
    FDomain: string;
  public
    constructor Create;
    property Id: String read FId write FId;
    property Username: String read FUsername write FUsername;
    property Domain: string read FDomain write FDomain;
    property Email: String read FEmail write FEmail;
  end;

implementation

uses
  SysUtils;

{ TUser }

constructor TUser.Create;
begin
  FUsername := GetEnvironmentVariable('USERNAME');
end;

end.
