unit Share.User;

interface

type
  // User metadata, which can be overwritten on a per event basis..
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
