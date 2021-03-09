unit ElasticAPM4D.User;

interface

type
  TUser = class
  private
    FEmail: String;
    FId: String;
    FUsername: String;
  public
    property Id: String read FId write FId;
    property Username: String read FUsername write FUsername;
    property Email: String read FEmail write FEmail;
  end;

implementation

end.
