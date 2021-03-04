unit ElasticAPM4D.User;

interface

type
  TUser = class
  private
    FEmail: String;
    FId: String;
    FUsername: String;
  public
    property id: String read FId write FId;
    property username: String read FUsername write FUsername;
    property email: String read FEmail write FEmail;
  end;

implementation

end.
