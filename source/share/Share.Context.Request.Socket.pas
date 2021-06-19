unit Share.Context.Request.Socket;

interface

type
  // <summary>
  // Socket holds information related to the recorded request,
  // such as whether or not data were encrypted and the remote address.
  // </summary>
  TContextRequestSocket = class
  private
    FEncrypted: Boolean;
    FRemote_address: string;
  public
    property Encrypted: Boolean read FEncrypted write FEncrypted;
    property Remote_address: string read FRemote_address write FRemote_address;
  end;

implementation

end.
