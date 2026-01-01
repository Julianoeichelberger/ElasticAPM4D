{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Response;

interface

type
  // <summary>
  // Response describes the HTTP response information in case the event was created as a result of an HTTP request
  // </summary>
  TResponse = class
  private
    FStatus_code: Integer;
    Fencoded_body_size: double;
    Ftransfer_size: double;
    Fdecoded_body_size: double;
  public
    constructor Create(const AStatusCode: Integer);
    property decoded_body_size: double read Fdecoded_body_size write Fdecoded_body_size;
    property encoded_body_size: double read Fencoded_body_size write Fencoded_body_size;
    property transfer_size: double read Ftransfer_size write Ftransfer_size;
  end;

implementation

{ TResponse }

constructor TResponse.Create(const AStatusCode: Integer);
begin
  FStatus_code := AStatusCode;
end;

end.
