{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Span.Context.Http;

interface

uses
  Apm4D.Share.Response;

type
  // <summary>
  // HTTP contains contextual information when the Apm4D.Span concerns an HTTP request
  // </summary>
  TSpanContextHttp = class
  private
    FMethod: string;
    FStatus_code: Integer;
    FUrl: String;
    Fresponse: TResponse;
  public
    constructor Create(const AHttpMethod: string);
    destructor Destroy; override;

    procedure AddStatusCode(const AStatusCode: Integer);
    // <summary>
    // URL is the raw url of the correlating HTTP request
    // </summary>
    property url: String read FUrl write FUrl;
    property Response: TResponse read Fresponse;
    property Method: String read FMethod;
    property Status_code: Integer read FStatus_code;
  end;

implementation

{ THttp }

procedure TSpanContextHttp.AddStatusCode(const AStatusCode: Integer);
begin
  FStatus_code := AStatusCode;
  Fresponse := TResponse.Create(AStatusCode);
end;

constructor TSpanContextHttp.Create(const AHttpMethod: string);
begin
  FMethod := AHttpMethod;
end;

destructor TSpanContextHttp.Destroy;
begin
  if Assigned(Fresponse) then
    Fresponse.Free;
  inherited;
end;

end.
