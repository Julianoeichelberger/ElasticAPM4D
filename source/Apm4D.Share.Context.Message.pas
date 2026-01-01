{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Context.Message;

interface

uses 
  Apm4D.Share.Context.Message.Age, Apm4D.Share.Context.Message.Queue;

type
  // <summary>
  // Message holds details related to message receiving and publishing if the captured event integrates with a messaging system
  // </summary>
  TContextMessage = class
  private
    // Body of the received message, similar to an HTTP request body
    FBody: string;
    FAge: TContextMessageAge;
    FHeaders: TObject;
    FQueue: TContextMessageQueue;
  public
    constructor Create(const AName, ABody: string);
    destructor Destroy; override;

    procedure AddAge(const AMilliseconds: Integer);
    property Headers: TObject read FHeaders write FHeaders;
  end;

implementation

uses 
  SysUtils;

{ TContextMessage }

procedure TContextMessage.AddAge(const AMilliseconds: Integer);
begin
  FAge := TContextMessageAge.Create(AMilliseconds);
end;

constructor TContextMessage.Create(const AName, ABody: string);
begin
  if not AName.IsEmpty then
    FQueue := TContextMessageQueue.Create(AName);

  FBody := ABody;
end;

destructor TContextMessage.Destroy;
begin
  if Assigned(FQueue) then
    FQueue.Free;
  if Assigned(FAge) then
    FAge.Free;
  if Assigned(FHeaders) then
    FHeaders.Free;
  inherited;
end;

end.
