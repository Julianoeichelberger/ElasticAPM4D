{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Error.Exception;

interface

uses 
  Apm4D.Share.Stacktrace;

type
  TErrorException = class
  private
    FCode: String;
    FHandled: Boolean;
    FMessage: String;
    FModule: String;
    FStacktrace: TArray<TStacktrace>;
    FType: String;
    Fattributes: TObject;
    Fparent: Integer;
    Fcause: TArray<TObject>;
  public
    constructor Create;
    destructor Destroy; override;

    property Attributes: TObject read Fattributes write Fattributes;
    property Cause: TArray<TObject> read Fcause write Fcause;
    property Code: String read FCode write FCode;
    property Handled: Boolean read FHandled write FHandled;
    property &Message: String read FMessage write FMessage;
    property Module: String read FModule write FModule;
    property Parent: Integer read Fparent write Fparent;
    property Stacktrace: TArray<TStacktrace> read FStacktrace write FStacktrace;
    property &Type: String read FType write FType;
  end;

implementation

{ TErrorException }

constructor TErrorException.Create;
begin
  FHandled := True;
end;

destructor TErrorException.Destroy;
var
  Item: TObject;
begin
  if Assigned(Fattributes) then
    Fattributes.Free;
  for Item in Fcause do
    Item.Free;
  inherited;
end;

end.
