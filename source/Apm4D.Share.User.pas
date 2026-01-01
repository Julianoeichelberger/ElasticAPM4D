{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.User;

interface

type
  // User Apm4D.Metadata, which can be overwritten on a per event basis..
  TUser = class
  private
    FEmail: String;
    FId: String;
    FUsername: String;
    FDomain: string;
  public
    constructor Create;
    property Id: String read FId;
    property Username: String read FUsername;
    property Domain: string read FDomain write FDomain;
    property Email: String read FEmail;
  end;

implementation

uses
  Apm4D.Settings;

{ TUser }

constructor TUser.Create;
begin
  FId := TApm4DSettings.User.Id;
  FUsername := TApm4DSettings.User.Name;
  FEmail := TApm4DSettings.User.Email;
end;

end.
