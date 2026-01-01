{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Service.Runtime;

interface

type
  // <summary>
  // Runtime holds information about the language runtime running the monitored service
  // </summary>
  TServiceRuntime = class
  private
    FName: string;
    FVersion: String;
  public
    constructor Create(const AName, AVersion: string);
    property Name: String read FName;
    property Version: String read FVersion;
  end;

implementation

{ TServiceRuntime }

constructor TServiceRuntime.Create(const AName, AVersion: string);
begin
  FName := AName;
  FVersion := AVersion;
end;

end.
