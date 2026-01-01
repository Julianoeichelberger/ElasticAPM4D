{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Service.Framework;

interface

type
  // <summary>
  // Framework holds information about the framework used in the monitored service.
  // </summary>
  TServiceFramework = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create(const AName, AVersion: string);

    property Name: String read FName;
    property Version: String read FVersion;
  end;

implementation

{ TServiceFramework }

constructor TServiceFramework.Create(const AName, AVersion: string);
begin
  FName := AName;
  FVersion := AVersion;
end;

end.
