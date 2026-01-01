{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Service.Node;

interface

type
  // <summary>
  // Node must be a unique meaningful name of the service node
  // </summary>
  TServiceNode = class
  private
    FConfigured_name: string;
  public
    constructor Create(const AConfiguredName: string);

    // <summary>
    // Name of the service node
    // </summary>
    property Configured_name: String read FConfigured_name;
  end;

implementation

{ TServiceNode }

constructor TServiceNode.Create(const AConfiguredName: string);
begin
  FConfigured_name := AConfiguredName;
end;

end.
