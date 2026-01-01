{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.Cloud.Machine;

interface

type
  // Machine on which the monitored service is running
  TMetadataCloudMachine = class
  private
    // ID of the cloud machine.
    Fid: string;
  public
    constructor Create(const Aid: string);
  end;

implementation

{ TMetadataCloudMachine }

constructor TMetadataCloudMachine.Create(const Aid: string);
begin
  Fid := Aid;
end;

end.
