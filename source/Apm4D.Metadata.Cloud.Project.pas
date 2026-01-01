{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Metadata.Cloud.Project;

interface

type
  // Project in which the monitored service is running
  TMetadataCloudProject = class
  private
    Fid: string;
    Fname: string;
  public
    // ID of the cloud project
    property id: string read Fid write Fid;
    // Name of the cloud project.
    property name: string read Fname write Fname;
  end;

implementation

end.
