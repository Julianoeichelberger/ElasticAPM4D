{*******************************************************}
{                                                       }
{             Delphi Elastic Apm Agent                  }
{                                                       }
{          Developed by Juliano Eichelberger            }
{                                                       }
{*******************************************************}
unit Apm4D.Share.Service.Language;

interface

type
  // <summary>
  // Language holds information about the programming language of the monitored service.
  // </summary>
  TServiceLanguage = class
  private
    FName: String;
    FVersion: String;
  public
    constructor Create;
    property Name: String read FName;
    property Version: String read FVersion;
  end;

implementation

{ TServiceLanguage }

constructor TServiceLanguage.Create;
begin
  FName := 'Delphi/Object Pascal';
  FVersion :=
{$IFDEF VER80} 'Delphi 1'; {$ENDIF}
{$IFDEF VER90} 'Delphi 2'; {$ENDIF}
{$IFDEF VER100} 'Delphi 3'; {$ENDIF}
{$IFDEF VER120} 'Delphi 4'; {$ENDIF}
{$IFDEF VER130} 'Delphi 5'; {$ENDIF}
{$IFDEF VER140} 'Delphi 6'; {$ENDIF}
{$IFDEF VER150} 'Delphi 7'; {$ENDIF}
{$IFDEF VER160} 'Delphi 8'; {$ENDIF}
{$IFDEF VER170} 'Delphi 2005'; {$ENDIF}
{$IFDEF VER180} 'Delphi 2006'; {$ENDIF}
{$IFDEF VER185} 'Delphi 2007'; {$ENDIF}
{$IFDEF VER200} 'Delphi 2009'; {$ENDIF}
{$IFDEF VER210} 'Delphi 2010'; {$ENDIF}
{$IFDEF VER220} 'Delphi XE'; {$ENDIF}
{$IFDEF VER230} 'Delphi XE2'; {$ENDIF}
{$IFDEF VER240} 'Delphi XE3'; {$ENDIF}
{$IFDEF VER250} 'Delphi XE4'; {$ENDIF}
{$IFDEF VER260} 'Delphi XE5'; {$ENDIF}
{$IFDEF VER270} 'Delphi XE6'; {$ENDIF}
{$IFDEF VER280} 'Delphi XE7'; {$ENDIF}
{$IFDEF VER290} 'Delphi XE8'; {$ENDIF}
{$IFDEF VER300} 'Delphi 10.0 Seattle'; {$ENDIF}
{$IFDEF VER310} 'Delphi 10.1 Berlin'; {$ENDIF}
{$IFDEF VER320} 'Delphi 10.2 Tokyo'; {$ENDIF}
{$IFDEF VER330} 'Delphi 10.3 Rio'; {$ENDIF}
{$IFDEF VER340} 'Delphi 10.4 Sidney'; {$ENDIF}
{$IFDEF VER350} 'Delphi 11 Alexandria'; {$ENDIF}
{$IFDEF VER360} 'Delphi 12 Yukon'
{$ELSE} 'Unknown Delphi Version'
{$ENDIF};
end;

end.
