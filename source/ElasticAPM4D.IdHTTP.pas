unit ElasticAPM4D.IdHTTP;

interface

uses
  System.Classes,
  System.SysUtils,
  IdHTTP;

type
  TElasticAPM4DIdHttp = class(TIdCustomHTTP)
  protected
    procedure DoRequest(const AMethod: TIdHTTPMethod; AURL: string; ASource, AResponseContent: TStream;
      AIgnoreReplies: array of Int16); override;
  end;

implementation

Uses
  ElasticAPM4D;

{ TElasticAPM4DIdHttp }

procedure TElasticAPM4DIdHttp.DoRequest(const AMethod: TIdHTTPMethod; AURL: string;
  ASource, AResponseContent: TStream; AIgnoreReplies: array of Int16);
begin
  try
    TElasticAPM4D.StartTransaction(Self, AURL);
    Try
      inherited;
    except
      on E: EIdHTTPProtocolException do
      begin
        TElasticAPM4D.AddError(Self, E);
        raise;
      end;
      on E: Exception do
      begin
        TElasticAPM4D.AddError(E);
        raise;
      end;
    end;
  Finally
    TElasticAPM4D.EndTransaction(Self);
  End;
end;

end.
