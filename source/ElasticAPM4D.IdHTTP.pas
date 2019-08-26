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
var
  LExistsTransaction: Boolean;
  LName: string;
begin
  LExistsTransaction := TElasticAPM4D.ExistsTransaction;
  LName := AURL;
  if not LExistsTransaction then
  begin
    TElasticAPM4D.StartTransaction(Self, AURL);
    LName := 'DoRequest';
  end;
  TElasticAPM4D.StartSpan(Self, LName);
  try
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
    TElasticAPM4D.EndSpan(Self);
    if not LExistsTransaction then
      TElasticAPM4D.EndTransaction(Self);
  End;
end;

end.
