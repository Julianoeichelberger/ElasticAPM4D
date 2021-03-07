unit ElasticAPM4D.Helper.Indy;

interface

uses
  System.Classes, System.SysUtils, IdHTTP;

type
  TIdHTTP = class(IdHTTP.TIdHTTP)
  protected
    procedure DoRequest(const AMethod: TIdHTTPMethod; AURL: string; ASource, AResponseContent: TStream;
      AIgnoreReplies: array of Int16); override;
  end;

implementation

Uses
  ElasticAPM4D;

{ TIdHTTP }

procedure TIdHTTP.DoRequest(const AMethod: TIdHTTPMethod; AURL: string;
  ASource, AResponseContent: TStream; AIgnoreReplies: array of Int16);
var
  LExistsTransaction: Boolean;
  LName: string;
begin
  LExistsTransaction := TElasticAPM4D.ExistsTransaction;
  LName := AURL;
  if not LExistsTransaction then
  begin
    TElasticAPM4D.StartTransaction('Indy', AURL);
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
      TElasticAPM4D.EndTransaction;
  End;
end;

end.
