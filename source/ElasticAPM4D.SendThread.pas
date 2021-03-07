unit ElasticAPM4D.SendThread;

interface

Uses
  System.Classes;

type
  TSendThread = class(TThread)
  private
    FJson: WideString;
    FHeader: string;
    FUrl: string;
    FCouldSend: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AURL: string);
    procedure Send(const AHeader: string; const AJson: WideString);
  end;

implementation

Uses
  System.IOUtils, System.SysUtils, IdHTTP, ElasticAPM4D.Resources;

{ TSendThread }

constructor TSendThread.Create(const AURL: string);
begin
  inherited Create(True);
  FCouldSend := True;
  FreeOnTerminate := True;
  FUrl := AURL;
  FHeader := '';
end;

procedure TSendThread.Execute;
var
  LHttp: TIdHTTP;
  LDataSend, LResult: TStringStream;
begin
  inherited;
  LHttp := TIdHTTP.Create;
  LDataSend := TStringStream.Create(FJson, TEncoding.UTF8);
  LResult := TStringStream.Create('');
  try
    LHttp.Request.ContentType := 'application/x-ndjson';
    LHttp.Request.Charset := 'gzip';
    LHttp.Request.CustomHeaders.AddValue(sHEADER_KEY, FHeader);
    try
      LHttp.Post(FUrl, LDataSend, LResult);
      // LDataSend.SaveToFile(GetCurrentDir + '/' + TPath.GetFileNameWithoutExtension(Application.ExeName) + '_'
      // + LHttp.ResponseCode.ToString + '_' + FHeader + '.txt');
    except
      FCouldSend := False;
      // LDataSend.SaveToFile(GetCurrentDir + '/' + TPath.GetFileNameWithoutExtension(Application.ExeName) + '_'
      // + LHttp.ResponseCode.ToString + '_' + FHeader + '.txt');
    end;
  finally
    LDataSend.Free;
    LResult.Free;
    LHttp.Free;
  end;
end;

procedure TSendThread.Send(const AHeader: string; const AJson: WideString);
begin
  FJson := AJson;
  FHeader := AHeader;
  Start;
end;

end.
