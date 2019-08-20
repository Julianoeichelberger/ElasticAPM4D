unit ElasticAPM4D.SendThread;

interface

Uses
  System.Classes;

type
  TElasticAPM4DSendThread = class(TThread)
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
  Vcl.Forms,
  IOUtils,
  SysUtils,
  IdHTTP;

{ TElasticAPM4DSendThread }

constructor TElasticAPM4DSendThread.Create(const AURL: string);
begin
  inherited Create(True);
  FCouldSend := True;
  FreeOnTerminate := True;
  FUrl := AURL;
  FHeader := '';
end;

procedure TElasticAPM4DSendThread.Execute;
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
    LHttp.Request.CustomHeaders.AddValue('elastic-apm-traceparent', FHeader);
    try
      LHttp.Post(FUrl, LDataSend, LResult);
      LDataSend.SaveToFile(GetCurrentDir + '/' + TPath.GetFileNameWithoutExtension(Application.ExeName) + '_'
        + LHttp.ResponseCode.ToString + '_' + FHeader + '.txt');
    except
      FCouldSend := False;
      LDataSend.SaveToFile(GetCurrentDir + '/' + TPath.GetFileNameWithoutExtension(Application.ExeName) + '_'
        + LHttp.ResponseCode.ToString + '_' + FHeader + '.txt');
    end;
  finally
    LDataSend.Free;
    LResult.Free;
    LHttp.Free;
  end;
end;

procedure TElasticAPM4DSendThread.Send(const AHeader: string; const AJson: WideString);
begin
  FJson := AJson;
  FHeader := AHeader;
  Start;
end;

end.
