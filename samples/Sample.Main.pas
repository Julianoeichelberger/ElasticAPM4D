unit Sample.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ELasticAPM4D, ELasticAPM4D.RESTClient, REST.HttpClient,
  REST.Types, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
begin
  RESTRequest.Resource := '01001000/json/';
  StartTransaction(RESTRequest); // REST.Client
  try
    RESTRequest.Execute;
  finally
    EndTransaction(RESTResponse);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TElasticAPM4D.StartTransaction('Sleep', 'Test');
  try
    try
      TElasticAPM4D.StartSpan('Sleep1');
      Sleep(500);
      TElasticAPM4D.EndSpan;

      TElasticAPM4D.StartSpan('Sleep2');
      Sleep(3000);
      TElasticAPM4D.EndSpan;

      Abort;
    except
      on E: exception do
        TElasticAPM4D.AddError(E);
    end;
  finally
    TElasticAPM4D.EndTransaction;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RESTRequest.Resource := '01001000/jso/';
  StartTransaction(RESTRequest);
  try
    try
      TElasticAPM4D.StartSpan('Sleep1');
      Sleep(500);
      TElasticAPM4D.EndSpan;

      TElasticAPM4D.StartSpan('Sleep2');
      Sleep(3000);
      TElasticAPM4D.EndSpan;

      RESTRequest.Execute;
    except
      on E: EHTTPProtocolException do
        TElasticAPM4D.AddError(E);
    end;
  finally
    EndTransaction(RESTResponse);
  end;
end;

end.
