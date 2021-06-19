unit Sample.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ELasticAPM4D, REST.HttpClient,
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
    procedure RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
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
  TApm.StartTransactionRequest(RESTRequest.Resource);
  try
    RESTRequest.Execute;
  finally
    TApm.EndTransaction(RESTRequest.Response.StatusCode);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TApm.StartTransaction('Sleeps', 'Test');
  try
    try
      TApm.StartSpan('Sleep1');
      Sleep(500);
      TApm.EndSpan;

      TApm.StartSpan('Sleep2');
      Sleep(750);
      TApm.EndSpan;

      Abort;
    except
      on E: exception do
        TApm.AddError(E);
    end;
  finally
    TApm.EndTransaction;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RESTRequest.Resource := '01001000/jso/';
  TApm.StartTransactionRequest(RESTRequest.Resource);
  try
    try
      TApm.StartSpan('Sleep1');
      Sleep(500);
      TApm.EndSpan;

      TApm.StartSpan('Sleep2');
      Sleep(650);
      TApm.EndSpan;

      RESTRequest.Execute;
    except
      on E: EHTTPProtocolException do
        TApm.AddError(E);
    end;
  finally
    TApm.EndTransaction(RESTRequest.Response.StatusCode);
  end;
end;

procedure TForm1.RESTRequestHTTPProtocolError(Sender: TCustomRESTRequest);
begin
  TApm.AddError(Sender.Response);
end;

end.
