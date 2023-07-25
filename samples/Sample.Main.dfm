object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Sample'
  ClientHeight = 409
  ClientWidth = 593
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnHttpOk: TButton
    Left = 16
    Top = 24
    Width = 153
    Height = 25
    Caption = 'RESTClient - 200 Ok'
    TabOrder = 0
    OnClick = btnHttpOkClick
  end
  object btnSimple: TButton
    Left = 16
    Top = 86
    Width = 153
    Height = 25
    Caption = 'Simple test'
    TabOrder = 2
    OnClick = btnSimpleClick
  end
  object btnHttpBadRequest: TButton
    Left = 16
    Top = 55
    Width = 153
    Height = 25
    Caption = 'RESTClient - 400 Bad request'
    TabOrder = 1
    OnClick = btnHttpBadRequestClick
  end
  object RESTClient: TRESTClient
    Accept = 'application/json, text/plain; q=0.9, text/html;q=0.8,'
    AcceptCharset = 'utf-8, *;q=0.8'
    BaseURL = 'https://viacep.com.br/ws'
    Params = <
      item
        Kind = pkHTTPHEADER
        Name = 'TESTE'
        Value = 'TESTE 1'
      end>
    Left = 384
    Top = 24
  end
  object RESTRequest: TRESTRequest
    Client = RESTClient
    Params = <>
    Response = RESTResponse
    SynchronizedEvents = False
    OnHTTPProtocolError = RESTRequestHTTPProtocolError
    Left = 464
    Top = 32
  end
  object RESTResponse: TRESTResponse
    ContentType = 'application/json'
    Left = 296
    Top = 40
  end
end
