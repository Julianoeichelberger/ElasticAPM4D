unit Share.Context.Page;

interface

type
  // <summary>
  // Page holds information related to the current page and page referers. It is only sent from RUM agents.
  // </summary>
  TContextPage = class
  private
    FReferer: String;
    FUrl: String;
  public
    // Referer holds the URL of the page that 'linked' to the current page.
    property Referer: String read FReferer write FReferer;
    property Url: String read FUrl write FUrl;
  end;

implementation

end.
