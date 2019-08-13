unit ElasticAPM4D.StacktraceFrame;

interface

type
  TElasticAPM4DStacktrace = class
  private
    FAbs_path: String;
    FColno: integer;
    FContext_line: String;
    FFilename: String;
    FFunction: String;
    FLibrary_frame: Boolean;
    FLineno: integer;
    FModule: String;
    FPost_context: TArray<String>;
    FPre_context: TArray<String>;
    Fvars: TObject;
  public
    property abs_path: String read FAbs_path write FAbs_path;
    property colno: integer read FColno write FColno;
    property context_line: String read FContext_line write FContext_line;
    property filename: String read FFilename write FFilename;
    property &function: String read FFunction write FFunction;
    property library_frame: Boolean read FLibrary_frame write FLibrary_frame default false;
    property lineno: integer read FLineno write FLineno;
    property module: String read FModule write FModule;
    property post_context: TArray<String> read FPost_context write FPost_context;
    property pre_context: TArray<String> read FPre_context write FPre_context;
    property vars: TObject read Fvars write Fvars;
  end;

implementation

end.
