unit ElasticAPM4D.Resources;

interface

const
  REGEx_LINE = '\(Line (?<linha>\d+)';
  REGEx_CLASSNAME = '(\T[a-zA-Z0-9_]+)';
  REGEx_CONTEXT_LINE = '(?<=\])(.*?)(?=\+)';

  LIST_REGEx_UNIT_NAME: array [0 .. 3] of string = ('\ ?"(?<arquivo>[0-9_a-zA-Z.]+)".*\)',
    '\] (vcl\.[a-zA-Z0-9_]+|Vcl\.[a-zA-Z0-9_]+)', '\] (system\.[a-zA-Z0-9_]+|System\.[a-zA-Z0-9_]+)',
    '\] ([a-zA-Z0-9_]+)');

  REGEx_MAKE_PARSE = '(?<=\])(.*?)(?=\+)';

  sHEADER = '00-%s-%s-01';

  sHEADER_KEY = 'elastic-apm-traceparent';
  sSpanJsonId = '{"span": %s}';
  sTransactionJsonId = '{"transaction": %s}}';
  sMetadataJsonId = '{"metadata": %s}';
  sErrorJsonId = '{"error": %s}';

  sTransactionNotFount = 'Current transaction not found';
  sDuplicateTransaction = 'Duplicate active transactions';

implementation

end.
