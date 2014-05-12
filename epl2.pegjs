{
  function makeInteger(o) {
    return parseInt(o.join(""), 10)
  }
}

start = ML_COMMENT


  
keywordAllowedIdent = IDENT
    / TICKED_STRING_LITERAL
    / AT
    / COUNT
    / ESCAPE
    / EVERY_EXPR
    / SUM
    / AVG
    / MAX
    / MIN
    / COALESCE
    / MEDIAN
    / STDDEV
    / AVEDEV
    / EVENTS
    / FIRST
    / LAST
    / WHILE
    / MERGE
    / MATCHED
    / UNIDIRECTIONAL
    / RETAINUNION
    / RETAININTERSECTION
    / UNTIL
    / PATTERN
    / SQL
    / METADATASQL
    / PREVIOUS
    / PREVIOUSTAIL
    / PRIOR
    / WEEKDAY
    / LW
    / INSTANCEOF
    / TYPEOF
    / CAST
    / SNAPSHOT
    / VARIABLE
    / INDEX
    / WINDOW
    / LEFT
    / RIGHT
    / OUTER
    / FULL
    / JOIN
    / DEFINE
    / PARTITION
    / MATCHES
    / CONTEXT
    / FOR
    / USING
escapableStr = IDENT / EVENTS / TICKED_STRING_LITERAL
escapableIdent = IDENT / TICKED_STRING_LITERAL
timePeriod = (  yearPart monthPart? weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / monthPart weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / weekPart dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / dayPart hourPart? minutePart? secondPart? millisecondPart?
    / hourPart minutePart? secondPart? millisecondPart?
    / minutePart secondPart? millisecondPart?
    / secondPart millisecondPart?
    / millisecondPart
    )
yearPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_YEARS / TIMEPERIOD_YEAR)
monthPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_MONTHS / TIMEPERIOD_MONTH)
weekPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_WEEKS / TIMEPERIOD_WEEK)
dayPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_DAYS / TIMEPERIOD_DAY)
hourPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_HOURS / TIMEPERIOD_HOUR)
minutePart = (numberconstant/IDENT/substitution) (TIMEPERIOD_MINUTES / TIMEPERIOD_MINUTE / MIN)
secondPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_SECONDS / TIMEPERIOD_SECOND / TIMEPERIOD_SEC) 
millisecondPart = (numberconstant/IDENT/substitution) (TIMEPERIOD_MILLISECONDS / TIMEPERIOD_MILLISECOND / TIMEPERIOD_MILLISEC) 
number = IntegerLiteral / FloatingPointLiteral
substitution = QUESTION
constant = numberconstant / stringconstant / BOOLEAN_TRUE / BOOLEAN_FALSE / VALUE_NULL
numberconstant = (MINUS / PLUS)? number
stringconstant = STRING_LITERAL / QUOTED_STRING_LITERAL

// JSON
jsonvalue = constant 
    / jsonobject
    / jsonarray
jsonobject = LCURLY jsonmembers RCURLY
jsonarray = LBRACK jsonelements? RBRACK
jsonelements = jsonvalue (COMMA jsonvalue)* (COMMA)?
jsonmembers = jsonpair (COMMA jsonpair)* (COMMA)?   
jsonpair = (stringconstant / keywordAllowedIdent) COLON jsonvalue


// Tokens
CREATE = 'create'
WINDOW = 'window'
IN_SET = 'in'
BETWEEN = 'between'
LIKE = 'like'
REGEXP = 'regexp'
ESCAPE = 'escape'
OR_EXPR = 'or'
AND_EXPR = 'and'
NOT_EXPR = 'not'
EVERY_EXPR = 'every'
EVERY_DISTINCT_EXPR = 'every-distinct'
WHERE = 'where'
AS = 'as'
SUM = 'sum'
AVG = 'avg'
MAX = 'max'
MIN = 'min'
COALESCE = 'coalesce'
MEDIAN = 'median'
STDDEV = 'stddev'
AVEDEV = 'avedev'
COUNT = 'count'
SELECT = 'select'
CASE = 'case'
ELSE = 'else'
WHEN = 'when'
THEN = 'then'
END = 'end'
FROM = 'from'
OUTER = 'outer'
INNER = 'inner'
JOIN = 'join'
LEFT = 'left'
RIGHT = 'right'
FULL = 'full'
ON = 'on'  
IS = 'is'
BY = 'by'
GROUP = 'group'
HAVING = 'having'
DISTINCT = 'distinct'
ALL = 'all'
ANY = 'any'
SOME = 'some'
OUTPUT = 'output'
EVENTS = 'events'
FIRST = 'first'
LAST = 'last'
INSERT = 'insert'
INTO = 'into'
VALUES = 'values'
ORDER = 'order'
ASC = 'asc'
DESC = 'desc'
RSTREAM = 'rstream'
ISTREAM = 'istream'
IRSTREAM = 'irstream'
SCHEMA = 'schema'
UNIDIRECTIONAL = 'unidirectional'
RETAINUNION = 'retain-union'
RETAININTERSECTION = 'retain-intersection'
PATTERN = 'pattern'
SQL = 'sql'
METADATASQL = 'metadatasql'
PREVIOUS = 'prev'
PREVIOUSTAIL = 'prevtail'
PREVIOUSCOUNT = 'prevcount'
PREVIOUSWINDOW = 'prevwindow'
PRIOR = 'prior'
EXISTS = 'exists'
WEEKDAY = 'weekday'
LW = 'lastweekday'
INSTANCEOF = 'instanceof'
TYPEOF = 'typeof'
CAST = 'cast'
CURRENT_TIMESTAMP = 'current_timestamp'
DELETE = 'delete'
SNAPSHOT = 'snapshot'
SET = 'set'
VARIABLE = 'variable'
UNTIL = 'until'
AT = 'at'
INDEX = 'index'
TIMEPERIOD_YEAR = 'year'
TIMEPERIOD_YEARS = 'years'
TIMEPERIOD_MONTH = 'month'
TIMEPERIOD_MONTHS = 'months'
TIMEPERIOD_WEEK = 'week'
TIMEPERIOD_WEEKS = 'weeks'
TIMEPERIOD_DAY = 'day'
TIMEPERIOD_DAYS = 'days'
TIMEPERIOD_HOUR = 'hour'
TIMEPERIOD_HOURS = 'hours'
TIMEPERIOD_MINUTE = 'minute'
TIMEPERIOD_MINUTES = 'minutes'
TIMEPERIOD_SEC = 'sec'
TIMEPERIOD_SECOND = 'second'
TIMEPERIOD_SECONDS = 'seconds' 
TIMEPERIOD_MILLISEC = 'msec'
TIMEPERIOD_MILLISECOND = 'millisecond'
TIMEPERIOD_MILLISECONDS = 'milliseconds'
BOOLEAN_TRUE = 'true'
BOOLEAN_FALSE = 'false'
VALUE_NULL = 'null'
ROW_LIMIT_EXPR = 'limit'
OFFSET = 'offset'
UPDATE = 'update'
MATCH_RECOGNIZE = 'match_recognize'
MEASURES = 'measures'
DEFINE = 'define'
PARTITION = 'partition'
MATCHES = 'matches'
AFTER = 'after'  
FOR = 'for'  
WHILE = 'while'  
USING = 'using'
MERGE = 'merge'
MATCHED = 'matched'
EXPRESSIONDECL = 'expression'
NEWKW = 'new'
START = 'start'
CONTEXT = 'context'
INITIATED = 'initiated'
TERMINATED = 'terminated'
DATAFLOW = 'dataflow'
CUBE = 'cube'
ROLLUP = 'rollup'
GROUPING = 'grouping'
GROUPING_ID = 'grouping_id'
SETS = 'sets'



// Operators
FOLLOWMAX_BEGIN =  '-['
FOLLOWMAX_END =  ']>'
FOLLOWED_BY =  '->'
GOES =  '=>'
EQUALS =  '='
SQL_NE =  '<>'
QUESTION =  '?'
LPAREN =  '('
RPAREN =  ')'
LBRACK =  '['
RBRACK =  ']'
LCURLY =  '{'
RCURLY =  '}'
COLON =  ' = '
COMMA =  ','
EQUAL =  '=='
LNOT =  '!'
BNOT =  '~'
NOT_EQUAL =  '!='
DIV =  '/'
DIV_ASSIGN =  '/='
PLUS =  '+'
PLUS_ASSIGN =  '+='
INC =  '++'
MINUS =  '-'
MINUS_ASSIGN =  '-='
DEC =  '--'
STAR =  '*'
STAR_ASSIGN =  '*='
MOD =  '%'
MOD_ASSIGN =  '%='
GE =  '>='
GT =  '>'
LE =  '<='
LT =  '<'
BXOR =  '^'
BXOR_ASSIGN =  '^='
BOR =  '/'
BOR_ASSIGN =  '/='
LOR =  '//'
BAND =  '&'
BAND_ASSIGN =  '&='
LAND =  '&&'
SEMI =  ''
DOT =  '.'
NUM_LONG =  '\u18FF'  // assign bogus unicode characters so the token exists
NUM_DOUBLE =  '\u18FE'
NUM_FLOAT =  '\u18FD'
ESCAPECHAR =  '\\'
ESCAPEBACKTICK =  '\`'
ATCHAR =  '@'

_ = [ \t\r\n\f]?
WS = _
SL_COMMENT = '//' [^\n\r]*
ML_COMMENT = '/*' [^(*/)]* '*/'
TICKED_STRING_LITERAL = '`' ( EscapeSequence / [^`\\] )* '`'
QUOTED_STRING_LITERAL = '\'' ( EscapeSequence / [^'\\] )* '\''
STRING_LITERAL = '"' ( EscapeSequence / [^\\""] )* '"'
EscapeSequence = '\\'( 'n' / 'r' / 't' / 'b'/ 'f' / '"' / '\'' / '\\' / UnicodeEscape / OctalEscape / .)
IDENT = ( [a-z,A-Z] / '_' / '$') ( [a-z,A-Z] / '_' / [0-9]  / '$')*
IntegerLiteral = DecimalIntegerLiteral 
    / HexIntegerLiteral 
    / OctalIntegerLiteral 
    / BinaryIntegerLiteral
FloatingPointLiteral = DecimalFloatingPointLiteral / HexadecimalFloatingPointLiteral
OctalEscape = '\\' ([0-3] [0-7] [0-7] / [0-7] [0-7] / [0-7])
UnicodeEscape = '\\' 'u' HexDigit HexDigit HexDigit HexDigit              
DecimalIntegerLiteral = DecimalNumeral IntegerTypeSuffix?
HexIntegerLiteral = HexNumeral IntegerTypeSuffix?
OctalIntegerLiteral = OctalNumeral IntegerTypeSuffix?
BinaryIntegerLiteral = BinaryNumeral IntegerTypeSuffix?
IntegerTypeSuffix = [lL]
DecimalNumeral = '0' / NonZeroDigit (Digits? / Underscores Digits)
Digits = Digit (DigitOrUnderscore* Digit)?
Digit = '0' / NonZeroDigit
NonZeroDigit = [1-9]
DigitOrUnderscore = Digit / '_'
Underscores = '_'+
HexNumeral = '0' [xX] HexDigits
HexDigits = HexDigit (HexDigitOrUnderscore* HexDigit)?
HexDigit = [0-9a-fA-F]
HexDigitOrUnderscore = HexDigit /  '_'
OctalNumeral = '0' Underscores? OctalDigits
OctalDigits = OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
OctalDigit = [0-7]
OctalDigitOrUnderscore = OctalDigit / '_'
BinaryNumeral = '0' [bB] BinaryDigits
BinaryDigits = BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
BinaryDigit = [01]
BinaryDigitOrUnderscore = BinaryDigit / '_'
DecimalFloatingPointLiteral = Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    /   '.' Digits ExponentPart? FloatTypeSuffix?
    /   Digits ExponentPart FloatTypeSuffix?
    /   Digits FloatTypeSuffix
ExponentPart =  ExponentIndicator SignedInteger
SignedInteger = sign=Sign? digits=[0-9]+ {return sign + makeInteger(digits)}
ExponentIndicator = [eE]
Sign = [+-]
FloatTypeSuffix = [fFdD]
HexadecimalFloatingPointLiteral = HexSignificand BinaryExponent FloatTypeSuffix?
HexSignificand = HexNumeral '.'? / '0' HexDigits? '.' HexDigits
BinaryExponent = BinaryExponentIndicator _ SignedInteger
BinaryExponentIndicator = [pP]