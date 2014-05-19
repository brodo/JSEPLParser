%lex

%% 
\s+                   /* skip whitespace */
// Tokens
"create"             return "CREATE";
"window"             return "WINDOW";
"in"                 return "IN_SET";
"between"            return "BETWEEN";
"like"               return "LIKE";
"regexp"             return "REGEXP";
"escape"             return "ESCAPE";
"or"                 return "OR_EXPR";
"and"                return "AND_EXPR";
"not"                return "NOT_EXPR";
"every"              return "EVERY_EXPR";
"every-distinct"     return "EVERY_DISTINCT_EXPR";
"where"              return "WHERE";
"as"                 return "AS";  
"sum"                return "SUM";
"avg"                return "AVG";
"max"                return "MAX";
"min"                return "MIN";
"coalesce"           return "COALESCE";
"median"             return "MEDIAN";
"stddev"             return "STDDEV";
"avedev"             return "AVEDEV";
"count"              return "COUNT";
"select"             return "SELECT";
"case"               return "CASE";
"else"               return "ELSE";
"when"               return "WHEN";
"then"               return "THEN";
"end"                return "END";
"from"               return "FROM";
"outer"              return "OUTER";
"inner"              return "INNER";
"join"               return "JOIN";
"left"               return "LEFT";
"right"              return "RIGHT";
"full"               return "FULL";
"on"                 return "ON";  
"is"                 return "IS";
"by"                 return "BY";
"group"              return "GROUP";
"having"             return "HAVING";
"distinct"           return "DISTINCT";
"all"                return "ALL";
"any"                return "ANY";
"some"               return "SOME";
"output"             return "OUTPUT";
"events"             return "EVENTS";
"first"              return "FIRST";
"last"               return "LAST";
"insert"             return "INSERT";
"into"               return "INTO";
"values"             return "VALUES";
"order"              return "ORDER";
"asc"                return "ASC";
"desc"               return "DESC";
"rstream"            return "RSTREAM";
"istream"            return "ISTREAM";
"irstream"           return "IRSTREAM";
"schema"             return "SCHEMA";
"unidirectional"     return "UNIDIRECTIONAL";
"retain-union"       return "RETAINUNION";
"retain-intersection"return "RETAININTERSECTION";
"pattern"            return "PATTERN";
"sql"                return "SQL";
"metadatasql"        return "METADATASQL";
"prev"               return "PREVIOUS";
"prevtail"           return "PREVIOUSTAIL";
"prevcount"          return "PREVIOUSCOUNT";
"prevwindow"         return "PREVIOUSWINDOW";
"prior"              return "PRIOR";
"exists"             return "EXISTS";
"weekday"            return "WEEKDAY";
"lastweekday"        return "LW";
"instanceof"         return "INSTANCEOF";
"typeof"             return "TYPEOF";
"cast"               return "CAST";
"current_timestamp"  return "CURRENT_TIMESTAMP";
"delete"             return "DELETE";
"snapshot"           return "SNAPSHOT";
"set"                return "SET";
"variable"           return "VARIABLE";
"until"              return "UNTIL";
"at"                 return "AT";
"index"              return "INDEX";
"year"               return "TIMEPERIOD_YEAR";
"years"              return "TIMEPERIOD_YEARS";
"month"              return "TIMEPERIOD_MONTH";
"months"             return "TIMEPERIOD_MONTHS";
"week"               return "TIMEPERIOD_WEEK";
"weeks"              return "TIMEPERIOD_WEEKS";
"day"                return "TIMEPERIOD_DAY";
"days"               return "TIMEPERIOD_DAYS";
"hour"               return "TIMEPERIOD_HOUR";
"hours"              return "TIMEPERIOD_HOURS";
"minute"             return "TIMEPERIOD_MINUTE";
"minutes"            return "TIMEPERIOD_MINUTES";
"sec"                return "TIMEPERIOD_SEC";
"second"             return "TIMEPERIOD_SECOND";
"seconds"            return "TIMEPERIOD_SECONDS"; 
"msec"               return "TIMEPERIOD_MILLISEC";
"millisecond"        return "TIMEPERIOD_MILLISECOND";
"milliseconds"       return "TIMEPERIOD_MILLISECONDS";
"true"               return "BOOLEAN_TRUE";
"false"              return "BOOLEAN_FALSE";
"null"               return "VALUE_NULL";
"limit"              return "ROW_LIMIT_EXPR";
"offset"             return "OFFSET";
"update"             return "UPDATE";
"match_recognize"    return "MATCH_RECOGNIZE";
"measures"           return "MEASURES";
"define"             return "DEFINE";
"partition"          return "PARTITION";
"matches"            return "MATCHES";
"after"              return "AFTER";  
"for"                return "FOR";  
"while"              return "WHILE";  
"using"              return "USING";
"merge"              return "MERGE";
"matched"            return "MATCHED";
"expression"         return "EXPRESSIONDECL";
"new"                return "NEWKW";
"start"              return "START";
"context"            return "CONTEXT";
"initiated"          return "INITIATED";
"terminated"         return "TERMINATED";
"dataflow"           return "DATAFLOW";
"cube"               return "CUBE";
"rollup"             return "ROLLUP";
"grouping"           return "GROUPING";
"grouping_id"        return "GROUPING_ID";
"sets"               return "SETS";

// Operators
"-["                 return "FOLLOWMAX_BEGIN";
"]>"                 return "FOLLOWMAX_END";
"->"                 return "FOLLOWED_BY";
"=>"                 return "GOES";
"="                  return "EQUALS";
"<>"                 return "SQL_NE";
"?"                  return "QUESTION";
"("                  return "LPAREN";
")"                  return "RPAREN";
"["                  return "LBRACK";
"]"                  return "RBRACK";
"{"                  return "LCURLY";
"}"                  return "RCURLY";
":"                  return "COLON";
","                  return "COMMA";
"=="                 return "EQUAL";
"!"                  return "LNOT";
"~"                  return "BNOT";
"!="                 return "NOT_EQUAL";
"/"                  return "DIV";
"/="                 return "DIV_ASSIGN";
"+"                  return "PLUS";
"+="                 return "PLUS_ASSIGN";
"++"                 return "INC";
"-"                  return "MINUS";
"-="                 return "MINUS_ASSIGN";
"--"                 return "DEC";
"*"                  return "STAR";
"*="                 return "STAR_ASSIGN";
"%"                  return "MOD";
"%="                 return "MOD_ASSIGN";
">="                 return "GE";
">"                  return "GT";
"<="                 return "LE";
"<"                  return "LT";
"^"                  return "BXOR";
"^="                 return "BXOR_ASSIGN";
"|"                  return "BOR";
"|="                 return "BOR_ASSIGN";
"||"                 return "LOR";
"&"                  return "BAND";
"&="                 return "BAND_ASSIGN";
"&&"                 return "LAND";
";"                  return "SEMI";
"."                  return "DOT";
"\u18FF"             return "NUM_LONG";  // assign bogus unicode characters so the token exists
"\u18FE"             return "NUM_DOUBLE";
"\u18FD"             return "NUM_FLOAT";
"\\"                 return "ESCAPECHAR";
"\`"                 return "ESCAPEBACKTICK";
"@"                  return "ATCHAR";



// Single-line comments
SL_COMMENT
  : '//'
    (~('\n'|'\r'))* ('\n'|'\r'('\n')?)?
    -> channel(HIDDEN)
  ;

// multiple-line comments
ML_COMMENT
      :     '/*' (.)*? '*/'
    -> channel(HIDDEN)
      ;

TICKED_STRING_LITERAL
    :   '`' ( EscapeSequence | ~('`'|'\\') )* '`'
    ;

QUOTED_STRING_LITERAL
    :   '\'' ( EscapeSequence | ~('\''|'\\') )* '\''
    ;

STRING_LITERAL
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
    ;

fragment
EscapeSequence  : '\\'
    ( 'n'
    | 'r'
    | 't'
    | 'b'
    | 'f'
    | '"'
    | '\''
    | '\\'
    | UnicodeEscape
    | OctalEscape
    | . // unknown, leave as it is
    )
    ;    

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer
IDENT 
  : ('a'..'z'|'_'|'$') ('a'..'z'|'_'|'0'..'9'|'$')*
  ;

IntegerLiteral
    :   DecimalIntegerLiteral
    |   HexIntegerLiteral
    |   OctalIntegerLiteral
    |   BinaryIntegerLiteral
    ;
 
FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;
    
fragment
DecimalIntegerLiteral
    :   DecimalNumeral IntegerTypeSuffix?
    ;

fragment
HexIntegerLiteral
    :   HexNumeral IntegerTypeSuffix?
    ;

fragment
OctalIntegerLiteral
    :   OctalNumeral IntegerTypeSuffix?
    ;

fragment
BinaryIntegerLiteral
    :   BinaryNumeral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   [lL]
    ;

fragment
DecimalNumeral
    :   '0'
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;    
