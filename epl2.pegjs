{
  function makeInteger(o) {
    return parseInt(o.join(""), 10)
  }
  function stringFromArray(arr) {
    return arr.reduce(function(x,y){return x+y;}, "");
  }
  
}

start = startEPLExpressionRule

//----------------------------------------------------------------------------
// Start _ Rules
//----------------------------------------------------------------------------
startPatternExpressionRule = prefix:(annotationEnum / expressionDecl)* _ pattern:patternExpression
  {
    return {"prefix": prefix,"pattern":pattern}
  }
startEPLExpressionRule = prefix:(annotationEnum / expressionDecl)* _ expression:eplExpression
  {
    return {"prefix": prefix ,"expression": expression}
  }
startEventPropertyRule = eventProperty
startJsonValueRule = jsonvalue
//----------------------------------------------------------------------------
// Expression _ Declaration
//----------------------------------------------------------------------------
expressionDecl = EXPRESSIONDECL _ classIdentifier? _ (LBRACK _ RBRACK)? _ expressionDialect? _ keywordNotAllowedIdent _ (LPAREN _ columnList? _ RPAREN)? _ expressionDef
expressionDialect = keywordNotAllowedIdent _ COLON
expressionDef = LCURLY _ expressionLambdaDecl? _ expression _ RCURLY / LBRACK _ stringconstant _ RBRACK          
    
expressionLambdaDecl = (keywordNotAllowedIdent / (LPAREN _ columnList _ RPAREN)) _ (GOES / FOLLOWED_BY)
//----------------------------------------------------------------------------
// Annotations
//----------------------------------------------------------------------------
annotationEnum = ATCHAR _ cls:classIdentifier _ attributes:annotationAttributes? 
  { return {"type": "annotation", "class": cls, "attributes": attributes }}
annotationAttributes = '(' _ val:( elementValuePairsEnum / elementValueEnum )? _ ')'
  {
    return val;
  }
elementValuePairsEnum = first:elementValuePairEnum _ rest:(COMMA _ elementValuePairEnum _)* 
  {
    rest = rest.map(function(element){
      return element[2];
    });
    rest.unshift(first);
    return rest;
  }
elementValuePairEnum = id:keywordNotAllowedIdent _ '=' _ va:elementValueEnum
  {
    return {"name": id, "value": va}
  }
elementValueEnum = annotationEnum
    / elementValueArrayEnum 
    / constant
    / classIdentifier        
elementValueArrayEnum = '{' _ (elementValueEnum _ (','_  elementValueEnum)*)? _ (',')? _ '}'

//----------------------------------------------------------------------------
// EPL _ expression
//----------------------------------------------------------------------------
eplExpression = context:contextExpr? _
    expression:(selectExpr 
    / createWindowExpr
    / createIndexExpr 
    / createVariableExpr
    / createSchemaExpr
    / createContextExpr
    / createExpressionExpr
    / onExpr
    / updateExpr 
    / createDataflow
    / fafDelete
    / fafUpdate
    / fafInsert) _ forexp:forExpr?
    {
      return {"context": context, "expression": expression, "for": forexp};
    }
contextExpr = CONTEXT _ keywordNotAllowedIdent
selectExpr = (INSERT _ insertIntoExpr)? 
    _ SELECT _ sel:selectClause
    _ FROM _ from:fromClause?
    _ matchRecog?
    (_ WHERE _ whereClause)?
    (_ GROUP _ BY _ groupByListExpr)? 
    (_ HAVING _ havingClause)?
    (_ OUTPUT _ outputLimit)?
    (_ ORDER _ BY _ orderByListExpr)?
    (_ ROW_LIMIT_EXPR _ rowLimit)?
    {
      return {"type": "select", "attributes": sel, "from": from}
    }

onExpr = ON _ onStreamExpr _ (onDeleteExpr / onSelectExpr _ (onSelectInsertExpr+ _ outputClauseInsert?)? / onSetExpr / onUpdateExpr / onMergeExpr)
onStreamExpr = (eventFilterExpression / patternInclusionExpression) _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)?
updateExpr = UPDATE _ ISTREAM _ updateDetails
updateDetails = classIdentifier _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ SET _ onSetAssignmentList _(WHERE _ whereClause)?
onMergeExpr = MERGE _ INTO? keywordNotAllowedIdent (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? (WHERE _ whereClause)? mergeItem+
mergeItem = (mergeMatched / mergeUnmatched)
mergeMatched = WHEN _ MATCHED (AND_EXPR _ expression)? mergeMatchedItem+
mergeMatchedItem = THEN (( UPDATE _ SET _ onSetAssignmentList) (WHERE _ whereClause)? / DELETE (WHERE _ whereClause)? / mergeInsert )
mergeUnmatched = WHEN _ NOT_EXPR _ MATCHED (AND_EXPR _ expression)? mergeUnmatchedItem+
mergeUnmatchedItem = THEN _ mergeInsert;    
mergeInsert = INSERT (INTO _ classIdentifier)? (LPAREN _ columnList _ RPAREN)? SELECT _ selectionList (WHERE _ whereClause)?
onSelectExpr  
    = (INSERT _ insertIntoExpr)?    
    SELECT (AND_EXPR? DELETE)? DISTINCT? selectionList
    onExprFrom?
    (WHERE _ whereClause)?    
    (GROUP _ BY _ groupByListExpr)?
    (HAVING _ havingClause)?
    (ORDER _ BY _ orderByListExpr)?
    (ROW_LIMIT_EXPR _ rowLimit)?
onUpdateExpr = UPDATE _ keywordNotAllowedIdent (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? SET _ onSetAssignmentList (WHERE _ whereClause)?
onSelectInsertExpr = INSERT _ insertIntoExpr _ SELECT _ selectionList (WHERE _ whereClause)?
outputClauseInsert = OUTPUT (FIRST / ALL)
onDeleteExpr = DELETE _ onExprFrom (WHERE _ whereClause)?
onSetExpr = SET _ onSetAssignmentList
onSetAssignmentList = onSetAssignment (COMMA _ onSetAssignment)*
onSetAssignment = eventProperty _ EQUALS _ expression / expression
onExprFrom = FROM _ keywordNotAllowedIdent (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)?
createWindowExpr = CREATE _ WINDOW _ keywordNotAllowedIdent (DOT _ viewExpression (DOT _ viewExpression)*)? (RETAINUNION/RETAININTERSECTION)? AS? 
      ( createWindowExprModelAfter / LPAREN _ createColumnList _ RPAREN)   
      (INSERT (WHERE _ expression)? )?
createWindowExprModelAfter = (SELECT _ createSelectionList _ FROM)? classIdentifier
createIndexExpr = CREATE (keywordNotAllowedIdent)? INDEX _ keywordNotAllowedIdent _ ON _ keywordNotAllowedIdent _ LPAREN _ createIndexColumnList _ RPAREN
createIndexColumnList = createIndexColumn (COMMA _ createIndexColumn)*
createIndexColumn = keywordNotAllowedIdent _ keywordNotAllowedIdent? 
createVariableExpr = CREATE _ keywordNotAllowedIdent? VARIABLE _ classIdentifier (LBRACK _ RBRACK)? keywordNotAllowedIdent (EQUALS _ expression)?
createColumnList = createColumnListElement (COMMA _ createColumnListElement)*
createColumnListElement = classIdentifier (classIdentifier (LBRACK _ RBRACK)?) 
createSelectionList = createSelectionListElement (COMMA _ createSelectionListElement)* 
createSelectionListElement = STAR / eventProperty (AS _ keywordNotAllowedIdent)? / constant _ AS _ keywordNotAllowedIdent
createSchemaExpr = CREATE _ keywordNotAllowedIdent? createSchemaDef
createSchemaDef = SCHEMA _ keywordNotAllowedIdent _ AS? ( variantList / LPAREN _ createColumnList? RPAREN ) createSchemaQual*
fafDelete = DELETE _ FROM _ classIdentifier (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? (WHERE _ whereClause)?
fafUpdate = UPDATE _ updateDetails
fafInsert = INSERT _ insertIntoExpr _ VALUES _ LPAREN _ expressionList _ RPAREN
createDataflow = CREATE _ DATAFLOW _ keywordNotAllowedIdent _ AS? gopList
gopList = gop _ gop*
gop = annotationEnum* (keywordNotAllowedIdent / SELECT) gopParams? gopOut? LCURLY _ gopDetail? COMMA? RCURLY / createSchemaExpr _ COMMA; 
gopParams = LPAREN _ gopParamsItemList _ RPAREN
gopParamsItemList = gopParamsItem (COMMA _ gopParamsItem)*
gopParamsItem = (classIdentifier / gopParamsItemMany) gopParamsItemAs?
gopParamsItemMany = LPAREN _ classIdentifier (COMMA _ classIdentifier) RPAREN
gopParamsItemAs = AS _ keywordNotAllowedIdent
gopOut = FOLLOWED_BY _ gopOutItem (COMMA _ gopOutItem)*
gopOutItem = classIdentifier _ gopOutTypeList?
gopOutTypeList = LT _ gopOutTypeParam (COMMA _ gopOutTypeParam)* GT;  
gopOutTypeParam = (gopOutTypeItem / QUESTION)
gopOutTypeItem = classIdentifier _ gopOutTypeList?
gopDetail = gopConfig (COMMA _ gopConfig)*
gopConfig = SELECT (COLON/EQUALS) LPAREN _ selectExpr _ RPAREN / keywordNotAllowedIdent (COLON/EQUALS) (expression / jsonobject / jsonarray)
streamFilterExpression = keywordNotAllowedIdent (DOT _ viewExpression (DOT _ viewExpression)*)?
createContextExpr = CREATE _ CONTEXT _ keywordNotAllowedIdent _ AS? _ createContextDetail
createExpressionExpr = CREATE _ expressionDecl
createContextDetail = createContextChoice / contextContextNested _ COMMA _ contextContextNested (COMMA _ contextContextNested)*
contextContextNested = CONTEXT _ keywordNotAllowedIdent _ AS? createContextChoice
createContextChoice = START _ (ATCHAR _ keywordNotAllowedIdent / createContextRangePoint) _ END _ createContextRangePoint
    / _ INITIATED _ (BY)? createContextDistinct? _ (ATCHAR _ keywordNotAllowedIdent _ AND_EXPR)? createContextRangePoint _ TERMINATED _ (BY)? _ createContextRangePoint
    / _ PARTITION _ (BY)? createContextPartitionItem _ (COMMA  _ createContextPartitionItem)* 
    / _ createContextGroupItem _ (COMMA _ createContextGroupItem)* FROM _ eventFilterExpression
    / _ COALESCE _ (BY)? createContextCoalesceItem (COMMA _ createContextCoalesceItem)* keywordNotAllowedIdent _ number (keywordNotAllowedIdent)?
createContextDistinct = DISTINCT _ LPAREN _ expressionList? RPAREN
createContextRangePoint = createContextFilter 
                / patternInclusionExpression (ATCHAR _ keywordNotAllowedIdent)?
                / crontabLimitParameterSet
                / AFTER _ timePeriod
createContextFilter = eventFilterExpression (AS? keywordNotAllowedIdent)?
createContextPartitionItem = eventProperty ((AND_EXPR/COMMA) eventProperty)* FROM _ eventFilterExpression
createContextCoalesceItem = libFunctionNoClass _ FROM _ eventFilterExpression
createContextGroupItem = GROUP _ BY? expression _ AS _ keywordNotAllowedIdent; 
createSchemaQual = keywordNotAllowedIdent _ columnList
variantList = variantListElement (COMMA _ variantListElement)*
variantListElement = STAR / classIdentifier
insertIntoExpr = (ISTREAM / RSTREAM / IRSTREAM)? INTO _ classIdentifier (LPAREN _ columnList _ RPAREN)?
columnList = keywordNotAllowedIdent (COMMA _ keywordNotAllowedIdent)*
fromClause = stream:streamExpression join:(regularJoin / outerJoinList)
{
  return {"stream": stream, "join": join};
}
regularJoin = (COMMA _ streamExpression)*
outerJoinList = outerJoin (outerJoin)*
outerJoin = (((LEFT/RIGHT/FULL) OUTER)? / (INNER)) JOIN _ streamExpression _ outerJoinIdent?
outerJoinIdent = ON _ outerJoinIdentPair (AND_EXPR _ outerJoinIdentPair)*
outerJoinIdentPair = eventProperty _ EQUALS _ eventProperty 
whereClause = evalOrExpression
selectClause = _ stream:(RSTREAM / ISTREAM / IRSTREAM)? _ d:DISTINCT? _ list:selectionList
  {
    var isDistinct = (d !== null);
    return {"type": "selections" , "isDistinct": isDistinct, "streamType": stream, "selectionList": list}
  }
selectionList = first:selectionListElement _ rest:(COMMA _ selectionListElement)*
  {
    rest = rest.map(function(e){return e[2]});
    rest.unshift(first)
    return rest;
  }
selectionListElement = element:(STAR / streamSelector / selectionListElementExpr)
  {
    return element; 
  }
selectionListElementExpr = exp:expression _ ann:selectionListElementAnno? (AS? IDENT)?
{
  return {"type": "selectionListElementExpr", "expression": exp, "annotation": ann};
}
selectionListElementAnno = ATCHAR _ keywordNotAllowedIdent
streamSelector = ident:keywordNotAllowedIdent (DOT STAR)? _ as:(AS _ keywordNotAllowedIdent)?
{
  return {"type": "streamSelector", "ident": ident}
}
streamExpression = exp:(eventFilterExpression / patternInclusionExpression / databaseJoinExpression / methodJoinExpression ) _
    view:(DOT _ viewExpression (DOT _ viewExpression)*)? _ as:(AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ uni:(UNIDIRECTIONAL)? _ retain:(RETAINUNION/RETAININTERSECTION)?
    {
      var isUnidirectional = (uni !== null),
        shouldRetain = (retain !== null);
      return {
        "expression": exp,
        "view": view,
        "as": as,
        "isUnidirectional": isUnidirectional,
        "shouldRetain": shouldRetain
      };
    }
forExpr = FOR _ keywordNotAllowedIdent (LPAREN _ expressionList? RPAREN)?
patternInclusionExpression = PATTERN _ ann:annotationEnum* _ LBRACK _ expr:patternExpression _ RBRACK {
  return {"annotation": ann, "expression": expr};
}
databaseJoinExpression = SQL _ COLON _ keywordNotAllowedIdent _ LBRACK (STRING_LITERAL / QUOTED_STRING_LITERAL) (METADATASQL (STRING_LITERAL / QUOTED_STRING_LITERAL))? RBRACK; 
methodJoinExpression = keywordNotAllowedIdent _ COLON _ classIdentifier (LPAREN _ expressionList? RPAREN)?
viewExpression = keywordNotAllowedIdent _ COLON (keywordNotAllowedIdent/MERGE) LPAREN _ expressionWithTimeList? RPAREN
groupByListExpr = groupByListChoice (COMMA _ groupByListChoice)*
groupByListChoice = expression / groupByCubeOrRollup / groupByGroupingSets
groupByCubeOrRollup = (CUBE / ROLLUP) LPAREN _ groupByCombinableExpr (COMMA _ groupByCombinableExpr)* RPAREN
groupByGroupingSets = GROUPING _ SETS _ LPAREN _ groupBySetsChoice (COMMA _ groupBySetsChoice)* RPAREN
groupBySetsChoice = groupByCubeOrRollup / groupByCombinableExpr
groupByCombinableExpr = expression / LPAREN (expression (COMMA _ expression)*)? RPAREN
orderByListExpr = orderByListElement (COMMA _ orderByListElement)*
orderByListElement = expression (ASC/DESC)?
havingClause = evalOrExpression
outputLimit = outputLimitAfter? (ALL/FIRST/LAST/SNAPSHOT)? 
  (
    ( EVERY_EXPR ( timePeriod / (number / keywordNotAllowedIdent) (EVENTS)))
    / ( AT _ crontabLimitParameterSet)
    / ( WHEN _ expression (THEN _ onSetExpr)? )
    / ( WHEN _ TERMINATED (AND_EXPR _ expression)? (THEN _ onSetExpr)? )
  ) outputLimitAndTerm?
outputLimitAndTerm = AND_EXPR _ WHEN _ TERMINATED (AND_EXPR _ expression)? (THEN _ onSetExpr)?
outputLimitAfter = AFTER (timePeriod / number _ EVENTS)  
rowLimit = (numberconstant / keywordNotAllowedIdent) ((COMMA / OFFSET) (numberconstant / keywordNotAllowedIdent))?;  
crontabLimitParameterSet = LPAREN _ expressionWithTimeList _ RPAREN;      
whenClause = (WHEN _ expression _ THEN _ expression)
elseClause = (ELSE _ expression);
//----------------------------------------------------------------------------
// Match _ recognize
//----------------------------------------------------------------------------
//
// Lowest _ precedence _ is _ listed _ first, order _ is (highest _ to _ lowest):  
// Single-character-ERE _ duplication * + ? {m,n}
// Concatenation
// Anchoring ^ $
// Alternation  \
//
matchRecog = MATCH_RECOGNIZE _ LPAREN _ matchRecogPartitionBy? matchRecogMeasures _ matchRecogMatchesSelection? matchRecogMatchesAfterSkip? matchRecogPattern 
    matchRecogMatchesInterval? matchRecogDefine _ RPAREN 
matchRecogPartitionBy = PARTITION _ BY _ expression (COMMA _ expression)*    
matchRecogMeasures = MEASURES _ matchRecogMeasureItem (COMMA _ matchRecogMeasureItem)*
matchRecogMeasureItem = expression (AS (keywordNotAllowedIdent)? )?
matchRecogMatchesSelection = ALL _ MATCHES
matchRecogPattern = PATTERN _ LPAREN _ matchRecogPatternAlteration _ RPAREN
matchRecogMatchesAfterSkip = AFTER _ IDENT _ IDENT _ IDENT _ IDENT _ IDENT
matchRecogMatchesInterval = keywordNotAllowedIdent _ timePeriod (OR_EXPR _ TERMINATED)?
matchRecogPatternAlteration = matchRecogPatternConcat (BOR _ matchRecogPatternConcat)* 
matchRecogPatternConcat = matchRecogPatternUnary+ 
matchRecogPatternUnary = matchRecogPatternNested / matchRecogPatternAtom
matchRecogPatternNested = LPAREN _ matchRecogPatternAlteration _ RPAREN (STAR / PLUS / QUESTION)?
matchRecogPatternAtom = keywordNotAllowedIdent ((STAR / PLUS / QUESTION) (QUESTION)? )?
matchRecogDefine = DEFINE _ matchRecogDefineItem (COMMA _ matchRecogDefineItem)* 
matchRecogDefineItem = keywordNotAllowedIdent _ AS _ expression 
//----------------------------------------------------------------------------
// Expression
//----------------------------------------------------------------------------
expression = caseExpression    
caseExpression = (CASE _ whenClause+ elseClause? END)
    / (CASE _ expression _ whenClause+ elseClause? END)
    / evalOrExpression 
evalOrExpression = evalAndExpression (OR_EXPR _ evalAndExpression)*
evalAndExpression = bitWiseExpression (AND_EXPR _ bitWiseExpression)*
bitWiseExpression = negatedExpression ((BAND/BOR/BXOR) negatedExpression)* 
negatedExpression = evalEqualsExpression / NOT_EXPR _ evalEqualsExpression
evalEqualsExpression = evalRelationalExpression ( 
  (EQUALS / IS / IS _ NOT_EXPR/ SQL_NE / NOT_EQUAL) 
  ( evalRelationalExpression / (ANY / SOME / ALL)
  ((LPAREN _ expressionList? RPAREN) / subSelectGroupExpression ) )
  )*
evalRelationalExpression = concatenationExpr ( 
      ( 
        ( 
          (LT/GT/LE/GE) 
            (
              concatenationExpr
              / (ANY / SOME / ALL) ( (LPAREN _ expressionList? RPAREN) / subSelectGroupExpression )
            )
        )*
      )  
      / (NOT_EXPR)? 
      (
        // Represent the optional NOT prefix using the token type by
        // testing 'n' and setting the token type accordingly.
        (IN_SET
            (LPAREN / LBRACK) expression  // brackets _ are _ for _ inclusive/exclusive
            (
              ( COLON (expression) )    // range
              /
              ( (COMMA _ expression)* )   // list of  values
            )
            (RPAREN / RBRACK) 
          )
        / IN_SET _ inSubSelectQuery
        / BETWEEN _ betweenList
        / LIKE _ concatenationExpr (ESCAPE _ stringconstant)?
        / REGEXP _ concatenationExpr
      ) 
    )
inSubSelectQuery = subQueryExpr
concatenationExpr = additiveExpression ( LOR _ additiveExpression ( LOR _ additiveExpression)* )?
additiveExpression = multiplyExpression ( (PLUS/MINUS) multiplyExpression )*
multiplyExpression = unaryExpression ( (STAR/DIV/MOD) unaryExpression )*
unaryExpression = MINUS _ eventProperty
    / constant
    / substitution
    / LPAREN _ expression _ RPAREN _ chainedFunction?
    / eventPropertyOrLibFunction
    / builtinFunc
    / arrayExpression
    / rowSubSelectExpression 
    / existsSubSelectExpression
    / NEWKW _ LCURLY _ newAssign (COMMA _ newAssign)* RCURLY
    
chainedFunction = DOT _ libFunctionNoClass (DOT _ libFunctionNoClass)*
newAssign = eventProperty (EQUALS _ expression)?
rowSubSelectExpression = subQueryExpr _ chainedFunction?
subSelectGroupExpression = subQueryExpr
existsSubSelectExpression = EXISTS _ subQueryExpr
subQueryExpr = LPAREN  SELECT _ DISTINCT? selectionList _ FROM _ subSelectFilterExpr (WHERE _ whereClause)? (GROUP _ BY _ groupByListExpr)? RPAREN
subSelectFilterExpr = eventFilterExpression (DOT _ viewExpression (DOT _ viewExpression)*)? (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? (RETAINUNION/RETAININTERSECTION)?
arrayExpression = LCURLY (expression (COMMA _ expression)* )? RCURLY _ chainedFunction?
builtinFunc = SUM _ LPAREN (ALL / DISTINCT)? expression _ aggregationFilterExpr? RPAREN     
    / AVG _ LPAREN (ALL / DISTINCT)? expression _ aggregationFilterExpr? RPAREN   
    / COUNT _ LPAREN
      (
        ((ALL / DISTINCT)? expression)
      /
        (STAR) 
      )
      aggregationFilterExpr? RPAREN           
    / MEDIAN _ LPAREN (ALL / DISTINCT)? expression _ aggregationFilterExpr? RPAREN  
    / STDDEV _ LPAREN (ALL / DISTINCT)? expression _ aggregationFilterExpr? RPAREN  
    / AVEDEV _ LPAREN (ALL / DISTINCT)? expression _ aggregationFilterExpr? RPAREN  
    / firstLastAggregation                
    / windowAggregation               
    / COALESCE _ LPAREN _ expression _ COMMA _ expression (COMMA _ expression)* RPAREN  
    / PREVIOUS _ LPAREN _ expression (COMMA _ expression)? RPAREN _ chainedFunction?  
    / PREVIOUSTAIL _ LPAREN _ expression (COMMA _ expression)? RPAREN _ chainedFunction?  
    / PREVIOUSCOUNT _ LPAREN _ expression _ RPAREN          
    / PREVIOUSWINDOW _ LPAREN _ expression _ RPAREN _ chainedFunction?      
    / PRIOR _ LPAREN _ number _ COMMA _ eventProperty _ RPAREN        
    / GROUPING _ LPAREN _ expression _ RPAREN           
    / GROUPING_ID _ LPAREN _ expressionList _ RPAREN          
    // MIN _ and _ MAX _ can _ also _ be "Math.min" static _ function _ and "min(price)" aggregation _ function _ and "min(a, b, c...)" built-in _ function
    // therefore _ handled _ in _ code _ via _ libFunction _ as _ below
    / INSTANCEOF _ LPAREN _ expression _ COMMA _ classIdentifier (COMMA _ classIdentifier)* RPAREN  
    / TYPEOF _ LPAREN _ expression _ RPAREN             
    / CAST _ LPAREN _ expression (COMMA / AS) classIdentifier _ RPAREN _ chainedFunction? 
    / EXISTS _ LPAREN _ eventProperty _ RPAREN            
    / CURRENT_TIMESTAMP (LPAREN _ RPAREN)? chainedFunction?       
    / ISTREAM _ LPAREN _ RPAREN               
    
firstLastAggregation = (FIRST / LAST) LPAREN (accessAggExpr (COMMA _ expression)?)? RPAREN _ chainedFunction?
lastAggregation = LPAREN (accessAggExpr (COMMA _ expression)?)? RPAREN _ chainedFunction?
windowAggregation = WINDOW _ LPAREN _ accessAggExpr? RPAREN _ chainedFunction?
accessAggExpr = STAR
    / propertyStreamSelector
    / expression
aggregationFilterExpr = COMMA _ expression
eventPropertyOrLibFunction = eventProperty 
    / libFunction
libFunction = libFunctionWithClass (DOT _ libFunctionNoClass)*
libFunctionWithClass = (classIdentifier _ DOT)? funcIdentTop (LPAREN _ libFunctionArgs? RPAREN)?; 
libFunctionNoClass = funcIdentChained (LPAREN _ libFunctionArgs? RPAREN)?;  
funcIdentTop = escapableIdent
    / MAX 
    / MIN
funcIdentChained = escapableIdent
    / MAX 
    / MIN 
    / WHERE 
    / SET 
    / AFTER 
    / BETWEEN
libFunctionArgs = (ALL / DISTINCT)? libFunctionArgItem (COMMA _ libFunctionArgItem)*
libFunctionArgItem = expressionLambdaDecl? expressionWithTime
betweenList = concatenationExpr _ AND_EXPR _ concatenationExpr;
//----------------------------------------------------------------------------
// Pattern event expressions / event pattern operators
//   Operators are: followed-by (->), or, and, not, every, where
//   Lowest precedence is listed first, order is (lowest to highest):  ->, or, and, not/every, within.
//   On the atomic level an expression has filters, and observer-statements.
//----------------------------------------------------------------------------
patternExpression = followedByExpression
followedByExpression = or:orExpression _ followed:(followedByRepeat)*
{
  return {"or": or, "followed-by": followed};
}
followedByRepeat = first:( FOLLOWED_BY /  ( FOLLOWMAX_BEGIN _ expression _ FOLLOWMAX_END)) last:orExpression 
orExpression = and:andExpression rest:( OR_EXPR _ andExpression)*
{
  rest = rest.map(function(e) {return e[1];});
  return {"and": and, "ors": rest};
}
andExpression = match:matchUntilExpression _ rest:( AND_EXPR _ matchUntilExpression)*
{
  rest = rest.map(function(e) {return e[1];});
  rest.unshift(match)
  return rest;
}
matchUntilExpression = range:matchUntilRange? _ qual:qualifyExpression _ until:(UNTIL _ qualifyExpression)?
{
  until = until === null? null : until[1];
  return {"range": range, "qualify": qual, "until": until };
}
qualifyExpression = first:(( EVERY_EXPR /   NOT_EXPR /   EVERY_DISTINCT_EXPR _ distinctExpressionList) _ matchUntilRange? )? guard:guardPostFix 
{
  var partOne, partTwo, operator;
  if(first !== null){
    partOne = first[0],
    partTwo = first[1],
    operator = partOne[0] || partOne[1] || partOne[2][1];
  } 
  return {"operator": operator, "distinct": partTwo, "guard": guard};

}
guardPostFix = pattern:(atomicExpression / LPAREN _ patternExpression _ RPAREN) _ where:((WHERE _ guardWhereExpression) /  (WHILE _ guardWhileExpression))?
{

  return {"expression": pattern, "where": where};
}
distinctExpressionList = LPAREN _ distinctExpressionAtom _ (COMMA _ distinctExpressionAtom)* _ RPAREN
distinctExpressionAtom = expressionWithTime
atomicExpression = obs:observerExpression /  pattern:patternFilterExpression
{
  if(typeof obs === 'undefined') { var obs = null;}
  return {"type":"atomic", "observer": obs, "patternFilter": pattern };
}
observerExpression = patternKeywordNotAllowedIdent _ COLON _ ( patternKeywordNotAllowedIdent /   AT) _ LPAREN _ expressionWithTimeList? RPAREN
guardWhereExpression = patternKeywordNotAllowedIdent _ COLON _ patternKeywordNotAllowedIdent _ LPAREN (expressionWithTimeList)? RPAREN
guardWhileExpression = LPAREN _ expression _ RPAREN
// syntax _ is [a:b] or [:b] or [a:] or [a]
matchUntilRange = LBRACK _ ( expression ( COLON _ expression?)? /  COLON _ expression) _ RBRACK
//----------------------------------------------------------------------------
// Filter expressions
//   Operators are the usual bunch =, <, >, =<, >= 
//   Ranges such as 'property in [a,b]' are allowed and ([ and )] distinguish open/closed range endpoints
//----------------------------------------------------------------------------
eventFilterExpression = (keywordNotAllowedIdent _ EQUALS)? classIdentifier (LPAREN _ expressionList? RPAREN)? propertyExpression?
propertyExpression = propertyExpressionAtomic (propertyExpressionAtomic)*
propertyExpressionAtomic = LBRACK _ propertyExpressionSelect? expression _ propertyExpressionAnnotation? (AS _ keywordNotAllowedIdent)? (WHERE _ expression)? RBRACK
propertyExpressionSelect = SELECT _ propertySelectionList _ FROM
propertyExpressionAnnotation = ATCHAR _ keywordNotAllowedIdent (LPAREN _ keywordNotAllowedIdent _ RPAREN)
propertySelectionList = propertySelectionListElement (COMMA _ propertySelectionListElement)*
propertySelectionListElement = STAR
  / propertyStreamSelector
  / expression (AS _ IDENT)?
propertyStreamSelector = keywordNotAllowedIdent _ DOT _ STAR (AS _ keywordNotAllowedIdent)?
patternFilterExpression = (keywordNotAllowedIdent _ EQUALS)? classIdentifier (LPAREN _ expressionList? RPAREN)? propertyExpression? patternFilterAnnotation?
patternFilterAnnotation = ATCHAR _ keywordNotAllowedIdent (LPAREN _ number _ RPAREN)?
classIdentifier = first:escapableStr rest:(DOT _ escapableStr)* 
  { 
    var fullClassName = first + stringFromArray(rest);
    return {"type": "classIdentifier", "name": fullClassName};
  }
classIdentifierNonGreedy = escapableStr (DOT _ escapableStr)*
expressionList = expression (COMMA _ expression)*
expressionWithTimeList = expressionWithTimeInclLast (COMMA _ expressionWithTimeInclLast)*
expressionWithTime = lastWeekdayOperand
  / timePeriod
  / expressionQualifyable
  / rangeOperand
  / frequencyOperand
  / lastOperator
  / weekDayOperator
  / numericParameterList
  / numberSetStar
expressionWithTimeInclLast = lastOperand / expressionWithTime
expressionQualifyable = expression (ASC / DESC / TIMEPERIOD_SECONDS / TIMEPERIOD_SECOND / TIMEPERIOD_SEC)?
numberSetStar = STAR
lastWeekdayOperand = LW
lastOperand = LAST
frequencyOperand = STAR _ DIV (number / keywordNotAllowedIdent / substitution)
rangeOperand = (number / keywordNotAllowedIdent / substitution) COLON (number / keywordNotAllowedIdent / substitution)
lastOperator = (number / keywordNotAllowedIdent / substitution) LAST
weekDayOperator = (number / keywordNotAllowedIdent /substitution) WEEKDAY
numericParameterList = LBRACK _ numericListParameter (COMMA _ numericListParameter)* RBRACK
numericListParameter = rangeOperand
  / frequencyOperand
  / numberconstant  
eventProperty = eventPropertyAtomic (DOT _ eventPropertyAtomic)*
 eventPropertyAtomic = eventPropertyIdent ( LBRACK _ number _ RBRACK (QUESTION)? /
      LPAREN (STRING_LITERAL / QUOTED_STRING_LITERAL) RPAREN (QUESTION)? / QUESTION)?
eventPropertyIdent = IDENT (ESCAPECHAR _ DOT _ IDENT?)*
keywordNotAllowedIdent = kw:(!keywords / keywords ) id:IDENT 
  {
    if(kw === undefined){ kw = "";}
    return kw+id;
  }
patternKeywordNotAllowedIdent = kw:(!patternKeywords / patternKeywords) id:IDENT
{
  if(kw === undefined){ kw = "";}
  return kw+id;
}

allKeywordsNotAllowedIntent = kw:(!allKeywords / allKeywords) id: IDENT
{
  if(kw === undefined){ kw = "";}
  return kw+id;
}
allKeywords = patternKeywords / keywords

patternKeywords = 
  AND_EXPR
  / OR_EXPR
  / EVERY_EXPR
  / NOT_EXPR

keywords = 
    TICKED_STRING_LITERAL
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


escapableStr = allKeywordsNotAllowedIntent / EVENTS / TICKED_STRING_LITERAL
escapableIdent = keywordNotAllowedIdent / TICKED_STRING_LITERAL
timePeriod = (  yearPart _ monthPart? weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / monthPart _ weekPart? dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / weekPart _ dayPart? hourPart? minutePart? secondPart? millisecondPart?
    / dayPart _ hourPart? minutePart? secondPart? millisecondPart?
    / hourPart _ minutePart? secondPart? millisecondPart?
    / minutePart _ secondPart? millisecondPart?
    / secondPart _ millisecondPart?
    / millisecondPart
    )
yearPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_YEARS / TIMEPERIOD_YEAR)
monthPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_MONTHS / TIMEPERIOD_MONTH)
weekPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_WEEKS / TIMEPERIOD_WEEK)
dayPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_DAYS / TIMEPERIOD_DAY)
hourPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_HOURS / TIMEPERIOD_HOUR)
minutePart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_MINUTES / TIMEPERIOD_MINUTE / MIN)
secondPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_SECONDS / TIMEPERIOD_SECOND / TIMEPERIOD_SEC) 
millisecondPart = (numberconstant/keywordNotAllowedIdent/substitution) (TIMEPERIOD_MILLISECONDS / TIMEPERIOD_MILLISECOND / TIMEPERIOD_MILLISEC) 
number = IntegerLiteral / FloatingPointLiteral
substitution = QUESTION
constant = numberconstant / stringconstant / BOOLEAN_TRUE / BOOLEAN_FALSE / VALUE_NULL
numberconstant = (MINUS / PLUS)? number
stringconstant = STRING_LITERAL / QUOTED_STRING_LITERAL
// JSON
jsonvalue = constant 
    / jsonobject
    / jsonarray
jsonobject = LCURLY _ jsonmembers _ RCURLY
jsonarray = LBRACK _ jsonelements? RBRACK
jsonelements = jsonvalue (COMMA _ jsonvalue)* (COMMA)?
jsonmembers = jsonpair (COMMA _ jsonpair)* (COMMA)?   
jsonpair = (stringconstant / IDENT) COLON _ jsonvalue
// Tokens
TOKENS = 
CREATE = "create"i
WINDOW = "window"i
IN_SET = "in"i
BETWEEN = "between"i
LIKE = "like"i
REGEXP = "regexp"i
ESCAPE = "escape"i
OR_EXPR = "or"i
AND_EXPR = "and"i
NOT_EXPR = "not"i
EVERY_EXPR = "every"i
EVERY_DISTINCT_EXPR = "every-distinct"i
WHERE = "where"i
AS = "as"i
SUM = "sum"i
AVG = "avg"i
MAX = "max"i
MIN = "min"i
COALESCE = "coalesce"i
MEDIAN = "median"i
STDDEV = "stddev"i
AVEDEV = "avedev"i
COUNT = "count"i
SELECT = "select"i
CASE = "case"i
ELSE = "else"i
WHEN = "when"i
THEN = "then"i
END = "end"i
FROM = "from"i
OUTER = "outer"i
INNER = "inner"i
JOIN = "join"i
LEFT = "left"i
RIGHT = "right"i
FULL = "full"i
ON = "on"i
IS = "is"i
BY = "by"i
GROUP = "group"i
HAVING = "having"i
DISTINCT = "distinct"i
ALL = "all"i
ANY = "any"i
SOME = "some"i
OUTPUT = "output"i
EVENTS = "events"i
FIRST = "first"i
LAST = "last"i
INSERT = "insert"i
INTO = "into"i
VALUES = "values"i
ORDER = "order"i
ASC = "asc"i
DESC = "desc"i
RSTREAM = "rstream"i
ISTREAM = "istream"i
IRSTREAM = "irstream"i
SCHEMA = "schema"i
UNIDIRECTIONAL = "unidirectional"i
RETAINUNION = "retain-union"i
RETAININTERSECTION = "retain-intersection"i
PATTERN = "pattern"i
SQL = "sql"i
METADATASQL = "metadatasql"i
PREVIOUS = "prev"i
PREVIOUSTAIL = "prevtail"i
PREVIOUSCOUNT = "prevcount"i
PREVIOUSWINDOW = "prevwindow"i
PRIOR = "prior"i
EXISTS = "exists"i
WEEKDAY = "weekday"i
LW = "lastweekday"i
INSTANCEOF = "instanceof"i
TYPEOF = "typeof"i
CAST = "cast"i
CURRENT_TIMESTAMP = "current_timestamp"i
DELETE = "delete"i
SNAPSHOT = "snapshot"i
SET = "set"i
VARIABLE = "variable"i
UNTIL = "until"i
AT = "at"i
INDEX = "index"i
TIMEPERIOD_YEAR = "year"i
TIMEPERIOD_YEARS = "years"i
TIMEPERIOD_MONTH = "month"i
TIMEPERIOD_MONTHS = "months"i
TIMEPERIOD_WEEK = "week"i
TIMEPERIOD_WEEKS = "weeks"i
TIMEPERIOD_DAY = "day"i
TIMEPERIOD_DAYS = "days"i
TIMEPERIOD_HOUR = "hour"i
TIMEPERIOD_HOURS = "hours"i
TIMEPERIOD_MINUTE = "minute"i
TIMEPERIOD_MINUTES = "minutes"i
TIMEPERIOD_SEC = "sec"i
TIMEPERIOD_SECOND = "second"i
TIMEPERIOD_SECONDS = "seconds"i
TIMEPERIOD_MILLISEC = "msec"i
TIMEPERIOD_MILLISECOND = "millisecond"i
TIMEPERIOD_MILLISECONDS = "milliseconds"i
BOOLEAN_TRUE = "true"i
BOOLEAN_FALSE = "false"i
VALUE_NULL = "null"i
ROW_LIMIT_EXPR = "limit"i
OFFSET = "offset"i
UPDATE = "update"i
MATCH_RECOGNIZE = "match_recognize"i
MEASURES = "measures"i
DEFINE = "define"i
PARTITION = "partition"i
MATCHES = "matches"i
AFTER = "after"i
FOR = "for"i
WHILE = "while"i
USING = "using"i
MERGE = "merge"i
MATCHED = "matched"i
EXPRESSIONDECL = "expression"i
NEWKW = "new"i
START = "start"i
CONTEXT = "context"i
INITIATED = "initiated"i
TERMINATED = "terminated"i
DATAFLOW = "dataflow"i
CUBE = "cube"i
ROLLUP = "rollup"i
GROUPING = "grouping"i
GROUPING_ID = "grouping_id"i
SETS = "sets"i
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
NUM_LONG =  '\u18FF'  // assign _ bogus _ unicode _ characters _ so _ the _ token _ exists
NUM_DOUBLE =  '\u18FE'
NUM_FLOAT =  '\u18FD'
ESCAPECHAR =  '\\'
ESCAPEBACKTICK =  '\`'
ATCHAR =  '@'
_ = [ \t\r\n\f]*
WS = _
SL_COMMENT = '//' [^\n\r]*
ML_COMMENT = '/*' [^(*/)]* '*/'
TICKED_STRING_LITERAL = '`' ( EscapeSequence / [^`\\] )* '`' 
QUOTED_STRING_LITERAL = '\'' ( EscapeSequence / [^'\\] )* '\''
STRING_LITERAL = '"' str:( EscapeSequence / [^\\""] )* '"'
  {
    return stringFromArray(str); 
  }
EscapeSequence = '\\'( 'n' / 'r' / 't' / 'b'/ 'f' / '"' / '\'' / '\\' / UnicodeEscape / OctalEscape / .)
IDENT = first:( [a-zA-Z] / '_' / '$') rest:( [a-zA-Z] / '_' / [0-9]  / '$')* 
  { return first + stringFromArray(rest);}
IntegerLiteral = DecimalIntegerLiteral 
    / HexIntegerLiteral 
    / OctalIntegerLiteral 
    / BinaryIntegerLiteral
FloatingPointLiteral = DecimalFloatingPointLiteral / HexadecimalFloatingPointLiteral
OctalEscape = '\\' ([0-3] [0-7] [0-7] / [0-7] [0-7] / [0-7])
UnicodeEscape = '\\' 'u' HexDigit _ HexDigit _ HexDigit _ HexDigit              
DecimalIntegerLiteral = DecimalNumeral _ IntegerTypeSuffix?
HexIntegerLiteral = HexNumeral _ IntegerTypeSuffix?
OctalIntegerLiteral = OctalNumeral _ IntegerTypeSuffix?
BinaryIntegerLiteral = BinaryNumeral _ IntegerTypeSuffix?
IntegerTypeSuffix = [lL]
DecimalNumeral = '0' / NonZeroDigit (Digits? / Underscores _ Digits)
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
    /   '.' Digits _ ExponentPart? FloatTypeSuffix?
    /   Digits _ ExponentPart _ FloatTypeSuffix?
    /   Digits _ FloatTypeSuffix
ExponentPart =  ExponentIndicator _ SignedInteger
SignedInteger = sig:Sign? digit:[0-9]+ {return sign + makeInteger(digits)}
ExponentIndicator = [eE]
Sign = [+-]
FloatTypeSuffix = [fFdD]
HexadecimalFloatingPointLiteral = HexSignificand _ BinaryExponent _ FloatTypeSuffix?
HexSignificand = HexNumeral '.'? / '0' HexDigits? '.' HexDigits
BinaryExponent = BinaryExponentIndicator _ SignedInteger
BinaryExponentIndicator = [pP]