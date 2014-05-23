{
  function makeInteger(o) {
    return parseInt(o.join(""), 10)
  }
  function stringFromArray(arr) {
    return arr.reduce(function(x,y){return x+y;}, "");
  }
  function isEmptyArray(arr){
    return arr instanceof Array && arr.length === 0;
  }
  function flattenArray(arr){
    if(!(arr instanceof Array)) return arr;
    arr = filterArray(arr);
    arr = arr.map(flattenArray);
    if(arr.length === 1) arr = arr[0];
    return arr;
  }
  function isInterestingValue(val){
    return val !== null && val !== "" && val !== " " && !isEmptyArray(val);
  }

  function filterArray(arr){
    return arr.filter(isInterestingValue)
  }

}

start = startEPLExpressionRule / startPatternExpressionRule

//----------------------------------------------------------------------------
// Start _ Rules
//----------------------------------------------------------------------------
startPatternExpressionRule = prefix:(annotationEnum / expressionDecl)* _ pattern:patternExpression
  {
    return {"prefix": prefix,"pattern":pattern}
  }
startEPLExpressionRule = prefix:(annotationEnum / expressionDecl)* _ expression:eplExpression
  {
    return {"prefix": prefix ,"body": expression}
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
    from:(_ FROM _ fromClause)?
    _ match:matchRecog?
    where:(_ WHERE _ whereClause)?
    group:(_ GROUP _ BY _ groupByListExpr)? 
    having:(_ HAVING _ havingClause)?
    output:(_ OUTPUT _ outputLimit)?
    orderBy:(_ ORDER _ BY _ orderByListExpr)?
    rowLimit:(_ ROW_LIMIT_EXPR _ rowLimit)?
    {
      from = (from instanceof Array)? from[3] : from;
      return {
        "type": "select", 
        "attributes": sel, 
        "from": from,
        "match": match,
        "where": flattenArray(where),
        "group": flattenArray(group),
        "having": flattenArray(having),
        "output": flattenArray(output),
        "orderBy": flattenArray(orderBy),
        "rowLimit": flattenArray(rowLimit)
      }
    }

onExpr = ON _ onStreamExpr _ (onDeleteExpr / onSelectExpr _ (onSelectInsertExpr+ _ outputClauseInsert?)? / onSetExpr / onUpdateExpr / onMergeExpr)
onStreamExpr = (eventFilterExpression / patternInclusionExpression) _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)?
updateExpr = UPDATE _ ISTREAM _ updateDetails
updateDetails = classIdentifier _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ SET _ onSetAssignmentList _(WHERE _ whereClause)?
onMergeExpr = MERGE _ INTO? keywordNotAllowedIdent (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? (WHERE _ whereClause)? mergeItem+
mergeItem = (mergeMatched / mergeUnmatched)
mergeMatched = WHEN _ MATCHED _ (AND_EXPR _ expression)? _ mergeMatchedItem+
mergeMatchedItem = THEN _ (( UPDATE _ SET _ onSetAssignmentList)_ (WHERE _ whereClause)? / DELETE (WHERE _ whereClause)? / mergeInsert )
mergeUnmatched = WHEN _ NOT_EXPR _ MATCHED _ (AND_EXPR _ expression)? _ mergeUnmatchedItem+
mergeUnmatchedItem = THEN _ mergeInsert;    
mergeInsert = INSERT _ (INTO _ classIdentifier)? _ (LPAREN _ columnList _ RPAREN)? _ SELECT _ selectionList _ (WHERE _ whereClause)?
onSelectExpr  
    = (INSERT _ insertIntoExpr)?    
    _ SELECT _ (AND_EXPR? _ DELETE)? _ DISTINCT? _ selectionList
    _ onExprFrom?
    _ (WHERE _ whereClause)?    
    _ (GROUP _ BY _ groupByListExpr)?
    _ (HAVING _ havingClause)?
    _ (ORDER _ BY _ orderByListExpr)?
    _ (ROW_LIMIT_EXPR _ rowLimit)?
onUpdateExpr = UPDATE _ keywordNotAllowedIdent _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ SET _ onSetAssignmentList _ (WHERE _ whereClause)?
onSelectInsertExpr = INSERT _ insertIntoExpr _ SELECT _ selectionList _ (WHERE _ whereClause)?
outputClauseInsert = OUTPUT _ (FIRST / ALL)
onDeleteExpr = DELETE _ onExprFrom _ (WHERE _ whereClause)?
onSetExpr = SET _ onSetAssignmentList
onSetAssignmentList = onSetAssignment _ (COMMA _ onSetAssignment)*
onSetAssignment = eventProperty _ EQUALS _ expression / expression
onExprFrom = FROM _ keywordNotAllowedIdent _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)?
createWindowExpr = CREATE _ WINDOW _ keywordNotAllowedIdent _ (DOT _ viewExpression (DOT _ viewExpression)*)? _ (RETAINUNION/RETAININTERSECTION)? _ AS? 
      ( createWindowExprModelAfter / LPAREN _ createColumnList _ RPAREN)   
      (INSERT (WHERE _ expression)? )?
createWindowExprModelAfter = (SELECT _ createSelectionList _ FROM)? _ classIdentifier
createIndexExpr = CREATE _ (keywordNotAllowedIdent)? _ INDEX _ keywordNotAllowedIdent _ ON _ keywordNotAllowedIdent _ LPAREN _ createIndexColumnList _ RPAREN
createIndexColumnList = createIndexColumn _ (COMMA _ createIndexColumn)*
createIndexColumn = keywordNotAllowedIdent _ keywordNotAllowedIdent? 
createVariableExpr = CREATE _ keywordNotAllowedIdent? _ VARIABLE _ classIdentifier _ (LBRACK _ RBRACK)? _ keywordNotAllowedIdent _ (EQUALS _ expression)?
createColumnList = createColumnListElement _ (COMMA _ createColumnListElement)*
createColumnListElement = classIdentifier _ (classIdentifier _ (LBRACK _ RBRACK)?) 
createSelectionList = createSelectionListElement _ (COMMA _ createSelectionListElement)* 
createSelectionListElement = STAR / eventProperty _ (AS _ keywordNotAllowedIdent)? / constant _ AS _ keywordNotAllowedIdent
createSchemaExpr = CREATE _ keywordNotAllowedIdent? _ createSchemaDef
createSchemaDef = SCHEMA _ keywordNotAllowedIdent _ AS? _ ( variantList / LPAREN _ createColumnList? RPAREN ) _ createSchemaQual*
fafDelete = DELETE _ FROM _ classIdentifier _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ (WHERE _ whereClause)?
fafUpdate = UPDATE _ updateDetails
fafInsert = INSERT _ insertIntoExpr _ VALUES _ LPAREN _ expressionList _ RPAREN
createDataflow = CREATE _ DATAFLOW _ keywordNotAllowedIdent _ AS? gopList
gopList = gop _ gop*
gop = annotationEnum* _ (keywordNotAllowedIdent / SELECT) _ gopParams? gopOut? LCURLY _ gopDetail? _ COMMA? _ RCURLY / createSchemaExpr _ COMMA; 
gopParams = LPAREN _ gopParamsItemList _ RPAREN
gopParamsItemList = gopParamsItem _ (COMMA _ gopParamsItem)*
gopParamsItem = (classIdentifier / gopParamsItemMany) _ gopParamsItemAs?
gopParamsItemMany = LPAREN _ classIdentifier _ (COMMA _ classIdentifier) _ RPAREN
gopParamsItemAs = AS _ keywordNotAllowedIdent
gopOut = FOLLOWED_BY _ gopOutItem _ (COMMA _ gopOutItem)*
gopOutItem = classIdentifier _ gopOutTypeList?
gopOutTypeList = LT _ gopOutTypeParam _ (COMMA _ gopOutTypeParam)* _ GT;  
gopOutTypeParam = (gopOutTypeItem / QUESTION)
gopOutTypeItem = classIdentifier _ gopOutTypeList?
gopDetail = gopConfig _ (COMMA _ gopConfig)*
gopConfig = SELECT _ (COLON/EQUALS) _ LPAREN _ selectExpr _ RPAREN / keywordNotAllowedIdent _ (COLON/EQUALS) _ (expression / jsonobject / jsonarray)
streamFilterExpression = keywordNotAllowedIdent _ (DOT _ viewExpression _ (DOT _ viewExpression)*)?
createContextExpr = CREATE _ CONTEXT _ keywordNotAllowedIdent _ AS? _ createContextDetail
createExpressionExpr = CREATE _ expressionDecl
createContextDetail = createContextChoice / contextContextNested _ COMMA _ contextContextNested _ (COMMA _ contextContextNested)*
contextContextNested = CONTEXT _ keywordNotAllowedIdent _ AS? _ createContextChoice
createContextChoice = START _ (ATCHAR _ keywordNotAllowedIdent / createContextRangePoint) _ END _ createContextRangePoint
    / _ INITIATED _ (BY)? _ createContextDistinct? _ (ATCHAR _ keywordNotAllowedIdent _ AND_EXPR)? _ createContextRangePoint _ TERMINATED _ (BY)? _ createContextRangePoint
    / _ PARTITION _ (BY)? _ createContextPartitionItem _ (COMMA  _ createContextPartitionItem)* 
    / _ createContextGroupItem _ (COMMA _ createContextGroupItem)* _ FROM _ eventFilterExpression
    / _ COALESCE _ (BY)? _ createContextCoalesceItem _ (COMMA _ createContextCoalesceItem)* _ keywordNotAllowedIdent _ number _ (keywordNotAllowedIdent)?
createContextDistinct = DISTINCT _ LPAREN _ expressionList? _ RPAREN
createContextRangePoint = createContextFilter 
                / patternInclusionExpression _ (ATCHAR _ keywordNotAllowedIdent)?
                / crontabLimitParameterSet
                / AFTER _ timePeriod
createContextFilter = eventFilterExpression _ (AS? keywordNotAllowedIdent)?
createContextPartitionItem = eventProperty _ ((AND_EXPR/COMMA) _ eventProperty)* _ FROM _ eventFilterExpression
createContextCoalesceItem = libFunctionNoClass _ FROM _ eventFilterExpression
createContextGroupItem = GROUP _ BY? _ expression _ AS _ keywordNotAllowedIdent; 
createSchemaQual = keywordNotAllowedIdent _ columnList
variantList = variantListElement _ (COMMA _ variantListElement)*
variantListElement = STAR / classIdentifier
insertIntoExpr = (ISTREAM / RSTREAM / IRSTREAM)? _ INTO _ classIdentifier _ (LPAREN _ columnList _ RPAREN)?
columnList = keywordNotAllowedIdent _ (COMMA _ keywordNotAllowedIdent)*
fromClause = stream:streamExpression _ join:(regularJoin / outerJoinList)
{
  stream.join = join;
  return stream;
}
regularJoin = (COMMA _ streamExpression)*
outerJoinList = outerJoin _ (outerJoin)*
outerJoin = (((LEFT/RIGHT/FULL) _ OUTER)? / (INNER)) _ JOIN _ streamExpression _ outerJoinIdent?
outerJoinIdent = ON _ outerJoinIdentPair _ (AND_EXPR _ outerJoinIdentPair)*
outerJoinIdentPair = eventProperty _ EQUALS _ eventProperty 
whereClause = evalOrExpression
selectClause = _ stream:(RSTREAM / ISTREAM / IRSTREAM)? _ d:DISTINCT? _ list:selectionList
{
  var isDistinct = (d !== null);
  return {"type": "selections" , "isDistinct": isDistinct, "streamType": stream, "selectionList": list}
}
selectionList = first:selectionListElement rest:(_ COMMA _ selectionListElement)*
{
  rest = rest.map(function(e){return e[2]});
  rest.unshift(first)
  return rest;
}
selectionListElement = element:(STAR / streamSelector / selectionListElementExpr)
{
  return element; 
}
selectionListElementExpr = exp:expression _ ann:selectionListElementAnno? _ (AS? _ allKeywordsNotAllowedIntent)?
{
  return {"type": "selectionListElement", "element": flattenArray(exp), "annotation": ann};
}
selectionListElementAnno = ATCHAR _ keywordNotAllowedIdent
streamSelector = ident:allKeywordsNotAllowedIntent (DOT STAR)? _ as:(AS _ allKeywordsNotAllowedIntent)?
{
  var asident = as === null ? null : as[1]
  return {"type": "streamSelector", "identifier": ident, "as": asident};
}
streamExpression = exp:(eventFilterExpression / patternInclusionExpression / databaseJoinExpression / methodJoinExpression ) _
  view:(DOT _ viewExpression _ (DOT _ viewExpression)*)? _ as:(AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ uni:(UNIDIRECTIONAL)? _ retain:(RETAINUNION/RETAININTERSECTION)?
{
  var isUnidirectional = (uni !== null),
    shouldRetain = (retain !== null);
  if(view !== null){
    var firstView = view[2];
    var restViewsArray = view[4];
    restViewsArray.unshift(firstView)
    view = restViewsArray;
  }
  return {
    "stream": exp,
    "view": view,
    "as": as,
    "isUnidirectional": isUnidirectional,
    "shouldRetain": shouldRetain
  };
}
forExpr = FOR _ keywordNotAllowedIdent _ (LPAREN _ expressionList? _ RPAREN)?
patternInclusionExpression = PATTERN _ ann:annotationEnum* _ LBRACK _ expr:patternExpression _ RBRACK {
  return {"annotation": ann, "pattern": expr};
}
databaseJoinExpression = SQL _ COLON _ keywordNotAllowedIdent _ LBRACK (STRING_LITERAL / QUOTED_STRING_LITERAL) _ (METADATASQL _ (STRING_LITERAL / QUOTED_STRING_LITERAL))? _ RBRACK; 
methodJoinExpression = keywordNotAllowedIdent _ COLON _ classIdentifier _ (LPAREN _ expressionList? RPAREN)?
viewExpression = nameSpace:keywordNotAllowedIdent COLON name:(IDENT/MERGE) _ LPAREN _ expr:expressionWithTimeList? _ RPAREN
{
  return {
    "namespace": nameSpace,
    "name": name,
    "parameter": flattenArray(expr)
  };
}
groupByListExpr = groupByListChoice _ (COMMA _ groupByListChoice)*
groupByListChoice = expression / groupByCubeOrRollup / groupByGroupingSets
groupByCubeOrRollup = (CUBE / ROLLUP) _ LPAREN _ groupByCombinableExpr _ (COMMA _ groupByCombinableExpr)* _ RPAREN
groupByGroupingSets = GROUPING _ SETS _ LPAREN _ groupBySetsChoice _ (COMMA _ groupBySetsChoice)* _ RPAREN
groupBySetsChoice = groupByCubeOrRollup / groupByCombinableExpr
groupByCombinableExpr = expression / LPAREN _ (expression _ (COMMA _ expression)*)? _ RPAREN
orderByListExpr = orderByListElement (COMMA _ orderByListElement)*
orderByListElement = expression _ (ASC/DESC)?
havingClause = evalOrExpression
outputLimit = outputLimitAfter? _ (ALL/FIRST/LAST/SNAPSHOT)? 
  (
    ( _ EVERY_EXPR _ ( timePeriod / (number / keywordNotAllowedIdent) _ (EVENTS)))
    / ( _ AT _ crontabLimitParameterSet)
    / ( _ WHEN _ expression (_ THEN _ onSetExpr)? )
    / ( _ WHEN _ TERMINATED (_ AND_EXPR _ expression)? _ ( THEN _ onSetExpr)? )
  ) outputLimitAndTerm?
outputLimitAndTerm = AND_EXPR _ WHEN _ TERMINATED _ (AND_EXPR _ expression)? _ (THEN _ onSetExpr)?
outputLimitAfter = AFTER _ (timePeriod / number _ EVENTS)  
rowLimit = (numberconstant / keywordNotAllowedIdent) _ ((COMMA / OFFSET) _ (numberconstant / keywordNotAllowedIdent))?;  
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
matchRecog = MATCH_RECOGNIZE _ LPAREN _ matchRecogPartitionBy? _ matchRecogMeasures _ matchRecogMatchesSelection? _ matchRecogMatchesAfterSkip? _ matchRecogPattern 
    matchRecogMatchesInterval? matchRecogDefine _ RPAREN 
matchRecogPartitionBy = PARTITION _ BY _ expression _ (COMMA _ expression)*    
matchRecogMeasures = MEASURES _ matchRecogMeasureItem _ (COMMA _ matchRecogMeasureItem)*
matchRecogMeasureItem = expression _ (AS (keywordNotAllowedIdent)? )?
matchRecogMatchesSelection = ALL _ MATCHES
matchRecogPattern = PATTERN _ LPAREN _ matchRecogPatternAlteration _ RPAREN
matchRecogMatchesAfterSkip = AFTER _ keywordNotAllowedIdent _ keywordNotAllowedIdent _ keywordNotAllowedIdent _ keywordNotAllowedIdent _ keywordNotAllowedIdent
matchRecogMatchesInterval = keywordNotAllowedIdent _ timePeriod (OR_EXPR _ TERMINATED)?
matchRecogPatternAlteration = matchRecogPatternConcat _ (BOR _ matchRecogPatternConcat)* 
matchRecogPatternConcat = matchRecogPatternUnary+ 
matchRecogPatternUnary = matchRecogPatternNested / matchRecogPatternAtom
matchRecogPatternNested = LPAREN _ matchRecogPatternAlteration _ RPAREN (STAR / PLUS / QUESTION)?
matchRecogPatternAtom = keywordNotAllowedIdent _ ((STAR / PLUS / QUESTION) _ (QUESTION)? )?
matchRecogDefine = DEFINE _ matchRecogDefineItem _ (COMMA _ matchRecogDefineItem)* 
matchRecogDefineItem = keywordNotAllowedIdent _ AS _ expression 
//----------------------------------------------------------------------------
// Expression
//----------------------------------------------------------------------------
expression = ce:caseExpression {
  return flattenArray(ce);
}
caseExpression = (CASE _ whenClause+ _ elseClause? _ END)
  / (CASE _ expression _ whenClause+ _ elseClause? _ END)
  / evalOrExpression

evalOrExpression = ev:evalAndExpression _ orexp:(OR_EXPR _ evalAndExpression)*
{
  return (typeof orexp === 'undefined' || orexp === null || isEmptyArray(orexp)) ? ev : {"andExpression": ev, "or": orexp};
}
evalAndExpression = bw:bitWiseExpression  andexp:(_ AND_EXPR _ bitWiseExpression)*
{
  return (typeof andexp === 'undefined' || andexp === null || isEmptyArray(andexp))? bw : {"bitWiseExpression": bw, "and": andexp};
}
bitWiseExpression = first:negatedExpression rest:(_ (BAND/BOR/BXOR) _ negatedExpression)*
{
  rest.unshift(first);
  return rest;
} 
negatedExpression = evalEqualsExpression / NOT_EXPR _ evalEqualsExpression
evalEqualsExpression = evalRelationalExpression  
  ( _ 
    (EQUALS / IS / IS _ NOT_EXPR/ SQL_NE / NOT_EQUAL) _ 
    ( evalRelationalExpression / (ANY / SOME / ALL) _
    ((LPAREN _ expressionList? _ RPAREN) / subSelectGroupExpression ) )
  )*

evalRelationalExpression = concatenationExpr _  
  ( 
    (LT/GT/LE/GE) _
      (
        concatenationExpr
        / (ANY / SOME / ALL) _ ( (LPAREN _ expressionList? _ RPAREN) / subSelectGroupExpression )
      )
  )* 
  / (_ NOT_EXPR _ )? 
  (
    // Represent the optional NOT prefix using the token type by
    // testing 'n' and setting the token type accordingly.
    (_ IN_SET _
        (LPAREN / LBRACK) _ expression _ // brackets are for inclusive/exclusive
        (
          ( COLON _ expression )    // range
          /
          ( COMMA _ expression )*   // list of  values
        )
        (RPAREN / RBRACK) 
    )
    / _ IN_SET _ inSubSelectQuery
    / _ BETWEEN _ betweenList
    / _ LIKE _ concatenationExpr _ (ESCAPE _ stringconstant)?
    / _ REGEXP _ concatenationExpr
  )


inSubSelectQuery = subQueryExpr
concatenationExpr = additiveExpression _ ( LOR _ additiveExpression (_ LOR _ additiveExpression)* )?
additiveExpression = multiplyExpression ( _ (PLUS/MINUS) _ multiplyExpression )*
multiplyExpression = first:unaryExpression _ rest:( _ (STAR/DIV/MOD) _ unaryExpression )*
{
  if(typeof rest === 'undefined' || rest === null || isEmptyArray(rest)){
    return first;
  } 
  else {
    return {"leftOperand":first, "operator":rest[0], "rightOperand":rest[1]};
  }
}
unaryExpression = 
    (MINUS _ eventProperty)
  / constant
  / substitution
  / (LPAREN _ expression _ RPAREN _ chainedFunction?)
  / eventPropertyOrLibFunction
  / builtinFunc
  / arrayExpression
  / rowSubSelectExpression 
  / existsSubSelectExpression
  / (NEWKW _ LCURLY _ newAssign  (_ COMMA _ newAssign)* _ RCURLY)

chainedFunction = DOT _ first:libFunctionNoClass _ rest:(DOT _ libFunctionNoClass)*
{
  rest = rest.map(function(f) {return f[2];});
  rest.unshift(first);
  return rest;
}
newAssign = eventProperty _ (EQUALS _ expression)?
rowSubSelectExpression = subQueryExpr _ chainedFunction?
subSelectGroupExpression = subQueryExpr
existsSubSelectExpression = EXISTS _ subQueryExpr
subQueryExpr = LPAREN _ SELECT _ DISTINCT? _ selectionList _ FROM _ subSelectFilterExpr _ (WHERE _ whereClause)? _ (GROUP _ BY _ groupByListExpr)? _ RPAREN
subSelectFilterExpr = eventFilterExpression _ (DOT _ viewExpression _ (DOT _ viewExpression)*)? _ (AS _ keywordNotAllowedIdent / keywordNotAllowedIdent)? _ (RETAINUNION/RETAININTERSECTION)?
arrayExpression = LCURLY _ (expression _ (COMMA _ expression)* )? _ RCURLY _ chainedFunction?
builtinFunc = 
    sumFunction    
  / avgFunction
  / countFunction
  / medianFunction 
  / stddevFunction
  / avedevFunction
  / firstLastAggregation                
  / windowAggregation               
  / coalesceFunction
  / previousFunction
  / previousTailFunction
  / previousCountFunction          
  / previousWindowFunction     
  / priorFunction
  / groupingFunction
  / groupingIDFunction
  // MIN and MAX can also be "Math.min" static function and "min(price)" aggregation function and "min(a, b, c...)" built-in function
  // therefore handled in code via libFunction as below
  / instanceOfFunction
  / typeofFunction
  / castFunction
  / existsFunction
  / timestampFunction
  / istreamFunction
  

avgFunction = AVG _ LPAREN _ distinct:(ALL / DISTINCT)? _ params:expression _ aggregator:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "avg",
    "distinct":distinct,
    "parameter": params,
    "aggregator": aggregator
  }; 
}
sumFunction = SUM _ LPAREN _ distinct:(ALL / DISTINCT)? _ params:expression _ aggregator:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "sum",
    "distinct":distinct,
    "parameter": params,
    "aggregator": aggregator
  };
}
countFunction = COUNT _ LPAREN _ ((distinct:(ALL / DISTINCT)? _ params:expression)/(STAR)) agg:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "count",
    "distinct":distinct,
    "parameter": params,
    "aggregator": agg
  };
}
medianFunction = MEDIAN _ LPAREN _ distinct:(ALL / DISTINCT)? _ params:expression _ agg:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "median",
    "distinct":distinct,
    "parameter": params,
    "aggregator": agg
  };
}
stddevFunction = STDDEV _ LPAREN _ distinct:(ALL / DISTINCT)? _ params:expression _ agg:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "stddev",
    "distinct":distinct,
    "parameter": params,
    "aggregator": agg
  };
}
avedevFunction = AVEDEV _ LPAREN _ distinct:(ALL / DISTINCT)? _ params:expression _ agg:aggregationFilterExpr? _ RPAREN
{
  return {
    "type": "function", 
    "name": "avedev",
    "distinct":distinct,
    "parameter": params,
    "aggregator": agg
  };
}
coalesceFunction = COALESCE _ LPAREN _ first:expression _ COMMA _ second:expression _ rest:(COMMA _ expression)* _ RPAREN
{
  var arr = [first, second];
  if(rest !== null){
    rest = rest.map(function(e) {return e[2];});
  }
  return {
    "type": "function", 
    "name": "coalesce",
    "parameters": flattenArray(arr.concat(rest))
  };  
}
previousFunction = PREVIOUS _ LPAREN _ exp:expression _ property:(COMMA _ expression)? _ RPAREN _ chained:chainedFunction?
{
  return {
    "type": "function", 
    "name": "prev",
    "parameter": exp,
    "property": property[2],
    "chainedFunction": chained
  };  
}
previousTailFunction = PREVIOUSTAIL _ LPAREN _ exp:expression _ property:(COMMA _ expression)? _ RPAREN _ chained:chainedFunction?
{
  return {
    "type": "function", 
    "name": "prevtail",
    "parameter": exp,
    "property": property[2],
    "chainedFunction": chained
  };  
}
previousCountFunction = PREVIOUSCOUNT _ LPAREN _ exp:expression _ RPAREN
{
  return {
    "type": "function", 
    "name": "prevcount",
    "parameter": exp,
  };   
} 
previousWindowFunction = PREVIOUSWINDOW _ LPAREN _ exp:expression _ RPAREN _ chained:chainedFunction?
{
  return {
    "type": "function", 
    "name": "prevwindow",
    "parameter": exp,
    "chainedFunction": chained
  };  
}
priorFunction = PRIOR _ LPAREN _ num:number _ COMMA _ prop:eventProperty _ RPAREN
{
  return {
    "type": "function", 
    "name": "prior",
    "number": num,
    "eventProperty": prop 
  };
}
groupingFunction = GROUPING _ LPAREN _ exp:expression _ RPAREN
{
  return {
    "type": "function", 
    "name": "grouping",
    "parameter": exp
  };
}
groupingIDFunction = GROUPING_ID _ LPAREN _ expr:expressionList _ RPAREN
{
  return {
    "type": "function", 
    "name": "grouping_id",
    "parameters": exp
  };
}
instanceOfFunction = INSTANCEOF _ LPAREN _ expr:expression _ COMMA _ type:classIdentifier _ moretypes:(COMMA _ classIdentifier)* _ RPAREN
{
  moretypes = moretypes.map(function(e) {return e[2];})
  moretypes.unshift(type);
  return {
    "type": "function", 
    "name": "instanceof",
    "expression": expr,
    "types": moretypes
  };
}
typeofFunction = TYPEOF _ LPAREN _ exp:expression _ RPAREN
{
  return {
    "type": "function", 
    "name": "typeof",
    "expression": exp
  }; 
}
castFunction = CAST _ LPAREN _ exp:expression _ (COMMA / AS) _ type:classIdentifier _ RPAREN _ chained:chainedFunction?
{
  return {
    "type": "function", 
    "name": "cast",
    "expression": exp,
    "type":type,
    "chainedFunction": chained
  };   
}
existsFunction = EXISTS _ LPAREN _ property:eventProperty _ RPAREN
{
  return {
    "type": "function", 
    "name": "exists",
    "property": property
  };   
}
timestampFunction = CURRENT_TIMESTAMP _ (LPAREN _ RPAREN)? _ chained:chainedFunction?
{
  return {
    "type": "function", 
    "name": "current_timestamp",
    "chainedFunction": chained
  };     
}
istreamFunction = ISTREAM _ LPAREN _ RPAREN
{
  return {
    "type": "function", 
    "name": "istream"
  }; 
}
firstLastAggregation = (FIRST / LAST) _ LPAREN _  (accessAggExpr _ (COMMA _ expression)?)? _ RPAREN _ chainedFunction?
lastAggregation = LPAREN _ (accessAggExpr _ (COMMA _ expression)?)? _ RPAREN _ chainedFunction?
windowAggregation = WINDOW _ LPAREN _ accessAggExpr? _ RPAREN _ chainedFunction?
accessAggExpr = STAR
    / propertyStreamSelector
    / expression
aggregationFilterExpr = COMMA _ exp:expression { return exp; }
eventPropertyOrLibFunction = eventProperty 
    / libFunction
libFunction = libFunctionWithClass (DOT libFunctionNoClass)*
libFunctionWithClass = (classIdentifier DOT)? _ funcIdentTop _ (LPAREN _ libFunctionArgs? _ RPAREN)?; 
libFunctionNoClass = funcIdentChained _ (LPAREN _ libFunctionArgs? RPAREN)?;  
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
libFunctionArgs = (ALL / DISTINCT)? _ libFunctionArgItem _ (COMMA _ libFunctionArgItem)*
libFunctionArgItem = expressionLambdaDecl? _ expressionWithTime
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
followedByRepeat = first:( FOLLOWED_BY /  ( FOLLOWMAX_BEGIN _ expression _ FOLLOWMAX_END)) _ last:orExpression 
orExpression = and:andExpression rest:( OR_EXPR _ andExpression)*
{
  return {"and": and, "ors": rest};
}
andExpression = match:matchUntilExpression _ rest:( AND_EXPR _ matchUntilExpression)*

matchUntilExpression = range:matchUntilRange? _ qual:qualifyExpression _ until:(UNTIL _ qualifyExpression)?
{
  until = until === null? null : until[1];
  return {"range": range, "qualify": qual, "until": until };
}
qualifyExpression = first:(( EVERY_EXPR /   NOT_EXPR /   EVERY_DISTINCT_EXPR _ distinctExpressionList) _ matchUntilRange? )? _ guard:guardPostFix 
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
distinctExpressionList = LPAREN _ distinctExpressionAtom (_ COMMA _ distinctExpressionAtom)* _ RPAREN
distinctExpressionAtom = expressionWithTime
atomicExpression = obs:observerExpression /  pattern:patternFilterExpression
{
  if(typeof obs === 'undefined') { var obs = null;}
  return {"type":"atomic", "observer": obs, "patternFilter": pattern };
}
observerExpression = patternKeywordNotAllowedIdent _ COLON _ ( patternKeywordNotAllowedIdent /   AT) _ LPAREN _ expressionWithTimeList? _ RPAREN
guardWhereExpression = patternKeywordNotAllowedIdent _ COLON _ patternKeywordNotAllowedIdent _ LPAREN _ (expressionWithTimeList)? _ RPAREN
guardWhileExpression = LPAREN _ expression _ RPAREN
// syntax _ is [a:b] or [:b] or [a:] or [a]
matchUntilRange = LBRACK _ ( expression _ ( COLON _ expression?)? /  COLON _ expression) _ RBRACK
//----------------------------------------------------------------------------
// Filter expressions
//   Operators are the usual bunch =, <, >, =<, >= 
//   Ranges such as 'property in [a,b]' are allowed and ([ and )] distinguish open/closed range endpoints
//----------------------------------------------------------------------------
eventFilterExpression = name:(keywordNotAllowedIdent _ EQUALS)? _ className:classIdentifier _ filter:(LPAREN _ expressionList? _ RPAREN)? _ property:propertyExpression?
{
  if(filter !== null) filter = filter[2];
  return {
    "name": name,
    "class": className,
    "filter": filter,
    "property": property
  };
}
propertyExpression = propertyExpressionAtomic _ (propertyExpressionAtomic)*
propertyExpressionAtomic = LBRACK _ propertyExpressionSelect? _ expression _ propertyExpressionAnnotation? _ (AS _ keywordNotAllowedIdent)? _ (WHERE _ expression)? _ RBRACK
propertyExpressionSelect = SELECT _ propertySelectionList _ FROM
propertyExpressionAnnotation = ATCHAR _ keywordNotAllowedIdent _ (LPAREN _ keywordNotAllowedIdent _ RPAREN)
propertySelectionList = propertySelectionListElement (_ COMMA _ propertySelectionListElement)*
propertySelectionListElement = STAR
  / propertyStreamSelector
  / expression (_ AS _ keywordNotAllowedIdent)?
propertyStreamSelector = keywordNotAllowedIdent _ DOT _ STAR (_ AS _ keywordNotAllowedIdent)?
patternFilterExpression = (keywordNotAllowedIdent _ EQUALS)? _ classIdentifier _ (LPAREN _ expressionList? RPAREN)? _ propertyExpression? _ patternFilterAnnotation?
patternFilterAnnotation = ATCHAR _ keywordNotAllowedIdent _ (LPAREN _ number _ RPAREN)?
classIdentifier = first:escapableStr rest:(DOT escapableStr)* 
{ 
  var fullClassName = first + stringFromArray(flattenArray(rest));
  return {"type": "classIdentifier", "name": fullClassName};
}
classIdentifierNonGreedy = escapableStr _ (DOT _ escapableStr)*
expressionList = first:expression  rest:(_ COMMA _ expression)*
{
  rest.unshift(first);
  return rest;
}
expressionWithTimeList = expressionWithTimeInclLast (_ COMMA _ expressionWithTimeInclLast)*
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
expressionQualifyable = expression _ (ASC / DESC / TIMEPERIOD_SECONDS / TIMEPERIOD_SECOND / TIMEPERIOD_SEC)?
numberSetStar = STAR
lastWeekdayOperand = LW
lastOperand = LAST
frequencyOperand = STAR _ DIV _ (number / keywordNotAllowedIdent / substitution)
rangeOperand = (number / keywordNotAllowedIdent / substitution) _ COLON _ (number / keywordNotAllowedIdent / substitution)
lastOperator = (number / keywordNotAllowedIdent / substitution) _ LAST
weekDayOperator = (number / keywordNotAllowedIdent /substitution) _ WEEKDAY
numericParameterList = LBRACK _ numericListParameter _ (COMMA _ numericListParameter)* _ RBRACK
numericListParameter = rangeOperand
  / frequencyOperand
  / numberconstant  
eventProperty = first:eventPropertyAtomic rest:(_ DOT _ eventPropertyAtomic)*
{
  rest.unshift(first);
  return flattenArray(rest);
}

eventPropertyAtomic = eventPropertyIdent _ ( LBRACK _ number _ RBRACK _ (QUESTION)? /
      LPAREN _ (STRING_LITERAL / QUOTED_STRING_LITERAL) _ RPAREN _ (QUESTION)? / QUESTION)?
eventPropertyIdent = allKeywordsNotAllowedIntent _ (ESCAPECHAR _ DOT _ allKeywordsNotAllowedIntent?)*
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
allKeywords = patternKeywords / keywords / additionalKeywords

additionalKeywords = FROM

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
    / PRIOR

escapableStr = allKeywordsNotAllowedIntent / EVENTS / TICKED_STRING_LITERAL
escapableIdent = keywordNotAllowedIdent / TICKED_STRING_LITERAL
timePeriod = (  yearPart _ monthPart? _ weekPart? _ dayPart? _ hourPart? _ minutePart? _ secondPart? _ millisecondPart?
    / monthPart _ weekPart? _ dayPart? _ hourPart? _ minutePart? _ secondPart? _ millisecondPart?
    / weekPart _ dayPart? _ hourPart? _ minutePart? _ secondPart? _ millisecondPart?
    / dayPart _ hourPart? _ minutePart? _ secondPart? _ millisecondPart?
    / hourPart _ minutePart? _ secondPart? _ millisecondPart?
    / minutePart _ secondPart? _ millisecondPart?
    / secondPart _ millisecondPart?
    / millisecondPart
    )
yearPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_YEARS / TIMEPERIOD_YEAR)
monthPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_MONTHS / TIMEPERIOD_MONTH)
weekPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_WEEKS / TIMEPERIOD_WEEK)
dayPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_DAYS / TIMEPERIOD_DAY)
hourPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_HOURS / TIMEPERIOD_HOUR)
minutePart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_MINUTES / TIMEPERIOD_MINUTE / MIN)
secondPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_SECONDS / TIMEPERIOD_SECOND / TIMEPERIOD_SEC) 
millisecondPart = (numberconstant/keywordNotAllowedIdent/substitution) _ (TIMEPERIOD_MILLISECONDS / TIMEPERIOD_MILLISECOND / TIMEPERIOD_MILLISEC) 
number = num:(IntegerLiteral / FloatingPointLiteral) { return flattenArray(num); }
substitution = QUESTION
constant = numberconstant / stringconstant / BOOLEAN_TRUE / BOOLEAN_FALSE / VALUE_NULL
numberconstant = (MINUS / PLUS)? _ number
stringconstant = STRING_LITERAL / QUOTED_STRING_LITERAL
// JSON
jsonvalue = constant 
    / jsonobject
    / jsonarray
jsonobject = LCURLY _ jsonmembers _ RCURLY
jsonarray = LBRACK _ jsonelements? RBRACK
jsonelements = jsonvalue (_ COMMA _ jsonvalue)* _ (COMMA)?
jsonmembers = jsonpair (_ COMMA _ jsonpair)* _ (COMMA)?   
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
COLON =  ':'
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
_ = [ \t\r\n\f]* {return null;}
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
{ 
  return first + stringFromArray(rest);
}
IntegerLiteral = DecimalIntegerLiteral / HexIntegerLiteral / OctalIntegerLiteral / BinaryIntegerLiteral
FloatingPointLiteral = DecimalFloatingPointLiteral / HexadecimalFloatingPointLiteral
OctalEscape = '\\' ([0-3] [0-7] [0-7] / [0-7] [0-7] / [0-7])
UnicodeEscape = '\\' 'u' HexDigit _ HexDigit _ HexDigit _ HexDigit              
DecimalIntegerLiteral = DecimalNumeral _ IntegerTypeSuffix?
HexIntegerLiteral = HexNumeral _ IntegerTypeSuffix?
OctalIntegerLiteral = OctalNumeral _ IntegerTypeSuffix?
BinaryIntegerLiteral = BinaryNumeral _ IntegerTypeSuffix?
IntegerTypeSuffix = [lL]
DecimalNumeral = zero:'0' / nzero:NonZeroDigit digits:(Digits? / Underscores _ Digits)
{
  var arr = [];
  if(typeof zero === 'undefined') var zero = null;
  if(zero !== null)  arr.push(zero);
  if(nzero !== null) arr.push(nzero);
  if(digits!== null) arr.push(digits);
  return makeInteger(arr);
}
Digits = first:Digit rest:(DigitOrUnderscore* Digit)?
{
  if(rest === null) rest = [];
  rest.unshift(first);
  return makeInteger(rest);
}
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
SignedInteger = sig:Sign? digit:[0-9]+ 
{
  return sign + makeInteger(digits)
}
ExponentIndicator = [eE]
Sign = [+-]
FloatTypeSuffix = [fFdD]
HexadecimalFloatingPointLiteral = HexSignificand _ BinaryExponent _ FloatTypeSuffix?
HexSignificand = HexNumeral '.'? / '0' HexDigits? '.' HexDigits
BinaryExponent = BinaryExponentIndicator _ SignedInteger
BinaryExponentIndicator = [pP]