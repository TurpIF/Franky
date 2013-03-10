unit UParser;

{$mode objfpc}

interface

uses UToken, USyntaxTree, UParseResult, UEnvironment, Error;

//Main function
function parse(const code : CTokenList) : CSyntaxTree;

//Product token and throw all exceptions
function parseToken(const rule : CToken; const ls : CTokenList; const n : Integer) : CParseResult;

{**************Terminal rules**************}
//Constante
function parseTkNumber	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkFloat	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkBool	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkString	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkId		(const ls : CTokenList; const n : Integer) : CParseResult;

//Delimitor
function parseLBrace		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseRBrace		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLSqBracket	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseRSqBracket	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLCurBracket	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseRCurBracket	(const ls : CTokenList; const n : Integer) : CParseResult;

//Aritmetic Operators
function parseOpAdd	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpSub	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpMul	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpDiv	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpMod	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpPow	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpInc	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpDec	(const ls : CTokenList; const n : Integer) : CParseResult;

//BoolOperators
function parseOpBoolAnd	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBoolOr	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBoolNot	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBoolIs	(const ls : CTokenList; const n : Integer) : CParseResult;

//Cmp Operators
function parseOpEq	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpNe	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpLt	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpGt	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpLe	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpGe	(const ls : CTokenList; const n : Integer) : CParseResult;

//Bit Operators
function parseOpBitAnd		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBitOr		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBitNo		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBitXor		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBitRShift	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpBitLShift	(const ls : CTokenList; const n : Integer) : CParseResult;

//Special Operators
function parseOpIn	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpGet	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpTo	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOpLet	(const ls : CTokenList; const n : Integer) : CParseResult;

//Keywords
function parseTkIf			(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkElse		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkFor			(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkWhile		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkBreak		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkContinue	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkReturn		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkFunction	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkClass		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkPrint		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkInput		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkPass		(const ls : CTokenList; const n : Integer) : CParseResult;

function parseTkEndl		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkSeparator	(const ls : CTokenList; const n : Integer) : CParseResult;

function parseTkCastStr		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkCastInt		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkCastFloat	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkCastBool	(const ls : CTokenList; const n : Integer) : CParseResult;

function parseTkFunctLength	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkFunctAt		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTkFunctTime	(const ls : CTokenList; const n : Integer) : CParseResult;

{**************Grammar rules**************}
//Operation
function parseOperande	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseGet		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseUnaire	(const ls : CTokenList; const n : Integer) : CParseResult;
function parsePow		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseMul		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseAdd		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseBool		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseCmp		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseTo		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseOperation	(const ls : CTokenList; const n : Integer) : CParseResult;

//Condition/Loop
function parseCondition	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLoop		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseWhile		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseFor		(const ls : CTokenList; const n : Integer) : CParseResult;
function parsePrint		(const ls : CTokenList; const n : Integer) : CParseResult;

//Declare var/function/class
function parseArgs			(const ls : CTokenList; const n : Integer) : CParseResult;
function parseArgument		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseReturn		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLetVar		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLetFunction	(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLetClass		(const ls : CTokenList; const n : Integer) : CParseResult;

//Lines/Blocks/Program
function parseLine		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseLines		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseBlock		(const ls : CTokenList; const n : Integer) : CParseResult;
function parseProgram	(const ls : CTokenList; const n : Integer) : CParseResult;

implementation

//Main function
function parse(const code : CTokenList) : CSyntaxTree;
var re : CParseResult;
begin
	try
		re := parseProgram(code, 0);
		parse := re.tree;
		
		if re.size <> code.length then
			raise nothingExpectedFound(code.at(re.size).toString());
	except
		raise;
	end;
end;

//Product token and throw all exceptions
function parseToken(const rule : CToken; const ls : CTokenList; const n : Integer) : CParseResult; begin
	if n >= ls.length then
		raise expectedToken(rule.toString());
	
	if rule.equals(ls.at(n)) then begin
		parseToken := CParseResult.Create(CSyntaxTree.Create(ls.at(n)), 1); end
	else
		raise expectedTokenFound(rule.toString(), ls.at(n).toString());
end;

{**************Terminal rules**************}
//Constante
function parseTkNumber	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkNumber	:= parseToken(CToken.TkNumber, ls, n); except raise end; end;
function parseTkFloat	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkFloat	:= parseToken(CToken.TkFloat, ls, n); except raise end; end;
function parseTkBool	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkBool	:= parseToken(CToken.TkBool, ls, n); except raise end; end;
function parseTkString	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkString	:= parseToken(CToken.TkStringChar, ls, n); except raise end; end;

function parseTkId(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseTkId := parseToken(CToken.TkId, ls, n);
		parseTkId := CParseResult.Create(CVariable.Create(parseTkId.tree), parseTkId.size);
	except raise end;
end;

function parseTkTrue(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseTkTrue := parseToken(CToken.TkTrue, ls, n);
		parseTkTrue := CParseResult.Create(CSyntaxTree.Create(CToken.getBoolean(true)), parseTkTrue.size);
	except raise end;
end;

function parseTkFalse(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseTkFalse := parseToken(CToken.TkFalse, ls, n);
		parseTkFalse := CParseResult.Create(CSyntaxTree.Create(CToken.getBoolean(false)), parseTkFalse.size);
	except raise end;
end;

//Delimitor
function parseLBrace		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseLBrace		:= parseToken(CToken.TkLBrace, ls, n); except raise end; end;
function parseRBrace		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseRBrace		:= parseToken(CToken.TkRBrace, ls, n); except raise end; end;
function parseLSqBracket	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseLSqBracket	:= parseToken(CToken.TkLSqBracket, ls, n); except raise end; end;
function parseRSqBracket	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseRSqBracket	:= parseToken(CToken.TkRSqBracket, ls, n); except raise end; end;
function parseLCurBracket	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseLCurBracket	:= parseToken(CToken.TkLCurBracket, ls, n); except raise end; end;
function parseRCurBracket	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseRCurBracket	:= parseToken(CToken.TkRCurBracket, ls, n); except raise end; end;

//Aritmetic Operators
function parseOpAdd	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpAdd	:= parseToken(CToken.TkOpAdd, ls, n); except raise end; end;
function parseOpSub	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpSub	:= parseToken(CToken.TkOpSub, ls, n); except raise end; end;
function parseOpMul	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpMul	:= parseToken(CToken.TkOpMul, ls, n); except raise end; end;
function parseOpDiv	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpDiv	:= parseToken(CToken.TkOpDiv, ls, n); except raise end; end;
function parseOpMod	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpMod := parseToken(CToken.TkOpMod, ls, n); except raise end; end;
function parseOpPow	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpPow	:= parseToken(CToken.TkOpPow, ls, n); except raise end; end;
function parseOpInc	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpInc	:= parseToken(CToken.TkOpInc, ls, n); except raise end; end;
function parseOpDec	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpDec	:= parseToken(CToken.TkOpDec, ls, n); except raise end; end;

//Bool Operators
function parseOpBoolAnd	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBoolAnd	:= parseToken(CToken.TkOpBoolAnd, ls, n); except raise end; end;
function parseOpBoolOr	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBoolOr	:= parseToken(CToken.TkOpBoolOr, ls, n); except raise end; end;
function parseOpBoolNot	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBoolNot	:= parseToken(CToken.TkOpBoolNot, ls, n); except raise end; end;
function parseOpBoolIs	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBoolIs	:= parseToken(CToken.TkOpBoolIs, ls, n); except raise end; end;

//Cmp Operators
function parseOpEq	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpEq	:= parseToken(CToken.TkOpEq, ls, n); except raise end; end;
function parseOpNe	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpNe	:= parseToken(CToken.TkOpNe, ls, n); except raise end; end;
function parseOpLt	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpLt	:= parseToken(CToken.TkOpLt, ls, n); except raise end; end;
function parseOpGt	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpGt	:= parseToken(CToken.TkOpGt, ls, n); except raise end; end;
function parseOpLe	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpLe	:= parseToken(CToken.TkOpLe, ls, n); except raise end; end;
function parseOpGe	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpGe	:= parseToken(CToken.TkOpGe, ls, n); except raise end; end;

//Bit Operators
function parseOpBitAnd		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitAnd		:= parseToken(CToken.TkOpBitAnd, ls, n); except raise end; end;
function parseOpBitOr		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitOr		:= parseToken(CToken.TkOpBitOr, ls, n); except raise end; end;
function parseOpBitNo		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitNo		:= parseToken(CToken.TkOpBitNo, ls, n); except raise end; end;
function parseOpBitXor		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitXor		:= parseToken(CToken.TkOpBitXor, ls, n); except raise end; end;
function parseOpBitRShift	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitRShift	:= parseToken(CToken.TkOpBitRShift, ls, n); except raise end; end;
function parseOpBitLShift	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpBitLShift	:= parseToken(CToken.TkOpBitLShift, ls, n); except raise end; end;

//Special Operators
function parseOpIn	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpIn	:= parseToken(CToken.TkOpIn, ls, n); except raise end; end;
function parseOpGet	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpGet	:= parseToken(CToken.TkOpGet, ls, n); except raise end; end;
function parseOpTo	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpTo	:= parseToken(CToken.TkOpTo, ls, n); except raise end; end;
function parseOpLet	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseOpLet	:= parseToken(CToken.TkOpLet, ls, n); except raise end; end;

//Keywords
function parseTkIf			(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkIf			:= parseToken(CToken.TkIf, ls, n); except raise end; end;
function parseTkElse		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkElse		:= parseToken(CToken.TkElse, ls, n); except raise end; end;
function parseTkFor			(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkFor			:= parseToken(CToken.TkFor, ls, n); except raise end; end;
function parseTkWhile		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkWhile		:= parseToken(CToken.TkWhile, ls, n); except raise end; end;
function parseTkBreak		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkBreak		:= parseToken(CToken.TkBreak, ls, n); except raise end; end;
function parseTkContinue	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkContinue	:= parseToken(CToken.TkContinue, ls, n); except raise end; end;
function parseTkReturn		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkReturn		:= parseToken(CToken.TkReturn, ls, n); except raise end; end;
function parseTkFunction	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkFunction	:= parseToken(CToken.TkFunction, ls, n); except raise end; end;
function parseTkClass		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkClass		:= parseToken(CToken.TkClass, ls, n); except raise end; end;
function parseTkPrint		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkPrint		:= parseToken(CToken.TkPrint, ls, n); except raise end; end;
function parseTkPass		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkPass		:= parseToken(CToken.TkPass, ls, n); except raise end; end;

function parseTkInput		(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseTkInput := parseToken(CToken.TkInput, ls, n);
		parseTkInput := CParseResult.Create(CInput.Create(), parseTkInput.size);
	except
		raise
	end;
end;

function parseTkEndl		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkEndl		:= parseToken(CToken.TkEndl, ls, n); except raise end; end;
function parseTkSeparator	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkSeparator	:= parseToken(CToken.TkSeparator, ls, n); except raise end; end;

function parseTkCastStr		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkCastStr		:= parseToken(CToken.TkCastStr, ls, n); except raise end; end;
function parseTkCastInt		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkCastInt		:= parseToken(CToken.TkCastInt, ls, n); except raise end; end;
function parseTkCastFloat	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkCastFloat	:= parseToken(CToken.TkCastFloat, ls, n); except raise end; end;
function parseTkCastBool	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkCastBool	:= parseToken(CToken.TkCastBool, ls, n); except raise end; end;

function parseTkFunctLength	(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkFunctLength	:= parseToken(CToken.TkFunctLength, ls, n); except raise end; end;
function parseTkFunctAt		(const ls : CTokenList; const n : Integer) : CParseResult; begin try parseTkFunctAt		:= parseToken(CToken.TkFunctAt, ls, n); except raise end; end;

function parseTkFunctTime	(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseTkFunctTime := parseToken(CToken.TkFunctTime, ls, n);
		parseTkFunctTime := CParseResult.Create(CTime.Create(), parseTkFunctTime.size);
	except
		raise
	end;
end;


{**************Grammar rules**************}
//Operation
function parseOperande(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseTkId(ls, n);
		r2 := parseArgument(ls, n + r1.size);
		parseOperande := CParseResult.Create(CCallFunction.Create(r1.tree, r2.tree), r1.size + r2.size);
	except
	try parseOperande	:= parseTkNumber(ls, n);	except
	try parseOperande	:= parseTkFloat(ls, n);		except
	try parseOperande	:= parseTkBool(ls, n);		except
	try parseOperande	:= parseTkString(ls, n);	except
	try parseOperande	:= parseTkId(ls, n);		except
	try parseOperande	:= parseLetFunction(ls, n);	except
	try parseOperande	:= parseLetClass(ls, n);	except
	try parseOperande	:= parseTkTrue(ls, n);		except
	try parseOperande	:= parseTkFalse(ls, n);		except
	try parseOperande	:= parseTkInput(ls, n);		except
	try parseOperande	:= parseTkFunctTime(ls, n);	except
	try
		r1 := parseLBrace(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		r3 := parseRBrace(ls, n + r1.size + r2.size);
		
		parseOperande := CParseResult.Create(r2.tree, r1.size + r2.size + r3.size);
	except
		raise;
	end; end; end; end; end; end; end; end; end; end; end; end; end;
end;


function parseGet(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseOperande(ls, n);
		
		try
			r2 := parseOpGet(ls, n + r1.size);
			r3 := parseGet(ls, n + r1.size + r2.size);
			
			parseGet := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parseGet := r1;
		end;
	except
		raise;
	end;
end;

function parseUnaire(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2 : CParseResult;
begin
	try
		try r1 := parseOpSub(ls, n);			except
		try r1 := parseOpInc(ls, n);			except
		try r1 := parseOpDec(ls, n);			except
		try r1 := parseOpBoolNot(ls, n);		except
		try r1 := parseOpBitNo(ls, n);			except
		try r1 := parseTkCastStr(ls, n);		except
		try r1 := parseTkCastInt(ls, n);		except
		try r1 := parseTkCastFloat(ls, n);		except
		try r1 := parseTkCastBool(ls, n);		except
		try r1 := parseTkFunctLength(ls, n);	except
		raise; end; end; end; end; end; end; end; end; end; end;
		
		r2 := parseGet(ls, n + r1.size);
		
		parseUnaire := CParseResult.Create(CCallFunction.Create(r1.tree, COpArgument.Create(r2.tree, NIL)), r1.size + r2.size);
	except
		try parseUnaire := parseGet(ls, n);	except
		raise; end;
	end;
end;

function parsePow(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseUnaire(ls, n);
		
		try
			try r2 := parseOpPow(ls, n + r1.size);			except
			try r2 := parseOpBitXor(ls, n + r1.size);		except
			try r2 := parseOpBitRShift(ls, n + r1.size);	except
			try r2 := parseOpBitLShift(ls, n + r1.size);	except
			try r2 := parseTkFunctAt(ls, n + r1.size);		except
			raise; end; end; end; end; end;
			r3 := parseMul(ls, n + r1.size + r2.size);
			
			parsePow := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parsePow := r1;
		end;
	except
		raise;
	end;
end;

function parseMul(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parsePow(ls, n);
		
		try
			try r2 := parseOpMul(ls, n + r1.size);		except
			try r2 := parseOpDiv(ls, n + r1.size);		except
			try r2 := parseOpMod(ls, n + r1.size);		except
			try r2 := parseOpBitAnd(ls, n + r1.size);	except
			raise; end; end; end; end;
			
			r3 := parseMul(ls, n + r1.size + r2.size);
			
			parseMul := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parseMul := r1;
		end;
	except
		raise;
	end;
end;

function parseAdd(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseMul(ls, n);
		
		try
			try r2 := parseOpAdd(ls, n + r1.size);		except
			try r2 := parseOpSub(ls, n + r1.size);		except
			try r2 := parseOpBitOr(ls, n + r1.size);	except
			raise; end; end; end;
			
			r3 := parseAdd(ls, n + r1.size + r2.size);
			
			parseAdd := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parseAdd := r1;
		end;
	except
		raise;
	end;
end;

function parseCmp(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseAdd(ls, n);
		
		try
			try r2 := parseOpEq(ls, n + r1.size);	except
			try r2 := parseOpNe(ls, n + r1.size);	except
			try r2 := parseOpLt(ls, n + r1.size);	except
			try r2 := parseOpGt(ls, n + r1.size);	except
			try r2 := parseOpLe(ls, n + r1.size);	except
			try r2 := parseOpGe(ls, n + r1.size);	except
			raise; end; end; end; end; end; end;
			
			r3 := parseCmp(ls, n + r1.size + r2.size);
			
			parseCmp := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parseCmp := r1;
		end;
	except
		raise;
	end;
end;

function parseBool(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseCmp(ls, n);
		
		try
			try r2 := parseOpBoolAnd(ls, n + r1.size);	except
			try r2 := parseOpBoolOr(ls, n + r1.size);	except
			try r2 := parseOpBoolIs(ls, n + r1.size);	except
			try r2 := parseOpIn(ls, n + r1.size);		except
			raise; end; end; end; end;
			
			r3 := parseBool(ls, n + r1.size + r2.size);
			
			parseBool := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r3.tree)), r1.size + r2.size + r3.size);
		except
			parseBool := r1;
		end;
	except
		raise;
	end;
end;

function parseTo(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseBool(ls, n);
		
		try
			r2 := parseOpTo(ls, n + r1.size);
			r3 := parseTo(ls, n + r1.size + r2.size);
			
			parseTo := CParseResult.Create(CCallFunction.Create(r2.tree, COpArgument.Create(r1.tree, r2.tree)), r1.size + r2.size + r3.size);
		except
			parseTo := r1;
		end;
	except
		raise;
	end;
end;

function parseOperation(const ls : CTokenList; const n : Integer) : CParseResult;
begin
	try parseOperation := parseTo(ls, n);	except
	raise; end;
end;

//Condition/Loop
function parseCondition(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3, r4, r5 : CParseResult;
begin
	parseCondition := NIL;
	try
		r1 := parseTkIf(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		r3 := parseBlock(ls, n + r1.size + r2.size);
		
		try
			r4 := parseTkElse(ls, n + r1.size + r2.size + r3.size);
			r5 := parseBlock(ls, n + r1.size + r2.size + r3.size + r4.size);
			
			parseCondition := CParseResult.Create(CCondition.Create(r2.tree, r3.tree, r5.tree), r1.size + r2.size + r3.size + r4.size + r5.size);
		except
		try	parseCondition := CParseResult.Create(CCondition.Create(r2.tree, r3.tree), r1.size + r2.size + r3.size);	except
		raise; end; end;
	except
		raise;
	end;
end;

function parseLoop(const ls : CTokenList; const n : Integer) : CParseResult;
begin
	try parseLoop := parseWhile(ls, n);	except
	//try parseLoop := parseFor(ls, n);	except
	raise; end; //end;
end;

function parseWhile(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseTkWhile(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		r3 := parseBlock(ls, n + r1.size + r2.size);
		
		parseWhile := CParseResult.Create(CWhile.Create(r2.tree, r3.tree), r1.size + r2.size + r3.size);
	except
		raise;
	end;
end;

function parseFor(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3, r4, r5 : CParseResult;
begin
	try
		r1 := parseTkFor(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		r3 := parseOpIn(ls, n + r2.size);
		r4 := parseOperation(ls, n + r1.size + r2.size + r3.size);
		r5 := parseBlock(ls, n + r1.size + r2.size + r3.size + r4.size);
		
		parseFor := CParseResult.Create(CFor.Create(r2.tree, r4.tree, r5.tree), r1.size + r2.size + r3.size + r4.size + r5.size);
	except
		raise;
	end;
end;

function parsePrint(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2 : CParseResult;
begin
	try
		r1 := parseTkPrint(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		
		parsePrint := CParseResult.Create(CPrint.Create(r2.tree), r1.size + r2.size);
	except
		raise;
	end;
end;

//Declare var/function/class
function parseArgs(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 :  CParseResult;
begin
	try
		r1 := parseOperation(ls, n);
		
		try
			r2 := parseTkSeparator(ls, n + r1.size);
			r3 := parseArgs(ls, n + r1.size + r2.size);
			parseArgs := CParseResult.Create(CArgument.Create(r1.tree, r3.tree), r1.size + r2.size + r3.size);
		except
			parseArgs := CParseResult.Create(CArgument.Create(r1.tree, NIL), r1.size);
		end;
	except
		raise;
	end;
end;

function parseArgument(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseLBrace(ls, n);
		r2 := parseArgs(ls, n + r1.size);
		r3 := parseRBrace(ls, n + r1.size + r2.size);
		
		parseArgument := CParseResult.Create(r2.tree, r1.size + r2.size + r3.size);
	except
	try
		r1 := parseLBrace(ls, n);
		r2 := parseRBrace(ls, n + r1.size);
		
		parseArgument := CParseResult.Create(r1.tree, r1.size + r2.size);
	except
		raise;
	end; end;
end;

function parseReturn(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2 : CParseResult;
begin
	try
		r1 := parseTkReturn(ls, n);
		r2 := parseOperation(ls, n + r1.size);
		
		parseReturn := CParseResult.Create(CReturn.Create(r2.tree), r1.size + r2.size);
	except
		raise;
	end;
end;

function parseLetVar(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseOperation(ls, n);
		r2 := parseOpLet(ls, n + r1.size);
		
		try r3 := parseLetVar(ls, n + r1.size + r2.size);		except
		try	r3 := parseOperation(ls, n + r1.size + r2.size);	except
		raise; end; end;
		
		parseLetVar := CParseResult.Create(CLetVar.Create(r1.tree, r3.tree), r1.size + r2.size + r3.size);
	except
		raise;
	end;
end;

function parseLetFunction(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseTkFunction(ls, n);
		r2 := parseArgument(ls, n + r1.size);
		r3 := parseBlock(ls, n + r1.size + r2.size);
		
		parseLetFunction := CParseResult.Create(CLetFunction.Create(r2.tree, r3.tree), r1.size + r2.size + r3.size);
	except
		raise;
	end;
end;

function parseLetClass(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseTkClass(ls, n);
		r2 := parseArgument(ls, n + r1.size);
		r3 := parseBlock(ls, n + r1.size + r2.size);
		
		parseLetClass := CParseResult.Create(CLetClass.Create(r2.tree, r3.tree), r1.size + r2.size + r3.size);
	except
		raise;
	end;
end;

//Lines/Blocks/Program
function parseLine(const ls : CTokenList; const n : Integer) : CParseResult;
begin
	try parseLine := parseLetVar(ls, n);		except
	try parseLine := parseOperation(ls, n);		except
	try parseLine := parseCondition(ls, n);		except
	try parseLine := parseLoop(ls, n);			except
	try parseLine := parseReturn(ls, n);		except
	try parseLine := parseTkBreak(ls, n);		except
	try parseLine := parseTkContinue(ls, n);	except
	try parseLine := parsePrint(ls, n);			except
	try parseLine := parseTkPass(ls, n);		except
	raise; end; end; end; end; end; end; end; end; end;
end;

function parseLines(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2 : CParseResult;
begin
	try
		r1 := parseLine(ls, n);
		
		try
			r2 := parseLines(ls, n + r1.size);
			parseLines := CParseResult.Create(CLines.Create(r1.tree, r2.tree), r1.size + r2.size);
		except
			parseLines := r1;
		end;
	except
		raise;
	end;
end;

function parseBlock(const ls : CTokenList; const n : Integer) : CParseResult;
var r1, r2, r3 : CParseResult;
begin
	try
		r1 := parseLCurBracket(ls, n);
		r2 := parseLines(ls, n + r1.size);
		r3 := parseRCurBracket(ls, n + r1.size + r2.size);
		
		parseBlock := CParseResult.Create(r2.tree, r1.size + r2.size + r3.size);
	except
	try
		parseBlock := parseLine(ls, n);
	except
	raise; end; end;
end;

function parseProgram(const ls : CTokenList; const n : Integer) : CParseResult; begin
	try
		parseProgram := parseLines(ls, n);
	except
		raise;
	end;
end;

end.

