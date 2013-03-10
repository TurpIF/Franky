program Test;

{$mode objfpc}

uses SysUtils, Classes, UToken, UStringTokenMap, ULexer, USyntaxTree, UParser, UEnvironment;

var lexerRules : CStringTokenMap; lexerResult : CTokenList; parserResult : CSyntaxTree; e : Exception; code, line : AnsiString; input : Text;// m : CMetaTree; parser : CParser;
begin
	CToken.init();
	
	lexerRules := CStringTokenMap.Create();
	lexerRules.append('function', CToken.TkFunction);
	lexerRules.append('continue', CToken.TkContinue);
	lexerRules.append('return', CToken.TkReturn);
	lexerRules.append('break', CToken.TkBreak);
	lexerRules.append('while', CToken.TkWhile);
	//lexerRules.append('class', CToken.TkClass);
	lexerRules.append('True', CToken.TkTrue);
	lexerRules.append('False', CToken.TkFalse);
	lexerRules.append('print', CToken.TkPrint);
	lexerRules.append('input', CToken.TkInput);
	lexerRules.append('else', CToken.TkElse);
	//lexerRules.append('for', CToken.TkFor);
	lexerRules.append('pass', CToken.TkPass);
	lexerRules.append('if', CToken.TkIf);
	
	lexerRules.append('len', CToken.TkFunctLength);
	lexerRules.append('at', CToken.TkFunctAt);
	lexerRules.append('time', CToken.TkFunctTime);
	
	lexerRules.append('str', CToken.TkCastStr);
	lexerRules.append('int', CToken.TkCastInt);
	lexerRules.append('float', CToken.TkCastFloat);
	lexerRules.append('bool', CToken.TkCastBool);
	
	lexerRules.append('**', CToken.TkOpPow);
	//lexerRules.append('++', CToken.TkOpInc);
	//lexerRules.append('--', CToken.TkOpDec);
	lexerRules.append('+', CToken.TkOpAdd);
	lexerRules.append('-', CToken.TkOpSub);
	lexerRules.append('*', CToken.TkOpMul);
	lexerRules.append('/', CToken.TkOpDiv);
	lexerRules.append('%', CToken.TkOpMod);
	
	lexerRules.append('and', CToken.TkOpBoolAnd);
	lexerRules.append('or', CToken.TkOpBoolOr);
	lexerRules.append('not', CToken.TkOpBoolNot);
	lexerRules.append('is', CToken.TkOpBoolIs);
	
	//lexerRules.append('&', CToken.TkOpBitAnd);
	//lexerRules.append('|', CToken.TkOpBitOr);
	//lexerRules.append('~', CToken.TkOpBitNo);
	//lexerRules.append('^', CToken.TkOpBitXor);
	//lexerRules.append('>>', CToken.TkOpBitRShift);
	//lexerRules.append('<<', CToken.TkOpBitLShift);
	
	lexerRules.append('==', CToken.TkOpEq);
	lexerRules.append('!=', CToken.TkOpNe);
	lexerRules.append('<=', CToken.TkOpLe);
	lexerRules.append('>=', CToken.TkOpGe);
	lexerRules.append('<', CToken.TkOpLt);
	lexerRules.append('>', CToken.TkOpGt);
	
	//lexerRules.append('in', CToken.TkOpIn);
	lexerRules.append('.', CToken.TkOpGet);
	lexerRules.append(':', CToken.TkOpTo);
	lexerRules.append('=', CToken.TkOpLet);
	
	lexerRules.append('(', CToken.TkLBrace);
	lexerRules.append(')', CToken.TkRBrace);
	lexerRules.append('[', CToken.TkLSqBracket);
	lexerRules.append(']', CToken.TkRSqBracket);
	lexerRules.append('{', CToken.TkLCurBracket);
	lexerRules.append('}', CToken.TkRCurBracket);
	
	lexerRules.append(#10, CToken.TkEndl);
	lexerRules.append(';', CToken.TkEndl);
	lexerRules.append(',', CToken.TkSeparator);
	
	assign(input, ParamStr(1));
	reset(input);
	line := '';
	code := '';
	while not eof(input) do begin
		readln(input, line);
		code := code + #10 + line;
	end;
	close(input);
	
	try
		lexerResult := lexer(code, lexerRules);
		//writeln(lexerResult.toString());
		
		//parserResult := parser.product('operation', lexerResult);
		parserResult := parse(lexerResult);
		//writeln(parserResult.toString());
		
		parserResult.eval(CEnvironment.Create());
	except on e : Exception do
		writeln(e.Message);
	end;
	
	//parser.Free();
	lexerRules.Free();
	CToken.freeTokens();
end.
