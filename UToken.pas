unit UToken;

{$mode objfpc}
{$static on}

interface

uses SysUtils, Classes, UList;

//Generator Pattern
type CTokenList = class;

type CToken = class(TObject)
	private
		//Enum of all the tokens in the language
		type EToken = (
			Number, Float, Bool, StringChar, Id, TkTrue, TkFalse,
			OpAdd, OpSub, OpMul, OpDiv, OpMod, OpPw, OpInc, OpDec,
			OpBoolAnd, OpBoolOr, OpBoolNot, OpBoolIs,
			OpEq, OpNe, OpLt, OpGt, OpLe, OpGe,
			OpBitAnd, OpBitOr, OpBitNo, OpBitXor, OpBitRShift, OpBitLShift,
			OpIn, OpGet, OpTo, OpLet,
			TkIf, TkElse, TkFor, TkWhile, TkBreak, TkContinue, TkReturn, TkFunction, TkClass, TkPrint, TkInput, TkPass,
			LBrace, RBrace, LSqBracket, RSqBracket, LCurBracket, RCurBracket,
			TkEndl, TkSeparator,
			TkCastStr, TkCastInt, TkCastFloat, TkCastBool,
			TkFunctLength, TkFunctAt, TkFunctTime,
			TkNothing
		);
	
	private
		//Value of a token
		var
		fIntVal		: LongInt;
		fFloatVal	: Double;
		fBoolVal	: Boolean;
		fStrVal		: AnsiString;
		
		//Type of a token
		var fWord	: CToken.EToken;
		
		//To String
		var fToStr	: String;
		
		//List of the instanciated tokens
		class var sLsToken	: CTokenList;
		
		//Basic tokens. Constante token are only to compare a wild token to a reference
		class var
		sTkNumber		: CToken;
		sTkFloat		: CToken;
		sTkBool			: CToken;
		sTkStringChar	: CToken;
		sTkId			: CToken;
		sTkTrue			: CToken;
		sTkFalse		: CToken;
		
		sTkOpAdd	: CToken;
		sTkOpSub	: CToken;
		sTkOpMul	: CToken;
		sTkOpDiv	: CToken;
		sTkOpMod	: CToken;
		sTkOpPow	: CToken;
		sTkOpInc	: CToken;
		sTkOpDec	: CToken;
		
		sTkOpBoolAnd	: CToken;
		sTkOpBoolOr		: CToken;
		sTkOpBoolNot	: CToken;
		sTkOpBoolIs		: CToken;
		
		sTkOpEq	: CToken;
		sTkOpNe	: CToken;
		sTkOpLt	: CToken;
		sTkOpGt	: CToken;
		sTkOpLe	: CToken;
		sTkOpGe	: CToken;
		
		sTkOpBitAnd		: CToken;
		sTkOpBitOr		: CToken;
		sTkOpBitNo		: CToken;
		sTkOpBitXor		: CToken;
		sTkOpBitRShift	: CToken;
		sTkOpBitLShift	: CToken;
		
		sTkOpIn		: CToken;
		sTkOpGet	: CToken;
		sTkOpTo		: CToken;
		sTkOpLet	: CToken;
		
		sTkLBrace		: CToken;
		sTkRBrace		: CToken;
		sTkLSqBracket	: CToken;
		sTkRSqBracket	: CToken;
		sTkLCurBracket	: CToken;
		sTkRCurBracket	: CToken;
		
		sTkIf		: CToken;
		sTkElse		: CToken;
		sTkFor		: CToken;
		sTkWhile	: CToken;
		sTkContinue	: CToken;
		sTkBreak	: CToken;
		sTkReturn	: CToken;
		sTkFunction	: CToken;
		sTkClass	: CToken;
		sTkPrint	: CToken;
		sTkInput	: CToken;
		sTkPass		: CToken;
		
		sTkFunctLength	: CToken;
		sTkFunctAt		: CToken;
		sTkFunctTime	: CToken;
		
		sTkEndl			: CToken;
		sTkSeparator	: CToken;
		
		sTkCastStr		: CToken;
		sTkCastInt		: CToken;
		sTkCastFloat	: CToken;
		sTkCastBool		: CToken;
		
		sTkNothing		: CToken;
		
		//All constructors are private to manage the memory
		constructor CreateToken(const tk : Etoken; const aToStr : String);
		constructor CreateNumber(const aIntVal : LongInt);
		constructor CreateFloat(const aFloatVal : Double);
		constructor CreateBoolean(const aBoolVal : Boolean);
		constructor CreateString(const aStrVal : AnsiString);
		constructor CreateId(const aName : AnsiString; const aAddr : LongInt);
		
		//The destruction of all the Tokens will be done automatically
		
		//Add created token to the list
		class function add(tk : CToken) : CToken; static;
		
	public
		//Initialisation of the generator. Must be called before all operations on tokens
		class procedure init(); static;
		
		//Destroy all tokens. Must be called only when tokens are not used anymore
		class procedure freeTokens(); static;
		
		//Number, float, bool, string and id generator
		class function getNumber(const aIntVal : LongInt)						: CToken; static;
		class function getFloat(const aFloatVal : Double)						: CToken; static;
		class function getBoolean(const aBoolVal : Boolean)						: CToken; static;
		class function getString(const aStrVal : AnsiString)					: CToken; static;
		class function getId(const aName : AnsiString; const aAddr : LongInt)	: CToken; static;
		
		//To debug
		function toString() : AnsiString; override;
		
		//To compare
		function equalsStrict(tk : CToken) : Boolean; virtual;
		function equals(tk : TObject) : Boolean; override;
		
		//Values getter
		property intValue	: LongInt		read fIntVal;
		property floatValue	: Double		read fFloatVal;
		property boolValue	: Boolean		read fBoolVal;
		property strValue	: AnsiString	read fStrVal;
		
		//Type getter
		property word	: CToken.EToken	read fWord;
		
		//Tokens getter
		class property TkNumber		: CToken read sTkNumber;
		class property TkFloat		: CToken read sTkFloat;
		class property TkBool		: CToken read sTkBool;
		class property TkStringChar	: CToken read sTkStringChar;
		class property TkId			: CToken read sTkId;
		class property TkTrue		: CToken read sTkTrue;
		class property TkFalse		: CToken read sTkFalse;
		
		class property TkOpAdd	: CToken read sTkOpAdd;
		class property TkOpSub	: CToken read sTkOpSub;
		class property TkOpMul	: CToken read sTkOpMul;
		class property TkOpDiv	: CToken read sTkOpDiv;
		class property TkOpMod	: CToken read sTkOpMod;
		class property TkOpPow	: CToken read sTkOpPow;
		class property TkOpInc	: CToken read sTkOpInc;
		class property TkOpDec	: CToken read sTkOpDec;
		
		class property TkOpBoolAnd	: CToken read sTkOpBoolAnd;
		class property TkOpBoolOr	: CToken read sTkOpBoolOr;
		class property TkOpBoolNot	: CToken read sTkOpBoolNot;
		class property TkOpBoolIs	: CToken read sTkOpBoolIs;
		
		class property TkOpEq	: CToken read sTkOpEq;
		class property TkOpNe	: CToken read sTkOpNe;
		class property TkOpLt	: CToken read sTkOpLt;
		class property TkOpGt	: CToken read sTkOpGt;
		class property TkOpLe	: CToken read sTkOpLe;
		class property TkOpGe	: CToken read sTkOpGe;
		
		class property TkOpBitAnd		: CToken read sTkOpBitAnd;
		class property TkOpBitOr		: CToken read sTkOpBitOr;
		class property TkOpBitNo		: CToken read sTkOpBitNo;
		class property TkOpBitXor		: CToken read sTkOpBitXor;
		class property TkOpBitRShift	: CToken read sTkOpBitRShift;
		class property TkOpBitLShift	: CToken read sTkOpBitLShift;
		
		class property TkOpIn	: CToken read sTkOpIn;
		class property TkOpGet	: CToken read sTkOpGet;
		class property TkOpTo	: CToken read sTkOpTo;
		class property TkOpLet	: CToken read sTkOpLet;
		
		class property TkLBrace			: CToken read sTkLBrace;
		class property TkRBrace			: CToken read sTkRBrace;
		class property TkLSqBracket		: CToken read sTkLSqBracket;
		class property TkRSqBracket		: CToken read sTkRSqBracket;
		class property TkLCurBracket	: CToken read sTkLCurBracket;
		class property TkRCurBracket	: CToken read sTkRCurBracket;
		
		class property TkIf			: CToken read sTkIf;
		class property TkElse		: CToken read sTkElse;
		class property TkFor		: CToken read sTkFor;
		class property TkWhile		: CToken read sTkWhile;
		class property TkFunction	: CToken read sTkFunction;
		class property TkClass		: CToken read sTkClass;
		class property TkReturn		: CToken read sTkReturn;
		class property TkBreak		: CToken read sTkBreak;
		class property TkContinue	: CToken read sTkContinue;
		class property TkPrint		: CToken read sTkPrint;
		class property TkInput		: CToken read sTkInput;
		class property TkPass		: CToken read sTkPass;
		
		class property TkFunctLength	: CToken read sTkFunctLength;
		class property TkFunctAt		: CToken read sTkFunctAt;
		class property TkFunctTime		: CToken read sTkFunctTime;
		
		class property TkEndl		: CToken read sTkEndl;
		class property TkSeparator	: CToken read sTkSeparator;
		
		class property TkCastStr	: CToken read sTkCastStr;
		class property TkCastInt	: CToken read sTkCastInt;
		class property TkCastFloat	: CToken read sTkCastFloat;
		class property TkCastBool	: CToken read sTkCastBool;
		
		class property TkNothing	: CToken read sTkNothing;
		
		//class property TkNothing	: CToken read sTkNothing;
end;

//Add a toString function to the token list
type CTokenList = class(specialize CList<CToken>)
	protected
		function nodeToStr(node : TPNode) : String; override;
end;

implementation

function CTokenList.nodeToStr(node : TPNode) : String; begin
	nodeToStr := node^.data.toString();
end;

//All constructors are private to manage the memory
constructor CToken.CreateToken(const tk : Etoken; const aToStr : String); begin
	fWord		:= tk;
	fToStr		:= aToStr;
	
	fIntVal		:= 0;
	fFloatVal	:= 0.0;
	fBoolVal	:= False;
	fStrVal		:= '';
end;

constructor CToken.CreateNumber(const aIntVal : LongInt); begin
	fWord		:= CToken.EToken.Number;
	fToStr		:= 'Number';
	
	fIntVal		:= aIntVal;
	fFloatVal	:= 0.0;
	fBoolVal	:= False;
	fStrVal		:= '';
end;

constructor CToken.CreateFloat(const aFloatVal : Double); begin
	fWord		:= CToken.EToken.Float;
	fToStr		:= 'Float';
	
	fIntVal		:= 0;
	fFloatVal	:= aFloatVal;
	fBoolVal	:= False;
	fStrVal		:= '';
end;

constructor CToken.CreateBoolean(const aBoolVal : Boolean); begin
	fWord		:= CToken.EToken.Bool;
	fToStr		:= 'Boolean';
	
	fIntVal		:= 0;
	fFloatVal	:= 0.0;
	fBoolVal	:= aBoolVal;
	fStrVal		:= '';
end;

constructor CToken.CreateString(const aStrVal : AnsiString); begin
	fWord		:= CToken.EToken.StringChar;
	fToStr		:= 'String';
	
	fIntVal		:= 0;
	fFloatVal	:= 0.0;
	fBoolVal	:= False;
	fStrVal		:= aStrVal;
end;

constructor CToken.CreateId(const aName : AnsiString; const aAddr : LongInt); begin
	fWord		:= CToken.EToken.Id;
	fToStr		:= 'Identifiant';
	
	fIntVal		:= aAddr;
	fFloatVal	:= 0.0;
	fBoolVal	:= False;
	fStrVal		:= aName;
end;

//Add created token to the list
class function CToken.add(tk : CToken) : CToken; begin
	CToken.sLsToken.Append(tk);
	add := tk;
end;

//Initialisation of the generator. Must be called before all operations on token
class procedure CToken.init(); begin
	CToken.sLsToken := CTokenList.Create();
	
	CToken.sTkNumber		:= CToken.getNumber(0);
	CToken.sTkFloat			:= CToken.getFloat(0.0);
	CToken.sTkBool			:= CToken.getBoolean(false);
	CToken.sTkStringChar	:= CToken.getString('');
	CToken.sTkId			:= CToken.getId('', 0);
	
	CToken.sTkOpAdd	:= add(CToken.CreateToken(CToken.EToken.OpAdd, '+'));
	CToken.sTkOpSub	:= add(CToken.CreateToken(CToken.EToken.OpSub, '-'));
	CToken.sTkOpMul	:= add(CToken.CreateToken(CToken.EToken.OpMul, '*'));
	CToken.sTkOpDiv	:= add(CToken.CreateToken(CToken.EToken.OpDiv, '/'));
	CToken.sTkOpMod	:= add(CToken.CreateToken(CToken.EToken.OpAdd, '%'));
	CToken.sTkOpPow	:= add(CToken.CreateToken(CToken.EToken.OpSub, '**'));
	CToken.sTkOpInc	:= add(CToken.CreateToken(CToken.EToken.OpMul, '++'));
	CToken.sTkOpDec	:= add(CToken.CreateToken(CToken.EToken.OpDiv, '--'));
	
	CToken.sTkOpBoolAnd	:= add(CToken.CreateToken(CToken.EToken.OpBoolAnd, 'and'));
	CToken.sTkOpBoolOr	:= add(CToken.CreateToken(CToken.EToken.OpBoolOr, 'or'));
	CToken.sTkOpBoolNot	:= add(CToken.CreateToken(CToken.EToken.OpBoolNot, 'not'));
	CToken.sTkOpBoolIs	:= add(CToken.CreateToken(CToken.EToken.OpBoolIs, 'is'));
	
	CToken.sTkOpEq	:= add(CToken.CreateToken(CToken.EToken.OpEq, '=='));
	CToken.sTkOpNe	:= add(CToken.CreateToken(CToken.EToken.OpNe, '!='));
	CToken.sTkOpLt	:= add(CToken.CreateToken(CToken.EToken.OpLt, '<'));
	CToken.sTkOpGt	:= add(CToken.CreateToken(CToken.EToken.OpGt, '>'));
	CToken.sTkOpLe	:= add(CToken.CreateToken(CToken.EToken.OpLe, '<='));
	CToken.sTkOpGe	:= add(CToken.CreateToken(CToken.EToken.OpGe, '>='));
	
	CToken.sTkOpBitAnd		:= add(CToken.CreateToken(CToken.EToken.OpBitAnd, '&'));
	CToken.sTkOpBitOr		:= add(CToken.CreateToken(CToken.EToken.OpBitOr, '|'));
	CToken.sTkOpBitNo		:= add(CToken.CreateToken(CToken.EToken.OpBitNo, '~'));
	CToken.sTkOpBitXor		:= add(CToken.CreateToken(CToken.EToken.OpBitXor, '^'));
	CToken.sTkOpBitLShift	:= add(CToken.CreateToken(CToken.EToken.OpBitRShift, '>>'));
	CToken.sTkOpBitRShift	:= add(CToken.CreateToken(CToken.EToken.OpBitLShift, '<<'));
	
	CToken.sTkOpIn	:= add(CToken.CreateToken(CToken.EToken.OpIn, 'in'));
	CToken.sTkOpGet	:= add(CToken.CreateToken(CToken.EToken.OpGet, '.'));
	CToken.sTkOpTo	:= add(CToken.CreateToken(CToken.EToken.OpTo, ':'));
	CToken.sTkOpLet	:= add(CToken.CreateToken(CToken.EToken.OpLet, '='));
	
	CToken.sTkIf		:= add(CToken.CreateToken(CToken.EToken.TkIf, 'if'));
	CToken.sTkElse		:= add(CToken.CreateToken(CToken.EToken.TkElse, 'else'));
	CToken.sTkWhile		:= add(CToken.CreateToken(CToken.EToken.TkWhile, 'while'));
	CToken.sTkFor		:= add(CToken.CreateToken(CToken.EToken.TkFor, 'for'));
	CToken.sTkContinue	:= add(CToken.CreateToken(CToken.EToken.TkContinue, 'continue'));
	CToken.sTkBreak		:= add(CToken.CreateToken(CToken.EToken.TkBreak, 'break'));
	CToken.sTkReturn	:= add(CToken.CreateToken(CToken.EToken.TkReturn, 'return'));
	CToken.sTkFunction	:= add(CToken.CreateToken(CToken.EToken.TkFunction, 'function'));
	CToken.sTkClass		:= add(CToken.CreateToken(CToken.EToken.TkClass, 'class'));
	CToken.sTkPass		:= add(CToken.CreateToken(CToken.EToken.TkPass, 'pass'));
	CToken.sTkTrue		:= add(CToken.CreateToken(CToken.EToken.TkTrue, 'True'));
	CToken.sTkFalse		:= add(CToken.CreateToken(CToken.EToken.TkFalse, 'False'));
	CToken.sTkPrint		:= add(CToken.CreateToken(CToken.EToken.TkPrint, 'print'));
	CToken.sTkInput		:= add(CToken.CreateToken(CToken.EToken.TkInput, 'input'));
	
	CToken.sTkFunctLength	:= add(CToken.CreateToken(CToken.EToken.TkFunctLength, 'len'));
	CToken.sTkFunctAt		:= add(CToken.CreateToken(CToken.EToken.TkFunctAt, 'at'));
	CToken.sTkFunctTime		:= add(CToken.CreateToken(CToken.EToken.TkFunctTime, 'time'));
	
	CToken.sTkLBrace		:= add(CToken.CreateToken(CToken.EToken.LBrace, '('));
	CToken.sTkRBrace		:= add(CToken.CreateToken(CToken.EToken.RBrace, ')'));
	CToken.sTkLSqBracket	:= add(CToken.CreateToken(CToken.EToken.LSqBracket, '['));
	CToken.sTkRSqBracket	:= add(CToken.CreateToken(CToken.EToken.RSqBracket, ']'));
	CToken.sTkLCurBracket	:= add(CToken.CreateToken(CToken.EToken.LCurBracket, '{'));
	CToken.sTkRCurBracket	:= add(CToken.CreateToken(CToken.EToken.RCurBracket, '}'));
	
	CToken.sTkEndl		:= add(CToken.CreateToken(CToken.EToken.TkEndl, ';'));
	CToken.sTkSeparator	:= add(CToken.CreateToken(CToken.EToken.TkSeparator, ','));
	
	CToken.sTkCastStr	:= add(CToken.CreateToken(CToken.EToken.TkCastStr, 'str'));
	CToken.sTkCastInt	:= add(CToken.CreateToken(CToken.EToken.TkCastInt, 'int'));
	CToken.sTkCastFloat	:= add(CToken.CreateToken(CToken.EToken.TkCastFloat, 'float'));
	CToken.sTkCastBool	:= add(CToken.CreateToken(CToken.EToken.TkCastBool, 'bool'));
	
	CToken.sTkNothing	:= add(CToken.CreateToken(CToken.EToken.TkNothing, '__n'));
end;

//Destroy all tokens. Must be called only when token are not used anymore
class procedure CToken.freeTokens();
var n : Integer;
begin
	for n := 0 to CToken.sLsToken.length - 1 do
		CToken.sLsToken.at(n).Free();
	
	CToken.sLsToken.Free();
	sLsToken := NIL;
end;

//Number, float, bool, string and id generator
class function CToken.getNumber(const aIntVal : LongInt)						: CToken; begin getNumber	:= add(CToken.CreateNumber(aIntVal));	end;
class function CToken.getFloat(const aFloatVal : Double)						: CToken; begin getFloat	:= add(CToken.CreateFloat(aFloatVal));	end;
class function CToken.getBoolean(const aBoolVal : Boolean)						: CToken; begin getBoolean	:= add(CToken.CreateBoolean(aBoolVal));	end;
class function CToken.getString(const aStrVal : AnsiString)						: CToken; begin getString	:= add(CToken.CreateString(aStrVal));	end;
class function CToken.getId(const aName : AnsiString; const aAddr : LongInt)	: CToken; begin getId		:= add(CToken.CreateId(aName, aAddr));	end;

//To debug
function CToken.toString() : AnsiString; begin
	toString := fToStr;
	
	if fWord = CToken.Etoken.Number		then toString := toString + '(' + IntToStr(fIntVal)						+ ')';
	if fWord = CToken.Etoken.Float		then toString := toString + '(' + FloatToStr(fFloatVal)					+ ')';
	if fWord = CToken.Etoken.StringChar	then toString := toString + '(' + fStrVal								+ ')';
	if fWord = CToken.Etoken.Id			then toString := toString + '(' + fStrVal + ', ' + IntToStr(fIntVal)	+ ')';
	if fWord = CToken.Etoken.Bool then
		if fBoolVal						then toString := toString + '(' + 'true'								+ ')'
		else								 toString := toString + '(' + 'false'								+ ')';
end;

//To compare
function CToken.equalsStrict(tk : CToken) : Boolean; begin
	equalsStrict := equals(tk);
	if (fWord = CToken.EToken.Number)		and (fIntVal <> tk.intValue)		then equalsStrict := false;
	if (fWord = CToken.EToken.Float)		and (fFloatVal <> tk.floatValue)	then equalsStrict := false;
	if (fWord = CToken.EToken.Bool)			and (fBoolVal <> tk.boolValue)		then equalsStrict := false;
	if (fWord = CToken.EToken.StringChar)	and (fStrVal <> tk.strValue)		then equalsStrict := false;
	if (fWord = CToken.EToken.Id)			and (fStrVal <> tk.strValue)		then equalsStrict := false;
end;
function CToken.equals(tk : TObject) : Boolean;
var t : CToken;
begin
	try
		t := tk as CToken;
		equals := fWord = t.word;
	except
		equals := false;
	end;
end;

end.
