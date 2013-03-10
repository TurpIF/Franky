unit UMetaParser;

{$mode objfpc}

interface

uses SysUtils, Classes, UMetaTree, UStringTokenMap, USyntaxTree, UToken, Error;//, USyntaxTree;

type CParseResult = class(TObject)
	private
		fTree : CSyntaxTree;
		fSize : integer;
	
	public
		constructor Create(aTree : CSyntaxTree; aSize : integer);
		
		property tree	: CSyntaxTree	read fTree	write fTree;
		property size	: integer		read fSize	write fSize;
end;

type CParser = class(TObject)
	private
		fMapGrammar		: CStringMetaTreeMap;	//List of all grammar rules
		fMapTerminal	: CStringTokenMap;		//List of all terminal rules
		fNothing		: String;				//The empty terminal
		
		function _product(rule : String; code : CTokenList; n : integer) : CParseResult;
		function _productNothing() : CParseResult;
		function _productTerminal(rule : CToken; code : CTokenList; n : integer) : CParseResult;
		function _productGrammar(rule : CMetaTree; code : CTokenList; n : integer) : CParseResult;
		
	public
		constructor Create();
		destructor Destroy(); override;
		
		//Add a new relation from a rule to a terminal
		procedure addTerminal(rule : String; terminal : CToken);
		
		//Add a new relation from a rule to a grammar rule
		procedure addGrammar(rule : String; grammar : CMetaTree);
		
		//Product a rule and return the corresponding syntax tree
		function product(rule : String; code : CTokenList) : CSyntaxTree;
		
		//Property of the empty terminal
		property nothing	: String	read fNothing	write fNothing;
end;

implementation

constructor CParseResult.Create(aTree : CSyntaxTree; aSize : integer);
begin
	fTree := aTree;
	fSize := aSize;
end;

constructor CParser.Create(); begin
	fMapTerminal	:= CStringTokenMap.Create();
	fMapGrammar		:= CStringMetaTreeMap.Create();
	fNothing := '_nothing';
end;

destructor CParser.Destroy(); begin
	inherited;
	
	fMapTerminal.Free();
	fMapGrammar.Free();
	
	fMapTerminal	:= NIL;
	fMapGrammar		:= NIL;
end;

//Add a new relation from a rule to a terminal
procedure CParser.addTerminal(rule : String; terminal : CToken); begin
	fMapTerminal.append(rule, terminal);
end;

//Add a new relation from a rule to a grammar rule
procedure CParser.addGrammar(rule : String; grammar : CMetaTree); begin
	fMapGrammar.append(rule, grammar);
end;

//Product a rule and return the corresponding syntax tree
function CParser.product(rule : String; code : CTokenList) : CSyntaxTree;
var re : CParseResult;
begin
	try
		re := _product(rule, code, 0);
		
		if re.size <> code.length then
			raise nothingExpectedFound(code.at(re.size).toString());
		
		product := re.tree;
	except
		raise;
	end;
end;

function CParser._product(rule : String; code : CTokenList; n : Integer) : CParseResult;
begin
	writeln('product : ', rule);
	try
		if fMapGrammar.at(rule) <> NIL then
			_product := _productGrammar(fMapGrammar.at(rule), code, n)
		else if fMapTerminal.at(rule) <> NIL then
			_product := _productTerminal(fMapTerminal.at(rule), code, n)
		else if rule = fNothing then
			_product := _productNothing()
		else begin
			raise expectedTokenFound(rule, code.at(n).toString())
		end;
	except
		raise;
	end;
end;

function CParser._productNothing() : CParseResult; begin
	writeln('product nothing');
	_productNothing := CParseResult.Create(CSyntaxTree.Create(CToken.TkNothing), 0);
end;

function CParser._productTerminal(rule : CToken; code : CTokenList; n : integer) : CParseResult; begin
	writeln('product terminal : ', rule.toString(), ' ', n);
	if n >= code.length then
		raise expectedToken(rule.toString());
	
	if rule.equals(code.at(n)) then
		_productTerminal := CParseResult.Create(CSyntaxTree.Create(code.at(n)), 1)
	else
		raise expectedTokenFound(rule.toString(), code.at(n).toString());
end;

function CParser._productGrammar(rule : CMetaTree; code : CTokenList; n : integer) : CParseResult;
var i, d, nbrE : integer; temp : CParseResult;
begin
	d := 0;
	nbrE := 0;
	
	writeln('product grammar : ', n, ' : ', rule.toString());
	writeln(rule.lsOr.toString());
	if rule.lsOr.length <> 0 then begin
		for i := 0 to rule.lsOr.length - 1 do begin
			try
				writeln(rule.lsOr.at(i).data);
				_productGrammar := _productGrammar(rule.lsOr.at(i), code, n);
				d := _productGrammar.size;
				break;
			except
				d := 0;
				nbrE := nbrE + 1;
			end;
		end;
		
		if nbrE = rule.lsOr.length + 1 then	nbrE := 1
		else	nbrE := 0;
	end
	else
		_productGrammar := _productNothing();
	
	if nbrE = 0 then
		if rule.lsAnd.length <> 0 then begin
			try
				for i := 0 to rule.lsAnd.length - 1 do begin
					temp := _productGrammar(rule.lsAnd.at(i), code, n + d);
					_productGrammar.tree.addChild(temp.tree);
					d := d + temp.size;
				end;
			except
				raise;
			end;
		end;
	
	if (rule.lsAnd.length = 0) and (rule.lsOr.length = 0) then
		try
			_productGrammar := _product(rule.data, code, n);
			d := _productGrammar.size;
			nbrE := 0;
		except
			raise;
		end;
	
	if nbrE = 0 then begin
		if rule.lsAnd.length = 0 then
			_productGrammar := CParseResult.Create(_productGrammar.tree, d)
		else
			_productGrammar := CParseResult.Create(_productGrammar.tree, d);
	end;
	
	if nbrE <> 0 then begin
		if n + d >= code.length then	raise expectedToken(rule.data);
		raise expectedTokenFound(rule.data, code.at(n + d).toString());
	end;
end;

end.
