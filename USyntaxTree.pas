unit USyntaxTree;

{$mode objfpc}

interface

uses SysUtils, Classes, UToken, UEnvironment, Error;

{type CTerminalTree = class(CSyntaxTree)
	public
		//Evaluation of an expression
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;}

type CTime = class(CSyntaxTree)
	public
		constructor Create();
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CInput = class(CSyntaxTree)
	public
		constructor Create();
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CArgument = class(CSyntaxTree)
	public
		constructor Create(left, right : CSyntaxTree);
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type COpArgument = class(CSyntaxTree)
	public
		constructor Create(left, right : CSyntaxTree);
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CCallFunction = class(CSyntaxTree)
	public
		constructor Create(name, args : CSyntaxTree);
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CCondition = class(CSyntaxTree)
	public
		constructor Create(cond, blockIf : CSyntaxTree);
		constructor Create(cond, blockIf, blockElse : CSyntaxTree);

		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CWhile = class(CSyntaxTree)
	public
		constructor Create(cond, block : CSyntaxTree);
	
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CFor = class(CSyntaxTree)
	public
		constructor Create(ite, list, block : CSyntaxTree);

		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CReturn = class(CSyntaxTree)
	public
		constructor Create(return : CSyntaxTree);

		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CLetVar = class(CSyntaxTree)
	public
		constructor Create(id, val : CSyntaxTree);

		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CLetFunction = class(CSyntaxTree)
	public
		constructor Create(args, block : CSyntaxTree);

		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CLetClass = class(CSyntaxTree)
	public
		constructor Create(parent, block : CSyntaxTree);
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CLines = class(CSyntaxTree)
	public
		constructor Create(left, right : CSyntaxTree);
		
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CPrint = class(CSyntaxTree)
	public
		constructor Create(operation : CSyntaxTree);
	
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

type CVariable = class(CSyntaxTree)
	public
		constructor Create(valeur : CSyntaxTree);
	
		function eval(environment : CEnvironment) : CSyntaxTree; override;
end;

implementation

function lengthStr(s : String) : Integer; begin
	lengthStr := length(s);
end;

constructor CTime.Create(); begin
	inherited Create(CToken.TkFunctTime);
end;

constructor CInput.Create(); begin
	inherited Create(CToken.TkInput);
end;

constructor CArgument.Create(left, right : CSyntaxTree); begin
	inherited Create(CToken.TkNothing);
	
	if left <> NIL then		addChild(left);
	if right <> NIL then	addChild(right);
end;

constructor COpArgument.Create(left, right : CSyntaxTree); begin
	inherited Create(CToken.TkNothing);
	
	if left <> NIL then		addChild(left);
	if right <> NIL then	addChild(right);
end;

constructor CCallFunction.Create(name, args : CSyntaxTree); begin
	inherited Create(CToken.TkFunction);
	
	addChild(name);
	addChild(args);
end;

constructor CCondition.Create(cond, blockIf : CSyntaxTree); begin
	inherited Create(CToken.TkIf);
	
	addChild(cond);
	addChild(blockIf);
end;

constructor CCondition.Create(cond, blockIf, blockElse : CSyntaxTree); begin
	inherited Create(CToken.TkIf);
	
	addChild(cond);
	addChild(blockIf);
	addChild(blockElse);
end;

constructor CWhile.Create(cond, block : CSyntaxTree); begin
	inherited Create(CToken.TkWhile);
	
	addChild(cond);
	addChild(block);
end;

constructor CFor.Create(ite, list, block : CSyntaxTree); begin
	inherited Create(CToken.TkFor);
	
	addChild(ite);
	addChild(list);
	addChild(block);
end;

constructor CReturn.Create(return : CSyntaxTree); begin
	inherited Create(CToken.TkReturn);
	
	addChild(return);
end;

constructor CLetVar.Create(id, val : CSyntaxTree); begin
	inherited Create(Ctoken.TkOpLet);
	
	addChild(id);
	addChild(val);
end;

constructor CLetFunction.Create(args, block : CSyntaxTree); begin
	inherited Create(CToken.TkFunction);
	
	addChild(args);
	addChild(block);
end;

constructor CLetClass.Create(parent, block : CSyntaxTree); begin
	inherited Create(CToken.TkClass);
	
	addChild(parent);
	addChild(block);
end;

constructor CLines.Create(left, right : CSyntaxTree); begin
	inherited Create(CToken.TkEndl);
	
	addChild(left);
	addChild(right);
end;

constructor CPrint.Create(operation : CSyntaxTree); begin
	inherited Create(CToken.TkPrint);
	
	addChild(operation);
end;

constructor CVariable.Create(valeur : CSyntaxTree); begin
	inherited Create(CToken.TkNothing);
	addChild(valeur);
end;

function CTime.eval(environment : CEnvironment) : CSyntaxTree;
begin
	eval := CSyntaxTree.Create(CToken.getFloat(Now * 100000));
end;

function CInput.eval(environment : CEnvironment) : CSyntaxTree;
var line : String;
begin
	readln(line);
	eval := CSyntaxTree.Create(CToken.getString(line));
end;

function COpArgument.eval(environment : CEnvironment) : CSyntaxTree;
begin
	eval := self;
end;

function CArgument.eval(environment : CEnvironment) : CSyntaxTree;
var i : integer; tree : CSyntaxTree;
begin
	try
	eval := CSyntaxTree.Create(CToken.TkNothing);
	
	if length >= 1 then	eval.addChild(self.child(0));
	
	if length >= 2 then begin
		tree := self.child(1) as CSyntaxTree;
		while tree.length = 2 do begin
			eval.addChild(tree.child(0));
			tree := tree.child(1) as CSyntaxTree;
		end;
		
		if tree.length = 1 then
			if (not tree.child(0).data.equals(CToken.TkId)) and tree.data.equals(CToken.TkNothing) then
				eval.addChild(tree.child(0));
		
		eval.addChild(tree);
	end;
	except
		raise
	end;
end;

function CCallFunction.eval(environment : CEnvironment) : CSyntaxTree;
var args : CSyntaxTree; a1, a2 : CToken; i : integer; str : string; env : CEnvironment;
begin
	try
	eval := NIL;
	args := (child(1) as CSyntaxTree).eval(environment);
	
	if (child(0).data = CToken.TkOpAdd) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue + a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue + a2.intValue));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue + a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue + a2.floatValue));
		end;
		
		if (a1.equals(CToken.TkStringChar)) and (a2.equals(CToken.TkStringChar)) then begin
			eval := CSyntaxTree.Create(CToken.getString(a1.strValue + a2.strValue));
		end;
		
		if eval = NIL then
			raise illegalType(a1.toString());
	end;
	
	if (child(0).data = CToken.TkOpSub) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue - a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue - a2.intValue));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue - a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue - a2.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('number or real', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpMul) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkNumber) and (a1.intValue = 0)) or (a1.equals(CToken.TkFloat) and (a1.floatValue = 0)) then
			eval := CSyntaxTree.Create(CToken.getNumber(0))
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue * a2.intValue));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue * a2.intValue));
			end;
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue * a2.floatValue));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue * a2.floatValue));
			end;
			
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkStringChar)) then begin
				if a1.intValue >= 0 then begin
					str := '';
					for i := 0 to a1.intValue - 1 do
						str := str + a2.strValue;
					eval := CSyntaxTree.Create(CToken.getString(str));
				end;
			end;
			
			if (a1.equals(CToken.TkStringChar)) and (a2.equals(CToken.TkNumber)) then begin
				if a2.intValue >= 0 then begin
					str := '';
					for i := 0 to a2.intValue - 1 do
						str := str + a1.strValue;
					eval := CSyntaxTree.Create(CToken.getString(str));
				end;
			end;
		end;
		
		if eval = NIL then
			raise illegalType(a1.toString());
	end;
	
	if (child(0).data = CToken.TkOpDiv) and (args.length = 2) then begin /// NE PAS OUBLIER LE CAS DE DIVISION PAR ZERO
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkNumber) and (a1.intValue = 0)) or (a1.equals(CToken.TkFloat) and (a1.floatValue = 0)) then
			eval := CSyntaxTree.Create(CToken.getNumber(0))
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
			
			if (a2.intValue = 0) and (a2.floatValue = 0) then
				raise divideByZero();
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue div a2.intValue));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue / a2.intValue));
			end;
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue / a2.floatValue));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue / a2.floatValue));
			end;
		end;
		
		if eval = NIL then
			raise illegalType('number or real', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpMod) and (args.length = 2) then begin /// NE PAS OUBLIER LE CAS DE DIVISION PAR ZERO
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkNumber) and (a1.intValue = 0)) or (a1.equals(CToken.TkFloat) and (a1.floatValue = 0)) then
			eval := CSyntaxTree.Create(CToken.getNumber(0))
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
			
			if (a2.intValue = 0) and (a2.floatValue = 0) then
				raise divideByZero();
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue mod a2.intValue));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue - a2.intValue * trunc(a1.floatValue / a2.intValue)));
			end;
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue - a2.floatValue * trunc(a1.intValue / a2.floatValue)));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
				eval := CSyntaxTree.Create(CToken.getFloat(a1.floatValue - a2.floatValue * trunc(a1.floatValue / a2.floatValue)));
			end;
		end;
		
		if eval = NIL then
			raise illegalType('number or real', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpPow) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkNumber) and (a1.intValue = 0)) or (a1.equals(CToken.TkFloat) and (a1.floatValue = 0)) then begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
			
			if (a2.intValue = 0) and (a2.floatValue = 0) then begin
				if a1.equals(CToken.TkNumber) then	eval := CSyntaxTree.Create(CToken.getNumber(1));
				if a1.equals(CToken.TkFloat) then	eval := CSyntaxTree.Create(CToken.getFloat(1.0));
			end
			else begin
				if a1.equals(CToken.TkNumber) then	eval := CSyntaxTree.Create(CToken.getNumber(0));
				if a1.equals(CToken.TkFloat) then	eval := CSyntaxTree.Create(CToken.getFloat(0.0));
			end;
		end
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
				if a1.intValue < 0 then
					eval := CSyntaxTree.Create(CToken.getNumber(trunc(-exp(ln(-a1.intValue) * a2.intValue))))
				else
					eval := CSyntaxTree.Create(CToken.getNumber(trunc(exp(ln(a1.intValue) * a2.intValue))));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
				if a1.floatValue < 0 then
					eval := CSyntaxTree.Create(CToken.getFloat(-exp(ln(-a1.floatValue) * a2.intValue)))
				else
					eval := CSyntaxTree.Create(CToken.getFloat(exp(ln(a1.floatValue) * a2.intValue)));
			end;
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
				if a1.intValue < 0 then
					eval := CSyntaxTree.Create(CToken.getFloat(-exp(ln(-a1.intValue) * a2.floatValue)))
				else
					eval := CSyntaxTree.Create(CToken.getFloat(exp(ln(a1.intValue) * a2.floatValue)));
			end;
			if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
				if a1.floatValue < 0 then
					eval := CSyntaxTree.Create(CToken.getFloat(-exp(ln(-a1.floatValue) * a2.floatValue)))
				else
					eval := CSyntaxTree.Create(CToken.getFloat(exp(ln(a1.floatValue) * a2.floatValue)));
			end;
		end;
		
		if eval = NIL then
			raise illegalType('number or real', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpSub) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(-a1.intValue ));
		end;
		if a1.equals(CToken.TkFloat) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(-a1.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('number or real', a1.toString());
	end;
	
	{if (child(0).data = CToken.TkOpInc) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue + 1));
		end;
	end;}
	
	{if (child(0).data = CToken.TkOpDec) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(a1.intValue - 1));
		end;
		
		if eval = NIL then
			raise illegalType('number', a1.toString());
	end;}
	
	if (child(0).data = CToken.TkOpBoolAnd) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkBool)) and (not a1.boolValue) then
			eval := CSyntaxTree.Create(CToken.getBoolean(false))
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkBool)) and (a2.equals(CToken.TkBool)) then begin
				eval := CSyntaxTree.Create(CToken.getBoolean(a1.boolValue and a2.boolValue));
			end;
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpBoolOr) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if (a1.equals(CToken.TkBool)) and a1.boolValue then
			eval := CSyntaxTree.Create(CToken.getBoolean(true))
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
		
			if (a1.equals(CToken.TkBool)) and (a2.equals(CToken.TkBool)) then begin
				eval := CSyntaxTree.Create(CToken.getBoolean(a1.boolValue or a2.boolValue));
			end;
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if (child(0).data = CToken.TkOpBoolNot) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkBool) then
			eval := CSyntaxTree.Create(CToken.getBoolean(not a1.boolValue));
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString());
	end;
	
	if ((child(0).data = CToken.TkOpBoolIs) or (child(0).data = CToken.TkOpEq)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue = a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue = (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) = a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue = a2.floatValue));
		end;
		if (a1.equals(CToken.TkStringChar)) and (a2.equals(CToken.TkStringChar)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.strValue = a2.strValue));
		end;
		if (a1.equals(CToken.TkBool)) and (a2.equals(CToken.TkBool)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.boolValue = a2.boolValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpNe)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue <> a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue <> (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) <> a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue <> a2.floatValue));
		end;
		if (a1.equals(CToken.TkStringChar)) and (a2.equals(CToken.TkStringChar)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.strValue <> a2.strValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpLt)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue < a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue < (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) < a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue < a2.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpGt)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue > a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue > (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) > a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue > a2.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpLe)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue <= a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue <= (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) <= a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue <= a2.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpGe)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
	
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue >= a2.intValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkNumber)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue >= (1.0 * a2.intValue)));
		end;
		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean((1.0 * a1.intValue) >= a2.floatValue));
		end;
		if (a1.equals(CToken.TkFloat)) and (a2.equals(CToken.TkFloat)) then begin
			eval := CSyntaxTree.Create(CToken.getBoolean(a1.floatValue >= a2.floatValue));
		end;
		
		if eval = NIL then
			raise illegalType('boolean', a1.toString(), a2.toString());
	end;
	
	if ((child(0).data = CToken.TkOpBitAnd)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkNumber) and (a1.intValue = 0) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(0));
		end
		else begin
			a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
	
			//if a1.equals(CToken.TkId) then
			//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
			//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
			//else
	
			if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
				//eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue and a2.intValue));
			end;
		end;
	end;
	
	if ((child(0).data = CToken.TkOpBitOr)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else

		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			//eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue or a2.intValue));
		end;
	end;
	
	if ((child(0).data = CToken.TkOpBitNo)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else

		if (a1.equals(CToken.TkNumber)) then begin
			//eval := CSyntaxTree.Create(CToken.getBoolean(not a1.intValue));
		end;
	end;
	
	if ((child(0).data = CToken.TkOpBitXor)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else

		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			//eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue xor a2.intValue));
		end;
	end;
	
	if ((child(0).data = CToken.TkOpBitRShift)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else

		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			//eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue >> a2.intValue));
		end;
	end;
	
	if ((child(0).data = CToken.TkOpBitLShift)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else

		if (a1.equals(CToken.TkNumber)) and (a2.equals(CToken.TkNumber)) then begin
			//eval := CSyntaxTree.Create(CToken.getBoolean(a1.intValue << a2.intValue));
		end;
	end;
	
	if ((child(0).data = CToken.TkCastStr)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if a1.equals(CToken.TkStringChar) then begin
			eval := CSyntaxTree.Create(a1);
		end;

		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(CToken.getString(IntToStr(a1.intValue)));
		end;
		
		if a1.equals(CToken.TkFloat) then begin
			eval := CSyntaxTree.Create(CToken.getString(FloatToStr(a1.floatValue)));
		end;
		
		if a1.equals(CToken.TkBool) then begin
			if a1.boolValue then
				eval := CSyntaxTree.Create(CToken.getString('true'))
			else
				eval := CSyntaxTree.Create(CToken.getString('false'));
		end;
		
		if eval = NIL then
			raise illegalCast(a1.toString(), 'string');
	end;
	
	if ((child(0).data = CToken.TkCastInt)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		try
		if a1.equals(CToken.TkStringChar) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(StrToInt(a1.strValue)));
		end;

		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(a1);
		end;
		
		if a1.equals(CToken.TkFloat) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(trunc(a1.floatValue)));
		end;
		except
			raise illegalCast(a1.toString(), 'integer');
		end;
		
		if eval = NIL then
			raise illegalCast(a1.toString(), 'integer');
	end;
	
	if ((child(0).data = CToken.TkCastFloat)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		try
		if a1.equals(CToken.TkStringChar) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(StrToFloat(a1.strValue)));
		end;

		if a1.equals(CToken.TkNumber) then begin
			eval := CSyntaxTree.Create(CToken.getFloat(a1.intValue * 1.0));
		end;
		
		if a1.equals(CToken.TkFloat) then begin
			eval := CSyntaxTree.Create(a1);
		end;
		except
			raise illegalCast(a1.toString(), 'float');
		end;
		
		if eval = NIL then
			raise illegalCast(a1.toString(), 'float');
	end;
	
	if ((child(0).data = CToken.TkCastBool)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;

		//if a1.equals(CToken.TkId) then
		//	if environment.get(a1.strValue, true).get('__add__', false) <> NIL then
		//		eval := environment.get(a1.strValue, true).get('__add__', false).tree.eval(environment);
		//else
		
		if a1.equals(CToken.TkStringChar) then begin
			if a1.strValue = '' then
				eval := CSyntaxTree.Create(CToken.getBoolean(false))
			else
				eval := CSyntaxTree.Create(CToken.getBoolean(true));
		end;

		if a1.equals(CToken.TkNumber) then begin
			if a1.intValue = 0 then
				eval := CSyntaxTree.Create(CToken.getBoolean(false))
			else
				eval := CSyntaxTree.Create(CToken.getBoolean(true));
		end;
		
		if a1.equals(CToken.TkFloat) then begin
			if a1.floatValue = 0.0 then
				eval := CSyntaxTree.Create(CToken.getBoolean(false))
			else
				eval := CSyntaxTree.Create(CToken.getBoolean(true));
		end;
		
		if eval = NIL then
			raise illegalCast(a1.toString(), 'boolean');
	end;
	
	if ((child(0).data = CToken.TkFunctLength)) and (args.length = 1) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkStringChar) then begin
			eval := CSyntaxTree.Create(CToken.getNumber(lengthStr(a1.strValue)));
		end;
		
		if eval = NIL then
			raise illegalType('string', a1.toString());
	end;
	
	if ((child(0).data = CToken.TkFunctAt)) and (args.length = 2) then begin
		a1 := (args.child(0) as CSyntaxTree).eval(environment).data;
		a2 := (args.child(1) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkStringChar) and a2.equals(CToken.TkNumber) then begin
			if (a2.intValue < 0) or (a2.intValue >= lengthStr(a1.strValue)) then
				raise indexOutRange();
			
			eval := CSyntaxTree.Create(CToken.getString(a1.strValue[a2.intValue + 1]));
		end;
		
		if eval = NIL then
			raise illegalType('string and number', a1.toString(), a2.toString);
	end;
	
	if (child(0).length = 1) and child(0).child(0).data.equals(CToken.TkId) then begin
		env := CEnvironment.Create(environment);
		
		if environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true) = NIL then
			raise idNotFound((child(0).child(0) as CSyntaxTree).data.strValue);
		
		//if (environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(0) as CSyntaxTree).eval(environment).length <> args.length then
			//raise wrongNumberArg((environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(0) as CSyntaxTree).eval(environment).length - 1, args.length - 1);
		
		for i := 0 to (environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(0) as CSyntaxTree).eval(environment).length - 1 do begin
			//write((environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(0) as CSyntaxTree).eval(environment).child(i).child(0).data.strValue);
			//write(' => ');
			//writeln((args.child(i) as CSyntaxTree).eval(environment).toString());
			env.add((environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(0) as CSyntaxTree).eval(environment).child(i).child(0).data.strValue, CEnvironment.Create((args.child(i) as CSyntaxTree).eval(environment), env));
		end;
		
		//writeln('gogo power rangers');
		eval := (environment.get((child(0).child(0) as CSyntaxTree).data.strValue, true).tree.child(1) as CSyntaxTree).eval(env);
		//writeln('Resultat : ', eval.toString());
		
		if eval.data = CToken.TkReturn then begin
			eval := (eval.child(0) as CSyntaxTree).eval(env);
		end;
		
		//writeln(eval.toString());
	end;
	
	except
		raise;
	end;
	
	if eval = NIL then
		raise illegalOperation(data.strValue);
end;

function CCondition.eval(environment : CEnvironment) : CSyntaxTree;
var args : CSyntaxTree; a1, a2 : CToken;
begin
	try
	eval := self;
	if length = 2 then begin
		a1 := (child(0) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkBool) and a1.boolValue then
			eval := (child(1) as CSyntaxTree).eval(environment);
	end;
	
	if length = 3 then begin
		a1 := (child(0) as CSyntaxTree).eval(environment).data;
		
		if a1.equals(CToken.TkBool) and a1.boolValue then
			eval := (child(1) as CSyntaxTree).eval(environment);
		if a1.equals(CToken.TkBool) and not a1.boolValue then
			eval := (child(2) as CSyntaxTree).eval(environment);
	end;
	except
		raise;
	end;
end;

function CFor.eval(environment : CEnvironment) : CSyntaxTree; begin
	eval := self;
end;

function CWhile.eval(environment : CEnvironment) : CSyntaxTree;
var args, re : CSyntaxTree; a1, a2 : CToken;
begin
	try
	eval := self;
	
	if length = 2 then begin
		a1 := (child(0) as CSyntaxTree).eval(environment).data;
		eval := CSyntaxTree(CToken.TkNothing);
		
		while (child(0) as CSyntaxTree).eval(environment).data.boolValue do begin
			eval := (child(1) as CSyntaxTree).eval(environment);
			
			if eval.data = CToken.TkBreak then
				break;
			if eval.data = CToken.TkReturn then begin
				break;
			end;
			//writeln(eval.toString());
		end;
	end;
	except
		raise;
	end;
end;

function CLetVar.eval(environment : CEnvironment) : CSyntaxTree; begin
	try
		eval := (child(1) as CSyntaxTree).eval(environment);
		environment.add((child(0) as CSyntaxTree).child(0).data.strValue, CEnvironment.Create(eval, environment));
	except
		raise;
	end;
end;

function CLetFunction.eval(environment : CEnvironment) : CSyntaxTree; begin
	eval := self;
end;

function CLetClass.eval(environment : CEnvironment) : CSyntaxTree; begin
	eval := self;
end;

function CReturn.eval(environment : CEnvironment) : CSyntaxTree; begin
	eval := self;
	//eval := CReturn.Create((child(0) as CSyntaxTree).eval(environment));
	//writeln(eval.toString());
end;

function CLines.eval(environment : CEnvironment) : CSyntaxTree;
var i : Integer; re : CSyntaxTree;
begin
	try
	eval := self;
	for i := 0 to length - 1 do begin
		eval := (child(i) as CSyntaxTree).eval(environment);
		
		if eval.data = CToken.TkReturn then begin
			eval := CReturn.Create((eval.child(0) as CSyntaxTree).eval(environment));
			break;
		end;
		
		if eval.data = CToken.TkBreak then	break;
		if eval.data = CToken.TkContinue then	break;
	end;
	
	except
		raise;
	end;
end;

function CPrint.eval(environment : CEnvironment) : CSyntaxTree;
var a : CToken;
begin
	try
	eval := NIL;
	a := (child(0) as CSyntaxTree).eval(environment).data;
	
	//write('>>');
	if a.equals(CToken.TkNumber) then begin
		writeln(a.intValue);
		eval := self;
	end;
	
	if a.equals(CToken.TkFloat) then begin
		writeln(a.floatValue);
		eval := self;
	end;
	
	if a.equals(CToken.TkBool) and a.boolValue then begin
		writeln('true');
		eval := self;
	end;
	
	if a.equals(CToken.TkBool) and (not a.boolValue) then begin
		writeln('false');
		eval := self;
	end;
	
	if a.equals(CToken.TkStringChar) then begin
		writeln(a.strValue);
		eval := self;
	end;
	except
		raise;
	end;
	
	if eval = NIL then begin
		raise unwritable();
	end;
end;

function CVariable.eval(environment : CEnvironment) : CSyntaxTree;
var env : CEnvironment;
begin
	try
	env := environment.get((child(0) as CSyntaxTree).eval(environment).data.strValue, true);
	if env = NIL then begin
		raise idNotFound((child(0) as CSyntaxTree).eval(environment).data.strValue);
	end
	else begin
		//writeln(toString(), env.tree.toString());
		eval := env.tree;
	end;
	except
		raise;
	end;
end;

end.
