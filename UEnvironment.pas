unit UEnvironment;

{$mode objfpc}

interface

uses SysUtils, Classes, UMap, UTree, UToken;

type CEnvironment = class;

type CSyntaxTree = class(specialize CTree<CToken>)
	public
		//To show the tree
		function toString() : AnsiString; override;
		
		//Evaluation of an expression
		function eval(environment : CEnvironment) : CSyntaxTree; virtual;
end;

type CStringEnvironmentMap = class(specialize CMap<String, CEnvironment>) end;

type CEnvironment = class(TObject)
	private
		fParent	: CEnvironment; //Parent environment
		fLsId	: CStringEnvironmentMap; //Link id of variable to his environment
		fTree	: CSyntaxTree; //Tree of the environment
			
	public
		constructor Create(); //Create a root
		constructor Create(parent : CEnvironment); //Create a sub environment
		constructor Create(tree : CSyntaxTree; parent : CEnvironment); //Create a sub environment with a tree
		
		//Don't destroy any tree. Only environment are destroy
		destructor Destroy(); override;
		
		//Return the environment of the variable. If there is not variable, return NIL.
		//Can check if the variable exist in the parent
		function get(id : String; checkInParent : Boolean) : CEnvironment;
		
		//Add a new entry in the list
		procedure add(id : string; environment : CEnvironment);
		
		//Getters
		property tree	: CSyntaxTree	read fTree;
end;

implementation

//To show the tree
function CSyntaxTree.toString() : AnsiString; begin
	toString := '{' + data.toString() + ' : ' + fLsChild.toString() + '}';
end;

//Evaluation of an expression
function CSyntaxTree.eval(environment : CEnvironment) : CSyntaxTree; begin
	eval := Self;
end;

//Create a root
constructor CEnvironment.Create(); begin
	fParent := NIL;
	fTree := NIL;
	
	fLsId := CStringEnvironmentMap.Create();
end;

//Create a sub environment
constructor CEnvironment.Create(parent : CEnvironment); begin
	Create();
	
	fParent := parent;
end;

//Create a sub environment with a tree
constructor CEnvironment.Create(tree : CSyntaxTree; parent : CEnvironment); begin
	Create(parent);
	fTree := tree;
end;

//Don't destroy any tree. Only environment are destroy
destructor CEnvironment.Destroy();
var i : integer;
begin
	for i := 0 to fLsId.length - 1 do
		fLsId.at(fLsId.key(i)).Free();
	fLsId.Free();
end;

//Return the environment of the variable. If there is not variable, return NIL.
//Can check if the variable exist in the parent
function CEnvironment.get(id : String; checkInParent : Boolean) : CEnvironment; begin
	get := fLsId.at(id);
	
	if (get = NIL) and checkInParent and (fParent <> NIL) then
		get := fParent.get(id, checkInParent);
end;

//Add a new entry in the list
procedure CEnvironment.add(id : string; environment : CEnvironment); begin
	fLsId.append(id, environment);
end;

end.

