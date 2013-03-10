unit UTree;

{$mode objfpc}

interface

uses Classes, SysUtils, UObjectList;

type generic CTree<T> = class(TObject)
	private
		var fLsChild	: CObjectList;	//List of child
		var fData		: T;			//Value on this node
		
		function getLength() : Integer;

	public
		constructor Create(aData : T);
		
		//Don't destroy data. User has to free this
		destructor Destroy(); override;
		
		//Get a child
		function child(n : Integer) : CTree;
		
		//Add a child in the list
		procedure addChild(c : CTree);
		
		//To show the tree
		function toString() : AnsiString; override;
		
		//Property
		property data	: T			read fData		write fData;
		property length	: Integer	read getLength;
end;

implementation

constructor CTree.Create(aData : T); begin
	inherited Create();
	
	fLsChild := CObjectList.Create();
	fData := aData;
end;

//Don't destroy data. User has to free this
destructor CTree.Destroy(); begin
	inherited;
	
	fLsChild.Free();
	
	fLsChild := NIL;
end;

//Get a child
function CTree.child(n : Integer) : CTree; begin
	try
		child := fLsChild.at(n) as CTree;
	except
		raise;
	end;
end;

//Add a child in the list
procedure CTree.addChild(c : CTree); begin
	fLsChild.append(c);
end;

//To show the tree
function CTree.toString() : AnsiString; begin
	toString := '{' + 'tree' + ' : ' + fLsChild.toString() + '}';
end;

//Get the number of children
function CTree.getLength() : Integer; begin
	getLength := fLsChild.length;
end;

end.

