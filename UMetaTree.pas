unit UMetaTree;

{$mode objfpc}

interface

uses SysUtils, Classes, UList, UMap;

type CMetaTree = class;

//Associate string to a meta tree for the grammar rules
type CStringMetaTreeMap = class(specialize CMap<String, CMetaTree>)
	protected
		function pairToString(k : String; obj : CMetaTree) : String; override;
end;

type CMetaTreeList = class(specialize CList<CMetaTree>)
	protected
		function nodeToStr(node : TPNode) : String; override;
end;

//Contains the grammar rules for parsing the code
type CMetaTree = class(TObject)
	private
		fData		: String;	//Name of the current rule
		fUseful		: Boolean;	//Is the current useful or not (for the SyntaxTree)
		
		fListAnd	: CMetaTreeList;	//List of other and grammar rules associated
		fListOr		: CMetaTreeList;	//List of other or grammar rules associated
		
		fDirection	: Integer;
		fListDir	: CMetaTreeList;
		fNextNDir	: CMetaTree;
		
		_fAnd : CMetaTree;
		_fOr : CMetaTree;
	
	public
		constructor Create(aData : String; aUseful : Boolean);
		destructor Destroy(); override;
		
		//Transform current rule to string
		function toString() : AnsiString; override;
		
		//Property
		property data	: String	read fData		write fData;
		property useful	: Boolean	read fUseful	write fUseful;
		
		property lsAnd	: CMetaTreeList	read fListAnd;
		property lsOr	: CMetaTreeList	read fListOr;
		
		property _and	: CMetaTree	read _fAnd;
		property _or	: CMetaTree	read _fOr;
		
		property direction	: Integer		read fDirection;
		property listDir	: CMetaTreeList	read fListDir;
		property nextNDir	: CMetaTree		read fNextNDir;
end;

//To write a grammer rule easily with only string for the grammar name, * for and, + for or, - to get this on SyntaxTree
//Exemple : A := -'B' * (-'C' + 'D') means A ::= B and (C or D) with B and C in the SyntaxTree
function meta(p : String) : CMetaTree;
function et(tr1, tr2 : CMetaTree) : CMetaTree;
function ou(tr1, tr2 : CMetaTree) : CMetaTree;

operator :=(p : String) f : CMetaTree;
operator -(tr : CMetaTree) f : CMetaTree;
operator +(tr1 : CMetaTree; tr2 : CMetaTree) f : CMetaTree;
operator *(tr1 : CMetaTree; tr2 : CMetaTree) f : CMetaTree;

implementation

function CStringMetaTreeMap.pairToString(k : String; obj : CMetaTree) : String; begin
	pairToString := k + ' : ' + obj.toString();
end;

function CMetaTreeList.nodeToStr(node : TPNode) : String; begin
	nodeToStr := node^.data.toString();
end;

constructor CMetaTree.Create(aData : String; aUseful : Boolean); begin
	inherited Create();
	
	fData := aData;
	fUseful := aUseful;
	
	fListAnd := CMetaTreeList.Create();
	fListOr := CMetaTreeList.Create();
	
	//_fAnd := NIL;
	//_fOr := NIL;
	
	
	fNextNDir := NIL;
end;

destructor CMetaTree.Destroy(); begin
	inherited;
	
	fListAnd.Free();
	fListOr.Free();
	
	fListAnd := NIL;
	fListOr := NIL;
end;

function CMetaTree.toString() : AnsiString;
var s1, s2 : String;
begin
	toString := '';
	{if (_fOr = NIL) and (_fAnd = NIL) then		toString := fData;
	if (_fOr <> NIL) and (_fAnd = NIL) then		toString := '[' + fData + ', ' + _fOr.toString() + ']';
	if (_fOr = NIL) and (_fAnd <> NIL) then		toString := fData + ' ; ' + _fAnd.toString();
	if (_fOr <> NIL) and (_fAnd <> NIL) then	toString := '[' + fData + ', ' + _fOr.toString() + ']' + ' ; ' + _fAnd.toString();}
	
	//toString := '(' + fData + ' ; ' + _fOr.toString() + ' ; ' ')';
	
	s1 := fListOr.toString();
	s1 := copy(s1, 2, length(s1) - 2);
	
	s2 := fListAnd.toString();
	s2 := copy(s2, 2, length(s2) - 2);
	
	if fListAnd.length <> 0 then
		toString := 'and (' + s2 + ')';
	if fListOr.length <> 0 then
		toString := 'or (' + s1 + ')';
	if (fListAnd.length = 0) and (fListOr.length = 0) then
		toString := fData;
	{toString := '(';
	if fListAnd.length <> 0 then begin
		if fListOr.length <> 0 then	toString := toString + '[' + fData + ', ' + s1 + ']'
		else						toString := toString + fData;
		toString := toString + ' ; ' + s2;
	end
	else begin
		if fListOr.length <> 0 then	toString := toString + '[' + fData + ', ' + s1 + ']'
		else						toString := toString + fData;
	end;
	toString := toString + ') ';}
end;

function meta(p : String) : CMetaTree; begin
	meta := p;
end;

function et(tr1, tr2 : CMetaTree) : CMetaTree;
var pTemp : CMetaTree;
begin
	{pTemp := tr1;
	while pTemp._fAnd <> NIL do	pTemp := pTemp._fAnd;
	pTemp._fAnd := tr2;
	et := tr1;}
	
	et := '_';
	et.lsAnd.append(tr1);
	et.lsAnd.append(tr2);
end;

function ou(tr1, tr2 : CMetaTree) : CMetaTree;
var pTemp : CMetaTree;
begin
	{if tr1._fAnd = NIL then begin
		pTemp := tr1;
		while pTemp._fOr <> NIL do	pTemp := pTemp._fOr;
		pTemp._fOr := tr2;
		ou := tr1;
	end
	else begin
		
	end;}
	
	ou := '_';
	ou.lsOr.append(tr1);
	ou.lsOr.append(tr2);
end;

operator :=(p : String) f : CMetaTree; begin
	f := CMetaTree.Create(p, true);
end;

operator -(tr : CMetaTree) f : CMetaTree; begin
	tr.Useful := true;
	f := tr;
end;

operator *(tr1 : CMetaTree; tr2 : CMetaTree) f : CMetaTree; begin
	f := et(tr1, tr2);
end;

operator +(tr1 : CMetaTree; tr2 : CMetaTree) f : CMetaTree; begin
	f := ou(tr1, tr2);
end;

end.
