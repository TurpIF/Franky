unit UList;

{$mode objfpc}

interface

uses Classes, SysUtils;

type generic CList<T> = class(TObject)
	private
		//One element of a list with the previous and the next element
		type TNode = record
			data : T;
			pPrev : ^TNode;
			pNext : ^TNode;
		end;

		type TPNode = ^TNode;
		
		var
		fHead : TPNode; //The head of the list
		fTail : TPNode; //The tail of the list
		fCursor : TPNode; //There is one cursor to avoid to range all the list everytime
		fCursorPos : Integer;
		fLength : integer;
		
		//Return the node at the position n
		function pAt(n : integer) : TPNode;
		
		//Getter for the value on the cursor
		function getCursor() : T;
		
	protected
		//To show the value of a node. Virtual to let a override
		function nodeToStr(node : TPNode) : string; virtual;
	
	public
		constructor Create();
		
		//The list don't free the elements. Elements have to be clear by user
		destructor Destroy(); override;
		
		//Return the element at the position n
		//If n is negative then it's like the begin is the end : -1 => end, -2 => end - 1, ...
		//If n is outside the range of the list then a exception is raised
		function at(n : integer) : T; virtual;
		
		//Add an element at the end of the list
		procedure append(x : T); virtual;
		
		//Add an element at the begin of the list
		procedure prepend(x : T); virtual;
		
		//Add an element at the position i
		//If i is outside the range of the list then a exception is raised
		procedure insert(x : T; i : integer); virtual;
		
		//Remove an element at the end
		procedure popTail();
		
		//Remove an element at the begin
		procedure popHead();
		
		//Remove an element at the position i
		//If i is outside the range of the list then a exception is raised
		procedure pop(i : integer);
		
		//To show all the list
		function toString() : ansistring; override;
		
		//Property to get length and cursor value
		property length: integer	read fLength;
		property cursor: T			read getCursor;
end;

implementation

constructor CList.Create();
begin
	inherited;
	
	fHead := NIL;
	fTail := NIL;
	fCursor := NIL;
	fCursorPos := 0;
	fLength := 0;
end;

//The list don't free the elements. Elements have to be clear by user
destructor CList.Destroy();
var i : integer;
begin
	inherited;
	
	for i := 0 to fLength - 1 do	popHead();
end;

//To show the value of a node. Virtual to let a override
function CList.nodeToStr(node : TPNode) : string;
begin
	nodeToStr := IntToStr(Integer(node^.data));
end;

//Getter for the value on the cursor
function CList.getCursor() : T; begin
	writeln(fCursorPos, Integer(fCursor));
	getCursor := fCursor^.data;
end;

//To show all the list
function CList.toString() : ansistring;
var pTemp : TPNode;
begin
	pTemp := fHead;
	toString := '[';
	while pTemp <> NIL do begin
		toString := toString + nodeToStr(pTemp);
		if pTemp^.pNext <> NIL then	toString := toString + ', ';
		pTemp := pTemp^.pNext;
	end;
	toString := toString + ']';
end;

//Return the node at the position n
function CList.pAt(n : integer) : TPNode;
var i : integer;
begin
	if n < 0 then			n := fLength + n;
	if n >= fLength then	raise Exception.Create('IndexError : list index out of range');
	
	if abs(fLength - 1 - n) < abs(0 - n) then begin
		if abs(fCursorPos - n) < abs(fLength - 1 - n) then begin
			pAt := fCursor;
			
			if fCursorPos < n then	for i := fCursorPos + 1 to n do pAt := pAt^.pNext
			else					for i := n + 1 to fCursorPos do pAt := pAt^.pPrev;
			fCursorPos := n;
			fCursor := pAt;
		end
		else begin
			pAt := fTail;
			for i := n + 1 to Length - 1 do	pAt := pAt^.pPrev;
		end;
	end
	else begin
		if abs(fCursorPos - n) < abs(0 - n) then begin
			pAt := fCursor;
			
			if fCursorPos < n then	for i := fCursorPos + 1 to n do pAt := pAt^.pNext
			else					for i := n + 1 to fCursorPos do pAt := pAt^.pPrev;
			fCursorPos := n;
			fCursor := pAt;
		end
		else begin
			pAt := fHead;
			for i := 1 to n do	pAt := pAt^.pNext;
		end;
	end;
end;

//Return the element at the position n
//If n is outside the range of the list then a exception is raised
function CList.at(n : integer) : T;
begin
	try
		at := pAt(n)^.data;
	except
		raise;
	end;
end;

//Add an element at the end of the list
procedure CList.append(x : T);
var pNew : TPNode;
begin
	new(pNew);
	pNew^.data := x;
	pNew^.pNext := NIL;
	
	if fTail = NIL then begin
		pNew^.pPrev := NIL;
		fHead := pNew;
		fTail := pNew;
		fCursor := pNew;
		fCursorPos := 0;
	end
	else begin
		fTail^.pNext := pNew;
		pNew^.pPrev := fTail;
		fTail := pNew;
	end;
	fLength := fLength + 1;
end;

//Add an element at the begin of the list
procedure CList.prepend(x : T);
var pNew : TPNode;
begin
	new(pNew);
	pNew^.data := x;
	pNew^.pPrev := NIL;
	
	if fTail = NIL then begin
		pNew^.pNext := NIL;
		fHead := pNew;
		fTail := pNew;
		fCursor := pNew;
		fCursorPos := 0;
	end
	else begin
		fHead^.pPrev := pNew;
		pNew^.pNext := fHead;
		fHead := pNew;
	end;
	fLength := fLength + 1;
	fCursorPos := fCursorPos + 1;
end;

//Add an element at the position i
//If i is outside the range of the list then a exception is raised
procedure CList.insert(x : T; i : integer);
var pTemp, pNew : TPNode;
begin
	try begin
		pTemp := pAt(i);
		if pTemp^.pNext = NIL then		append(x)
		else if pTemp^.pPrev = NIL then	prepend(x)
		else begin
			new(pNew);
			pNew^.data := x;
			
			pTemp^.pPrev^.pNext := pNew;
			pNew^.pPrev := pTemp^.pPrev;
			pNew^.pNext := pTemp;
			
			fLength := fLength + 1;
			if i < fCursorPos then	fCursorPos := fCursorPos + 1;
		end;
	end;
	except
		raise;
	end;
end;

//Remove an element at the end
procedure CList.popTail();
var pTemp : TPNode;
begin
	pTemp := fTail;
	fTail := pTemp^.pPrev;
	if fTail <> NIL then	fTail^.pNext := NIL;
	
	fLength := fLength - 1;
	
	dispose(pTemp);
end;

//Remove an element at the begin
procedure CList.popHead();
var pTemp : TPNode;
begin
	pTemp := fHead;
	fHead := pTemp^.pNext;
	if fHead <> NIL then	fHead^.pPrev := NIL;
	
	fLength := fLength - 1;
	
	dispose(pTemp);
end;

//Remove an element at the position i
		//If i is outside the range of the list then a exception is raised
procedure CList.pop(i : integer);
var pTemp : TPNode;
begin
	try
		pTemp := pAt(i);
		if pTemp^.pPrev = NIL then		popHead()
		else if pTemp^.pNext = NIL then	popTail()
		else begin
			pTemp^.pPrev^.pNext := pTemp^.pNext;
			pTemp^.pNext^.pPrev := pTemp^.pPrev;
			
			fLength := fLength - 1;
			
			dispose(pTemp);
		end;
	except
		raise;
	end;
end;

end.
