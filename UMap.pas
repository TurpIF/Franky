unit UMap;

{$mode objfpc}

interface

uses SysUtils, Classes, UList, UPair;

type generic CMap<T1, T2> = class(TObject)
	private
		type TPair = specialize CPair<T1, T2>;
		type CPairList = specialize CList<TPair>;
		
		var fList : CPairList;
		
		function getLength() : Integer;
	
	protected 
		//To compare to keys. To override for TObject.equals
		function cmpKey(k1, k2 : T1) : Boolean; virtual;
		
		//To show a pair
		function pairToString(k : T1; obj : T2) : String; virtual;
	
	public
		//Create an empty map
		constructor Create();
		
		//Free all pair objects but pairs doesn't free the two objects. It must be free by user.
		destructor Destroy(); override;
		
		//Access to an object with the key
		function at(k : T1) : T2;
		
		//Access to a key with the list indice
		function key(n : Integer) : T1;
		
		//Add a new element
		procedure append(k : T1; obj : T2);
		
		//To debug
		function toString() : AnsiString; override;
		
		//property to get the size of the map
		property length	: Integer	read getLength;
end;

implementation

//Create an empty map
constructor CMap.Create();
begin
	inherited;
	fList := CPairList.Create;
end;

//Free all pair objects but pairs doesn't free the two objects. It must be free by user.
destructor CMap.Destroy();
var i : Integer;
begin
	inherited;
	for i := 0 to fList.length - 1 do
		fList.at(i).Free;
end;

function CMap.getLength() : Integer; begin
	getLength := fList.length;
end;

//To show a pair
function CMap.pairToString(k : T1; obj : T2) : String; begin
	pairToString := 'key : obj';
end;

//To compare to keys. To override for TObject.equals
function CMap.cmpKey(k1, k2 : T1) : Boolean; begin
	cmpKey := k1 = k2;
end;

//Access to an object with the key
function CMap.at(k : T1) : T2;
var i : Integer;
begin
	at := NIL;
	for i := 0 to fList.length - 1 do begin
		if cmpKey(fList.at(i).first, k) then begin
			at := fList.at(i).second;
			break;
		end;
	end;
end;

//Access to a key with the list indice
function CMap.key(n : Integer) : T1;
begin
	try
		key := fList.at(n).first;
	except
		raise;
	end;
end;

//Add a new element
procedure CMap.append(k : T1; obj : T2);
var i : integer;
begin
	if at(k) <> NIL then begin
		for i := 0 to fList.length - 1 do begin
			if cmpKey(fList.at(i).first, k) then begin
				fList.at(i).second := obj;
				break;
			end;
		end;
	end
	else
		fList.append(TPair.Create(k, obj));
end;

//To debug
function CMap.toString() : AnsiString;
var i : integer;
begin
	toString := '{';
	for i := 0 to fList.length - 1 do begin
		toString := toString + pairToString(fList.at(i).first, fList.at(i).second);
		if i <> fList.length - 1 then	toString := toString + ', ';
	end;
	toString := toString + '}';
end;

end.
