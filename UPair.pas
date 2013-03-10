unit UPair;

{$mode objfpc}

interface

uses SysUtils, Classes;

type generic CPair<T1, T2> = class(TObject)
	private
		fFirst	: T1;
		fSecond	: T2;
	
	public
		constructor Create(aFirst : T1; aSecond : T2);
		
		//There is no destructor. Object used have to be free by user
		
		//variable getters and setters by property
		property first	: T1	read fFirst		write fFirst;
		property second	: T2	read fSecond	write fSecond;
end;

implementation

constructor CPair.Create(aFirst : T1; aSecond : T2); begin
	fFirst := aFirst;
	fSecond := aSecond;
end;

end.
