unit UParseResult;

{$mode objfpc}

interface

uses SysUtils, Classes, UEnvironment;

type CParseResult = class(TObject)
	private
		fTree : CSyntaxTree;
		fSize : integer;
	
	public
		constructor Create(aTree : CSyntaxTree; aSize : integer);
		
		property tree	: CSyntaxTree	read fTree	write fTree;
		property size	: integer		read fSize	write fSize;
end;

implementation

constructor CParseResult.Create(aTree : CSyntaxTree; aSize : integer);
begin
	fTree := aTree;
	fSize := aSize;
end;

end.
