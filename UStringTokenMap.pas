unit UStringTokenMap;

{$mode objfpc}

interface

uses SysUtils, Classes, UMap, UToken;

//Associate string to token for the lexer
type CStringTokenMap = class(specialize CMap<String, CToken>)
	protected
		function pairToString(k : String; obj : CToken) : String; override;
end;

implementation

//Override the toString function to show something
function CStringTokenMap.pairToString(k : String; obj : CToken) : String; begin
	pairToString := k + ' : ' + obj.toString();
end;

end.
