unit UObjectList;

{$mode objfpc}

interface

uses SysUtils, Classes, UList;

type CObjectList = class(specialize CList<TObject>)
	protected
		function nodeToStr(node : TPNode) : String; override;
end;

implementation

function CObjectList.nodeToStr(node : TPNode) : String; begin
	nodeToStr := node^.data.toString();
end;

end.
