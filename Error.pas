unit Error;

//Error manager
//All error used in the program

//TO DO : remplacer les fonctions par des classes polymorphes de Exception pour pouvoir avoir une gestion plus fine et plus correct des erreurs.

{$mode objfpc}

interface

uses SysUtils;

function illegalChar(char : String) : Exception;
function expectedToken(token : String) : Exception;
function expectedTokenFound(token, found : String) : Exception;
function nothingExpectedFound(found : String) : Exception;

function illegalOperation(found : String) : Exception;
function wrongNumberArg(expected, found : integer) : Exception;
function illegalCast(expected, found : String) : Exception;
function illegalType(expected, foundL, foundR : String) : Exception;
function illegalType(expected, found : String) : Exception;
function illegalType(found : String) : Exception;
function divideByZero() : Exception;
function unwritable() : Exception;
function idNotFound(id : String) : Exception;
function indexOutRange() : Exception;

implementation

function illegalChar(char : String) : Exception; begin
	illegalChar := Exception.Create('Lexer Error : illegal character "' + char + '"');
end;

function expectedToken(token : String) : Exception; begin
	expectedToken := Exception.Create('Syntax Error : "' + token + '" expected but nothing found');
end;

function expectedTokenFound(token, found : String) : Exception; begin
	expectedTokenFound := Exception.Create('Syntax Error : "' + token + '" expected but "' + found + '" found');
end;

function nothingExpectedFound(found : String) : Exception; begin
	nothingExpectedFound := Exception.Create('Syntax Error : nothing expected but "' + found + '" found');
end;

function illegalOperation(found : String) : Exception; begin
	illegalOperation := Exception.Create('Illegal operation : operation `' + found + '` found but not supported');
end;

function wrongNumberArg(expected, found : integer) : Exception; begin
	wrongNumberArg := Exception.Create('Syntax Error : wrong number of parameters specified. ' + IntToStr(expected) + ' expected but ' + IntToStr(found) + ' given');
end;

function illegalCast(expected, found : String) : Exception; begin
	illegalCast := Exception.Create('Illegal Cast : impossible to cast `' + found + '` to ' + expected);
end;

function illegalType(expected, foundL, foundR : String) : Exception; begin
	illegalType := Exception.create('Illegal Type : type given are not supported by the function. `' + expected + '` expected but (`' + foundL + '`, `' + foundR + '`) given');
end;

function illegalType(expected, found : String) : Exception; begin
	illegalType := Exception.create('Illegal Type : type given are not supported by the function. `' + expected + '` expected but `' + found + '` given');
end;

function illegalType(found : String) : Exception; begin
	illegalType := Exception.create('Illegal Type : type given are not supported by the function. `' + found + '` given');
end;

function divideByZero() : Exception; begin
	divideByZero := Exception.Create('Illegal operation : divide by zero');
end;

function unwritable() : Exception; begin
	unwritable := Exception.create('Can''t read or write variables of this type');
end;

function idNotFound(id : String) : Exception; begin
	idNotFound := Exception.Create('Identifier not found `' + id + '`');
end;

function indexOutRange() : Exception; begin
	indexOutRange := Exception.Create('Index out of range');
end;

end.
