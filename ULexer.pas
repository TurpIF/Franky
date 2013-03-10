unit ULexer;

{$mode objfpc}

interface

uses SysUtils, UToken, UStringTokenMap, Error;

//Transform a string to a list of token with the rules given
//Transform a unknow word to a identifiant
//Transform a numeric word to a number or a float
//Ignore spaces and tabulations
function lexer(code : AnsiString; rules : CStringTokenMap) : CTokenList;

implementation

//Return one word of str at n where all chars are in the alphabet
function getWord(str, alphabet : AnsiString; at : Integer) : String;
var i : Integer;
begin
	getWord := '';
	for i := at to length(str) do
		if pos(str[i], alphabet) <> 0 then	getWord := getWord + str[i]
		else	break;
end;

//Transform a string to a list of token with the rules given
//Transform a unknow word to a identifiant
//Transform a numeric word to a number or a float
//Ignore spaces and tabulations
function lexer(code : AnsiString; rules : CStringTokenMap) : CTokenList;
var n, i : Integer; word, dec : AnsiString; f : Boolean;
begin
	lexer := CTokenList.Create();
	n := 1;
	
	while n <= length(code) do begin
		//Spaces
		word := getWord(code, ' '#9#13#10, n);
		if word <> '' then begin
			n := n + length(word);
			continue;
		end;
		
		//String
		if code[n] = '''' then begin
			word := '';
			for i := n + 1 to length(code) do
				if code[i] = '''' then	break
				else	word := word + code[i];
			
			//If all the code is in the string
			if length(code) = length(word) + n then
				raise Error.expectedToken('''');
			
			lexer.append(CToken.getString(word));
			n := n + length(word) + 2;
			continue;
		end;
		
		//Number
		word := getWord(code, '0123456789', n);
		if word <> '' then
			if (length(code) >= n + length(word)) and (code[n + length(word)] = '.') then begin
				dec := getWord(code, '0123456789', n + length(word) + 1);
				lexer.append(CToken.getFloat(StrToFloat(word + ',' + dec)));
				n := n + length(word) + 2 + length(dec);
				continue;
			end
			else begin
				lexer.append(CToken.getNumber(StrToInt(word)));
				n := n + length(word);
				continue;
			end;
		
		//Keywords and identifiants
		word := getWord(code, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_', n);
		if word <> '' then
			if rules.at(word) <> NIL then begin
				lexer.append(rules.at(word));
				n := n + length(word);
				continue;
			end
			else begin
				lexer.append(CToken.getId(word, 0));
				n := n + length(word);
				continue;
			end;
		
		//Operators
		f := False;
		for i := 0 to rules.length - 1 do begin
			word := rules.key(i);
			if length(word) <= length(code) - n + 1 then
				if word = copy(code, n, length(word)) then begin
					lexer.append(rules.at(word));
					n := n + length(word);
					f := True;
					break;
				end;
		end;
		if f then continue;
		
		writeln(code, code[n]);
		raise Error.illegalChar(code[n]);
	end;
end;

end.
