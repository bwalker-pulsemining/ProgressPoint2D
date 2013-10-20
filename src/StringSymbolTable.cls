USING Progress.Lang.*.

CLASS StringSymbolTable :
	DEF TEMP-TABLE _ttSymbols
		FIELD fKey AS CHARACTER 
		FIELD fValue AS CHARACTER.
	
	DEF VAR _countOfKeys AS INT.
	
	METHOD PUBLIC VOID Put(INPUT key AS CHAR, INPUT val AS CHAR)  :
		CREATE _ttSymbols.
		
		ASSIGN 	_ttSymbols.fKey = key 
				_ttSymbols.fVal = val.
				
		_countOfKeys = _countOfKeys + 1.		
	END METHOD.
	
	METHOD PUBLIC VOID Remove(INPUT key AS CHAR)  :
		FIND FIRST _ttSymbols WHERE _ttSymbols.fKey = key NO-ERROR.
		
		IF AVAIL _ttSymbols THEN DELETE _ttSymbols NO-ERROR.
		
		_countOfKeys = _countOfKeys - 1.
	END METHOD.
	
	METHOD PUBLIC LOGICAL Contains(INPUT key AS CHAR) :
		return CAN-FIND (FIRST _ttSymbols WHERE _ttSymbols.fKey = key).
	END METHOD.
	
	METHOD PUBLIC CHAR Get(INPUT key AS CHAR) :
		FIND FIRST _ttSymbols WHERE _ttSymbols.fKey = key NO-ERROR.
		return (IF AVAIL _ttSymbols THEN _ttSymbols.fValue ELSE ?).   
	END METHOD.
	
	METHOD PUBLIC INT CountOfKeys() :
		return _countOfKeys.
	END METHOD.
	
	METHOD OVERRIDE PUBLIC CHAR ToString() : 
		return "something cool".
	END METHOD.
END CLASS.
