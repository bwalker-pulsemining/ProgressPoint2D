
CLASS SomeQueryHandler :
	DEF VAR _formData AS StringSymbolTable.
	
	CONSTRUCTOR PUBLIC SomeQueryHandler (INPUT formData AS STRINGSYMBOLTABLE):
		_formData  = formData.
	END CONSTRUCTOR.
	
	METHOD PUBLIC STRINGSYMBOLTABLE GetFormData()  :
		return _formData.
	END METHOD.
END CLASS.