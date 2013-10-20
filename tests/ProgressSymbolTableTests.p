
USING Progress.Lang.*.

PROCEDURE testCanNewUpASymbolTable:
    DEFINE VARIABLE mySymbolTable AS StringSymbolTable.
	
	mySymbolTable = NEW StringSymbolTable().
	
	RUN assertEqualsInt(2, 2).
END.

PROCEDURE testWhenAKeyIsAdded_ThenTheSymbolTableShouldContainTheKey:
    DEFINE VARIABLE st AS StringSymbolTable.
	
	st = NEW StringSymbolTable().
	
	st:Put('MyKey','MyVal').
	
	RUN assertTrue(st:Contains('MyKey')).
	RUN assertEqualsChar('MyVal', st:Get('MyKey')).
END.

PROCEDURE testWhenGettingAKey_GivenANewSymbolTable_ThenTheValueShouldBeNull:
    DEFINE VARIABLE st AS StringSymbolTable.
	
	st = NEW StringSymbolTable().
	
	RUN assertNull(st:Get('AnyKey')).
END.

PROCEDURE testWhenGettingAKey_Given2NewSymbolTables_ThenSymbolTablesShouldBeThereOwnInstances:
    DEFINE VARIABLE st1 AS StringSymbolTable.
    DEFINE VARIABLE st2 AS StringSymbolTable.
	
	st1 = NEW StringSymbolTable().
	st2 = NEW StringSymbolTable().
	
	st1:Put('MyKey','MyVal').
	
	RUN assertEqualsChar('MyVal', st1:Get('MyKey')).
	RUN assertNull(st2:Get('MyKey')).
END.

PROCEDURE testWhenGivingASymbolTableToAQueryHandler_ThenTheQueryHandlerShouldGetTheReference:
    DEFINE VARIABLE st AS StringSymbolTable.
	DEFINE VARIABLE qh AS SomeQueryHandler.
	
	st = NEW StringSymbolTable().
	qh = NEW SomeQueryHandler(st).
	
	st:Put('MyKey','MyVar').
	
	RUN assertEqualsChar('MyVar',qh:GetFormData():Get('MyKey')).
END.

PROCEDURE testGivenANewSymbolTable_ThenThereShouldBeZeroItems:
    DEF VAR st AS StringSymbolTable.
	DEF VAR result AS INT.
	
	st = NEW StringSymbolTable().
	result = st:CountOfKeys().
	
	RUN assertEqualsInt(0,result).
END.

PROCEDURE testGivenASymbolTable_WhenWeAddAKey_ThenTheKeyCountShouldBe1:
    DEF VAR st AS StringSymbolTable.
	DEF VAR result AS INT.
	
	st = NEW StringSymbolTable().
	st:Put('AKey','SomeVal').
	
	result = st:CountOfKeys().
	
	
	RUN assertEqualsInt(1,result).
END.


PROCEDURE testGivenASymbolTable_WeShouldBeAbleToRemoveAKey_ThenTheSymbolTableShouldNotHaveThatKey:
    DEF VAR st AS StringSymbolTable.
	DEF VAR result AS INT.
	
	st = NEW StringSymbolTable().
	st:Put('AKey','SomeVal').
	st:Remove('AKey').
	
	
	
	
	RUN assertNull(st:Get('AKey')).

END.


PROCEDURE testGivenASymbolTable_WeShouldBeAbleToRemoveAKey_ThenTheCountOfKeysShouldBeReduced :
    DEF VAR st AS StringSymbolTable.
	DEF VAR result AS INT.
	
	st = NEW StringSymbolTable().
	st:Put('AKey','SomeVal').
	st:Remove('AKey').
	
	result = st:CountOfKeys().
	
	RUN assertEqualsInt(0, result).
END.



