/* */

FUNCTION DataPoolIterator RETURNS LOGICAL
    (INPUT cPath        AS CHARACTER):
        
    DEFINE VARIABLE lAvailable     AS LOGICAL    NO-UNDO.
    RUN __Datapool.iterate(INPUT cPath,
                           OUTPUT lAvailable).
    RETURN lAvailable.
END.

FUNCTION DataPoolRecord RETURNS CHARACTER
    (INPUT cPath        AS CHARACTER):
    DEFINE VARIABLE cValue        AS CHARACTER   NO-UNDO.
    
    RUN __Datapool.getField(INPUT cPath,
                            OUTPUT cValue).
    RETURN cValue.
END.

FUNCTION DataPoolReset RETURNS LOGICAL
    (INPUT cPath        AS CHARACTER):
    RUN __Datapool.reset (INPUT cPath).
END.