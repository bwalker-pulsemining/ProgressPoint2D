
{prounit/plugins/builtin-DataPool/datapool.i}

PROCEDURE testSample:
    DEFINE VARIABLE iLength         AS INTEGER       NO-UNDO.

    DO WHILE DataPoolIterator("/"):
        iLength = LENGTH(DataPoolRecord("/name")).
        RUN assertEqualsInt (DataPoolRecord("/length"), iLength).
    END.
END.

PROCEDURE testChildren:
    DEFINE VARIABLE iCounter       AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iItemCounter   AS INTEGER       NO-UNDO.

    iItemCounter = 0.
    DO WHILE DataPoolIterator("/"):
        iCounter = 0.
        
        DO WHILE DataPoolIterator("/items"):
            ASSIGN
                iItemCounter = iItemCounter + 1
                iCounter     = iCounter     + 1.
    
            RUN assertEqualsInt(DataPoolRecord("/items/id"), iItemCounter).
        END.
        
        RUN assertEqualsInt (iCounter, INTEGER(DataPoolRecord("/counter"))).
    END.
END.