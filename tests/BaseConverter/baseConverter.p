PROCEDURE intToBinary:
    DEFINE INPUT PARAMETER iValue       AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER binString   AS CHARACTER    NO-UNDO.

    RUN genericIntConverter (iValue, 2, OUTPUT binString).
END.

PROCEDURE binaryToInt:
    DEFINE INPUT PARAMETER cBinary      AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iResult     AS INTEGER      NO-UNDO.

    RUN genericStringConverter (INPUT cBinary, 2, OUTPUT iResult).
END.

PROCEDURE intToHex:
    DEFINE INPUT PARAMETER iValue       AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER hexString   AS CHARACTER    NO-UNDO.

    RUN genericIntConverter (iValue, 16, OUTPUT hexString).

END.

PROCEDURE hexToInt:
    DEFINE INPUT PARAMETER cHex      AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER iResult     AS INTEGER      NO-UNDO.
    
    RUN genericStringConverter (INPUT cHex, 16, OUTPUT iResult).
END.


PROCEDURE genericIntConverter:
    DEFINE INPUT PARAMETER iValue       AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER iBase        AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER cString     AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE iResting            AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cResting            AS CHARACTER    NO-UNDO.

    DO WHILE iValue > 0:
        iResting = iValue MOD iBase.

        CASE iResting:
            WHEN 10 THEN cResting = "a".
            WHEN 11 THEN cResting = "b".
            WHEN 12 THEN cResting = "c".
            WHEN 13 THEN cResting = "d".
            WHEN 14 THEN cResting = "e".
            WHEN 15 THEN cResting = "f".
            OTHERWISE
                cResting = STRING(iResting).
        END CASE.
        
        cString = cResting + cString.
        iValue = (iValue - iResting) / iBase.
    END.

END.

PROCEDURE genericStringConverter:
    DEFINE INPUT PARAMETER cString      AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER iBase        AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER iResult     AS INTEGER      NO-UNDO.

    DEFINE VARIABLE iPos                AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iValue              AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cValue              AS CHARACTER    NO-UNDO.

    DO iPos = LENGTH(cString) TO 1 BY -1:
        cValue = SUBSTRING(cString, iPos, 1).

        CASE cValue:
            WHEN "a" THEN iValue = 10.
            WHEN "b" THEN iValue = 11.
            WHEN "c" THEN iValue = 12.
            WHEN "d" THEN iValue = 13.
            WHEN "e" THEN iValue = 14.
            WHEN "f" THEN iValue = 15.
            OTHERWISE
                iValue = INTEGER(cValue).
        END CASE.

        ASSIGN
            iValue = iValue * EXP(iBase, (LENGTH(cString) - iPos))
            iResult = iResult + iValue.
    END.
END.
