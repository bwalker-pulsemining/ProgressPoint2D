DEFINE VARIABLE hConverter      AS HANDLE       NO-UNDO.

PROCEDURE initialize:
    RUN baseConverter.p PERSISTENT SET hConverter.
END.
    
PROCEDURE testIntToBin:
    DEFINE VARIABLE cResult         AS CHARACTER        NO-UNDO.

    /* Test an odd number */
    RUN intToBinary IN hconverter(INPUT 137,
                                  OUTPUT cResult).
    RUN AssertEqualsChar(cResult, "10001001").

    /* Test an even number */
    RUN intToBinary IN hConverter (INPUT 1200,
                                   OUTPUT cResult).
    RUN AssertEqualsChar(cResult, "10010110000").
END.

PROCEDURE testBinToInt:
    DEFINE VARIABLE iValue          AS INTEGER          NO-UNDO.

    RUN binaryToInt IN hConverter (INPUT "10001001",
                                   OUTPUT iValue).
    RUN AssertEqualsInt(iValue, 137).
END.

PROCEDURE testIntToHex:
    DEFINE VARIABLE cResult         AS CHARACTER        NO-UNDO.

    RUN intToHex IN hconverter(INPUT 137,
                                  OUTPUT cResult).
    RUN AssertEqualsChar(cResult, "89").


    RUN intToHex IN hconverter(INPUT 1200,
                                  OUTPUT cResult).
    RUN AssertEqualsChar(cResult, "4B0").
END.

PROCEDURE testHexToInt:
    DEFINE VARIABLE iValue          AS INTEGER          NO-UNDO.

    RUN hexToInt IN hConverter (INPUT "4b0",
                                OUTPUT iValue).
    RUN assertEqualsInt(iValue, 1200).
END.

PROCEDURE dispose:
    IF VALID-HANDLE(hConverter) THEN
        DELETE OBJECT hConverter.
END.
