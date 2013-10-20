/*
 * This program demonstrates ProUnit usage with temp-table comparison.
 * It's very similar to tst_sample_tt_.p but in this version, one field is not taken into account.
 */


/*------------------------------------------------------------------------------
Overall variables
------------------------------------------------------------------------------*/
/* --- Temp tables --- */
DEFINE TEMP-TABLE ttAlpha NO-UNDO
    FIELD   cFieldA     AS CHARACTER
    FIELD   iFieldB     AS INTEGER
    FIELD   dtFieldC    AS DATETIME.

DEFINE TEMP-TABLE ttBeta      NO-UNDO      LIKE ttAlpha.
DEFINE TEMP-TABLE ttGamma     NO-UNDO      LIKE ttAlpha.
DEFINE TEMP-TABLE ttDelta     NO-UNDO      LIKE ttAlpha.

DEFINE TEMP-TABLE ttEpsilon   NO-UNDO      LIKE ttAlpha
    FIELD lFieldC       AS LOGICAL.

/* --- DataSets --- */
DEFINE DATASET dsOmega1 FOR ttAlpha, ttBeta.
DEFINE DATASET dsOmega2 FOR ttGamma, ttDelta.


/* --- Variables --- */
DEFINE VARIABLE dtNow           AS DATETIME.
DEFINE VARIABLE iIncrement      AS INTEGER.
DEFINE VARIABLE cExceptFields   AS CHARACTER    INITIAL "dtFieldC".



/*------------------------------------------------------------------------------
Some code to initialize the environment or database before running the test.
------------------------------------------------------------------------------*/
PROCEDURE initialize:
    DEFINE  VARIABLE    i         AS INTEGER      NO-UNDO.
    DEFINE  VARIABLE    lIgnore   AS LOGICAL      NO-UNDO.
    DEFINE  VARIABLE    iCount    AS INTEGER      NO-UNDO   INITIAL 5.


    /* --- Ask to compare all fields or use exception on field(s) --- */
    MESSAGE "OK to use field " + QUOTER(cExceptFields) + " as except field?" SKIP(1)
            "- yes --> this field will be ignored from temp-table comparisons" SKIP
            "- no  --> this field will be taken into account in temp-table comparisons"
            SET lIgnore VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "What kinf of comparison?".
    IF NOT lIgnore THEN
        ASSIGN cExceptFields = "".

    /* --- Datetime initialization --- */
    ASSIGN dtNow = NOW.

    /* --- Build sample TT --- */
    EMPTY TEMP-TABLE ttAlpha.
    EMPTY TEMP-TABLE ttBeta.
    EMPTY TEMP-TABLE ttGamma.
    EMPTY TEMP-TABLE ttDelta.
    EMPTY TEMP-TABLE ttEpsilon.

    DO i = 0 TO iCount :
        CREATE  ttAlpha.
        ASSIGN  ttAlpha.cFieldA     = "Content #" + STRING(i)
                ttAlpha.iFieldB     = i
                ttAlpha.dtFieldC    = ADD-INTERVAL(dtNow, iIncrement, "days")
                iIncrement          = iIncrement + 1.
        VALIDATE ttAlpha.
        RELEASE ttAlpha.

        CREATE  ttBeta.
        ASSIGN  ttBeta.cFieldA      = "Content #" + STRING(iCount - i)
                ttBeta.iFieldB      = iCount - i
                ttBeta.dtFieldC     = ADD-INTERVAL(dtNow, iIncrement, "days")
                iIncrement          = iIncrement + 1.
        VALIDATE ttBeta.
        RELEASE ttBeta.

        CREATE  ttGamma.
        ASSIGN  ttGamma.cFieldA     = "Content #" + STRING(iCount + 1 - i)
                ttGamma.iFieldB     = iCount + 1 - i
                ttGamma.dtFieldC    = ADD-INTERVAL(dtNow, iIncrement, "days")
                iIncrement          = iIncrement + 1.
        VALIDATE ttGamma.
        RELEASE ttGamma.

        CREATE  ttDelta.
        ASSIGN  ttDelta.cFieldA     = "Content #" + STRING(i + 1)
                ttDelta.iFieldB     = i + 1
                ttDelta.dtFieldC    = ADD-INTERVAL(dtNow, iIncrement, "days")
                iIncrement          = iIncrement + 1.
        VALIDATE ttDelta.
        RELEASE ttDelta.

    END.
    TEMP-TABLE ttEpsilon:COPY-TEMP-TABLE(TEMP-TABLE ttAlpha:HANDLE, FALSE, FALSE, TRUE).
    /* ttAlpha = ttBeta  |  ttBeta <> ttGamma  |  ttGamma = ttDelta | ttEpsilon ~= ttAlpha (but there is one more field) */
END.


/*------------------------------------------------------------------------------
Some code run before every test to reset internal states, if needed.
------------------------------------------------------------------------------*/
PROCEDURE setUp:
END.


/*------------------------------------------------------------------------------
Some code run after a test to restore, log or something else.
------------------------------------------------------------------------------*/
PROCEDURE tearDown:
END.


/*------------------------------------------------------------------------------
Dispose everything, free resource, close files, disconnect databases, etc.
------------------------------------------------------------------------------*/
PROCEDURE dispose:
    EMPTY TEMP-TABLE ttAlpha.
    EMPTY TEMP-TABLE ttBeta.
    EMPTY TEMP-TABLE ttGamma.
    EMPTY TEMP-TABLE ttDelta.
    EMPTY TEMP-TABLE ttEpsilon.
END.




/*------------------------------------------------------------------------------
TT have same structure and are equals
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual1_ShouldSuccess:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttBeta:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT have same structure and are different
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual2_ShouldFail:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttGamma:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are equals (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual3_ShouldSuccess:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are different (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual4_ShouldFail:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttGamma:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT have same structure and are equals
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict1_ShouldSuccess:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttBeta:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT have same structure and are different
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict2_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttGamma:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are equals (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict3_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are different (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict4_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttGamma:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
Type conflict
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualDS1_ShouldFail:
    DEFINE VARIABLE hds AS HANDLE.
    ASSIGN hds = DATASET dsOmega1:HANDLE.
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT hds, INPUT cExceptFields).
END.


/*------------------------------------------------------------------------------
Type conflict
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrictDS1_ShouldFail:
    DEFINE VARIABLE hds AS HANDLE.
    ASSIGN hds = DATASET dsOmega1:HANDLE.
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT hds, INPUT cExceptFields).
END.
