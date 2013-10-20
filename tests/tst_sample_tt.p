/*
 * This program demonstrates ProUnit usage with temp-table comparison.
 * It's very similar to tst_sample_tt_except.p but in this version, all fields are taken into account.
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
DEFINE VARIABLE dtNow AS DATETIME.



/*------------------------------------------------------------------------------
Some code to initialize the environment or database before running the test.
------------------------------------------------------------------------------*/
PROCEDURE initialize:
    DEFINE  VARIABLE    i         AS INTEGER      NO-UNDO.
    DEFINE  VARIABLE    iCount    AS INTEGER      NO-UNDO   INITIAL 5.


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
                ttAlpha.dtFieldC    = dtNow.
        VALIDATE ttAlpha.
        RELEASE ttAlpha.

        CREATE  ttBeta.
        ASSIGN  ttBeta.cFieldA      = "Content #" + STRING(iCount - i)
                ttBeta.iFieldB      = iCount - i
                ttBeta.dtFieldC     = dtNow.
        VALIDATE ttBeta.
        RELEASE ttBeta.

        CREATE  ttGamma.
        ASSIGN  ttGamma.cFieldA     = "Content #" + STRING(iCount + 1 - i)
                ttGamma.iFieldB     = iCount + 1 - i
                ttGamma.dtFieldC    = dtNow.
        VALIDATE ttGamma.
        RELEASE ttGamma.

        CREATE  ttDelta.
        ASSIGN  ttDelta.cFieldA     = "Content #" + STRING(i + 1)
                ttDelta.iFieldB     = i + 1
                ttDelta.dtFieldC    = dtNow.
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
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttBeta:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT have same structure and are different
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual2_ShouldFail:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttGamma:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are equals (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual3_ShouldSuccess:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are different (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqual4_ShouldFail:
    RUN assertEqualsTT(INPUT TEMP-TABLE ttGamma:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT have same structure and are equals
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict1_ShouldSuccess:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttBeta:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT have same structure and are different
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict2_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttGamma:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are equals (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict3_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
TT don't have same structure and are different (common fields)
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrict4_ShouldFail:
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttGamma:HANDLE, INPUT TEMP-TABLE ttEpsilon:HANDLE, INPUT "").
END.


/*------------------------------------------------------------------------------
Type conflict
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualDS1_ShouldFail:
    DEFINE VARIABLE hds AS HANDLE.
    ASSIGN hds = DATASET dsOmega1:HANDLE.
    RUN assertEqualsTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT hds, INPUT "").
END.


/*------------------------------------------------------------------------------
Type conflict
------------------------------------------------------------------------------*/
PROCEDURE testTTEqualStrictDS1_ShouldFail:
    DEFINE VARIABLE hds AS HANDLE.
    ASSIGN hds = DATASET dsOmega1:HANDLE.
    RUN assertEqualsStrictTT(INPUT TEMP-TABLE ttAlpha:HANDLE, INPUT hds, INPUT "").
END.
