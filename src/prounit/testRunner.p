/*******************************************************************************
**
**     Program: testRunner.p
** Description: Kernel of PUnit. Runs all the tests and collect the results.
**      Author: Flavio Eduardo de Córdova
**     Created: 2004/12/30
**
********************************************************************************
**
**  Revision 1.8  2013/07/xx  SMarmotte
**  - added new parameter to __internal_BufferCompare to be able to exclude some fields from temp-table comparison
**  - updated __internal_TestTTEqualityByHandles, assertEqualsTTExtended, assertEqualsTT and assertEqualsStrictTT to reflect __internal_BufferCompare signature update
**  - minor code improvements
**
**  Revision 1.7  2013/06/23  SMarmotte
**  - implemented function __internal_BufferCompare to compare two buffers in a strict way (both buffer must have same fields count, name and value)
**  - added assertEqualsStrictTT
**
**  Revision 1.6  2013/05/17  SMarmotte
**  - improved assertEqualsTT: it now generates XML export of both full TT and both differences in the temporary folder
**  - implemented assertEqualsDataSet (not yet fully tested)
**  - renamed TT comparison function to __internal_TestTTEqualityByHandles and added it as private function
**
**  Revision 1.5  2013/05/05  SMarmotte
**  - added assertEqualsTT to test equality of 2 TT (for OE 10+)
**    - differences are exported to XML file in the temporary directory
**  - added assertEqualsDataSet to test equality of 2 DataSets (for OE 10+)
**  - minor code improvements
**
**  Revision 1.4  2006/05/24 22:57:46  fcordova
**  *** empty log message ***
**
**  Revision 1.3  2006/03/12 11:17:49  fcordova
**  Plugins Improvements.
**
**  Revision 1.2  2005/12/11 23:59:43  fcordova
**  *** empty log message ***
**
**  Revision 1.1  2005/11/01 01:09:50  fcordova
**  *** empty log message ***
**
**  Revision 0.9  2005/09/17 03:33:36  fcordova
**  - expectError, expectedStop and expectQuit created.
**
**  Revision 0.7  2005/08/27 19:57:04  fcordova
**  - Added new assertMethod
**  - small bugs fixed
**
**  Revision 0.6  2005/07/04 12:47:41  fcordova
**  - Batch Executor
**
**  Revision 0.5  2005/06/03 22:31:12  fcordova
**  - small bugs...
**
**  Revision 0.4  2005/05/02 20:36:26  fcordova
**  - New User interface.
**
**  Revision 0.3  2005/04/26 12:00:14  fcordova
**  - New User Interface.
**
**  Revision 0.2  2005/03/04 22:12:41  fcordova
**  - Tag test.
**
****************************************************************************/



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttUnitTest
    FIELD seq               AS INTEGER
    FIELD name              AS CHARACTER
    FIELD result            AS LOGICAL INITIAL ?
    FIELD txtMessage        AS CHARACTER
    FIELD firstAssertFailed AS INTEGER INITIAL 0
    FIELD totalTime         AS INTEGER
    FIELD runnable          AS LOGICAL INITIAL TRUE
    INDEX principal IS PRIMARY seq.

DEFINE TEMP-TABLE ttListeners NO-UNDO
    FIELD hListener     AS HANDLE
    FIELD priority      AS INTEGER
    INDEX priority priority.

DEFINE TEMP-TABLE ttEvent
    FIELD eventNum      AS INTEGER
    FIELD eventMessage  AS CHARACTER
    INDEX main is PRIMARY UNIQUE eventNum.



/* -------------------------------------------------------------------------- */
/* VARIALBES                                                                  */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE iEventNumber        AS INTEGER      NO-UNDO.
DEFINE VARIABLE iAssertCounter      AS INTEGER      NO-UNDO.
DEFINE VARIABLE iFirstAssertFailed  AS INTEGER      NO-UNDO.

DEFINE VARIABLE lExpectedError      AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lExpectedStop       AS LOGICAL      NO-UNDO.
DEFINE VARIABLE lExpectedQuit       AS LOGICAL      NO-UNDO.

DEFINE VARIABLE cTempFolder         AS CHARACTER    NO-UNDO.



/* -------------------------------------------------------------------------- */
/* Main-Block (program entry point)                                           */
/* -------------------------------------------------------------------------- */

/* Sets the temp-table scope to the program */
FIND FIRST ttUnitTest NO-LOCK NO-ERROR.

/* Prepare path to temp folder */
ASSIGN cTempFolder = OS-GETENV("TEMP").
IF cTempFolder = ? OR cTempFolder = "" THEN
    ASSIGN cTempFolder = SESSION:TEMP-DIRECTORY.
ASSIGN cTempFolder = cTempFolder + (IF OPSYS = "WIN32" THEN "~\" ELSE "/").




/*------------------------------------------------------------------------------
    Procedure: getVersion
  Description: Returns this API current version.
------------------------------------------------------------------------------*/
PROCEDURE getVersion:
    RETURN "$Revision: 1.8 $".
END.


/*------------------------------------------------------------------------------
     Function: getLabelForStatus
  Description: Retrieve the label for a specified status
------------------------------------------------------------------------------*/
FUNCTION getLabelForStatus RETURNS CHARACTER
    (INPUT lStatus  AS LOGICAL):
    IF lStatus THEN
        RETURN "Success".
    ELSE IF NOT lStatus THEN
        RETURN "Fail".
    RETURN "Warning".
END.


/*------------------------------------------------------------------------------
    Procedure: addListener
  Description: Adds a listener that receives execution events while tests
               are being performed.
------------------------------------------------------------------------------*/
PROCEDURE addListener:
    DEFINE INPUT PARAMETER hL       AS HANDLE       NO-UNDO.

    CREATE ttListeners.
    ASSIGN ttListeners.hListener = hL.
    VALIDATE ttListeners.
    RELEASE ttListeners.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: performTest
  Description: Runs the tests in the program passed as argument.
               The results are sent to the registered listeners as events.
------------------------------------------------------------------------------*/
PROCEDURE performTest:
    DEFINE INPUT PARAMETER cTestCase        AS CHARACTER    NO-UNDO.


    DEFINE VARIABLE hTestCase   AS HANDLE                   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL                  NO-UNDO.
    DEFINE VARIABLE iCounter    AS INTEGER                  NO-UNDO.
    DEFINE VARIABLE hProcedure  AS HANDLE                   NO-UNDO.


    /* Load the program persistently */
    DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:
        lError = YES.
        IF SEARCH(cTestCase) <> ? THEN DO:
            RUN VALUE(cTestCase) PERSISTENT SET hTestCase NO-ERROR.
            ASSIGN lError = NO.
        END.
    END.
    IF lError OR NOT VALID-HANDLE(hTestCase) THEN DO:
        CREATE ttUnitTest.
        ASSIGN  ttUnitTest.seq      = 0
                ttUnitTest.name     = "<< Program Loading - " + cTestCase + " - " + ERROR-STATUS:GET-MESSAGE(1) + " >>"
                iFirstAssertFailed  = 0.

        RUN fail("Test case program could not be loaded" + IF ERROR-STATUS:ERROR THEN ": " + ERROR-STATUS:GET-MESSAGE(1) ELSE ".").

        /* Notify listeners about execution result */
        RUN notifyTestRun (BUFFER ttUnitTest).
    END.
    ELSE DO:
        /* Inspect all the tests in the program handle */
        RUN inspectTestCases (hTestCase).

        /* Initialize it! */
        RUN initialize IN hTestCase NO-ERROR.

        /* Run the tests */
        hTestCase:ADD-SUPER-PROCEDURE(THIS-PROCEDURE).

        /* Adds runner's super-procedures as super-procedures on test program */
        DO iCounter = 1 to NUM-ENTRIES(THIS-PROCEDURE:SUPER-PROCEDURES):
            ASSIGN hProcedure = WIDGET-HANDLE(ENTRY(iCounter, THIS-PROCEDURE:SUPER-PROCEDURES)).
            IF VALID-HANDLE(hProcedure) THEN
                hTestCase:ADD-SUPER-PROCEDURE(hProcedure).
        END.

        RUN runUnitTests (hTestCase).
        hTestCase:REMOVE-SUPER-PROCEDURE(THIS-PROCEDURE).

        /* Cleanup ! */
        RUN dispose IN hTestCase NO-ERROR.
        IF VALID-HANDLE(hTestCase) THEN
            DELETE OBJECT hTestCase.
    END.
    RETURN "".
END.


/* -------------------- Asserts -------------------------------------*/


/*------------------------------------------------------------------------------
    Procedure: fail
  Description: Procedure called by the test when it fails.
------------------------------------------------------------------------------*/
PROCEDURE fail:
    DEFINE INPUT PARAMETER cMsg         AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE lWillUpdate         AS LOGICAL      NO-UNDO.


    ASSIGN lWillUpdate = AVAILABLE ttUnitTest AND iFirstAssertFailed = 0.
    RUN assertTrue(FALSE).

    IF lWillUpdate THEN
        ASSIGN ttUnitTest.txtMessage = cMsg.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertTrue
  Description: Check if the parameter received is true. If not the test fails.
------------------------------------------------------------------------------*/
PROCEDURE assertTrue:
    DEFINE INPUT PARAMETER lValor       AS LOGICAL      NO-UNDO.


    RUN recordAssert(INPUT lValor, STRING(lValor), YES).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertFalse
  Description: Check if the parameter received is FALSE. If not the test fails.
------------------------------------------------------------------------------*/
PROCEDURE assertFalse:
    DEFINE INPUT PARAMETER lValor       AS LOGICAL      NO-UNDO.


    RUN recordAssert(INPUT NOT lValor,
                     INPUT STRING(lValor),
                     INPUT NO).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsChar
  Description: Used to assert that two strings have the same value.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsChar:
    DEFINE INPUT PARAMETER str1         AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER str2         AS CHARACTER    NO-UNDO.


    RUN recordAssert(INPUT (str1 = str2),
                     INPUT str1,
                     INPUT str2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsInt
  Description: Used to assert that two integer values have the same value.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsInt:
    DEFINE INPUT PARAMETER int1         AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER int2         AS INTEGER      NO-UNDO.


    RUN recordAssert(INPUT (int1 = int2),
                     INPUT int1,
                     INPUT int2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsHandle
  Description: Used to assert that two handles point to the same program.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsHandle:
    DEFINE INPUT PARAMETER hdl1         AS HANDLE      NO-UNDO.
    DEFINE INPUT PARAMETER hdl2         AS HANDLE      NO-UNDO.


    RUN recordAssert(INPUT (hdl1 = hdl2),
                     INPUT hdl1,
                     INPUT hdl2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsDate
  Description: Used to assert that two dates have the same value.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsDate:
    DEFINE INPUT PARAMETER date1        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER date2        AS HANDLE       NO-UNDO.


    RUN recordAssert(INPUT date1 = date2,
                     INPUT date1,
                     INPUT date2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsDecimal
  Description: Used to assert that two decimals have the same value.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsDecimal:
    DEFINE INPUT PARAMETER dec1         AS DECIMAL      NO-UNDO.
    DEFINE INPUT PARAMETER dec2         AS DECIMAL      NO-UNDO.


    RUN recordAssert(INPUT dec1 = dec2,
                     INPUT dec1,
                     INPUT dec2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsLogical
  Description: Used to assert that two logical values match.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsLogical:
    DEFINE INPUT PARAMETER log1        AS LOGICAL      NO-UNDO.
    DEFINE INPUT PARAMETER log2        AS LOGICAL      NO-UNDO.


    RUN recordAssert(INPUT log1 = log2,
                     INPUT log1,
                     INPUT log2).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertNull
  Description: Used to assert that a handle is null.
------------------------------------------------------------------------------*/
PROCEDURE assertNull:
    DEFINE INPUT PARAMETER hdl          AS HANDLE       NO-UNDO.


    RUN recordAssert(INPUT NOT VALID-HANDLE(hdl),
                     INPUT hdl,
                     INPUT "?").
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertNotNull
  Description: Used to assert that a handle points to a valid handle.
------------------------------------------------------------------------------*/
PROCEDURE assertNotNull:
    DEFINE INPUT PARAMETER hdl          AS HANDLE       NO-UNDO.


    RUN recordAssert(INPUT VALID-HANDLE(hdl),
                     INPUT "?",
                     INPUT "<>?").
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: __internal_BufferCompare
  Description: Used to compare two records from two different temp-tables
  ReturnValue: - "" if records are equals
               - "**" if records are different without structure error
               - "**" + error description otherwise
------------------------------------------------------------------------------*/
FUNCTION __internal_BufferCompare
    RETURNS CHARACTER PRIVATE
    (
        INPUT p_hBufferA        AS HANDLE,
        INPUT p_hBufferB        AS HANDLE,
        INPUT p_lStrictMode     AS LOGICAL,           /* True: all fields must be equals in both TT, and both TT must have the same field count and name - False: act like BUFFER-COMPARE() */
        INPUT p_cExcludedFields AS CHARACTER          /* List of fields to be excluded from comparison, separator is comma (,) */
    ) :

    DEFINE VARIABLE lEqual        AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE i             AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cListFields   AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cErrors       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iNumFieldsA   AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iNumFieldsB   AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hFieldA       AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hFieldB       AS HANDLE       NO-UNDO.


    /* Buffers must be available */
    IF NOT p_hBufferA:AVAILABLE OR NOT p_hBufferB:AVAILABLE THEN
        RETURN "Either buffer A or buffer B is not available.".

    /* Okay, now handle manually the strict mode ... */
    ASSIGN  iNumFieldsA = p_hBufferA:NUM-FIELDS
            iNumFieldsB = p_hBufferB:NUM-FIELDS
            cListFields = ""
            cErrors     = ""
            lEqual      = YES.            /* Default behavior is "equality" ... unless we found they are not equal */
    IF p_cExcludedFields = ? THEN
        ASSIGN p_cExcludedFields = "".

    /* Enum all fields of buffer A and try to find a match with buffer B */
    DO i = 1 TO iNumFieldsA :
        ASSIGN  hFieldA = p_hBufferA:BUFFER-FIELD(i)
                cListFields = cListFields + hFieldA:NAME + ","
                hFieldB = p_hBufferB:BUFFER-FIELD(hFieldA:NAME).
        IF NOT VALID-HANDLE(hFieldB) THEN DO:
            ASSIGN cErrors = cErrors + "Field " + QUOTER(hFieldA:NAME) + " was not found in buffer B." + CHR(10).
            IF p_lStrictMode THEN
                ASSIGN lEqual = NO.
        END.
        ELSE DO:
            /* Ensure buffer types are the same */
            IF hFieldA:DATA-TYPE <> hFieldB:DATA-TYPE THEN DO:
                ASSIGN  cErrors = cErrors + "Fields " + QUOTER(hFieldA:NAME) + " is of type " + QUOTER(hFieldA:DATA-TYPE) + " in buffer A but of type " + QUOTER(hFieldB:DATA-TYPE) + " in buffer B." + CHR(10)
                        lEqual = NO.
            END.
            /* Ensure buffer values are the same (except if field is found in exception list) */
            ELSE IF hFieldA:BUFFER-VALUE <> hFieldB:BUFFER-VALUE THEN DO:
                IF LOOKUP(hFieldA:NAME, p_cExcludedFields, ",") = 0 THEN        /* Field is not excluded */
                    ASSIGN  lEqual  = NO.
            END.
        END.
    END.
    ASSIGN cListFields = TRIM(cListFields, ",").

    /* If we get here, it means that all fields in buffer A were found in buffer B */
    DO i = 1 TO iNumFieldsB :
        ASSIGN hFieldB = p_hBufferB:BUFFER-FIELD(i).
        IF LOOKUP(hFieldB:NAME, cListFields) = 0 THEN DO:
            ASSIGN cErrors = cErrors + "Field " + QUOTER(hFieldB:NAME) + " was not found in buffer A." + CHR(10).
            IF p_lStrictMode THEN
                ASSIGN lEqual = NO.
        END.
        /* No ELSE here because all the job is already done */
    END.

    /* Retrun value */
    IF lEqual THEN
        RETURN "".
    RETURN "** " + cErrors.
END FUNCTION.


/*------------------------------------------------------------------------------
    Procedure: __internal_TestTTEqualityByHandles
  Description: Used to assert that two temp tables contains same data.
  ReturnValue: "" if TT are equals, valid mismatch description otherwise
------------------------------------------------------------------------------*/
FUNCTION __internal_TestTTEqualityByHandles
    RETURNS CHARACTER PRIVATE
    (
        INPUT p_hTempTableA       AS HANDLE,
        INPUT p_hTempTableB       AS HANDLE,
        INPUT p_lStrictMode       AS LOGICAL,
        INPUT p_cParentContainer  AS CHARACTER,       /* Can be "DATASET" */
        INPUT p_cExcludedFields   AS CHARACTER        /* List of fields to be excluded from comparison, separator is comma (,) */
    ) :
    DEFINE  VARIABLE    lcResult        AS CHARACTER    NO-UNDO     INITIAL "".

&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) < 10 &THEN
    ASSIGN lcResult = "Progress OpenEdge v10 or higher is required.".
    RUN fail(lcResult).
&ELSE
    DEFINE  VARIABLE    hTempTableA     AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    hTempTableB     AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    hQueryA         AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    hBufferA        AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    hQueryB         AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    hBufferB        AS HANDLE       NO-UNDO.
    DEFINE  VARIABLE    lTTEquals       AS LOGICAL      NO-UNDO     INITIAL TRUE.
    DEFINE  VARIABLE    cDiff           AS CHARACTER    NO-UNDO.
    DEFINE  VARIABLE    lcXmlB          AS LONGCHAR     NO-UNDO.



    /* ------------------------------------------------ */
    /* Step 1 - Create new temp tables from duplicating */
    /* ------------------------------------------------ */
    CREATE TEMP-TABLE hTempTableA.
    hTempTableA:CREATE-LIKE(p_hTempTableA:DEFAULT-BUFFER-HANDLE).
    hTempTableA:TEMP-TABLE-PREPARE(p_hTempTableA:NAME).
    hTempTableA:COPY-TEMP-TABLE(p_hTempTableA, FALSE, FALSE).

    CREATE TEMP-TABLE hTempTableB.
    hTempTableB:CREATE-LIKE(p_hTempTableB:DEFAULT-BUFFER-HANDLE).
    hTempTableB:TEMP-TABLE-PREPARE(p_hTempTableB:NAME).
    hTempTableB:COPY-TEMP-TABLE(p_hTempTableB, FALSE, FALSE).


    /* ------------------------- */
    /* Step 2 - Prepare queries  */
    /* ------------------------- */
    /* Query for table A */
    CREATE QUERY hQueryA.
    ASSIGN  hBufferA = hTempTableA:DEFAULT-BUFFER-HANDLE.
    hQueryA:ADD-BUFFER(hBufferA).
    hQueryA:QUERY-PREPARE("FOR EACH " + hTempTableA:NAME + " NO-LOCK").
    hQueryA:QUERY-OPEN().

    /* Query for table B */
    CREATE QUERY hQueryB.
    ASSIGN  hBufferB = hTempTableB:DEFAULT-BUFFER-HANDLE.
    hQueryB:ADD-BUFFER(hBufferB).
    hQueryB:QUERY-PREPARE("FOR EACH " + hTempTableB:NAME + " NO-LOCK").
    hQueryB:QUERY-OPEN().

    hQueryA:GET-FIRST().
    hQueryB:GET-FIRST().

    /* --------------------------------------------------- */
    /* Step 3 - Remove records both in table A and table B */
    /* --------------------------------------------------- */
    DO WHILE NOT hQueryA:QUERY-OFF-END:
        DO WHILE NOT hQueryB:QUERY-OFF-END:
            ASSIGN cDiff = __internal_BufferCompare(INPUT hBufferA, INPUT hBufferB, p_lStrictMode, p_cExcludedFields).
            IF cDiff = "" THEN DO:
                hBufferA:BUFFER-DELETE().
                hBufferB:BUFFER-DELETE().
                LEAVE.
            END.
            ELSE IF cDiff <> "**" THEN DO:  /* "**" tells buffers are NOT equal. Description is following (if any). */
                IF INDEX(lcResult, cDiff) = 0 THEN        /* Only append if description is new. */
                    ASSIGN lcResult = lcResult + cDiff.
            END.
            hQueryB:GET-NEXT().
        END.
        hQueryB:GET-FIRST().                /* rewind */
        hQueryA:GET-NEXT().
    END.
    IF lcResult > "" THEN
        ASSIGN lcResult = lcResult + CHR(10).

    /* --------------------------- */
    /* Step 4 - Check for equality */
    /* --------------------------- */
    /* Both TT should be empty if they were equal */
    IF hTempTableA:HAS-RECORDS OR hTempTableB:HAS-RECORDS THEN
        ASSIGN lTTEquals = FALSE.

    /* --------------------------------------- */
    /* Step 6 - Export TT in cas of difference */
    /* --------------------------------------- */
    IF NOT lTTEquals THEN DO:
        DEFINE VARIABLE lcToday     AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE lcOutFileA  AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE lcOutFileB  AS CHARACTER   NO-UNDO.


        ASSIGN  lcToday     = STRING(YEAR(TODAY), "9999") + "-" + STRING(MONTH(TODAY), "99") + "-" + STRING(DAY(TODAY), "99") + "-" + STRING(MTIME).


        /* ------------------------------------- */
        /* Prepare target file names - diff only */
        /* ------------------------------------- */
        ASSIGN  lcOutFileA  = cTempFolder + "TTdump_" + lcToday + "_#1_" + p_cParentContainer + "_" + hTempTableA:NAME + "_diff.xml"
                lcOutFileB  = cTempFolder + "TTdump_" + lcToday + "_#2_" + p_cParentContainer + "_" + hTempTableB:NAME + "_diff.xml".

        /* Export differences (common records have already been removed) */
        hTempTableA:WRITE-XML("FILE", lcOutFileA, TRUE, ?, ?, ?, ?).
        hTempTableB:WRITE-XML("LONGCHAR", lcXmlB, TRUE, ?, ?, ?, ?).

        /* Dirty but easy way to ensure both TT will have same name in both files */
        IF hTempTableA:NAME <> hTempTableB:NAME THEN
            ASSIGN  lcXmlB = REPLACE(lcXmlB, "<" + hTempTableB:NAME, "<" + hTempTableA:NAME)
                    lcXmlB = REPLACE(lcXmlB, "</" + hTempTableB:NAME, "</" + hTempTableA:NAME).

        COPY-LOB FROM lcXmlB TO FILE lcOutFileB NO-CONVERT NO-ERROR.
        ASSIGN lcResult = lcResult + "TT mismatch. See dump files: " + CHR(10) + QUOTER(lcOutFileA) + CHR(10) + " and " + CHR(10) + QUOTER(lcOutFileB).


        /* ---------------------------------------- */
        /* Prepare target file names - full content */
        /* ---------------------------------------- */
        ASSIGN  lcOutFileA  = REPLACE(lcOutFileA, hTempTableA:NAME + "_diff.xml", p_hTempTableA:NAME + "_full.xml")
                lcOutFileB  = REPLACE(lcOutFileB, hTempTableB:NAME + "_diff.xml", p_hTempTableB:NAME + "_full.xml").

        /* Export full TT */
        p_hTempTableA:WRITE-XML("FILE", lcOutFileA, TRUE, ?, ?, ?, ?).
        p_hTempTableB:WRITE-XML("LONGCHAR", lcXmlB, TRUE, ?, ?, ?, ?).

        /* Dirty but easy way to ensure both TT will have same name in both files */
        IF p_hTempTableA:NAME <> p_hTempTableB:NAME THEN
            ASSIGN  lcXmlB = REPLACE(lcXmlB, "<" + p_hTempTableB:NAME, "<" + p_hTempTableA:NAME)
                    lcXmlB = REPLACE(lcXmlB, "</" + p_hTempTableB:NAME, "</" + p_hTempTableA:NAME).

        COPY-LOB FROM lcXmlB TO FILE lcOutFileB NO-CONVERT NO-ERROR.

        ASSIGN lcResult = lcResult + CHR(10) + " and " + CHR(10) + QUOTER(lcOutFileA) + CHR(10) + " and " + CHR(10) + QUOTER(lcOutFileB).
    END.

    /* ---------------- */
    /* Step 7 - Cleanup */
    /* ---------------- */
    IF VALID-HANDLE(hTempTableA)        THEN DELETE OBJECT hTempTableA.
    IF VALID-HANDLE(hTempTableB)        THEN DELETE OBJECT hTempTableB.

    hQueryA:QUERY-CLOSE().
    DELETE OBJECT hQueryA.
    hQueryB:QUERY-CLOSE().
    DELETE OBJECT hQueryB.

&ENDIF
    RETURN lcResult.
END FUNCTION.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsTTExtended
  Description: Used to assert that two temp tables contains same data.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsTTExtended PRIVATE:
    DEFINE INPUT PARAMETER p_hTempTableA        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_hTempTableB        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_lStrictMode        AS LOGICAL      NO-UNDO.
    DEFINE INPUT PARAMETER p_cExcludedFields    AS CHARACTER    NO-UNDO.


&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) < 10 &THEN
    RUN fail("Progress OpenEdge v10 or higher is required.").
    RETURN "".
&ELSE
    DEFINE VARIABLE lcErrorDesc                 AS CHARACTER   NO-UNDO.


    IF p_hTempTableA:TYPE = "TEMP-TABLE" AND p_hTempTableB:TYPE = "TEMP-TABLE" THEN DO:
        ASSIGN lcErrorDesc = __internal_TestTTEqualityByHandles(p_hTempTableA, p_hTempTableB, p_lStrictMode, "", p_cExcludedFields).
        RUN recordAssert(   INPUT lcErrorDesc = "",
                            INPUT "Data content mismatch for TTs " + p_hTempTableA:NAME + " and " + p_hTempTableB:NAME + CHR(10) + lcErrorDesc,
                            INPUT "TT equality").
    END.
    ELSE
        RUN recordAssert(   INPUT FALSE,
                            INPUT "** Invalid parameters type: <" + p_hTempTableA:TYPE + ", " + p_hTempTableB:TYPE + ">",
                            INPUT "<TEMP-TABLE, TEMP-TABLE>").
    RETURN "".
&ENDIF
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsTT
  Description: Used to assert that two temp tables contains same data.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsTT:
    DEFINE INPUT PARAMETER p_hTempTableA        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_hTempTableB        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_cExcludedFields    AS CHARACTER    NO-UNDO.


    RUN assertEqualsTTExtended(INPUT p_hTempTableA, INPUT p_hTempTableB, INPUT FALSE, INPUT p_cExcludedFields).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsStrictTT
  Description: Used to assert that two temp tables contains same data.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsStrictTT:
    DEFINE INPUT PARAMETER p_hTempTableA        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_hTempTableB        AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_cExcludedFields    AS CHARACTER    NO-UNDO.


    RUN assertEqualsTTExtended(INPUT p_hTempTableA, INPUT p_hTempTableB, INPUT TRUE, INPUT p_cExcludedFields).
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: assertEqualsDataSet
  Description: Used to assert that two datasets contains same data.
------------------------------------------------------------------------------*/
PROCEDURE assertEqualsDataSet:
    DEFINE INPUT PARAMETER p_hDataSetA          AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER p_hDataSetB          AS HANDLE       NO-UNDO.


&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) < 10 &THEN
    RUN fail("Progress OpenEdge v10 or higher is required.").
    RETURN "".
&ELSE
    IF p_hDataSetA:TYPE = "DATASET" AND p_hDataSetB:TYPE = "DATASET" THEN DO:
        DEFINE VARIABLE iNumTempTableA      AS INTEGER     NO-UNDO.
        DEFINE VARIABLE iNumTempTableB      AS INTEGER     NO-UNDO.
        DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.
        DEFINE VARIABLE cTTNames            AS CHARACTER   NO-UNDO  INITIAL "".
        DEFINE VARIABLE hTTA                AS HANDLE      NO-UNDO.
        DEFINE VARIABLE hTTB                AS HANDLE      NO-UNDO.
        DEFINE VARIABLE lEqual              AS LOGICAL     NO-UNDO  INITIAL TRUE.
        DEFINE VARIABLE lcErrorDesc         AS CHARACTER   NO-UNDO.


        ASSIGN  iNumTempTableA = p_hDataSetA:NUM-BUFFERS
                iNumTempTableB = p_hDataSetB:NUM-BUFFERS.

        /* Browse TT from dataset A */
        DO i = 1 TO iNumTempTableA:
            ASSIGN hTTA = p_hDataSetA:GET-BUFFER-HANDLE(i).
            IF LOOKUP(CAPS(hTTA:NAME), cTTNames) = 0 THEN DO:
                ASSIGN cTTNames = cTTNames + CAPS(hTTA:NAME) + ",".

                ASSIGN hTTB = p_hDataSetB:GET-BUFFER-HANDLE(CAPS(hTTA:NAME)) NO-ERROR.
                IF hTTB <> ? AND VALID-HANDLE(hTTB) THEN DO:
                    ASSIGN lEqual = (__internal_TestTTEqualityByHandles(hTTA, hTTB, FALSE, p_hDataSetA:NAME, ?) = "").
                    RUN recordAssert(   INPUT lEqual,
                                        INPUT "Data content mismatch for TTs in DATASET " + p_hDataSetA:NAME + ":" + hTTA:NAME + " and " + p_hDataSetB:NAME + ":" + hTTB:NAME + CHR(10) + lcErrorDesc,
                                        INPUT "DATASET equality").
                    LEAVE.
                END.
                ELSE DO:
                    RUN fail("Temp-table " + CAPS(hTTA:NAME) + " found in first dataset but not in second one.").
                    ASSIGN lEqual = FALSE.
                    LEAVE.
                END.
            END.
        END.

        /* Browse TT from dataset B, only if no difference was found yet */
        IF lEqual THEN DO:
            DO i = 1 TO iNumTempTableB:
                ASSIGN hTTB = p_hDataSetB:GET-BUFFER-HANDLE(i).
                IF LOOKUP(CAPS(hTTB:NAME), cTTNames) > 0 THEN DO:           /*  Each TT in DATASET B should exist in DATASET A */
                    ASSIGN hTTA = p_hDataSetA:GET-BUFFER-HANDLE(CAPS(hTTB:NAME)) NO-ERROR.
                    IF hTTA <> ? AND VALID-HANDLE(hTTA) THEN DO:
                        ASSIGN lEqual = (__internal_TestTTEqualityByHandles(hTTA, hTTB, FALSE, p_hDataSetB:NAME, ?) = "").
                        RUN recordAssert(   INPUT lEqual,
                                            INPUT "Data content mismatch for TTs in DATASET " + p_hDataSetB:NAME + ":" + hTTB:NAME + " and " + p_hDataSetA:NAME + ":" + hTTA:NAME + CHR(10) + lcErrorDesc,
                                            INPUT "DATASET equality").
                        LEAVE.
                    END.
                    /* No ELSE here because the TT MUST be found in DATASET A because the TT name was found in the list of TT in DATASET A */
                END.
                ELSE DO:
                    RUN fail("Temp-table " + CAPS(hTTB:NAME) + " found in second dataset but not in first one.").
                    ASSIGN lEqual = FALSE.
                    LEAVE.
                END.
            END.
        END.

    END.
    ELSE
        RUN recordAssert(   INPUT FALSE,
                            INPUT "** Invalid parameters type: <" + p_hDataSetA:TYPE + ", " + p_hDataSetB:TYPE + ">",
                            INPUT "<DATASET, DATASET>").
    RETURN "".
&ENDIF
END.


/*------------------------------------------------------------------------------
    Procedure: expectError
  Description: Used to sinalize an ERROR is expected.
------------------------------------------------------------------------------*/
PROCEDURE expectError:
    DEFINE INPUT PARAMETER lValue       AS LOGICAL          NO-UNDO.


    ASSIGN lExpectedError = lValue.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: expectStop
  Description: Used to sinalize an STOP is expected.
------------------------------------------------------------------------------*/
PROCEDURE expectStop:
    DEFINE INPUT PARAMETER lValue       AS LOGICAL          NO-UNDO.


    ASSIGN lExpectedStop = lValue.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: expectQuit
  Description: Used to sinalize an QUIT is expected.
------------------------------------------------------------------------------*/
PROCEDURE expectQuit:
    DEFINE INPUT PARAMETER lValue       AS LOGICAL          NO-UNDO.


    ASSIGN lExpectedQuit = lValue.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: checkPoint
---------------------------------------------------------------------------*/
PROCEDURE checkPoint:
    DEFINE INPUT PARAMETER cMessage     AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE cStatus             AS CHARACTER        NO-UNDO.


    IF iAssertCounter = 0 THEN
        ASSIGN cStatus = getLabelForStatus(?).
    ELSE
        ASSIGN cStatus = getLabelForStatus(IFirstAssertFailed = 0).

    RUN recordEvent('Checkpoint: "' + cMessage + '" - Last assert: ' + STRING(iAssertCounter) + " - Current Status: " + cStatus).
    RETURN "".
END.

/***** Plugins Methods **********************/
/*------------------------------------------------------------------------------
    Procedure: Plugins.fail
---------------------------------------------------------------------------*/
PROCEDURE Plugins.fail:
    DEFINE INPUT PARAMETER cPluginName      AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iSequence               AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cCurrentTest            AS CHARACTER        NO-UNDO.

    DEFINE BUFFER bTests                    FOR ttUnitTest.


    FIND LAST bTests NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bTests THEN
        ASSIGN iSequence = 0.
    ASSIGN iSequence = iSequence + 1.

    IF AVAILABLE ttUnitTest THEN
        cCurrentTest = " - " + ttUnitTest.name.

    CREATE bTests.
    ASSIGN  bTests.seq        = iSequence
            bTests.name       = "Plugin Fail (" + cPluginName + ")" + cCurrentTest
            bTests.result     = FALSE
            bTests.txtMessage = cMessage
            bTests.runnable   = FALSE.

    /* Notify listeners about execution result */
    RUN notifyTestRun (BUFFER bTests).
    RETURN "".
END.


/*************** Private Methods ************************/

/*------------------------------------------------------------------------------
    Procedure: recordAssert
---------------------------------------------------------------------------*/
PROCEDURE recordAssert PRIVATE:
    DEFINE INPUT PARAMETER lAssertion       AS LOGICAL          NO-UNDO.
    DEFINE INPUT PARAMETER cGottenValue     AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cExpectedValue   AS CHARACTER        NO-UNDO.


    IF cGottenValue = ? THEN
        ASSIGN cGottenValue = "?".
    IF cExpectedValue = ? THEN
        ASSIGN cExpectedValue = "?".

    ASSIGN iAssertCounter = iAssertCounter + 1.
    IF iFirstAssertFailed = 0 AND lAssertion = FALSE THEN
        ASSIGN iFirstAssertFailed = iAssertCounter.

    IF ttUnitTest.RESULT <> FALSE THEN DO: /* Just update status if it has not recorded a previous error */
        ASSIGN  ttUnitTest.result = lAssertion
                ttUnitTest.firstAssertFailed = iFirstAssertFailed.

        IF NOT lAssertion THEN
            ASSIGN ttUnitTest.txtMessage = "Expected: " + QUOTER(cExpectedValue) +
                                           "Got: " + QUOTER(cGottenValue).
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: inspectTestCases
---------------------------------------------------------------------------*/
PROCEDURE inspectTestCases PRIVATE:
    DEFINE INPUT PARAMETER hProgram     AS HANDLE       NO-UNDO.

    DEFINE VARIABLE iCont               AS INTEGER      NO-UNDO.
    DEFINE VARIABLE procName            AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE procSignature       AS CHARACTER    NO-UNDO.


    EMPTY TEMP-TABLE ttUnitTest.

    DO iCont = 1 TO NUM-ENTRIES(hProgram:INTERNAL-ENTRIES):
        ASSIGN  procName = ENTRY(iCont, hProgram:INTERNAL-ENTRIES)
                procSignature = hProgram:GET-SIGNATURE(procName).

        /* O nome da procedure deve começar com "test" */
        IF NOT procName BEGINS "test" THEN
            NEXT.

        /* Se nao for uma procedure ou se tiver parâmetros, desconsidera */
       IF NOT procSignature = "PROCEDURE,," THEN
           NEXT.

       CREATE ttUnitTest.
       ASSIGN  ttUnitTest.seq      = iCont
               ttUnitTest.name     = procName.
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: recordEvent
---------------------------------------------------------------------------*/
PROCEDURE recordEvent:
    DEFINE INPUT PARAMETER cMessage        AS CHARACTER    NO-UNDO.


    iEventNumber = iEventNumber + 1.
    CREATE ttEvent.
    ASSIGN  ttEvent.eventNum     = iEventNumber
            ttEvent.eventMessage = cMessage.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: runUnitTests
---------------------------------------------------------------------------*/
PROCEDURE runUnitTests PRIVATE:
    DEFINE INPUT PARAMETER hSuite   AS HANDLE       NO-UNDO.

    DEFINE VARIABLE iTime      AS INTEGER                   NO-UNDO.
    DEFINE VARIABLE lRunFine   AS LOGICAL   INITIAL NO      NO-UNDO.

    DEFINE VARIABLE lQuitDetected       AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE lStopDetected       AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE lErrorDetected      AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE lExceptionExpected  AS LOGICAL      NO-UNDO.


    FOR EACH ttUnitTest EXCLUSIVE-LOCK:
        IF NOT ttUnitTest.runnable THEN
            NEXT.

        RUN notifyRunningTest (BUFFER ttUnitTest).
        RUN setUp IN hSuite NO-ERROR.

        /* Reset assert counter */
        ASSIGN  iAssertCounter     = 0
                iFirstAssertFailed = 0
                iTime              = ETIME
                lExpectedError     = NO
                lExpectedStop      = NO
                lExpectedQuit      = NO.

        /* Runs the test */
        DO ON QUIT UNDO, LEAVE:
            ASSIGN lQuitDetected = YES.
            DO ON STOP UNDO, LEAVE:
                ASSIGN lStopDetected = YES.
                DO ON ERROR UNDO, LEAVE:
                    ASSIGN  lRunFine = NO
                            ERROR-STATUS:ERROR = NO.
                    RUN VALUE(ttUnitTest.NAME) IN hSuite NO-ERROR.
                    ASSIGN  lRunFine = YES
                            lErrorDetected = ERROR-STATUS:ERROR.
                END.
                ASSIGN lStopDetected = NO.
            END.
            ASSIGN lQuitDetected = NO.
        END.

        IF lQuitDetected THEN
            ASSIGN  lStopDetected  = NO
                    lErrorDetected = NO.
        ELSE IF lStopDetected THEN
            ASSIGN  lErrorDetected = NO.

        ASSIGN lExceptionExpected = lExpectedError OR lExpectedStop OR lExpectedQuit.

        /* Checks expected errors */
        IF lExpectedError AND NOT lErrorDetected THEN
            RUN fail("Expected ERROR but no error was raised").
        ELSE IF lExpectedStop AND NOT lStopDetected THEN
            RUN fail("Expected STOP but no stop was raised").
        ELSE IF lExpectedQuit AND NOT lQuitDetected THEN
            RUN fail("Expected QUIT but no quit was raised").
        ELSE IF NOT lExceptionExpected THEN DO:
            IF ERROR-STATUS:ERROR = YES THEN
                RUN fail("Error running file: " + ERROR-STATUS:GET-MESSAGE(1)).
            ELSE IF COMPILER:ERROR = YES THEN
                RUN fail("Error compiling file at " + COMPILER:FILE-NAME + ":" + STRING(COMPILER:ERROR-ROW) + ":" + STRING(COMPILER:ERROR-COL)).
            ELSE IF NOT lRunFine THEN
                RUN fail("Errors occured while running the test.").
        END.

        ASSIGN  iTime = ETIME - iTime
                ttUnitTest.totalTime = iTime.

        RUN tearDown IN hSuite NO-ERROR.

        /* Notify listeners about execution result */
        RUN notifyTestRun (BUFFER ttUnitTest).
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: notifyRunningTest
---------------------------------------------------------------------------*/
PROCEDURE notifyRunningTest PRIVATE:
    DEFINE PARAMETER BUFFER bTest FOR ttUnitTest.


    FOR EACH ttListeners:
        IF VALID-HANDLE(ttListeners.hListener) THEN
            RUN RunningTest IN ttListeners.hListener (bTest.name) NO-ERROR.
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: notifyTestRun
---------------------------------------------------------------------------*/
PROCEDURE notifyTestRun PRIVATE:
    DEFINE PARAMETER BUFFER bTest FOR ttUnitTest.

    DEFINE VARIABLE cFinalMessage   AS CHARACTER  NO-UNDO.


    FOR EACH ttListeners:
        IF VALID-HANDLE(ttListeners.hListener) THEN DO:
            FOR EACH ttEvent BY ttEvent.eventNum DESC:
                ASSIGN cFinalMessage = cFinalMessage + "~n" + ttEvent.eventMessage.
            END.
            EMPTY TEMP-TABLE ttEvent.

            IF bTest.txtMessage <> "" THEN
                ASSIGN cFinalMessage = bTest.txtMessage + cFinalMessage.
            ELSE
                ASSIGN cFinalMessage = TRIM(cFinalMessage).

            RUN testRun IN ttListeners.hListener
                (bTest.name,
                 bTest.result,
                 cFinalMessage,
                 bTest.firstAssertFailed,
                 bTest.totalTime) NO-ERROR.
        END.
    END.
    RETURN "".
END.
