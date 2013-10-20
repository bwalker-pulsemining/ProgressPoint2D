/*******************************************************************************
**
**     Program: batchRunner.p
** Description: Runs ProUnit in Batch Mode.
**      Author: Flavio Eduardo de Córdova
**     Created: 12/30/2004
**
********************************************************************************
**
**  Revision 1.5  2013/07/21  SMarmotte
**  - improved parameters detection:
**  -- better error handling
**  -- parameters can now be extracted from either a global shared variable or the SESSION:PARAMETERS string
**
**  Revision 1.4  2013/06/28  SMarmotte
**  - empty the parameter temp-table before parsing parameters
**  - minor code improvements
**
**  Revision 1.3  2013/05/21  SMarmotte
**  - added reference to suiteRunner.i and refactored code in this way
**
**  Revision 1.2  2005/12/12 00:04:41  fcordova
**  *** empty log message ***
**
**  Revision 1.1  2005/11/01 01:09:49  fcordova
**  *** empty log message ***
**
*******************************************************************************/


/* -------------------------------------------------------------------------- */
/* CONSTANTS                                                                  */
/* -------------------------------------------------------------------------- */
&SCOPED-DEFINE PARAM_NUM        5



/* -------------------------------------------------------------------------- */
/* INCLUDES                                                                   */
/* -------------------------------------------------------------------------- */
{prounit/suiteRunner.i}



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttParameter  NO-UNDO
    FIELD paramStart        AS INTEGER
    FIELD paramName         AS CHARACTER
    FIELD paramValue        AS CHARACTER
    INDEX paramIdx IS PRIMARY UNIQUE
        paramStart
        paramName.



/* -------------------------------------------------------------------------- */
/* VARIABLES                                                                  */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE iCounter                AS INTEGER          NO-UNDO.
DEFINE VARIABLE iGeneralStatus          AS INTEGER          NO-UNDO.
DEFINE VARIABLE VALID_PARAMETERS        AS CHARACTER        NO-UNDO     EXTENT {&PARAM_NUM}
        INITIAL ["-projectFile",
                 "-resultFile",
                 "-resultTemplate",
                 "-verbose",
                 "-runningAnt"].



/* -------------------------------------------------------------------------- */
/* SHARED VARIABLE                                                            */
/* -------------------------------------------------------------------------- */
/* Global variable that can be used to specify parameters.
   See online help for more information about this variable. */
DEFINE NEW GLOBAL SHARED VARIABLE cInitBatchParams  AS CHARACTER        NO-UNDO.



/* -------------------------------------------------------------------------- */
/* FUNCTION                                                                   */
/* -------------------------------------------------------------------------- */
FUNCTION getParamValue RETURNS CHARACTER (INPUT cParamName AS CHARACTER) FORWARD.




/* -------------------------------------------------------------------------- */
/* MAIN BLOCK                                                                 */
/* -------------------------------------------------------------------------- */
RUN parseParameters.

/* Shows Info in the console */
RUN showInfo("ProUnit Batch Executor").
RUN showInfo("Input Parameters:").
FOR EACH ttParameter:
    RUN showInfo("~t"  + ttParameter.paramName + ": " + ttParameter.paramValue).
END.
RUN showInfo(" ").
RUN showInfo("PROPATH").
DO iCounter = 1 TO NUM-ENTRIES(PROPATH):
    RUN showInfo("~t- " + ENTRY(iCounter, PROPATH)).
END.
RUN showInfo(" ").

mainBlock:
DO:
    /* Checks environment to add prounit.pl, if needed */
    RUN checkEnvironment.
    IF RETURN-VALUE <> "OK" THEN DO:
        RUN showError ("ProUnit library not found. This internal error should never happen.").
        ASSIGN iGeneralStatus = 3.
        LEAVE mainBlock.
    END.

    IF getParamValue("-projectFile") = ? THEN DO:
        RUN showError ("No project file set. Check the parameters string given (see online documentation).").
        ASSIGN iGeneralStatus = 2.
        LEAVE mainBlock.
    END.

    /* Validate input file */
    ASSIGN FILE-INFO:FILE-NAME = getParamValue("-projectFile").
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        RUN showError ("Input project file not found: " + getParamValue("-projectFile")).
        ASSIGN iGeneralStatus = 1.
        LEAVE mainBlock.
    END.

    ASSIGN iGeneralStatus = 0.
    RUN runBatch (INPUT getParamValue("-projectFile"),
                  INPUT getParamValue("-resultFile"),
                  INPUT getParamValue("-resultTemplate")).
END.

IF getParamValue("-runningAnt") <> "true" THEN
    QUIT.

IF iGeneralStatus = 0 THEN
    RETURN "0".
RETURN STRING(iGeneralStatus) + " - Test Suite execution failed.".


/************************ INTERNAL PROCEDURES AND FUNCTIONS *****************************/


/*------------------------------------------------------------------------------
  Procedure: parseParameters
Description: Parses the input parameters into a temp-table.
------------------------------------------------------------------------------*/
PROCEDURE parseParameters:
    DEFINE VARIABLE iCounter        AS INTEGER              NO-UNDO.
    DEFINE VARIABLE iPosition       AS INTEGER              NO-UNDO.
    DEFINE VARIABLE cParams         AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE cTmp            AS CHARACTER            NO-UNDO.

    DEFINE BUFFER bParameter        FOR ttParameter.


    /* Initializations */
    EMPTY TEMP-TABLE ttParameter.
    IF cInitBatchParams > "" THEN
        ASSIGN cParams = cInitBatchParams.
    ELSE
        ASSIGN cParams = SESSION:PARAMETER.

    /* Store each param in the temp-table */
    DO iCounter = 1 TO {&PARAM_NUM}:
        ASSIGN iPosition = INDEX(cParams, VALID_PARAMETERS[iCounter]).
        IF NOT iPosition > 0 THEN
            NEXT.

        CREATE ttParameter.
        ASSIGN  ttParameter.paramStart = iPosition
                ttParameter.paramName  = VALID_PARAMETERS[iCounter]
                ttParameter.paramValue = ?.
        VALIDATE ttParameter.
        RELEASE ttParameter.
    END.

    /* Browse the temp-table */
    FOR EACH ttParameter EXCLUSIVE-LOCK:
        FIND FIRST bParameter NO-LOCK WHERE bParameter.paramStart > ttParameter.paramStart NO-ERROR.
        IF AVAILABLE bParameter THEN
            ASSIGN iPosition = bParameter.paramStart.
        ELSE
            ASSIGN iPosition = LENGTH(cParams) + 1.

        ASSIGN cTmp = TRIM(SUBSTRING(cParams, ttParameter.paramStart, (iPosition - ttParameter.paramStart))) NO-ERROR.
        IF cTmp > "" AND NUM-ENTRIES(cTmp, "=") = 2 THEN
            ASSIGN ttParameter.paramValue = REPLACE(ENTRY(2, cTmp, "="), "'", "").
        ELSE
            DELETE ttParameter.
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
  Procedure: runBatch
Description: Runs the suite.
------------------------------------------------------------------------------*/
PROCEDURE runBatch:
    DEFINE INPUT PARAMETER cProjectFile     AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cResultFile      AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cResultTPL       AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE hSuiteRunner            AS HANDLE           NO-UNDO.


    RUN prounit/suiteRunner.p PERSISTENT SET hSuiteRunner NO-ERROR.
    IF VALID-HANDLE(hSuiteRunner) THEN DO:
        RUN addListener IN hSuiteRunner (THIS-PROCEDURE).

        RUN setProjectFile IN hSuiteRunner (cProjectFile).
        IF RETURN-VALUE <> "" THEN
            RUN showError(RETURN-VALUE).
        ELSE DO:
            RUN loadPlugins IN hSuiteRunner.
            /* Runs the suite */
            DO ON QUIT UNDO, LEAVE ON ERROR UNDO, LEAVE:
                RUN runSuite IN hSuiteRunner.
            END.

            /* Export results */
            IF getParamValue("-resultFile") <> ?  AND getParamValue("-resultFile") <> "" THEN DO:
                RUN showInfo(" ").
                RUN showInfo("Saving result file: " + getParamValue("-resultFile")).
                RUN saveResults IN hSuiteRunner (getParamValue("-resultFile"), getParamValue("-resultTemplate")) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    RUN showInfo("An error occured while saving result file.").
            END.
        END.

        DELETE OBJECT hSuiteRunner.
    END.
    RETURN "".
END.


/*------------------------------------------------------------------------------
  Procedure: showError
Description: Show an error.
------------------------------------------------------------------------------*/
PROCEDURE showError:
    DEFINE INPUT PARAMETER cMessage     AS CHARACTER        NO-UNDO.

    SESSION:DEBUG-ALERT = TRUE.
    IF SESSION:DISPLAY-TYPE = "TTY" THEN
        PUT UNFORMATTED "** Error: " cMessage SKIP.
    ELSE
		PUT UNFORMATTED "** Error: " cMessage SKIP.
        /*MESSAGE "Error: " cMessage VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/
    RETURN "".
END.


/*------------------------------------------------------------------------------
  Procedure: showInfo
Description: Show an information message.
------------------------------------------------------------------------------*/
PROCEDURE showInfo:
    DEFINE INPUT PARAMETER cMessage     AS CHARACTER        NO-UNDO.


    /* For now, shows only when TTY.. In the future we can plan do something different */
    IF SESSION:DISPLAY-TYPE <> "TTY" THEN
        RETURN "".

    IF getParamValue("-verbose") <> "true" THEN
        RETURN "".

    PUT UNFORMATTED cMessage SKIP.
    RETURN "".
END.


/*------------------------------------------------------------------------------
   Function: getParamValue
Description: Returns a parameter value.
------------------------------------------------------------------------------*/
FUNCTION getParamValue RETURNS CHARACTER (INPUT cParamName AS CHARACTER):
    FIND ttParameter NO-LOCK WHERE ttParameter.paramName = cParamName NO-ERROR.
    IF AVAILABLE ttParameter THEN
        RETURN TRIM(TRIM(ttParameter.paramValue), '"').
    RETURN ?.
END.


/*------------------------------------------------------------------------------
  Procedure: checkEnvironment
Description: Check for environment and adjust PROPATH if needed
------------------------------------------------------------------------------*/
PROCEDURE checkEnvironment:
    /* find suiteRunner */
    IF SEARCH("prounit/suiteRunner.r") <> ? THEN
        RETURN "OK".

    /* Running source ? */
    IF SEARCH("prounit/suiteRunner.p") <> ? THEN
        RETURN "OK".

    /* Library available ? */
    IF SEARCH("prounit.pl") <> ? THEN DO:
        /* Add it to the propath */
        PROPATH = PROPATH + "," + SEARCH("prounit.pl").
        RETURN "OK".
    END.

    RETURN "NOK".
END.


/*--------------------------------------------------------------------------*/
/*-------------------------------- EVENTS ----------------------------------*/
/*--------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------
    Procedure: beforeRunningSuite
  Description: Event called before suite execution.
------------------------------------------------------------------------------*/
PROCEDURE beforeRunningSuite:
    DEFINE INPUT PARAMETER cSuiteFile       AS CHARACTER        NO-UNDO.


    RUN showInfo ("Running suite " + cSuiteFile + "...").
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: beforeRunningTestCase
  Description: Event called before a Test Case execution.
------------------------------------------------------------------------------*/
PROCEDURE beforeRunningTestCase:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestCaseName    AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE cTestPath               AS CHARACTER        NO-UNDO.


    cTestPath = SEARCH(cTestCaseName).
    IF cTestPath = ? THEN
        cTestPath = "Not found".

    RUN showInfo("  Running Test Case: " + cTestCaseName + " (" + cTestPath + ")").
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: beforeRunningTest
  Description: Event called before each test in the Test Case.
------------------------------------------------------------------------------*/
PROCEDURE beforeRunningTest:
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: afterRunningTest
  Description: .
------------------------------------------------------------------------------*/
PROCEDURE afterRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER          NO-UNDO.


    RUN showInfo ("      Test: " + cTestName + " - " + getStatusLabel(iStatus)).
    IF iStatus = {&STATUS_FAIL} THEN DO:
        RUN showInfo ("        First Assert Failed: " + STRING(iFirstAssert) +
                            "  Message: " + cMessage).
        /* Signals that one error has occured */
        iGeneralStatus = 1.
    END.
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: afterRunningTestCase
  Description: Event called after a test case has finished his execution.
------------------------------------------------------------------------------*/
PROCEDURE afterRunningTestCase:
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.


    RUN showInfo ("    Test Case Status: " + getStatusLabel(iStatus)).
    RUN showInfo ("    Execution Time: " + STRING(iTimeSpent, "HH:MM:SS")).
    RUN showInfo (" ").
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: afterRunningSuite
  Description: event called after all the suite has finished.
------------------------------------------------------------------------------*/
PROCEDURE afterRunningSuite:
    DEFINE INPUT PARAMETER cProject     AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.


    RUN showInfo ("Suite Final Status: " + getStatusLabel(iStatus)).
    RUN showInfo ("Total Execution Time: " + STRING(iTimeSpent, "HH:MM:SS")).
    RETURN "".
END PROCEDURE.
