/*******************************************************************************
**
**     Program: template.p
** Description: Template to export results as junit XML file
**      Author: Flavio Eduardo de Córdova
**     Created: ???
**
********************************************************************************
**
**  Revision 1.1  SMarmotte
**  - the XML is now compliant to JUnit XML files exported from Eclipse IDE
**  - made all functions private
**  - removed duplication of Progress version property in target XML
**  - added more information about Progress in the property tag (client type, batch mode, remote)
**  - minor code optimizations
**
**  Revision 1.0
**  - first release
**
*******************************************************************************/

{prounit/suiteRunner.i}


PROCEDURE setTestData:
    DEFINE INPUT PARAMETER TABLE FOR ttTestItem.
    DEFINE INPUT PARAMETER TABLE FOR ttUnitTest.
END.


FUNCTION getLastSlashPosition RETURNS INTEGER PRIVATE (INPUT cInputString AS CHARACTER):
    RETURN MAX(R-INDEX(cInputString, "/"), R-INDEX(cInputString, "~\")).
END.


FUNCTION removeSpecialChars RETURNS CHARACTER PRIVATE (INPUT cInString AS CHARACTER):
    ASSIGN  cInString = REPLACE(cInString, "~\", "_")
            cInString = REPLACE(cInString, "/", "_")
            cInString = REPLACE(cInString, "*", "_")
            cInString = REPLACE(cInString, "?", "_")
            cInString = REPLACE(cInString, CHR(34), "_")      /* CHR(34) = '"' */
            cInString = REPLACE(cInString, "<", "_")
            cInString = REPLACE(cInString, ">", "_")
            cInString = REPLACE(cInString, "|", "_")
            cInString = REPLACE(cInString, ":", "_").
    RETURN (IF cInString > "" THEN cInString ELSE "").
END.


FUNCTION getPackageFor RETURNS CHARACTER PRIVATE (INPUT cInString AS CHARACTER):
    DEFINE VARIABLE cPath AS CHARACTER   NO-UNDO.


    IF getLastSlashPosition(cInString) = 0 THEN
        RETURN "".

    ASSIGN  cPath = SUBSTRING(cInString, 1, getLastSlashPosition(cInString) - 1)
            cPath = REPLACE(cPath, "/", ".")
            cPath = REPLACE(cPath, "~\", ".")
            cPath = removeSpecialChars(cPath).
    RETURN (IF cPath > "" THEN cPath ELSE "").
END.


FUNCTION getClassNameFor RETURNS CHARACTER PRIVATE (INPUT cInString AS CHARACTER):
    DEFINE VARIABLE cClass AS CHARACTER   NO-UNDO.


    IF getLastSlashPosition(cInString) = 0 THEN
        ASSIGN cClass = cInString.
    ELSE
        ASSIGN cClass = SUBSTRING(cInString, getLastSlashPosition(cInString) + 1).

    ASSIGN cClass = REPLACE(cClass, ".", "_").
    RETURN (IF cClass > "" THEN cClass ELSE "").
END.


PROCEDURE dumpTestCase PRIVATE:
    DEFINE INPUT PARAMETER iTestItem    AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER hXMLDoc     AS HANDLE       NO-UNDO.

    DEFINE VARIABLE iTests              AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iErrors             AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iTime               AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hTestSuite          AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hTestRun            AS HANDLE       NO-UNDO.

    DEFINE BUFFER ttTestItem FOR ttTestItem.
    DEFINE BUFFER ttUnitTest FOR ttUnitTest.


    FIND ttTestItem WHERE ttTestItem.id = iTestItem NO-LOCK.

    /* Accounts errors, tests and time */
    FOR EACH ttUnitTest WHERE ttUnitTest.testItemId = iTestItem NO-LOCK:
        ASSIGN  iTests = iTests + 1
                iTime  = iTime  + ttUnitTest.totalTime.
        IF ttUnitTest.runningStatus = {&STATUS_FAIL} THEN
            ASSIGN iErrors = iErrors + 1.
    END.

    /* Starts XML */
    CREATE X-DOCUMENT hXMLDoc.
    CREATE X-NODEREF hTestSuite.
    CREATE X-NODEREF hTestRun.

    /* Set /<testrun> tag */
    hXMLDoc:CREATE-NODE(hTestRun, "testrun", "ELEMENT").
    hXMLDoc:APPEND-CHILD(hTestRun).
    hTestRun:SET-ATTRIBUTE("errors", STRING(iErrors)).
    hTestRun:SET-ATTRIBUTE("failures", "0").
    hTestRun:SET-ATTRIBUTE("ignored", "0").
    hTestRun:SET-ATTRIBUTE("name", getPackageFor(ttTestItem.itemName)).
    hTestRun:SET-ATTRIBUTE("project", "ProUnit Test Framework").
    hTestRun:SET-ATTRIBUTE("started", STRING(iTests)).
    hTestRun:SET-ATTRIBUTE("tests", STRING(iTests)).

    /* Set /<testrun>/<testsute> tag */
    hXMLDoc:CREATE-NODE(hTestSuite, "testsuite", "ELEMENT").
    hTestRun:APPEND-CHILD(hTestSuite).
    hTestSuite:SET-ATTRIBUTE("name", LEFT-TRIM(getPackageFor(ttTestItem.itemName) + "." + getClassNameFor(ttTestItem.itemName), ".")).
    hTestSuite:SET-ATTRIBUTE("time", TRIM(STRING(iTime / 1000))).

    /* Set the /<testrun>/<properties> tag */
    RUN addProperties (INPUT hXMLDoc, INPUT hTestRun).

    /* Add tests to the XML - /<testrun>/<testsute>/<testcase> */
    FOR EACH ttUnitTest WHERE ttUnitTest.testItemID = iTestItem NO-LOCK:
        RUN dumpTest (INPUT hXMLDoc,
                      INPUT hTestSuite,
                      INPUT ttTestItem.itemName,
                      BUFFER ttUnitTest).
    END.

    /* Disposing variables */
    DELETE OBJECT hTestSuite.
    DELETE OBJECT hTestRun.
    RETURN "".
END.


PROCEDURE dumpTest PRIVATE:
    DEFINE INPUT PARAMETER hDoc         AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER hParent      AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER cTestName    AS CHARACTER    NO-UNDO.

    DEFINE PARAMETER BUFFER bTest       FOR ttUnitTest.

    DEFINE VARIABLE hTestCase       AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hFailure        AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hFailureText    AS HANDLE       NO-UNDO.


    CREATE X-NODEREF hTestCase.
    hDoc:CREATE-NODE(hTestCase, "testcase", "ELEMENT").
    hParent:APPEND-CHILD(hTestcase).

    hTestCase:SET-ATTRIBUTE("classname", LEFT-TRIM(getPackageFor(cTestName) + "." + getClassNameFor(cTestName), ".")).
    hTestCase:SET-ATTRIBUTE("name", bTest.NAME).
    hTestCase:SET-ATTRIBUTE("time", STRING(bTest.totalTime / 1000, ">>>>>9.999")).

    IF bTest.runningStatus = {&STATUS_FAIL} THEN DO:
        CREATE X-NODEREF hFailure.
        hDoc:CREATE-NODE(hFailure, "failure", "ELEMENT").
        hTestCase:APPEND-CHILD(hFailure).
        hFailure:SET-ATTRIBUTE("message", bTest.txtMessage).
        hFailure:SET-ATTRIBUTE("type", "prounit.framework.AssertionFailedError").

        CREATE X-NODEREF hFailureText.
        hDoc:CREATE-NODE(hFailureText, "", "TEXT").
        hFailure:APPEND-CHILD(hFailureText).
        hFailureText:NODE-VALUE = "prounit.framework.AssertionFailedError:" + CHR(13) +
                                  bTest.txtMessage +
                                 " at " + cTestName + "." + bTest.NAME + "(Unknown Source) " +
                                 "First Assert Failed: " + STRING(bTest.firstAssertFailed).

        DELETE OBJECT hFailure.
        DELETE OBJECT hFailureText.
    END.

    DELETE OBJECT hTestCase.
    RETURN "".
END.


PROCEDURE addProperties PRIVATE:
    DEFINE INPUT PARAMETER hDoc     AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER hParent  AS HANDLE       NO-UNDO.

    DEFINE VARIABLE hProperties     AS HANDLE       NO-UNDO.


    CREATE X-NODEREF hProperties.
    hDoc:CREATE-NODE(hProperties, "properties", "ELEMENT").
    hParent:APPEND-CHILD(hProperties).

    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "progress.version", INPUT PROVERSION).
    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "progress.path",    INPUT PROPATH).
    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "progress.client",  INPUT SESSION:CLIENT-TYPE).
    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "progress.batch",   INPUT STRING(SESSION:BATCH-MODE)).
    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "progress.remote",  INPUT STRING(SESSION:REMOTE)).
    RUN addProperty (INPUT hDoc, INPUT hProperties, INPUT "os.name",          INPUT OPSYS).

    DELETE OBJECT hProperties.
    RETURN "".
END.


PROCEDURE addProperty PRIVATE:
    DEFINE INPUT PARAMETER hDoc             AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER hParent          AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER cPropertyName    AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER cPropertyValue   AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE hProperty               AS HANDLE       NO-UNDO.


    CREATE X-NODEREF hProperty.
    hDoc:CREATE-NODE(hProperty, "property", "ELEMENT").
    hParent:APPEND-CHILD(hProperty).
    hProperty:SET-ATTRIBUTE("name" , cPropertyName).
    hProperty:SET-ATTRIBUTE("value", cPropertyValue).

    DELETE OBJECT hProperty.
    RETURN "".
END.


PROCEDURE saveResults:
    DEFINE INPUT PARAMETER cFileName    AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cOutputFile         AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE hOutputXML          AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cDirectory          AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cPreviousDateFormat AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cPreviousNumFormat  AS CHARACTER    NO-UNDO.


    ASSIGN  cPreviousDateFormat     = SESSION:DATE-FORMAT
            cPreviousNumFormat      = SESSION:NUMERIC-FORMAT
            SESSION:DATE-FORMAT     = "mdy"
            SESSION:NUMERIC-FORMAT  = "American".

    ASSIGN cDirectory  = SUBSTRING(cFileName, 1, getLastSlashPosition(cFileName)).
    IF cDirectory = "" THEN
        ASSIGN cDirectory = "./".

    FOR EACH ttTestItem WHERE ttTestItem.TYPE = {&TEST_CASE} NO-LOCK:
        RUN dumpTestCase IN THIS-PROCEDURE (INPUT ttTestItem.id, OUTPUT hOutputXML).
        ASSIGN cOutputFile = cDirectory + "TEST-" + removeSpecialChars(ttTestItem.itemName).
        hOutputXML:SAVE("file", cOutputFile + ".xml").
        DELETE OBJECT hOutputXML.
    END.

    ASSIGN  SESSION:DATE-FORMAT     = cPreviousDateFormat
            SESSION:NUMERIC-FORMAT  = cPreviousNumFormat.
    RETURN "".
END.
