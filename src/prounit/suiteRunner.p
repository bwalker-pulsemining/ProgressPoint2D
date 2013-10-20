/****************************************************************************
**
**     Program: SuiteRunner.p
** Description: Runs all the tests of a suite.
**      Author: Flavio Eduardo de Córdova
**     Created: 06/28/2004
**
********************************************************************************
**
**  Revision 1.4  2013/06/29  SMarmotte
**  - renamed testTools.p to databaseTools.p
**
**  Revision 1.3  2013/06/22  SMarmotte
**  - added persistent load of testTools.p which defines tools to easly load data from files to temp-tables or datasets
**
**  Revision 1.2  2013/05/21  SMarmotte
**  - added error handling on result file save
**  - refactored pieces of code because of function getStatusLabel now included in suiteRunner.i
**  - enhanced templates detection so that R-CODE templates can be embedded to PL library
**  - fixed bug when a plugin referenced in configuration file doed not exist
**  - fixed detection of templates .xsl files
**  - removed empty entry when enumerating result templates
**  - improved error handling when saving results
**  - the template name in XML is now specific to the template - so you now can
**    save results from multiple templates in the same output directory
**  - templates can also be in form of RCODE
**
**  Revision 1.1  2013/05/09  SMarmotte
**  - added procedure to get test count to export (used by GUI before saving results)
**  - corrected problem with getResultTemplates when R-CODES were in PL
**  - minor code improvements
**
**  Revision 1.0
**  - initial version
**
*****************************************************************************/


/* -------------------------------------------------------------------------- */
/* CONSTANTS                                                                  */
/* -------------------------------------------------------------------------- */
&SCOPED-DEFINE DEFAULT_PRIORITY 50
&SCOPED-DEFINE PLUGIN_PRIORITY  20



/* -------------------------------------------------------------------------- */
/* INCLUDES                                                                   */
/* -------------------------------------------------------------------------- */
{prounit/suiteRunner.i}
{prounit/plugins.i}



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttListener        NO-UNDO
    FIELD hListener     AS HANDLE
    FIELD Priority      AS INTEGER
    INDEX principal IS PRIMARY UNIQUE
        hListener
    INDEX priority
        Priority
        hListener.

DEFINE TEMP-TABLE ttLoadedPlugins no-undo
    FIELD iSeq              AS INTEGER
    FIELD hPlugin           AS HANDLE
    INDEX principal IS PRIMARY UNIQUE
        iSeq.



/* -------------------------------------------------------------------------- */
/* VARIABLES                                                                  */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE cProjectFile            AS CHARACTER        NO-UNDO.
DEFINE VARIABLE iCurrentItem            AS INTEGER          NO-UNDO.
DEFINE VARIABLE iTestCaseStatus         AS INTEGER          NO-UNDO.
DEFINE VARIABLE lastExecDate            AS DATE             NO-UNDO.
DEFINE VARIABLE lastExecTime            AS INTEGER          NO-UNDO.





/*---------------------------------------------------------------------------
    Procedure: getItemCountToDump
  Description: gets the item count to dump (used before creating result files)
-------------------------------------------------------------------------*/
PROCEDURE getItemCountToDump :
    DEFINE INPUT PARAMETER iParentId    AS INTEGER      NO-UNDO.
    DEFINE OUTPUT PARAMETER opiItemCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

    DEFINE BUFFER bTests FOR ttTestItem.


    FOR EACH bTests WHERE bTests.parentSet = iParentId NO-LOCK:
        IF bTests.TYPE = {&TEST_SET} THEN DO:
            ASSIGN iCount = 0.
            RUN getItemCountToDump(INPUT bTests.id, OUTPUT iCount).
            ASSIGN opiItemCount = opiItemCount + iCount.
        END.
        ELSE DO:
            FOR EACH ttUnitTest WHERE ttUnitTest.testItemId = bTests.id NO-LOCK:
                ASSIGN opiItemCount = opiItemCount + 1.
            END.
        END.
    END.

    RETURN "".
END PROCEDURE.


/*---------------------------------------------------------------------------
    Procedure: setProjectFile
  Description: sets the XML file for a project.
-------------------------------------------------------------------------*/
PROCEDURE setProjectFile:
    DEFINE INPUT PARAMETER cProject     AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE hXMLDoc                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK                     AS LOGICAL      NO-UNDO.


    ASSIGN cProjectFile = cProject.

    EMPTY TEMP-TABLE ttTestItem.
    EMPTY TEMP-TABLE ttUnitTest.

    CREATE X-DOCUMENT hXMLDoc.

    ASSIGN lOK = hXMLDoc:LOAD("file", cProjectFile, FALSE).
    IF NOT lOK THEN DO:
        DELETE OBJECT hXMLDoc.
        RETURN "Could not read configuration file".
    END.

    RUN setProjectXML (hXMLDoc).
    DELETE OBJECT hXMLDoc.

    RETURN RETURN-VALUE.
END PROCEDURE.


/*---------------------------------------------------------------------------------
    Procedure: setPlugins
  Description: Sets suite's active plugins.
----------------------------------------------------------------------------------*/
PROCEDURE setPlugins:
    DEFINE INPUT PARAMETER TABLE FOR ttPlugins.


    RETURN "".
END.


/*---------------------------------------------------------------------------------
    Procedure: setProjectXML
  Description: Sets the suite configuration as a XML file.
----------------------------------------------------------------------------------*/
PROCEDURE setProjectXML:
    DEFINE INPUT PARAMETER hDocument        AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hRoot                   AS HANDLE       NO-UNDO.

    DEFINE VARIABLE lOK                     AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE iCount                  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hPlugins                AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hPlugin                 AS HANDLE       NO-UNDO.


    EMPTY TEMP-TABLE ttTestItem.
    EMPTY TEMP-TABLE ttUnitTest.

    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hPlugins.
    CREATE X-NODEREF hPlugin.
    lOK = hDocument:GET-CHILD(hRoot, 1).

    IF hRoot:NAME <> "ProUnitTestSuite" AND hRoot:NAME <> "PUnitTestSet" THEN DO: /* Compatibility mode */
        DELETE OBJECT hRoot.
        RETURN "Invalid configuration XML.".
    END.

    /* Load Plugins */
    DO iCount = 1 TO hRoot:NUM-CHILDREN:
        hRoot:GET-CHILD(hPlugins, iCount).

        IF hPlugins:NAME = "plugins" THEN DO:
            DO iCount = 1 TO hPlugins:NUM-CHILDREN:
                hPlugins:GET-CHILD(hPlugin, iCount).
                IF SEARCH("prounit/templates/" + hPlugin:GET-ATTRIBUTE("name") + "/template.p") > "" OR SEARCH("prounit/templates/" + hPlugin:GET-ATTRIBUTE("name") + "/template.r") > "" THEN DO:
                    CREATE ttPlugins.
                    ASSIGN  ttPlugins.pluginSeq  = iCount
                            ttPlugins.pluginName = hPlugin:GET-ATTRIBUTE("name")
                            ttPlugins.isActive   = hPlugin:GET-ATTRIBUTE("active") = "yes".
                    VALIDATE ttPlugins.
                    RELEASE ttPlugins.
                END.
            END.
            LEAVE.
        END.
    END.

    /* Loads Items */
    RUN loadItems (hRoot).

    DELETE OBJECT hPlugin.
    DELETE OBJECT hPlugins.
    DELETE OBJECT hRoot.

    RETURN "".
END.


/*---------------------------------------------------------------------------------
    Procedure: setProjectData
----------------------------------------------------------------------------------*/
PROCEDURE setProjectData:
    DEFINE INPUT PARAMETER TABLE-HANDLE hData.

    DEFINE VARIABLE hBuffer             AS HANDLE       NO-UNDO.


    EMPTY TEMP-TABLE ttTestItem.
    EMPTY TEMP-TABLE ttUnitTest.

    ASSIGN hBuffer = BUFFER ttTestItem:HANDLE.

    RUN transferData IN THIS-PROCEDURE (hData:DEFAULT-BUFFER-HANDLE, hBuffer).
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: runSuite
  Description: Start running the suite.
-------------------------------------------------------------------------*/
PROCEDURE runSuite:
    DEFINE VARIABLE hRunner             AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hToolsTT            AS HANDLE           NO-UNDO.
    DEFINE VARIABLE iStatus             AS INTEGER          NO-UNDO.

    DEFINE VARIABLE iSuiteTimer         AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iTestCaseTimer      AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iSuiteStatus        AS INTEGER          NO-UNDO.


    RUN prounit/databaseTools.p PERSISTENT SET hToolsTT.
    SESSION:ADD-SUPER-PROCEDURE(hToolsTT).

    RUN prounit/testRunner.p PERSISTENT SET hRunner.
    RUN addListener IN hRunner (THIS-PROCEDURE).

    /* RUN loadPlugins (INPUT hRunner). */
    RUN activatePlugins (INPUT hRunner).
    RUN fireBeforeRunningSuite (cProjectFile).

    ASSIGN  iSuiteTimer  = TIME
            iSuiteStatus = 0
            lastExecDate = TODAY
            lastExecTime = TIME.

    FOR EACH ttTestItem WHERE ttTestItem.TYPE = {&TEST_CASE} BY ttTestItem.id:
        RUN fireBeforeRunningTestCase (INPUT ttTestItem.id).
        ASSIGN  iCurrentItem    = ttTestItem.id
                iTestCaseTimer  = TIME
                iTestCaseStatus = 0.

        RUN performTest IN hRunner (INPUT ttTestItem.ItemName).

        ASSIGN iTestCaseTimer = TIME - iTestCaseTimer.

        IF iTestCaseStatus > iSuiteStatus THEN
            ASSIGN iSuiteStatus = iTestCaseStatus.

        RUN fireAfterRunningTestCase (INPUT ttTestItem.id,
                                      INPUT iTestCaseTimer,
                                      INPUT iTestCaseStatus).
    END.

    ASSIGN iSuiteTimer = TIME - iSuiteTimer.
    RUN fireAfterRunningSuite (cProjectFile,
                               iSuiteTimer,
                               iSuiteStatus).

    /* Looks for an error */
    IF CAN-FIND(FIRST ttUnitTest WHERE ttUnitTest.runningStatus = {&STATUS_FAIL}) THEN
        ASSIGN iStatus = {&STATUS_FAIL}.
    ELSE IF CAN-FIND(FIRST ttUnitTest WHERE ttUnitTest.runningStatus = {&STATUS_WARNING}) THEN
        ASSIGN iStatus = {&STATUS_WARNING}.
    ELSE
        ASSIGN iStatus = {&STATUS_SUCCESS}.
    /*    RUN unloadPlugins.   */
    DELETE OBJECT hRunner.

    SESSION:REMOVE-SUPER-PROCEDURE(hToolsTT).
    DELETE OBJECT hToolsTT.
    RETURN "".
END PROCEDURE.


/*---------------------------------------------------------------------------
    Procedure: loadPlugins
  Description: Loads plugins in memory.
-------------------------------------------------------------------------*/
PROCEDURE loadPlugins:
    DEFINE VARIABLE hPlugin         AS HANDLE           NO-UNDO.


    /* First, unload any plugin already loaded */
    RUN unloadPlugins.

    FOR EACH ttPlugins WHERE ttPlugins.isActive = TRUE NO-LOCK BY ttPlugins.pluginSeq:
        RUN VALUE("prounit/plugins/" + ttPlugins.pluginName + "/plugin.p") PERSISTENT SET hPlugin NO-ERROR.
        IF NOT ERROR-STATUS:ERROR AND VALID-HANDLE(hPlugin) THEN DO:
            CREATE ttLoadedPlugins.
            ASSIGN  ttLoadedPlugins.hPlugin = hPlugin
                    ttLoadedPlugins.iSeq    = ttPlugins.pluginSeq.
            RUN addPriorityListener IN THIS-PROCEDURE (hPlugin, {&PLUGIN_PRIORITY}). /* Priority for plugins to be notified first */
            RELEASE ttLoadedPlugins.
        END.
    END.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: activatePlugins
  Description: Prepare plugins to be used by the test.
-------------------------------------------------------------------------*/
PROCEDURE activatePlugins:
    DEFINE INPUT PARAMETER hRunner          AS HANDLE       NO-UNDO.


    FOR EACH ttLoadedPlugins BY ttLoadedPlugins.iSeq:
        IF VALID-HANDLE(ttLoadedPlugins.hPlugin) THEN DO:
            hPlugin:ADD-SUPER-PROCEDURE(hRunner).
            hRunner:ADD-SUPER-PROCEDURE(hPlugin).
        END.
    END.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: unloadPlugins
  Description: Unloads plugins
-------------------------------------------------------------------------*/
PROCEDURE unloadPlugins:
    FOR EACH ttLoadedPlugins BY ttLoadedPlugins.iSeq DESC:
        RUN removeListener(ttLoadedPlugins.hPlugin).
        IF VALID-HANDLE(ttLoadedPlugins.hPlugin) THEN
            DELETE OBJECT ttLoadedPlugins.hPlugin.
        DELETE ttLoadedPlugins.
    END.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: addListener
  Description: Adds a listener to this program. The listener receives execution events.
-------------------------------------------------------------------------*/
PROCEDURE addListener:
    DEFINE INPUT PARAMETER hL       AS HANDLE       NO-UNDO.


    RUN addPriorityListener(hL, {&DEFAULT_PRIORITY}).
    RETURN "".
END PROCEDURE.

PROCEDURE addPriorityListener:
    DEFINE INPUT PARAMETER hL           AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER iPriority    AS INTEGER      NO-UNDO.


    IF NOT VALID-HANDLE(hL) THEN
        RETURN.

    FIND ttListener WHERE ttListener.hListener = hL NO-LOCK NO-ERROR.
    IF AVAILABLE ttListener THEN
        RETURN.

    CREATE ttListener.
    ASSIGN  ttListener.hListener = hL
            ttListener.priority  = iPriority.
    RELEASE ttListener.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: removeListener
  Description: Removes a listener to this program.
-------------------------------------------------------------------------*/
PROCEDURE removeListener:
    DEFINE INPUT PARAMETER hL       AS HANDLE       NO-UNDO.


    FIND ttListener WHERE ttListener.hListener = hL EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ttListener THEN
        RETURN.

    DELETE ttListener.
    RETURN "".
END PROCEDURE.


/*-------------------------------------------------------------------------
    Procedure: saveResultsXML
  Description: Saves the test results as XML
-------------------------------------------------------------------------*/
PROCEDURE saveResultsXML PRIVATE:
    DEFINE INPUT PARAMETER cResultFile      AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cTemplate        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE hXMLDoc                 AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hRoot                   AS HANDLE           NO-UNDO.
    DEFINE VARIABLE iCounter                AS INTEGER          NO-UNDO.
    DEFINE VARIABLE hProcessing             AS HANDLE           NO-UNDO.
    DEFINE VARIABLE iGlobalStatus           AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cDestinationFolder      AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cTemplateFileXsl        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lErrorFound             AS LOGICAL          NO-UNDO.


    CREATE X-DOCUMENT hXMLDoc.
    CREATE X-NODEREF hRoot.

    IF cTemplate > "" THEN DO:
        ASSIGN  cResultFile         = REPLACE(cResultFile, "~\", "/")
                cDestinationFolder  = SUBSTRING(cResultFile, 1, R-INDEX(cResultFile, "/"))
                FILE-INFO:FILE-NAME = cDestinationFolder
                cDestinationFolder  = FILE-INFO:FULL-PATHNAME.

        RUN installTemplateXsl (INPUT cTemplate, INPUT cDestinationFolder, OUTPUT cTemplateFileXsl) NO-ERROR.
        ASSIGN lErrorFound = ERROR-STATUS:ERROR.

        IF NOT lErrorFound THEN DO:
            /* As the template files are kept in the same directory as the result file, the template name doesn't matter */
            ASSIGN cTemplate = "template.xsl".

            CREATE X-NODEREF hProcessing.
            hXMLDoc:CREATE-NODE(hProcessing, "xml-stylesheet", "PROCESSING-INSTRUCTION").
            hProcessing:NODE-VALUE = "type=" + QUOTER("text/xsl") + " href=" + QUOTER(cTemplateFileXsl).
            hXMLDoc:APPEND-CHILD(hProcessing).
            DELETE OBJECT hProcessing.
        END.
    END.

    /* Save the document if no error happened */
    IF NOT lErrorFound THEN DO:
        hXMLDoc:CREATE-NODE(hRoot, "ProUnitExecutionLog", "ELEMENT").
        hXMLDoc:APPEND-CHILD(hRoot).
        hRoot:SET-ATTRIBUTE("timestamp", STRING(lastExecDate) + " - " + STRING(lastExecTime, "HH:MM:SS")).

        RUN dumpItems (INPUT hXMLDoc,
                       INPUT 0,
                       INPUT hRoot,
                       OUTPUT iGlobalStatus).
        hroot:SET-ATTRIBUTE("status", getStatusLabel (iGlobalStatus)).

        RUN fireSavingResults (INPUT hXMLDoc).

        ASSIGN lErrorFound = NOT hXMLDoc:SAVE("file", cResultFile) NO-ERROR.
    END.

    DELETE OBJECT hXMLDoc.
    DELETE OBJECT hRoot.

    IF lErrorFound THEN
        RETURN ERROR "".
    RETURN "".
END.


/*-------------------------------------------------------------------------
    Procedure: saveResultsCustom
  Description: Saves the test results using a program.
-------------------------------------------------------------------------*/
PROCEDURE saveResultsCustom PRIVATE:
    DEFINE INPUT PARAMETER cResultFile      AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cTemplate        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE lEndReached             AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE hProgram                AS HANDLE           NO-UNDO.


    saving:
    DO ON ERROR UNDO, LEAVE:
        RUN VALUE("prounit/templates/" + cTemplate + "/template.p") PERSISTENT SET hProgram NO-ERROR.
        IF NOT VALID-HANDLE(hProgram) THEN
            LEAVE saving.

        /* Sends results to the custom program */
        RUN setTestData IN hProgram (INPUT TABLE ttTestItem, INPUT TABLE ttUnitTest).

        /* Asks to save it*/
        RUN saveResults IN hProgram (INPUT cResultFile).
        ASSIGN lEndReached = TRUE.
    END.

    /*
    TODO: fire an event instead of showing a message, to avoid coupling to any specific user interface (gui or chui).
    IF NOT lEndReached THEN
        MESSAGE "ProUnit detected an error while running the template." VIEW-AS ALERT-BOX INFO BUTTONS OK.
    */
    IF NOT lEndReached THEN
        RETURN ERROR "".
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: saveResults
  Description: Saves the execution report.
-------------------------------------------------------------------------*/
PROCEDURE saveResults:
    DEFINE INPUT PARAMETER cResultFile      AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cTemplate        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE cAvailableTemplates     AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cTemplateType           AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE lErrorFound             AS LOGICAL          NO-UNDO.


    RUN getResultTemplates (OUTPUT cAvailableTemplates).

    ASSIGN cTemplate = TRIM(cTemplate).

/*MESSAGE "saveResults" SKIP
        "cResultFile = " + QUOTER(cResultFile) SKIP
        "cTemplate = " + QUOTER(cTemplate) SKIP(1)
        "cAvailableTemplates = " + QUOTER(cAvailableTemplates) SKIP(1)
        "path to search = " + QUOTER("prounit/templates/" + cTemplate + "/template.xsl") SKIP
        "search result = " + QUOTER(SEARCH("prounit/templates/" + cTemplate + "/template.xsl")) SKIP(1)
        REPLACE(PROPATH, ",", CHR(13) + CHR(10))
        VIEW-AS ALERT-BOX WARNING.*/

    /* If the templated received is not a valid template, don't use one */
    IF SEARCH("prounit/templates/" + cTemplate + "/template.p") <> ? OR SEARCH("prounit/templates/" + cTemplate + "/template.r") <> ? THEN DO:
        /*MESSAGE "[1] - Using saveResultcustom because either template.p or template.r has been found." VIEW-AS ALERT-BOX WARNING.*/
        RUN saveResultsCustom (cResultFile, cTemplate).
    END.
    ELSE IF SEARCH("prounit/templates/" + cTemplate + "/template.xsl") <> ? THEN DO:
        /*MESSAGE "[2] - Using saveResultsXML with template name: " + QUOTER(cTemplate) VIEW-AS ALERT-BOX WARNING.*/
        RUN saveResultsXML (cResultFile, cTemplate) NO-ERROR.
        ASSIGN lErrorFound = ERROR-STATUS:ERROR.
    END.
    ELSE DO:
        /*MESSAGE "[3] - Using saveResultsXML without template name." VIEW-AS ALERT-BOX WARNING.*/
        RUN saveResultsXML (cResultFile, "") NO-ERROR.
        ASSIGN lErrorFound = ERROR-STATUS:ERROR.
    END.

    IF NOT lErrorFound THEN DO:
        RUN fireResultsSaved(cResultFile).
        RETURN "".
    END.
    RETURN ERROR "".
END PROCEDURE.


/*-------------------------------------------------------------------------
    Procedure: look4Dir
  Description: go through the propath looking for a specific directory.
-------------------------------------------------------------------------*/
PROCEDURE look4Dir:
    DEFINE INPUT PARAMETER cDirName     AS CHARACTER        NO-UNDO.
    DEFINE OUTPUT PARAMETER cDirPath    AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iCounter            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cPropathEntry       AS CHARACTER        NO-UNDO.


    DO iCounter = 1 TO NUM-ENTRIES(PROPATH):
        FILE-INFO:FILE-NAME = ENTRY(iCounter, PROPATH) + "/" + cDirName.
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            NEXT.

        IF NOT FILE-INFO:FILE-TYPE BEGINS "D" THEN
            NEXT.

        ASSIGN cDirPath = FILE-INFO:FULL-PATHNAME.
        LEAVE.
    END.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: getResultTemplates
  Description: Returns the available templates for execution reports.
-------------------------------------------------------------------------*/
PROCEDURE getResultTemplates:
    DEFINE OUTPUT PARAMETER cTemplates      AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE i                       AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cBaseName               AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cFullPath               AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cAttr                   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cTemplateDir            AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cBuildinTemplates       AS CHARACTER        NO-UNDO     INITIAL "_builtin-CSV,_builtin-IndentedText,_builtin-JUnit".


    ASSIGN cTemplates = "".

    /* Special case where templates are embedded in PL */
    IF INDEX(SEARCH("prounit/suiteRunner.r"), ".pl<<") > 0 THEN DO:
        DO i = 1 TO NUM-ENTRIES(cBuildinTemplates) :
            IF SEARCH("prounit/templates/" + ENTRY(i, cBuildinTemplates) + "/template.r") > "" THEN
                ASSIGN cTemplates = cTemplates + "," + ENTRY(i, cBuildinTemplates).
        END.
    END.

    /* Loog for templates according to directory content */
    RUN look4Dir (INPUT "prounit/templates", OUTPUT cTemplateDir).
    IF cTemplateDir > "" AND cTemplateDir <> "?" THEN DO:
        ASSIGN FILE-INFO:FILE-NAME = cTemplateDir.
        IF FILE-INFO:FULL-PATHNAME > "" THEN DO:
            INPUT FROM OS-DIR(FILE-INFO:FULL-PATHNAME).
            REPEAT:
                IMPORT cBaseName cFullPath cAttr.

                /* Skip directories */
                IF cBaseName = "." OR cBaseName = ".." OR cAttr <> "D"  THEN
                    NEXT.

                /* XSL-Based TEMPLATE */
                ASSIGN FILE-INFO:FILE-NAME = cFullPath + "/template.xsl".
                IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
                    ASSIGN cTemplates = cTemplates + "," + cBaseName.
                    NEXT.
                END.

                /* Program-based template, source code */
                ASSIGN FILE-INFO:FILE-NAME = cFullPath + "/template.p".
                IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
                    ASSIGN cTemplates = cTemplates + "," + cBaseName.
                    NEXT.
                END.

                /* Program-based template, runnable code */
                ASSIGN FILE-INFO:FILE-NAME = cFullPath + "/template.r".
                IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
                    ASSIGN cTemplates = cTemplates + "," + cBaseName.
                    NEXT.
                END.
            END.
            INPUT CLOSE.
        END.
    END.

    /* Ensure there is no empty entry */
    ASSIGN cTemplates = TRIM(cTemplates, ",").
    RETURN "".
END PROCEDURE.


/*---------------------------------------------------------------------------
    Procedure: addTestItem
  Description: Adds an item to the test suite
-------------------------------------------------------------------------*/
PROCEDURE addTestItem:
    DEFINE INPUT PARAMETER iSeq         AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER iType        AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER cName        AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER iParentItem  AS INTEGER      NO-UNDO.


    CREATE ttTestItem.
    ASSIGN  ttTestItem.id           = iSeq
            ttTestItem.TYPE         = iType
            ttTestItem.iTemName     = cName
            ttTestItem.parentSet    = iParentItem.
    RELEASE ttTestItem.
    RETURN "".
END.

/*------------------------------------------------------------------------------------------*/
/*-------------------------------------- PRIVATE METHODS -----------------------------------*/
/*------------------------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------
    Procedure: loadItems
  Description: loadItems of a XML node.
-------------------------------------------------------------------------*/
PROCEDURE loadItems:
    DEFINE INPUT PARAMETER hParentElement       AS HANDLE       NO-UNDO.

    DEFINE VARIABLE iCount                  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hEntry                  AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK                     AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE cName                   AS CHARACTER    NO-UNDO.


    CREATE X-NODEREF hEntry.

    DO iCount = 1 TO hParentElement:NUM-CHILDREN:
        ASSIGN lOK = hParentElement:GET-CHILD(hEntry, iCount).
        IF NOT lOK THEN
            NEXT.

        IF hEntry:SUBTYPE <> "ELEMENT" THEN
            NEXT.

        IF hEntry:NAME = "plugins" THEN
            NEXT.

        ASSIGN cName = hEntry:GET-ATTRIBUTE("name").
        IF cName = "" THEN /* Tries old version attribute - testcase */
            ASSIGN cName = hEntry:GET-ATTRIBUTE("testcase").

        RUN addTestItem(    INPUT INTEGER(hEntry:GET-ATTRIBUTE("seq")),
                            INPUT (IF hEntry:NAME = "TestSet" THEN {&TEST_SET} ELSE {&TEST_CASE}),
                            INPUT cName,
                            INPUT INTEGER(hParentElement:GET-ATTRIBUTE("seq"))).

        IF hEntry:NAME = "TestSet" THEN
            RUN loadItems (hEntry).
    END.

    DELETE OBJECT hEntry.
    RETURN "".
END PROCEDURE.


PROCEDURE runningTest:
    DEFINE INPUT PARAMETER cUnitTestName    AS CHARACTER    NO-UNDO.

    RUN fireBeforeRunningTest (iCurrentItem, cUnitTestName).
    RETURN "".
END.

PROCEDURE testRun:
    DEFINE INPUT PARAMETER cUnitTestName    AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER lResult          AS LOGICAL      NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER      NO-UNDO.

    DEFINE BUFFER bTest     FOR ttUnitTest.

    DEFINE VARIABLE iStatus                 AS INTEGER      NO-UNDO.


    IF lResult THEN
        ASSIGN iStatus = {&STATUS_SUCCESS}.
    ELSE IF lResult = NO THEN
        ASSIGN iStatus = {&STATUS_FAIL}.
    ELSE
        ASSIGN iStatus = {&STATUS_WARNING}.

    IF iStatus > iTestCaseStatus THEN
        ASSIGN iTestCaseStatus = iStatus.

    FIND LAST bTest WHERE bTest.testItemId = iCurrentItem NO-LOCK NO-ERROR.
    CREATE ttUnitTest.
    ASSIGN  ttUnitTest.testItemId        = iCurrentItem
            ttUnitTest.seq               = (IF AVAILABLE bTest THEN bTest.seq ELSE 0) + 1
            ttUnitTest.NAME              = cUnitTestName
            ttUnitTest.runningStatus     = iStatus
            ttUnitTest.txtMessage        = cMessage
            ttUnitTest.firstAssertFailed = iFirstAssert
            ttUnitTest.totalTime         = iTotalTime.

    RUN UpdateStatus (iCurrentItem, iStatus).

    RUN fireAfterRunningTest (iCurrentItem,
                               cUnitTestName,
                               iStatus,
                               cMessage,
                               iFirstAssert,
                               iTotalTime).
    RELEASE ttUnitTest.
    RETURN "".
END.


/*---------------------------------------------------------------------------
    Procedure: dumpItems
  Description: Write result tests
-------------------------------------------------------------------------*/
PROCEDURE dumpItems :
    DEFINE INPUT PARAMETER hXMLDoc      AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER iParentId    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER hParentNode  AS HANDLE       NO-UNDO.
    DEFINE OUTPUT PARAMETER iStatus     AS INTEGER      NO-UNDO.

    DEFINE BUFFER bTests FOR ttTestItem.
    DEFINE VARIABLE hEntry                  AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hResult                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cElementName            AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE iChildStatus            AS INTEGER      NO-UNDO.

    CREATE X-NODEREF hEntry.


    ASSIGN iStatus = {&STATUS_SUCCESS}.

    FOR EACH bTests WHERE bTests.parentSet = iParentId NO-LOCK:
        IF bTests.TYPE = {&TEST_SET} THEN
            ASSIGN cElementName = "TestSet".
        ELSE
            ASSIGN cElementName = "TestCase".

        hXMLDoc:CREATE-NODE(hEntry, cElementName, "ELEMENT").
        hParentNode:APPEND-CHILD(hEntry).
        hEntry:SET-ATTRIBUTE("seq", STRING(bTests.id)).
        hEntry:SET-ATTRIBUTE("name", bTests.ItemName).

        IF bTests.TYPE = {&TEST_SET} THEN DO:
            RUN dumpItems(INPUT hXMLDoc,
                          INPUT bTests.id,
                          INPUT hEntry,
                          OUTPUT iChildStatus).
            IF iChildStatus > iStatus THEN
                ASSIGN iStatus = iChildStatus.
        END.
        ELSE DO:
            CREATE X-NODEREF hResult.

            FOR EACH ttUnitTest WHERE ttUnitTest.testItemId = bTests.id NO-LOCK:
                ASSIGN iChildStatus = ttUnitTest.runningStatus.

                IF iChildStatus > iStatus THEN
                    ASSIGN iStatus = iChildStatus.

                hXMLDoc:CREATE-NODE(hResult, "Test", "ELEMENT").
                hResult:SET-ATTRIBUTE("seq", STRING(ttUnitTest.seq)).
                hResult:SET-ATTRIBUTE("name", ttUnitTest.Name).
                hResult:SET-ATTRIBUTE("status", getStatusLabel (iChildStatus)).
                hResult:SET-ATTRIBUTE("message", ttUnitTest.txtMessage).
                hResult:SET-ATTRIBUTE("FirstAssertFailed", STRING(ttUnitTest.firstAssertFailed)).
                hResult:SET-ATTRIBUTE("TotalTime", STRING(ttUnitTest.totalTime)).
                hEntry:APPEND-CHILD(hResult).
            END.

            DELETE OBJECT hResult.
        END.
        hEntry:SET-ATTRIBUTE("status", getStatusLabel (bTests.runningStatus)).
    END.

    DELETE OBJECT hEntry.
    RETURN "".
END PROCEDURE.



/* -------------------------------------------------------------------------- */
/* installTemplateXsl                                                            */
/* -------------------------------------------------------------------------- */
PROCEDURE installTemplateXsl:
    DEFINE INPUT PARAMETER cTemplate        AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER cDestFolder      AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER cOutXslFile     AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cFileName               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cFullName               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lDirectoryCopied        AS LOGICAL      NO-UNDO     INITIAL FALSE.


    ASSIGN cFileName = REPLACE(SEARCH("prounit/templates/" + cTemplate + "/template.xsl"), "~\", "/").
    IF cFileName > "" THEN DO:
        ASSIGN FILE-INFO:FILE-NAME = SUBSTRING(cFileName, 1, R-INDEX(cFileName, "/")).
        INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME) NO-ATTR-LIST.
        REPEAT:
            IMPORT cFileName cFullName.
            IF cFileName = "." OR cFileName = ".." THEN
                NEXT.

            IF cFileName = "template.xsl" THEN DO:
                IF LENGTH(cTemplate) > 2 AND SUBSTRING(cTemplate, 2, 1) = "-" THEN
                    ASSIGN  cTemplate = SUBSTRING(cTemplate, 3).
                ASSIGN cOutXslFile = "template_" + LC(cTemplate) + ".xsl".
                OS-COPY VALUE(cFullName) VALUE(cDestFolder + "/" + cOutXslFile).
            END.
            ELSE
                OS-COPY VALUE(cFullName) VALUE(cDestFolder + "/" + cFileName).
            ASSIGN lDirectoryCopied = TRUE.
        END.
        INPUT CLOSE.
    END.

    IF NOT lDirectoryCopied THEN
        RETURN ERROR "".
    RETURN "".
END.


/* -------------------------------------------------------------------------- */
/* updateStatus                                                               */
/* -------------------------------------------------------------------------- */
PROCEDURE updateStatus:
    DEFINE INPUT PARAMETER iTestItem        AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER      NO-UNDO.

    DEFINE BUFFER bItem     FOR ttTestItem.
    DEFINE BUFFER bParent   FOR ttTestItem.


    IF iStatus = {&STATUS_SUCCESS} THEN
        RETURN.

    FIND bItem WHERE bItem.id = iTestItem EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bItem THEN
        RETURN.

    IF bItem.runningStatus < iStatus THEN
        ASSIGN bItem.runningStatus = iStatus.

    RUN updateStatus (bItem.parentSet, iStatus).
    RETURN "".
END.


/* -------------------------------------------------------------------------- */
/* transferData                                                               */
/* -------------------------------------------------------------------------- */
PROCEDURE transferData :
    DEFINE INPUT PARAMETER hSourceBuffer            AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER hDestBuffer              AS HANDLE       NO-UNDO.

    DEFINE VARIABLE hQuery                          AS HANDLE       NO-UNDO.


    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hSourceBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + hSourceBuffer:NAME + " NO-LOCK").
    hQuery:QUERY-OPEN().

    hQuery:GET-FIRST().
    DO WHILE hSourceBuffer:AVAILABLE:
        hDestBuffer:BUFFER-CREATE().
        hDestBuffer:BUFFER-COPY(hSourceBuffer).
        hDestBuffer:BUFFER-RELEASE().
        hQuery:GET-NEXT().
    END.
    hQuery:QUERY-CLOSE().
    DELETE OBJECT hQuery.

    RETURN "".
END PROCEDURE.


/* -------------------------------------------------------------------------- */
/* EVENTS                                                                     */
/* -------------------------------------------------------------------------- */
PROCEDURE fireBeforeRunningSuite:
    DEFINE INPUT PARAMETER cFile            AS CHARACTER        NO-UNDO.
    DEFINE BUFFER ttListener FOR ttListener.


    FOR EACH ttListener USE-INDEX priority:
        RUN beforeRunningSuite IN hListener (INPUT cFile) NO-ERROR.
    END.
    RETURN "".
END.


PROCEDURE fireBeforeRunningTestCase:
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.

    DEFINE BUFFER bTestItem     FOR ttTestItem.
    DEFINE BUFFER ttListener    FOR ttListener.


    FIND bTestItem WHERE bTestItem.id = iItemId NO-LOCK NO-ERROR.
    FOR EACH ttListener USE-INDEX priority:
        RUN beforeRunningTestCase IN hListener (INPUT iItemId,  INPUT bTestItem.itemName) NO-ERROR.
    END.
    RETURN "".
END.

PROCEDURE fireBeforeRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.


    FOR EACH ttListener USE-INDEX priority:
        RUN beforeRunningTest IN hListener (INPUT iTestCaseId, INPUT cTestName) NO-ERROR.
    END.
    RETURN "".
END.


PROCEDURE fireAfterRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER          NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.


    FOR EACH ttListener USE-INDEX priority:
        RUN afterRunningTest IN hListener (iCurrentItem, cTestName, iStatus, cMessage, iFirstAssert, iTotalTime) NO-ERROR.
    END.
    RETURN "".
END.

PROCEDURE fireAfterRunningTestCase:
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.


    IF iStatus = 0 THEN
        ASSIGN iStatus = {&STATUS_WARNING}.

    FOR EACH ttListener USE-INDEX priority:
        RUN AfterRunningTestCase IN hListener (INPUT iItemId, INPUT iTimeSpent, INPUT iStatus) NO-ERROR.
    END.
    RETURN "".
END.


PROCEDURE fireAfterRunningSuite:
    DEFINE INPUT PARAMETER cFile        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.

    FOR EACH ttListener USE-INDEX priority:
        RUN afterRunningSuite IN hListener (INPUT cFile, INPUT iTimeSpent, INPUT iStatus) NO-ERROR.
    END.
    RETURN "".
END.


PROCEDURE fireSavingResults:
    DEFINE INPUT PARAMETER hDocument      AS HANDLE         NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.

    FOR EACH ttListener USE-INDEX priority:
        RUN savingResults IN hListener (INPUT hDocument) NO-ERROR.
    END.
    RETURN "".
END.


PROCEDURE fireResultsSaved:
    DEFINE INPUT PARAMETER cFile        AS CHARACTER        NO-UNDO.

    DEFINE BUFFER ttListener FOR ttListener.


    FOR EACH ttListener USE-INDEX priority:
        RUN resultsSaved IN hListener (INPUT cFile) NO-ERROR.
    END.
    RETURN "".
END.
