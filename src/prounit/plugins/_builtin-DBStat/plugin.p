/*****************************************************************************
**     Program: DBMononitor Plugin (plugin.p)
** Description: ProUnit's plugin for monitoring database access on tests.
*****************************************************************************/

&SCOPED-DEFINE BEFORE_MODE  1
&SCOPED-DEFINE AFTER_MODE   2

DEFINE TEMP-TABLE ttSnapshot    NO-UNDO
    FIELD db-name       AS CHARACTER
    FIELD table-id      AS INTEGER

    FIELD readsBefore   AS INTEGER
    FIELD updatesBefore AS INTEGER
    FIELD createsBefore AS INTEGER
    FIELD deletesBefore AS INTEGER

    FIELD reads         AS INTEGER
    FIELD updates       AS INTEGER
    FIELD creates       AS INTEGER
    FIELD deletes       AS INTEGER

    INDEX snapshots IS PRIMARY UNIQUE
        db-name
        table-id.

DEFINE TEMP-TABLE ttActivitySummary
    FIELD testCaseId        AS INTEGER
    FIELD testName          AS CHARACTER
    FIELD db-name           AS CHARACTER
    FIELD table-name        AS CHARACTER
    FIELD readCount         AS INTEGER
    FIELD updateCount       AS INTEGER
    FIELD createCount       AS INTEGER
    FIELD deleteCount       AS INTEGER
    INDEX main  IS PRIMARY UNIQUE
        testCaseId
        testName
        db-name
        table-name.

/*---------------------------------------------------------------------------
    Procedure: beforeRunningSuite
  Description: Resets statistics.
-------------------------------------------------------------------------*/
PROCEDURE beforeRunningSuite:
    DEFINE INPUT PARAMETER cFile            AS CHARACTER        NO-UNDO.
    EMPTY TEMP-TABLE ttSnapshot.
    EMPTY TEMP-TABLE ttActivitySummary.
END.

/*---------------------------------------------------------------------------
    Procedure: beforeRunningTest
  Description: Resets statistics.
-------------------------------------------------------------------------*/
PROCEDURE beforeRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId          AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER cTestName            AS CHARACTER    NO-UNDO.

    RUN takePicture ({&BEFORE_MODE}).
END PROCEDURE.

/*---------------------------------------------------------------------------
    Procedure: afterRunningTest
  Description: Accounts database's activity for the test.
---------------------------------------------------------------------------*/  
PROCEDURE afterRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER          NO-UNDO.

    RUN takePicture ({&AFTER_MODE}).
    RUN accountChanges (INPUT iTestCaseId,
                        INPUT cTestName).
END.

PROCEDURE savingResults:
    DEFINE INPUT PARAMETER hXMLDoc          AS HANDLE         NO-UNDO.

    DEFINE VARIABLE hDocRoot                AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hRoot                   AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hTCEntry                AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hTestEntry              AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hStat                   AS HANDLE           NO-UNDO.

    IF NOT VALID-HANDLE(hXMLDoc) THEN
        RETURN.

    CREATE X-NODEREF  hDocRoot.
    CREATE X-NODEREF  hRoot.
    CREATE X-NODEREF  hTCEntry.
    CREATE X-NODEREF  hTestEntry.
    CREATE X-NODEREF  hStat.

    hXMLDoc:GET-DOCUMENT-ELEMENT(hDocRoot). 
    hXMLDoc:CREATE-NODE(hRoot, "DBMonitor", "ELEMENT").
    hDocRoot:APPEND-CHILD(hRoot).

    FOR EACH ttActivitySummary
        BREAK BY testCaseId
              BY testName:

        /* Test Case Node */
        IF FIRST-OF(ttActivitySummary.testCaseId) THEN DO:
            hXMLDoc:CREATE-NODE(hTCEntry, "TestCase", "ELEMENT").
            hRoot:APPEND-CHILD(hTCEntry).
            hTCEntry:SET-ATTRIBUTE("id", STRING(ttActivitySummary.testCaseId)).
        END.

        /* Test procedure Node */
        IF FIRST-OF(ttActivitySummary.testName) THEN DO:
            hXMLDoc:CREATE-NODE(hTestEntry, "TestName", "ELEMENT").
            hTCEntry:APPEND-CHILD(hTestEntry).
            hTestEntry:SET-ATTRIBUTE("name", ttActivitySummary.testName).
        END.

        hXMLDoc:CREATE-NODE(hStat, "DBStat", "ELEMENT").
        hTestEntry:APPEND-CHILD(hStat).
        hStat:SET-ATTRIBUTE("table"  , ttActivitySummary.db-name + "." + ttActivitySummary.table-name).
        hStat:SET-ATTRIBUTE("reads"  , STRING(ttActivitySummary.readCount)).
        hStat:SET-ATTRIBUTE("updates", STRING(ttActivitySummary.updateCount)).
        hStat:SET-ATTRIBUTE("creates", STRING(ttActivitySummary.createCount)).
        hStat:SET-ATTRIBUTE("deletes", STRING(ttActivitySummary.deleteCount)).
    END.

    DELETE OBJECT hDocRoot.
    DELETE OBJECT hRoot.
    DELETE OBJECT hStat.
    DELETE OBJECT hTestEntry.
    DELETE OBJECT hTCEntry.
END.

/*--------------------------------------------------------------------*/
/*------------------------ Internal procedures -----------------------*/
/*--------------------------------------------------------------------*/

PROCEDURE takePicture:
    DEFINE INPUT PARAMETER iMode            AS INTEGER              NO-UNDO.

    DEFINE VARIABLE hTTBuffer               AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hBuffer                 AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hQuery                  AS HANDLE               NO-UNDO.
    DEFINE VARIABLE iCounter                AS INTEGER              NO-UNDO.
    DEFINE VARIABLE cStatFields             AS CHARACTER EXTENT 4   NO-UNDO
                INITIAL ["_TableStat-read", 
                         "_TableStat-update", 
                         "_TableStat-create", 
                         "_TableStat-delete"].
    DEFINE VARIABLE cBeforeFields           AS CHARACTER EXTENT 4   NO-UNDO
                INITIAL ["readsBefore", 
                         "updatesBefore", 
                         "createsBefore", 
                         "deletesBefore"].
    DEFINE VARIABLE cAfterFields            AS CHARACTER EXTENT 4   NO-UNDO
                INITIAL ["reads",
                         "updates", 
                         "creates", 
                         "deletes"].
          
    DEFINE VARIABLE hStatId                 AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hStatField              AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hTTField                AS HANDLE               NO-UNDO.
    DEFINE VARIABLE iCount                  AS INTEGER              NO-UNDO.
    DEFINE VARIABLE cDatabase               AS CHARACTER            NO-UNDO.

    DO iCount = 1 TO NUM-DBS:
        cDatabase = LDBNAME(iCount).

        CREATE BUFFER hBuffer FOR TABLE cDatabase + "._TableStat" NO-ERROR.
        IF NOT VALID-HANDLE(hBuffer) THEN
            RETURN.
    
        ASSIGN
            hStatId     = hBuffer:BUFFER-FIELD("_TableStat-id")
            hTTBuffer   = BUFFER ttSnapshot:HANDLE.
    
        CREATE QUERY hQuery.
        hQuery:ADD-BUFFER(hBuffer).
        hQuery:QUERY-PREPARE("FOR EACH _TableStat").
        hQuery:QUERY-OPEN().
    
        hQuery:GET-FIRST(NO-LOCK).
        DO WHILE hBuffer:AVAILABLE:
            FIND ttSnapshot
                WHERE ttSnapshot.db-name  = cDatabase
                  AND ttSnapshot.table-id = hStatId:BUFFER-VALUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ttSnapshot THEN DO:
                CREATE ttSnapshot.
                ASSIGN
                    ttSnapshot.db-name  = cDatabase           
                    ttSnapshot.table-id = hStatId:BUFFER-VALUE.
            END.
    
            DO iCounter = 1 TO 4:
                hStatField = hBuffer:BUFFER-FIELD(cStatFields[iCounter]).
                
                IF iMode = {&BEFORE_MODE} THEN
                    hTTField = hTTBuffer:BUFFER-FIELD(cBeforeFields[iCounter]).
                ELSE
                    hTTField = hTTBuffer:BUFFER-FIELD(cAfterFields[iCounter]).
    
                hTTField:BUFFER-VALUE = hStatField:BUFFER-VALUE.
                
            END.
            hQuery:GET-NEXT(NO-LOCK).
        END.
    
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        DELETE OBJECT hBuffer.
    END.
END.

PROCEDURE getTableName:
    DEFINE INPUT PARAMETER cDatabase        AS CHARACTER            NO-UNDO.
    DEFINE INPUT PARAMETER iTableId         AS INTEGER              NO-UNDO.
    DEFINE OUTPUT PARAMETER cTableName      AS CHARACTER            NO-UNDO.

    DEFINE VARIABLE hBuffer                 AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hQuery                  AS HANDLE               NO-UNDO.
    DEFINE VARIABLE hField                  AS HANDLE               NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE cDatabase + "._File" NO-ERROR.
    IF NOT VALID-HANDLE(hBuffer) THEN
        RETURN.

    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH _File WHERE _File._File-number = " + STRING(iTableId)).
    hQuery:QUERY-OPEN().

    hQuery:GET-FIRST(NO-LOCK).
    IF hBuffer:AVAILABLE THEN DO:
        ASSIGN
            hField     = hBuffer:BUFFER-FIELD("_File-name")
            cTableName = hField:BUFFER-VALUE.
    END.
    hQuery:QUERY-CLOSE().

    DELETE OBJECT hQuery.
    DELETE OBJECT hBuffer.
END.

PROCEDURE accountChanges:
    DEFINE INPUT PARAMETER cTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE cTableName              AS CHARACTER        NO-UNDO.

    FOR EACH ttSnapshot
        NO-LOCK:

        /* Se não mudou nada, desconsidera */
        IF ttSnapshot.reads   = ttSnapshot.readsBefore   AND
           ttSnapshot.creates = ttSnapshot.createsBefore AND
           ttSnapshot.updates = ttSnapshot.updatesBefore AND
           ttSnapshot.deletes = ttSnapshot.deletesBefore THEN
            NEXT.

        RUN getTableName (INPUT ttSnapshot.db-name, 
                          INPUT ttSnapshot.table-id, 
                          OUTPUT cTableName).

        IF NOT CAN-FIND(ttActivitySummary
                        WHERE ttActivitySummary.testCaseId  = cTestCaseId       
                          AND ttActivitySummary.testName    = cTestName         
                          AND ttActivitySummary.db-name     = ttSnapshot.db-name
                          AND ttActivitySummary.table-name  = cTableName) THEN DO:
            CREATE ttActivitySummary.
            ASSIGN
                ttActivitySummary.testCaseId  = cTestCaseId      
                ttActivitySummary.testName    = cTestName         
                ttActivitySummary.db-name     = ttSnapshot.db-name 
                ttActivitySummary.table-name  = cTableName
                ttActivitySummary.readCount   = ttSnapshot.reads   - ttSnapshot.readsBefore    
                ttActivitySummary.updateCount = ttSnapshot.updates - ttSnapshot.updatesBefore
                ttActivitySummary.createCount = ttSnapshot.creates - ttSnapshot.createsBefore    
                ttActivitySummary.deleteCount = ttSnapshot.deletes - ttSnapshot.deletesBefore.
        END.

    END.
END.


