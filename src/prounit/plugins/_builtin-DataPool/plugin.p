/*
*/

DEFINE TEMP-TABLE ttPoolInfo
    FIELD poolId    AS CHARACTER
    FIELD poolPos   AS INTEGER INITIAL 0
    FIELD poolHdl   AS HANDLE
    INDEX principal is PRIMARY UNIQUE
        poolId.

DEFINE VARIABLE hDocument    AS HANDLE        NO-UNDO.
DEFINE VARIABLE hRoot        AS HANDLE        NO-UNDO.

PROCEDURE beforeRunningTestCase:
    DEFINE INPUT PARAMETER iItemId    AS INTEGER        NO-UNDO.
    DEFINE INPUT PARAMETER cItemName  AS CHARACTER 	    NO-UNDO. 
    
    DEFINE VARIABLE cDataFile         AS CHARACTER      NO-UNDO.
    
    cDataFile = REPLACE(cItemName, ".p", ".xml").
    
    IF SEARCH(cDataFile) <> ? THEN DO:
        CREATE X-DOCUMENT hDocument.
        hDocument:LOAD("file", SEARCH(cDataFile), FALSE).
    END.
END.


PROCEDURE beforeRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId        AS INTEGER    NO-UNDO.    
    DEFINE INPUT PARAMETER cTestName          AS CHARACTER  NO-UNDO.
    
    DEFINE VARIABLE hParentRoot               AS HANDLE     NO-UNDO.
    DEFINE VARIABLE iCount                    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE lFound                    AS LOGICAL    NO-UNDO.
    
    IF NOT VALID-HANDLE(hDocument) THEN
        RETURN.
        
    CREATE X-NODEREF hParentRoot.
    CREATE X-NODEREF hRoot.
        
    hDocument:GET-DOCUMENT-ELEMENT(hParentRoot).
    
    finding:
    DO iCount = 1 TO hParentRoot:NUM-CHILDREN:
        hParentRoot:GET-CHILD(hRoot, iCount).
        IF hRoot:SUBTYPE <> "ELEMENT" THEN
            NEXT.
            
        IF hRoot:GET-ATTRIBUTE("test") = cTestName THEN DO:
            lFound = TRUE.
            LEAVE finding.
        END.
    END.
    
    IF NOT lFound THEN
        DELETE OBJECT hRoot.
    DELETE OBJECT hParentRoot.
    
    EMPTY TEMP-TABLE ttPoolInfo.
    
    CREATE ttPoolInfo.
    ASSIGN
        ttPoolInfo.poolId  = "/"
        ttPoolInfo.poolHdl = hRoot.
END.

PROCEDURE afterRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER          NO-UNDO.
    
    DEFINE BUFFER ttPoolInfo FOR ttPoolInfo.
    
    IF VALID-HANDLE(hRoot) THEN
        DELETE OBJECT hRoot.
        
    FOR EACH ttPoolInfo
        EXCLUSIVE-LOCK:
        IF VALID-HANDLE(ttPoolInfo.poolHdl) THEN
            DELETE OBJECT ttPoolInfo.poolHdl.
        DELETE ttPoolInfo.
    END.
END.

PROCEDURE afterRunningTestCase:
    DEFINE INPUT PARAMETER iItemId    AS INTEGER        NO-UNDO.
    DEFINE INPUT PARAMETER cItemName  AS CHARACTER 	    NO-UNDO. 
    DEFINE INPUT PARAMETER iStatus    AS INTEGER        NO-UNDO.
    
    IF VALID-HANDLE(hDocument) THEN
        DELETE OBJECT hDocument.
END.

/************/

PROCEDURE __Datapool.iterate:
    DEFINE INPUT PARAMETER cPath        AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER lAvailable  AS LOGICAL      NO-UNDO.
    
    DEFINE BUFFER bInnerPool FOR ttPoolInfo.
    DEFINE BUFFER ttPoolInfo FOR ttPoolInfo.
    
    DEFINE VARIABLE hPoolRecord         AS HANDLE       NO-UNDO.
    DEFINE VARIABLE iCounter            AS INTEGER      NO-UNDO.

    FIND ttPoolInfo
        WHERE ttPoolInfo.poolId = cPath
          AND VALID-HANDLE(ttPoolInfo.poolHdl)
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttPoolInfo THEN
        RETURN.
    
    IF ttPoolInfo.poolHdl:NUM-CHILDREN < ttPoolInfo.poolPos THEN
        RETURN.
        
    /* Delete references to inner pools */
    RUN clearChildren (ttPoolInfo.poolId).
    
    CREATE X-NODEREF hPoolRecord.
    checking:
    DO iCounter = ttPoolInfo.poolPos + 1 TO ttPoolInfo.poolHdl:NUM-CHILDREN:
        ttPoolInfo.poolHdl:GET-CHILD(hPoolRecord, iCounter).
        ttPoolInfo.poolPos = iCounter.
        
        IF hPoolRecord:SUBTYPE <> "ELEMENT"    OR
           hPoolRecord:NAME    <> "PoolRecord" THEN
            NEXT.
            
        LEAVE checking.
    END.
    
    lAvailable = (iCounter <= ttPoolInfo.poolHdl:NUM-CHILDREN).
        
    /* Load Inner Pools */
    IF lAvailable THEN DO:
        RUN inspectDataPools (INPUT ttPoolInfo.poolId + IF ttPoolInfo.poolId <> "/" THEN "/" ELSE "",
                              INPUT hPoolRecord).
    END.
    DELETE OBJECT hPoolRecord.
END.

PROCEDURE __Datapool.getField:
    DEFINE INPUT PARAMETER cPath        AS CHARACTER    NO-UNDO.
    DEFINE OUTPUT PARAMETER cValue      AS CHARACTER    NO-UNDO.
    
    DEFINE VARIABLE hPoolRecord         AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hFieldTag           AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hValue              AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cFieldName          AS CHARACTER    NO-UNDO.
    
    DEFINE VARIABLE iCount              AS INTEGER      NO-UNDO.
    
    ASSIGN
        cFieldName = SUBSTRING(cPath, R-INDEX(cPath, "/") + 1)
        cPath      = SUBSTRING(cPath, 1, MAX(R-INDEX(cPath, "/") - 1, 1)).
        
    FIND ttPoolInfo
        WHERE ttPoolInfo.poolId = cPath
          AND VALID-HANDLE(ttPoolInfo.poolHdl)
        NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE ttPoolInfo THEN
        RETURN.
        
    CREATE X-NODEREF hPoolRecord.
    CREATE X-NODEREF hFieldTag.
    CREATE X-NODEREF hValue.
    
    ttPoolInfo.poolHdl:GET-CHILD(hPoolRecord, ttPoolInfo.poolPos).

    DO iCount = 1 TO hPoolRecord:NUM-CHILDREN:
        hPoolRecord:GET-CHILD(hFieldTag, iCount).
            
        IF hFieldTag:SUBTYPE <> "ELEMENT" THEN
            NEXT.

        IF hFieldTag:SUBTYPE      = "ELEMENT"  AND
           hFieldTag:NAME         = cFieldName AND
           hFieldTag:NUM-CHILDREN > 0          THEN DO:
            hFieldTag:GET-CHILD(hValue, 1).
             cValue = hValue:NODE-VALUE.
        END.
    END.
    
    DELETE OBJECT hValue.
    DELETE OBJECT hFieldTag.
    DELETE OBJECT hPoolRecord.
END.

PROCEDURE __Datapool.reset:
    DEFINE INPUT PARAMETER cPath        AS CHARACTER    NO-UNDO.
    
    FIND ttPoolInfo
        WHERE ttPoolInfo.poolId = cPath
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ttPoolInfo THEN DO:
        RUN clearChildren(ttPoolInfo.poolId).
        ttPoolInfo.poolPos = 0.
    END.
END.

PROCEDURE clearChildren:
    DEFINE INPUT PARAMETER cParentId       AS CHARACTER    NO-UNDO.
    
    DEFINE BUFFER bInnerPool FOR ttPoolInfo.
    
    FOR EACH bInnerPool
        WHERE bInnerPool.poolId BEGINS cParentId
          AND bInnerPool.poolId <> cParentId
        EXCLUSIVE-LOCK:
    
        IF VALID-HANDLE(bInnerPool.poolHdl) THEN
            DELETE OBJECT bInnerPool.poolHdl.
        DELETE bInnerPool.
    END.
END.

PROCEDURE inspectDataPools PRIVATE:
    DEFINE INPUT PARAMETER cCurrentPath    AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER hElement        AS HANDLE       NO-UNDO.
    
    DEFINE VARIABLE iChild                 AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hChild                 AS HANDLE       NO-UNDO.
    
    DEFINE BUFFER ttPoolInfo FOR ttPoolInfo.
    
    CREATE X-NODEREF hChild.
    
    DO iChild = 1 TO hElement:NUM-CHILDREN:
        hElement:GET-CHILD(hChild, iChild).
            
        IF NOT hChild:SUBTYPE = "Element"  OR
           NOT hChild:NAME    = "TestPool" THEN
            NEXT.
            
        /* Found a TestPool */
        CREATE ttPoolInfo.
        ASSIGN
            ttPoolInfo.poolId = cCurrentPath + hChild:GET-ATTRIBUTE("name").
                    
        CREATE X-NODEREF ttPoolInfo.poolHdl.
        hElement:GET-CHILD(ttPoolInfo.poolHdl, iChild).
    END.
    
    DELETE OBJECT hChild.
END.
