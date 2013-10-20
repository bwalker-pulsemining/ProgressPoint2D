/**********************************************************************
** Program: MemMonitor (plugin.p)
** Description: ProUnit plugin for monitoring persistent procedures.
**********************************************************************/

DEFINE TEMP-TABLE ttDynObjects      NO-UNDO
    FIELD objectType                AS CHARACTER
    FIELD ObjectHandle             AS HANDLE.

DEFINE TEMP-TABLE ttNewObjects   NO-UNDO LIKE ttDynObjects.

DEFINE TEMP-TABLE ttAccounting      NO-UNDO
    FIELD objectType                AS CHARACTER
    FIELD objectName                AS CHARACTER
    FIELD instances                 AS INTEGER
    INDEX main
        objectType
        objectName.

PROCEDURE MemMonitor.restart:
    RUN startAccounting.
END PROCEDURE.


/* Event Procedures */           
           
PROCEDURE beforeRunningTestCase:
    DEFINE INPUT PARAMETER iItemId          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cItemName        AS CHARACTER        NO-UNDO.

    RUN startAccounting.
END.

PROCEDURE afterRunningTestCase:
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.

    RUN checkMemoryLeak.
END.


/******************** PRIVATE METHODS ************************/

PROCEDURE checkMemoryLeak PRIVATE:
    DEFINE VARIABLE cObjects         AS CHARACTER   NO-UNDO.

    RUN checkObjects.
    IF NOT CAN-FIND(FIRST ttAccounting) THEN
        RETURN.

    FOR EACH ttAccounting
        NO-LOCK
        BREAK BY ttAccounting.objectType:

        IF FIRST-OF(ttAccounting.objectType) THEN
            cObjects = "".

        IF LENGTH(cObjects) > 0 THEN
            cObjects = cObjects + "~n".
        cObjects = cObjects + ttAccounting.ObjectName + " (" + 
                              STRING(ttAccounting.instances) + ")".

        IF LAST-OF(ttAccounting.objectType) THEN DO:
            RUN Plugins.fail("MemMonitor", ttAccounting.objectType + ": " + cObjects).
        END.
    END.
    
END PROCEDURE.


PROCEDURE checkObjects PRIVATE:
    DEFINE VARIABLE cFormattedObject        AS CHARACTER    NO-UNDO.

    EMPTY TEMP-TABLE ttNewObjects.
    EMPTY TEMP-TABLE ttAccounting.
    RUN accountHandles ("Procedure",
                        SESSION:FIRST-PROCEDURE,
                        INPUT-OUTPUT TABLE ttNewObjects).
    RUN accountHandles ("Buffer",
                        SESSION:FIRST-BUFFER,
                        INPUT-OUTPUT TABLE ttNewObjects).
    &IF NOT PROVERSION BEGINS "9" &THEN
        RUN accountHandles ("Dataset",
                            SESSION:FIRST-DATASET,
                            INPUT-OUTPUT TABLE ttNewObjects).
        RUN accountHandles ("Data Source",
                            SESSION:FIRST-DATA-SOURCE,
                            INPUT-OUTPUT TABLE ttNewObjects).
        RUN accountHandles ("Query",
                            SESSION:FIRST-QUERY,
                            INPUT-OUTPUT TABLE ttNewObjects).
    &ENDIF
    
    RUN accountHandles ("AppServer",
                        SESSION:FIRST-SERVER,
                        INPUT-OUTPUT TABLE ttNewObjects).
    RUN accountHandles ("ServerSocket",
                        SESSION:FIRST-SERVER-SOCKET,
                        INPUT-OUTPUT TABLE ttNewObjects).
    RUN accountHandles ("Socket",
                        SESSION:FIRST-SOCKET,
                        INPUT-OUTPUT TABLE ttNewObjects).

    /* Delete from ttNewObjects records that were collected in the beginning */
    FOR EACH ttNewObjects:
        FIND ttDynObjects
            WHERE ttDynObjects.objectType   = ttNewObjects.objectType
              AND ttDynObjects.ObjectHandle = ttNewObjects.objectHandle
            NO-LOCK NO-ERROR.
        IF AVAILABLE ttDynObjects THEN
            DELETE ttNewObjects.
        ELSE DO:

        END.
    END.

    FOR EACH ttNewObjects:
        IF NOT VALID-HANDLE(ttNewObjects.objectHandle) THEN
            NEXT.

        RUN VALUE("format" + ttNewObjects.objectType) 
                (INPUT ttNewObjects.objectHandle,
                 OUTPUT cFormattedObject).

        FIND ttAccounting
            WHERE ttAccounting.objectType = ttNewObjects.objectType
              AND ttAccounting.objectName = cFormattedObject
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE ttAccounting THEN DO:
            CREATE ttAccounting.
            ASSIGN
                ttAccounting.objectType = ttNewObjects.objectType
                ttAccounting.objectName = cFormattedObject.
        END.
        ttAccounting.instances = ttAccounting.instances + 1.
    END.
END.

PROCEDURE startAccounting PRIVATE:
    EMPTY TEMP-TABLE ttDynObjects.
    RUN accountHandles ("Procedure",
                        SESSION:FIRST-PROCEDURE,
                        INPUT-OUTPUT TABLE ttDynObjects).
    RUN accountHandles ("Buffer",
                        SESSION:FIRST-BUFFER,
                        INPUT-OUTPUT TABLE ttDynObjects).
    &IF NOT PROVERSION BEGINS "9" &THEN
        RUN accountHandles ("Dataset",
                            SESSION:FIRST-DATASET,
                            INPUT-OUTPUT TABLE ttDynObjects).
        RUN accountHandles ("Data Source",
                            SESSION:FIRST-DATA-SOURCE,
                            INPUT-OUTPUT TABLE ttDynObjects).
        RUN accountHandles ("Query",
                            SESSION:FIRST-QUERY,
                            INPUT-OUTPUT TABLE ttDynObjects).
    &ENDIF
    RUN accountHandles ("AppServer",
                        SESSION:FIRST-SERVER,
                        INPUT-OUTPUT TABLE ttDynObjects).
    RUN accountHandles ("ServerSocket",
                        SESSION:FIRST-SERVER-SOCKET,
                        INPUT-OUTPUT TABLE ttDynObjects).
    RUN accountHandles ("Socket",
                        SESSION:FIRST-SOCKET,
                        INPUT-OUTPUT TABLE ttDynObjects).
END PROCEDURE.


PROCEDURE accountHandles PRIVATE:
    DEFINE INPUT        PARAMETER cType            AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER hFirstItem       AS HANDLE    NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE hTT.

    DEFINE VARIABLE hObject                 AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hTypeField              AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hHandleField            AS HANDLE           NO-UNDO.
    DEFINE VARIABLE hBuffer                 AS HANDLE           NO-UNDO.

    ASSIGN
        hBuffer      = hTT:DEFAULT-BUFFER-HANDLE
        hTypeField   = hBuffer:BUFFER-FIELD("objectType")
        hHandleField = hBuffer:BUFFER-FIELD("objectHandle").

    hObject = hFirstItem.
    DO WHILE VALID-HANDLE(hObject):
        hBuffer:BUFFER-CREATE().
        ASSIGN
            hTypeField:BUFFER-VALUE   = cType
            hHandleField:BUFFER-VALUE = hObject.
        hBuffer:BUFFER-RELEASE().
        hObject = hObject:NEXT-SIBLING.
    END.

    DELETE OBJECT hTT.
END.

PROCEDURE formatProcedure PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    cText = hObject:FILE-NAME.
END PROCEDURE.

PROCEDURE formatBuffer PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    cText = hObject:TABLE.
END PROCEDURE.


&IF NOT PROVERSION BEGINS "9" &THEN

PROCEDURE formatDataset PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iCount              AS INTEGER          NO-UNDO.

    DO iCount = 1 TO hObject:NUM-TOP-BUFFERS:
        IF iCount > 1 THEN
            cText = cText + ",".
        cText = cText + hObject:GET-TOP-BUFFER(iCount):TABLE.
    END.
END PROCEDURE.

PROCEDURE formatDataSource PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iCount              AS INTEGER          NO-UNDO.

    DO iCount = 1 TO hObject:NUM-SOURCE-BUFFERS:
        IF iCount > 1 THEN
            cText = cText + ",".
        cText = cText + hObject:GET-SOURCE-BUFFER(iCount):TABLE.
    END.

END PROCEDURE.

&ENDIF

PROCEDURE formatQuery PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iCount              AS INTEGER          NO-UNDO.

    IF hObject:NUM-BUFFERS = 0 THEN
        cText = "<No Buffers>".
    DO iCount = 1 TO hObject:NUM-BUFFERS:
        IF iCount > 1 THEN
            cText = cText + ",".
        cText = cText + hObject:GET-BUFFER-HANDLE(iCount):TABLE.
    END.
END PROCEDURE.

PROCEDURE formatAppServer PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    cText = hObject:NAME.

END PROCEDURE.

PROCEDURE formatServerSocket PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    cText = STRING(hObject).

END PROCEDURE.

PROCEDURE formatSocket PRIVATE:
    DEFINE INPUT PARAMETER hObject      AS HANDLE           NO-UNDO.
    DEFINE OUTPUT PARAMETER cText       AS CHARACTER        NO-UNDO.

    cText = hObject:REMOTE-HOST + ":" + STRING(hObject:REMOTE-PORT).
END PROCEDURE.
