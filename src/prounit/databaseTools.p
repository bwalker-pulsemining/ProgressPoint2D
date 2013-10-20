/*******************************************************************************
**
**     Program: databaseTools.p
** Description: Tool functions for physical tables and temp-tables. Those tools are designed to be used in test programs.
**      Author: SMarmotte
**     Created: 2013/06/22
**
********************************************************************************
**
**  Revision 1.3  2013/07/05  SMarmotte
**  - added exclusion to code not compatible with Progress v9
**  - added trigger disable when importing .d file into physical table
**  - added new procedure to dynamically delete records from a physical table according to a query
**  - added new procedures to dynamically create temp-table like a physical table and dynamically delete temp-table
**  - added functions to connect / disconnect same databases as AppServer does, with filter
**  - added better error handling to functions created in revision 1.2
**  - improved procedures LoadDFileToTempTable, LoadDFileToDBTable, LoadDFileToTableFromHandle, ExportDBTableToTempTableViaQuery
**  - added ability to gather current revision for the file through getVersion procedure
**  - renamed file to "databaseTools.p"
**  - updated code to produce run-time warning when run from Progress v9
**  - corrected delimiter that wasn't taken into account in __internal_dynImport
**  - improved error handling in __internal_dynExport and __internal_dynImport
**  - using temp-table to handle error 132 (record already existing) when loading data from .d file to physical table or temp-table
**  - added procedure LoadDFilesToDBTablesFromDirectory
**
**  Revision 1.2  2013/06/28  SMarmotte, on behalf of Sopra Group
**  - created new procedures:
**    - LoadDFileToTempTable
**    - LoadDFileToDBTable
**    - LoadDFileToTableFromHandle
**    - ExportDBTableToTempTableViaQuery
**
**  Revision 1.1  2013/06/27  SMarmotte
**  - improved __internal_dynExport and __internal_dynImport to work properly with OpenEdge v10.x
**  - corrected field order detection
**
**  Revision 1.0  2013/06/22  SMarmotte
**  - implemented functions __internal_dynExport and  __internal_dynImport
**  - initial version
**
*******************************************************************************/



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
{prounit/databaseInfo.i}

DEFINE TEMP-TABLE TT_BUFS_POS NO-UNDO
    FIELD cFieldName    AS CHARACTER
    FIELD iPosition     AS INTEGER
    INDEX idx IS PRIMARY UNIQUE iPosition.



/* -------------------------------------------------------------------------- */
/* FUNCTION FORWARDS                                                          */
/* -------------------------------------------------------------------------- */
&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) >= 10 &THEN
FUNCTION __internal_dynExport RETURNS LONGCHAR PRIVATE
    (INPUT p_hRecord    AS HANDLE,
     INPUT p_cDelim     AS CHARACTER) FORWARDS:

FUNCTION __internal_dynImport RETURNS INTEGER PRIVATE
    (INPUT p_hRecord    AS HANDLE,
     INPUT p_cDelim     AS CHARACTER,
     INPUT p_lcData     AS LONGCHAR) FORWARDS:
&ENDIF




/* -------------------------------------------------------------------------- */
/* Main-Block (program entry point)                                           */
/* -------------------------------------------------------------------------- */

/* No RETURN here, as the program is run persistent */



/*------------------------------------------------------------------------------
    Procedure: getVersion
  Description: Returns this API current version.
------------------------------------------------------------------------------*/
PROCEDURE getVersion:
    RETURN "$Revision: 1.3 $".
END PROCEDURE.



&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) >= 10 &THEN
/*------------------------------------------------------------------------------
     Function: __internal_dynExport
  Description: Dynamically export a buffer to a longchar variable.
       Return: Longchar string in .d format, "" on error
------------------------------------------------------------------------------*/
FUNCTION __internal_dynExport RETURNS LONGCHAR PRIVATE (INPUT p_hRecord AS HANDLE, INPUT p_cDelim AS CHARACTER):
    DEFINE VARIABLE hFld        AS HANDLE         NO-UNDO.
    DEFINE VARIABLE iCnt        AS INTEGER        NO-UNDO.
    DEFINE VARIABLE iExtnt      AS INTEGER        NO-UNDO.
    DEFINE VARIABLE cTmp        AS LONGCHAR       NO-UNDO.
    DEFINE VARIABLE cArray      AS LONGCHAR       NO-UNDO.
    DEFINE VARIABLE cResult     AS LONGCHAR       NO-UNDO.


    ASSIGN cResult = ?.
    IF p_hRecord:TYPE = "BUFFER" THEN DO:
        EMPTY TEMP-TABLE TT_BUFS_POS.

        /* This is needed in order to export fields according to their position order */
        DO iCnt = 1 TO p_hRecord:NUM-FIELDS:
            ASSIGN hFld = p_hRecord:BUFFER-FIELD(iCnt).

            CREATE TT_BUFS_POS.
            ASSIGN  TT_BUFS_POS.cFieldName  = hFld:NAME
                    TT_BUFS_POS.iPosition   = hFld:UNIQUE-ID.
            VALIDATE TT_BUFS_POS.
            RELEASE TT_BUFS_POS.
        END.

        /* Now export fields in according to their position order */
        FOR EACH TT_BUFS_POS NO-LOCK BY TT_BUFS_POS.iPosition.
            ASSIGN hFld = p_hRecord:BUFFER-FIELD(TT_BUFS_POS.cFieldName).

            DO  iExtnt = (IF hFld:EXTENT = 0 THEN 0 ELSE 1) TO hFld:EXTENT:
                ASSIGN cTmp = "".

                IF hFld:BUFFER-VALUE(iExtnt) = ? THEN
                    ASSIGN cTmp = "?".
                ELSE DO:
                    IF hFld:DATA-TYPE = "character" OR hFld:DATA-TYPE = "longchar" THEN
                        ASSIGN cTmp = QUOTER(hFld:BUFFER-VALUE(iExtnt)).
                    ELSE IF hFld:DATA-TYPE = "raw" THEN
                        ASSIGN cTmp = '"' + BASE64-ENCODE(hFld:BUFFER-VALUE(iExtnt)) + '"'.
                    ELSE IF hFld:DATA-TYPE = "datetime" OR hFld:DATA-TYPE = "datetime-tz" THEN
                         ASSIGN cTmp = ISO-DATE(hFld:BUFFER-VALUE(iExtnt)).
                    ELSE
                        ASSIGN cTmp = STRING(hFld:BUFFER-VALUE(iExtnt)).
                END.
                ASSIGN cResult = cResult + cTmp + p_cdelim.
            END.
        END.
    END.

    RETURN cResult.
END FUNCTION.


/*------------------------------------------------------------------------------
     Function: __internal_dynImport
  Description: Dynamically import a buffer from a longchar variable.
       Return: error-code (0 if OK, other error code otherwise)
------------------------------------------------------------------------------*/
FUNCTION __internal_dynImport RETURNS INTEGER PRIVATE (INPUT p_hRecord AS HANDLE, INPUT p_cDelim AS CHARACTER, INPUT p_lcData AS LONGCHAR):
    DEFINE VARIABLE hFld        AS HANDLE                       NO-UNDO.
    DEFINE VARIABLE iCnt        AS INTEGER                      NO-UNDO.
    DEFINE VARIABLE iPos        AS INTEGER      INITIAL 1       NO-UNDO.
    DEFINE VARIABLE iPosEnd     AS INTEGER                      NO-UNDO.
    DEFINE VARIABLE iExtnt      AS INTEGER                      NO-UNDO.
    DEFINE VARIABLE iExtrCnt    AS INTEGER                      NO-UNDO.
    DEFINE VARIABLE iImprCnt    AS INTEGER                      NO-UNDO.
    DEFINE VARIABLE cTmp        AS LONGCHAR                     NO-UNDO.
    DEFINE VARIABLE cArray      AS LONGCHAR                     NO-UNDO.
    DEFINE VARIABLE iResult     AS INTEGER      INITIAL 1       NO-UNDO.


    IF p_hRecord:TYPE = "BUFFER" THEN DO:
        ASSIGN iResult = 0.
        EMPTY TEMP-TABLE TT_BUFS_POS.

        /* This is needed in order to export fields according to their position order */
        DO iCnt = 1 TO p_hRecord:NUM-FIELDS:
            ASSIGN hFld = p_hRecord:BUFFER-FIELD(iCnt).

            CREATE TT_BUFS_POS.
            ASSIGN  TT_BUFS_POS.cFieldName  = hFld:NAME
                    TT_BUFS_POS.iPosition   = hFld:UNIQUE-ID.
            VALIDATE TT_BUFS_POS.
            /*MESSAGE "<<" + QUOTER(TT_BUFS_POS.cFieldName) + " - " + QUOTER(TT_BUFS_POS.iPosition) + ">>" VIEW-AS ALERT-BOX INFORMATION.*/
            RELEASE TT_BUFS_POS.
        END.

        /* Now export fields in according to their position order */
        FOR EACH TT_BUFS_POS NO-LOCK BY TT_BUFS_POS.iPosition.
            ASSIGN hFld = p_hRecord:BUFFER-FIELD(TT_BUFS_POS.cFieldName).
            /*MESSAGE "<< About to handle field " + QUOTER(TT_BUFS_POS.cFieldName) +  ">>" VIEW-AS ALERT-BOX INFORMATION.*/

            DO iExtnt = (IF hFld:EXTENT = 0 THEN 0 ELSE 1) TO hFld:EXTENT:

                /* Extract next text fragment in cTmp */
                ASSIGN cTmp = "".
                IF SUBSTRING(p_lcData, iPos, 1) = '"' THEN
                    ASSIGN iPosEnd = INDEX(p_lcData, '"' + p_cDelim, iPos + 1).
                ELSE
                    ASSIGN iPosEnd = INDEX(p_lcData, p_cDelim, iPos + 1).
                IF iPosEnd = 0 THEN                     /* Case when no end found --> use until string end */
                    ASSIGN iPosEnd = LENGTH(p_lcData).

                ASSIGN  cTmp = TRIM(SUBSTRING(p_lcData, iPos, iPosEnd - iPos + 1), p_cDelim)
                        iPos = iPosEnd + 1
                        iExtrCnt = iExtrCnt + 1.
                IF cTmp BEGINS '"' THEN
                    ASSIGN cTmp = SUBSTRING(cTmp, 2, LENGTH(cTmp) - 2).
                /*MESSAGE "<<" + REPLACE(STRING(cTmp), " ", "*") + ">>" VIEW-AS ALERT-BOX INFORMATION.*/

                /* Import into buffer */
                IF cTmp = ? OR cTmp = "?" THEN
                    ASSIGN hFld:BUFFER-VALUE(iExtnt) = ?.
                ELSE DO:
                    block_dynimport:
                    DO TRANSACTION ON ERROR UNDO block_dynimport, LEAVE block_dynimport:

                        IF hFld:DATA-TYPE = "character" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = STRING(cTmp) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "longchar" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = cTmp NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "raw" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = BASE64-DECODE(cTmp) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "logical" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = LOGICAL(STRING(cTmp)) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "date" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = DATE(STRING(cTmp)) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "decimal" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = DECIMAL(STRING(cTmp)) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "integer" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = INTEGER(STRING(cTmp)) NO-ERROR.
&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) >= 11 &THEN
                        ELSE IF hFld:DATA-TYPE = "int64" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = INT64(cTmp) NO-ERROR.
&ENDIF
                        ELSE IF hFld:DATA-TYPE = "handle" OR hFld:DATA-TYPE = "com-handle" OR hFld:DATA-TYPE = "recid" THEN
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = INTEGER(STRING(cTmp)) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "datetime" THEN                                                                /* All those STRING(cTmp) look dummy but are required in v10 in order to avoid errors */
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = DATETIME(INTEGER(SUBSTRING(STRING(cTmp), 6, 2)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 9, 2)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 1, 4)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 12, 2)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 15, 2)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 18, 2)),
                                                                        INTEGER(SUBSTRING(STRING(cTmp), 21, 3))     ) NO-ERROR.
                        ELSE IF hFld:DATA-TYPE = "datetime-tz" THEN                                                             /* All those STRING(cTmp) look dummy but are required in v10 in order to avoid errors */
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = DATETIME-TZ( INTEGER(SUBSTRING(STRING(cTmp), 6, 2)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 9, 2)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 1, 4)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 12, 2)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 15, 2)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 18, 2)),
                                                                            INTEGER(SUBSTRING(STRING(cTmp), 21, 3)),
                                                                            (IF SUBSTRING(STRING(cTmp), 24, 1) = "+" THEN 1 ELSE -1) * 60 * INTEGER(SUBSTRING(STRING(cTmp), 25, 2)) + INTEGER(SUBSTRING(STRING(cTmp), 28, 2)) ) NO-ERROR.
                        ELSE
                            ASSIGN hFld:BUFFER-VALUE(iExtnt) = ? NO-ERROR.

                        /* Check for error */
                        IF ERROR-STATUS:ERROR THEN DO:
                            ASSIGN iResult = ERROR-STATUS:GET-NUMBER(1).
                            UNDO block_dynimport, LEAVE block_dynimport.
                        END.

                        ASSIGN iImprCnt = iImprCnt + 1.
                    END.
                END.
            END.
        END.
    END.

    RETURN iResult.
END FUNCTION.
&ENDIF


/*------------------------------------------------------------------------------
    Procedure: LoadDFileToTempTable
  Description: Dynamically load data from .d file to a temp-table.
------------------------------------------------------------------------------*/
PROCEDURE LoadDFileToTempTable:
    DEFINE INPUT PARAMETER p_cFileName              AS CHARACTER    NO-UNDO.    /* The .d file name */
    DEFINE INPUT PARAMETER p_hTargetTable           AS HANDLE       NO-UNDO.    /* Handle to the target temp-table */
    DEFINE INPUT PARAMETER p_lEmpty                 AS LOGICAL      NO-UNDO.    /* Empty the table before injecting .d data? */
    DEFINE INPUT PARAMETER p_lOverwriteByPrimKey    AS LOGICAL      NO-UNDO.    /* Overwrite records if already existing? */

    DEFINE VARIABLE hShadowTT   AS HANDLE       NO-UNDO.                        /* This TT will contain only the record to insert - internally used by LoadDFileToTableFromHandle */


    /* Create temp table for insertion */
    CREATE TEMP-TABLE hShadowTT /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
    hShadowTT:CREATE-LIKE(p_hTargetTable:DEFAULT-BUFFER-HANDLE).
    hShadowTT:TEMP-TABLE-PREPARE("tt_" + p_hTargetTable:NAME + "_insert").      /* That name wil be used later in queries */
    hShadowTT:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

    /* Load */
    RUN LoadDFileToTableFromHandle(INPUT p_cFileName, INPUT hShadowTT, INPUT p_hTargetTable, INPUT p_lEmpty, INPUT p_lOverwriteByPrimKey) NO-ERROR.
    DELETE OBJECT hShadowTT.

    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR (IF RETURN-VALUE <> "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)).
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: LoadDFileToDBTable
  Description: Dynamically load data from .d file to a physical table.
------------------------------------------------------------------------------*/
PROCEDURE LoadDFileToDBTable:
    DEFINE INPUT PARAMETER p_cFileName              AS CHARACTER    NO-UNDO.    /* The .d file name */
    DEFINE INPUT PARAMETER p_cTableName             AS CHARACTER    NO-UNDO.    /* The name of target table - be aware of database logical name identifier */
    DEFINE INPUT PARAMETER p_lEmpty                 AS LOGICAL      NO-UNDO.    /* Empty the table before injecting .d data? */
    DEFINE INPUT PARAMETER p_lOverwriteByPrimKey    AS LOGICAL      NO-UNDO.    /* Overwrite records if already existing? */

    DEFINE VARIABLE hTargetBuffer   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hShadowTT       AS HANDLE       NO-UNDO.                    /* This TT will contain only the record to insert - internally used by LoadDFileToTableFromHandle */


    CREATE BUFFER hTargetBuffer FOR TABLE p_cTableName /*IN WIDGET-POOL "ProUnitDatabaseTools"*/ NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(hTargetBuffer) THEN
        RETURN ERROR "Unable to create dynamic buffer for table " + QUOTER(p_cTableName) + ".~n" + ERROR-STATUS:GET-MESSAGE(1).

    /* Disable triggers */
    hTargetBuffer:DISABLE-LOAD-TRIGGERS(NO).
    hTargetBuffer:DISABLE-DUMP-TRIGGERS().

    /* Create temp table for insertion */
    CREATE TEMP-TABLE hShadowTT /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
    hShadowTT:CREATE-LIKE(hTargetBuffer).
    hShadowTT:TEMP-TABLE-PREPARE("tt_" + p_cTableName + "_insert").             /* That name wil be used later in queries */
    hShadowTT:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

    /* Load */
    RUN LoadDFileToTableFromHandle(INPUT p_cFileName, INPUT hShadowTT, INPUT hTargetBuffer, INPUT p_lEmpty, INPUT p_lOverwriteByPrimKey) NO-ERROR.
    DELETE OBJECT hShadowTT.
    DELETE OBJECT hTargetBuffer.

    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR (IF RETURN-VALUE <> "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)).
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: LoadDFilesToDBTablesFromDirectory
  Description: Dynamically load a set of .d files to many physical table.
------------------------------------------------------------------------------*/
PROCEDURE LoadDFilesToDBTablesFromDirectory:
    DEFINE INPUT PARAMETER p_cDirPath               AS CHARACTER    NO-UNDO.    /* The directory path name */
    DEFINE INPUT PARAMETER p_lEmpty                 AS LOGICAL      NO-UNDO.    /* Empty the table before injecting .d data? */
    DEFINE INPUT PARAMETER p_lOverwriteByPrimKey    AS LOGICAL      NO-UNDO.    /* Overwrite records if already existing? */

    DEFINE VARIABLE cBaseName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFullName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAttribs   AS CHARACTER   NO-UNDO.


    ASSIGN FILE-INFO:FILE-NAME = p_cDirPath.
    IF FILE-INFO:FILE-TYPE MATCHES "*D*R*" THEN DO:
        INPUT FROM OS-DIR(p_cDirPath).
        REPEAT:
            IMPORT cBaseName cFullName cAttribs.
            IF cBaseName <> "." AND cBaseName <> ".." AND cAttribs MATCHES "*F*" AND LENGTH(cBaseName) > 2 AND SUBSTRING(cBaseName, LENGTH(cBaseName) - 1, 2) = ".d" THEN DO:
                RUN LoadDFileToDBTable(INPUT cFullName, INPUT SUBSTRING(cBaseName, 1, LENGTH(cBaseName) - 2), INPUT p_lEmpty, INPUT p_lOverwriteByPrimKey) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    INPUT CLOSE.
                    RETURN ERROR "Failed to import " + QUOTER(cFullName) + " to table " + SUBSTRING(cBaseName, 1, LENGTH(cBaseName) - 2) + " - " + (IF RETURN-VALUE <> "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)).
                END.
            END.
        END.
        INPUT CLOSE.
    END.
    ELSE
        RETURN ERROR "Invalid path. It should be a valid directory full path name.".

    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: LoadDFileToTableFromHandle
  Description: Dynamically load data from .d file to a temp-table or to a physical table.
------------------------------------------------------------------------------*/
PROCEDURE LoadDFileToTableFromHandle PRIVATE:
    DEFINE INPUT PARAMETER p_cFileName              AS CHARACTER    NO-UNDO.    /* The .d file name */
    DEFINE INPUT PARAMETER p_hTestTempTable         AS HANDLE       NO-UNDO.    /* The TT for converting data to record - used for easly handle data after */
    DEFINE INPUT PARAMETER p_hTargetTable           AS HANDLE       NO-UNDO.    /* The target table or temp-table */
    DEFINE INPUT PARAMETER p_lEmpty                 AS LOGICAL      NO-UNDO.    /* Empty the table before injecting .d data? */
    DEFINE INPUT PARAMETER p_lOverwriteByPrimKey    AS LOGICAL      NO-UNDO.    /* Overwrite records if already existing? */


&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) < 10 &THEN
    RETURN ERROR "** Warning: you are using features that requires OpenEdge v10 or higher.".
&ELSE
    /* Counters */
    DEFINE VARIABLE i                       AS INTEGER      NO-UNDO.
    DEFINE VARIABLE j                       AS INTEGER      NO-UNDO.

    /* Used for dynamic database accesses */
    DEFINE VARIABLE hTargetBuffer           AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK                     AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE iReturn                 AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hQuery                  AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cErrorDesc              AS CHARACTER    NO-UNDO     INITIAL "".

    /* User for error 132 handling (record already existing) */
    DEFINE VARIABLE cIndexInfo              AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cPKFields               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cPKWhereClause          AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cQueryString            AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE hTestTempTableBuffer    AS HANDLE       NO-UNDO.

    /* Used for .d line extraction */
    DEFINE VARIABLE lcData                  AS LONGCHAR     NO-UNDO.
    DEFINE VARIABLE iNumLines               AS INTEGER      NO-UNDO.
    DEFINE VARIABLE lcLine                  AS LONGCHAR     NO-UNDO.
    DEFINE VARIABLE iCurLine                AS INTEGER      NO-UNDO.


    /* Load .d file */
    COPY-LOB FROM FILE p_cFileName TO lcData NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        RETURN ERROR "Unable to load .d file " + QUOTER(p_cFileName) + ".".

    /* Get buffer to target table */
    IF p_hTargetTable:TYPE = "TEMP-TABLE" THEN
        ASSIGN hTargetBuffer = p_hTargetTable:DEFAULT-BUFFER-HANDLE.
    ELSE IF p_hTargetTable:TYPE = "BUFFER" THEN
        ASSIGN hTargetBuffer = p_hTargetTable.
    ASSIGN hTestTempTableBuffer = p_hTestTempTable:DEFAULT-BUFFER-HANDLE.

    /* Empty target table if needed */
    ASSIGN cErrorDesc = "".
    IF p_lEmpty THEN DO:
        CREATE QUERY hQuery /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
        hQuery:SET-BUFFERS(hTargetBuffer).
        ASSIGN lOK = hQuery:QUERY-PREPARE("FOR EACH " + hTargetBuffer:NAME + " EXCLUSIVE-LOCK.") NO-ERROR.
        IF lOK THEN DO:
            ASSIGN lOK = hQuery:QUERY-OPEN() NO-ERROR.
            IF lOK THEN DO TRANSACTION:
                hQuery:GET-FIRST().
                DO WHILE NOT hQuery:QUERY-OFF-END:
                    ASSIGN lOK = hTargetBuffer:BUFFER-DELETE() NO-ERROR.
                    IF NOT lOK THEN DO:
                        ASSIGN cErrorDesc = ERROR-STATUS:GET-MESSAGE(1).
                        LEAVE.
                    END.
                    hQuery:GET-NEXT().
                END.
                hQuery:QUERY-CLOSE().
            END.
        END.
        DELETE OBJECT hQuery.
    END.
    IF cErrorDesc > "" THEN
        RETURN ERROR cErrorDesc.

    /* Do import */
    IF VALID-HANDLE(hTargetBuffer) THEN DO:

        /* Get next line from .d file */
        ASSIGN  iCurLine = 1
                iNumLines = NUM-ENTRIES(lcData, CHR(10)).

        DO WHILE iCurLine < iNumLines:
            IF RIGHT-TRIM(ENTRY(iCurLine, lcData, CHR(10)), CHR(10) + CHR(13)) = "." THEN
                LEAVE.
            ASSIGN  lOK = NO
                    lcLine = "".
            DO WHILE NOT lOK:
                ASSIGN  lcLine = lcLine + ENTRY(iCurLine, lcData, CHR(10))
                        lcLine = RIGHT-TRIM(lcLine, CHR(10) + CHR(13))
                        iCurLine = iCurLine + 1.
                IF NUM-ENTRIES(lcData, '"') MODULO 2 = 1 THEN       /* We found an opening dbquote and its close counterpart --> OK */
                    ASSIGN lOK = YES.
            END.

            /* Got the line in lcLine */
            /*MESSAGE QUOTER(STRING(lcLine)) VIEW-AS ALERT-BOX.*/

            /* -------------------------------------------------------------------------------- */
            /* Merge to target table */
            blk_trans_create:
            DO TRANSACTION:

                ASSIGN lOK = hTargetBuffer:BUFFER-CREATE() NO-ERROR.
                IF NOT lOK THEN
                    UNDO blk_trans_create, LEAVE blk_trans_create.

                ASSIGN iReturn = DYNAMIC-FUNCTION("__internal_dynImport", INPUT hTargetBuffer, INPUT " ", INPUT lcLine).
                IF iReturn <> 0 THEN DO:
                    ASSIGN lOK = hTargetBuffer:BUFFER-DELETE() NO-ERROR.        /* Be clean: remove partial result */
                    IF NOT lOK THEN
                        UNDO blk_trans_create, LEAVE blk_trans_create.

                    /* In case of error 132 (record already existing), try to remove existing record, according to the primary index only */
                    IF iReturn = 132 AND p_lOverwriteByPrimKey THEN DO:

                        /* Step 1: gather each field participating in primary unique index --> variable cPKFields */
                        ASSIGN  i = 1
                                cPKFields = "".
                        DO WHILE hTargetBuffer:INDEX-INFORMATION(i) > "":
                            ASSIGN cIndexInfo = hTargetBuffer:INDEX-INFORMATION(i)
                                   i = i + 1.

                            /* Handle only primary unique index, it may be the cause of error 132 */
                            IF ENTRY(2, cIndexInfo, ",") = "1" AND ENTRY(3, cIndexInfo, ",") = "1" THEN DO:
                                ASSIGN j = 5.
                                DO WHILE j <= NUM-ENTRIES(cIndexInfo, ",") :
                                    ASSIGN  cPKFields = TRIM(cPKFields + "," + ENTRY(j, cIndexInfo, ","), ",")
                                            j = j + 2.
                                END.
                            END.
                            ASSIGN cIndexInfo = "".
                        END.

                        /* Step 2: prepare the where clause for query */
                        ASSIGN cPKWhereClause = "".
                        DO i = 1 TO NUM-ENTRIES(cPKFields, ",") :
                            ASSIGN cPKWhereClause = cPKWhereClause +
                                                    (IF cPKWhereClause = "" THEN " WHERE " ELSE " AND ") +
                                                    p_hTargetTable:NAME + "." + ENTRY(i, cPKFields, ",") + " = " + hTestTempTableBuffer:NAME + "." + ENTRY(i, cPKFields, ",").
                        END.
                        ASSIGN cQueryString = "FOR EACH " + p_hTestTempTable:NAME + " NO-LOCK,    EACH " + hTargetBuffer:NAME + " EXCLUSIVE-LOCK " + cPKWhereClause.

                        /* Step 3: perform previous import in the test temp-table */
                        hTestTempTableBuffer:EMPTY-TEMP-TABLE().
                        ASSIGN lOK = hTestTempTableBuffer:BUFFER-CREATE() NO-ERROR.
                        IF NOT lOK THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.

                        IF DYNAMIC-FUNCTION("__internal_dynImport", INPUT hTestTempTableBuffer, INPUT " ", INPUT lcLine) <> 0 THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.

                        ASSIGN lOK = hTestTempTableBuffer:BUFFER-VALIDATE() NO-ERROR.
                        IF NOT lOK THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.

                        ASSIGN lOK = hTestTempTableBuffer:BUFFER-RELEASE() NO-ERROR.
                        IF NOT lOK THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.
                        /*MESSAGE "TT has records? " + STRING(p_hTestTempTable:HAS-RECORDS) SKIP "TT has been prepared? " + STRING(p_hTestTempTable:PREPARED) VIEW-AS ALERT-BOX INFORMATION.*/

                        /* Step 4: run query and delete matching records from the target table */
                        ASSIGN cErrorDesc = "".
                        CREATE QUERY hQuery /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
                        hQuery:SET-BUFFERS(hTestTempTableBuffer, hTargetBuffer) NO-ERROR.
                        hQuery:QUERY-PREPARE(cQueryString) NO-ERROR.
                        IF lOK THEN DO:
                            ASSIGN lOK = hQuery:QUERY-OPEN() NO-ERROR.
                            IF lOK THEN DO :
                                hQuery:GET-FIRST().
                                DO WHILE NOT hQuery:QUERY-OFF-END:
                                    /*MESSAGE "OK, I found one record according to the query." VIEW-AS ALERT-BOX INFORMATION.*/
                                    ASSIGN lOK = hTargetBuffer:BUFFER-DELETE() NO-ERROR.
                                    IF NOT lOK THEN
                                        ASSIGN cErrorDesc = ERROR-STATUS:GET-MESSAGE(1).
                                    LEAVE.                                      /* Only one record is necessary */
                                END.
                                hQuery:QUERY-CLOSE().
                            END.
                        END.

                        DELETE OBJECT hQuery.
                        IF cErrorDesc > "" THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.

                        /* Step 5: try importing again */
                        ASSIGN lOK = hTargetBuffer:BUFFER-CREATE() NO-ERROR.
                        IF NOT lOK THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.

                        ASSIGN iReturn = DYNAMIC-FUNCTION("__internal_dynImport", INPUT hTargetBuffer, INPUT " ", INPUT lcLine).
                        IF iReturn <> 0 THEN
                            UNDO blk_trans_create, LEAVE blk_trans_create.
                    END. /* error 132 handling */
                    ELSE
                        UNDO blk_trans_create, LEAVE blk_trans_create.
                END.

                ASSIGN lOk = hTargetBuffer:BUFFER-VALIDATE() NO-ERROR.
                IF NOT lOK THEN
                    UNDO blk_trans_create, LEAVE blk_trans_create.

                ASSIGN lOk = hTargetBuffer:BUFFER-RELEASE() NO-ERROR.
                IF NOT lOK THEN
                    UNDO blk_trans_create, LEAVE blk_trans_create.
            END. /* DO TRANSACTION: */
            /* -------------------------------------------------------------------------------- */

        END. /* .d file read */
    END. /* VALID-HANDLE(hTargetBuffer) */
    ELSE
        RETURN ERROR "Error getting buffer to target table.".

    RETURN "".
&ENDIF
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: ExportDBTableToTempTableViaQuery
  Description: Dynamically export data from a physical table to a temp-table, according to a query.
         Note: The target temp-table will be emptied before processing.
------------------------------------------------------------------------------*/
PROCEDURE ExportDBTableToTempTableViaQuery:
    DEFINE INPUT PARAMETER p_cTableName             AS CHARACTER    NO-UNDO.    /* The name of target table - be aware of database logical name identifier */
    DEFINE INPUT PARAMETER p_hTargetTempTable       AS HANDLE       NO-UNDO.    /* The target temp-table */
    DEFINE INPUT PARAMETER p_cQueryWhereClause      AS CHARACTER    NO-UNDO.    /* Where-clause for selecting records */


    DEFINE VARIABLE hSourceBuffer   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hTargetBuffer   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hQuery          AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK             AS LOGICAL	    NO-UNDO.
    DEFINE VARIABLE cErrorDesc      AS CHARACTER    NO-UNDO     INITIAL "".



    /* Get buffer to tables */
    IF p_hTargetTempTable:TYPE <> "TEMP-TABLE" THEN
        RETURN ERROR "Invalid temp-table handle. Parameter #2 must be handle to temp-table.".

    CREATE BUFFER hSourceBuffer FOR TABLE p_cTableName /*IN WIDGET-POOL "ProUnitDatabaseTools"*/ NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(hSourceBuffer) THEN
        RETURN ERROR "Unable to create dynamic buffer for table " + QUOTER(p_cTableName) + ".~n" + ERROR-STATUS:GET-MESSAGE(1).

    ASSIGN hTargetBuffer = p_hTargetTempTable:DEFAULT-BUFFER-HANDLE.
    hTargetBuffer:EMPTY-TEMP-TABLE().

    /* Ensure the where clause begins with "where " */
    IF NOT TRIM(p_cQueryWhereClause, " ") BEGINS "where " THEN
        ASSIGN p_cQueryWhereClause = "WHERE " + p_cQueryWhereClause.

    /* Browse the table */
    CREATE QUERY hQuery /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
    hQuery:SET-BUFFERS(hSourceBuffer).
    ASSIGN lOK = hQuery:QUERY-PREPARE("FOR EACH " + hSourceBuffer:NAME + " NO-LOCK " + p_cQueryWhereClause) NO-ERROR.
    IF lOK THEN DO:
        ASSIGN lOK = hQuery:QUERY-OPEN() NO-ERROR.
        IF lOK THEN DO:
            hQuery:GET-FIRST().
            DO WHILE NOT hQuery:QUERY-OFF-END:
                blk_trans_export:
                DO TRANSACTION:
                    ASSIGN lOK = hTargetBuffer:BUFFER-CREATE() NO-ERROR.
                    IF NOT lOK THEN
                        UNDO blk_trans_export, LEAVE blk_trans_export.

                    ASSIGN lOK = hTargetBuffer:BUFFER-COPY(hSourceBuffer) NO-ERROR.
                    IF NOT lOK THEN
                        UNDO blk_trans_export, LEAVE blk_trans_export.
&IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) >= 10 &THEN
                    ASSIGN lOk = hTargetBuffer:BUFFER-VALIDATE() NO-ERROR.
                    IF NOT lOK THEN
                        UNDO blk_trans_export, LEAVE blk_trans_export.
&ENDIF
                    ASSIGN lOk = hTargetBuffer:BUFFER-RELEASE() NO-ERROR.
                    IF NOT lOK THEN
                        UNDO blk_trans_export, LEAVE blk_trans_export.
                END. /* DO TRANSACTION: */

                hQuery:GET-NEXT().
            END.
            hQuery:QUERY-CLOSE().
        END.
    END.
    DELETE OBJECT hQuery.
    DELETE OBJECT hSourceBuffer.

    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: CreateTTLikePhysicalDBTable
  Description: Create a dynamic temp-table like a physical database table.
------------------------------------------------------------------------------*/
PROCEDURE CreateTTLikePhysicalDBTable:
    DEFINE INPUT PARAMETER p_cPhysicalTableName   AS CHARACTER  NO-UNDO.        /* Physical table name */
    DEFINE INPUT PARAMETER p_cTempTableName       AS CHARACTER  NO-UNDO.        /* Target temp-table name to be used in queries */
    DEFINE OUTPUT PARAMETER p_hTTHandle           AS HANDLE     NO-UNDO.        /* Handle receiving the TT handle */

    DEFINE VARIABLE hTargetBuffer   AS HANDLE       NO-UNDO.


    ASSIGN p_hTTHandle = ?.

    CREATE BUFFER hTargetBuffer FOR TABLE p_cPhysicalTableName /*IN WIDGET-POOL "ProUnitDatabaseTools"*/ NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(hTargetBuffer) THEN
        RETURN ERROR "Unable to create dynamic buffer for table " + QUOTER(p_cPhysicalTableName) + ".~n" + ERROR-STATUS:GET-MESSAGE(1).

    CREATE TEMP-TABLE p_hTTHandle /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
    p_hTTHandle:CREATE-LIKE(hTargetBuffer).
    p_hTTHandle:TEMP-TABLE-PREPARE(IF p_cTempTableName > "" THEN p_cTempTableName ELSE "tt_" + p_cPhysicalTableName).
    p_hTTHandle:DEFAULT-BUFFER-HANDLE:EMPTY-TEMP-TABLE().

    DELETE OBJECT hTargetBuffer.
    RETURN "".
END.
/* Followinf function is here for ascending compatibility */
PROCEDURE CreateTTLikePhysicalDB:
    DEFINE INPUT PARAMETER p_cPhysicalTableName   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER p_cTempTableName       AS CHARACTER  NO-UNDO.
    DEFINE OUTPUT PARAMETER p_hTTHandle           AS HANDLE     NO-UNDO.
    RUN CreateTTLikePhysicalDBTable(INPUT p_cPhysicalTableName, INPUT p_cTempTableName, OUTPUT p_hTTHandle).
END.


/*------------------------------------------------------------------------------
    Procedure: DeleteTTByHandle
  Description: Delete a dynamic temp-table from its handle.
------------------------------------------------------------------------------*/
PROCEDURE DeleteTTByHandle:
    DEFINE INPUT-OUTPUT PARAMETER p_hTTHandle   AS HANDLE     NO-UNDO.          /* Handle to TT to be destroyed */

    IF VALID-HANDLE(p_hTTHandle) THEN
        DELETE OBJECT p_hTTHandle.
    ASSIGN p_hTTHandle = ?.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: DeleteDBTableRecordsViaQuery
  Description: Delete records from physical table according to a query.
------------------------------------------------------------------------------*/
PROCEDURE DeleteDBTableRecordsViaQuery:
    DEFINE INPUT PARAMETER p_cPhysicalTableName     AS CHARACTER    NO-UNDO.    /* Physical table name */
    DEFINE INPUT PARAMETER p_cQueryWhereClause      AS CHARACTER    NO-UNDO.    /* Where-clause for selecting records */

    DEFINE VARIABLE hTargetBuffer   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hQuery          AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK             AS LOGICAL	    NO-UNDO.
    DEFINE VARIABLE cErrorDesc      AS CHARACTER    NO-UNDO     INITIAL "".


    CREATE BUFFER hTargetBuffer FOR TABLE p_cPhysicalTableName /*IN WIDGET-POOL "ProUnitDatabaseTools"*/ NO-ERROR.
    IF ERROR-STATUS:ERROR OR NOT VALID-HANDLE(hTargetBuffer) THEN
        RETURN ERROR "Unable to create dynamic buffer for table " + QUOTER(p_cPhysicalTableName) + ".~n" + ERROR-STATUS:GET-MESSAGE(1).

    /* Disable triggers */
    hTargetBuffer:DISABLE-LOAD-TRIGGERS(NO).
    hTargetBuffer:DISABLE-DUMP-TRIGGERS().

    /* Ensure the where clause begins with "where " */
    IF NOT TRIM(p_cQueryWhereClause, " ") BEGINS "where " THEN
        ASSIGN p_cQueryWhereClause = "WHERE " + p_cQueryWhereClause.

    CREATE QUERY hQuery /*IN WIDGET-POOL "ProUnitDatabaseTools"*/.
    hQuery:SET-BUFFERS(hTargetBuffer).
    ASSIGN lOK = hQuery:QUERY-PREPARE("FOR EACH " + hTargetBuffer:NAME + " EXCLUSIVE-LOCK " + p_cQueryWhereClause) NO-ERROR.
    IF lOK THEN DO:
        ASSIGN lOK = hQuery:QUERY-OPEN() NO-ERROR.
        IF lOK THEN DO TRANSACTION:
            hQuery:GET-FIRST().
            DO WHILE NOT hQuery:QUERY-OFF-END:
                ASSIGN lOK = hTargetBuffer:BUFFER-DELETE() NO-ERROR.
                IF NOT lOK THEN DO:
                    ASSIGN cErrorDesc = ERROR-STATUS:GET-MESSAGE(1).
                    LEAVE.
                END.
                hQuery:GET-NEXT().
            END.
            hQuery:QUERY-CLOSE().
        END.
    END.
    DELETE OBJECT hQuery.
    DELETE OBJECT hTargetBuffer.

    IF cErrorDesc > "" THEN
        RETURN ERROR cErrorDesc.
    RETURN "".
END.


/*------------------------------------------------------------------------------
    Procedure: LocalConnectAppServerDB
  Description: Query AppServer for connected databases and connect some of them
               to current session.
------------------------------------------------------------------------------*/
PROCEDURE LocalConnectAppServerDB:
    DEFINE INPUT PARAMETER p_hAppServer           AS HANDLE     NO-UNDO.        /* Handle to AppServer */
    DEFINE INPUT PARAMETER p_cLogicalNamesList    AS CHARACTER  NO-UNDO.        /* List of logical database names to connect (search is performed using AppServer logical names and aliases) */

    DEFINE VARIABLE cCnxString            AS CHARACTER    NO-UNDO     INITIAL "".
    DEFINE VARIABLE cProgram              AS CHARACTER    NO-UNDO     INITIAL "prounit/appserverDB.p".
    DEFINE VARIABLE cTmp                  AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lAppServerConnected   AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE lCanConnectDB         AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE i                     AS INTEGER      NO-UNDO.


    /* Do nothing if there is already connected database */
    IF CAN-FIND(FIRST TT_DATABASE NO-LOCK WHERE TT_DATABASE.lIsConnected) THEN
        RETURN "".

    /* Do nothing if remote session is local session */
    IF p_hAppServer = SESSION THEN
        RETURN "".

    /* Ensure handle is valid */
    IF NOT VALID-HANDLE(p_hAppServer) THEN
        RETURN ERROR "Invalid AppServer handle.".

    /* Ensure AppServer is connected */
    ASSIGN lAppServerConnected = p_hAppServer:CONNECTED() NO-ERROR.
    IF NOT lAppServerConnected THEN
        RETURN ERROR "Disconnected from AppServer.".

    /* Ask AppServer to give us its connected databases */
    IF NOT CAN-FIND(FIRST TT_DATABASE NO-LOCK) THEN DO:
        RUN VALUE(cProgram) ON p_hAppServer (OUTPUT TABLE TT_DATABASE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            RETURN ERROR "Unable to call " + QUOTER(cProgram) + " on AppServer part.~n" + (IF RETURN-VALUE <> "" THEN RETURN-VALUE ELSE ERROR-STATUS:GET-MESSAGE(1)).
    END.

    /* Ensure the list is valid */
    IF p_cLogicalNamesList = ? THEN
        ASSIGN p_cLogicalNamesList = "".

    /* Perform DB connections */
    FOR EACH TT_DATABASE EXCLUSIVE-LOCK:
        ASSIGN cCnxString = " " + TT_DATABASE.cConnectionString.

        /* Correct connection string */
        ASSIGN cCnxString = REPLACE(cCnxString, ",", " ").

        /* Keep only file name (basename) of physical database name if full path is specified */
        IF INDEX(TT_DATABASE.cPhysicalName, "/") > 0 THEN
            ASSIGN TT_DATABASE.cPhysicalName = ENTRY(NUM-ENTRIES(TT_DATABASE.cPhysicalName, "/"), TT_DATABASE.cPhysicalName, "/").
        ELSE IF INDEX(TT_DATABASE.cPhysicalName, "~\") > 0 THEN
            ASSIGN TT_DATABASE.cPhysicalName = ENTRY(NUM-ENTRIES(TT_DATABASE.cPhysicalName, "~\"), TT_DATABASE.cPhysicalName, "~\").

        /* Ensure physical database name does not contain directory separator */
        ASSIGN i = LOOKUP("-db", cCnxString, " ").
        IF i > 0 AND i + 1 <= NUM-ENTRIES(cCnxString, " ") THEN DO:
            ASSIGN cTmp = ENTRY(i + 1, cCnxString, " ").
            IF INDEX(cTmp, "/") > 0 OR INDEX(cTmp, "~\") > 0 THEN
                ASSIGN ENTRY(i + 1, cCnxString, " ") = TT_DATABASE.cPhysicalName.
        END.

        /* Inject physical database name in connection string if missing */
        IF INDEX(cCnxString, " -db ") = 0 THEN
            ASSIGN cCnxString = cCnxString + " -db " + TT_DATABASE.cPhysicalName.

        /* Inject logical database name in connection string if missing */
        IF INDEX(cCnxString, " -ld ") = 0 THEN
            ASSIGN cCnxString = cCnxString + " -ld " + TT_DATABASE.cLogicalName.

        /* Remove triggers expression */
        ASSIGN i = LOOKUP("-trig", cCnxString, " ").
        IF i > 0 AND i + 1 <= NUM-ENTRIES(cCnxString, " ") THEN DO:
            ASSIGN  ENTRY(i + 0, cCnxString, " ") = ""
                    ENTRY(i + 1, cCnxString, " ") = "".
        END.

        /* Remove multiple spaces */
        DO WHILE INDEX(cCnxString, "  ") > 0:
            ASSIGN cCnxString = TRIM(REPLACE(cCnxString, "  ", " ")).
        END.

        /* Detect if connection can be performed */
        ASSIGN lCanConnectDB = NO.
        IF p_cLogicalNamesList = "" THEN                                                /* No field specified --> every database will be connected */
            ASSIGN lCanConnectDB = YES.
        ELSE IF LOOKUP(TT_DATABASE.cLogicalName, p_cLogicalNamesList, ",") > 0 THEN     /* Logical name found --> OK, allow connection */
            ASSIGN lCanConnectDB = YES.
        ELSE DO i = 1 TO NUM-ENTRIES(TT_DATABASE.cAliasList, ",") :                     /* Look in alias list */
            IF LOOKUP(ENTRY(i, TT_DATABASE.cAliasList, ","), p_cLogicalNamesList, ",") > 0 THEN DO:
                ASSIGN lCanConnectDB = YES.
                LEAVE.
            END.
        END.

        /* Do connection if needed */
        IF lCanConnectDB THEN DO:
            /*MESSAGE cCnxString VIEW-AS ALERT-BOX INFORMATION.*/
            CONNECT VALUE(cCnxString) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                RETURN ERROR "Unable to connect to database with connection string: " + QUOTER(cCnxString) + ".".

            /* Create aliases */
            DO i = 1 TO NUM-ENTRIES(TT_DATABASE.cAliasList, ","):
                CREATE ALIAS VALUE(ENTRY(i, TT_DATABASE.cAliasList, ",")) FOR DATABASE VALUE(TT_DATABASE.cLogicalName) NO-ERROR.
                /* We do not handle error here because we can't. */
            END.

            /* Update connection status in temp-table */
            ASSIGN TT_DATABASE.lIsConnected = YES.
        END.
        VALIDATE TT_DATABASE.
    END.
    RETURN "".
END PROCEDURE.


/*------------------------------------------------------------------------------
    Procedure: LocalDisconnectAppServerDB
  Description: Disconnect previously connected databases got from AppServer.
------------------------------------------------------------------------------*/
PROCEDURE LocalDisconnectAppServerDB:
    DEFINE VARIABLE i             AS INTEGER      NO-UNDO.


    FOR EACH TT_DATABASE EXCLUSIVE-LOCK WHERE TT_DATABASE.lIsConnected:
        /* Delete aliases */
        DO i = 1 TO NUM-ENTRIES(TT_DATABASE.cAliasList, ",") :
            DELETE ALIAS VALUE(ENTRY(i, TT_DATABASE.cAliasList, ",")).
            /* We can't handle error here. */
        END.

        /* Do disconnection */
        DISCONNECT VALUE(TT_DATABASE.cLogicalName) NO-ERROR.
        /* We do not handle error here. */

        /* Update connection status in temp-table */
        ASSIGN TT_DATABASE.lIsConnected = NO.
        VALIDATE TT_DATABASE.
    END.
    RETURN "".
END PROCEDURE.
