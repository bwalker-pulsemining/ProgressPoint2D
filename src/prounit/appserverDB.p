/*******************************************************************************
**
**     Program: appserverDB.p
** Description: Detect currently connected database and forward to caller.
**      Author: SMarmotte
**     Created: 2013/06/29
**
********************************************************************************
**
**  Revision 1.0  2013/06/29  SMarmotte
**  - initial version
**
*******************************************************************************/

{prounit/databaseInfo.i}


DEFINE OUTPUT PARAMETER TABLE FOR TT_DATABASE.


DEFINE VARIABLE i           AS INTEGER      NO-UNDO.
DEFINE VARIABLE j           AS INTEGER      NO-UNDO.
DEFINE VARIABLE cLocalIP    AS CHARACTER    NO-UNDO   INITIAL "".


/* Initializations */
EMPTY TEMP-TABLE TT_DATABASE.
ASSIGN cLocalIP = ENTRY(1, SESSION:SERVER-CONNECTION-ID, "::") NO-ERROR.        /* @ip of AppServer */

/* Browse database connections */
REPEAT i = 1 TO NUM-DBS:
    CREATE TT_DATABASE.

    /* Get physical, logical database name and connection string */
    ASSIGN  TT_DATABASE.cPhysicalName       = PDBNAME(i)
            TT_DATABASE.cLogicalName        = LDBNAME(i)
            TT_DATABASE.cConnectionString   = " " + DBPARAM(i) + " "
            TT_DATABASE.lIsConnected        = NO
            TT_DATABASE.cConnectionString   = REPLACE(TT_DATABASE.cConnectionString, " 0.0.0.0 ", cLocalIP)
            TT_DATABASE.cConnectionString   = REPLACE(TT_DATABASE.cConnectionString, " 127.0.0.1 ", cLocalIP)
            TT_DATABASE.cConnectionString   = REPLACE(TT_DATABASE.cConnectionString, " localhost ", cLocalIP).

    /* Get aliases */
    REPEAT j = 1 TO NUM-ALIASES:
        IF LDBNAME(ALIAS(j)) = LDBNAME(i) THEN
            ASSIGN TT_DATABASE.cAliasList = TRIM(TT_DATABASE.cAliasList + "," + ALIAS(j), ",").
    END.

    VALIDATE TT_DATABASE.
    RELEASE TT_DATABASE.
END.

RETURN "".
