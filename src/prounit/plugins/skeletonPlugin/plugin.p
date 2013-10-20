/*******************************************************************************
**
**     Program: plugin.p
** Description: plugin skeleton getting notifications about run tests
**      Author: SMarmotte
**     Created: 05/21/2013
**
********************************************************************************
**
**  Revision 1.0  2013/05/21 SMarmotte
**  - first release
**
*******************************************************************************/


/* -------------------------------------------------------------------------- */
/* beforeRunningTestCase                                                      */
/* -------------------------------------------------------------------------- */
PROCEDURE beforeRunningTestCase:
    DEFINE INPUT PARAMETER p_iItemId    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER p_cItemName  AS CHARACTER 	NO-UNDO.


    MESSAGE "[beforeRunningTestCase]" SKIP(1)
            "p_iItemId = " + QUOTER(p_iItemId) SKIP
            "p_cItemName = " + QUOTER(p_cItemName)
            VIEW-AS ALERT-BOX INFORMATION
            TITLE THIS-PROCEDURE:FILE-NAME.
    RETURN "".
END.


/* -------------------------------------------------------------------------- */
/* beforeRunningTest                                                          */
/* -------------------------------------------------------------------------- */
PROCEDURE beforeRunningTest:
    DEFINE INPUT PARAMETER p_iTestCaseId    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER p_cTestName      AS CHARACTER    NO-UNDO.


    MESSAGE "[beforeRunningTest]" SKIP(1)
            "p_iTestCaseId = " + QUOTER(p_iTestCaseId) SKIP
            "p_cTestName = " + QUOTER(p_cTestName)
            VIEW-AS ALERT-BOX INFORMATION
            TITLE THIS-PROCEDURE:FILE-NAME.
    RETURN "".
END.


/* -------------------------------------------------------------------------- */
/* afterRunningTest                                                           */
/* -------------------------------------------------------------------------- */
PROCEDURE afterRunningTest:
    DEFINE INPUT PARAMETER p_iTestCaseId        AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER p_cTestName          AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER p_iStatus            AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER p_cMessage           AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER p_iFirstAssert       AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER p_iTotalTime         AS INTEGER          NO-UNDO.


    MESSAGE "[beforeRunningTest]" SKIP(1)
            "p_iTestCaseId = " + QUOTER(p_iTestCaseId) SKIP
            "p_cTestName = " + QUOTER(p_cTestName) SKIP
            "p_iStatus = " + QUOTER(p_iStatus) SKIP
            "p_cMessage = " + QUOTER(p_cMessage) SKIP
            "p_iFirstAssert = " + QUOTER(p_iFirstAssert) SKIP
            "p_iTotalTime = " + QUOTER(p_iTotalTime) SKIP
            VIEW-AS ALERT-BOX INFORMATION
            TITLE THIS-PROCEDURE:FILE-NAME.
    RETURN "".
END.


/* -------------------------------------------------------------------------- */
/* afterRunningTestCase                                                       */
/* -------------------------------------------------------------------------- */
PROCEDURE afterRunningTestCase:
    DEFINE INPUT PARAMETER p_iItemId        AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER p_cItemName      AS CHARACTER 	    NO-UNDO.
    DEFINE INPUT PARAMETER p_iStatus        AS INTEGER          NO-UNDO.


    MESSAGE "[beforeRunningTestCase]" SKIP(1)
            "p_iItemId = " + QUOTER(p_iItemId) SKIP
            "p_cItemName = " + QUOTER(p_cItemName)
            "p_iStatus = " + QUOTER(p_iStatus) SKIP
            VIEW-AS ALERT-BOX INFORMATION
            TITLE THIS-PROCEDURE:FILE-NAME.
    RETURN "".
END.
