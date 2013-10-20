/*******************************************************************************
**
**     Program: template.p
** Description: Template to export results as simple text file
**      Author: SMarmotte
**     Created: 05/21/2013
**
********************************************************************************
**
**  Revision 1.0  2013/05/21 SMarmotte
**  - first release
**
*******************************************************************************/

{prounit/suiteRunner.i}


FUNCTION getIndentLevel RETURNS INTEGER PRIVATE (p_iTestItemId AS INTEGER) FORWARDS.


/* -------------------------------------------------------------------------- */
/* setTestData                                                                */
/* -------------------------------------------------------------------------- */
PROCEDURE setTestData:
    DEFINE INPUT PARAMETER TABLE FOR ttTestItem.
    DEFINE INPUT PARAMETER TABLE FOR ttUnitTest.


    RETURN "".
END.




/* -------------------------------------------------------------------------- */
/* saveResults                                                                */
/* -------------------------------------------------------------------------- */
PROCEDURE saveResults:
    DEFINE INPUT PARAMETER p_cFileName      AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cIndent     AS CHARACTER   NO-UNDO.


    /* Remove the ".xml" extension */
    IF p_cFileName MATCHES "*.xml" THEN
        ASSIGN  p_cFileName = RIGHT-TRIM(p_cFileName, "xml")
                p_cFileName = RIGHT-TRIM(p_cFileName, ".")
                p_cFileName = p_cFileName + ".txt".

    /* Export as text */
    OUTPUT TO VALUE(p_cFileName).
    RUN procExport(INPUT 0).    /* Because of test items hierarchy, we need to recurse when browsing the TT */
    OUTPUT CLOSE.

    RETURN "".
END.



PROCEDURE procExport PRIVATE:
    DEFINE INPUT PARAMETER p_id AS INTEGER.

    DEFINE BUFFER bTI FOR ttTestItem.
    DEFINE VARIABLE cIndent     AS CHARACTER   NO-UNDO.


    FOR EACH bTI NO-LOCK WHERE bTI.parentSet = p_id:
        ASSIGN cIndent = FILL(" ", getIndentLevel(bTI.id)).
        IF bTI.type = {&TEST_CASE} THEN DO:
            PUT UNFORMATTED cIndent + bTI.itemName + " (" + getStatusLabel(bTI.runningStatus) + ")" SKIP.
            FOR EACH ttUnitTest NO-LOCK WHERE ttUnitTest.testItemId = bTI.id:
                PUT UNFORMATTED cIndent + "  " + QUOTER(ttUnitTest.name) + " --> " + getStatusLabel(ttUnitTest.runningStatus) SKIP.
                IF ttUnitTest.runningStatus <> {&STATUS_SUCCESS} THEN
                  PUT UNFORMATTED   cIndent + "    " + "First failed assert: " + STRING(ttUnitTest.firstAssertFailed) SKIP
                                    cIndent + "    " + "Message: " + REPLACE(ttUnitTest.txtMessage, CHR(10), " - ") SKIP.
            END.
        END.
        ELSE IF bTI.type = {&TEST_SET} THEN DO:
            PUT UNFORMATTED "" SKIP.
            PUT UNFORMATTED cIndent + bTI.itemName SKIP.
            RUN procExport(INPUT bTI.id).
        END.
    END.
    RETURN "".
END PROCEDURE.



FUNCTION getIndentLevel RETURNS INTEGER PRIVATE (p_iTestItemId AS INTEGER):
    DEFINE BUFFER bTI FOR ttTestItem.
    DEFINE VARIABLE iIndent     AS INTEGER     NO-UNDO      INITIAL 0.
    DEFINE VARIABLE iParentSet  AS INTEGER     NO-UNDO.


    FIND FIRST bTI NO-LOCK WHERE bTI.id = p_iTestItemId NO-ERROR.
    DO WHILE AVAILABLE(bTI):
        ASSIGN  iIndent = iIndent + 2
                iParentSet = bTI.parentSet.
        FIND FIRST bTI NO-LOCK WHERE bTI.id = iParentSet NO-ERROR.
    END.
    RETURN (IF iIndent > 0 THEN iIndent - 2 ELSE 0).
END FUNCTION.
