/*******************************************************************************
**
**     Program: template.p
** Description: Template to export results as CSV file
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


FUNCTION getTestSetPath RETURNS CHARACTER PRIVATE (p_iTestItemId AS INTEGER) FORWARDS.


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

    DEFINE VARIABLE cTestSetPath    AS CHARACTER   NO-UNDO.


    /* Remove the ".xml" extension */
    IF p_cFileName MATCHES "*.xml" THEN
        ASSIGN  p_cFileName = RIGHT-TRIM(p_cFileName, "xml")
                p_cFileName = RIGHT-TRIM(p_cFileName, ".")
                p_cFileName = p_cFileName + ".csv".

    /* Export as text */
    OUTPUT TO VALUE(p_cFileName).
    PUT UNFORMATTED "Test Set Full Path;Test Case;Procedure Test Name;Result;First Failed Assert;Error Description" SKIP.
    RUN procExport(INPUT 0).    /* Because of test items hierarchy, we need to recurse when browsing the TT */
    OUTPUT CLOSE.

    RETURN "".
END.



PROCEDURE procExport PRIVATE:
    DEFINE INPUT PARAMETER p_id AS INTEGER.

    DEFINE BUFFER bTI FOR ttTestItem.


    FOR EACH bTI NO-LOCK WHERE bTI.parentSet = p_id:
        IF bTI.type = {&TEST_CASE} THEN DO:
            FOR EACH ttUnitTest NO-LOCK WHERE ttUnitTest.testItemId = bTI.id:
                PUT UNFORMATTED getTestSetPath(bTI.parentSet) + ";" +
                                bTI.itemName + ";" +
                                ttUnitTest.name + ";" +
                                getStatusLabel(bTI.runningStatus) + ";" +
                                (IF ttUnitTest.runningStatus = {&STATUS_SUCCESS} THEN "" ELSE STRING(ttUnitTest.firstAssertFailed)) + ";" +
                                (IF ttUnitTest.runningStatus = {&STATUS_SUCCESS} THEN "" ELSE REPLACE(ttUnitTest.txtMessage, CHR(10), " - "))
                                SKIP.
            END.
        END.
        ELSE IF bTI.type = {&TEST_SET} THEN
            RUN procExport(INPUT bTI.id).
    END.
    RETURN "".
END PROCEDURE.



FUNCTION getTestSetPath RETURNS CHARACTER PRIVATE (p_iTestItemId AS INTEGER):
    DEFINE BUFFER bTI FOR ttTestItem.
    DEFINE VARIABLE cPath       AS CHARACTER   NO-UNDO      INITIAL "".
    DEFINE VARIABLE iParentSet  AS INTEGER     NO-UNDO.


    FIND FIRST bTI NO-LOCK WHERE bTI.id = p_iTestItemId NO-ERROR.
    DO WHILE AVAILABLE(bTI):
        ASSIGN  cPath = bTI.itemName + "/" + cPath
                iParentSet = bTI.parentSet.
        FIND FIRST bTI NO-LOCK WHERE bTI.id = iParentSet NO-ERROR.
    END.
    RETURN RIGHT-TRIM(cPath, "/").
END FUNCTION.
