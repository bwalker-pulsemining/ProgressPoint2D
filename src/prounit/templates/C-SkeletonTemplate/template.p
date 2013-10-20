/*******************************************************************************
**
**     Program: template.p
** Description: Template to export results as simple .d files
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


    /* Remove the ".xml" extension */
    IF p_cFileName MATCHES "*.xml" THEN
        ASSIGN  p_cFileName = RIGHT-TRIM(p_cFileName, "xml")
                p_cFileName = RIGHT-TRIM(p_cFileName, ".").

    OUTPUT TO VALUE(p_cFileName + "_ttTestItem.d").
    FOR EACH ttTestItem NO-LOCK:
        EXPORT ttTestItem.
    END.
    OUTPUT CLOSE.

    OUTPUT TO VALUE(p_cFileName + "_ttUnitTest.d").
    FOR EACH ttUnitTest NO-LOCK:
        EXPORT ttUnitTest.
    END.
    OUTPUT CLOSE.

    RETURN "".
END.

