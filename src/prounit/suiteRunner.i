/*******************************************************************************
**
**     Include: suiteRunner.i
** Description: Tables and pre-processors definitions.
**      Author: Flavio Eduardo de Córdova
**     Created: ???
**
********************************************************************************
**
**  Revision 1.1  2013/05/21  SMarmotte
**  - added function getStatusLabel common to many source files
**
*******************************************************************************/



/* -------------------------------------------------------------------------- */
/* DEFINES                                                                    */
/* -------------------------------------------------------------------------- */
&GLOBAL-DEFINE TEST_SET         1
&GLOBAL-DEFINE TEST_CASE        2

&GLOBAL-DEFINE STATUS_SUCCESS   1
&GLOBAL-DEFINE STATUS_WARNING   2
&GLOBAL-DEFINE STATUS_FAIL      3



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttTestItem NO-UNDO
    FIELD id            AS INTEGER
    FIELD type          AS INTEGER          /* {&TEST_SET}, {&TEST_CASE} */
    FIELD itemName      AS CHARACTER
    FIELD runningStatus AS INTEGER          INITIAL 1
    FIELD parentSet     AS INTEGER          INITIAL 0
    INDEX idxMain       IS PRIMARY UNIQUE id
    INDEX idxParentItem parentSet.

DEFINE TEMP-TABLE ttUnitTest NO-UNDO
    FIELD testItemId        AS INTEGER
    FIELD seq               AS INTEGER
    FIELD name              AS CHARACTER
    FIELD runningStatus     AS INTEGER      INITIAL 1
    FIELD txtMessage        AS CHARACTER
    FIELD firstAssertFailed AS INTEGER      INITIAL 0
    FIELD totalTime         AS INTEGER
    INDEX idxMain IS PRIMARY testItemId seq.



/* -------------------------------------------------------------------------- */
/*    Function: getStatusLabel                                                */
/* Description: Returns a label for each valid execution status.              */
/* -------------------------------------------------------------------------- */
FUNCTION getStatusLabel RETURNS CHARACTER (p_iStatus AS INTEGER) :
    CASE p_iStatus:
        WHEN {&STATUS_FAIL}     THEN RETURN "Fail".
        WHEN {&STATUS_WARNING}  THEN RETURN "Warning".
        WHEN {&STATUS_SUCCESS}  THEN RETURN "Success".
    END CASE.
    RETURN "???".
END.
