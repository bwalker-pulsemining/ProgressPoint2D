/*******************************************************************************
**
**     Program: startProUnitBatch.p
** Description: Runs ProUnit in Batch Mode.
**      Author: Flavio Eduardo de Córdova
**     Created: 12/30/2004
**
********************************************************************************
**
**  Revision 1.2  2013/07/22 SMarmotte
**  - renamed to be coherent with other starter
**
**  Revision 1.1  2013/09/15 SMarmotte
**  - added propath adjustement
**  - if the program was the first launched by Progress, it will end the session
**  - renamed with explicit name telling this is the first program to run in batch mode
**
*******************************************************************************/

DEFINE VARIABLE cSearch     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iIndex      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOldPropath AS CHARACTER   NO-UNDO  INITIAL ?.


/* Try to locate current file (maybe .p file, maybe .r file) */
ASSIGN cSearch = SEARCH("startProUnitBatch.p").
IF cSearch = ? THEN
    ASSIGN cSearch = SEARCH("startProUnitBatch.r").

IF cSearch <> ? THEN DO:
    /* If file is embedded in a PL file, then extract path to PL */
    ASSIGN iIndex = INDEX(cSearch, ".pl<<").
    IF iIndex > 0 THEN
        ASSIGN cSearch = SUBSTRING(cSearch, 1, iIndex + 3).

    /* Extract the directory name from the file  */
    ASSIGN  cSearch = SUBSTRING(cSearch, 1, MAXIMUM(R-INDEX(cSearch, "\"), R-INDEX(cSearch, "/")) - 1)
            cSearch = REPLACE(cSearch, "~\", "/").

    /* Adjust propath */
    ASSIGN  cOldPropath = PROPATH
            PROPATH = cSearch + "," + PROPATH.
            PROPATH = cSearch + "/prounit" + "," + PROPATH.
END.
PUT UNFORMATTED cSearch SKIP.
/* Run the main program */
RUN prounit/batchRunner.p.
IF cOldPropath > "" THEN
    ASSIGN PROPATH = cOldPropath.

/* Exit */
IF PROGRAM-NAME(2) = ? THEN
    QUIT.
RETURN "".

