/*******************************************************************************
**
**  File: startProUnitGUI.p (entry point to ProUnit GUI)
**  Description: Graphical interface for ProUnit
**  Author: Flavio Eduardo de Córdova
**  Created: 12/30/2004
**
********************************************************************************
**
**  Revision 1.5  2013/05/09 SMarmotte
**  - added propath adjustement (for .wrx file)
**  - renamed file from .w to .p with explicit name telling this is the first program to run in GUI mode
**  - if the program was the first launched by Progress, it will end the session
**
**  Revision 1.4  2005/05/02 20:35:54  fcordova
**  - new User interface.
**
**  Revision 1.3  2005/04/26 12:00:14  fcordova
**  - new User Interface.
**
**  Revision 1.2  2005/03/15 12:52:59  fcordova
**  - it wasn't showing the results correctly after running a suite with more than one test case.
**
**  Revision 1.1  2005/03/04 22:11:22  fcordova
**  - first version.
**
*******************************************************************************/

DEFINE VARIABLE cSearch     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iIndex      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOldPropath AS CHARACTER   NO-UNDO  INITIAL ?.


/* Try to locate current file (maybe .p file, maybe .r file) */
ASSIGN cSearch = SEARCH("startProUnitGUI.p").
IF cSearch = ? THEN
    ASSIGN cSearch = SEARCH("startProUnitGUI.r").

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

/* Run the main window */
RUN prounit/proUnitGUI.w.
IF cOldPropath > "" THEN
    ASSIGN PROPATH = cOldPropath.

/* Exit */
IF PROGRAM-NAME(2) = ? THEN
    QUIT.
RETURN "".
