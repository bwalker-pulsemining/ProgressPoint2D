&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*******************************************************************************
**
**  File: proUnitGUI.w
**  Description: Graphical interface for ProUnit
**  Author: Flavio Eduardo de Córdova
**  Created: 2004/12/30
**
********************************************************************************
**
**  Revision 2.2  2013/06/29  SMarmotte
**  - added icon to main window
**  - added database tools revision in ProUnit version display
**  - when used with Progress v9, ProUnit now displays a warning telling all feature will not be usable
**
**  Revision 2.1  2013/06/10  SMarmotte
**  - main window is now displaying the ProUnit release as major version information instead of GUI revision and core revision
**  - added protection to avoid some unit tests to close PoUnit main window especially with some GUI unit tests
**  - the <ESC> keyboard key will not close the main window anymore
**  - corrected the final status when no test was marked as passed or failed
**  - ProUnit now ensures suiteRunner is properly loaded before starting tests run (suiteRunner.p may be rarely unloaded in case of problems with some unit tests)
**  - minor GUI improvements
**
**  Revision 2.0  2013/05/22  SMarmotte
**  - added error handling on result file save
**  - added browse and plugins refresh and when loading configuration file
**  - added reference to suiteRunner.i and refactored code in this way
**  - added a link to the ProUnit website
**  - fixed bug when a plugin referenced in configuration file does not exist
**  - when selecting a test case file, the focus now goes to the "OK" button
**  - improved main window load
**  - improved plugins dialog box load and GUI
**  - added information about Progress OpenEdge version
**  - corrected an error when selecting an axisting file to save result (last char before extension was lost)
**  - minor GUI changes and improvements
**
**  Revision 1.9  2013/05/15  SMarmotte
**  - fixed plugin error (duplicated records error - 132) when a project was loaded from XML file
**  - improved plugins dialog box
**  - improved "Save result" dialog box
**  - corrected bugs with "Save result" dialog box where no file is selected and OK is pressed
**  - added item count check before saving (this avoids some errors)
**  - extended maximum test case path size (was limited to 40 chars)
**  - better error handling in GUI
**
**  Revision 1.8  2013/05/08  SMarmotte
**  - removed menus (because they were poor)
**    - exit option is the top-right window close button
**    - versions are now displayed in the window title bar and in the root element of the treeview widget
**  - added rename button for test sets
**  - added default button to creation dialogs (test case, test set)
**  - reverted to on-the-fly frame creation for data feed and merged improvements of v1.7
**  - corrected movement bugs in the treeview (up, down, left, right buttons)
**  - changed GUI and visual space
**
**  Revision 1.7  2009/01/07 19:17:51  spark_br
**  - added include with plugin's temp-table definition
**  - changed suiteRunner to use the include
**  - changed prounitGUI to use include
**  - changed visual space and add testCase dialog
**
**  Revision 1.6  2006/03/12 11:16:43  fcordova
**  Bug fix: Displaying error messages on text editor while the suite is running.
**
**  Revision 1.5  2006/02/02 23:16:04  fcordova
**  *** empty log message ***
**
**  Revision 1.4  2006/01/05 23:43:07  fcordova
**  *** empty log message ***
**
**  Revision 1.3  2005/12/29 14:52:06  fcordova
**  Plugins
**
**  Revision 1.2  2005/12/12 00:04:24  fcordova
**  *** empty log message ***
**
**  Revision 1.1  2005/11/01 01:09:50  fcordova
**  *** empty log message ***
**
**  Revision 0.4  2005/05/02 20:35:54  fcordova
**  New User interface.
**
**  Revision 0.3  2005/04/26 12:00:14  fcordova
**  New User Interface.
**
**  Revision 0.2  2005/03/15 12:52:59  fcordova
**  It wasn't showing the results correctly after running a suite with more than one test case.
**
**  Revision 0.1  2005/03/04 22:11:22  fcordova
**  First version.
**
*******************************************************************************/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* -------------------------------------------------------------------------- */
/* CONSTANTS                                                                  */
/* -------------------------------------------------------------------------- */
&SCOPED-DEFINE THIS-VERSION "$Revision: 2.2 $"
&SCOPED-DEFINE THIS-RELEASE "10"



/* -------------------------------------------------------------------------- */
/* INCLUDES                                                                   */
/* -------------------------------------------------------------------------- */
{prounit/suiteRunner.i}
{prounit/plugins.i}



/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttTestItemGUI NO-UNDO LIKE ttTestItem
    FIELD seq           AS INTEGER
    FIELD TreeViewHdl   AS COM-HANDLE
    INDEX sequence      seq.

DEFINE TEMP-TABLE ttUnitTestGUI NO-UNDO LIKE ttUnitTest
    FIELD runningStatusText AS CHARACTER.



/* -------------------------------------------------------------------------- */
/* VARIALBES                                                                  */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE iSelection          AS INTEGER              NO-UNDO.
DEFINE VARIABLE chRoot              AS COM-HANDLE           NO-UNDO.
DEFINE VARIABLE iCurrentTest        AS INTEGER              NO-UNDO.
DEFINE VARIABLE lastExecDate        AS DATE                 NO-UNDO.
DEFINE VARIABLE lastExecTime        AS INTEGER              NO-UNDO.
DEFINE VARIABLE lastElapsedTime     AS INTEGER              NO-UNDO.
DEFINE VARIABLE hRunner             AS HANDLE               NO-UNDO.
DEFINE VARIABLE iResultSequence     AS INTEGER              NO-UNDO     INITIAL 0.
DEFINE VARIABLE iLastStatus         AS INTEGER              NO-UNDO.
DEFINE VARIABLE iStatus             AS INTEGER              NO-UNDO.
DEFINE VARIABLE iPreviousRed        AS INTEGER              NO-UNDO.
DEFINE VARIABLE iPreviousGreen      AS INTEGER              NO-UNDO.
DEFINE VARIABLE iPreviousBlue       AS INTEGER              NO-UNDO.
DEFINE VARIABLE lCanClose           AS LOGICAL              NO-UNDO     INITIAL YES.



/* Global variable that can be used to start proUnitGUI with some test cases already loaded.
   More than one test case may be set using comma.
   See online help for more information about this variable */
DEFINE NEW GLOBAL SHARED VARIABLE cInitTestCases    AS CHARACTER        NO-UNDO.



/* -------------------------------------------------------------------------- */
/* EXTERNAL LIBRARY FUNCTIONS                                                 */
/* -------------------------------------------------------------------------- */
PROCEDURE ShellExecuteA EXTERNAL "shell32.dll" :
    DEFINE INPUT PARAMETER hWnd             AS LONG.
    DEFINE INPUT PARAMETER lpOperation      AS CHARACTER.
    DEFINE INPUT PARAMETER lpFile           AS CHARACTER.
    DEFINE INPUT PARAMETER lpParameters     AS CHARACTER.
    DEFINE INPUT PARAMETER lpDirectory      AS CHARACTER.
    DEFINE INPUT PARAMETER nShowCmd         AS LONG.
    DEFINE RETURN PARAMETER hInstance       AS LONG.
END.



/* -------------------------------------------------------------------------- */
/* DEBUG CODE                                                                 */
/* -------------------------------------------------------------------------- */
OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "puTemp.txt").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brwResult

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUnitTestGUI

/* Definitions for BROWSE brwResult                                     */
&Scoped-define FIELDS-IN-QUERY-brwResult ttUnitTestGUI.name ttUnitTestGUI.runningStatusText ttUnitTestGUI.totalTime ttUnitTestGUI.firstAssertFailed ttUnitTestGUI.txtMessage
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwResult
&Scoped-define SELF-NAME brwResult
&Scoped-define QUERY-STRING-brwResult FOR EACH ttUnitTestGUI WHERE ttUnitTestGUI.testItemId = iSelection
&Scoped-define OPEN-QUERY-brwResult OPEN QUERY {&SELF-NAME} FOR EACH ttUnitTestGUI WHERE ttUnitTestGUI.testItemId = iSelection.
&Scoped-define TABLES-IN-QUERY-brwResult ttUnitTestGUI
&Scoped-define FIRST-TABLE-IN-QUERY-brwResult ttUnitTestGUI


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brwResult}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwResult errorMsg btAddSet btAddTC btDown ~
btLoad btPlugin btRemove btRename btRun btSave btSaveResult btLeft btRight ~
btUp FL_URL FL_V9WARNING FL_PROVERSION lblStatus
&Scoped-Define DISPLAYED-OBJECTS errorMsg FL_URL FL_V9WARNING FL_PROVERSION ~
lblStatus

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE cfImageList AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chcfImageList AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE cfProgressBar AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chcfProgressBar AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btAddSet  NO-FOCUS FLAT-BUTTON
     LABEL "Add a Set"
     SIZE 4.4 BY 1.05 TOOLTIP "Add a test set (folder)".

DEFINE BUTTON btAddTC AUTO-GO  NO-FOCUS FLAT-BUTTON
     LABEL "Add a Test"
     SIZE 4.4 BY 1.05 TOOLTIP "Add a test case (dedicated program)".

DEFINE BUTTON btDown  NO-FOCUS FLAT-BUTTON
     LABEL "Down"
     SIZE 4.4 BY 1.05 TOOLTIP "Move down (if possible)".

DEFINE BUTTON btLeft  NO-FOCUS FLAT-BUTTON
     LABEL "Left"
     SIZE 4.4 BY 1.05 TOOLTIP "Move left (if possible)".

DEFINE BUTTON btLoad  NO-FOCUS FLAT-BUTTON
     LABEL "Load"
     SIZE 4.4 BY 1.05 TOOLTIP "Load project from XML file".

DEFINE BUTTON btPlugin  NO-FOCUS FLAT-BUTTON
     LABEL "Conf.Plugins"
     SIZE 4.4 BY 1.05 TOOLTIP "Configure plugins".

DEFINE BUTTON btRemove  NO-FOCUS FLAT-BUTTON
     LABEL "Remove"
     SIZE 4.4 BY 1.05 TOOLTIP "Remove selected item (test set or test case)".

DEFINE BUTTON btRename  NO-FOCUS FLAT-BUTTON
     LABEL "Rename"
     SIZE 4.4 BY 1.05 TOOLTIP "Rename selected test set".

DEFINE BUTTON btRight  NO-FOCUS FLAT-BUTTON
     LABEL "Right"
     SIZE 4.4 BY 1.05 TOOLTIP "Move right (if possible)".

DEFINE BUTTON btRun  NO-FOCUS FLAT-BUTTON
     LABEL "Run all tests!"
     SIZE 4.4 BY 1.05 TOOLTIP "Run all tests".

DEFINE BUTTON btSave  NO-FOCUS FLAT-BUTTON
     LABEL "Save"
     SIZE 4.4 BY 1.05 TOOLTIP "Save project to XML file".

DEFINE BUTTON btSaveResult  NO-FOCUS FLAT-BUTTON
     LABEL "Save results"
     SIZE 4.4 BY 1.05 TOOLTIP "Save results".

DEFINE BUTTON btUp  NO-FOCUS FLAT-BUTTON
     LABEL "Up"
     SIZE 4.4 BY 1.05 TOOLTIP "Move up (if possible)".

DEFINE VARIABLE errorMsg AS CHARACTER
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 169 BY 3.57
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FL_PROVERSION AS CHARACTER FORMAT "X(256)":U INITIAL "Progress | OpenEdge version xxxx"
      VIEW-AS TEXT
     SIZE 45 BY .76 NO-UNDO.

DEFINE VARIABLE FL_URL AS CHARACTER FORMAT "X(256)":U INITIAL "http://sourceforge.net/projects/prounit/"
      VIEW-AS TEXT
     SIZE 45 BY .76 TOOLTIP "Visit web site"
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FL_V9WARNING AS CHARACTER FORMAT "X(256)":U
      VIEW-AS TEXT
     SIZE 66 BY .76
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE lblStatus AS CHARACTER FORMAT "X(256)":U
     LABEL "Final status"
      VIEW-AS TEXT
     SIZE 18 BY .71 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwResult FOR
      ttUnitTestGUI SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwResult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwResult C-Win _FREEFORM
  QUERY brwResult DISPLAY
      ttUnitTestGUI.name                FORMAT "x(40)"              WIDTH 40    COLUMN-LABEL "Test Procedure Name"
      ttUnitTestGUI.runningStatusText   FORMAT "x(10)"              WIDTH 10    COLUMN-LABEL "Result"
      ttUnitTestGUI.totalTime           FORMAT ">>>,>>9 ms"         WIDTH 10    COLUMN-LABEL "Time"
      ttUnitTestGUI.firstAssertFailed   FORMAT ">>>,>>9"            WIDTH 8     COLUMN-LABEL "1st fail"
      ttUnitTestGUI.txtMessage          FORMAT "x(100)"                         COLUMN-LABEL "Error description"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 169 BY 30
         BGCOLOR 15  ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brwResult AT ROW 2.67 COL 56
     errorMsg AT ROW 32.91 COL 56 NO-LABEL
     btAddSet AT ROW 1.24 COL 18
     btAddTC AT ROW 1.24 COL 24
     btDown AT ROW 1.24 COL 55
     btLoad AT ROW 1.24 COL 2
     btPlugin AT ROW 1.24 COL 92
     btRemove AT ROW 1.24 COL 33
     btRename AT ROW 1.24 COL 39.2
     btRun AT ROW 1.24 COL 77
     btSave AT ROW 1.24 COL 8
     btSaveResult AT ROW 1.24 COL 86
     btLeft AT ROW 1.24 COL 61.6
     btRight AT ROW 1.24 COL 67.6
     btUp AT ROW 1.24 COL 49
     FL_URL AT ROW 1 COL 180 NO-LABEL
     FL_V9WARNING AT ROW 1.43 COL 108 NO-LABEL
     FL_PROVERSION AT ROW 1.81 COL 180 NO-LABEL
     lblStatus AT ROW 36.71 COL 25 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 224 BY 36.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "ProUnit Test Framework"
         HEIGHT             = 36.67
         WIDTH              = 224
         MAX-HEIGHT         = 38.05
         MAX-WIDTH          = 274.8
         VIRTUAL-HEIGHT     = 38.05
         VIRTUAL-WIDTH      = 274.8
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brwResult 1 DEFAULT-FRAME */
ASSIGN
       brwResult:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN
       errorMsg:RETURN-INSERTED IN FRAME DEFAULT-FRAME  = TRUE
       errorMsg:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FL_PROVERSION IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN
       FL_PROVERSION:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FL_URL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN
       FL_URL:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FL_V9WARNING IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN
       FL_V9WARNING:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwResult
/* Query rebuild information for BROWSE brwResult
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUnitTestGUI WHERE ttUnitTestGUI.testItemId = iSelection.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE brwResult */
&ANALYZE-RESUME




/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME cfImageList ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 98
       HEIGHT          = 1.67
       WIDTH           = 8
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.67
       COLUMN          = 1
       HEIGHT          = 33.81
       WIDTH           = 54
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME cfProgressBar ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 36.71
       COLUMN          = 57
       HEIGHT          = .71
       WIDTH           = 166
       HIDDEN          = no
       SENSITIVE       = yes.
/* cfImageList OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
/* cfProgressBar OCXINFO:CREATE-CONTROL from: {0713E8D2-850A-101B-AFC0-4210102A8DA7} type: ProgressBar */
      cfImageList:MOVE-BEFORE(brwResult:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame:MOVE-AFTER(cfImageList).
      cfProgressBar:MOVE-AFTER(errorMsg:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON CLOSE OF C-Win /* ProUnit Test Framework */
DO:
    IF NOT lCanClose THEN
        RETURN NO-APPLY.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ProUnit Test Framework */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    /* This case occurs when the user presses the "Esc" key. */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ProUnit Test Framework */
DO:
    IF lCanClose THEN DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        IF VALID-HANDLE(hRunner) THEN
            DELETE OBJECT hRunner.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON GO OF FRAME DEFAULT-FRAME
DO:
    APPLY "CHOOSE" TO btRun.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwResult
&Scoped-define SELF-NAME brwResult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwResult C-Win
ON ROW-DISPLAY OF brwResult IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE cellHandle      AS HANDLE       NO-UNDO.


    ASSIGN  cellHandle = ttUnitTestGUI.runningStatusText:HANDLE IN BROWSE brwResult.
            errorMsg:SCREEN-VALUE = ttUnitTestGUI.txtMessage.

    IF ttUnitTestGUI.runningStatus = {&STATUS_SUCCESS} THEN
        ASSIGN  cellHandle:BGCOLOR = 10                 /* Light green */
                cellHandle:FGCOLOR = 0.
    ELSE IF ttUnitTestGUI.runningStatus = {&STATUS_FAIL} THEN
        ASSIGN  cellHandle:BGCOLOR = 12                 /* Light red */
                cellHandle:FGCOLOR = 15.
    ELSE
        ASSIGN  cellHandle:BGCOLOR = 14                 /* Yellow */
                cellHandle:FGCOLOR = 0.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwResult C-Win
ON VALUE-CHANGED OF brwResult IN FRAME DEFAULT-FRAME
DO:
    ASSIGN errorMsg:SCREEN-VALUE = ttUnitTestGUI.txtMessage.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddSet C-Win
ON CHOOSE OF btAddSet IN FRAME DEFAULT-FRAME /* Add a Set */
DO:
    DEFINE VARIABLE cTestSet        AS CHARACTER    NO-UNDO     VIEW-AS FILL-IN SIZE 41 BY 1.0.

    DEFINE BUTTON btOK              LABEL "OK" AUTO-GO SIZE 10 BY 1.2.

    DEFINE FRAME fAddTestSet
        "Test Set Name (Folder):" VIEW-AS TEXT AT ROW 1.5 COL 4
        cTestSet FORMAT "x(40)" NO-LABEL AT ROW 2.5 COL 4
        btOK AT ROW 2.4 COL 46
        WITH
            THREE-D
            SIZE 60 BY 5
            VIEW-AS DIALOG-BOX
            TITLE "New Test Set"
            DEFAULT-BUTTON btOK.

    ON "WINDOW-CLOSE":U OF FRAME fAddTestSet DO:
        ASSIGN cTestSet:SCREEN-VALUE = "".
        APPLY "CHOOSE" TO btOK.
        RETURN "".
    END.


    IF chCtrlFrame:TreeView:SelectedItem:KEY BEGINS "test_" THEN
        MESSAGE "Can't add item here." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    ELSE DO:
        ENABLE ALL WITH FRAME fAddTestSet.
        UPDATE cTestSet WITH FRAME fAddTestSet.
        HIDE FRAME fAddTestSet.

        IF cTestSet <> ? AND cTestSet <> "" THEN
            RUN addNewSet (cTestSet, chCtrlFrame:TreeView:SelectedItem).
    END.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btAddTC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btAddTC C-Win
ON CHOOSE OF btAddTC IN FRAME DEFAULT-FRAME /* Add a Test */
DO:
    DEFINE VARIABLE cTestName       AS CHARACTER    NO-UNDO     VIEW-AS FILL-IN SIZE 41 BY 1.0.
    DEFINE BUTTON btOK                                          LABEL "OK" AUTO-GO SIZE 10 BY 1.2.
    DEFINE BUTTON btBrowse                                      LABEL "..." SIZE 5 BY 1.2.

    DEFINE FRAME fAddTestCase
        "Test program name:" VIEW-AS TEXT AT ROW 1.5 COL 4
        cTestName FORMAT "x(260)" NO-LABEL AT ROW 2.5 COL 4
        btBrowse AT ROW 2.4 COL 46
        btOK AT ROW 2.4 COL 55
        WITH
            THREE-D
            SIZE 70 BY 5
            VIEW-AS DIALOG-BOX
            TITLE "New Test Case"
            DEFAULT-BUTTON btOK.

    ON "WINDOW-CLOSE":U OF FRAME fAddTestCase DO:
        ASSIGN cTestName:SCREEN-VALUE = "".
        APPLY "CHOOSE" TO btOK.
        RETURN "".
    END.

    ON "CHOOSE":U OF btBrowse IN FRAME fAddTestCase DO:
        DEFINE VARIABLE i             AS INTEGER      NO-UNDO.
        DEFINE VARIABLE chosenFile    AS CHARACTER    NO-UNDO.
        DEFINE VARIABLE tempChar      AS CHARACTER    NO-UNDO.


        SYSTEM-DIALOG GET-FILE chosenFile FILTERS "Procedures (.p)" "*.p", "All files" "*" MUST-EXIST.

        IF chosenFile <> "" THEN DO:
          ASSIGN tempChar = ENTRY(NUM-ENTRIES(chosenFile, "\"), chosenFile, "\").

          DO i = 1 TO NUM-ENTRIES(chosenFile, "\") - 1:
              IF SEARCH(tempChar) <> ? THEN DO:
                  ASSIGN chosenFile = tempChar.
                  LEAVE.
              END.
              ELSE
                  ASSIGN tempChar = ENTRY(NUM-ENTRIES(chosenFile, "\") - i, chosenFile, "\") + "\" + tempChar.
          END.
          IF SUBSTRING(chosenFile, 2, 2) = ":~\" THEN
              ASSIGN chosenFile = LC(SUBSTRING(chosenFile, 1, 1)) + SUBSTRING(chosenFile, 2).

          ASSIGN cTestName:SCREEN-VALUE = chosenFile.
          APPLY "ENTRY":U TO btOK.
        END.
        RETURN "".
    END.


    IF chCtrlFrame:TreeView:SelectedItem:KEY BEGINS "test_" THEN
        MESSAGE "Can't add item here." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    ELSE DO:
        ENABLE ALL WITH FRAME fAddTestCase.
        UPDATE cTestName WITH FRAME fAddTestCase.
        HIDE FRAME fAddTestCase.

        IF cTestName > "" THEN
            RUN addNewTest (cTestName, chCtrlFrame:TreeView:SelectedItem).
    END.

    RETURN NO-APPLY.        /* NO-APPLY in order not to run tests automatically */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDown C-Win
ON CHOOSE OF btDown IN FRAME DEFAULT-FRAME /* Down */
DO:
    DEFINE VARIABLE chSelected          AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cSelectedKey        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE itemp               AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cNextKey            AS CHARACTER        NO-UNDO.

    DEFINE BUFFER bCurrent FOR ttTestItemGUI.
    DEFINE BUFFER bNext    FOR ttTestItemGUI.


    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't move this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    ASSIGN cSelectedKey = chSelected:KEY.

    /* If it's root or there's no node after the current, returns.. */
    IF chSelected:KEY  = "root" OR  chSelected:NEXT = 0 THEN DO:
        MESSAGE "Can't do this move." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    ASSIGN cNextKey = chSelected:NEXT:KEY.

    /* Reset sequences to ensure the current order on the temp-table */
    RUN resetSequences (chRoot, INPUT-OUTPUT iTemp).

    /* find records in temp-table */
    FIND bCurrent WHERE bCurrent.id = INTEGER(ENTRY(2, cSelectedKey, "_")) NO-LOCK NO-ERROR.
    FIND bNext WHERE bNext.id = INTEGER(ENTRY(2, cNextKey, "_")) NO-LOCK NO-ERROR.

    /* Switches Sequences */
    ASSIGN  iTemp        = bCurrent.seq
            bCurrent.seq = bNext.seq
            bNext.seq    = iTemp.

    RUN redrawTreeView.

    ASSIGN  chSelected = chCtrlFrame:TreeView:Nodes:ITEM(cSelectedKey)
            chCtrlFrame:TreeView:SelectedItem = chSelected.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLeft
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLeft C-Win
ON CHOOSE OF btLeft IN FRAME DEFAULT-FRAME /* Left */
DO:
    DEFINE VARIABLE chSelected      AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE chParent        AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE chGrandParent   AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cSelectedKey    AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iCounter        AS INTEGER          NO-UNDO.


    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't move this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    ASSIGN  cSelectedKey    = chSelected:KEY
            chParent        = chSelected:PARENT.

    /* Changing from root OR parent is root */
    IF chSelected:KEY = "root" OR NOT VALID-HANDLE(chParent) OR chParent:KEY = "root" THEN DO:
        MESSAGE "Movement not allowed here." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    RUN resetSequences (chRoot, INPUT-OUTPUT iCounter).

    ASSIGN chGrandParent = chParent:PARENT.
    IF NOT VALID-HANDLE(chGrandParent) OR chGrandParent = ? THEN DO:
        MESSAGE "Can't find parent item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    FIND ttTestItemGUI WHERE ttTestItemGUI.id = INTEGER(ENTRY(2, cSelectedKey, "_")) EXCLUSIVE-LOCK NO-ERROR.

    ASSIGN  ttTestItemGUI.parentSet = 0.
    ASSIGN  ttTestItemGUI.parentSet = INTEGER(ENTRY(2, chGrandParent:KEY, "_")) NO-ERROR.      /* Do NOT group ASSIGN */

    RUN redrawTreeView.

    ASSIGN  chSelected = chCtrlFrame:TreeView:Nodes:ITEM(cSelectedKey)
            chCtrlFrame:TreeView:SelectedItem = chSelected.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btLoad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btLoad C-Win
ON CHOOSE OF btLoad IN FRAME DEFAULT-FRAME /* Load */
DO:
    DEFINE VARIABLE cInputFile     AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lOK            AS LOGICAL      NO-UNDO.


    SYSTEM-DIALOG GET-FILE cInputFile
        TITLE      "Load Test Info"
        FILTERS    "XML File (*.xml)"   "*.xml"
        MUST-EXIST
        UPDATE lOK.

    IF NOT lOK THEN
        RETURN.

    RUN loadConfiguration (cInputFile).
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btPlugin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btPlugin C-Win
ON CHOOSE OF btPlugin IN FRAME DEFAULT-FRAME /* Conf.Plugins */
DO:
    DEFINE VARIABLE hWindow         AS HANDLE       NO-UNDO.


    RUN prounit/pluginsEdit.w PERSISTENT SET hWindow.
    IF NOT VALID-HANDLE(hWindow) OR COMPILER:ERROR THEN
        MESSAGE "** Internal error while loading prounit/pluginsEdit.w" VIEW-AS ALERT-BOX ERROR.
    ELSE DO:
        ASSIGN C-Win:SENSITIVE = FALSE.
        RUN setCaller IN hWindow (THIS-PROCEDURE).
        WAIT-FOR CLOSE OF hWindow.
        ASSIGN C-Win:SENSITIVE = TRUE.
    END.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRemove C-Win
ON CHOOSE OF btRemove IN FRAME DEFAULT-FRAME /* Remove */
DO:
    DEFINE VARIABLE chSelected          AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE lConfirmation       AS LOGICAL          NO-UNDO.

    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't delete this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    MESSAGE "Confirm delete?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lConfirmation.
    IF lConfirmation THEN
        RUN deleteItem (INTEGER(ENTRY(2, chSelected:KEY, "_"))).

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRename C-Win
ON CHOOSE OF btRename IN FRAME DEFAULT-FRAME /* Rename */
DO:
    DEFINE VARIABLE chSelected          AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE lConfirmation       AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE id                  AS INTEGER          NO-UNDO.

    DEFINE VARIABLE cTestName       AS CHARACTER    NO-UNDO     VIEW-AS FILL-IN SIZE 41 BY 1.0.
    DEFINE BUTTON btOK                                          LABEL "OK" AUTO-GO SIZE 10 BY 1.2.

    DEFINE BUFFER bTests FOR ttTestItemGUI.

    DEFINE FRAME fRenameTestCase
        "New test set name (folder):" VIEW-AS TEXT AT ROW 1.5 COL 4
        cTestName FORMAT "x(40)" NO-LABEL AT ROW 2.5 COL 4
        btOK AT ROW 2.4 COL 46
        WITH
            THREE-D
            SIZE 60 BY 5
            VIEW-AS DIALOG-BOX
            TITLE "New Test Set Name"
            DEFAULT-BUTTON btOK.

    ON "WINDOW-CLOSE":U OF FRAME fRenameTestCase DO:
        ASSIGN cTestName:SCREEN-VALUE = "".
        APPLY "CHOOSE" TO btOK.
        RETURN "".
    END.


    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't rename this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    ASSIGN id = INTEGER(ENTRY(2, chSelected:KEY, "_")).
    FIND FIRST bTests NO-LOCK WHERE bTests.id = id NO-ERROR.
    IF AVAILABLE(bTests) THEN DO:
        IF bTests.TYPE <> {&TEST_SET} THEN
            MESSAGE "Can't rename test case." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        ELSE DO:
            ASSIGN cTestName = chSelected:TEXT.
            ENABLE ALL WITH FRAME fRenameTestCase.
            UPDATE cTestName WITH FRAME frenameTestCase.
            HIDE FRAME fRenameTestCase.

            IF cTestName <> ? AND cTestName <> "" THEN
                ASSIGN  bTests.itemName = cTestName
                        chSelected:TEXT = cTestName.
        END.
    END.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRight C-Win
ON CHOOSE OF btRight IN FRAME DEFAULT-FRAME /* Right */
DO:
    DEFINE VARIABLE chSelected      AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE chPrev          AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cSelectedKey    AS CHARACTER        NO-UNDO     INITIAL ?.


    ASSIGN  chSelected = ?
            chPrev = ?
            cSelectedKey = ?.

    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't move this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    IF chSelected <> ? AND VALID-HANDLE(chSelected) THEN
        ASSIGN  chPrev = chSelected:Previous
                cSelectedKey = chSelected:KEY.

    /* Changing from root OR no previous node OR not a test set */
    IF chSelected:KEY = "root" OR chPrev = ? OR NOT VALID-HANDLE(chPrev) OR NOT chPrev:KEY BEGINS "set" THEN DO:
        MESSAGE "Movement not allowed here." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    FIND ttTestItemGUI WHERE ttTestItemGUI.id = INTEGER(ENTRY(2, cSelectedKey, "_")) EXCLUSIVE-LOCK NO-ERROR.
    ASSIGN ttTestItemGUI.parentSet = INTEGER(ENTRY(2, chPrev:KEY, "_")).

    RUN redrawTreeView.

    ASSIGN  chSelected = chCtrlFrame:TreeView:Nodes:ITEM(cSelectedKey)
            chCtrlFrame:TreeView:SelectedItem = chSelected.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btRun C-Win
ON CHOOSE OF btRun IN FRAME DEFAULT-FRAME /* Run all tests! */
DO:
    DEFINE VARIABLE iTimer      AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iCounter    AS INTEGER      NO-UNDO.


    EMPTY TEMP-TABLE ttUnitTestGUI.

    RUN resetSequences (chRoot, INPUT-OUTPUT iCounter).
    RUN resetIcons.

    ASSIGN iCounter = 0.
    FOR EACH ttTestItemGUI WHERE ttTestItemGUI.TYPE = {&TEST_CASE} NO-LOCK:
        ASSIGN iCounter = iCounter + 1.
        OS-CREATE-DIR VALUE(SESSION:TEMP-DIRECTORY + "ProUnit_BKUP").
        OS-COPY VALUE(ttTestItemGUI.itemName) VALUE(SESSION:TEMP-DIRECTORY + "ProUnit_BKUP/" + ttTestItemGUI.itemName).
    END.

    IF iCounter = 0 THEN
        MESSAGE "There is no test to run." VIEW-AS ALERT-BOX WARNING.
    ELSE DO:
        /* Ensure suite runner is properly loaded */
        IF NOT VALID-HANDLE(hRunner) THEN
            RUN initialize-core.

        ASSIGN  lastExecDate    = TODAY
                lastExecTime    = TIME
                iTimer          = TIME
                iStatus         = -1
                chCFProgressBar:ProgressBar:Max = iCounter
                chCFProgressBar:ProgressBar:Value = 0.01
                lCanClose       = NO
                C-Win:SENSITIVE = NO.

        /* Start tests */
        RUN setProjectData IN hRunner (INPUT TABLE ttTestItemGUI).
        RUN setPlugins     IN hRunner (INPUT TABLE ttPlugins).
        RUN loadPlugins    IN hRunner.
        DO ON QUIT UNDO, LEAVE ON ERROR UNDO, LEAVE:
            RUN runSuite IN hRunner.
        END.

        /* Looks for final status */
        IF CAN-FIND(FIRST ttUnitTestGUI WHERE ttUnitTestGUI.runningStatus = {&STATUS_FAIL}) THEN
            ASSIGN iStatus = {&STATUS_FAIL}.
        ELSE IF CAN-FIND(FIRST ttUnitTestGUI WHERE ttUnitTestGUI.runningStatus = {&STATUS_WARNING}) THEN
            ASSIGN iStatus = {&STATUS_WARNING}.
        ELSE IF CAN-FIND(FIRST ttUnitTestGUI WHERE ttUnitTestGUI.runningStatus = {&STATUS_SUCCESS}) THEN
            ASSIGN iStatus = {&STATUS_SUCCESS}.

        /* If nothing was found, assume a warning */
        IF iStatus = -1 THEN
            ASSIGN iStatus = {&STATUS_WARNING}.
        RUN updateStatus(iStatus).

        /* Show short report */
        ASSIGN lastElapsedTime = TIME - iTimer.
        MESSAGE "Execution finished." SKIP "Elapsed Time:" STRING(lastElapsedTime, "HH:MM:SS") VIEW-AS ALERT-BOX INFORMATION.

        ASSIGN  lCanClose       = YES
                C-Win:SENSITIVE = YES
                C-Win:HIDDEN    = NO.
    END.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave C-Win
ON CHOOSE OF btSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    DEFINE VARIABLE cOutputFile     AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE lOK             AS LOGICAL      NO-UNDO.


    SYSTEM-DIALOG GET-FILE cOutputFile
        TITLE      "Save Test Suite Configuration"
        FILTERS    "XML File (*.xml)"   "*.xml"
        SAVE-AS
        ASK-OVERWRITE
        UPDATE lOK.

    IF NOT lOK THEN
        RETURN.

    ASSIGN cOutputFile = RIGHT-TRIM(cOutputFile, ".xml") + ".xml".
    RUN dumpConfiguration (INPUT cOutputFile).
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSaveResult
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSaveResult C-Win
ON CHOOSE OF btSaveResult IN FRAME DEFAULT-FRAME /* Save results */
DO:
    RUN DumpResults.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btUp C-Win
ON CHOOSE OF btUp IN FRAME DEFAULT-FRAME /* Up */
DO:
    DEFINE VARIABLE chSelected   AS COM-HANDLE   NO-UNDO.
    DEFINE VARIABLE cSelectedKey AS CHARACTER    NO-UNDO.


    ASSIGN chSelected = chCtrlFrame:TreeView:SelectedItem.
    IF NOT VALID-HANDLE(chSelected) OR chSelected:KEY = "root" THEN DO:
        MESSAGE "Can't move this item." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    ASSIGN cSelectedKey = chSelected:KEY.

    /* If it's root or there's no node after the current, returns.. */
    IF chSelected:KEY = "root" OR chSelected:Previous  = 0 THEN DO:
        MESSAGE "Can't do this move." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        RETURN "".
    END.

    chCtrlFrame:TreeView:SelectedItem = chSelected:Previous.
    APPLY "CHOOSE":U TO btDown.

    ASSIGN  chSelected = chCtrlFrame:TreeView:Nodes:ITEM(cSelectedKey)
            chCtrlFrame:TreeView:SelectedItem = chSelected.

    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Click
PROCEDURE CtrlFrame.TreeView.Click .
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  None required for OCX.
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSelectedId         AS CHARACTER        NO-UNDO.


    ASSIGN cSelectedId = chCtrlFrame:TreeView:SelectedItem:KEY.

    IF cSelectedId = "root" OR cSelectedId BEGINS "set" THEN
        ASSIGN iSelection = 0.
    ELSE
        ASSIGN iSelection = INTEGER(ENTRY(2, cSelectedId, "_")).

    {&OPEN-QUERY-{&BROWSE-NAME}}
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.KeyUp
PROCEDURE CtrlFrame.TreeView.KeyUp .
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  Required for OCX.
    KeyCode
    Shift
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER p-KeyCode AS INTEGER NO-UNDO.
    DEFINE INPUT        PARAMETER p-Shift   AS INTEGER NO-UNDO.


    RUN CtrlFrame.TreeView.Click.
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FL_URL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FL_URL C-Win
ON LEFT-MOUSE-CLICK OF FL_URL IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE hInstance   AS INTEGER      NO-UNDO.


    ASSIGN FL_URL.
    RUN ShellExecuteA(0, "open", FL_URL, "", "", 1, OUTPUT hInstance).
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames. */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to terminate it. */
ON CLOSE OF THIS-PROCEDURE DO:
    IF NOT lCanClose THEN
        RETURN NO-APPLY.

    ASSIGN C-Win:HIDDEN = TRUE.
    RUN disable_UI.
    COLOR-TABLE:SET-RED-VALUE(6, iPreviousRed).
    COLOR-TABLE:SET-GREEN-VALUE(6, iPreviousGreen).
    COLOR-TABLE:SET-BLUE-VALUE(6, iPreviousBlue).
    COLOR-TABLE:SET-DYNAMIC(6, FALSE).
    RETURN "".
END.


/* Best default for GUI applications is... */
PAUSE 0 BEFORE-HIDE.

/* Saev color table */
ASSIGN  iPreviousRed = COLOR-TABLE:GET-RED-VALUE(6)
        iPreviousGreen = COLOR-TABLE:GET-GREEN-VALUE(6)
        iPreviousBlue = COLOR-TABLE:GET-BLUE-VALUE(6).
COLOR-TABLE:SET-DYNAMIC(6, TRUE).
COLOR-TABLE:SET-RED-VALUE(6, 209).
COLOR-TABLE:SET-GREEN-VALUE(6, 209).
COLOR-TABLE:SET-BLUE-VALUE(6, 203).

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    OUTPUT CLOSE.
    OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "puTemp.txt").

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addNewSet C-Win
PROCEDURE addNewSet :
/*------------------------------------------------------------------------------
  Purpose: Add a new TestSet in the Tree
  Parameters:
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cTestSetName         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER chCurrentNode        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iNewSequence                AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iParent                     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE chItem                      AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cParentId                   AS CHARACTER        NO-UNDO.


    /* Identifies Parent */
    cParentId = chCtrlFrame:TreeView:SelectedItem:Key.
    IF cParentId = "root" THEN
        ASSIGN iParent = 0.
    ELSE IF cParentId BEGINS "test_" THEN
        RETURN "".
    ELSE
        ASSIGN iParent = INTEGER( ENTRY(2, cParentId, "_") ) NO-ERROR.

    /* Identifies new Set Sequence */
    FIND LAST ttTestItemGUI NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttTestItemGUI THEN
        ASSIGN iNewSequence = 1.
    ELSE
        ASSIGN iNewSequence = ttTestItemGUI.id + 1.

    chItem = chCtrlFrame:TreeView:Nodes:Add(cParentId , 4,"set_" + STRING(iNewSequence), cTestSetName, 1,).
    ASSIGN chCtrlFrame:TreeView:SelectedItem = chItem.

    /* Creates Record */
    CREATE ttTestItemGUI.
    ASSIGN  ttTestItemGUI.id           = iNewSequence
            ttTestItemGUI.ItemName     = cTestSetName
            ttTestItemGUI.parentSet    = iParent
            ttTestItemGUI.TreeViewHdl  = chItem
            ttTestItemGUI.TYPE         = {&TEST_SET}.
    RELEASE ttTestItemGUI.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addNewTest C-Win
PROCEDURE addNewTest :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cTestName            AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER chCurrentNode        AS CHARACTER        NO-UNDO.

    DEFINE VARIABLE iNewSequence                AS INTEGER          NO-UNDO.
    DEFINE VARIABLE iParent                     AS INTEGER          NO-UNDO.
    DEFINE VARIABLE chItem                      AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE chNewItem                   AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cParentId                   AS CHARACTER        NO-UNDO.


    IF SEARCH(cTestName) = ? THEN DO:
        MESSAGE "Program " + QUOTER(cTestName) " not found." SKIP
                "Need to adjust your propath?"
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN "".
    END.

    /* Identifies Parent */
    ASSIGN  chItem      = chCtrlFrame:TreeView:SelectedItem
            cParentId   = chItem:Key.

    IF cParentId = "root" THEN
        ASSIGN iParent = 0.
    ELSE IF cParentId BEGINS "test_" THEN
        RETURN "".
    ELSE
        ASSIGN iParent = INTEGER( ENTRY(2, cParentId, "_") ) NO-ERROR.

    /* Identifies new Test Sequence */
    FIND LAST ttTestItemGUI NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttTestItemGUI  THEN
        ASSIGN iNewSequence = 1.
    ELSE
        ASSIGN iNewSequence = ttTestItemGUI.id + 1.

    chNewItem = chCtrlFrame:TreeView:Nodes:Add(cParentId , 4,"test_" + STRING(iNewSequence), cTestName, 2,).
    ASSIGN  chCtrlFrame:TreeView:SelectedItem = chNewItem.      /* Weird.. it's not showing if not selected */
            chCtrlFrame:TreeView:SelectedItem = chItem.         /* Reselect previous selected item */

    /* Creates Record */
    CREATE ttTestItemGUI.
    ASSIGN  ttTestItemGUI.id           = iNewSequence
            ttTestItemGUI.type         = {&TEST_CASE}
            ttTestItemGUI.iTemName     = cTestName
            ttTestItemGUI.parentSet    = iParent
            ttTestItemGUI.TreeViewHdl  = chNewItem.
    RELEASE ttTestItemGUI.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterRunningTest C-Win
PROCEDURE afterRunningTest :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iTestCaseId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cTestName        AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iStatus          AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cMessage         AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert     AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime       AS INTEGER          NO-UNDO.

    DEFINE BUFFER bTests FOR ttTestItemGUI.
    DEFINE BUFFER bTestSet FOR ttTestItemGUI.

    DEFINE VARIABLE chItem                  AS COM-HANDLE   NO-UNDO.
    DEFINE VARIABLE iId                     AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iImageId                AS INTEGER      NO-UNDO.
    DEFINE VARIABLE testRow                 AS ROWID        NO-UNDO.


    /* Registers the result in the temp-table */
    CREATE ttUnitTestGUI.
    ASSIGN  iResultSequence                 = iResultSequence + 1
            ttUnitTestGUI.runningStatusText = " " + getStatusLabel(iStatus)
            ttUnitTestGUI.testItemId        = iTestCaseId
            ttUnitTestGUI.seq               = iResultSequence
            ttUnitTestGUI.name              = cTestName
            ttUnitTestGUI.txtMessage        = cMessage
            ttUnitTestGUI.firstAssertFailed = iFirstAssert
            ttUnitTestGUI.totalTime         = iTotalTime
            testRow                         = ROWID(ttUnitTestGUI)
            ttUnitTestGUI.runningStatus     = iStatus.

    /* Refreshes browser */
    ASSIGN iSelection = iTestCaseId.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    REPOSITION {&BROWSE-NAME} TO ROWID testRow.
    PROCESS EVENTS.

    IF iStatus = {&STATUS_SUCCESS} THEN
        RETURN.

    /* Updates treeView */
    FIND bTests WHERE bTests.id = iTestCaseId NO-LOCK NO-ERROR.

    IF iStatus = {&STATUS_FAIL} THEN
        ASSIGN iImageId = 3. /* Error */
    ELSE
        ASSIGN iImageid = 4. /* Undefined */

    IF bTests.TreeViewHdl:IMAGE <> 3 THEN
        bTests.TreeViewHdl:IMAGE = iImageId.

    /* Goes up in the treeView, changing the icon if needed */
    FIND bTestSet WHERE bTestSet.id = bTests.parentSet NO-LOCK NO-ERROR.
    DO WHILE AVAILABLE bTestSet:
        /* If it's already marked as error, leave */
        IF bTestSet.TreeViewHdl:IMAGE = 3 THEN
            LEAVE.

        /* Changes the image */
        ASSIGN bTestSet.TreeViewHdl:IMAGE = iImageId.

        /* Goes up again */
        FIND bTestSet WHERE bTestSet.id = bTestSet.parentSet NO-LOCK NO-ERROR.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE afterRunningTestCase C-Win
PROCEDURE afterRunningTestCase :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.

    ASSIGN chCFProgressBar:ProgressBar:Value = INTEGER(chCFProgressBar:ProgressBar:Value) + 1.

    PROCESS EVENTS.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beforeRunningTestCase C-Win
PROCEDURE beforeRunningTestCase :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER cItemName    AS CHARACTER        NO-UNDO.

    DEFINE BUFFER bItem FOR ttTestItemGUI.


    FIND bItem WHERE bItem.id = iItemId NO-LOCK NO-ERROR.
    ASSIGN chCtrlFrame:TreeView:SelectedItem = bItem.TreeViewHdl.

    PROCESS EVENTS.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the
               OCXs in the interface.
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "proUnitGUI.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chcfImageList = cfImageList:COM-HANDLE
    UIB_S = chcfImageList:LoadControls( OCXFile, "cfImageList":U)
    cfImageList:NAME = "cfImageList":U
    chcfProgressBar = cfProgressBar:COM-HANDLE
    UIB_S = chcfProgressBar:LoadControls( OCXFile, "cfProgressBar":U)
    cfProgressBar:NAME = "cfProgressBar":U
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "proUnitGUI.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyTemplateFiles C-Win
PROCEDURE copyTemplateFiles :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cTemplate        AS CHARACTER    NO-UNDO.
    DEFINE INPUT PARAMETER cDestFolder      AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE cFileName               AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cFullName               AS CHARACTER    NO-UNDO.


    ASSIGN FILE-INFO:FILE-NAME = "prounit/templates/" + cTemplate.

    INPUT FROM OS-DIR (FILE-INFO:FULL-PATHNAME) NO-ATTR-LIST.
    REPEAT:
        IMPORT cFileName
               cFullName.
        IF cFileName = "." OR cFileName = ".." THEN
            NEXT.

        OS-COPY VALUE(cFullName) VALUE(cDestFolder + cFileName).
    END.
    INPUT CLOSE.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteItem C-Win
PROCEDURE deleteItem :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iItemId      AS INTEGER      NO-UNDO.

    DEFINE VARIABLE cPrefix             AS CHARACTER    NO-UNDO.

    DEFINE BUFFER bItem     FOR ttTestItemGUI.
    DEFINE BUFFER bChild    FOR ttTestItemGUI.

    FIND bItem WHERE bItem.id = iItemId EXCLUSIVE-LOCK NO-ERROR.
    FOR EACH bChild WHERE bChild.parentSet = iItemId NO-LOCK:
        RUN deleteItem(bChild.id).
    END.

    IF bItem.TYPE = {&TEST_SET} THEN
        ASSIGN cPrefix = "set_".
    ELSE
        ASSIGN cPrefix = "test_".

    chCtrlFrame:TreeView:Nodes:Remove(cPrefix + STRING(bItem.id)).
    DELETE bItem.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpConfiguration C-Win
PROCEDURE dumpConfiguration :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cFileName        AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE hXMLDoc                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hRoot                   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hPlugins                AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hPlugin                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE iCounter                AS INTEGER      NO-UNDO.


    RUN resetSequences(INPUT chRoot, INPUT-OUTPUT iCounter).

    CREATE X-DOCUMENT hXMLDoc.
    CREATE X-NODEREF hRoot.

    CREATE X-NODEREF hPlugins.
    CREATE X-NODEREF hPlugin.

    hXMLDoc:CREATE-NODE(hRoot, "ProUnitTestSuite", "ELEMENT").
    hXMLDoc:APPEND-CHILD(hRoot).

    /* Dump plugins */
    hXMLDoc:CREATE-NODE(hPlugins, "plugins", "ELEMENT").
    hRoot:APPEND-CHILD(hPlugins).
    FOR EACH ttPlugins:
        hXMLDoc:CREATE-NODE(hPlugin, "plugin", "ELEMENT").
        hPlugin:SET-ATTRIBUTE("name", ttPlugins.pluginName).
        hPlugin:SET-ATTRIBUTE("active", STRING(ttPlugins.isActive)).
        hPlugins:APPEND-CHILD(hPlugin).
    END.

    /* Dump test cases/sets */
    RUN dumpItems (INPUT hXMLDoc,
                   INPUT 0,
                   INPUT hRoot).

    hXMLDoc:SAVE("file", cFileName).

    DELETE OBJECT hXMLDoc.
    DELETE OBJECT hRoot.
    DELETE OBJECT hPlugins.
    DELETE OBJECT hPlugin.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dumpItems C-Win
PROCEDURE dumpItems :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER hXMLDoc      AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER iParentId    AS INTEGER      NO-UNDO.
    DEFINE INPUT PARAMETER hParentNode  AS HANDLE       NO-UNDO.

    DEFINE BUFFER bTests FOR ttTestItemGUI.
    DEFINE VARIABLE hEntry                  AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hResult                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE cElementName            AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cStatus                 AS CHARACTER    NO-UNDO.


    CREATE X-NODEREF hEntry.

    FOR EACH bTests WHERE bTests.parentSet = iParentId NO-LOCK:

        IF bTests.type = {&TEST_SET} THEN
            ASSIGN cElementName = "TestSet".
        ELSE
            ASSIGN cElementName = "TestCase".

        hXMLDoc:CREATE-NODE(hEntry, cElementName, "ELEMENT").
        hParentNode:APPEND-CHILD(hEntry).
        hEntry:SET-ATTRIBUTE("seq", STRING(bTests.seq)).
        hEntry:SET-ATTRIBUTE("name", bTests.ItemName).

        IF bTests.type = {&TEST_SET} THEN
            RUN dumpItems(INPUT hXMLDoc,
                          INPUT bTests.id,
                          INPUT hEntry).
    END.

    DELETE OBJECT hEntry.
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DumpResults C-Win
PROCEDURE DumpResults :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOutputFile     AS CHARACTER    NO-UNDO
        FORMAT "x(256)"
        VIEW-AS FILL-IN
        SIZE 55 BY 1.0.

    DEFINE VARIABLE cTemplate       AS CHARACTER
        VIEW-AS COMBO-BOX INNER-LINES 15
        LIST-ITEMS ""
        DROP-DOWN-LIST
        SIZE 30 BY 1.0.

    DEFINE VARIABLE lOK             AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE iItemCount      AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cList           AS CHARACTER    NO-UNDO.

    DEFINE BUTTON btBrowse LABEL "..."      SIZE 5  BY 1.2.
    DEFINE BUTTON btOK     LABEL "OK"       SIZE 15 BY 1.2.
    DEFINE BUTTON btCancel LABEL "Cancel"   SIZE 15 BY 1.2.

    DEFINE FRAME f-dump
        "Template:" VIEW-AS TEXT AT ROW 1.5 COL 4
        cTemplate    NO-LABEL FORMAT "x(30)" AT ROW 2.3 COL 4

        "Output File:" VIEW-AS TEXT AT ROW 3.5 COL 4
        cOutputFile  NO-LABEL AT ROW 4.3 COL 4
        btBrowse     AT ROW 4.2 COL 61


        btOK         AT ROW 6.0 COL 10
        btCancel     AT ROW 6.0 COL 50
        WITH THREE-D VIEW-AS DIALOG-BOX SIDE-LABELS SIZE 70 BY 8
             TITLE "Save Execution Log".

    ON "CHOOSE":U OF btBrowse IN FRAME f-dump DO:
        SYSTEM-DIALOG GET-FILE cOutputFile
            TITLE      "Output File"
            FILTERS    "XML File (*.xml)"   "*.xml"
            SAVE-AS
            UPDATE lOK.
        IF lOK THEN DO WITH FRAME f-dump:
            /* Ensure .xml file extension */
            IF cOutputFile MATCHES "*~~.xml" THEN
                ASSIGN  cOutputFile = RIGHT-TRIM(cOutputFile, "xml")
                        cOutputFile = RIGHT-TRIM(cOutputFile, ".").
            ASSIGN  cOutputFile = cOutputFile + ".xml".
            DISPLAY cOutputFile.
        END.
        RETURN "".
    END.

    ON "CHOOSE":U OF btCancel DO:
        HIDE FRAME f-dump.
        RETURN "".
    END.

    ON "CHOOSE":U OF btOK DO:
        ASSIGN  cOutputFile
                cTemplate.

        IF cTemplate = ? OR cTemplate = "" THEN DO:
            MESSAGE "A template is required." VIEW-AS ALERT-BOX WARNING.
            RETURN NO-APPLY.
        END.
        ELSE IF cOutputFile = ? OR cOutputFile = "" THEN DO:
            MESSAGE "A file name is required." VIEW-AS ALERT-BOX WARNING.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            /* Ensure .xml file extension */
            IF cOutputFile MATCHES "*~~.xml" THEN
                ASSIGN  cOutputFile = RIGHT-TRIM(cOutputFile, "xml")
                        cOutputFile = RIGHT-TRIM(cOutputFile, ".").
            ASSIGN  cOutputFile = cOutputFile + ".xml".
                    FILE-INFO:FILE-NAME = cOutputFile.
            DISPLAY cOutputFile WITH FRAME f-dump.

            IF FILE-INFO:FULL-PATHNAME <> ? THEN DO:
                MESSAGE "File" + QUOTER(cOutputFile) + "already exists." SKIP "Overwrite ?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lOK.
                IF NOT lOK THEN
                    RETURN NO-APPLY.
            END.

            RUN saveResults IN hRunner (cOutputFile, cTemplate) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
                MESSAGE "An error occured while saving file." VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            ELSE IF cTemplate > "" THEN
                MESSAGE "Output file has been saved." SKIP(1)
                        "If you are willing to move this file, just remember the template files!"
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ELSE
                MESSAGE "File saved:" cOutputFile VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        END.
        RETURN "".
    END.

    ON "WINDOW-CLOSE":U OF FRAME f-dump DO:
        APPLY "CHOOSE" TO btCancel.
        RETURN "".
    END.


    IF NOT VALID-HANDLE(hRunner) THEN
        MESSAGE   "Internal error: unable to query previous tests run." SKIP
                  "This is most likely due to an unhandled error while running tests."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    ELSE DO:
        /* Is there something to export? */
        ASSIGN iItemCount = 0.
        RUN getItemCountToDump IN hRunner (INPUT 0, OUTPUT iItemCount).
        IF iItemCount = 0 THEN
            MESSAGE "There is nothing to save." VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        ELSE DO:
            /* Initialize drop-down list */
            RUN getResultTemplates IN hRunner (OUTPUT cList).
            ASSIGN cTemplate:LIST-ITEMS = cList.

            /* Show frame */
            VIEW FRAME f-dump.
            ENABLE ALL WITH FRAME f-dump.
            WAIT-FOR "CHOOSE" OF btOK, btCancel IN FRAME f-dump.
            HIDE FRAME f-dump.
        END.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.
  DISPLAY errorMsg FL_URL FL_V9WARNING FL_PROVERSION lblStatus
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brwResult errorMsg btAddSet btAddTC btDown btLoad btPlugin btRemove
         btRename btRun btSave btSaveResult btLeft btRight btUp FL_URL
         FL_V9WARNING FL_PROVERSION lblStatus
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPlugins C-Win
PROCEDURE getPlugins :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttPlugins.


    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iEntry          AS INTEGER      NO-UNDO.
    DEFINE VARIABLE cInitTest       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE cInitialTitle   AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE hAPI            AS HANDLE       NO-UNDO.
    DEFINE VARIABLE guiVersion      AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE engineVersion   AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE dbToolsVersion  AS CHARACTER    NO-UNDO.


    /* Center window and apply back color (used to set RGB color) */
    ASSIGN  C-Win:X = (SESSION:WORK-AREA-WIDTH-PIXELS - C-Win:WIDTH-PIXELS) / 2
            C-Win:Y = (SESSION:WORK-AREA-HEIGHT-PIXELS - C-Win:HEIGHT-PIXELS) / 2
            C-Win:HIDDEN = FALSE.
            FRAME {&FRAME-NAME}:BGCOLOR = 6.

    /* Load icons (developpement location differs from run-time location, so there are two icon load tries) */
    IF NOT C-Win:LOAD-ICON("prounit/prounit.ico":U) THEN
        C-Win:LOAD-ICON("prounit/images/prounit.ico":U).
    IF NOT C-Win:LOAD-SMALL-ICON("prounit/prounit.ico":U) THEN
        C-Win:LOAD-SMALL-ICON("prounit/images/prounit.ico":U).

    /* In case of Progress < v10.x, show a warning telling it wil partially work */
    IF INTEGER(TRIM(SUBSTRING(PROVERSION, 1, 2), ".")) < 10 THEN
        ASSIGN FL_V9WARNING = "Progress v9.x detected. Some cool features will not be usable.".

    /* Load button pictures */
    DO WITH FRAME {&FRAME-NAME}:
        btAddSet:LOAD-IMAGE-UP("prounit/images/bttestset.bmp").
        btAddTC:LOAD-IMAGE-UP("prounit/images/bttest.bmp").
        btRemove:LOAD-IMAGE-UP("prounit/images/btdelete.bmp").
        btRename:LOAD-IMAGE-UP("prounit/images/btrename.bmp").
        btRun:LOAD-IMAGE-UP("prounit/images/btplay.bmp").
        btSave:LOAD-IMAGE-UP("prounit/images/btsave.bmp").
        btLoad:LOAD-IMAGE-UP("prounit/images/btload.bmp").
        btSaveResult:LOAD-IMAGE-UP("prounit/images/btsavreport.bmp").
        btUp:LOAD-IMAGE-UP("prounit/images/btup.bmp").
        btDown:LOAD-IMAGE-UP("prounit/images/btdown.bmp").
        btLeft:LOAD-IMAGE-UP("prounit/images/btleft.bmp").
        btRight:LOAD-IMAGE-UP("prounit/images/btright.bmp").
        btPlugin:LOAD-IMAGE-UP("prounit/images/btplugin.bmp").
    END.

    /* Gather API version for test runner */
    RUN prounit/testRunner.p PERSISTENT SET hAPI NO-ERROR.
    IF NOT VALID-HANDLE(hAPI) OR COMPILER:ERROR THEN DO:
        MESSAGE "** Internal error while loading prounit/testRunner.p" VIEW-AS ALERT-BOX ERROR.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    RUN getVersion IN hAPI NO-ERROR.
    ASSIGN engineVersion = RETURN-VALUE.
    DELETE PROCEDURE hAPI.

    /* Gather API version for database tools */
    RUN prounit/databaseTools.p PERSISTENT SET hAPI NO-ERROR.
    IF NOT VALID-HANDLE(hAPI) OR COMPILER:ERROR THEN DO:
        MESSAGE "** Internal error while loading prounit/databaseTools.p" VIEW-AS ALERT-BOX ERROR.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    RUN getVersion IN hAPI NO-ERROR.
    ASSIGN dbToolsVersion = RETURN-VALUE.
    DELETE PROCEDURE hAPI.

    /* Prepare and display module revisions in window title bar */
    ASSIGN  engineVersion   = REPLACE(engineVersion, "$", "")
            engineVersion   = REPLACE(engineVersion, "Revision: ", "")
            engineVersion   = TRIM(engineVersion)
            dbToolsVersion  = REPLACE(dbToolsVersion, "$", "")
            dbToolsVersion  = REPLACE(dbToolsVersion, "Revision: ", "")
            dbToolsVersion  = TRIM(dbToolsVersion)
            guiVersion      = REPLACE({&THIS-VERSION}, "$", "")
            guiVersion      = REPLACE(guiVersion, "Revision: ", "")
            guiVersion      = TRIM(guiVersion)
            cInitialTitle   = C-Win:TITLE
            C-Win:TITLE     = cInitialTitle + " - Release " + {&THIS-RELEASE} + " (engine rev. " + engineVersion + ", " + "GUI rev. " + guiVersion + ", DB tools rev. " + dbToolsVersion + ")"
            FL_PROVERSION   = "Progress | OpenEdge version: " + STRING(PROVERSION).
    DISPLAY FL_PROVERSION.

    /* Create image list and assign to treeview */
    ASSIGN  chCtrlFrame:TreeView:ImageList = chCFImageList:ImageList
            chRoot = chCtrlFrame:TreeView:Nodes:Add(, ,"root" , cInitialTitle, ,)
            chCtrlFrame:TreeView:SelectedItem = chRoot.

    /* Core initializations */
    RUN initialize-core.

    /* Load configuration files according to parameter variable */
    IF NUM-ENTRIES(cInitTestCases) = 1 AND cInitTestCases MATCHES "*~~.xml" THEN
        RUN loadConfiguration (INPUT cInitTestCases).
    ELSE DO:
        DO iEntry = 1 TO NUM-ENTRIES(cInitTestCases):
            ASSIGN cInitTest = ENTRY(iEntry, cInitTestCases).
            RUN addNewTest (cInitTest, chRoot).
        END.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-core C-Win
PROCEDURE initialize-core :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    RUN prounit/suiteRunner.p PERSISTENT SET hRunner NO-ERROR.
    IF NOT VALID-HANDLE(hRunner) OR COMPILER:ERROR THEN DO:
        MESSAGE "** Internal error while loading prounit/suiteRunner.p" VIEW-AS ALERT-BOX ERROR.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.
    RUN addListener IN hRunner (INPUT THIS-PROCEDURE).

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadConfiguration C-Win
PROCEDURE loadConfiguration :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cFileName        AS CHARACTER    NO-UNDO.

    DEFINE VARIABLE hXMLDoc                 AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hRoot                   AS HANDLE       NO-UNDO.
    DEFINE VARIABLE lOK                     AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE iCount                  AS INTEGER      NO-UNDO.
    DEFINE VARIABLE hPlugins                AS HANDLE       NO-UNDO.
    DEFINE VARIABLE hPlugin                 AS HANDLE       NO-UNDO.


    FOR EACH ttTestItemGUI WHERE ttTestItemGUI.parentSet = 0 NO-LOCK:
        RUN DeleteItem(ttTestItemGUI.id).
    END.
    EMPTY TEMP-TABLE ttTestItemGUI.
    EMPTY TEMP-TABLE ttUnitTestGUI.
    EMPTY TEMP-TABLE ttPlugins.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ASSIGN errorMsg:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

    CREATE X-DOCUMENT hXMLDoc.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hPlugins.
    CREATE X-NODEREF hPlugin.

    IF NOT hXMLDoc:LOAD("file", cFileName, FALSE) THEN DO:
        MESSAGE "Could not load configuration file." VIEW-AS ALERT-BOX ERROR.
        RETURN "".
    END.

    /* TODO: This code is almost the same as suiteRunner:setProjectXML */

    lOK = hXMLDoc:GET-CHILD(hRoot, 1).
    IF NOT lOK THEN DO:
        DELETE OBJECT hRoot.
        DELETE OBJECT hXMLDoc.
        MESSAGE "Could not read configuration file." VIEW-AS ALERT-BOX ERROR.
        RETURN "".
    END.

    IF NOT hRoot:NAME = "ProUnitTestSuite" AND NOT hRoot:NAME = "PUnitTestSet" THEN DO:     /* Compatibility mode */
        DELETE OBJECT hRoot.
        DELETE OBJECT hXMLDoc.
        MESSAGE "Invalid configuration file." VIEW-AS ALERT-BOX ERROR.
        RETURN "".
    END.

    /* Load plugins */
    DO iCount = 1 TO hRoot:NUM-CHILDREN:
        hRoot:GET-CHILD(hPlugins, iCount).
        IF hPlugins:NAME = "plugins" THEN DO:
            DO iCount = 1 TO hPlugins:NUM-CHILDREN:
                hPlugins:GET-CHILD(hPlugin, iCount).

                /* Ensure the plugin exists */
                IF SEARCH("prounit/templates/" + hPlugin:GET-ATTRIBUTE("name") + "/template.p") = ? AND SEARCH("prounit/templates/" + hPlugin:GET-ATTRIBUTE("name") + "/template.r") = ? THEN
                    MESSAGE "A plugin referenced in the configuration was not found." SKIP "The plugin " + QUOTER(hPlugin:GET-ATTRIBUTE("name")) + " has been skipped." VIEW-AS ALERT-BOX WARNING.
                ELSE DO:
                    CREATE ttPlugins.
                    ASSIGN  ttPlugins.pluginSeq  = iCount
                            ttPlugins.pluginName = hPlugin:GET-ATTRIBUTE("name")
                            ttPlugins.isActive   = hPlugin:GET-ATTRIBUTE("active") = "yes".
                    VALIDATE ttPlugins.
                    RELEASE ttPlugins.
                END.
            END.
            LEAVE.
        END.
    END.

    /* Loads Items */
    RUN loadItems (hRoot, chRoot).

    DELETE OBJECT hPlugin.
    DELETE OBJECT hPlugins.
    DELETE OBJECT hXMLDoc.
    DELETE OBJECT hRoot.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadItems C-Win
PROCEDURE loadItems :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER hParentElement       AS HANDLE       NO-UNDO.
    DEFINE INPUT PARAMETER chParentNode         AS COM-HANDLE   NO-UNDO.

    DEFINE VARIABLE hEntry                      AS HANDLE       NO-UNDO.
    DEFINE VARIABLE iCont                       AS INTEGER      NO-UNDO.
    DEFINE VARIABLE lOK                         AS LOGICAL      NO-UNDO.
    DEFINE VARIABLE cName                       AS CHARACTER    NO-UNDO.
    DEFINE VARIABLE chCurrentNode               AS COM-HANDLE   NO-UNDO.


    CREATE X-NODEREF hEntry.

    DO iCont = 1 TO hParentElement:NUM-CHILDREN:
        ASSIGN lOK = hParentElement:GET-CHILD(hEntry, iCont).
        IF NOT lOK THEN
            NEXT.

        ASSIGN cName = hEntry:GET-ATTRIBUTE("name").
        IF cName = "" THEN          /* Tries old version attribute - testcase */
            cName = hEntry:GET-ATTRIBUTE("testcase").

        IF hEntry:NAME = "TestCase" THEN
            RUN addNewTest (cName, chParentNode).
        ELSE IF hEntry:NAME = "TestSet" THEN DO:
            RUN addNewSet  (cName, chParentNode).
            /* Here, the newest node must be already selected */
            ASSIGN chCurrentNode = chCtrlFrame:TreeView:SelectedItem.
            RUN loadItems (hEntry, chCurrentNode).

            /* Reselect previous item to avoid creation errors */
            ASSIGN chCtrlFrame:TreeView:SelectedItem = chParentNode.
        END.
    END.

    DELETE OBJECT hEntry.
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redrawItems C-Win
PROCEDURE redrawItems :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER cParentKey      AS CHARACTER NO-UNDO.

    DEFINE BUFFER bItem     FOR ttTestItemGUI.

    DEFINE VARIABLE iParentId   AS INTEGER              NO-UNDO.
    DEFINE VARIABLE cPrefix     AS CHARACTER            NO-UNDO.
    DEFINE VARIABLE iImage      AS INTEGER              NO-UNDO.
    DEFINE VARIABLE chNewItem   AS COM-HANDLE           NO-UNDO.


    ASSIGN iParentId = (IF cParentKey = "root" THEN 0 ELSE INTEGER(ENTRY(2, cParentKey, "_"))).

    FOR EACH bItem WHERE bItem.parentSet = iParentId EXCLUSIVE-LOCK BY bItem.seq:
        IF bItem.type = {&TEST_SET} THEN
            ASSIGN  iImage  = 1
                    cPrefix = "set_".
        ELSE
            ASSIGN  iImage  = 2
                    cPrefix = "test_".

        chNewItem = chCtrlFrame:TreeView:Nodes:Add(cParentKey,
                                                   4,
                                                   cPrefix + STRING(bItem.id),
                                                   bItem.ItemName,
                                                   iImage, ).
        ASSIGN bItem.TreeViewHdl = chNewItem.
        IF bItem.type = {&TEST_SET} THEN
            RUN redrawItems(cPrefix + STRING(bItem.id)).
    END.
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redrawTreeView C-Win
PROCEDURE redrawTreeView :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE chItem      AS COM-HANDLE       NO-UNDO.


    /* Remove all root's children */
    DO WHILE chRoot:Children > 0:
        ASSIGN chItem = chRoot:child.
        chCtrlFrame:TreeView:Nodes:Remove(chItem:KEY).
    END.

    RUN redrawItems("root").
    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetIcons C-Win
PROCEDURE resetIcons :
/*------------------------------------------------------------------------------
  Purpose: resets Icons on the treeView
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iImageId        AS INTEGER          NO-UNDO.
    DEFINE BUFFER bTestItems        FOR ttTestItemGUI.

    FOR EACH bTestItems NO-LOCK:
        ASSIGN bTestItems.TreeViewHdl:IMAGE = bTestItems.type.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetSequences C-Win
PROCEDURE resetSequences :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER hNode        AS COM-HANDLE       NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iSequence    AS INTEGER          NO-UNDO.

    DEFINE VARIABLE iCounter            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE chItem              AS COM-HANDLE       NO-UNDO.
    DEFINE VARIABLE cItemKey            AS CHARACTER        NO-UNDO.

    DEFINE BUFFER bTest     FOR ttTestItemGUI.


    ASSIGN chItem = hNode:Child.
    DO WHILE VALID-HANDLE(chItem):
        ASSIGN  cItemKey = chItem:KEY
                iSequence = iSequence + 1.

        FIND bTest WHERE bTest.id = INTEGER(ENTRY(2, cItemKey, "_")) EXCLUSIVE-LOCK NO-ERROR.

        ASSIGN bTest.seq = iSequence.

        IF cItemKey BEGINS "set_" THEN
            RUN resetSequences (INPUT chItem, INPUT-OUTPUT iSequence).
        ASSIGN chItem = chItem:NEXT.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPlugins C-Win
PROCEDURE setPlugins :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER TABLE FOR ttPlugins.


    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateStatus C-Win
PROCEDURE updateStatus :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iStatus      AS INTEGER  NO-UNDO.


    DO WITH FRAME {&FRAME-NAME} :
        ASSIGN lblStatus:SCREEN-VALUE = " " + getStatusLabel(istatus).
        CASE iStatus:
            WHEN {&STATUS_SUCCESS} THEN
                ASSIGN  lblStatus:BGCOLOR = 10   /* Green */
                        lblStatus:FGCOLOR = 0.
            WHEN {&STATUS_WARNING} THEN
                ASSIGN  lblStatus:BGCOLOR = 14   /* Yellow */
                        lblStatus:FGCOLOR = 0.
            WHEN {&STATUS_FAIL} THEN
                ASSIGN  lblStatus:BGCOLOR = 12   /* Red */
                        lblStatus:FGCOLOR = 15.
        END.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

