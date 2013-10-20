&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win
/*******************************************************************************
**
**  File: pluginsEdit.w
**  Description: Plugins selection for ProUnit
**  Author: Flavio Eduardo de Córdova
**  Created: 12/30/2004
**
********************************************************************************
**
**  Revision 1.2  2013/05/21  SMarmotte
**  - enhanced plugins detection so that plugins can be embedded to PL library
**  - improved window load stage
**  - changed tab order for widgets
**  - added default selection in both lists
**  - improved GUI: "<" and ">" buttons are only enabled if a movement can be performed
**
**  Revision 1.1  2013/05/15  SMarmotte
**  - fixed plugin error (duplicated records error - 132) when enabling plugins
**
*******************************************************************************/

/*----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* -------------------------------------------------------------------------- */
/* TEMP TABLES                                                                */
/* -------------------------------------------------------------------------- */
{prounit/plugins.i}

DEFINE TEMP-TABLE ttWorkingTT LIKE ttPlugins.





/* -------------------------------------------------------------------------- */
/* VARIALBES                                                                  */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE hCaller     AS HANDLE       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS availableList btActivate btDeactivate ~
activeList btOK RECT-4
&Scoped-Define DISPLAYED-OBJECTS availableList activeList

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btActivate
     LABEL ">"
     SIZE 6 BY 1.14.

DEFINE BUTTON btDeactivate
     LABEL "<"
     SIZE 6 BY 1.14.

DEFINE BUTTON btOK
     LABEL "OK"
     SIZE 15 BY 1.14.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL
     SIZE 84.4 BY 8.81.

DEFINE VARIABLE activeList AS CHARACTER
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT SCROLLBAR-VERTICAL
     SIZE 35 BY 7.14 NO-UNDO.

DEFINE VARIABLE availableList AS CHARACTER
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SORT SCROLLBAR-VERTICAL
     SIZE 35 BY 7.14 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     availableList AT ROW 2.43 COL 4 NO-LABEL
     btActivate AT ROW 4.57 COL 40.8
     btDeactivate AT ROW 6 COL 40.8
     activeList AT ROW 2.43 COL 49 NO-LABEL
     btOK AT ROW 10.38 COL 37
     "Available Plugins" VIEW-AS TEXT
          SIZE 17 BY .71 AT ROW 1.48 COL 4
     "Active Plugins" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1.48 COL 49
     RECT-4 AT ROW 1.24 COL 1.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1
         SIZE 86.4 BY 10.91
         DEFAULT-BUTTON btOK.


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
         TITLE              = "Edit Plugins"
         HEIGHT             = 10.91
         WIDTH              = 86.4
         MAX-HEIGHT         = 10.91
         MAX-WIDTH          = 87.8
         VIRTUAL-HEIGHT     = 10.91
         VIRTUAL-WIDTH      = 87.8
         MIN-BUTTON         = no
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
   FRAME-NAME Custom                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Edit Plugins */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Edit Plugins */
DO:
    /* This event will close the window and terminate the procedure.  */
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btActivate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btActivate C-Win
ON CHOOSE OF btActivate IN FRAME DEFAULT-FRAME /* > */
DO:
    IF availableList:SCREEN-VALUE <> ? THEN DO:
        activeList:ADD-LAST(availableList:SCREEN-VALUE).
        availableList:DELETE(availableList:SCREEN-VALUE).
    END.
    RUN defaultSelection.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btDeactivate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btDeactivate C-Win
ON CHOOSE OF btDeactivate IN FRAME DEFAULT-FRAME /* < */
DO:
    IF activeList:SCREEN-VALUE <> ? THEN DO:
        availableList:ADD-LAST(activeList:SCREEN-VALUE).
        activeList:DELETE(activeList:SCREEN-VALUE).
    END.
    RUN defaultSelection.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btOK C-Win
ON CHOOSE OF btOK IN FRAME DEFAULT-FRAME /* OK */
DO:
    DEFINE VARIABLE iCounter        AS INTEGER      NO-UNDO.


    IF VALID-HANDLE(hCaller) THEN DO:
        EMPTY TEMP-TABLE ttPlugins.
        DO iCounter = 1 TO activeList:NUM-ITEMS:
            CREATE ttPlugins.
            ASSIGN  ttPlugins.pluginSeq  = iCounter
                    ttPlugins.pluginName = activeList:ENTRY(iCounter)
                    ttPlugins.isActive   = YES.
            RELEASE ttPlugins.
        END.
        RUN setPlugins IN hCaller (INPUT TABLE ttPlugins) NO-ERROR.
    END.

    APPLY "CLOSE" TO THIS-PROCEDURE.
    RETURN "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME}
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Center window */
ASSIGN  C-Win:X = (SESSION:WORK-AREA-WIDTH-PIXELS - C-Win:WIDTH-PIXELS) / 2
        C-Win:Y = (SESSION:WORK-AREA-HEIGHT-PIXELS - C-Win:HEIGHT-PIXELS) / 2
        C-Win:HIDDEN = FALSE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    RUN loadAvailablePlugins.
    RUN defaultSelection.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE defaultSelection C-Win
PROCEDURE defaultSelection :
/*------------------------------------------------------------------------------
  Purpose:     Select first item if none is selected in both list
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cList           AS CHARACTER    NO-UNDO.


    DO  WITH FRAME {&FRAME-NAME}:
        IF availableList:SCREEN-VALUE = ? THEN DO:
            ASSIGN cList = availableList:LIST-ITEMS.
            IF cList > "" THEN
                ASSIGN availableList:SCREEN-VALUE = ENTRY(1, cList).
        END.

        IF activeList:SCREEN-VALUE = ? THEN DO:
            ASSIGN cList = activeList:LIST-ITEMS.
            IF cList > "" THEN
                ASSIGN activeList:SCREEN-VALUE = ENTRY(1, cList).
        END.

        ASSIGN  btActivate:SENSITIVE = (availableList:NUM-ITEMS > 0)
                btDeactivate:SENSITIVE = (activeList:NUM-ITEMS > 0).
    END.

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
  DISPLAY availableList activeList
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE availableList btActivate btDeactivate activeList btOK RECT-4
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadAvailablePlugins C-Win
PROCEDURE loadAvailablePlugins :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE i                       AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cPath                   AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE iCounter                AS INTEGER          NO-UNDO.
    DEFINE VARIABLE cPlugin                 AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cFullName               AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cAttributes             AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cBuildinPlugins         AS CHARACTER        NO-UNDO     INITIAL "_builtin-DataPool,_builtin-DBStat,_builtin-MemMonitor,_builtin-WSMocker".


    /* Special case where plugins are embedded in PL */
    IF INDEX(SEARCH("prounit/pluginsEdit.r"), ".pl<<") > 0 THEN DO:
        DO i = 1 TO NUM-ENTRIES(cBuildinPlugins) :
            IF SEARCH("prounit/plugins/" + ENTRY(i, cBuildinPlugins) + "/plugin.r") > "" THEN DO:
                CREATE ttPlugins.
                ASSIGN ttPlugins.pluginName = ENTRY(i, cBuildinPlugins).
                RELEASE ttPlugins.
            END.
        END.
    END.

    /* Read available plugins in the propath */
    DO iCounter = 1 TO NUM-ENTRIES(PROPATH):
        ASSIGN FILE-INFO:FILE-NAME = ENTRY(iCounter, PROPATH) + "/prounit/plugins".
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            NEXT.

        INPUT FROM OS-DIR(FILE-INFO:FULL-PATHNAME).
        REPEAT:
            IMPORT cPlugin cFullName cAttributes.
            IF cPlugin = "."  OR cPlugin = ".." THEN
                NEXT.

            ASSIGN FILE-INFO:FILE-NAME = cFullName + "/plugin.p".
            IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                ASSIGN FILE-INFO:FILE-NAME = cFullName + "/plugin.r".
                IF FILE-INFO:FULL-PATHNAME = ? THEN
                    NEXT.
            END.

            IF CAN-FIND(ttPlugins WHERE ttPlugins.pluginName = cPlugin) THEN
                NEXT.

            CREATE ttPlugins.
            ASSIGN ttPlugins.pluginName = cPlugin.
            RELEASE ttPlugins.
        END.
        INPUT CLOSE.
    END.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCaller C-Win
PROCEDURE setCaller :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER hC       AS HANDLE       NO-UNDO.


    IF NOT VALID-HANDLE(hC) THEN
        RETURN.

    ASSIGN hCaller = hC.
    EMPTY TEMP-TABLE ttWorkingTT.

    RUN getPlugins IN hCaller (OUTPUT TABLE ttWorkingTT) NO-ERROR.

    FOR EACH ttPlugins WITH FRAME {&FRAME-NAME}:
        ttPlugins.isActive = CAN-FIND(  FIRST ttWorkingTT
                                        WHERE ttWorkingTT.pluginName = ttPlugins.pluginName
                                        AND ttWorkingTT.isActive   = TRUE).

        IF ttPlugins.isActive THEN
            activeList:ADD-LAST(ttPlugins.pluginName).
        ELSE
            availableList:ADD-LAST(ttPlugins.pluginName).
    END.

    RUN defaultSelection.

    RETURN "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

