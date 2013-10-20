/**********************************************************
** WSMocker
** Plugin used to mock WebSpeed Environment so WS programs
** may be tested using ProUnit.
**
** This plugin saves data in an XML document with the following
** structure:
** <data>
**   <param name="abc" iscookie="true">value 1</param>
**   <param name="xyz" iscookie="false">value 2</param>
** </data>
*************************************************************/

DEFINE NEW GLOBAL SHARED VARIABLE web-utilities-hdl    AS HANDLE    NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE REQUEST_METHOD       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED STREAM WebStream.

DEFINE VARIABLE cCurrentContentType                    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutStreamFile                         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsStreamOpen                          AS LOGICAL   NO-UNDO.

DEFINE VARIABLE hDocument                              AS HANDLE    NO-UNDO.

web-utilities-hdl = THIS-PROCEDURE.

PROCEDURE debug_getXMLAsText:
    DEFINE OUTPUT PARAMETER cXML    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE ptr    as MEMPTR NO-UNDO.
    
    hDocument:SAVE("memptr", ptr).
    cXML = GET-STRING(ptr, 1).
    SET-SIZE(ptr) = 0.
END.

/*--------------------------------------------------------------------
   Exposed Procedures
--------------------------------------------------------------------*/
PROCEDURE WSMocker.setParameter:
    DEFINE INPUT PARAMETER cParamName        AS CHARACTER           NO-UNDO.
    DEFINE INPUT PARAMETER cParamValue       AS CHARACTER           NO-UNDO.
    
    DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hNewParameter            AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hText                    AS HANDLE              NO-UNDO.
    
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hNewParameter.
    CREATE X-NODEREF hText.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
    hDocument:CREATE-NODE(hNewParameter, "param", "ELEMENT").
    hNewParameter:SET-ATTRIBUTE("name", cParamName).
    hNewParameter:SET-ATTRIBUTE("iscookie", "false").
    hParent:APPEND-CHILD(hNewParameter).
        
    hDocument:CREATE-NODE(hText, "", "TEXT").
    hText:NODE-VALUE = cParamValue.
    hNewParameter:APPEND-CHILD(hText).
        
    DELETE OBJECT hParent.
    DELETE OBJECT hNewParameter.
    DELETE OBJECT hText.
END.

PROCEDURE WSMocker.setCookie:
    DEFINE INPUT PARAMETER cCookieName       AS CHARACTER            NO-UNDO.
    DEFINE INPUT PARAMETER cCookieValue      AS CHARACTER            NO-UNDO.
        DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hNewParameter            AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hText                    AS HANDLE              NO-UNDO.
    
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hNewParameter.
    CREATE X-NODEREF hText.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
    hDocument:CREATE-NODE(hNewParameter, "param", "ELEMENT").
    hNewParameter:SET-ATTRIBUTE("name", cCookieName).
    hNewParameter:SET-ATTRIBUTE("iscookie", "true").
    hParent:APPEND-CHILD(hNewParameter).
        
    hDocument:CREATE-NODE(hText, "", "TEXT").
    hText:NODE-VALUE = cCookieValue.
    hNewParameter:APPEND-CHILD(hText).
        
    DELETE OBJECT hParent.
    DELETE OBJECT hNewParameter.
    DELETE OBJECT hText.
END.

PROCEDURE WSMocker.getCookie:
    DEFINE INPUT PARAMETER cCookieName        AS CHARACTER           NO-UNDO.
    DEFINE OUTPUT PARAMETER cCookieValue      AS CHARACTER           NO-UNDO.

    DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hParameter               AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hText                    AS HANDLE              NO-UNDO.
    DEFINE VARIABLE iCounter                 AS INTEGER             NO-UNDO.
    
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hParameter.
    CREATE X-NODEREF hText.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
        
    looking:
    DO iCounter = 1 TO hParent:NUM-CHILDREN:
        hParent:GET-CHILD(hParameter, iCounter).
            
        IF hParameter:GET-ATTRIBUTE("name")     = cCookieName AND
           hParameter:GET-ATTRIBUTE("iscookie") = "true"      THEN DO:
            hParameter:GET-CHILD(hText, 1).
            cCookieValue = hText:NODE-VALUE.
    
            LEAVE looking.
        END.
    END.
        
    DELETE OBJECT hParent.
    DELETE OBJECT hParameter.
    DELETE OBJECT hText.
END.

PROCEDURE WSMocker.deleteCookie:
    DEFINE INPUT PARAMETER cCookieName        AS CHARACTER           NO-UNDO.
    
    DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hParameter               AS HANDLE              NO-UNDO.
    DEFINE VARIABLE iCounter                 AS INTEGER             NO-UNDO.
    
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hParameter.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
        
    looking:
    DO iCounter = 1 TO hParent:NUM-CHILDREN:
        hParent:GET-CHILD(hParameter, iCounter).
            
        IF hParameter:GET-ATTRIBUTE("name")     = cCookieName AND
           hParameter:GET-ATTRIBUTE("iscookie") = "true"      THEN DO:
            hParent:REMOVE-CHILD(hParameter).
            LEAVE looking.
        END.
    END.
        
    DELETE OBJECT hParent.
    DELETE OBJECT hParameter.
END.

PROCEDURE WSMocker.invokeWS:
    DEFINE INPUT PARAMETER cProgram        as CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cRequestMethod  AS CHARACTER        NO-UNDO.
    
    ASSIGN
        REQUEST_METHOD      = cRequestMethod
        cCurrentContentType = ?.
        
    RUN VALUE(cProgram).
    
END.

PROCEDURE WSMocker.getWebStreamFile:
    DEFINE OUTPUT PARAMETER cFile        AS CHARACTER        NO-UNDO.
    cFile = cOutStreamFile.
END.

PROCEDURE WSMocker.closeWebStream:
    OUTPUT STREAM WebStream CLOSE.
    lIsStreamOpen = FALSE.
END.

/*-----------------------------------------------------------------
  Overriden Functions
-----------------------------------------------------------------*/
FUNCTION get-value RETURNS CHARACTER
    (INPUT cParamName    as CHARACTER):

    DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hParameter               AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hText                    AS HANDLE              NO-UNDO.
    DEFINE VARIABLE iCounter                 AS INTEGER             NO-UNDO.
    DEFINE VARIABLE cReturningValue          AS CHARACTER           NO-UNDO.
    
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hParameter.
    CREATE X-NODEREF hText.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
        
    looking:
    DO iCounter = 1 TO hParent:NUM-CHILDREN:
        hParent:GET-CHILD(hParameter, iCounter).
            
        IF hParameter:GET-ATTRIBUTE("name") = cParamName THEN DO:
            hParameter:GET-CHILD(hText, 1).
            cReturningValue = hText:NODE-VALUE.
    
            LEAVE looking.
        END.
    END.
        
    DELETE OBJECT hParent.
    DELETE OBJECT hParameter.
    DELETE OBJECT hText.
    
    RETURN cReturningValue.
        
END.

FUNCTION set-cookie RETURNS LOGICAL 
    (INPUT cName     AS CHARACTER,
     INPUT cValue    AS CHARACTER,
     INPUT dtExpDate AS DATE,
     INPUT iExpTime  AS INTEGER,
     INPUT cPath     AS CHARACTER,
     INPUT cDomain   AS CHARACTER,
     INPUT lSecure   AS LOGICAL):

     RUN WSMocker.setCookie (INPUT cName,
                             INPUT cValue).
END.

PROCEDURE outputContentType:
    DEFINE INPUT PARAMETER cContentType        AS CHARACTER    NO-UNDO.
    cCurrentContentType = cContentType.
END.

FUNCTION get-cookie RETURNS CHARACTER
    (INPUT cCookieName        as CHARACTER):
    RETURN get-value (cCookieName).
END.

FUNCTION delete-cookie RETURNS LOGICAL
    (INPUT cCookieName        AS CHARACTER,
     INPUT cPath              AS CHARACTER,
     INPUT cDomain            AS CHARACTER):
    RUN WSMocker.deleteCookie(cCookieName).
END.

/*-----------------------------------------------------------------
  ProUnit Events
-----------------------------------------------------------------*/
PROCEDURE beforeRunningTestCase:
    DEFINE INPUT PARAMETER iItemId    AS INTEGER        NO-UNDO.
    DEFINE INPUT PARAMETER cItemName  AS CHARACTER 	    NO-UNDO. 
    
    DEFINE VARIABLE hRoot             AS HANDLE         NO-UNDO.
    
    CREATE X-DOCUMENT hDocument.
    CREATE X-NODEREF  hRoot.
    
    hDocument:CREATE-NODE(hRoot, "data", "ELEMENT").
    hDocument:APPEND-CHILD(hRoot).
    DELETE OBJECT hRoot.
END.

PROCEDURE beforeRunningTest:
    DEFINE INPUT PARAMETER iTestCaseId        AS INTEGER    NO-UNDO.
    DEFINE INPUT PARAMETER cTestName          AS CHARACTER  NO-UNDO.

    ASSIGN 
        cCurrentContentType = ""
        cOutStreamFile      = SESSION:TEMP-DIRECTORY + "WSMocker_" + cTestName + "_" + STRING(etime) + "_" + STRING(RANDOM(1, 1000)).

    OUTPUT STREAM WebStream TO VALUE(cOutStreamFile).
    lIsStreamOpen = TRUE.
END.

PROCEDURE afterRunningTest:
    DEFINE INPUT PARAMETER iCurrentItem    AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER cTestName       AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER iStatus         AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER cMessage        AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER iFirstAssert    AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER iTotalTime      AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE hParent                  AS HANDLE              NO-UNDO.
    DEFINE VARIABLE hParameter               AS HANDLE              NO-UNDO.
    DEFINE VARIABLE iCounter                 AS INTEGER             NO-UNDO.
    
    /* Deletes parameters not set as cookies */
    CREATE X-NODEREF hParent.
    CREATE X-NODEREF hParameter.
    
    hDocument:GET-DOCUMENT-ELEMENT(hParent).
        
    looking:
    DO iCounter = 1 TO hParent:NUM-CHILDREN:
        hParent:GET-CHILD(hParameter, iCounter).
            
        IF hParameter:GET-ATTRIBUTE("iscookie") = "false" THEN DO:
            hParent:REMOVE-CHILD(hParameter).
            iCounter = iCounter - 1.
        END.
    END.
        
    DELETE OBJECT hParent.
    DELETE OBJECT hParameter.
    
    IF lIsStreamOpen THEN 
        RUN WSMocker.closeWebStream.
    OS-DELETE VALUE(cOutStreamFile).
    
    IF cCurrentContentType = ? THEN
        RUN Plugin.fail("WSMocker", "No content type defined on " + cTestName).
END.

PROCEDURE afterRunningTestCase:
    DEFINE INPUT PARAMETER iItemId      AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iTimeSpent   AS INTEGER          NO-UNDO.
    DEFINE INPUT PARAMETER iStatus      AS INTEGER          NO-UNDO.
    
    DELETE OBJECT hDocument.
END.