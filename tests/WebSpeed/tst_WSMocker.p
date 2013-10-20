/****************************************************
** Program: tst_WSMocker.p
** Tests a web program using Webspeed Mocker.
****************************************************/

DEFINE STREAM tempStream.
&scoped-define LITERAL_1 "This is my first statement"
&scoped-define LITERAL_2 "Something else here"
&scoped-define LITERAL_3 "Camels can't consume crisp cookies"

PROCEDURE testWebExecution:
    DEFINE VARIABLE cCookieValue        AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE cFile               AS CHARACTER         NO-UNDO.
    
    DEFINE VARIABLE ptrFileData         AS MEMPTR            NO-UNDO.
    DEFINE VARIABLE cFileContent        AS CHARACTER         NO-UNDO.
    
    RUN WSMocker.setParameter ("parameter1", {&LITERAL_1}).
    RUN WSMocker.setParameter ("parameter2", {&LITERAL_2}).
    RUN WSMocker.setCookie    ("cookie1"   , {&LITERAL_3}).
    
    RUN assertEqualsChar({&LITERAL_1}, DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "parameter1")).
    RUN assertEqualsChar({&LITERAL_2}, DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "parameter2")).
    RUN assertEqualsChar({&LITERAL_3}, DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "cookie1")).

    RUN WSMocker.invokeWS ("samples/WebSpeed/WSProgram.p", "GET").
    
    RUN WSMocker.getCookie("resultingCookie", OUTPUT cCookieValue).
    RUN assertEqualsChar(cCookieValue, "GET," + 
                                        STRING(LENGTH({&LITERAL_1})) + "," +
                                        STRING(LENGTH({&LITERAL_2})) + "," + 
                                        STRING(LENGTH({&LITERAL_3}))).
    
    RUN WSMocker.getWebStreamFile (OUTPUT cFile).
    RUN WSMocker.closeWebStream.
    
    FILE-INFO:FILE-NAME = cFile.
    SET-SIZE(ptrFileData) = FILE-INFO:FILE-SIZE + 1.
    
    INPUT  STREAM tempStream FROM VALUE(cFile) BINARY.
    IMPORT STREAM tempStream ptrFileData.
    INPUT  STREAM tempStream CLOSE.
    
    cFileContent = GET-STRING(ptrFileData, 1).
    SET-SIZE(ptrFileData) = 0.
    
    RUN assertEqualsChar(cFileContent, "GET," + 
                                        STRING(LENGTH({&LITERAL_1})) + "," +
                                        STRING(LENGTH({&LITERAL_2})) + "," + 
                                        STRING(LENGTH({&LITERAL_3}))).
    
END.

/*** Cookie must be set, parameters must return "" */
PROCEDURE testParamPersistence:
    RUN assertEqualsChar(DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "parameter1"), "").
    RUN assertEqualsChar(DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "parameter2"), "").
    RUN assertEqualsChar(DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "cookie1"), {&LITERAL_3}).
    
    /* Now deletes a cookie */
    RUN WSMocker.deleteCookie (INPUT "cookie1").
    RUN assertEqualsChar(DYNAMIC-FUNCTION ("get-value" in THIS-PROCEDURE, "cookie1"), "").
END.
