/************************************
** WSProgram.p
** Simple program that uses WebSpeed functions just to test WSMocker.
** It receives two parameters (parameter1 and parameter2) and a cookie (cookie1).
** then, it saves another cookie called resultingCookie with the request method
** (GET or POST) and a list of input parameters and cookie's lengths. 
** It will also show the result in the webstream.
**
** For example:
**    parameter1=Hello ProUnit Users
**    parameter2=I am glad do be here
**    cookie=That is a cookie
** After processing the resultingCookie will have this value: 19,20,16 
***********************************/

{src/web/method/wrap-cgi.i}

RUN process-web-request.

PROCEDURE output-header :
    RUN outputContentType IN web-utilities-hdl ("text/html":U).
END PROCEDURE.

PROCEDURE process-web-request:
    DEFINE VARIABLE cParameter1        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cParameter2        AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cCookieValue       as CHARACTER        NO-UNDO.
    
    DEFINE VARIABLE cResult            AS CHARACTER        NO-UNDO.
    
    ASSIGN        cParameter1  = get-value("parameter1")
        cParameter2  = get-value("parameter2")
        cCookieValue = get-cookie("cookie1")
        cResult      = REQUEST_METHOD              + "," +
                       STRING(LENGTH(cParameter1)) + "," +
                       STRING(LENGTH(cParameter2)) + "," +
                       STRING(LENGTH(cCookieValue)).
                       
    set-cookie("resultingCookie", 
                cResult, ?, ?, ?, ?, ?).
                       
    RUN output-header.
    {&OUT} 
        cResult.
END.