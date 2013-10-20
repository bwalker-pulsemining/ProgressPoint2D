/*------------------------------------------------------------------------------
Some code to initialize the environment or database before running the test.
------------------------------------------------------------------------------*/
PROCEDURE initialize:
END.


/*------------------------------------------------------------------------------
Some code run before every test to reset internal states, if needed.
------------------------------------------------------------------------------*/
PROCEDURE setUp:
END.


/*------------------------------------------------------------------------------
Some code run after a test to restore, log or something else.
------------------------------------------------------------------------------*/
PROCEDURE tearDown:
END.


/*------------------------------------------------------------------------------
Dispose everything, free resource, close files, disconnect databases, etc.
------------------------------------------------------------------------------*/
PROCEDURE dispose:
END.


/*------------------------------------------------------------------------------
One of your tests
------------------------------------------------------------------------------*/
PROCEDURE testSomething:
END.


/*------------------------------------------------------------------------------
Full database test
------------------------------------------------------------------------------*/
PROCEDURE testDatabase:
    /* Connect to database, inject initial data, then disconnect from database */
    RUN LocalConnectAppServerDB(INPUT hAppServer, "myDBLogicalName") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    RUN LoadDFileToDBTable(INPUT "c:\initial.d", INPUT "MY_TABLE", INPUT NO, INPUT YES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    RUN LocalDisconnectAppServerDB NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.



    /* Run our program to be tested. This program may change MY_TABLE records. */
    RUN my_test.p ON hAppServer (INPUT ....) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.



    /* Connect to database, gather changed data, then disconnect from database */
    RUN LocalConnectAppServerDB(INPUT hAppServer, "myDBLogicalName") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    /* Create temp-tables like physical table */
    RUN CreateTTLikePhysicalDBTable(INPUT "MY_TABLE", INPUT "a_name_for_your_tt", OUTPUT hTTExpected) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.
    RUN CreateTTLikePhysicalDBTable(INPUT "MY_TABLE", INPUT "another_name_for_your_tt", OUTPUT hTTGot) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    RUN ExportDBTableToTempTableViaQuery(INPUT "MY_TABLE", INPUT hTTGot, INPUT YES, INPUT NO, INPUT "where somefield = 12") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    RUN LoadDFileToTempTable(INPUT "c:\expected.d", INPUT hTTExpected, INPUT YES, INPUT NO) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.

    RUN assertEqualsTT(hTTExpected, hTTGot).

    RUN DeleteTTByHandle(INPUT hTTExpected).
    RUN DeleteTTByHandle(INPUT hTTGot).

    RUN LocalDisconnectAppServerDB NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        MESSAGE RETURN-VALUE SKIP ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX WARNING.
END.
