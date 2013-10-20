DEFINE VARIABLE hBufferLeak                         AS HANDLE   NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hSSocketLeak      AS HANDLE   NO-UNDO.

PROCEDURE initialize:
    IF VALID-HANDLE(hSSocketLeak) THEN DO:
        hSSocketLeak:DISABLE-CONNECTIONS() NO-ERROR.
        DELETE OBJECT hSSocketLeak.
    END.
END PROCEDURE.

PROCEDURE testLeaveProcedure:
    RUN samples/Plugins/empty.p PERSISTENT.
    RUN samples/Plugins/empty.p PERSISTENT.
END PROCEDURE.

PROCEDURE testLeaveBuffer:
    CREATE BUFFER hBufferLeak FOR TABLE "customer".
END PROCEDURE.

PROCEDURE testLeaveQuery:
    DEFINE VARIABLE hQuery      AS HANDLE   NO-UNDO.

    CREATE QUERY hQuery.
    hQuery:ADD-BUFFER(hBufferLeak).
    hQuery:QUERY-PREPARE("for each customer").
END PROCEDURE.

PROCEDURE testLeaveServerSocket:
    CREATE SERVER-SOCKET hSSocketLeak.
    hSSocketLeak:ENABLE-CONNECTIONS("-S 12345").

END PROCEDURE.

PROCEDURE testLeaveSocket:
    DEFINE VARIABLE hSocket     AS HANDLE   NO-UNDO.

    CREATE SOCKET hSocket.
    hSocket:CONNECT("-H localhost -S 12345").

END PROCEDURE.
