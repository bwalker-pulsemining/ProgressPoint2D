/******************************************************************************
**      Program: tst_dbmon.p
**  Description: Do some database manipulation to be caught by the 
**               DBMon plugin.
** Requirements: You must have a sports database connected to run this test.
**        Notes: Of course database manipulation should not be done here since 
**               the test should be using another component for it. However, 
**               as we just want to capture database activity there's no need 
**               to create another program.
******************************************************************************/

   
/* This test will count 3 records of customer - where city = boston and
   5 recors of order.
   However, since the field used in the customer query uses a field that is
   not present in any index, all the records will be read and not only the
   3 records counted. */
PROCEDURE testReadRecords:
    DEFINE VARIABLE iCountCustomers     AS INTEGER      NO-UNDO.
    DEFINE VARIABLE iCountOrders        AS INTEGER      NO-UNDO.

    FOR EACH customer
        WHERE customer.city = "boston"
        NO-LOCK:
        iCountCustomers = iCountCustomers + 1.
        FOR EACH order
            WHERE order.cust-num = customer.cust-num
            NO-LOCK:
            iCountOrders = iCountOrders + 1.
        END.
    END.

    RUN assertEqualsInt(3, iCountCustomers).
    RUN assertEqualsInt(5, iCountOrders).
    
END.
              
/* Creates recors of State for each brazilian state */
PROCEDURE testCreateRecords:

    RUN createState ("AC (BR)", "Acre"               , "Brasil").
    RUN createState ("AL (BR)", "Alagoas"            , "Brasil").
    RUN createState ("AM (BR)", "Amazonas"           , "Brasil").
    RUN createState ("AP (BR)", "Amapa"              , "Brasil").
    RUN createState ("BA (BR)", "Bahia"              , "Brasil").
    RUN createState ("CE (BR)", "Ceara"              , "Brasil").
    RUN createState ("DF (BR)", "Distrito Federal"   , "Brasil").
    RUN createState ("ES (BR)", "Espirito Santo"     , "Brasil").
    RUN createState ("GO (BR)", "Goias"              , "Brasil").
    RUN createState ("MA (BR)", "Maranhao"           , "Brasil").
    RUN createState ("MG (BR)", "Minas Gerais"       , "Brasil").
    RUN createState ("MS (BR)", "Mato Grosso do Sul" , "Brasil").
    RUN createState ("MT (BR)", "Mato Grosso"        , "Brasil").
    RUN createState ("PA (BR)", "Para"               , "Brasil").
    RUN createState ("PB (BR)", "Paraiba"            , "Brazil").
    RUN createState ("PE (BR)", "Pernambuco"         , "Brazil").
    RUN createState ("PI (BR)", "Piaui"              , "Brazil").
    RUN createState ("PR (BR)", "Parana"             , "Brazil").
    RUN createState ("RJ (BR)", "Rio de Janeiro"     , "Brazil").
    RUN createState ("RN (BR)", "Rio Grande do Norte", "Brazil").
    RUN createState ("RO (BR)", "Rondonia"           , "Brazil").
    RUN createState ("RR (BR)", "Roraima"            , "Brazil").
    RUN createState ("RS (BR)", "Rio Grando de Sul"  , "Brazil").
    RUN createState ("SC (BR)", "Santa Catarina"     , "Brazil").
    RUN createState ("SE (BR)", "Sergipe"            , "Brazil").
    RUN createState ("SP (BR)", "Sao Paulo"          , "Brazil").
    RUN createState ("TO (BR)", "Tocantins"          , "Brazil").

    RUN assertTrue(TRUE).
END.                                                  

/*** Changes all states where region = Brasil (s) to Brazil (z).
    Notice that even though only 14 records are changed, 78 (all) are read
    since there's no index for this field. */
PROCEDURE testUpdateRecords:
    DEFINE VARIABLE iCountStates        AS INTEGER      NO-UNDO.
    FOR EACH state
        WHERE state.region = "Brasil"
        EXCLUSIVE-LOCK:
        iCountStates = iCountStates + 1.
        state.region = "Brazil".
    END.

    RUN assertEqualsInt(14, iCountStates).
END.

/**** Deletes all records create (14), but read 78 (full table) records as in 
      the update test.
      The surprising effect here is that customer table has a high number of 
      reads over a validation that checks if the state is related to a customer.
      Since there's no index, every delete creates a full table scan on customer,
      so 27 * 83 = 2241 reads.
      */
PROCEDURE testDeleteRecords:
    DELETE FROM state
        WHERE state.region = "Brazil".
    RUN assertTrue(TRUE).
END.


/** Utility procedures */

PROCEDURE createState PRIVATE:
    DEFINE INPUT PARAMETER cState       AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cStateName   AS CHARACTER        NO-UNDO.
    DEFINE INPUT PARAMETER cRegion      AS CHARACTER        NO-UNDO.

    CREATE state.
    ASSIGN
        state.state      = cState
        state.state-name = cStateName
        state.region     = cRegion.
END.
