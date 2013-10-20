/*******************************************************************************
**
**     Include: databaseInfo.i
** Description: Temp table for databases information.
**      Author: SMarmotte
**     Created: 2013/06/29
**
********************************************************************************
**
**  Revision 1.0  2013/06/29  SMarmotte
**  - initial version
**
*******************************************************************************/

DEFINE TEMP-TABLE TT_DATABASE NO-UNDO
    FIELD cPhysicalName       AS CHARACTER
    FIELD cLogicalName        AS CHARACTER
    FIELD cAliasList          AS CHARACTER
    FIELD cConnectionString   AS CHARACTER
    FIELD lIsConnected        AS LOGICAL
    INDEX dxi IS PRIMARY UNIQUE cLogicalName.
