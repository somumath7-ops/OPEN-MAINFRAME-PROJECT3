//COBAGEJ  JOB (ACCT),&SYSUID,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//***************************************************/
//COBRUN   EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COBUNEMP),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COBUNEMP),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//COBRUN    EXEC PGM=COBUNEMP
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//UNEMPAGE  DD DSN=&SYSUID..UNEMP.CLAIMS.BYAGE,DISP=SHR
//PRTLINE   DD SYSOUT=*
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
/*
//*****************************************************************
