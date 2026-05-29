//COB33CVJ JOB (ACCT),&SYSUID,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//***************************************************/
//COBRUN   EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(COB33CVD),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(COB33CVD),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//COBRUN    EXEC PGM=COB33CVD
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//COVIDATA  DD DSN=Z84549.COVID19.PS,DISP=SHR
//PRTLINE   DD SYSOUT=*,DCB=(LRECL=80,RECFM=FB),OUTLIM=1000
//SYSOUT    DD SYSOUT=*
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
//*****************************************************************
