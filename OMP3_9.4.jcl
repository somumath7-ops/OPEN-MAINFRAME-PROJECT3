//COBHACKJ JOB (ACCT),&SYSUID,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
//*
//***************************************************/
//COBRUN   EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(HACKER),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(HACKER),DISP=SHR
//***************************************************/
// IF RC = 0 THEN
//***************************************************/
//HACKRUN   EXEC PGM=HACKER
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//HACKFILE  DD DSN=ZOS.PUBLIC.HACKER.NEWS.D250518,DISP=SHR
//PRTLINE   DD  DSN=Z84549.HACK.OUT.PS,DISP=SHR
//SYSOUT    DD SYSOUT=*
//SORTSTEP  EXEC PGM=ICETOOL
//TOOLMSG   DD SYSOUT=*
//DFSMSG    DD SYSOUT=*
//SORTIN    DD DSN=Z84549.HACK.OUT.PS,DISP=SHR
//SORTOUT   DD SYSOUT=*,DCB=(LRECL=150,RECFM=FB),OUTLIM=35000
//SYSOUT    DD SYSOUT=*
//TOOLIN    DD *
            DATASORT FROM(SORTIN) TO(SORTOUT) HEADER(2) USING(CTL1)
/*
//CTL1CNTL  DD *
            SORT FIELDS(145,6,CH,D),SKIPREC=2

//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ELSE
// ENDIF
//***************************************************************
