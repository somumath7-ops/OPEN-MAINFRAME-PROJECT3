# OPEN-MAINFRAME-PROJECT3
OMP3 COBOL CODES AND JCLS
THE REPOSITORY CONATAINS OPEN MAINFRAME PROJECT CHALLENGES 9.1 THROUGH 9.4 INCLUDING COBOL CODES, MAPSETS AND JCLS


OMP3_9.1_COB.cbl
----------------

This program clears the jumble created by updating a dataset containing US Presidents, their allocated spend and actual spend. This fixes with the overlapping rows in the report printed in SYSOUT. 



OMP3_9.2.cbl
------------
This is a solution for Open Mainframe Project COBOL Programming Course #3
OMP$COB3 CHALLENGE 9.2.1 AND 9.2.2

The task is to create a COVID-19 Summary Report of all the countries in the world. JSON file was downloaded from the given API and converted to CSV. A COBOL program is written to reformat the data for display from the uploaded CSV file. The program also writes a report in the SYSOUT.

      *    CODE PERTAINING TO 9.2.1 ARE COMMENTED OUT TO ENABLE 9.2.2.
      *    CHALLENGE 9.2.1 - SEQUENTIAL FILE READ & DISPLAY ONE BELOW OTHER
      *    CHALLENGE 9.2.2 - SEQUENTIAL FILE READ AND DISPLAY IN A REPORT FORMAT
      *    SEQUENTIAL FILE: Z84549.COVID19.PS
      *    JCL TO COMPILE AND RUN THIS PROGRAM: Z84549.JCL.COB33CVJ.
      *    -------------------------------------------------------------
      *    This program reads a sequential file containing Covid-19 data
      *    and displays the data on the console. The input file is a physical
      *    sequential file that contains records of global country wise Covid-19
      *    data. PS file is loaded from a csv file on local machine using
      *    IND$FILE SEND command in TSO.
      *    The comma-separated values (CSV) file  is with the following format:
      *    Sl No, Country, Date, Cases, Deaths, Recovered, Active
      *    The program reads each record from the input file, parses the data,
      *    and displays the relevant information on the console. The program
      *    also handles end-of-file conditions & displays appropriate messages.


OMP3_9.3.1_2
------------

*OMPCOB3 CHALLENGE 9.3
      *---------------------
      *CHALLENGE 9.3.1: Missouri Unemployment Claims by Age data
      *was loaded into a CSV file on the PC. This in turn was transferred
      *from local machine CSV to MVS PS by using the command IND$File in
      *the TSO command section.

      *CHALLENGE 9.3.2: The data in PS was first sorted using PGM=SORT
      *in JCL and loaded into A KSDS using IDCAMS.

      *MVS PS: Z84549.UNEMP.CLAIMS.BYAGE.PS
      *VSAM KSDS: Z84549.UNEMP.CLAIMS.BYAGE
      *JCL to load VSAM KSDS from PS: Z84549.JCL.LOADVSAM
      *
      *This program reads this VSAM KSDS and outputs in PRINTLINE SYSOUT
      *as a formatted report. This is a sequential read and display of
      *a VSAM KSDS file.

      *JCL to compile and run this program: Z84549.JCL.COBAGEJ

      *CHALLENGE 9.3.3: This challenge is to interactively inquire details
      *of a record by entering the RECORD ID. This is achieved in the next
      *COBOL program: COBSUB1.(OMP3_9.3.3.cbl)


OMP3_9.3.3.cbl
--------------

This is a solution for Open Mainframe Project COBOL Programming Course #3 OMPCOB3 CHALLENGE 9.3.3

The task is to read data pertaining to unemployment claims of the state of Missouri. The data is categorized by industry, race, ethnicity, age and gender. Though all the five CSV files are uploaded into the mainframe PS and then to KSDS, the data pertaining to the Age category was chosen for printing report and for querying using CICS MAPS.

      *This program reads the unemployment claims by age data from a VSAM file
      *     based on the key received from a map in CICS region and then
      *     fetched record from a KSDS file for that record key field.

      *    BMS MAPSET: MAPSET3
      *    SOURCE LIBRARY: Z84549.MAPS.SRCLIB
      *    COPY LIBRARY: Z84549.MAPS.COPYLIB
      *    LOAD LIBRARY: &SYSUID..CICS.PROD.DFHLOAD

      *    JCL TO COMPILE BMS MAPSET: Z84549.JCL.COMPMAPS
      *
      *    VSAM KSDS: Z84549.UNEMP.CLAIMS.BYAGE
      *    CICS COPYBOOK:DFH620.CICS.SDFHCOB

      *    JCL TO COMPILE THIS PROGRAM: Z84549.JCL.CICSCOB

      *    SAMPLE VALID RECORD IDs to test: 10012011,10012012,10012013,
      *    10012014,10012015,10012017,10012018,10012020,10012021,10012022


OMP3_9.4.cbl
------------

*OMP$COB3 CHALLENGE 9.4: HACKER NEW RANKINGS FOR MAINFRAME/
      *COBOL POSTS.
      *
      *  This program reads the input csv file from a Zos dataset:
      *  ZOS.PUBLIC.HACKER.NEWS.D250518. The file mentioned in the
      *  challenge i.e. ZOS.PUBLIC.HACKER.NEWS was ignored since it had
      *  only two Mainframe/Cobol posts.
      *
      *  JCL TO COMPILE AND RUN/SORT THIS PROGRAM:Z84549.JCL.COBHAC1J.jcl.
      *-------------------------------------------------------------
      *  This program reads a physical sequential file mentioned above
      *  from the Zos environment and checks for various forms of strings
      *  Cobol and Mainframes. This is accomplished by converting entire
      *  Title string to lower case and looking for those words. Data was
      *  parsed using Unstring statement and further the Date field was
      *  parsed to separate date and time fields. Ranking score was
      *  calculated as per the given formula. Junk data was removed by
      *  first unstringing into a string field and then using NUMVAL to get
      * the numeric values of date time.
      *
      *  The outputted data was loaded in a PS file: Z84549.HACK.OUT.PS
      *  and the sorted output with the score in descending order using
      *  ICETOOL was printed in PRINTTLINE SYSOUT. Display statements
      *  were used to debug the program during run time.



