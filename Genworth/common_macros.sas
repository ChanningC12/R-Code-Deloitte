
/****************************************************************
This program contains common utility macros and formats

List of macros in this module:

charToHash(inString, outHash) 
	Calculates hash totals for a string variable
toDate(inFld,outFld)
	Converts 8-character date fields from a mmddyyyy format to a yyyymmdd format
mmDDyyToDate(inDate,outDate)
	Converts 8-character date fields from a mmddyyyy format to number of months from Jan 1901
yyMMddToDate(inDate,outDate)
	Converts 8-character date fields from a yyMMdd format to number of months from Jan 1901
mmYYtoDate(inDate,outDate)
	Converts 8-character date fields from a mmYYYY format to number of months from Jan 1901
toDateYYmm(inDate,outDate)
	This macro converts number of months from Jan 1901 to a character YYYYMM date
prtQCreport( libName, fileName, runCode=1 ) 
	Prints QC reports on a file
prtCAPreport( libName, fileName ) ;
	Prints a Excel loadable file containing capping logic
mergeAll(inTgtLib, inTgtFile, mergeKey=,outDataFile=, inSpecFile=,outMergeCode= );
	Generate and run the SAS code to merge different SAS data files
util_prtQCreport ( _libname_ =, _sasname_ = )
	Generate QC reports using prtQCreport macro and send each report to its own output file
EMPTYYN(DSNAME=&syslast);
	Gets number of observations in a dataset
MYEXCEL(LIBDSN,DOWNLOAD, VARNLABL, LIMIT, MOD, TITLE)
    SAS Macro to dump the contents of a SAS Data Set to a   
      CSV File for input to an Excel Spread Sheet      
        using existing SAS Formats to format output
qc_rpts( _libname_=RAW, _type_=);
	Generate QC reports for SAS datasets / views
mycontents(_libname_=RAW, _output_=mycontents.csv);
	get a list of SAS Datasets within a  
	specific library and provide some  
	details about date created, num of OBS,
	and Num of Vars for each dataset      
list_metadata(_libname_=RAW, _memtype_=VIEW);
	create report on all SAS selected types
          (VIEW  or DATA)     and the SAS vars   
          within each one.  Sorted by type and   
          variable name and then by variable     
          name and type 
crtFmt(infile,fmtName,label,start,end);
	Creates SAS formats from a SAS table

loadExcelToSAS(xmlFilename,outLib);
	This macro reads an Excel file and loads every tab into a SAS table of the same name
crtSAScodeFromTable(inlib,infile,outMacroNm,testMacroNm,startRecCnt=2,firstRecShowsDefaults=1);
	This macro converts a SAS table into if..then.. SAS code.
crtSAScodeFromExcel(xmlFilename);
	This macro converts an Excel spreadsheet inot if..then.. SAS code.


*****************************************************************/
proc format;
value $hash
 "A","a" = 11
 "B","b" = 12
 "C","c" = 13
 "D","d" = 14
 "E","e" = 15
 "F","f" = 16
 "G","g" = 17
 "H","h" = 18
 "I","i" = 19
 "J","j" = 20
 "K","k" = 21
 "L","l" = 22
 "M","m" = 23
 "N","n" = 24
 "O","o" = 25
 "P","p" = 26
 "Q","q" = 27
 "R","r" = 28
 "S","s" = 29
 "T","t" = 30
 "U","u" = 31
 "V","v" = 32
 "W","w" = 33
 "X","x" = 34
 "Y","y" = 35
 "Z","z" = 36
     "1" = 1
     "2" = 2
     "3" = 3
     "4" = 4
     "5" = 5
     "6"= 6
     "7" = 7
     "8" = 8
     "9" = 9
  other  = 0
 ;
run;


data _oneRecFile_;input a $;
cards;
A123
;
%let workPath = %sysfunc(pathname(work)) ; ** The SAS work directory ***;


/************************************
This macro calculates the hash total for a string field
************************************/
%macro charToHash(inString, outHash);
   &outHash=0;
   do i = 1 to length(&inString);
       &outHash = &outHash + input(put(substr(&inString, i , 1),$hash.),2.);
   end;
   drop i;
%mend;



/**************************************************

Convert two digit year, month and date fields into one date field.
****************************************************/
%macro getYYYYmmDD(inYear, inMonth, inDay, outDate);
  &outDate = '        ';
  if  input(substr(put(year("&sysdate"d) + 10 , $4.),3,2), 2.)  < input(&inYear, 2.) then do;
     &outDate = '19'||&inYear;
  end;
  else do;
     &outDate = '20'||&inYear;
  end;
  %if "&inMonth" ne "" %then %do;
     %if "&inDay" = "" %then %do;
          &outDate = substr(&outDate,1,4) || &inMonth;
     %end; %else %do;
          &outDate = substr(&outDate,1,4) || &inMonth || &inDay;
     %end;
  %end;
   
%mend;


/************************************
This macro converts 8-character date fields from a mmddyyyy format to a yyyymmdd format.
************************************/
%macro toDate(inFld,outFld);
   format &outFld $8.;
   &outFld = substr(&inFld,5,4)||substr(&infld,1,2)||substr(&infld,3,2);
%mend;


/************************************
This macro converts 8-character date fields from a mmddyyyy format to number of months from Jan 1901
************************************/
%macro mmDDyyToDate(inDate,outDate);
   &outDate=0;
   &outDate = (input(substr(compress(&inDate,' -/'),5,4),4.)
                  - 1901 ) * 12
             + input(substr(compress(&inDate,' -/'),1,2),2.);
%mend;

/************************************
This macro converts 8-character date fields from a yyMMdd format to number of months from Jan 1901
************************************/
%macro yyMMddToDate(inDate,outDate);
   &outDate=0;
   &outDate = (input(substr(compress(&inDate,' -/'),1,4),4.)
                  - 1901 ) * 12
             + input(substr(compress(&inDate,' -/'),5,2),2.);
%mend;


/************************************
This macro converts 8-character date fields from a mmYYYY format to number of months from Jan 1901
************************************/
%macro mmYYtoDate(inDate,outDate);
   &outDate=0;
   &outDate = (input(substr(compress(&inDate,' -/'),3,4),4.)
                  - 1901 ) * 12
             + input(substr(compress(&inDate,' -/'),1,2),2.);
%mend;

/************************************
This macro converts number of months from Jan 1901 to a character YYYYMM date
************************************/
%macro toDateYYmm(inDate,outDate);
   length &outDate $6.;

   yyyy = int((&inDate - 1)/12 + 1901);
   mm =  &inDate - (yyyy - 1901)*12;

   &outDate = yyyy*100 + mm;
   drop yyyy mm;

%mend;










/*****************
This macro creates and runs the SAS code to print all QC reports on a file.
The following reports will be printed by this macro:
            - Print 'proc contents'
                - Print first 20 observations in the file
                - Print a Proc Freq report on all short character variables in the file
              that are less than 6 charactrs long
                - Create a missing indicator on all long character variables in the file and
              print a Proc Freq report on all these missing indicators
            - Print Proc Univariate on all numeric variables in the file.
The reports are printed to the calling programs .lst file.

Parameters:
        libName: The library name of the SAS data file on which QC is to be done
      fileName: The name of the SAS data file on which QC is to be done


******************/
%macro prtQCreport( libName, fileName, runCode=1 ) ;

    *** get the variable list  ***;
    proc contents data=&libName..&fileName noprint out=allVars;  run;



    ***seggregate variables into numeric, short string and long string variables  ***;
    data num char2 char3;
     set allvars;
     file  "&workpath./_qcCode_.sas" notitle linesize=250 pagesize=500;
     if _n_ = 1 then do;
       put "*******Printing QC reports *****;";
       put "title 'QC Report for file &libname..&fileName ';";
       put " proc contents data=&libname..&filename;";
       put "proc print data=&libname..&filename (obs=20) ;";
       put " data in;set &libname..&fileName;";
     end;

     if type=1 then output num;
     if type=2 then do;
        if length < 5 then output char2;
        output char3;
     end;


   *** create missing indicator variables and length variables ***;
   data _NULL_;  set char3 end=lastobs;
   file  "&workpath./_qcCode_.sas" mod notitle linesize=250 pagesize=500;
   if length(name) > 30 then name = substr(name,1,30);
   put "  m_" name "='0';";
   put "  if " name " eq '' then m_" name "='1';";
   put "  l_" name "=0;";
   put "  h_" name "=0;";

   put "  l_" name "= length(compress(" name "));";
   put '%charToHash(inString=' name ', outHash=h_' name ');' ; 



   *** create MIN MAX and SUM totals for all variables ***;
   data _NULL_;  set _oneRecFile_ end=lastobs;
   file  "&workpath./_qcCode_.sas" mod notitle linesize=250 pagesize=500;
    put 'run;proc means data=in MIN MAX SUM;run;';




   **** create proc frequency statement for short character and derived variables ***;
   data _NULL_;  set char2 end=lastobs;
   file  "&workpath./_qcCode_.sas" mod notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
      put "proc freq data=in;  tables";
   end;
   put name;
   if lastobs then put ' /missing;';

   **** create proc frequency statement for long character variables ***;
   data _NULL_;  set char3 end=lastobs;
   file  "&workpath./_qcCode_.sas" mod notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
      put "proc freq data=in;  tables";
   end;
   if length(name) > 30 then name = substr(name,1,30);
   put "l_" name;
   if lastobs then put ' /missing;';


   **** create proc univariate statement for numeric variables ***;
   data _NULL_;  set num end=lastobs;
    file  "&workpath./_qcCode_.sas" mod notitle linesize=250 pagesize=500;
    if _n_ = 1 then do;
       put "proc univariate data=in;  var";
    end;
    put name;
    if lastobs then put ';';

    data _null_ ;
     set allvars;
     file  print notitle linesize=250 pagesize=500;
     if _n_ = 1 then do;
       put "title ;";
     end;
     stop;

   run;

   *** Run the above generated code ***;
   %if "&runCode" = "1" %then %do;
      %include "&workpath./_qcCode_.sas" ;
   %end;

%mend;







/*****************
This macro will be called by macro prtCapReport.

Do not use this macro directly in your programs.
Do not use this macro directly in your programs.
Do not use this macro directly in your programs.

This macro reads in a Proc Frequency report and creates a SAS file from the report.
The sas file will contain the following fields from the proc Freq report:
        varName         : The name of the variable
        varStartValue   : The starting value of the variable
        varEndValue     : The ending value for the variable
        varFreqPerc     : The frequency (as a percentage) of observations with variable
                                  value between the starting value and the ending value
        cappedValue     : Suggested capped value
        indVarName              : Suggested indicator variable name

If parameter 'Regroup' is set to 1, this macro will also group variable values into optimal groups.


******************/
%macro crtCapCSV(regroup=0);

   *** Read the Proc Freq report in to a SAS file ***;
   data crtCapCSV_a;infile "&workPath./_freq_.txt"  lrecl =32767 missover truncover delimiter = " ";
   retain varName varNamePrefix;
   format srcText1-srcText6 varName varNamePrefix $50.;
   input srcText1 $  srcText2 $ srcText3 $ srcText4 $ srcText5 $ srcText6 $;

   if _n_ = 1 then do;
     varname="";varNamePrefix="";
   end;

   if right(left(trim(srcText1))) = "" then return;
   if index(srcText1,"Title#Title#Title")> 0 or
      index(srcText2,"Title#Title#Title")> 0 then do;
      varNamePrefix = "";
      return;
   end;

   if srcText1= "The" and srcText2 = "FREQ" and srcText3 = "Procedure" then do;
      varNamePrefix = "";
      return;
   end;
   if srcText1= "Cumulative" and srcText2 = "Cumulative" then return;
   if index(srcText1,"----------") > 0 then do;
         varNamePrefix = "";
         return;
   end;

   if srcText2= "Frequency" and srcText3 = "Percent" and srcText4 = "Frequency" then do;
      varNamePrefix = compress(varNamePrefix || srcText1);
      varName = varNamePrefix;
      return;
   end;

   length dummyText $200.;drop dummyText;
   dummyText = compress(compress(srcText2) ||
                        compress(srcText3)
                   , "0123456789. ");
   if dummyText ne  "" or compress(srcText2)="" or
      compress(srcText3) = "" or compress(srcText4) = ""
      then do;
      varNamePrefix = compress(varNamePrefix || srcText1);
      varName = varNamePrefix;
      return;
   end;

   format varStartValue varEndValue $50.;
   varFreq=0;varFreqPerc=0;varCumFreq=0;varCumFreqPerc=0;
   if compress(srcText5) = "" then do;
       varStartValue = "";
       varFreq = srcText1;
       varFreqPerc=srcText2;
       varCumFreq=srcText3;
       varCumFreqPerc=srcText4;
   end; else do;
       varStartValue = srcText1;
       varFreq = srcText2;
       varFreqPerc=srcText3;
       varCumFreq=srcText4;
       varCumFreqPerc=srcText5;
   end;
   varEndValue = varStartValue;
   varName = compress(upcase(varName));
   keep varName varStartValue varEndValue varFreq varFreqPerc varCumFreq varCumFreqPerc;
   if varFreq=. or varCumFreq=. then do;
      varNamePrefix = compress(varNamePrefix || srcText1 );
      varName = compress(varNamePrefix);
      return;
   end;
   varNameFound=1;
   output;

   ***Get total number  of observations from last row of each Freq report ***;
   proc sort data=crtCapCSV_a;by varName descending varCumFreq;
   data crtCapCSV_a;set crtCapCSV_a;by varName;
   retain totObs;
   if first.varName then totObs = varCumFreq;

   ***Create a missing value row ***;
   proc sort data=crtCapCSV_a;by varName varCumFreq;
   data crtCapCSV_a;set crtCapCSV_a;by varName;
   output;
   if first.varName and
      compress(varStartValue) not in ("",".") then do;
      varStartValue = "Missing";
      varEndValue = "";
      varFreq = 0; varFreqPerc=0;
      varCumFreq = 0;varCumFreqPerc=0;
      output;
   end;

   ***Get total number  of observations with missing values from first row of each Freq report ***;
   proc sort data=crtCapCSV_a;by varName varCumFreq;
   data crtCapCSV_a;set crtCapCSV_a;by varName;
   retain missingCnt;
   if first.varName then missingCnt = 0;
   if first.varName and
      compress(varStartValue) in ("",".","Missing") then missingCnt = varCumFreq;


   *** Make variable value groups ***;
   %if "&reGroup" = "1" %then %do;
      data crtCapCSV_a;set crtCapCSV_a;by varName;
      retain h_grpId h_grpFreq;drop h_grpFreq;
      if first.varName then do;
         h_grpId=0; h_grpFreq=0;
      end;

      *** Do not group missing values and values with more than 5 percent data ***;
      if compress(varStartValue) in ("",".","Missing") or
         varFreq / (totObs - missingCnt) ge .05 then do;
         h_grpId = h_grpId + 1;h_grpFreq=0;
      end;

      output; **Write it out **;

      h_grpFreq = h_grpFreq + varFreq;
      ** once a group has more than 5 percent data, start a new group for the next row ***;
      if h_grpFreq / (totObs - missingCnt) ge .03 then do;
         h_grpId = h_grpId + 1; h_grpFreq=0;
      end;
      return;

      *** Calculate starting values,ending values, frequencies for each group ***;
      data crtCapCSV_a;set crtCapCSV_a;by varName h_grpId;
      retain h_startValue h_grpFreq h_grpFreqPerc ;
      drop   h_startValue h_grpFreq h_grpFreqPerc h_grpId;
      if first.h_grpId then do;
         h_startValue = varStartValue;
         h_grpFreq = 0;
         h_grpFreqPerc=0;
      end;
      h_grpFreq = h_grpFreq + varFreq;
      h_grpFreqPerc = h_grpFreqPerc + varFreqPerc;

      if last.h_grpId;

      varStartValue = h_startValue;
      varFreq = h_grpFreq;
      varFreqPerc = h_grpFreqPerc;
      output;

      proc sort data=crtCapCSV_a;by varname varCumFreq;

   %end;   ** Regrouping is now done ***;


   ***Make decisions on capped values ***;
   data crtCapCSV_a;set crtCapCSV_a;
    by varName;
   retain cappedValue;
   if first.varname then cappedValue=-1;

   *** Assign a capped value to each group ***;
   *** But skip missing value entries***;
   if compress(varStartValue) in (".","","Missing") then do;
   end; else do;
      cappedValue= cappedValue + 1;
      if cappedValue = 0 and
         varStartValue < 0 and
         varStartValue ne . then cappedValue = cappedValue + 1;
      if cappedValue = 0 and
         varStartValue > 0 then cappedValue = cappedValue + 1;
   end;
   keep varName varStartValue varEndValue varFreq totObs missingCnt cappedValue;

   *** Find the median group and use its capped value as the median capped value ***;
   data crtCapCSV_b (keep=varName medianCappedValue);set crtCapCSV_a;
     by varname;
    retain medianCappedValue cumFreq;
    if first.varName then do;
       medianCappedValue=.;cumFreq=0;
    end;
    ***skip the missing value group for median calculations ***;
    if compress(varStartValue) in (".","","Missing") then return;
    cumFreq = cumFreq + varFreq;
    if medianCappedValue = . and
       cumFreq / (totObs - missingCnt) ge 0.5 then medianCappedValue = cappedValue;
    if last.varName then output;
    return;

   data crtCapCSV_a;merge crtCapCSV_a crtCapCSV_b;
     by varName;
   data crtCapCSV_a;set crtCapCSV_a;
   format indVarName $50.;
   indVarName = "";
   if compress(varStartValue) in (".","","Missing") then do;
      *** Set the median capped value to the missing value  group   ***;
      cappedValue = medianCappedValue;
      *** Create missing indicator variable names for missing value groups ***;
      ****** indVarName = compress(varName || "_M");

   end;
   if compress(varStartValue) in (".","","Missing") then do;
      ** Frequency for missing value group is based on all observations ***;
      varFreqPerc = round(varFreq * 100 / totObs,0.01);
      varEndValue="";
   end;else do;
      ** Frequency for other groups is based on only the non-missing observations ***;
      varFreqPerc = round(varFreq * 100 / (totObs - missingCnt),.01);
   end;

   keep varName varStartValue varEndValue varFreqPerc cappedValue indVarName;

   *** Append the capping file to freqRpt file ***;
   proc append data=crtCapCSV_a base=freqRpt;

%mend;








/*****************
This macro print a Excel loadable capping file containing the following columns:
        varName         : The name of the variable
        varStartValue   : The starting value of the variable
        varEndValue     : The ending value for the variable
        varFreqPerc     : The frequency (as a percentage) of observations with variable
                                  value between the starting value and the ending value
        cappedValue     : Suggested capped value
        indVarName              : Suggested indicator variable name

The capping information will be printed for all the numeric variables and all character variables
that are less than 10 characters long.

For the character variables, the capping information is based on a Proc Freq report on the
entire dataset.
For the numeric variables, the capping information is based on a Proc Freq report on a
10000 record sample of the original dataset.



The capping file will have the same name as the parameter 'filename' passed to this macro
    with a cap_ prefix and a .txt extension.
The fields in the capping file will be separated by the ~ character.

Parameters:
        libName: The library name of the SAS data file for which capping file is to be created.
      fileName: The name of the SAS data file for which capping file is to be created.
******************/



%macro prtCAPreport( libName, fileName ) ;

   %if "&libname" = "" %then %let libname=WORK;


    *** get the variable list  ***;
    proc contents data=&libName..&fileName noprint out=allVars;  run;

    ***seggregate variables into numeric, short string and long string variables  ***;
    data num char2 longChar;
     set allvars;
     if type=1 then output num;
     if type=2 then do;
        if length < 4 then do;
            output char2;
        end;else do;
            output longChar;
        end;
     end;

   **** create proc frequency statements for short character variables ***;
   data _NULL_;  set char2 end=lastobs;
   file  "&workpath./_capCode_.sas" notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
      put 'proc printto print="&workPath./_freq_.txt" new;';
      put "proc freq data=&libname..&filename;  tables";
   end;
   put name;
   if lastobs then do;
      put ' /missing;';
      put 'title "Title#Title#Title";run;title;';
      put 'proc printto;';
      put '%crtCapCSV(regroup=0)';
   end;



   **** create proc frequency statements for a sample data for numeric variables ***;
      data num;set num longChar;run;
   data _NULL_;  set num end=lastobs;
    file  "&workpath./_capCode_.sas" mod notitle linesize=250 pagesize=500;
    if _n_ = 1 then do;
       put "data _temp_;set &libname..&filename nobs=nrecs;";
       put "randnum=ranuni(777); drop randnum;";
       put "if randnum < 20000/nrecs then output;run;";
       put 'proc printto print="&workPath./_freq_.txt" new;';
       put "proc freq data=_temp_;  tables";
    end;
    put name;
    if lastobs then do;
      put ' /missing;';
      put 'title "Title#Title#Title";run;title;';
      put 'proc printto;';
      put '%crtCapCSV(regroup=1)';
    end;

   run;

   *** Run the above generated code ***;
   %include "&workpath./_capCode_.sas" ;


    *** Print the capping file to a .txt file ***;
    data _NULL_;  set freqRpt end=lastobs;
    file  "cap_&filename..txt" notitle linesize=250 pagesize=5000;
    c="~";x=-1;
    if _n_ = 1 then do;
       put "varName~startValue~endValue~freqPerc~cappedValue~indicatorVarName~altVarName~SourceDataFileName";
    end;
    put  varName +x c +x varStartValue +x c +x varEndValue +x c +x
         varFreqPerc +x c +x cappedValue +x c +x indVarName +x c +x
         "  " c +x "&libname..&filename" c c c;


%mend;







/*****************
This macro will generate and run the SAS code to cap all variables in the input file.

The capping will be done only for variables in the inDataFile that are referred to in the
inCapFile. All other variables in the inDataFile will be left untouched.


Parameters:
        inDataLib: The library name of the SAS data file for which capping is to be done.
                 If this parameter is not passed, it will be defaulted to WORK
      inDataFile: The name of the SAS data file for which capping is to be done.
     outDataFile: The name of the output SAS dataset after all variables are capped.
                  If this parameter is not passed,
                  the outDataFile will be defaulted to inDataFile.
      inCapFile : The name of the ~ delimited text file that contains the capping rules.
                  If this parameter is not passed, it will be defaulted to cap_&inDataFile..txt
     outCapCode: The name of the file to which the capping code should be written to.
                        If this paramter is not passed, the capping code will be written to
                  the SAS temporary work area.

Additional parameters:
outCapVarList: Macro variables that is a list of variables in the capping text files  
runFreq	Y/N switch for running proc freq on all capped and indicator variables, The default is Y	 
crtIndVar	Y/N switch for creating indicator variables. The default is Y	   
nocapVarList:	Pass a list of variables that the user does not wish to cap. 	   
              (excluding nocap variables and indicator variables )
outFmtLib: The library to which capping formats need to be written to	   
assignFmt	Y/N switch to indicate whether to assign format or to create capped dataset. The default is 'N'	   

******************/
%macro docapping(inDataLib,
                    inDataFile, 
                    outDataFile=, 
                    inCapFile=,
                    outCapCode=,
                    outCapVarList=mylist,
                    runFreq=Y,
                    crtIndVar=Y,                    
                    nocapVarList=,
                    outfmtlib=work,
                    assignFmt=N);
                    
   %if "&outDataFile" = "" %then %let outDataFile=&inDataFile;
   %if "&inDataLib" = "" %then %let inDataLib=WORK;
   %if "&inCapFile" = "" %then %let inCapFile=cap_&inDataFile..txt ;
   %if "&outCapCode" = "" %then %let outCapCode=&workPath./capCode.sas ;

   %let runFreq=%cmpres(&runFreq.);
   %let crtIndVar=%cmpres(&crtIndVar.);
   %let assignFmt=%cmpres(&assignFmt.);
   
   *** Process all observations ***;
   %let _OBS_   = %sysfunc(getoption(OBS));
   options OBS=MAX;

   /* Read the comma delimited file that has the capping rules */
   data xform (keep=varName startValue endValue
                    newValue catVarName recNo)
        altFmt (keep=varName altVarName);
      infile "&inCapFile" delimiter='~' dsd missover ;
      length varName startValue endValue newValue
             catVarName altVarName $50. ;
      input varName $ startValue $ endValue $ freqPerc $
            newValue $  catVarName $ altVarName $;
      recNo = _n_;
      varname = upcase(compress(varname));
      altvarname = upcase(compress(altVarname));
      
      if varname = '' then delete;
      if missing(newValue) then newValue=".";
      if newValue = '' and catVarName = '' and altVarName = '' then delete;
      
      newValue = compress(newValue);
      ****startValue = left(right(startValue));
      ****endValue = left(right(endValue));
      if upcase(compress(startValue)) = "MISSING" then startValue =  "Missing";
      
      catvarname = upcase(compress(catvarname));
      
      
      if altVarName = "" then do;
         output xform;
      end;else do;
         output altFmt;
      end;
   run;   

   **** Copy other variables capping lines for variables with altFmt ***;
   proc sort data=altFmt(keep=varName altVarName)
        out=altFmt nodupkey;
      by varName;
   run;
         
   proc sql noPrint;
        create index
        varName on xform
       ( varName ) ;
   run;
   
   proc sql noPrint;
       create index
       altVarName on altFmt
       ( altVarName ); 
   run;
   
   proc sql noprint;
       create table xform2 as
       select A.varName , X.startValue
             ,X.endValue ,X.newValue
             ,x.catVarName ,x.recNo
         from altFmt as A, xform as X
        where X.varname = A.altVarName; 
   run;
   
   data xform2 missing2;set xform2;
        ***Do not copy categorical variable names from other variable ***;
         catVarName = "";

        if compress(startValue) in (".","","Missing") then do;
           output missing2;
        end;else do;
           output xform2;
        end;
   run;
        
   data missing1;set xform;
        if compress(startValue) in (".","","Missing") then output;
   run;
        
   proc sort data=missing1 nodupkey;
      by varName;
   run;
      
   proc sort data=missing2 nodupkey;
      by varName;
   run;
      
   data missing2;merge missing1(in=in_1) missing2(in=in_2);
     by varName;
     if in_2 and not in_1;
   run;
   
   data xform;
      set xform xform2 missing2;
      *** make up a format name based on first 8 characters of variable name ***;
      length fmtName $8.;fmtName = compress(varname," _1234567890");
      if length(right(left(fmtname))) > 6 then do;
         fmtname = substr(fmtname,1,6) || "  ";
      end;
      fmtCnt=1;
   run;

   proc sort data=xform;
      by varName;
   run;   


   *** Make up a format name that is unique to every variable ***;     
   data xform;set xform;by varName;
   retain varCount;drop varCount;
   if _n_ = 1 then do;
       varCount=0;
   end;
   if first.varName then do;
          varCount=varCount+1;
   end;
   fmtName="F"||put(varCount,z5.);
   fmtName=translate(fmtName,"abcdefghij","0123456789");



   ***Get variables that are in the data and their type ****;
  
   ***modified by ZBS 5/16/06 ***;
   proc contents data=&inDataLib..&inDataFile %if "&nocapVarList" NE "" %THEN (drop=&nocapVarList); 
        noprint out=varsInData;
   run;
   ***modified by ZBS 5/16/06 ***;
        
   data varsInData;
      set varsInData (keep=name type);
      keep varName type;
      length varname $50.;
      varName = upcase(name);
   run;
      
   proc sort data=varsInData;
      by varName;
   run;   

   ***Keep only variables that are in the data ***;
   data xform;
      merge xform (in=in_1)
            varsInData (in=in_data);
     by varName;
     if in_1 and in_data;
   run;  

   data xform catform; 
      set xform;
      if newValue ne "" then output xform;
      if catvarname ne '' then output catform;
   run;      

   proc sort data=xform(keep=varName) out=varlist nodupkey;
      by varname;
   run;
      
   proc sort data=catform(keep=catVarName) out=catVarlist nodupkey;
      by catVarName;
   run;  
   
   *** count format names that are the same for different variables ***;
   proc sort data=xform;
      by fmtName varName recNo;
   run; 
      
   proc summary data=xform nway missing;
     by fmtName varName ;
     var fmtCnt;
     output out=fmtCnt MAX=;
   quit;  

   proc summary data=fmtCnt nway missing;
      by fmtname ;
      var fmtCnt;
      output out=fmtCnt SUM=;
   run;   

   ***Rename formats that have same names ***;
   data xForm;
      merge xForm (drop=fmtCnt)
            fmtCnt(keep=fmtName fmtCnt);
      by fmtName;
   run;
   
   data xform;
      set xform;
      by fmtName varName;

      retain fmtNum;
      if first.fmtName then fmtNum = 0;
      if first.varName then fmtNum = fmtNum + 1;
      fmtNameLength = 7;
      if type = 1 then fmtNameLength = 8; ***numeric format***;
      drop fmtNameLength ;
      
      fmtNameNew = fmtName;
      
      if fmtCnt > 99 then do;
         fmtNameNew = compress( substr(fmtName,1,fmtNameLength - 3) || put(fmtNum,z3.)  );
      end; else if fmtCnt > 9 then do;
         fmtNameNew = compress( substr(fmtName,1,fmtNameLength - 2) || put(fmtNum,z2.)  );
      end; else if fmtCnt > 1 then do;
         fmtNameNew = compress( substr(fmtName,1,fmtNameLength - 1) || put(fmtNum,z1.)  );
      end;
      
      fmtNameNew = translate ( fmtNameNew , 'abcdefghij', '0123456789' )  ;
   run;

   data xform;
      set xform;
      drop fmtNameNew;
      fmtName=fmtNameNew;
   run;   

   proc sort data=xform;
      by varname;
   run;   
   
   ***First print all the formats ****;
   proc sort data=xform;
      by fmtName recNo;
   run;
      
   data temp;
     set xform;
     if newValue ne "" and fmtName ne "";
   run;
   
   proc sort nodupkey data=temp;
       by fmtName startValue endValue newValue;
   run;    
   
   proc sort data=temp;
      by fmtName recNo;
   run;   

   data temp;
      set temp;
      by fmtName;
      retain other_found;
      drop other_found;
      output;
      if first.fmtName then other_found=0;
      
      if upcase(startValue) = "OTHER" or
         upcase(endValue) = "OTHER" then do;
        other_found=1;
      end;
      
      if last.fmtName and other_found=0 then do;
         startValue = "OTHER"; endValue="";newvalue=".";
         output;
      end;
      return;
   run;
    
   data _null_;
      set temp end=lastobs;
      by fmtName;
      file "&outCapCode" notitle linesize=200 pagesize=32000;
      x = -1;dollar='$';quote="'";
      if type = 1 then do;
         dollar='';quote="";
      end;
      
      if first.fmtName then do;
         put "proc format lib=&outfmtlib.;value " dollar +x fmtName;
      end;
      
      if endValue = "" then endValue = startValue;
      
      if compress(startValue) in (".","","Missing") then do;
         startValue = "";
         if type = 1 then startValue = ".";
      end;
      
      if compress(endValue) in (".","","Missing") then do;
         endValue = "";
         if type = 1 then endValue = ".";
      end;
      
      if upcase(startValue) = "OTHER" or
         upcase(endValue) = "OTHER" then do;
         startValue = "OTHER";
         endValue = "";
      end;
      
      if upcase(startValue) = "LOW" or
         upcase(startValue) = "OTHER" then do;
         put startValue @;
      end; else do;
           put quote +x startValue +x quote @;
      end;
      if endValue ne "" then do;
         if upcase(endValue) = "HIGH" then do;
            put "  -  HIGH " @;
         end; else do;
            put "  -  " quote +x endValue +x quote @;
         end;
      end;
      put " = " newValue ;
      if last.fmtName then do;
         put ";";
      end;
   run;
   
   ***modified by ZBS 5/16/06 ***;
   ***generate macro variables***;
   proc sort data=xform nodupkey out=getVarlist;
      by varname;
   run; 
   
   data getVarlist; 
     set getVarlist; 
     if type = 2 then fmtName=compress("$"||upcase(fmtName));
   run;     
   
   %let _CAT_VARNAME_LIST_=;   
   proc sql noprint;   
      select upcase(varname)
         into: _LIST_
         separated by ' '       
         from getVarlist; 
      
      select fmtName
         into: _FMTNAME_
         separated by ' '       
         from getVarlist;                   
       
      select count(*)
         into: _CNT_LIST_
         separated by ' '       
         from getVarlist;
      
      select upcase(catVarName)
          into: _CAT_VARNAME_LIST_
          separated by ' '       
          from catform;         
   quit;
   
   %global %str(&outCapVarList.);
   %let %str(&outCapVarList.)=&_LIST_.;
   %if &_CAT_VARNAME_LIST_= %then %let crtIndVar=N;

   **Print DATA statement if crated capped dataset or indicator varaibles***;
   **DATA statement will not be crated if assigning format only***;
   %if %UPCASE(&assignFmt) ne Y  or %upcase(&crtIndVar) eq Y %then %do;      
      data _null_;
         set varlist catVarlist;
         file "&outCapCode" mod notitle linesize=200 pagesize=32000;
         x = -1;
         if _n_ = 1 then do;
            put "DATA errFile(keep=varName varValue) " ;
            put "&outDataFile (drop = varName varValue";
            %if %UPCASE(&assignFmt) eq Y %then put ");" %str(;) ;
         end;
         if _n_ > 1 then stop;
       run;  
    %end;
    
    **Print additional data statement lines for creating capped dataset***;
    %if %UPCASE(&assignFmt) ne Y %then %do; 
       data _null_;
          set varlist end=lastobs;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          x = -1;
          put varname +x  '_' ;
       run;   
         
       data _null_;
          set varlist catVarlist;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          x = -1;
          if _n_ = 1 then do;
             put ");" ;
          end;
          if _n_ > 1 then stop;
       run;   
      
       **Print SET statement  ***;
       data _null_;
          set varlist catVarList;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          x = -1;
          if _n_ = 1 then do;
             put "SET &inDataLib..&inDataFile " ;
          end;
          if _n_ > 1 then stop;
       run;
          
       data _null_;
          set varlist end=lastobs;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          x = -1;
          if _n_ = 1 then do;
             put '( rename=(';
          end;
          put varname ' = ' varname +x '_' ;
          if lastobs then do;
             put '))';
          end;
       run;
          
       data _null_;set varlist catvarlist;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          x = -1;
          if _n_ = 1 then do;
             put ";" ;
             put 'length varName varValue $50.; ';
          end;
          if _n_ > 1 then stop;
        run;  
             
       proc sort data=xform;
          by varName recNo;
       run;   
   
      ***Print statements to CAP variables ***;
      data _null_;
         set xform end=lastobs; by varName;
         file "&outCapCode" mod notitle linesize=200 pagesize=32000;
         x = -1;
         if first.varName then do;
         put varname '=  .;' @;
         if type = 1 then do;
            put varname '=  put(' varName +x "_ ," fmtName +x '.'     ');';
         end;else do;
            put varname '=  put(left(right(' varName +x "_)) , $" fmtName +x '.'     ');';
         end;
         ***Print statements to check if capping worked OK ***;
         put 'if ' varname '=. then do;';
         put '    varName="' varname '";'  'varValue =' varName +x '_ ;' ;
         put '    output errFile;';
         put 'end;';
      end;
    %end;
   
    ***Print additional data statement lines for creating indicator varaibles***;
    %if %upcase(&crtIndVar) eq Y %then %do;
      ***Create indicator variables ****;
      proc sort data=catform;
         by varName recNo;
      run;   
      
      %if %UPCASE(&assignFmt) eq Y %then %do;
          data _null_;
             set varlist catVarList;
             file "&outCapCode" mod notitle linesize=200 pagesize=32000;
             x = -1;
             if _n_ = 1 then do;
                put "SET &inDataLib..&inDataFile;" ;
             end;
             if _n_ > 1 then stop;
          run;
      %end;   

      ***Print statements to initialize categorical variables ***;      
      data _null_;set catVarlist end=lastobs;
         file "&outCapCode" mod notitle linesize=200 pagesize=32000;
         x = -1;
         if _n_ = 1 then do;
            put; put '/* Initialize categorical variables */';
         end;
         put catvarname +x   '  = 0; ' ;
         if lastobs then put;
      run;   
      
      data _null_  ;
         set catform end=lastobs; 
         by varName;
         
         file "&outCapCode" mod notitle linesize=200 pagesize=32000;
         x = -1; quote="'"; 
         txtGE="ge";
         txtLE="le";
         
         if length(compress(startValue)) > 1 and substr(compress(startValue),length(compress(startValue)),1) = "<"
            then do;
            txtGE = "gt";
            startValue = translate(startValue," ", "<");
         end;
         
         if length(compress(endValue)) > 1 and substr(compress(endValue),1,1) = "<" then do;
            txtLE = "lt";
            endValue = translate(endValue," ", "<");
         end;
         
         if compress(startValue) in ("",".","Missing") then startValue="";
         if compress(endValue)   in ("",".","Missing") then endValue="";
         
         
         if compress(catVarName) =  '' then return;
         if upcase(compress(startValue)) = "LOW"   and compress(endValue)   = "" then return;
         if upcase(compress(endValue))   = "HIGH"  and compress(startValue) = "" then return;
         
         if type = 1 then do;
            quote = " ";
            if startValue = "" then startValue = ".";
            if endValue = ""   then endValue   = ".";
         end;
         
         %if %UPCASE(&assignFmt) ne Y %then %let varsuffix= "_  ";
            %else %let varsuffix=" ";
                           
         if upcase(compress(startValue)) = "LOW" then do;
            put "if " varName +x  &varsuffix. txtLE quote +x endValue +x quote " then " catVarName " = 1;";
            return;
         end;
         
         if upcase(compress(endValue)) = "HIGH" then do;
            put "if " varName +x  &varsuffix. txtGE quote +x startValue +x quote " then " catVarName " = 1;";
            return;
         end;
         
         if compress(endValue) eq "" then endValue=startValue;
         
         put "if "  varName +x  &varsuffix.  txtGE quote +x startValue +x quote @;
         put " AND "  varName +x  &varsuffix.  txtLE quote +x endValue   +x quote @;
         put " then " catvarname  ' = 1;' ;
      run;
   %end;   
      
   ***close data statement****;
   %if %UPCASE(&assignFmt) ne Y  or %upcase(&crtIndVar) eq Y %then %do;
       data _null_;
          set varList  
              catvarList (rename=(catVarName=varName)) 
              end=lastobs;
          file "&outCapCode" mod notitle linesize=200 pagesize=32000;
          if _n_ = 1 then do;
             put "output &outDataFile;run;" ; put; put;
             ***Print statements to process effFile ***;
             %if %UPCASE(&assignFmt) ne Y %then %do; 
                 put '****Processing errFile ****;';
                 put 'proc sort data=errFile nodupkey;';
                 put '  by varname varvalue;';
                 put 'data errFile;set errfile;';
                 put 'by varName;';
                 put 'retain N;drop N;';
                 put 'if first.varName then N = 0 ;';
                 put 'N=N+1;';
                 put 'if N < 20 or last.varname then output;';
                 put 'proc print data=errFile;';
                 put 'title "Capping values were NOT specified for the following Data";';
                 put 'run;title;';put;put;      
             %end;   
          end;
          if lastobs then put '; run;' ;      
       run;
   %end;  
   
   options OBS=&_OBS_;
   %include "&outCapCode";            
   
   %if %cmpres(%upcase(&outDataFile.)) ne %cmpres(%upcase(&inDataLib..&inDataFile.)) %then %do;
      %if %UPCASE(&crtIndVar) NE Y and %UPCASE(&assignFmt) EQ Y %then %do;
         data &outDataFile.;
            dummy ='';
         run;
      %end;   
      
      proc contents data=&outDataFile. %if "&nocapVarList" NE "" %THEN (drop=&nocapVarList);
              OUT=varsInData noprint; 
      run;       
      
      proc sql noprint;
         select upcase(LIBNAME)
         into: outDataLib
         separated by ' '
         from varsInData(obs=1);
         
         select upcase(memname)
         into: outDataFile
         separated by ' ' 
         from varsInData(obs=1);
      quit;    
   %end;
   %else %do; 
       %let outDataLib=&inDataLib.;
       %let outDataFile=&inDataFile.;  
   %end;           
    
   %if %UPCASE(&assignFmt) EQ Y and 
       %UPCASE(&crtIndVar) NE Y and  
       %cmpres(%upcase(&outDataLib..&outDataFile.)) ne %cmpres(%upcase(&inDataLib..&inDataFile.))
       %then %do;      
       
       proc datasets library=&outDataLib. nolist;
          delete &outdatafile;
       quit;
       
       proc datasets library=&inDataLib. nolist;
          copy out=&outDataLib. memtype=data;
             select &indatafile;
       quit; 
       
       proc datasets library=&outDataLib. nolist;
          CHANGE &indatafile=&outdatafile;
       quit; 
   %end;    

    %if "&outfmtlib" ne "" %then %do;
        options fmtsearch=(&outfmtlib);
    %end;
                                    
   %if %UPCASE(&assignFmt) EQ Y %then %do;    
       %put count_list=&_CNT_LIST_;                                                           
       proc datasets library=&outDataLib. nolist;         
          modify &outDataFile.;
             format %cmpres(%do i = 1 %to &_CNT_LIST_; 
                               %qscan(&_LIST_,&i,%str( )) %qscan(&_FMTNAME_,&i,%str( )).
                            %end;) 
                    %str(;)
       quit;                    
   %end;   
   
   **** Run PROC FREQ on selected variables  ***;
   %if %upcase(&runFreq) eq Y %then %do; 
      
      proc freq data=&outDataLib..&outDataFile; 
         tables &_LIST_ %if %upcase(&crtIndVar)=Y %THEN &_CAT_VARNAME_LIST_; %str(;)
      run;   

   %end;   

%mend ;



/***************************************************************
This macro unassigns all format associations in the dataset
*****************************************************************/
%macro unassignFmt(inDataLib,
                   inDataFile);
                   
   ***This macro will remove all format assignment from a dataset***;                   
                    
   proc datasets library=&inDataLib nolist;
      modify &inDataFile;
         format _all_ ;
   run;

%mend unassignFmt;





/*****************
This macro will generate and run the SAS code to merge different SAS data files.

******************/
%macro mergeAll(inTgtLib, inTgtFile, mergeKey=,outDataFile=, inSpecFile=,outMergeCode= );

   %let ERR_FLAG=0;
   %if "&inTgtLib" = "" %then %let inTgtLib=WORK;
   %if "&inSpecFile" = "" %then %let inSpecFile=inSpecFile ;
   %if "&mergeKey" = "" %then %let mergeKey=polKey;
   %if "&outDataFile" = "" %then %let outDataFile=allVars_preCap ;
   %if "&outMergeCode" = "" %then %let outMergeCode=&workPath./mergeCode.sas ;

   proc printto print="&outMergeCode" new;
   data specFile;set &inSpecFile;
   file print notitle linesize=100 pagesize=32000;
   x = -1;
   varname = upcase(compress(varname));
   if varname = '' then delete;
   libName = upcase(compress(libName));
   if libname = '' then libname = "WORK";
   if fileName = '' then delete;
   inclCnt = 1;
   if upcase(addDelete) = "D" then inclCnt = -1;
   keep varName libName fileName inclCnt;
   if varName = "ALL" then do;
      put "proc contents data=" libname +x "."
          filename " noprint out=_temp_;";
      put "data _temp_;set _temp_;";
      put "keep varName libName fileName inclCnt;";
      put "length varName $32. libName $8. fileName $32.;";
      put "varName = upcase(compress(name));";
      put 'libName = "' libname '";' ;
      put 'fileName = "' fileName '";';
      put 'inclCnt = ' inclCnt ';' ;
      put "proc append base=specfile data=_temp_;";
      delete;
   end;

   proc printto;
   %include "&outMergeCode"; run;

   proc sort data=specFile;
     by varName libName FileName;

   proc summary data=specFile nway missing;
     by varName libName FileName;
    var inclCnt;
    output out=specFile sum=;

   data specfile;set specFile;
   keep varName libName FileName;
   if inclCnt > 0;

   data errFile;set specfile;
     by varName;
   if first.varName and last.varName then delete;
   if varName = upcase("&mergeKey") then delete;

   data _null_;set errFile;
   call symput("ERR_FLAG",1);
   if _n_ > 1 then stop;

   proc print data=errFile;
   title "List of Duplicate Field Names.";run;title;

  %if &ERR_FLAG ne 0 %then %do;
      %makeErr(Errors occurred. Read .lst file to get a list of errors. )
      %goto endOfMacro;
  %end;

  proc printto print="&outMergeCode" new;
  proc sort data=specFile;by fileName varName;
  data _null_;set specfile;
  file print notitle linesize=100 pagesize=32000;
  x = -1;
  if _n_ = 1 then do;
     put "data &outDataFile;" ;
     put "merge &inTgtLib..&inTgtFile";
     put "(in=in_tgt keep= &mergeKey " ;
  end;
  if fileName = upcase("&inTgtFile") and
     libName = upcase("&inTgtLib");;
  if varName ne upcase("&mergeKey") ;
  put varname;

  data _null_;set specfile;
    by fileName;
  file print notitle linesize=100 pagesize=32000;
  x = -1;
  if _n_ = 1 then do;
     put " )" ;
  end;
  if fileName = upcase("&inTgtFile") and
     libName = upcase("&inTgtLib") then return ;

  if first.fileName then do;
     put libname +x "." fileName " (keep=&mergeKey ";
  end;

  if varName ne upcase("&mergeKey") then do;
      put varname;
  end;

  if last.fileName then do;
     put " )";
  end;

  data _null_;set specfile;
    by fileName;
  file print notitle linesize=100 pagesize=32000;
  x = -1;
  if _n_ > 1 then stop;
  put " ;" ;
  put "if in_tgt ; ";
  put "run;";put;put;


  proc printto;
  %include "&outMergeCode"; run;


%endOfMacro:
%mend;






/***********************
This macro simply abends the SAS process
************************/
%macro makeErr(errMsg);
    &errMsg
%mend;

/*****************************************************************
Program: util_prtQCreport.sas
By: Charles Patridge
On: 01/10/2005
Purpose: generate QC reports using prtQCreport macro and send each
         report to its own output file
         and prefix each one with "qc_" for easier sorting of lst files
Sample call to macro:
%util_prtQCreport( _libname_=raw, _sasname_=polsys_aa );

******************************************************************/

  %macro util_prtQCreport ( _libname_ =, _sasname_ = );
    proc printto print="qc_&_sasname_..lst" new; run;
     %prtQCreport(libname=&_libname_ , filename=&_sasname_ ); run;
    proc printto; run;
  %mend util_prtQCreport;



/*****************************************************************
EMPTYYN Macro to get NUMBER OF OBS 
Sample call:
%emptyyn( raw.polsys_aa )
******************************************************************/
  %GLOBAL EMPTYYN NUMOBS DSN;
  %MACRO EMPTYYN(DSNAME=&syslast);
    %let dsn = &dsname;
    DATA _NULL_;
     IF 0 THEN SET &DSNAME NOBS=NUMOBS;
     IF NUMOBS > 0 THEN EMPTYYN = 'N';
                   ELSE EMPTYYN = 'Y';
     CALL SYMPUT('EMPTYYN',PUT(EMPTYYN, $1.));
     CALL SYMPUT('NUMOBS' ,PUT(NUMOBS , BEST.));
     STOP;
    RUN;
  %MEND EMPTYYN;




  /*****************************************************************
  MYEXCEL.SAS           
    SAS Macro to dump the contents of a SAS Data Set to a   
      CSV File for input to an Excel Spread Sheet      
        using existing SAS Formats to format output       
                                                
     Macro Vars                                       
         LIBDSN   = SAS Dataset name ( CFDATA.AAAlines)     
         DOWNLOAD = CSV File ("/export/home/patridge/xx.csv")
         VARNLABL = Y or N (Place Var Names and Labels on   
                            1st and 2nd line of CSV File) 
         LIMIT    = # of obs (none = all records)        
                                                          
         MOD      = MOD (Need to Append output to existing  
                         CSV File)                         
        TITLE    = Y/N  Name of SAS Dataset will be put in 
                         to the spreadsheet.             

 Sample call to MACRO:
 dump 50 records from testfile into testfile.csv with name of SAS dataset included 
  %myexcel( experian.testfile , "testfile.csv", N, 50,    , Y );

 dump entire testfile into /export/home/patridge/testfile.csv with Labels and
       append to existing file if it exists 
  %myexcel( experian.testfile , "/export/home/patridge/testfile.csv", Y,   , MOD,   )


  ************************************************************/

  %MACRO MYEXCEL(LIBDSN,DOWNLOAD, VARNLABL, LIMIT, MOD, TITLE);
   FILENAME OUTFILE &DOWNLOAD LRECL=8000 RECFM=V ;
   PROC CONTENTS DATA=&LIBDSN
   OUT=_TEMP_(KEEP=NAME TYPE VARNUM LABEL FORMAT FORMATD FORMATL) NOPRINT;
   RUN;

   PROC SORT DATA=_TEMP_ OUT=_TEMP_; BY VARNUM; RUN;

   DATA _NULL_; SET _TEMP_ END=EOF;
    CALL SYMPUT('ZVR'||(LEFT(PUT(_N_,5.))),NAME);
    CALL SYMPUT('TYP'||(LEFT(PUT(_N_,5.))),LEFT(PUT(TYPE,8.)));
    IF LABEL = ' ' THEN LABEL = NAME;
    CALL SYMPUT('LBL'||(LEFT(PUT(_N_,5.))),LEFT(LABEL));
    CALL SYMPUT('FMT'||(LEFT(PUT(_N_,5.))),LEFT(FORMAT));
    CALL SYMPUT('FMD'||(LEFT(PUT(_N_,5.))),PUT(FORMATD,BEST.));
    CALL SYMPUT('FML'||(LEFT(PUT(_N_,5.))),PUT(FORMATL,BEST.));
    IF EOF THEN CALL SYMPUT('TOTAL',LEFT(PUT(_N_,8.)));
   RUN;

  DATA _NULL_;
   %DO ZI=1 %TO &TOTAL;
     LENGTH  TMP&ZI $40 ;
     TMP&ZI = "&&FMT&ZI" || "&&FML&ZI" || "." || "&&FMD&ZI";
     if compress("&&FML&ZI") = "0" then do;
       TMP&ZI = "&&FMT&ZI" || ".";
     end;
     TMP&ZI = COMPRESS(TMP&ZI);
     IF "&&FMT&ZI" = " " THEN TMP&ZI = 'BEST.' ;
     CALL SYMPUT("FMT&ZI", TMP&ZI);
   %END;
  RUN;

   DATA _NULL_;
    FILE OUTFILE  NOPRINT NOTITLES &MOD;
    SET &LIBDSN;
    FORMAT _NUMERIC_ BEST12.;
     IF _N_ = 1 THEN DO;
         PUT 'SEP=!';
        %if &title = Y %then %do;
         put "SASDSN=&LIBDSN" '!';
        %end;
     END;
     %if &limit gt 0 %then %do;
        if _n_ gt &limit then stop;
     %end;
    %IF &VARNLABL EQ Y %THEN %DO;
       IF _N_ = 1 THEN DO;
        PUT
       %DO YI =1 %TO &TOTAL;
         %CMPRES("'&&ZVR&YI") +(1) '!'
       %END;
       +(-1)' '; /* REMOVE LAST HANGING COMMA DELIMITER */
      END;
       IF _N_ = 1 THEN DO;
        PUT
       %DO XI =1 %TO &TOTAL;
         %CMPRES("'&&LBL&XI") +(1) '!'
       %END;
       +(-1)' '; /* REMOVE LAST HANGING COMMA DELIMITER */
      END;
    %END;
    PUT
     %DO WI=1 %TO &TOTAL;
       %IF &&TYP&WI=1 %THEN %DO;  /* IF NUMERIC VARIABLE */
           &&ZVR&WI  &&FMT&WI +(1) '!'
       %END;
       %ELSE %DO;   /* IF CHARACTER VARIABLE */
          " "  &&ZVR&WI  +(-1) "!"
       %END;
     %END;
     +(-1) ' '; /* REMOVE THE EXTRA COMMA AT THE END */
   RUN;
   %MEND MYEXCEL;





/*****************************************************************
Program: qc_rpts.sas
By: Charles Patridge
On: 01/10/2005
Purpose: generate QC reports for SAS datasets / views
Sample call of macro:
%qc_rpts (_libname_=RAW, _type_=DATA );
%qc_rpts (_libname_=RAW, _type_=VIEW );

******************************************************************/

%macro qc_rpts( _libname_=RAW, _type_=);
 %let _libname_ = %trim(%upcase( &_libname_ ));
 %let _type_    = %trim(%upcase( &_type_    ));

%global _sasdsns_;
/*** get list of sas datasets or views ***/
 /*** determine howmany there are and force names to be lowercase ***/
 data _listdsns_;
  set sashelp.vstabvw (keep=libname memname memtype);
  where libname = "&_libname_" and memtype = "&_type_";
  memname = lowcase(memname);
 run;

 %emptyyn( dsname=work._listdsns_);
 %let _howmany_ = &numobs;

 /*** now process the list through some macro routines ***/
 %macro loop_rpt( type=VIEW);
  %do _ii_ = 1 %to &_howmany_ ;
    data _null_;
     recd = &_ii_;
     set _listdsns_ point=recd;
     call symput('_sasname_', trim(memname));
     stop;
    run;

    proc printto print="qc_&_sasname_..lst" new; run;

     %prtQCreport(libname=&_libname_ , filename=&_sasname_ );

    proc printto; run;
  %end;
 %mend loop_rpt;

 %loop_rpt;
 %mend qc_rpts;





/*************************************************
 mycontents.sas             
  get a list of SAS Datasets within a  
    specific library and provide some  
    details about date created, num of OBS,
    and Num of Vars for each dataset      
                                         
   and dump output to a CSV file    
                                
 1st parameter: _libname_ = Name of SAS Lib 
 2nd parameter: _output_  = Name of output 

Sample call to macro 
%mycontents( _libname_=RAW,
             _output_ ="/export/home/patridge/mycontents.csv" ); 

                                            
************************************************/

%macro mycontents(_libname_=RAW, _output_=mycontents.csv);
proc sql noprint;
   create table _mycontents_ as
   select libname, memname, memtype, crdate, nobs, nvar
   from    sashelp.vtable
   where   libname = "&_libname_";
quit;

%myexcel( work._mycontents_, "&_output_", Y );
%mend mycontents;





 /*****************************************************
 list_metadata.sas                                
 Author: Charles Patridge                        
                                                 
 Purpose: create report on all SAS selected types
          (VIEW  or DATA)     and the SAS vars   
          within each one.  Sorted by type and   
          variable name and then by variable     
          name and type                          
                                                 
 1st parameter:                                  
  _libname_   = name of SAS Library to report on 
                ie RAW                           
 2nd parameter:                                  
  _memtype_   = DATA or VIEW                     
                                                 
 %list_metadata( _libname_ =RAW,_memtype_=VIEW); 
                                                 
 IT IS IMPORTANT THAT YOU INCLUDE the CORRECT     
 LIBRARY.TXT for the library you select          
 ex: %include "/DMS/SOURCE/HMIC/AUTO/library.txt";

 Sample call to macro - make sure to include library.txt 
 ex: %include "/DMS/SOURCE/HMIC/AUTO/library.txt";
 %list_metadata(_libname_=RAW,_memtype_=VIEW); 
 %list_metadata(_libname_=RAW,_memtype_=DATA); 


 *****************************************************/

 %macro list_metadata(_libname_=RAW, _memtype_=VIEW);
  %let _memtype_ = %upcase( &_memtype_ ); /*** make sure it is upper case ***/
  %let _libname_ = %upcase( &_libname_ ); /*** make sure it is upper case ***/

   data _path_;
    set sashelp.vslib (where=(libname="&_libname_"));
    call symput('_path_', trim(path));
   run;

  data metadata;
   set sashelp.vcolumn (where=(libname = "&_libname_" and memtype="&_memtype_"));
   name = lowcase(name);
   memname = lowcase(memname);
  run;

  proc sort data=metadata out=metadata; by memname name; run;

  title1 "List of SAS &_memtype_ within &_libname_";
  title2 "Physical Path = &_path_";
  title3 "Sorted by Member and Variable";
  proc print data=metadata n ;
    by memname;
    id memname;
    pageby memname;
    var name type length npos varnum format format informat idxusage;
  run;

  proc sort data=metadata out=metadata; by name memname; run;

  title3 "Sorted by Variable and Member";
  proc print data=metadata n ;
    by name;
    id name;
    var memname type length npos varnum format informat idxusage;
  run;

  proc datasets library=work nolist; delete _path_ metadata; quit;

 %mend list_metadata;



/************************************************
Create a format from a SAS dataset
parameters: 
	infile: the SAS dataset from which a format needs to be created
	fmtName: The name of the format
	label: the variable name which contains the converted value
	start: the variable name which contains the starting original value 
	end: the variable name which contains the ending original value 

Example:
	Say that you have SAS table called classTable that contains two columns namely, class and classGroup
	This table maps each class to a classGroup.
	You want to create a format called fmtClsG that can be used to convert class to a class group.
	You can create this format by calling this macro, using the following syntax:
		%crtFmt(infile=work.classTable,fmtName=fmtClsG,label=classGroup,start=class,end=class)

	You can then use the above format in your data step by using the following syntax:
	data A; set A;
	  myClassGroup = put( myClass , fmtClsG. )
	run;

	The above code works if the variable myClass is numeric. 
	If myClass is a character variable, then you need to create character format. You can create a character format
	by putting a $ prefix to the format name. Thus you would call the %crtFmt macro using the following syntax:

		%crtFmt(infile=work.classTable,fmtName=$fmtClsG,label=classGroup,start=class,end=class)


*************************************************/

%macro crtFmt(infile,fmtName,label,start,end);
    proc sort data=&infile  out=_ctrl_ nodupkey;
      by &start;

   data _ctrl_;set _ctrl_;
   if &start > &end then delete;

   data _ctrl_;set _ctrl_ end=lastobs;
   keep start end label fmtName hlo; 
   start = &start;end = &end;
   label=&label;fmtName = "&fmtName";
   hLo=" ";output;
   **put in default value ***;
   if lastobs then do;
     hLo="O";
     label="";start="";end="";
     output;
   end;
proc format cntlin=_ctrl_;

%mend;



/***********************************************
This macro makes SAS names from the contents of a sting variable
This macro is called by loadExcelToSAS() macro. 
This macro cannot be used from other programs.
************************************************/
%macro _makeSASname_(inVar,outVar);
   &outVar = &inVar;
   ***Remove all unacceptable characters ***;
   &outVar = compress(&outVar,' "');
   &outVar = compress(&outVar," ~`'!@#$%^&*()-[]{}|\<>?/:;");

   ***if the variable has just numbers, prefix it with an underscore ***;
   if missing(compress(&outVar,"0123456789")) then &outVar = "_"|| &outVar;

%mend;







/***********************************************
This macro loads an Excel tab into a SAS table
This macro is called by loadExcelToSAS() macro. 
This macro cannot be used from other programs.
************************************************/
%macro _loadThisTab_(outLib,tabName);
   ****Keep only data that belongs to this tab ***;
   data _temp_; set _ExcelData_;
    if name = "&tabName";

   ***Write the Exceldata to a delimited file ***;
   data _null_;set _temp_;
     by WorksheetIndex rowIndex columnIndex ; 
     file  "&workpath./&tabName..txt" lrecl=32767;
   c= "~";x=-1;retain h_columnIndex; 

   if first.rowIndex then do;
       h_columnIndex=0;
   end;
   ***write out extra delimiters for missing cell values ***;
   h_columnIndex = h_columnIndex + 1;
   if columnIndex > h_columnIndex then do;
      do i = 1 to (columnIndex  - h_columnIndex);
          put c +x @;
      end;
   end;

   **write out the actual cell value ***;
   put value +x c +x @ ;
   h_columnIndex = columnIndex;
 

   if last.rowIndex then do;
      ***write out extra delimiters for missing cell values ***;
      if ExpandedColumnCount > h_columnIndex then do;
         do i = 1 to (ExpandedColumnCount - h_columnIndex);
             ***** put c +x @;
         end;
      end;

       put;
   end;

run;

   ***Import the above generated delimited file into a SAS table ***;
   data _null_;set _oneRecFile_;
     file  "&workpath./_sasCode1_.sas" lrecl=32767;
    if _n_ > 1 then stop;
   put "proc import datafile='&workpath./&tabName..txt'";
   put " out=&outlib..&tabName DBMS=DLM replace; delimiter='~';getNames=yes; run;" ;
   run; %include "&workpath./_sasCode1_.sas" ;run;
   

%mend;




/***********************************************
This macro reads an Excel file and loads every tab into a SAS table of the same name
************************************************/
%macro loadExcelToSAS(xmlFilename,outLib);
   *** Process all observations ***;
   %let _OBS_   = %sysfunc(getoption(OBS));
   options OBS=MAX;

   %if "&outLib"="" %then %do;
       %let outLib=work;
   %end;

   ***Assign a library name to the XML file ***;
   libname _xmlLib_ xml "&xmlFilename" xmlmap="/java/aqsWB/deloitte.map" access=readonly ;

   ***Read the list of tabs in the Excel file ***;
   data _table_;set _xmlLib_ ._table;

   ***Convert the tab names to SAS names ***;
   data _table_;set _table_;
   %_makeSASname_(name,name);
   WorksheetIndex = _n_;
   keep name WorksheetIndex ExpandedColumnCount;

   ***Read the individual cells in the Excel data ***;
   data _ExcelData_;set _xmlLib_ ._ExcelData;
   run;

   *** bring in the tab name and the number of columns in each tab ***;
   data _ExcelData_;merge _ExcelData_ _table_;
     by WorksheetIndex ; run;



   ***Correct the column index field and the column name fields  **;
   data _ExcelData_;set _ExcelData_ ;
     by WorksheetIndex rowIndex;
     retain missingColumnCount;
     drop missingColumnCount;
     if first.rowIndex then missingColumnCount=0;
     columnIndex = columnIndex + missingColumnCount;
     if altColumnIndex = . or altColumnIndex = columnIndex then do;
     end; else do;
          missingColumnCount = missingColumnCount + altColumnIndex - columnIndex;
          **Correct the columnIndex to account for missing cells in the prior columns ***;
          columnIndex = altColumnIndex ;
     end;

     if rowIndex = 1 then do;
       ***make the column headings to be SAS names ***;
       %_makeSASname_(value,value);
     end;

     keep  WorksheetIndex name ExpandedColumnCount cellIndex rowIndex columnIndex type value; 
     if missing(value) then delete;


   **Create SAS code to load one tab at a time to a SAS table ***;
   data _null_;set _table_;
     file  "&workpath./_sasCode2_.sas" ;
   put '%_loadThisTab_(' "&outlib," name   ')';
   run; %include "&workpath./_sasCode2_.sas" ;run;


   *** Clean up  ***;
   options OBS=&_OBS_;
   proc sql;drop table _table_;
   proc sql;drop table _ExcelData_;


%mend;












/**********************************************************************************
This macro converts a SAS table into if..then.. SAS code.
This SAS code translates one or more input variables into one or more output variables. 

***********************************************************************************/
%macro crtSAScodeFromTable(inlib,infile,outMacroNm,testMacroNm,startRecCnt=2,firstRecShowsDefaults=1,outSAScode=);

    proc contents data=&inlib..&infile 
     out=_varList_ (keep=name type length )
     noprint memtype=all;

   ***Separate range variables, input variables and output variables into 3 datasets ***;
   data _fromList_ _toList_ _inList_ _outList_ ;set _varList_;
    format upCaseName $32.;
    if missing(compress(name)) then delete;

    if substr(upcase(compress(name)),1,2) = "IN" then do;
       name = substr(name,3);upCaseName=upCase(name);output _inList_;return;
    end;
    if substr(upcase(compress(name)),1,2) = "TO" then do;
       name = substr(name,3);upCaseName=upCase(name);output _toList_;return;
    end;
    if substr(upcase(compress(name)),1,3) = "OUT" then do;
       name = substr(name,4);upCaseName=upCase(name);output _outList_;return;
    end;

    if substr(upcase(compress(name)),1,4) = "FROM" then do;
       name = substr(name,5);upCaseName=upCase(name);output _fromList_;return;
    end;

   proc sort nodupkey data=_fromList_;by upCaseName;
   proc sort nodupkey data=_toList_;by upCaseName;

   data _null_;merge _fromList_ (in=in_fromList)
                          _toList_ (in=in_toList keep=upCaseName);
    by upCaseName;
   err = compress("E R R O R"); x=-1;
   if not in_fromList then do;
      put err +x ": Column from" upCaseName " not found in &infile.";
      abort 2;
   end;
   if not in_toList then do;
      put err +x ": Column to" upCaseName" not found in &infile.";
      abort 2;
   end;


    ****Make a list of range variables in a macro array ****;
    %let rangeVarListsize=0;
    data _null_;set _fromList_ end=lastobs;
    file "&workPath./_temp_.sas" lrecl=32767 ;
    if _n_ = 1 then do;
       put '%let i=0;';
    end;
     put '%let i = %eval(&i+1);%let rangeVar&i=' name  ';%let rangeVarType&i=' type ';'; 
     if lastobs then do;
        put '%let rangeVarListsize=&i;';
     end;
    run; %include "&workPath./_temp_.sas" ; run; 


    ****Make a list of input variables in a macro array ****;
    %let inVarListsize=0;
    data _null_;set _inList_ end=lastobs;
    file "&workPath./_temp_.sas" lrecl=32767 ;
    if _n_ = 1 then do;
       put '%let i=0;';
    end;
     put '%let i = %eval(&i+1);%let inVar&i=' name  ';%let inVarType&i=' type ';'; 
     if lastobs then do;
        put '%let inVarListsize=&i;';
     end;
    run; %include "&workPath./_temp_.sas" ; run; 

    ****Make a list of output variables in a macro array ****;
    %let outVarListsize=0;
    data _null_;set _outList_ end=lastobs;
    file "&workPath./_temp_.sas" lrecl=32767 ;
    if _n_ = 1 then do;
       put '%let i=0;';
    end;
     put '%let i = %eval(&i+1);%let outVar&i=' name  ';%let outVarType&i=' type ';'; 
     if lastobs then do;
        put '%let outVarListsize=&i;';
     end;
    run; %include "&workPath./_temp_.sas" ; run; 

    %if &outVarListsize = 0 %then %do;
      %put Warning: No output variables found in &infile;
      %goto endOfMacro;
    %end;

    %if %eval(&inVarListsize+&rangeVarListsize) = 0 %then %do;
      %put Warning: No input variables found in &infile ; 
      %goto endOfMacro;
    %end;

    ****Now starting writing the if..then.. SAS code *****;
    data _null_;set &inlib..&infile  end=lastobs;
    file "&workPath./_temp_.sas" lrecl=32767 ;

     x = -1;  recId = _n_ + &startRecCnt - 1;


    if _n_ = 1 then do;
       put '%macro ' "&outMacroNm;";
       put '   RecId = 0;';
       %if "&firstRecShowsDefaults" = "1" %then %do;
          %do i = 1 %to &outVarListSize;
             if NOT missing(out&&outVar&i) then do;
                if &&outVarType&i = 2 then do;
                   put "      &&outVar&i = "    '"'    out&&outVar&i  +x   '";' ;
                end; else do;
                   put "      &&outVar&i = "           out&&outVar&i  +x   ' ;'    ;
                end;
              end;
          %end;
          return;
       %end;
    end;

    put '      if 1 = 1 ' ; 


    %do i = 1 %to &rangeVarListSize;
       if missing(from&&rangeVar&i) or missing(to&&rangeVar&i) then do;
       end; else do;
          if &&rangeVarType&i = 2 then do;
             put "         and &&rangeVar&i >= "    '"'    from&&rangeVar&i  +x   '"' ;
             put "         and &&rangeVar&i <= "    '"'    to&&rangeVar&i    +x   '"' ;
          end; else do;
             put "         and &&rangeVar&i >= "           from&&rangeVar&i  +x       ;
             put "         and &&rangeVar&i <= "           to&&rangeVar&i    +x       ;
          end;
        end;
    %end;


    %do i = 1 %to &inVarListsize;
       if NOT missing(in&&inVar&i) then do;
          if &&inVarType&i = 2 then do;
             put "         and &&inVar&i = "    '"'    in&&inVar&i  +x   '"' ;
          end; else do;
             put "         and &&inVar&i = "           in&&inVar&i  +x       ;
          end;
        end;
    %end;

    put '      then do;';
    put '         RecId = ' recId ';';

    %do i = 1 %to &outVarListSize;
       if NOT missing(out&&outVar&i) then do;
          if &&outVarType&i = 2 then do;
             put "         &&outVar&i = "    '"'    out&&outVar&i  +x   '";' ;
          end; else do;
             put "         &&outVar&i = "           out&&outVar&i  +x   ' ;'    ;
          end;
        end;
    %end;




    put '      end;';

    if lastobs then do;
        put '%mend;';
    end;

    run; 

   %if "&outSAScode" ne "" %then %do;
        data _null_;
        infile "&workPath./_temp_.sas" lrecl=32767 truncover;
        file "&outSAScode"  mod lrecl=32767 ;
        input @1 fullRec $500. ;
        put fullRec;
        run;
   %end; %else %do;
        %include "&workPath./_temp_.sas" ; run; 
   %end;

  
   %if "&testMacroNm" = "" %then %goto endOfMacro;


    *** Now create the test macro to test the above macro****;
    data _null_;set _oneRecFile_  end=lastobs;
    file "&workPath./_temp_.sas" lrecl=32767 ;
    x = -1;  
    put '%macro  ' "&testMacroNm;";
    put "      data _temp_;set &inlib..&infile;";
    put "      origRecId = _n_  + &startRecCnt - 1;";

    %do i = 1 %to &rangeVarListSize;
        put "      &&rangeVar&i = from&&rangeVar&i;" ;
    %end;
    %do i = 1 %to &inVarListsize;
        put "      &&inVar&i = in&&inVar&i;" ;
    %end;

    put '      %' "&outMacroNm"  ';';
    put '      errFlag="  ";';
    put '      if recId ne origRecId then errFlag = "*";';

    %do i = 1 %to &rangeVarListSize;
        put "      if to&&rangeVar&i <= from&&rangeVar&i and not missing(to&&rangeVar&i)then errFlag='**';" ;
    %end;
    put '      proc print data=_temp_;';
    put '        id origRecid;';  
    put '%mend;';
    run; 

   %if "&outSAScode" ne "" %then %do;
        data _null_;
        infile "&workPath./_temp_.sas" lrecl=32767 truncover;
        file "&outSAScode"  mod lrecl=32767 ;
        input @1 fullRec $500. ;
        put fullRec;
        run;
   %end; %else %do;
        %include "&workPath./_temp_.sas" ; run; 
   %end;
   

%endOfMacro:
     

%mend;





/**********************************************************************************
This macro converts a SAS table into if..then.. SAS code.
This SAS code translates one or more input variables into one or more output variables. 

***********************************************************************************/
%macro crtSAScodeFromExcel(xmlFilename,testCode=0,outSAScode=);
    %SYSEXEC mkdir &workpath./xml; 

    libname _XLIB_  "&workpath./xml" ;
    %loadExcelToSAS(&xmlFilename,_XLIB_)

    %if "&outSAScode" ne "" %then %do;
          data _null_;set _null_;
          file "&outSAScode" lrecl=32767 ;  run;
    %end;


     proc contents data=_XLIB_._all_ noprint out=_temp_;

    proc sort data=_temp_ (keep=libname memname) nodupkey;by libname memname;

    data _null_;set _temp_;
    file "&workPath./_crtSAScodeFromExcel_.sas" lrecl=32767 ;
    put '%crtSAScodeFromTable(inlib=_XLIB_,infile=' memname 
        ',outMacroNm=get' memname ',testMacroNm=test' memname ",outSAScode=&outSasCode"   ');';
     run; %include "&workPath./_crtSAScodeFromExcel_.sas" ; run; 

    %if "&outSAScode" ne "" %then %do;
         %include "&outSAScode";
    %end;


    %if "&testCode" = "1" %then %do;
       data _null_;set _temp_;
       file "&workPath./_crtSAScodeFromExcel2_.sas" lrecl=32767 ;
       put '%test' memname ;
        run; %include "&workPath./_crtSAScodeFromExcel2_.sas" ; run; 
    %end;

    proc sql;drop table _temp_;
    libname _XLIB_;
    %SYSEXEC rm -r &workpath./xml; 
%mend;



/*******************************************************************************
This is an internal macro used summarize quickly in memory.
This macro can be used to create different summaries in one pass of the data.
This macro uses hash tables and is very memory intensive.
Please do not use this macro in your programs without consulting with RK
********************************************************************************/
%macro _accumulateInArray_(sumBy=zip, lastKey=, summaryVars=, prefix=, arraySize=) ;
   %local _firstRec_ _lastRec_;

   %if "&arraySize" = "" %then %do;
       %let arraySize=20;
   %end;

   %if "&lastKey" = "" %then %do;
      %let _firstRec_ =%str(_n_=1);
      %let _lastRec_ =%str(lastobs);
   %end;%else %do;
      %let _firstRec_ =%str(first.&lastKey);
      %let _lastRec_ =%str(last.&lastKey);
   %end;

   %if "&prefix" = "" %then %do;
      %let prefix =&sumBy;
   %end;

   if &_firstRec_ then do;
         &prefix.__ERR =0;&prefix.__M=0;&prefix.__MAX = &sumby; &prefix.__MIN = &sumby;

         retain &prefix.__ERR &prefix.__M &prefix.__MAX &prefix.__MIN ;
         call missing(&prefix.__MAX , &prefix.__MIN ); 
   end;

   if missing(&prefix.__MAX) or &prefix.__MAX < &sumby then do;
      &prefix.__MAX = &sumby; 
   end;
   if missing(&prefix.__MIN) or &prefix.__MIN > &sumby then do;
      &prefix.__MIN = &sumby; 
   end;

   if missing(&sumby) then &prefix.__M = 1;

   %if "&summaryVars" eq "" %then %do;
       %goto endOfMacro;
   %end;

   %let _numVars_=0;
   %let _numVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
   if _n_ = 1 then do; 
      declare hash &prefix.__hash; 
      declare hiter &prefix.__hiter;
      &prefix.__hash = _new_ hash(hashexp:1);
      &prefix.__hiter = _new_ hiter("&prefix.__hash");
   end;

   if &_firstRec_ then do;
      &prefix.__copy = &sumby;
      &prefix.__hash.delete();
      &prefix.__hash = _new_ hash(hashexp:1);
      rc = &prefix.__hash.defineKey("&sumBy");
      rc = &prefix.__hash.defineData("&prefix.__copy"   

      %do _iVars_ = 1 %to &_numVars_;
           , "&prefix.__sum&_iVars_"
      %end;
      ); 
      rc = &prefix.__hash.defineDone(); 
      call missing(&prefix.__copy
      %do _iVars_ = 1 %to &_numVars_;
           , &prefix.__sum&_iVars_
      %end;
       );
    end;

   use_other=0;drop use_other;
   rc = &prefix.__hash.find();
   if rc ne 0 then do;
      if &prefix.__hash.num_items > &arraySize then do;
         &prefix.__ERR =1;use_other=1;
      end; else do; 
         &prefix.__copy = &sumby;
         rc = &prefix.__hash.add();
      end;
   end;

   %do _iVars_ = 1 %to &_numVars_;
       %let _thisVar_ = %scan( &summaryVars, &_iVars_); 
       &prefix.__sum&_iVars_ = sum(&prefix.__sum&_iVars_ , &_thisVar_ );
   %end;

   if not use_other then do;
      &prefix.__copy = &sumby;
      rc = &prefix.__hash.replace();
   end;


   if &_lastRec_ then do;
      &prefix.__PREV2=.;drop &prefix.__PREV2;
      &prefix.__PREV = &sumby; call missing(&prefix.__PREV);
      &prefix.__CNT=&prefix.__hash.num_items - &prefix.__M; 

      &prefix.__hiter.delete();
      &prefix.__hiter = _new_ hiter("&prefix.__hash");
      rc = &prefix.__hiter.first();
      do while (rc=0);
         if missing(&prefix.__PREV2) or missing(&prefix.__prev)
             or &prefix.__prev2 <  &prefix.__sum1 then do;
            &prefix.__PREV  = &prefix.__copy ;
            &prefix.__PREV2 = &prefix.__sum1 ;
         end;
         rc = &prefix.__hiter.next();
      end;
   end;

%endOfMacro:
%mend;

%macro set_current;

   %global current;
   %global current_dir;
   %global date_time;
   %global workPath;

    proc sql noprint;
       select xpath
         into : current_log_file
         separated by ' '
         from dictionary.extfiles
         where fileref='#LN00004';
    quit;
    proc sql noprint;
       select xpath
         into : current_path_file
         separated by ' '
         from dictionary.extfiles
         where fileref='#LN00006';
    quit;
    
    title;
    
    %let date_time= %sysfunc(putn(%str(%'&SYSDATE.%')D,WEEKDATE30.)) &SYSTIME;

    %if "&current_log_file" ne "/dev/tty/" %then %do;
       %let current=%sysfunc(trim(%SUBSTR(&SYSPROCESSNAME,8)));
       filename xxx pipe "dirname &current_log_file.";       
       data test;
          length current_dir $200 ;
          infile xxx length=l;
          input @;
          input @1 current_dir $varying200. l;
          call symput ("current_dir", compress(current_dir)||"/");
       run;
    %end;      
    %else %let current=%trim(%SUBSTR(&SYSPROCESSNAME,8));
    
    %let current=%qscan(&current,1,%str(.));
    %let workPath = %sysfunc(pathname(work)) ; 

%mend set_current;

/*******************************************************************************
This macro will create delimited text file given a dataset

Macro Parameters
   indsn= input dataset name
   outfile= output text file name,
   outtype= possible options are CSV and XLS,
   dlm=The delimiter,
   where= Subsetting Condition,
   keep= Which variables to keep. The column order in the output statement will 
         be the same as the column order in this statement,                
   drop=Whe variables to drop,
   leadingDlm=Indicates whether there will be a leading delimeter (Y,N),
   gzip=Indicates if the output file will be zipped (Y,N)
********************************************************************************/

%macro out_dlm (indsn=, 
                outfile=,
                outtype=,
                dlm=@,
                where=1,
                keep=,                
                drop=,
                leadingDlm=n,
                gzip=N);
                
   %if %upcase(&outtype)=CSV %then %let dlm=%str(,);;    
   
   data _temp_; 
      %if "&keep" ne "" %then retain &keep;;
      set &indsn (where=(0)); 
   run;
      
   proc contents data=_temp_(keep=&keep drop=&drop) varnum 
                 out=vardesc (keep=Name Label varnum) noprint;                 
   run;        
           
   proc sort data=vardesc;
      by varnum; 
   run; 
        
   data vardesc;
      length label $50.;   
      set vardesc;
      if label = "" then label = Name;
   run;

   proc sql noprint;
   select trim(label)
     into :label
     separated by "&dlm."
     from vardesc;       
   quit;

   %if %upcase(&leadingDlm) ne Y %then %let FILENAME = &workpath./_temp_.txt;
      %else %let FILENAME = &outfile.;

   FILENAME dat "&FILENAME" lrecl=32767;
   
   DATA _null_;
      FILE dat;     
      if _n_ = 1 then do;
         %if %upcase(&outtype)=XLS %then put "sep=&dlm." %str(;);
         put "&dlm.&label.";
      end;   
      %if "&keep" ne "" %then retain &keep;;
      SET &indsn. (keep=&keep drop=&drop where=(&where));
      format _all_;
      PUT (_all_) ("&dlm.");
   RUN;

  %set_current;   
  %if %upcase(&leadingDlm) ne Y %then %do;
    x "cd &current_dir.;sed -e 's/^&dlm.//g' &workpath./_temp_.txt > &outfile.;
       rm &workpath./_temp_.txt";
  %end;
   
  %if %upcase(&gzip)=Y %then %do;
      x "cd &current_dir.; 
         rm -f &outfile..gz;
         gzip &outfile";
  %end;
%mend out_dlm ;

/*******************************************************************************
This macro start creating a pdf file
******************************************************************************/
%macro pdf (style=sasweb,
            orientation=landscape,
            pdfout=);
   
   libname tools "/home/zshams/tools/";         
   ods path tools.tmplmst(read) sashelp.tmplmst(read);
   
   %if &pdfout = %then %do; 
      %set_current;
      %let pdfout=&current..pdf;
   %end; 
     
   ods pdf close;
   options orientation=&orientation;
   %if %upcase(&orientation)=PORTRAIT %then %do; 
      options LS=80; 
   %end;   
   %else %do; 
      options linesize=120 pagesize=60;
   %end;    
       
   ods pdf file="&pdfout." style=&style;
   
%mend pdf;   
   
/*******************************************************************************
This macro start creating an XLS file
******************************************************************************/   
%macro xls (style=xlsasweb,
            xlsout=);
   
   libname tools "/home/zshams/tools/";         
   ods path tools.tmplmst(read) sashelp.tmplmst(read);
   
   %if "&xlsout" = "" %then %do; 
      %set_current;
      %let xlsout=&current..xls;
   %end; 
   
   ods tagsets.ExcelXP file="&xlsout." style=&style;
   
%mend;

%macro zipIt(files=&current..xls, 
             zipfile=&current..zip, 
             zip=Y,
             mail=n);
   
   %set_current;          
   
   ods _all_ close;

      %if %upcase(&zip)=Y %then %do;
          x "cd &current_dir.; 
             rm -f &zipfile..gz;
             zip -q &zipfile &files";
          %let outfile = &zipfile;
      %end;
      %else %let outfile = &files;
      %if %upcase(&mail)=Yj %then %do; 
         x "cd &current_dir.; 
            uuencode &outfile &outfile | mailx -s %str('&outfile.') $user@deloitte.com";
      %end;      
   
%mend;

%macro  printToWorkBook (toPrintDSN=,
                         cellsPerSheet=20000);

   %let count_files = %eval(1 + %length(%cmpres(&toPrintDSN)) - 
                             %length(%sysfunc(compress(&toPrintDSN))));
   

   %do i = 1 %to &count_files; 
      %let sheet_name = %qscan(&toPrintDSN,&i,%str( ));
      proc contents data=&SHEET_NAME out=vars noprint; 
      run;
      proc sql noprint;
         select count(*) into: count_fields separated by ' ' from vars;
      quit;    
      %let num_rows = %sysevalf(&cellsPerSheet / &count_fields,ceil);
      ODS TAGSETS.EXCELXP OPTIONS(SHEET_NAME="&sheet_name.");
      proc print data=&sheet_name.(obs=&num_rows) noobs;
      run;       
   %end;   

    
%mend printToWorkBook;

%macro sheetName(sheet_name);
   ODS TAGSETS.EXCELXP OPTIONS(SHEET_NAME="&sheet_name.");
%mend;   







/*******************************************************************************
This macro creates a PROC frequency of all variables in a given SAS dataset. 

This macro uses hash tables and is very memory intensive.

Parameters: 
       inLib: the SAS library where the input dataset is stored. Defautl value is WORK.
	infile: the SAS dataset from which a format needs to be created
	appendToSASfile: The output SAS dataset to which the frequency report should be appended to 
                        Default value is &infile._freqRpt
	replace: When set to 1, this report will replace whatever is in the output SAS file  
                Otherwise, this rpeort will get appended to output SAS file  
                Default value is 0

The frequency report is written to a SAS file with the following columns:
     sourceFile varName varValue varFreq


Example:

%crtFreqRpt(inlib=RAW,infile=alstmf);
The above line of code produces a frequency report of all the variables in file RAW.alstmf. 
This report is written to a SAS dataset called alstmf_freqRpt. 

%crtFreqRpt(inlib=RAW,infile=alstmf,appendToSASfile=myFile);
%crtFreqRpt(inlib=RAW,infile=alcovr,appendToSASfile=myFile);
The above two lines of code will produce frequency reports on all fields in RAW.alstmf & RAW.alcovr tables.
Both the reports are appended to the same myFile SAS dataset

%crtFreqRpt(inlib=RAW,infile=alcovr,appendToSASfile=myFile,replace=1);
The above line of code produces the frequency report on RAW.alcovr file but instead of appending the report to the otput file, it replaces it. 

********************************************************************************/
%macro crtFreqReport( inlib=WORK, inFile=, appendToSASfile=,replace=0) ;
   %let workPath = %sysfunc(pathname(work));
   %if "&inlib" = "" %then %let inlib=WORK;
   %if "&infile" = "" %then  %goto endOfMacro;
   %if "&appendToSASfile" = "" %then %do; %let appendToSASfile=&infile._freqRpt; %let replace=1; %end;

   proc contents data=&inlib..&inFile out=_dict_ 
        (keep=name type length)
        noprint memtype=all;

  data _dict_;set _dict_ (rename=(name=varName type=varType length=varLen));
       varName = upcase(varName);
       if varType = 2 and varLen > 50 then delete; /***delete long character fields ***/
       if length(compress(varName)) > 29 then delete;  /***delete variable names that are longer than 29 bytes ***/

    
    data _null_; set _dict_  end=lastobs;
     file  "&workpath./_qcCode_.sas" notitle linesize=250 pagesize=500;
     if _n_ = 1 then do;
       put "data _freqRpt_;set &inLib..&inFile end=lastobs;" ; 
       put 'format _sourceFile varName varValue $45.;keep _sourceFile varName varValue varFreq varFreqPerc;';
       put "_sourceFile=UPCASE('&infile');";
     end;
     put '%_getFrequency_(sumBy=' varName ' );';  

     run;%include "&workpath./_qcCode_.sas"; run; 
     
    %if "&replace" = "1" %then %do;
        proc sql;drop table &appendToSASfile;quit;run;
    %end;
    proc append base=&appendToSASfile data=_freqRpt_  (rename=(_sourceFile=sourceFile));  

%endOfMacro:
%mend;




/*******************************************************************************
This is an internal macro used to create FREQUENCY of all variables in a dataset.
This macro can be used to create different summaries in one pass of the data.
This macro uses hash tables and is very memory intensive.
********************************************************************************/
%macro _getFrequency_(sumBy) ;
   if _n_ = 1 then do; 
      &sumBy.__M=0;retain &sumBy.__M; /*** __M keeps missing count **/ 
      &sumBy.__V=0;retain &sumBy.__V; /*** __V keeps non-missing count **/ 
      &sumBy.__E=0;retain &sumBy.__E; /** __E is the error flag for the variable ***/

      declare hash &sumBy.__H;  /** __H is the hash table for the variable **/
      declare hiter &sumBy.__T;  /** __T is the iterator for the variable **/
      &sumBy.__H = _new_ hash(hashexp:1);
      &sumBy.__T = _new_ hiter("&sumBy.__H");

      &sumBy.__C = &sumby;  /** __C keeps a copy of the variable ***/
      rc = &sumBy.__H.defineKey("&sumBy");
      rc = &sumBy.__H.defineData("&sumBy.__C" , "&sumBy.__N"      ); /** __N is the frequency counter **/
      rc = &sumBy.__H.defineDone(); 
      call missing(&sumBy.__C, &sumBy.__N);
    end;

   if missing(&sumby) then &sumBy.__M = &sumBy.__M + 1;
   if not missing(&sumby) then &sumBy.__V = &sumBy.__V + 1;

   use_other=0;drop use_other;
   rc = &sumBy.__H.find();
   if rc ne 0 then do;
      if &sumBy.__H.num_items > 250 then do;
         use_other=1; &sumBy.__E=1;
      end; else do; 
         &sumBy.__C = &sumby;
         rc = &sumBy.__H.add();
      end;
   end;

   &sumBy.__N = sum(&sumBy.__N , 1 );


   if not use_other then do;
      &sumBy.__C = &sumby;
      rc = &sumBy.__H.replace();
   end;


   if lastobs then do;
      varName = UPCASE("&sumBy");

      if &sumBy.__M > 0 then do;
         varValue = "Missing";varfreq=&sumBy.__M;varFreqPerc=&sumBy.__M/_n_; output;
      end;

      if &sumBy.__E ne 0 then do;
         varValue = "nonMissing";varfreq=&sumBy.__V;varFreqPerc=&sumBy.__V/_n_;output;
      end;

      &sumBy.__T = _new_ hiter("&sumBy.__H");
      rc = &sumBy.__T.first(); iterCount_E=0;
      
      do while (rc=0 and iterCount_E < 25);
         if &sumBy.__E = 1 then iterCount_E = iterCount_E + 1;
         varValue = &sumBy.__C; 
         varValue = compress(varValue);
         varFreq = &sumBy.__N ; varFreqPerc=&sumBy.__N/_n_;
         if not missing(varValue) then output ; 
         rc = &sumBy.__T.next();
      end;
   end;

%endOfMacro:
%mend;




/******************************************************************************
This macro can merge a master file with a details file and create summarize the detail records to the master level.  

Input files: MasterFile (&masterFile) and DetailFile (&detailFile) 
             
Output file:  Summarized file called &outlib..&outfile
              The summarized file is at the level specified by &summaryBy

    
Description of Parameters:
masterFile : The name of the SAS file which needs to be summarized
              If not specified, the masterFile will be same as the detailed file

summaryBy  : The summary will be produced at the level specified by variables given here

detailFile : The name of the SAS file which needs to be merged with the master file
              If not specified, the detailFile will be same as the masterFile

mergeBy    : List of key fields by which the master file needs to be merged with the detailFile
              If not specified, the mergeBy will be same as summaryBy
additionalWhereClause: Any Where clause that needs to be added in addition to the mergeBy fields given above
outlib   : the library to which the output summary needs to be written to
outfile  : the name of the output summarized SAS file 

MINfieldList: list of fields in the detailed file which will be summarized based on MIN aggregation 
MAXfieldList: list of fields in the detailed file which will be summarized based on MAX aggregation 
SUMfieldList: list of fields in the detailed file which will be summarized based on SUM aggregation 
IDfieldList: list of fields in the detailed file which will be summarized as ID fields 
codeAtStart: The name of the SAS code that needs to be run at the start of the summary process
codeAtEnd: Then name of the SAS code that needs to be run at the end of the summary process      



Brief summary of how the macro works:
  - A master key list will first be prepared by doing a PROC SORT NODUPKEY of the masterFile ( by &summaryBy )
	If &summaryBy is not given, the &masterFile will be used as-is
  - The master key list will be merged with the detailed file by the mergeBy keys. This could be a many-to-many merge. 
  - At this point, the merged dataset has multiple rows per target key
  - The code that is given in &codeAtStart is run on this dataset
  - The merged dataset will be summarized to the target key level. Each field will be summarized using
     MIN, MAX, SUM or ID aggregation methods. 
  - The code that is given in &codeAtEnd is run on this summary dataset


Example code: 

The full signature for this macro is 
       %crtSummary(masterFile=,summaryBy=, detailFile=, mergeBy=, outLib=, outFile=, 
       MINfieldList=, MAXfieldList=, SUMfieldList=, IDfieldList=,codeAtstart=,codeAtEnd= );

But, you do not need pass all the parameters. 

Sample code to rollup premium to a policy term level:
       %crtSummary(masterFile=work.policyFile,summaryBy=policyNumber termExpirationDate, 
                    outLib=WORK, outFile=premFile, 
                    SUMfieldList=policyWrittenPremium );

Bring in some additional information along with premium: 
       %crtSummary(masterFile=work.policyFile,summaryBy=policyNumber termExpirationDate, 
                    outLib=WORK, outFile=premFile, 
                    SUMfieldList=policyWrittenPremium 
                    IDfieldList=termEffectiveDate policyYear 
                    MAXfieldList=generalAgent policyState);
                      		
Bring in the most recent agent and policyState information:
       %crtSummary(masterFile=work.policyFile,summaryBy=policyNumber termExpirationDate, 
                    outLib=WORK, outFile=premFile, 
                    SUMfieldList=policyWrittenPremium 
                    IDfieldList=termEffectiveDate policyYear 
                    MAXfieldList=generalAgent:accountingDate policyState:accountingDate);

Rolling up losses:
       %crtSummary(masterFile=work.lossFile,summaryBy=policyNumber termExpirationDate ClaimNumber, 
                    outLib=WORK, outFile=lossFile, 
                    SUMfieldList=paidLossAmount ReseveAmount 
                    IDfieldList=accidentDate 
                    );

Rolling up losses as of 20071231:
       %crtSummary(masterFile=work.lossFile,summaryBy=policyNumber termExpirationDate ClaimNumber, 
                    outLib=WORK, outFile=lossFile, 
                    SUMfieldList=paidLossAmount ReseveAmount 
                    IDfieldList=accidentDate, additionalWhereClause=%str(accountingDate <= "20071231")
                    );

Merging lossFle and premiumFile:
       %crtSummary(masterFile=work.premFile,summaryBy=policyNumber termExpirationDate, 
                    detailFile=lossFile, mergeBy=policyNumber termExpirationDate,
                    outLib=WORK, outFile=premLossFile, 
                    SUMfieldList=paidLossAmount ReseveAmount 
                    );



********************************************************************************/
%macro crtSummary(masterFile=,summaryBy=, detailFile=, mergeBy=, additionalWhereClause=, outLib=WORK, outFile=temp, 
       MINfieldList=, MAXfieldList=, SUMfieldList=, IDfieldList=, CNTfieldList=,codeAtstart=,codeAtEnd=, splitQueryBy= );

     

   %if "&splitQueryBy" ne "" %then %do;
       %_crtSummaryInSplits_(masterFile=&masterFile,summaryBy=&summaryBy, detailFile=&detailFile, mergeBy=&mergeBy, 
            additionalWhereClause=&additionalWhereClause, outLib=&outLib, outFile=&outFile, 
       MINfieldList=&MINfieldList, MAXfieldList=&MAXfieldList, SUMfieldList=&SUMfieldList, 
       IDfieldList=&IDfieldList, CNTfieldList=&CNTfieldList,codeAtstart=&codeAtstart,codeAtEnd=&codeAtEnd, splitQueryBy=&splitQueryBy );    
      %goto endOfMacro;
   %end;


       %****Set up defaults ****;
       %IF "&detailFile" = "" %THEN %DO; %LET detailFile=&masterFile; %END;
       %IF "&mergeBy" = "" %THEN %DO; %LET mergeBy=&summaryBy; %END;

       %IF "&summaryBy" = "" %THEN %DO; %LET summaryBy=_RECSEQNO_; %END;

       %LET _simpleVars_ =&MINfieldList &MAXfieldList &SUMfieldList &IDfieldList &CNTfieldList;



       %LET _firstFieldList_=;%LET _secondFieldList_=;
       %LET _numOfvars_ = %eval(1 + %length(%cmpres(&_simpleVars_))
            - %length(%sysfunc(compress(&_simpleVars_)));
       %DO _vTS_ = 1 %TO &_numOfvars_ ;
             %LET _thisVar_ = %scan(&_simpleVars_, &_vTS_);
             %LET _firstFieldList_  = &_firstFieldList_   %scan(&_thisVar_,1,:) ; 
             %LET _secondFieldList_ = &_secondFieldList_  %scan(&_thisVar_,2,:) ;
         %END;


       %**Validate parameters ***;
       %IF "&masterFile" = ""  %THEN %DO;
           %_putErrorMessage_(msg=Macro parameter MasterFile has to be specified);
           %goto endOfMacro;
       %END;  


       %_chkDupes_(varList=&summaryBy &_firstFieldList_)
       %IF "&_dupes_" ne "" %THEN %DO; 
          %_putErrorMessage_(msg=Macro parameters summaryBy MINfieldList MAXfieldList SUMfieldList IDfieldList CNTfieldList cannot have duplicate variable names like &_dupes_);
          %goto endOfMacro;
       %END;

       %_chkDupes_(varList=&mergeBy &_firstFieldList_)
       %IF "&_dupes_" ne "" %THEN %DO; 
          %_putErrorMessage_(msg=Macro parameters mergeBy MINfieldList MAXfieldList SUMfieldList IDfieldList CNTfieldList cannot have duplicate variable names like &_dupes_);
          %goto endOfMacro;
       %END;

       %IF "&mergeBy" = "" %THEN %DO;
           %_putErrorMessage_(msg=Macro parameter MergeBy has to be specified );
           %goto endOfMacro;
       %END;

       %IF "&_simpleVars_" = "" %THEN %DO;
           %_putErrorMessage_(msg=At least one aggregate field must be specified in MIN, MAX,SUM, ID, CNT field list );
           %goto endOfMacro;
       %END;


       %_removeDupes_(varList=&_firstFieldList_ &_secondFieldList_, returnList=_simpleVars_ );
       

      %IF "&mergeBy" = "&summaryBy" and "&masterFile" = "&detailFile"  %THEN %DO;
           proc sort data=&masterFile (keep=&mergeBy &_firstFieldList_ &_secondFieldList_ ) out =&outlib..&outfile;by &mergeBy; run;
           %goto startSummary;
      %END; 




      %IF "&summaryBy" = "" or "&summaryBy" = "_RECSEQNO_" %THEN %DO;
          data _masterFile_;set &masterFile;_RECSEQNO_ = _N_;
      %END; %ELSE %DO;
          proc sort data=&masterFile nodupkey out=_masterFile_;
            by &summaryBy; 
      %END;
   

      %***Create an index on mergeBy variables ***;
      %LET _numOfvars_ = %eval(1 + %length(%cmpres(&mergeBy))
                - %length(%sysfunc(compress(&mergeBy)));
      proc sql noprint noerrorstop;
      create index
      %IF &_numOfVars_ = 1 %THEN %DO;
          &mergeBy 
      %END;%ELSE %DO;
          DTL_mergeBy_index
      %END;
      on &detailFile (
         %LET _thisVar_ = %scan(&mergeBy, 1);
         &_thisVar_
         %DO _vTS_ = 2 %TO &_numOfvars_ ;
             %LET _thisVar_ = %scan(&mergeBy, &_vTS_);
             , &_thisVar_  
         %END;
      ) ; quit;


     %****SQL statement to merge mster and detail file ****;
     PROC SQL noprint SORTMSG THREADS  ;create table &outlib..&outfile as
        SELECT MSTR.*  
               %LET _numOfvars_ = %eval(1 + %length(%cmpres(&_simpleVars_))
                - %length(%sysfunc(compress(&_simpleVars_)));
               %DO _vTS_ = 1 %TO &_numOfvars_ ;
                    %LET _thisVar_ = %scan(&_simpleVars_, &_vTS_);%LET _thisVar_ = %scan(&_thisVar_,1,:); 
                    , DTL.&_thisVar_  
               %END;
         FROM _masterFile_ as MSTR LEFT JOIN &detailFile as DTL
         ON
         %LET _numOfvars_ = %eval(1 + %length(%cmpres(&mergeBy))
                - %length(%sysfunc(compress(&mergeBy)));
         %LET _thisVar_ = %scan(&mergeBy, 1);
         MSTR.&_thisVar_ = DTL.&_thisVar_      

         %DO _vTS_ = 2 %TO &_numOfvars_ ;
             %LET _thisVar_ = %scan(&mergeBy, &_vTS_);
             AND MSTR.&_thisVar_ = DTL.&_thisVar_  
         %END;

         %IF "&additionalWhereClause" ne "" %THEN %DO;
              AND &additionalWhereClause
         %END;


         ORDER BY
         %LET _numOfvars_ = %eval(1 + %length(%cmpres(&summaryBy))
                     - %length(%sysfunc(compress(&summaryBy)));
         %LET _thisVar_ = %scan(&summaryBy, 1);
         MSTR.&_thisVar_
         %DO _vTS_ = 2 %TO &_numOfvars_ ;
            %LET _thisVar_ = %scan(&summaryBy, &_vTS_);
           , MSTR.&_thisVar_  
         %END;
      ; quit;   


     %IF "&codeAtStart" ne "" %THEN %DO;
          data &outlib..&outfile;set &outlib..&outfile;by &summaryBy;
          %&codeAtStart ;
     %END;

%startSummary:

      %LET _numOfvars_ = %eval(1 + %length(%cmpres(&summaryBy))
              - %length(%sysfunc(compress(&summaryBy)));
      %LET _lastKey_ = %scan(&summaryBy, &_numOfvars_);

     data &outlib..&outfile;set &outlib..&outfile;by &summaryBy;

     %_crtSummaryCode_(summary=ID,fieldList=&IDfieldList,lastKey=&_lastKey_)
     %_crtSummaryCode_(summary=MIN,fieldList=&MINfieldList,lastKey=&_lastKey_)
     %_crtSummaryCode_(summary=MAX,fieldList=&MAXfieldList,lastKey=&_lastKey_)
     %_crtSummaryCode_(summary=SUM,fieldList=&SUMfieldList,lastKey=&_lastKey_)
     %_crtSummaryCode_(summary=CNT,fieldList=&CNTfieldList,lastKey=&_lastKey_)

     if last.&_lastKey_ then do;
        %IF "&codeAtEnd" ne "" %THEN %DO; %&codeAtEnd ;   %END;
        output;
     end;



%endOfMacro:
%mend;



/*******************************************************************************
This is an internal macro used by crtSummary. 

This macro creates the SAS code to calculate aggregation like SUM, MIN, MAX, CNT etc.
*********************************************************************************/

%macro _crtSummaryCode_(summary=,fieldList=,lastKey=);
    %IF "&summary" = "" OR "&fieldList" = "" OR "&lastKey" = ""  OR "&summary" = "ID" %THEN %DO;
        %goto endOfMacro;
    %END; 

    /** parse the variables into two lists ***/
    %local _fldList1_;%local _fldList2_;
    %LET _numOfvars_ = %eval(1 + %length(%cmpres(&fieldList))
                - %length(%sysfunc(compress(&fieldList)));
    %LET _fldList1_=;%LET _fldList2_=;

    %DO _vTS_ = 1 %TO &_numOfvars_ ;
        %LET _thisVar_ = %scan(&fieldList, &_vTS_);
        %LET _thisVar1_ = %scan(&_thisVar_,1,:);  /***Get the variable before the colon**/
        %LET _thisVar2_ = %scan(&_thisVar_,2,:);  /***Get the variable after  the colon**/
        %IF "&_thisVar2_" = "" %THEN %DO; %LET _thisVar2_ = &_thisVar1_; %END;
    
        %LET _fldList1_= &_fldList1_ &_thisVar1_;
        %LET _fldList2_= &_fldList2_ &_thisVar2_;
    %END;

    %IF "&summary" = "SUM" %THEN %goto crtSUMcode;
    %IF "&summary" = "MIN" %THEN %goto crtMINcode;
    %IF "&summary" = "MAX" %THEN %goto crtMAXcode;
    %IF "&summary" = "CNT" %THEN %goto crtCNTcode;


    %goto endOfMacro;


%crtSUMcode:
    %DO _vTS_ = 1 %TO &_numOfvars_ ;
        %LET _thisVar_ = %scan(&_fldList1_, &_vTS_);
        retain &_thisVar_.__1; drop &_thisVar_.__1;
        if first.&lastKey then do;  &_thisVar_.__1=.; end;
        &_thisVar_.__1 = sum(&_thisVar_.__1, &_thisVar_ );
        if last.&lastkey then do; &_thisVar_ = &_thisVar_.__1; end; 
     %END;

    %goto endOfMacro;
%crtMINcode:
    %DO _vTS_ = 1 %TO &_numOfvars_ ;
        %LET _thisVar1_ = %scan(&_fldList1_, &_vTS_);
        %LET _thisVar2_ = %scan(&_fldList2_, &_vTS_);
        retain &_thisVar1_.__1 &_thisVar1_.__2; drop &_thisVar1_.__1 &_thisVar1_.__2;
        if first.&lastKey then do;  &_thisVar1_.__1=&_thisVar1_; &_thisVar1_.__2=&_thisVar2_;end;
        if missing(&_thisVar1_.__1) or
           missing(&_thisVar1_.__2) or
          ( not missing(&_thisVar1_ ) and
            not missing(&_thisVar2_ ) and
            &_thisVar2_ < &_thisVar1_.__2) then do;
            &_thisVar1_.__2 = &_thisVar2_;
            &_thisVar1_.__1 = &_thisVar1_;
        end;
        if last.&lastkey then do; &_thisVar1_ = &_thisVar1_.__1; end; 
     %END;



    %goto endOfMacro;

%crtMAXcode:
    %DO _vTS_ = 1 %TO &_numOfvars_ ;
        %LET _thisVar1_ = %scan(&_fldList1_, &_vTS_);
        %LET _thisVar2_ = %scan(&_fldList2_, &_vTS_);
        retain &_thisVar1_.__1 &_thisVar1_.__2; drop &_thisVar1_.__1 &_thisVar1_.__2;
        if first.&lastKey then do;  &_thisVar1_.__1=&_thisVar1_; &_thisVar1_.__2=&_thisVar2_;end;
        if missing(&_thisVar1_.__1) or
           missing(&_thisVar1_.__2) or
          ( not missing(&_thisVar1_ ) and
            not missing(&_thisVar2_ ) and
            &_thisVar2_ > &_thisVar1_.__2) then do;
            &_thisVar1_.__2 = &_thisVar2_;
            &_thisVar1_.__1 = &_thisVar1_;
        end;
        if last.&lastkey then do; &_thisVar1_ = &_thisVar1_.__1; end; 
     %END;


    %goto endOfMacro;

%crtCNTcode:


    %DO _vTS_ = 1 %TO &_numOfvars_ ;
       %LET _thisVar_ = %scan(&_fldList1_, &_vTS_);
       if _n_ = 1 then do; 
         declare hash &_thisVar_.__H;  /*** __H is the hash table for the variable ***/
         declare hiter &_thisVar_.__T; /*** __T is the iterator for the variable ***/
         &_thisVar_.__H = _new_ hash(hashexp:1);
         &_thisVar_.__T = _new_ hiter("&_thisVar_.__H");
       end;
 
       if first.&lastKey then do;
         &_thisVar_.__H.delete();
         &_thisVar_.__H = _new_ hash(hashexp:1);
         &_thisVar_.__C = &_thisVar_; /*** __C keeps a copy of the variable **/
         rc = &_thisVar_.__H.defineKey("&_thisVar_");
         rc = &_thisVar_.__H.defineData("&_thisVar_.__C" , "&_thisVar_.__C"      ); 
         rc = &_thisVar_.__H.defineDone(); 
         call missing(&_thisVar_.__C);
       end;

      &_thisVar_.__C = &_thisVar_;drop &_thisVar_ &_thisVar_.__C rc;
      rc = &_thisVar_.__H.find(); 
      if rc ne 0 and not missing(&_thisVar_.__C) then do;
         &_thisVar_.__C = &_thisVar_;
         rc = &_thisVar_.__H.add();
      end;
      if last.&lastkey then do; 
         &_thisVar_.CNT=.;  &_thisVar_.CNT = &_thisVar_.__H.num_items; 
      end;
      



   %END;
   %goto endOfMacro;

%endOfMacro:
%mend;



/************************************************************************
This is an internal macro that checks to see if there are duplicate variable names in a list.
If a duplicate is found, the macro variable _dupes_ is set to the the name of the first duplicte variable
  else it is set to blanks.
************************************************************************/
%macro _chkDupes_(varList);
    %global _dupes_;
    %LET _dupes_=;
    %IF "&varList" = "" %THEN %goto endOfMacro;
    %LET _varList_ = %upcase(&varlist);

    %LET _numOfvars_ = %eval(1 + %length(%cmpres(&_varList_ ))
                     - %length(%sysfunc(compress(&_varList_ )));


    %DO _chkI_ = 1 %TO %eval(&_numOfVars_ - 1) ; 
         %LET _varI_ = %scan(&_varList_ , &_chkI_);%LET _varI_ = %scan(&_varI_,1,:);  

        %DO _chkJ_ = %eval(&_chkI_ + 1) %TO &_numOfVars_ ; 
            %LET _varJ_ = %scan(&_varList_ , &_chkJ_);%LET _varJ_ = %scan(&_varJ_,1,:);  
            %IF "&_varI_" = "&_varJ_" %THEN %DO;
               %LET _dupes_=&_varJ_; %goto endOfMacro;
            %END;
        %END;
    %END;


%endOfMacro:
%mend;


/************************************************************************
This is an internal macro that removes duplicate entries from a variable list.
************************************************************************/
%macro _removeDupes_(varList, returnList);
    %LET workPath = %sysfunc(pathname(work));
    %IF "&varList" = "" %THEN %goto endOfMacro;
    %LET _varList_ = %upcase(&varlist);
    %LET &returnList=; 

    %LET _numOfvars_ = %eval(1 + %length(%cmpres(&_varList_ ))
                     - %length(%sysfunc(compress(&_varList_ )));

    proc sql inobs=1; create table _oneRecFile_ as select * from dictionary.options ;quit;
    data _null_; set _oneRecFile_ end=lastobs;
     file  "&workpath./_varList_.txt" notitle linesize=250 pagesize=500;
    %DO _chkI_ = 1 %TO %eval(&_numOfVars_) ; 
         %LET _varI_ = %scan(&_varList_ , &_chkI_);%LET _varI_ = %scan(&_varI_,1,:);  
         put "&_varI_  " ; 
    %END;

   data _dummy_;infile "&workpath./_varList_.txt";format A $32.;input A $; run;

    proc sql noprint;
	select distinct A into :&returnList separated by ' ' from _dummy_ ; quit;

    proc sql;drop table _dummy_;

%endOfMacro:
%mend;


%macro _crtSummaryInSplits_(masterFile=,summaryBy=, detailFile=, mergeBy=, additionalWhereClause=, outLib=, outFile=, 
       MINfieldList=, MAXfieldList=, SUMfieldList=, IDfieldList=, CNTfieldList=,codeAtstart=,codeAtEnd=, splitQueryBy= );
    
    %if "&detailFile" ne "" %then %do;
       %let detailFile=%sysfunc(upcase(&detailFile));
    %end;
    %let masterFile=%sysfunc(upcase(&masterFile));
    %IF "&detailFile" = "&masterFile" %THEN %DO; %LET detailFile=; %END;

   %LET _DataType_=;
   proc contents data=&masterFile out=_dict_ (keep=name type length) noprint memtype=all;	
   proc sql noprint;
       select type into :_DataType_ separated by ' ' from _dict_ where upcase(name) = upcase("&splitQueryBy"); quit;
   proc sql noprint;drop table _dict_; 

   %LET _splitValues_=;
   proc sql noprint;
   select distinct &splitQueryBy into :_splitValues_ separated by ' ' from &masterFile order by &splitQueryBy; quit;
   %LET _numOfValues_ = %eval(1 + %length(%cmpres(&_splitValues_))- %length(%sysfunc(compress(&_splitValues_)));
 
 
   %DO _vTSplit_ = 1 %TO &_numOfValues_ ;
      %LET _thisValue_ = %scan(&_splitValues_, &_vTSplit_);
      %if "&_DataType_" = "2" %then %do;
          data _masterFile_temp_;set &masterFile;WHERE &splitQueryBy = "&_thisValue_"; run;
      %end;%else %do;
         data _masterFile_temp_;set &masterFile;WHERE &splitQueryBy = &_thisValue_; run;
      %end; 
      %if "&detailFile" = "" %then %do;
         %crtSummary(masterFile=_masterFile_temp_,summaryBy=&summaryBy,
             detailFile=, mergeBy=&mergeBy, additionalWhereClause=&additionalWhereClause,
             outLib=WORK, outFile=_outfile_&_vTSplit_, 
             MINfieldList=&MINfieldList, MAXfieldList=&MAXfieldList, 
             SUMfieldList=&SUMfieldList, IDfieldList=&IDfieldList, CNTfieldList=&CNTfieldList,
             codeAtstart=&codeAtstart,codeAtEnd=&codeAtEnd, splitQueryBy=);
      %end;%else %do;
             %if "&_DataType_" = "2" %then %do;
                 data _detailFile_temp_;set &detailFile;WHERE &splitQueryBy = "&_thisValue_"; run;
             %end;%else %do;
                 data _detailFile_temp_;set &detailFile;WHERE &splitQueryBy = &_thisValue_; run;
             %end; 
         %crtSummary(masterFile=_masterFile_temp_,summaryBy=&summaryBy,
             detailFile=_detailFile_temp_, mergeBy=&mergeBy, additionalWhereClause=&additionalWhereClause,
             outLib=WORK, outFile=_outfile_&_vTSplit_, 
             MINfieldList=&MINfieldList, MAXfieldList=&MAXfieldList, 
             SUMfieldList=&SUMfieldList, IDfieldList=&IDfieldList, CNTfieldList=&CNTfieldList,
             codeAtstart=&codeAtstart,codeAtEnd=&codeAtEnd, splitQueryBy=);
      %end;      
      proc sql;drop table _masterFile_temp_;proc sql;drop table _detailFile_temp_; 
   %END;

   data &outLib..&outFile;set 
   %DO _vTSplit_ = 1 %TO &_numOfValues_ ;
        _outfile_&_vTSplit_
   %END;
   ; run;

   %DO _vTSplit_ = 1 %TO &_numOfValues_ ;
        proc sql;drop table _outfile_&_vTSplit_;
   %END;
%mend;



/************************************************************************
This is an internal macro that writes an error message into the log and creates a user abort of the SAS job 
************************************************************************/
%macro _putErrorMessage_(msg);
   %put %sysfunc(compress(E R R O R: ))&msg; data _null_;abort 255;
%mend;






/*=======================================================
 Macro:      checkDupes.sas

 	This macro will print a report showing total number of records and number of records that were duplicates.
	If duplciates were found, this macro will also print a sample 10 records that show the duplicate entries.
                                       
       
 Usage:

     %checkDupes(inFile=myLib.polVars,primaryKey=policyNumber ExpiryDate); 

    The above code will check to see if the SAS file myLib.polVars has unique records for every "policyNumber ExpiryDate" combination.
                                 
=========================================================*/


%macro checkDupes(inFile,primaryKey); 
   %let _numOfvars_ = %eval(1 + %length(%cmpres(&primaryKey ))
                  - %length(%sysfunc(compress(&primaryKey ))); 
   %let _lastKey_ =  %scan(&primaryKey , &_numOfvars_);

   %let _NUMOBS_=0;
   data _null_;set &infile nobs=numObs;
   if _n_ > 1 then stop;
    CALL SYMPUT('_NUMOBS_' ,PUT(NUMOBS , best16.));


   proc sort data=&infile out=_dupesFile_; by &primaryKey;
   data _dupesFile_;set _dupesFile_; by &primaryKey;
   if not first.&_lastKey_;
   if not last.&_lastKey_;

   %let _NUMDUPES_=0;
   data _null_;set _dupesFile_ nobs=numObs;
   if _n_ > 1 then stop;
    CALL SYMPUT('_NUMDUPES_' ,PUT(NUMOBS , best16.));

   title "Checking duplicate Primary Key entries in &infile ";
   data _null_;set &infile; file print;
   if _n_ > 1 then stop; 
   totObs=&_NUMOBS_;dupObs=&_NUMDUPES_;
   put;put;put;
   put "Primary key                 = &primaryKey";        put; 
   put "Total Number of records     = " totObs comma16. ;  put;
   put "Number of duplicate records = " dupObs comma16.;   put;
   put;put;put; run;title;run;   
   
   %IF %eval(&_NUMDUPES_) > 0 %THEN %DO;
        proc print data=_dupesFile_ (obs=10);
        title "Sample records from &infile that show duplicate primary keys";run;title;run;   
   %END;
   
   proc sql noprint;drop table _dupesFile_;

%mend;




/*=======================================================
 Macro:      verifyMasterDetailMatch.sas         
 purpose:      Checks to match the master file and the detail file by a specified foreign key    


       
 Usage:

     %verifyMasterDetailMatch(masterFile=myLib.policyFile,detailFile=myLib.claimFile,foreignKey=policyNumber ExpiryDate); 

    The above code will check to see if the all the cliam records map to records in the policy file. 
=========================================================*/


%macro verifyMasterDetailMatch(masterFile,detailFile,foreignKey); 
   proc sort data=&masterFile (keep=&foreignKey) out=_masterFile_;by &foreignKey;
   data _masterFile_;set _masterFile_;numOfRecords=1;
   proc summary data=_masterFile_ nway;by &foreignKey;var numOfRecords;output out=_masterFile_ sum=; 
   
   proc sort data=&detailFile (keep=&foreignKey) out=_detailFile_;by &foreignKey;   
   data _detailFile_;set _detailFile_;numOfRecords=1;
   proc summary data=_detailFile_ nway;by &foreignKey;var numOfRecords;output out=_detailFile_ sum=;    

   %let _numMasterRecords_=0;
   data _null_;set &masterFile nobs=numObs;
   if _n_ > 1 then stop;
   CALL SYMPUT('_numMasterRecords_' ,PUT(NUMOBS , best16.));  
   
   %let _numMasterKeys_=0;   
   data _null_;set _masterFile_ nobs=numObs;
   if _n_ > 1 then stop;
   CALL SYMPUT('_numMasterKeys_' ,PUT(NUMOBS , best16.));    

   %let _numDetailRecords_=0;
   data _null_;set &detailFile nobs=numObs;
   if _n_ > 1 then stop;
   CALL SYMPUT('_numDetailRecords_' ,PUT(NUMOBS , best16.));      
   
   %let _numDetailKeys_=0;   
   data _null_;set _DetailFile_ nobs=numObs;
   if _n_ > 1 then stop;
   CALL SYMPUT('_numDetailKeys_' ,PUT(NUMOBS , best16.));    
   
   
   data _mergedFile_ _masterNoMatch_ _detailNoMatch_;
   merge _masterFile_ (in=in_master rename=(numOfRecords=masterRecCount)) _detailFile_(in=in_detail rename=(numOfRecords=detailRecCount)) ; 
   by &foreignKey;   
   retain  _numNoMatchMasterKeys_ _numNoMatchMasterRecords_ _numNoMatchDetailrecords_ _numNoMatchDetailKeys_;
   if _n_ = 1 then do;
      _numNoMatchDetailKeys_    = 0; 
      _numNoMatchDetailRecords_ = 0;
      _numNoMatchMasterKeys_    = 0;
      _numNoMatchMasterRecords_ = 0;
   end;

   if in_master or in_detail;
   if not in_detail then do;
      _numNoMatchMasterKeys_ = sum(1,_numNoMatchMasterKeys_);
      _numNoMatchMasterRecords_ = sum(masterRecCount,_numNoMatchMasterRecords_);
      output _masterNoMatch_;
   end;   
   if not in_master then do;
      _numNoMatchDetailKeys_ = sum(1,_numNoMatchDetailKeys_);
      _numNoMatchDetailRecords_ = sum(detailRecCount,_numNoMatchDetailRecords_);
      output _detailNoMatch_;      
   end; 
   output _mergedFile_;
   run;
   
   %let _numNoMatchMasterKeys_=0;
   %let _numNoMatchDetailKeys_=0;   
   title "Checking Foreign key match between &masterFile and &detailFile";   

   data _null_;set _mergedFile_  end=lastobs;file print;
   if lastobs;
   CALL SYMPUT('_numNoMatchMasterKeys_' ,PUT(_numNoMatchMasterKeys_ , best16.));  
   CALL SYMPUT('_numNoMatchDetailKeys_' ,PUT(_numNoMatchDetailKeys_ , best16.));   
   

   _numMasterRecords_ = &_numMasterRecords_;
   _numMasterKeys_    = &_numMasterKeys_; 
   _numDetailRecords_ = &_numDetailRecords_;
   _numDetailKeys_    = &_numDetailKeys_;      
   
   put;put;put;
   put "Foreign Key = &foreignKey";
   put;
   
   put "Master File = &masterFile";
   put "Number of Records in Master File                                          = " _numMasterRecords_ comma13.;
   put "Number of Records in Master File with no match in Detail File             = " _numNoMatchMasterRecords_ comma13.;   
   put;  
   put "Number of Unique Foreign Keys in Master File                              = " _numMasterKeys_ comma13.;
   put "Number of Unique Foreign Keys in Master File with no match in Detail File = " _numNoMatchMasterKeys_ comma13.; 
   put;   put;

   put "Detail File = &detailFile";
   put "Number of Records in Detail File                                          = " _numDetailRecords_ comma13.;
   put "Number of Records in Detail File with no match in Master File             = " _numNoMatchDetailRecords_ comma13.;  
   put;
   put "Number of Unique Foreign Keys in Detail File                              = " _numDetailKeys_ comma13.;
   put "Number of Unique Foreign Keys in Detail File with no match in Master File = " _numNoMatchDetailKeys_ comma13.; 
   put;   
   
   put;put;put; run;title;run; 

 
  
   /******************
   %IF %eval(&_numNoMatchMasterKeys_) > 0 %THEN %DO;
        data _masterNoMatch_;set _masterNoMatch_ (obs=10);
        numberOfRecords = masterRecCount; format numberOfRecords  comma13.;
        proc print data=_masterNoMatch_ (obs=10);
        var &foreignKey numberOfRecords ;
        title "Sample keys from MasterFile that has NO match in the detail file";run;title;run;   
   %END;
  **********************/
   
   %IF %eval(&_numNoMatchDetailKeys_) > 0 %THEN %DO;
        data _detailNoMatch_;set _detailNoMatch_ (obs=10);
        numberOfRecords = detailRecCount; format numberOfRecords  comma13.;
        proc print data=_detailNoMatch_ (obs=10);
        var &foreignKey numberOfRecords ;
        title "Sample keys from DetailFile that has NO match in the master file";run;title;run;   
   %END;
   
   proc sql noprint;drop table _masterFile_;
   proc sql noprint;drop table _detailFile_;
   proc sql noprint;drop table _mergedFile_;
   proc sql noprint;drop table _masterNoMatch_; 
   proc sql noprint;drop table _detailNoMatch_;     
    
%mend;


	%macro corrmatx(libnamex=, dsname=, var1=,vars=,val=0.5,outfile=);

options mprint;
proc corr data = &libnamex..&dsname outp=corrmtx;
var &var1.;
with &vars.;
run; 

data corrmtx(drop = _type_ _freq_);
set corrmtx;
if _type_= "CORR";
run;



proc sort data =  corrmtx;by _NAME_;run;
data corrcol;
set corrmtx;

rename &var1.=&VAR1._1
_NAME_= Variable_name;

run;

data corrcol_&var1.;
retain var1 var2 corr;
set corrcol;
&var1=&VAR1._1*1;
if ABS(&var1) >= ABS(&val.);
abs_&var1. = ABS(&var1.);
drop &VAR1._1;
run;

proc sort data=corrcol_&var1. nodupkey;by &var1. Variable_name;run;
proc sort data=corrcol_&var1. ;by  descending abs_&var1. &var1. Variable_name;run;

proc export data= corrcol_&var1.
OUTFILE = "&outfile."
DBMS = csv
REPLACE;
run;

%mend;

/***********************************************************
**** Macro creates QC Report in HTML format  **************
**** dst is the path/directory to store the output files **
************************************************************/

%Macro HTM_QCreport(libname, filename, dst, freqsize=40, samplesize=20);
%let workPath = %sysfunc(pathname(work));
    *** create the destination folder to store the output files;
    libname __temp "&dst.";
    /*%if &SYSLIBRC ne 0 %then %do;
      %sysexec mkdir &dst;
    %end;*/

    data _null_;
      call symput('fdate', left(put(date(), weekdate.)));
      call symput('ftime', left(put(time(), hhmm.)));
    run;

    *** get the variable list  ***;
    proc contents data=&libName..&fileName noprint out=allVars;  run;

    proc printto print = "&workpath./_qcCode_.sas" new;  ** prints _qcCode_.sas as a temp file **;

    ***seggregate variables into numeric, short string and long string variables  ***;
    data num char2 char3;
      set allvars;
      file  print notitle linesize=250 pagesize=500;
      if _n_ = 1 then do;
         put "*******Prepare to write all output to html files******;";
         put "ods listing close;";
         put "ods html path='&&dst'(url=none) contents='contents.htm' body='body.htm' frame='main.htm';";
         put "*******Printing QC reports *****;";
         put "title1 '  ';";
         put "title2 'QC Report For';";
         put "title3 'File %upcase(&fileName)';";
         put "title4 'Run Time and Date: &ftime &fdate ';";
         put " proc contents data=&libname..&filename;";
         /** put " proc print data=&libname..&filename (obs=20) ;"; **/
         put "data in; set &libname..&fileName;";
      end;
      output char2;
      if type=1 then output num;
      if type=2 then do;
         if length >= 5 then output char3;
      end;
    run;

    data char3; set char3; name2 = substr(name, 1, 28); run;
    
   *** create missing indicator variables and length variables ***;
   data _NULL_;  set char3 end=lastobs;
   file  print notitle linesize=250 pagesize=500;

   put "   m_" name2 " = '0';";
   put "     if " name " eq ' ' then m_" name2 " = '1';";
   put "   len_" name2 " = 0;";
   put "   len_" name2 " = length(compress(" name "));";
   if lastobs then put "run;";

   data _NULL_;
       file  print notitle linesize=250 pagesize=500;

       **** Get the total number of observations using the system variable _n_ ****;
       put '%let num=0; %let num1=0;';
       put 'data _null_; set &&libname..&&filename end=lastobs;';
       put '   if lastobs then do; call symput("num", _n_); end; run;';
       put '%let num1=%trim(&num);';

   **** create proc frequency statement for short character and derived variables ***;
   data _NULL_;  set char2 end=lastobs;
   file  print notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
      put "Title1 ' ';";
      put "Title2 'Frequency REPORT For All Variables in &fileName';";
      put "Title3 'Run Time and Date: &ftime &fdate ';";
      put 'Title4 "Total Number of Observations: &num1";';
   end;
   if _n_ = 2 then do;
      put "Title1;";
      put "Title2;";
      put "Title3;";
      put 'Title4;';
   end;
     put "proc freq data=in order=freq;  tables";
     put name;
     put " /missing noprint outcum out=" name ";";
     put "proc print data=" name " (obs=&freqsize) label noobs; run;";
        
   **** create proc frequency statement for long character variables ***;
   /* data _NULL_;  set char3 end=lastobs;
   file  print notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
      put "proc freq data=in;  tables";
   end;
   put "m_" name2;
   if lastobs then put ' /missing;'; */

   /** Get the missing value count for long character variables **/
   data _NULL_;  set char3 end=lastobs;
      file  print notitle linesize=250 pagesize=500;
      if _n_ = 1 then do;
         put "data summary; set in end=lastobs; keep ";
      end;

      put "   n_" name2 " ";

      if lastobs then put ";";

   data _NULL_; set char3 end=lastobs;
      file  print notitle linesize=250 pagesize=500;
      put "  retain n_" name2 ";";
      put "  n_" name2 " + m_" name2 ";";

      if lastobs then put "  if lastobs then output;";

   data _NULL_; set char3 end=lastobs;
      file  print notitle linesize=250 pagesize=500;
      if _n_=1 then do;
        put "data missingcount; set summary end=lastobs; keep Variable RecMissing RecNonmissing;";
        put "Variable = '                             ';";
      end;
      put '   Variable = "' name '";  RecMissing = n_' name2 ';  RecNonmissing = &num1 - RecMissing;';
      put "  output;";

   data _NULL_;
      file  print notitle linesize=250 pagesize=500;
      put "Title1 ' ';";
      put "Title2 'Frequency REPORT For missing records in &fileName - Summary of Long Character Variables';";
      put "Title3 'Run Time and Date: &ftime &fdate ';";
      put 'Title4 "Total Number of Observations: &num1";';
      put "proc print data=missingcount;";


   **** create proc univariate statement for numeric variables ***;
   data _NULL_;  set num end=lastobs;
    file  print notitle linesize=250 pagesize=500;

       put "proc univariate data=in noprint;  var";
       put name ";";
       put "    output out=uni_" _n_ ;
       put "    min=min_val max=max_val mean=mean_val sum=sum_val nmiss=num_miss n=num_nonmiss";
       put "    pctlpts=10 25 50 75 90 pctlpre=Prctl  pctlname=_10 _25 _50 _75 _90";
       put "run;";
       put "data uni_" _n_ "; set uni_" _n_ ";";
       put "  variable='                                        ';";
       put "  variable='" name "';";
       put "run;";

   data _NULL_;  set num end=lastobs;
    file  print notitle linesize=250 pagesize=500;
    if _n_ = 1 then do;
       put "data complete; set";
    end;
       put "uni_" _n_;
    if lastobs then do;
       put ";";
       put "run;";

       ***** Generating a report from Proc Uni *****;
       /** put "proc contents data=complete; run;"; **/
       put "proc report data=complete ;";
       put "column variable min_val max_val mean_val sum_val";
       put "       Prctl_10 Prctl_25 Prctl_50 Prctl_75 Prctl_90";
       put "       num_miss;";
       put "  define variable   / ORDER  WIDTH=30 'Variable';";
       put "  define min_val    / ANALYSIS  WIDTH=12  'MIN';";
       put "  define max_val    / ANALYSIS  WIDTH=12  'MAX';";
       put "  define mean_val   / ANALYSIS  WIDTH=12  FORMAT=12.2  'MEAN';";
       put "  define sum_val   / ANALYSIS  WIDTH=12  'SUM';";
       put "  define Prctl_10   / ANALYSIS  WIDTH=12  '10th %';";
       put "  define Prctl_25   / ANALYSIS  WIDTH=12  '25th %';";
       put "  define Prctl_50   / ANALYSIS  WIDTH=12  '50th %';";
       put "  define Prctl_75   / ANALYSIS  WIDTH=12  '75th %';";
       put "  define Prctl_90   / ANALYSIS  WIDTH=12  '90th %';";
       put "  define num_miss   / ANALYSIS  WIDTH=9  '# MISSING';";
       put "  endcomp;";
       put " Title1 ' ';";
       put " Title2 'PROC REPORT For &fileName - Summary of Numeric Variables';";
       put " Title3 'Run Time and Date: &ftime &fdate ';";
       put ' Title4 "Total Number of Observations: &num1";';
       put "run;";
       put "proc printto;";

    end;

   data _NULL_;
    file  print notitle linesize=250 pagesize=500;
    *****  Print out 20 sample data  *****;
       put "proc print data=&libname..&filename (obs=&samplesize);";
       put " Title1 ' ';";
       put " Title2 ' ';";
       put " Title3 'Sample Data Are Printed out as Follows:'; run;";

    data _null_ ;
     set allvars;
     file  print notitle linesize=250 pagesize=500;
     if _n_ = 1 then do;
       put "title ;";
       put "ods html close;";
       put "ods listing;";
     end;
     stop;

   run;

   proc printto; run;

  ***** Run generated SAS code to create QC Report *****;
  %include "&workpath./_qcCode_.sas";

%Mend;
%macro correlation(libnamex=, dsname=, vars=,val=0.5,outfile=);

options mprint;
proc corr data = &libnamex..&dsname outp=corrmtx nosimple noprint;
var &vars;
run; 

data corrmtx(drop = _type_ _freq_);
set corrmtx;
if _type_= "CORR";
run;



proc sort data =  corrmtx;by _NAME_;run;

proc transpose data=corrmtx out=corrcol  name=Variable_List prefix=Corr;
var _ALL_;
by _NAME_;
run;

data corrcol;
set corrcol;
if variable_list ne "_NAME_";
run;

data corrcol;
set corrcol;
corr=corr1*1;
if ABS(corr)>= ABS(&val.);
drop corr1;
ABSCORR=ABS(CORR);
run;

data corrcol(drop=_name_ variable_list);
retain var1 var2 corr;
set corrcol;
if _name_ = variable_list then delete;
if _name_>variable_list then do;
var1=_name_ ;
var2=variable_list;
end;

else do;
var1 = variable_list;
 var2 = _name_;
end;
run;
proc sort data=corrcol nodupkey;by var1 var2;run;
proc sort data=corrcol ;by  descending ABScorr var1 var2;run;

proc export data= corrcol
OUTFILE = "&outfile."
DBMS = csv
REPLACE;
run;

%mend;
































