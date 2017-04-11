
/***************************************************************************************************
This program contains a list of macros used in the Univariate process.

Global macro variables and their descriptions:
   premfile : The name of the SAS file which contains the premium-loss info.
   uniFile  : The name of the SAS file where the univariate summaries are to be appended to.
   groupBy  : The variable for which separate univariate summaries are to be generated.
   batchId  : Any string that can uniquely identify all the univariate summaries in a batch.
              For example, you can run one batch with all the data, and another batch after dropping CAT losses.
              OR youcan run one batch with all the data and another batch where null D&B records are deleted.

List of macros and their descriptions:
     genUniCall(sasfile, outfile) :
          Generates macro call statements to call Uni macro.
          For every variable in &sasfile, one statement will be generated.
          The generated statements will written to the text file &outfile.
          If string &outfile is empty, the statements will be written to the default print file.
          The macro statements thus generated can be copied to your SAS programs.
     uni (varName,varFile)
          This macro generates univariate summary for variable &varName.
          This variable should be in file &varFile.
          If string &varFile is empty then the variable should be in file &premFile.
          Files &varFile and &premFile will be merged to create the univariate summary.
          The summary will be time-stamped and appended to file &uniFile.
          If string &uniFile is empty, then the summary will be printed to file &varName.lst.
     printUni(infile , outfile)
          Prints univariate summary in file &infile to a '@' separated text file &outfile.

Examples on how to use these macros:
   First include this program into your program.
      %include uni.sas;
   Then define all the data libraries:
      libname inlib   "/DMS4/......";
      libname outlib  "/DMS4/......";
      libname census  "/DMS4/......";
      libname weather  "/DMS4/......";
      libname DandB  "/DMS4/......";
   Then initialize the premfile,unifile and groupBy macro variables.
      %let batchId=First Batch;
      %let premFile=inlib.fireprem;
      %let uniFile=outlib.fireuni;
      %groupBy=newRenew;
   Then call the uni macro as many times as you want.
      %uni ( varName = popCount , varFile = census.cenFile)
      %uni ( varName = Rheat    , varFile = weather.weaFile )
      .
      .
      .
      .

   To print the univariate report for all the variables for which summaries have already been created, call the printUni macro
      printUni( infile=outlib.fireuni, outfile=allUni.lst )

   If you do not want to append the univariate summaries to a SAS file but just print a report, then set unifile to a null string.
      %let uniFile=;
   And then call the uni macros.
      %uni ( varName = popCount , varFile = census.cenFile)
      %uni ( varName = Rheat    , varFile = weather.weaFile )
      .
      .
      .
      .
   In the above example, univariate summary will be printed in separate files for each variable. The files are named
   as variable name suffixed with a .lst extension.

  If there are too many variables for which you need to generate uni summaries, you can use the genUniCall macro
  to create the %uni statements for you. The statements thus created can be copied and pasted into your uni programs.
  Examples of how you can the genUniCall macro,
         1.  %genUniCall ( sasfile=census.cenFile , outfile=)
                    will generate calls to Uni macro once for every variable in census.cenfile.
                  The statements will be printed to the default print file
         2.  %genUniCall ( sasfile=weather.weaFile , outfile=weather.lst)
                    will generate calls to Uni macro once for every variable in weather.weafile.
                  The statements will be printed to file called weather.lst

***************************************************************************************************/



/* store current date and time in macro variables fdate and ftime */
%global fdate ftime;
data _null_;
call symput("fdate",left(put("&sysdate"d,yymmdd.)));
call symput ("ftime", left(right("&systime")) );


%macro genUniCall(sasfile, outfile); *Generates macro call statements to call Uni macro;
/***************************************************************************************************
This macro will generate macro call statements for every variable in file &sasfile.
The generated statements will look like this: %Uni( varName = rheat , varFile = mylib.weather )
The generated statements will be written to file &outfile;
If string &outfile is empty then the generated statements will be written to the default print file.

The generated statements can be copied into SAS programs where the Uni macro needs to be invoked.
***************************************************************************************************/
     proc contents data=&sasfile out=varlist noprint;

     data varlist;set varlist;
     /* drop variables for which univariate report are NOT to be generated */
     if LEFT(RIGHT(UPCASE(name)))  in
        ('KEY','POLKEY','LOSS','LOSS2K','TLOSS2K','LOSS200K','TLOSS200K',
         'ACCTPREM','PREM','PREM2','PREMMAN3','CLMCNT','CC','CLMCNT','ZIPCODE','ZIP_CODE',
         'UWGCO','PRODKY','POLTMSBU','PRCD','PTCD',
         'LR','LR90','LR95','LR99','LRLOG','LRLOG95','LRSQRT','LRTDRT','LRCBRT',
         'ACCTKEY','ACCTNUM','ACCTZIP','ACTSTATE','ADDR1','ADDR2','ADDR3',
         'AMTDUE30','AMT_PDUE','BASYMD','BNKRPTDT', 'POLNUM', 'POLPREFIX', 'EXPDATE')
          then delete;

     varname = name;
     keep varname;


     proc sort data=varlist;
       by varname;

     %if "&outfile" ne "" %then %do;
        data _null_; set varlist; ;
          file "&outFile" ;
         put '%Uni( varname = ' varname ", varFile = &sasfile  )";
         run;
     %end;


%mend;





***This is an internal macro ***;
%macro _addSuffix_(_varList_, _suffix_);
    %let _numOfRenameVars_ = %eval(1 + %length(%cmpres(&_varList_)) -
                             %length(%sysfunc(compress(&_varList_)));

    %do _iRenameVars_ = 1 %to &_numOfRenameVars_;
          %let _thisRenameVar_ = %scan( &_varList_, &_iRenameVars_); 
          RENAME &_thisRenameVar_ = &_thisRenameVar_.&_suffix_ ;
    %end;
%mend;

***This is an internal macro ***;
%macro _removeSuffix_(_varList_, _suffix_);
    %let _numOfRenameVars_ = %eval(1 + %length(%cmpres(&_varList_)) -
                             %length(%sysfunc(compress(&_varList_)));

    %do _iRenameVars_ = 1 %to &_numOfRenameVars_;
          %let _thisRenameVar_ = %scan( &_varList_, &_iRenameVars_); 
          &_thisRenameVar_ = &_thisRenameVar_.&_suffix_ ;
       %end;
%mend;



%macro _getRatios_(_ratioVars_, _numeratorVars_, _denomVars_ , _summaryVars_);   
* Relativities and other calculations for Uni report are done here   ***;
/***************************************************************************************************
All the calculations for the Univariate report are done in this macro.
This macro is called by Uni macro.
***************************************************************************************************/
    %let _numOfRatioVars_ = %eval(1 + %length(%cmpres(&_denomVars_)) -
                             %length(%sysfunc(compress(&_denomVars_)));

   ***Calculate ratios and relativities ***;
    %do _iRatio_ = 1 %to &_numOfRatioVars_;
          %let _thisRatioVar_ = %scan( &_ratioVars_, &_iRatio_); 
          %let _thisNumeratorVar_ = %scan( &_numeratorVars_, &_iRatio_); 
          %let _thisDenomVar_ = %scan( &_denomVars_, &_iRatio_); 

         &_thisRatioVar_ = .;
         if &_thisDenomVar_ ne 0 then do;
            &_thisRatioVar_ = &_thisNumeratorVar_ / &_thisDenomVar_ ; 
         end;
         &_thisRatioVar_._REL =.;
         if &_thisNumeratorVar_._TOT ne 0 then do;
            &_thisRatioVar_._REL = (&_thisRatioVar_ * &_thisDenomVar_._TOT / &_thisNumeratorVar_._TOT ) - 1 ; 
         end;

    %end;


    %let _numOfSummaryVars_ = %eval(1 + %length(%cmpres(&_summaryVars_)) -
                             %length(%sysfunc(compress(&_summaryVars_)));
   ***Calculate percentages of totals ***;
    %do _iRatio_ = 1 %to &_numOfSummaryVars_;
          %let _thisSummaryVar_ = %scan( &_summaryVars_, &_iRatio_); 
          &_thisSummaryVar_._PERC =.;
          if &_thisSummaryVar_._TOT ne 0 then do;
             &_thisSummaryVar_._PERC = &_thisSummaryVar_ / &_thisSummaryVar_._TOT ; 
          end;
    %end;

%mend;



%macro uni (varName,varFile,summaryVars=,ratioList=); * This macro generates univariate summary for one variable **;
/***************************************************************************************************
This macro will create univariate summary for one variable &varName. The univariate summaries
will be available in work.results2. This summary will also be appended to file &uniFile.
The univariate reports will be generated for variable given by &varname in file &varFile.

Description of macro variables passed to this macro:
        &varName: The variable for which the univariate summary is to be created
        &varFile: The SAS file where variable &varName can be found. This file is assumed to be sorted by KEY.
                If string &varFile is empty, then it is assumed that variable &varName can be found in the premium file.

Global macro variables used in this macro:
        &premfile:  The file which contains premium and loss info. This file is assumed to be sorted by KEY.
    &groupBy :  The univariate report will be created separately for each group defined by variable &groupby. A combined
                        univariate summary will also be created.
                  For example,
                                if &groupby is a variable called newRenew which has two unique values 'new' & 'renew',
                        then the univariate reports are generated for 'new' data and 'renew' data  separately.
                                A combined univariate for 'new' and 'renew' combined is also created.
                  If string &groupBy is empty then the univariate reports are generated only for all data combined.
        &unifile : The sas file to which the univariate report is to be appended to. If string &unifile is empty,
                 then the univariate report summary is not appended to any file. It will instead be
                 printed to a file called &varname.lst.
        &fdate , &ftime :  date and time stamp. Every summary line appended to file &unifile will also have
                                the date and time stamp. If for one variable, univariate reports were run at two different times,
                                this date & time stamp will help differentiate these two reports.


How this macro works:
   - first file &varFile and &premFile will be merged by KEY
   - the merged file will be summarized by groupby variable and the the Uni variable.
   - all the ratios, relativities and other calculations will be performed on this summary
   - time stamp info will be added to the summary
   - the summary will then be appended to file &uniFile

***************************************************************************************************/
 

  /* if the &premfile and &varfile are different then merge them by KEY; otherwise just use the &premfile */
  %if &varFile = &premFile OR "&varFile" = "" %then %do;
      data results1;set &premfile;
  %end; %else %do;
      data results1;merge &premfile (in=in_prem )
                          &varFile  (in=in_var keep= &key &varname);
        by &key;
        if in_prem; 
  %end;
   if polcnt = . then polcnt=1;
   %if "&summaryVars"="" %then %do;
       clmcnt1000 = clmcnt * 1000;
   %end;
   %calcDerivedFields
   run;




   %if "&summaryVars"="" %then %do;
            %let summaryVars=%str(polcnt prem premman3 clmcnt clmcnt1000 loss loss200k tloss200k);
   %end;

   %if "&ratioList" = "" %then %do;
      %let ratioList= %str(
           lr loss prem
           lr2k loss200k prem
           tlr2k tloss200k prem
           freq clmcnt1000 prem
           lr2 loss premman3
           lr2k2 loss200k premman3
           tlr2k2 tloss200k premman3
           freq2 clmcnt1000 premman3
           severity loss clmcnt		
);
   %end;

 
   ***Separate ratioList to 3 separate lists *****;
    %let _ratioVars_=;
    %let _relativityVars_=;
    %let _numeratorVars_=;
    %let _denomVars_=; 
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&ratioList)) -
                             %length(%sysfunc(compress(&ratioList)));
    %do _iUni_ = 1 %to &_numOfVars_ %by 3;
          %let _thisVar_ = %scan( &ratioList, &_iUni_);
          %let _ratioVars_= &_ratioVars_ &_thisVar_;  
          %let _relativityVars_=&_relativityVars_ &_thisVar_._REL ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+1));
          %let _numeratorVars_= &_numeratorVars_  &_thisVar_; 

          %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+2));
          %let _denomVars_= &_denomVars_ &_thisVar_ ;      
    %end;


    ***Create list of 'Percentage' and 'Total' variable names ****;
    %let _PERCvars_=;
    %let _TOTvars_=;
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
    %do _iUni_ = 1 %to &_numOfVars_;
          %let _thisVar_ = %scan( &summaryVars, &_iUni_);

          %let _PERCvars_=&_PERCvars_ &_thisVar_._PERC ;  
          %let _TOTvars_=&_TOTvars_ &_thisVar_._TOT ;  

    %end;


 /*
   Summarize the results by &groupby only if &groupby is a non-null string.
 */
  %if "&groupBy" ne "" %then %do;
      PROC summary data=results1 missing nway;
       var &summaryVars;
      class &groupBy &varName;
      output out=results1 sum=;

      PROC summary data=results1 missing nway;
        by &groupBy;
       var &summaryVars;
      output out=total1 sum=&_TOTvars_;
      
      data results1;merge results1 total1;
        by &groupBy;
      ind=1;

      data total1;set total1;
      %_removeSuffix_(&summaryVars , _TOT );

      ind=2;

   %end;

/*
ind1:  quantities within both var-value and groupby-value                       results1
ind2:  quantities by group-by value  (sum accross all var-values)               total1
ind3:  quantities within var-value   (sum accross all group-by values)  results2
ind4:  grand sum - sum across both groupby-values and var-values                total2
*/

  /*  summarize the results by &varname */
  PROC summary data=results1 missing nway;
     var &summaryVars ;
     class &varName;
     output out=results2 sum=;
  PROC summary data=results2 missing nway;
     var &summaryVars ;
     output out=total2 sum=&_TOTvars_;
 
  data results2;set results2; dummyVar=1;
  data total2;set total2;dummyVar=1;
  data results2;merge results2 total2;
    by dummyVar;
  ind=3;
  data total2;set total2;
  %_removeSuffix_(&summaryVars , _TOT ); 

  ind=4;

   /* calculate ratios and relativities on the summary */
   data results2;
   %if "&groupBy" = "" %then %do;
       set results2 total2;
   %end;%else %do;
       set results1 total1 results2 total2;
   %end;

   %_getRatios_(&_ratioVars_, &_numeratorVars_, &_denomVars_ , &summaryVars);  ** calculate ratios and relativities;
   format &summaryVars  comma12.0
          &_ratioVars_  8.4
        
   
          &_relativityVars_  &_percVars_ percent7.1
          
          
          batchId varValue varName grpName grpValue $45.;

    batchId="&batchId";varvalue=&varname;varname ="&varname";
    if ind in (2,4) then varvalue = '  Total';
    %if "&groupBy" ne "" %then %do;
         grpValue=&groupBy;grpName ="&groupBy";
         if ind in (3,4) then grpvalue = '  Combined';
    %end;

    runtime = "&fdate &ftime"; ***Put the date-time stamp on  every record  ;

    drop &_TOTvars_ &varname &groupBy dummyVar;

    proc sort data=results2;by grpvalue varvalue;

    /*Append summary to &unifile or print summary to a .lst file */
    %if "&unifile" = "" %then %do;
        %printUni ( infile= results2 , outfile = &varname..lst )  *print summary *;
    %end; %else %do;
        proc append base=&uniFile data=results2;  * append to file *;
    %end;

    run;
%mend;



%macro printUni(infile , outfile, inCapFile,outputVars=); **Print the univariate summary to a '@' delimited file **;
/***************************************************************************************************
This macro prints the univariate summary in &infile to &outfile .
If string &outfile is empty then the summary is printed to the default print file.
If inCapFile is specified then the variable values are formatted to reflect  
entries in the inCapFile. 
If there were more than one univariate summaries for the same variable, this macro prints only the latest one.
***************************************************************************************************/
   %if "&outputVars" = "" %then %do;
      %let outputVars=%str(
           polcnt polcnt_PERC prem prem_PERC premman3
           CLMCNT LOSS LOSS200K TLOSS200K
           freq freq2 severity
           lr lr2 lr2k lr2k2 tlr2k tlr2k2
           freq_Rel freq2_rel severity_Rel
           lr_Rel lr2_Rel lr2k_Rel
           lr2k2_Rel tlr2k_Rel tlr2k2_rel
      );
   %end;

    proc sort data=&infile out=unidata;
      by batchId grpName varName descending runtime;

    /* keep only the latest summary for each grpName-varName combination */
    data unidata;set unidata;
      by batchId grpname varname;
    retain h_runtime;
    if first.varname then do;
         h_runtime = runtime;
    end;
    upVarName = upcase(varName);
    if runtime = h_runtime;
    drop h_runtime runtime;

    varName = compress(varName);
    varValue = compress(varValue);



    %if "&inCapFile" ne "" %then %do;
        %doFormat(inCapFile=&inCapFile,inUniFile=uniData);
    %end;


    proc sort data=unidata out=unidata;
      by batchId grpName varName grpValue varValue;



     data _null_; set unidata;
      by batchId grpName varName grpValue;
     c="@";x=-1;


     %if "&outfile" ne "" %then %do;
           file "&outFile" lrecl=32767 ;
     %end; %else %do;
           file "temp_file.lst" lrecl=32767 ;
     %end;

     if first.batchID  = 1 then do;
        line0="@Batch@SpltVar@spltVal@varName";
        line1="@varvalue @";

        %let _numOfVars_ = %eval(1 + %length(%cmpres(&outputVars)) -
                             %length(%sysfunc(compress(&outputVars)));
        put line0 +x line1 +x @;
        %do _iUni_ = 1 %to &_numOfVars_;
           %let _thisVar_ = %scan( &outputVars, &_iUni_);
           put "&_thisVar_ @" @;
        %end;
        put; 
     end;

   if varValue = 'zzTotal' then do;
      varvalue = 'Total';
      return;
   end;

   %let _numOfVars_ = %eval(1 + %length(%cmpres(&outputVars)) -
                      %length(%sysfunc(compress(&outputVars)));
   put c +x batchid c +x grpName c +x grpValue c +x varName
       c +x varvalue +x c +x @;

   %do _iUni_ = 1 %to &_numOfVars_;
      %let _thisVar_ = %scan( &outputVars, &_iUni_);
      if missing(&_thisVar_ ) then do; 
         put "   " +x c +x  @;
      end; else do;
         put &_thisVar_ +x c +x  @;
      end;
   %end;
   put; 

    run;
%mend;


%macro doformat(inCapFile=,inUniFile=,outUniFile=,upVarName=upVarName,varValue=varValue);
/***************************************************************************************************
Variable values are formatted to reflect entries in the inCapFile. 
inUniFile is the Uni summary file
outUniFile is formatted Uni summary file.
If outUniFile is not specified, it will be defaulted to inUniFile.
***************************************************************************************************/
   %if "&outUniFile" = "" %then %let outUniFile =&inUniFile;

   /* Read the comma delimited file that has the capping rules */
   data xform (keep=&upVarName startValue endValue 
                    &varValue)
        altFmt (keep=&upVarName altVarName);
   infile "&inCapFile" delimiter='~' dsd missover ;
   length &upVarName startValue endValue &varValue 
          catVarName altVarName $45. ;
   input &upVarName $ startValue $ endValue $ freqPerc $ 
         &varValue $  catVarName $ altVarName $;
   recNo = _n_;
   &upVarName = upcase(compress(&upVarName));
   
   altvarname = upcase(compress(altVarname));

   if &upVarName = '' then delete;
   if &varValue = '' and altVarName = '' then delete;

   if altvarName ne "" then do;
      &varValue="";
   end;

   &varValue = compress(&varValue);
   startValue = compress(startValue);
   endValue = compress(endValue) ; 
   if altVarName = "" then do;
      output xform;
   end;else do;
      output altFmt;
   end;
     
   proc sort data=altFmt(keep=&upVarName altVarName) 
        out=altFmt nodupkey;by &upVarName;

   proc sql noPrint;
   create index
       &upVarName on xform
   ( &upVarName ) ;
   proc sql noPrint;
   create index
       altVarName on altFmt
   ( altVarName ) ;


   proc sql noprint;
   create table xform2 as
   select A.&upVarName , X.startValue 
      ,X.endValue ,X.&varValue 
   from altFmt as A, xform as X
   where X.&upVarName = A.altVarName; 

  data xform;set xform xform2;

  proc sort data=xform;
    by &upVarName &varValue startValue;

  data xform;set xform;
    by &upVarName &varValue; 
    length C $5.;
    C = ',';
    retain fmtValue;
    length fmtValue $50.;
   if first.&varValue then do;
      fmtValue = "";
      numValue=0;drop numValue;
      numValue = &varValue;
      C = "(" || put(numValue,z2.) ||"):";
      if upcase(startValue) = "OTHER" then do;
         C = "(99):";
      end;
   end;
   if startValue = "" or startValue = "." then startValue= "Missing";
   if upCase(startValue) = "OTHER" then endValue="";
   if endValue   = "" or endValue   = "." then endValue  = "Missing";

   fmtValue = compress(fmtValue || C || startValue);

   if startValue ne endValue and 
      endValue ne "Missing" then do;
     fmtValue = compress(fmtValue||"-"||endValue);
   end;
   if last.&varValue;
   keep &upVarName &varValue fmtValue ;   
   output;
   
   %if "&inUniFile" = "" %then %goto endOfMacro;

   proc sort data=xform;
     by &upVarName &varValue;

   data &outUniFile;
      set &inUniFile;
      &varValue=compress(&varValue);
   run; 
   
   proc sort data=&inUniFile out=&outUniFile;
     by &upVarName &varValue;
   run; 

   data &outUniFile;merge &outUnifile (in=in_uni)
                          xform (in=in_xform);
   by &upVarName &varValue;
   if in_uni; drop fmtValue;
   if in_xform then do;
      &varValue = fmtValue;
   end;
   
   run;
      
%endOfMacro:   

%mend;

%macro getVarDef(inCapFile,outTxtFile,MOD);
/***************************************************************************************************
get variable definitions from the cap file  
***************************************************************************************************/
   %if "&inCapFile" = "" %then %goto endOfMacro;
   %if "&outTxtFile" = "" %then %let outTxtFile=temp.txt;
   %if "&mod" = "1" or %upcase("&mod")= "MOD" %then %do;
       %let mod =mod;
   %end; %else %do;
       %let mod=;
   %end;
   /* Read the comma delimited file that has the capping rules */
   data xform (keep=upVarName startValue endValue 
                    varValue)
        altFmt (keep=upVarName altVarName);
   infile "&inCapFile" delimiter='~' dsd missover ;
   length upVarName startValue endValue varValue 
          catVarName altVarName $45. ;
   input upVarName $ startValue $ endValue $ freqPerc $ 
         varValue $  catVarName $ altVarName $;

   upVarname = upcase(compress(upVarname));
   altvarname = upcase(compress(altVarname));

   if upVarname = '' then delete;
   if missing(varValue) then varValue=".";
   if varValue = '' and altVarName = '' then delete;

   if altvarName ne "" then do;
      varValue="";
   end;

   varValue = compress(varValue);
   startValue = compress(startValue);
   endValue = compress(endValue) ; 
   if altVarName = "" then do;
      output xform;
   end;else do;
      output altFmt;
   end;


   proc sort data=altFmt(keep=upVarName altVarName) 
        out=altFmt nodupkey;by upVarName;

   proc sql noPrint;
   create index
       upVarName on xform
   ( upVarName ) ;
   proc sql noPrint;
   create index
       altVarName on altFmt
   ( altVarName ) ;


   proc sql noprint;
   create table xform2 as
   select A.upVarName , X.startValue 
      ,X.endValue ,X.varValue 
   from altFmt as A, xform as X
   where X.upVarname = A.altVarName; 

  data xform;set xform xform2;
   varValueN=0;varValueN=varValue;

  proc sort data=xform;
    by upVarName varValueN varValue startValue;

  data _null_;set xform;
    by upVarName varValueN varValue; 
  file "&outTxtFile" &mod lrecl=32767 ;
  length c $5.; C=",";
  if _n_ =  1  and "&mod" = "" then do;
    put "varName@definition";
  end;
  length fmtValue $1000.;
  retain fmtValue; 

   if first.upVarName then do;
     fmtValue = "";
   end;

   if first.varValue then do;
     fmtValue = compress(fmtValue || varValue || "=" );
   end; else do;
     fmtValue = compress(fmtValue||",");
   end;
   if startValue = "" or startValue = "." then startValue= "Missing";
   if upCase(startValue) = "OTHER" then endValue="";
   if endValue   = "" or endValue   = "." then endValue  = "Missing";

   fmtValue = compress(fmtValue || startValue );

   if startValue ne endValue and 
      endValue ne "Missing" then do;
      fmtValue = compress(fmtValue || "-" || endValue );
   end;

  if last.varValue then do;
    fmtValue = compress(fmtValue || ";");
  end;


   if last.upVarName;
   put upVarName "@" fmtValue; 
   


%endOfMacro:   

%mend;


/******************************************************************
This is an internal macro used by the FAST-UNI process.
Do not use this macro in your programs
*******************************************************************/
%macro _fastUni_(uniVars, lastKey=, summaryVars=, arraySize=60) ;

   %if "&arraySize" = "" %then %do;
       %let arraySize=60;
   %end;

   %let _numUniVars_ = %eval(1 + %length(%cmpres(&uniVars)) -
                          %length(%sysfunc(compress(&uniVars)));
   %do _iUniVars_ = 1 %to &_numUniVars_;
          %let _thisUniVar_ = %scan( &uniVars, &_iUniVars_); 
          %_accumulateInArray_(sumBy=&_thisUniVar_, lastkey=&lastkey, summaryVars=&summaryVars
                , prefix=XY&_iUniVars_, arraySize=&arraySize) ;
   %end;

   format varName varValue $45.;keep varName varValue &summaryVars;


   %let _numUniVars_ = %eval(1 + %length(%cmpres(&uniVars)) -
                          %length(%sysfunc(compress(&uniVars)));
   %let _numSumVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                          %length(%sysfunc(compress(&summaryVars)));

   %local _lastRec_;
   %if "&lastKey" = "" %then %do;
      %let _lastRec_ =%str(lastobs);
   %end;%else %do;
      %let _lastRec_ =%str(last.&lastKey);
   %end;

   if &_lastRec_ then do;
      %do _iUniVars_ = 1 %to &_numUniVars_;
          %let _thisUniVar_ = %scan( &uniVars, &_iUniVars_); 
          varName = "&_thisUniVar_";

         XY&_iUniVars_.__hiter = _new_ hiter("XY&_iUniVars_.__hash");
         rc = XY&_iUniVars_.__hiter.first();
         do while (rc=0);
            varValue = XY&_iUniVars_.__copy; 
            varValue = compress(varValue);
            %do _isumVars_ = 1 %to &_numsumVars_;
                %let _thisSumVar_ = %scan( &summaryVars, &_iSumVars_); 
                &_thisSumVar_ = XY&_iUniVars_.__sum&_iSumVars_ ; 
            %end;
            output ; 
            rc = XY&_iUniVars_.__hiter.next();
         end;
      %end;
   end;

%mend;


%macro calcDerivedFields;
   if polcnt = . then polcnt=1;
   if clmcnt1000 = . then clmcnt1000 = clmcnt * 1000;
%mend;

%macro fastUni (infile,splitVars, uniVars,summaryVars=,ratioList=,uniFile=,batchId=,arraySize=60);
/***************************************************************************************************
This macro will create univariate summary for a list of variables given in &uniVars. 
The univariate summaries will be available in work.results2. 
This summary will also be appended to file &uniFile.

Description of macro variables passed to this macro:
        &infile: The SAS file based on which the univariate reports are to be created. 
        &splitvar :  Name of the split Variable. 
        &uniVars: A list of variables for which the univariate summary is to be created
        &summaryVars:  A list of variables who need to be summed up (like polCnt, actPrem, loss, ultLoss etc.)
        &ratioList: A list of variables to calculate ratios.Given in three variable groups (ratio name, numerator, denominator)
        &uniFile: The SAS file to which the Uni report will be saved to. 
        batchID:  Any string used to uniquely describe the list of variables in uniVars.
                  Used soley to be written out to the Unifile. Does not serve any other purpose 
                  
***************************************************************************************************/
    %if "&summaryVars"="" %then %do;
            %let summaryVars=%str(polcnt prem premman3 clmcnt clmcnt1000 loss loss200k tloss200k);
   %end;

   %if "&ratioList" = "" %then %do;
      %let ratioList= %str(
           lr loss prem
           lr2k loss200k prem
           tlr2k tloss200k prem
           freq clmcnt1000 prem
           lr2 loss premman3
           lr2k2 loss200k premman3
           tlr2k2 tloss200k premman3
           freq2 clmcnt1000 premman3
           severity loss clmcnt		
);
   %end;

 
   ***Separate ratioList to 3 separate lists *****;
    %let _ratioVars_=;
    %let _relativityVars_=;
    %let _numeratorVars_=;
    %let _denomVars_=; 
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&ratioList)) -
                             %length(%sysfunc(compress(&ratioList)));
    %do _iUni_ = 1 %to &_numOfVars_ %by 3;
          %let _thisVar_ = %scan( &ratioList, &_iUni_);
          %let _ratioVars_= &_ratioVars_ &_thisVar_;  
          %let _relativityVars_=&_relativityVars_ &_thisVar_._REL ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+1));
          %let _numeratorVars_= &_numeratorVars_  &_thisVar_; 

          %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+2));
          %let _denomVars_= &_denomVars_ &_thisVar_ ;      
    %end;


    ***Create list of 'Percentage' and 'Total' variable names ****;
    %let _PERCvars_=;
    %let _TOTvars_=;
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
    %do _iUni_ = 1 %to &_numOfVars_;
          %let _thisVar_ = %scan( &summaryVars, &_iUni_);

          %let _PERCvars_=&_PERCvars_ &_thisVar_._PERC ;  
          %let _TOTvars_=&_TOTvars_ &_thisVar_._TOT ;  

    %end;
  
  %let _lastKey_=;

   %if "&splitVars" = "" %then %do;
       data _summaryFile_;set &infile end=lastobs;
   %end; %else %do;
       %let _numOfSplitVars_ = %eval(1 + %length(%cmpres(&splitVars)) -
                             %length(%sysfunc(compress(&splitVars)));
       %let _lastKey_ = %scan( &splitVars, &_numOfSplitVars_);

       proc sort data=&infile out=_summaryFile_;by &splitVars;
       data _summaryFile_;set _summaryFile_ end=lastobs;
         by &splitVars;
   %end;

   %calcDerivedFields ; ****The user can overwrite this macro to define derived fields ***;

   keep &splitVars;
   ***Use fastUni Macro to summarize the data ****;
   %_fastUni_(uniVars=&uniVars, lastKey=&_lastKey_, summaryVars=&summaryVars, arraySize=&arraySize) 
   run;

   %if "&splitVars" = "" %then %do;
           %_splitSummary_ (inSummaryFile=_summaryFile_,
                            splitVar=,
                            uniVars=&uniVars,
                            summaryVars=&summaryVars,ratioList=&ratioList,uniFile=&uniFile,batchId=&batchId);
   %end; %else %do;
       %let _numOfSplitVars_ = %eval(1 + %length(%cmpres(&splitVars)) -
                             %length(%sysfunc(compress(&splitVars)));
       %do _iSplit_ = 1 %to &_numOfSplitVars_;
          %let _thisSplitVar_ = %scan( &splitVars, &_iSplit_); 
           %_splitSummary_ (inSummaryFile=_summaryFile_,
                            splitVar= &_thisSplitVar_,
                            uniVars=&uniVars,
                            summaryVars=&summaryVars,ratioList=&ratioList,uniFile=&uniFile,batchId=&batchId);
        %end;
   %end;


%mend;   



*******This is an internal macro  *****;
%macro _splitSummary_ (inSummaryFile,splitVar, uniVars,summaryVars=,ratioList=,uniFile=,batchId=);

   data results1;set &inSummaryFile;
   format grpName grpValue batchId $45.;
   grpName="&splitVar";grpValue=&splitVar;batchId="&batchId";


   proc sort data=results1;by batchId grpName grpValue varName varValue;

   PROC summary data=results1 missing nway;
    by  batchId grpName grpValue varName varValue;
       var &summaryVars;
      output out=results1 sum=;

   PROC summary data=results1 missing nway;
     by  batchId grpName grpValue varName;
    var &summaryVars;
   output out=total1 sum=&_TOTvars_;

  data results1;merge results1 total1;
    by batchId grpName grpValue varName;
  ind=1;

  data total1;set total1;
  %_removeSuffix_(&summaryVars , _TOT );
  ind=2;

   proc sort data=results1;
     by batchId varName varValue;
  PROC summary data=results1 missing nway;
     by batchId varName varValue;
     var &summaryVars ;
     output out=results2 sum=;

   proc sort data=total1;
     by batchId varName ;
  PROC summary data=total1 missing nway;
     by batchId varName;
     var &summaryVars ;
     output out=total2 sum=&_TOTvars_;
 
  data results2;set results2; dummyVar=1;
  data total2;set total2;dummyVar=1;
  data results2;merge results2 total2;
     by batchId varName;
  ind=3;
  data total2;set total2;
  %_removeSuffix_(&summaryVars , _TOT ); 

  ind=4;

   /* calculate ratios and relativities on the summary */
   data results2;
   %if "&splitVar" = "" %then %do;
       *****set results2 total2;
       set results1 total1 results2 total2;
   %end;%else %do;
       set results1 total1 results2 total2;
   %end;

   %_getRatios_(&_ratioVars_, &_numeratorVars_, &_denomVars_ , &summaryVars);  ** calculate ratios and relativities;
   format &summaryVars  comma12.0
          &_ratioVars_  8.4
          &_relativityVars_  &_percVars_ percent7.1 ; 
    if ind in (2,4) then varvalue = '  Total';
    if ind in (3,4) then do; 
        grpvalue = '  Combined';
        grpName="&splitVar";
    end; 

    runtime = "&fdate &ftime"; ***Put the date-time stamp on  every record  ;

    drop &_TOTvars_ dummyVar;

    proc sort data=results2;by grpvalue varvalue;

    /*Append summary to &unifile or print summary to a .lst file */
    %if "&unifile" ne "" %then %do;
        proc append base=&uniFile data=results2;  * append to file *;
    %end;

    run;
%mend;

%macro dedup(stringw=,passyes=No);
      %global _unique__;
      %local _count_;
      %let _count_=1;
      %let _unique__=;

      %do %while(%quote(%scan(&stringw,&_count_,%str( ))) ne);
          %if %qsysfunc(indexw(%upcase(&_unique__),%upcase(%scan(&stringw,&_count_,%str( )))))=0 %then 
          %let _unique__=&_unique__ %scan(&stringw,&_count_,%str( )); 
          %let _count_=%eval(&_count_+1);
      %end;
      
      %if %upcase(&passyes)=YES %then
         %do;
              &_unique__ %* generate count *;
         %end;
%mend dedup;

%macro attachsplits(list=,
                    splits=,
                    outList=,
                    outClassVars=);

   %include "/PROJECTS/GENWORTH_2016/02_CODES/UNI/dedup.sas";
                   
   %local count_list; 
   %local count_splits; 
   %local i;
   %local j;
   %local myList; 
   
   %let List= %dedup(stringw=%cmpres(&splits &list),passyes=Yes);
   %let count_list=0;
   %do %while(%qscan(&list,%eval(&count_list+1),%str( )) ne %str());
      %let count_list = %eval(&count_list+1);
   %end;     
      
   %let count_splits=0;
   %do %while(%qscan(&splits,%eval(&count_splits+1),%str( )) ne %str());
      %let count_splits = %eval(&count_splits+1);
   %end;            
         
   %if &outClassvars ne %then %do; 
      %let myList = %upcase(%dedup(stringw=%sysfunc(TRANWRD(&List,%str(*),%nrstr( ))),passyes=Yes));
      %let %str(&outClassvars)=&mylist;
   %end;
   %let myList =;       
   %do i= 1 %to &count_splits; 
      %let splits_word = %qscan (&splits, &i , %str( ));
      %do j= 1 %to &count_list;
         %let list_word = %qscan(&list,&j,%str( ));
         %if &splits_word ne &list_word %then %let myList = &myList. &splits_word.*&list_word.;          
      %end;
      %let myList = %cmpres(&myList.);
   %end;
   
   %let %str(&outlist)=&mylist;
                   
%mend attachSplits;

%macro fastuni2(dataset=,
                where=1,
                list=,
                splits=,
                classvars=,
                pctvars=,
                ratioList=,
                out=summarize,
                outtxt=,
                keep=,
                drop=,
                relBaseLevel=,
                stats=mean,
                statVars=,
                statsWgt=,
                format=,
                varsPerItr=250, 
                inCapFile=,
                CPUCOUNT=,
                obs=max);

   %let ini_cpucount = %sysfunc(getoption(CPUCOUNT));
   %put ini_cpucount = &ini_cpucount;
   %if &CPUCOUNT = %then %do; 
      %let CUPCOUNT = &ini_cpucount; 
      %if &ini_cpucount < 8 %then  %let CUPCOUNT = 8; 
      options THREADS CPUCOUNT=&CUPCOUNT;    
   %end; 
   
   *declacre local variables;    
   %local dataset where list splits classvars pctvars ratioList stats statVars statsWgt out 
          print format obs relBaseLevel
          ratio relativity numerator numeratorVars denom denomVars pctvars
          count_list count_splits count_classvars count_statVars count_stats numOfVars
          uniqueVars;
   
   proc contents data=&dataset. out=vars noprint; 
   run;        
   
   * determine which additional variables must be kept for the where clause;     

   %let addl_keep=;
   %if "&where" ne "1" and  "&where" ne "" %then %do;
      %let where=%upcase(&where);
      proc sql noprint;
         select upcase(NAME)
         into: addl_keep
         separated by ' '       
         from vars
         where(index("&where",compress(upcase(NAME)))>0); 
      quit; 
   %end;      
   
   %if "&statVars" ne "" %then %do;      
      %let statVars=%upcase(%cmpres(&statVars));         
      proc sql noprint;
         select upcase(NAME)
         into: statVars
         separated by ' '       
         from vars
         where(indexw("&statVars",compress(upcase(NAME)))>0); 
      quit; 
   %end;   
 
   %if "&list" ne "" %then %do;      
      %let list=%upcase(%cmpres(&list));         
      proc sql noprint;
         select upcase(NAME)
         into: list
         separated by ' '       
         from vars
         where(indexw("&list",compress(upcase(NAME)))>0); 
      quit; 
   %end;    
 
 
   
   %if &ratioList= %then %do; 
      %let numOfVars=0; 
      %let relativity=;
      %let numerator=;
      %let denom=;
   %end;    
   %else %do;   
      ***Separate ratiodList to 3 separate lists *****; 
      %let numOfVars= %eval(1 + %length(%cmpres(&ratioList)) -
                               %length(%sysfunc(compress(&ratioList)));
      %do _iUni_ = 1 %to &numOfVars %by 3;
            %let _thisVar_ = %scan( &ratioList, &_iUni_);
            %let ratio= &ratio &_thisVar_;  
            %let relativity=&relativity &_thisVar_._REL ;  
      
            %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+1));
            %let numerator= &numerator  &_thisVar_; 
      
            %let _thisVar_ = %scan( &ratioList, %eval(&_iUni_+2));
            %let denom= &denom &_thisVar_ ;      
      %end;
   %end;       
   
   %let list=%CMPRES(&list);
   %let classvars=%CMPRES(&classvars);
         
   %let uniqueVars = %upcase(%dedup(stringw=%sysfunc(TRANWRD(&list,%str(*),%nrstr( ))),
                            passyes=Yes));   
                              
   %if "&classvars" ne "" %then %do;
      %let count_classvars=0;
      %let new_classvars=;
      %let classvars_word =%upcase(%qscan(&classvars,&count_classvars+1,%str( )));
      %do %while(&classvars_word ne %str());              
         %if %qsysfunc(indexw(&uniqueVars,&classvars_word))ne 0 %then 
            %let new_classvars = &new_classvars &classvars_word;
         %let count_classvars = %eval(&count_classvars+1);
         %let classvars_word =%qscan(&classvars,&count_classvars+1,%str( ));
      %end;
      %let classvars=&new_classvars;     
   %end;
   %else %let classvars=&uniqueVars;   
     
   %let count_list=0;
   %do %while(%qscan(&list,&count_list+1,%str( )) ne %str());
      %let count_list = %eval(&count_list+1);
   %end;  
     
   %let count_classvars=0;
   %do %while(%qscan(&classvars,&count_classvars+1,%str( )) ne %str());
      %let count_classvars = %eval(&count_classvars+1);
   %end;

   %let count_pctvars=0;
   %do %while(%qscan(&pctvars,&count_pctvars+1,%str( )) ne %str());
      %let count_pctvars= %eval(&count_pctvars+1);
   %end;

   %let count_statVars=0;
   %do %while(%qscan(&statVars,&count_statVars+1,%str( )) ne %str());
      %let count_statVars = %eval(&count_statVars+1);
   %end;
   
   %let count_stats=0;
   %do %while(%qscan(&stats,&count_stats+1,%str( )) ne %str());
      %let count_stats= %eval(&count_stats+1);
   %end;

   %let sumvars=%dedup(stringw=%cmpres(&denom &numerator &pctvars.),passyes=Yes);   
   %let count_sumvars=0;
   %do %while(%qscan(&sumvars,&count_sumvars+1,%str( )) ne %str());
      %let count_sumvars= %eval(&count_sumvars+1);
   %end;
  
   %if "&splits." ne "" %then %do;   
      %if &relBaseLevel eq %then %let relBaseLevel=1;
      %let splits=%cmpres(&splits);
      %let len_splits = %LENGTH(&splits);
      
      %let count_splits=0;
      %do %while(%qscan(&splits,&count_splits+1,%str( )) ne %str());
         %let count_splits= %eval(&count_splits+1);
      %end;
   %end; 
   %else %if &relBaseLevel eq %then %let relBaseLevel=0;
       
   %let m=0;      
   %let n=0;      
   %do %while(&m < &count_list);
      %let minilist=;
      %let n = %eval(&n+1);
      %let o=0;
      %do %while(&o<&varsPerItr. and %qscan(&list,%eval(&m+1),%str( )) ne);        
         %let m = %eval(&m+1); 
         %let o = %eval(&o+1); 
         %let minilist= %cmpres(&minilist %qscan(&list,&m,%str( )));            
      %end;      
      %if "&splits" ne "" %then %do;
         %let new_classvars=;
         %attachSplits(list=&minilist,            
                       splits=&splits,             
                       outList=minilist,          
                       outClassVars=new_classvars);
         %let classvars=&new_classvars;
         %let count_classvars=0;
         %do %while(%qscan(&classvars,&count_classvars+1,%str( )) ne %str());
            %let count_classvars = %eval(&count_classvars+1);
         %end;                  
      %end;
      
      options FULLSTIMER;
      proc means data=&dataset (keep= &sumvars. &addl_keep. &classvars &statVars &statsWgt.
                                where=(&where)
                                obs=&obs) 
                  noprint CHARTYPE MISSING SUMSIZE=MAX; 
         
         class &classvars / mlf;
         types &minilist();
         var &sumvars;
         output out=out_&n.(drop=_freq_) sum(&sumvars)=&sumvars       
         %if "&statVars" ne "" and &statsWgt. eq %then %do; 
            %do i = 1 %to &count_stats;                                  
                %let stats_word = %qscan (&stats, &i , %str( ));         
                &stats_word.(&statVars)=
            %end;      
         %end; /autoname WAYS NOINHERIT; 
         %if "&format" ne "" %then format &format;;      
      run;
      
      %if "&statVars" ne "" and &statsWgt. ne %then %do;
         proc means data=&dataset (keep=&addl_keep. &classvars &statVars &statsWgt.
                                where=(&where)
                                obs=&obs) 
                  noprint CHARTYPE MISSING SUMSIZE=MAX; 
         
            class &classvars / mlf;
            %if "&statsWgt." ne "" %then weight &statsWgt. %str(;) ;         
            types &minilist();                                                          
            var &statVars.;                              
            output out=outstat_&n.(drop=_freq_)                                
                  %do i = 1 %to &count_stats;                                  
                     %let stats_word = %qscan (&stats, &i , %str( ));         
                     &stats_word.(&statVars)=
                  %end;
                   /autoname WAYS NOINHERIT;       
            
            %if "&format" ne "" %then format &format;;
         run;                                      
         
         data out_&n.;
            merge out_&n.
                  outstat_&n.;
            by _type_ ;      
         run;       
      %end;               
      options NOFULLSTIMER;
               
      proc sql noprint;
         select max(_way_) as _way_
           into : maxway
           separated by ' '
           from out_&n.;
      quit;            
          
      data out_&n. (drop=index_m i incl current_class
                       &classvars _type_ upVarName varValue
                       _way_);                                                  
         length current_class $50.;
         split0='ALL';
         array Split(1:%eval(&maxway.+1)) $50.;
         level0='ALL';
         array Level(1:%eval(&maxway.+1)) $50.;
         length &sumvars 8.;             
         set out_&n. (in=in1);
         index_m=0;
         do i = 1 to LENGTH(_type_);
            incl = substr(_type_,i,1);   
            if incl ne "0" then do;
               index_m = index_m + 1; 
               current_class=UPCASE(SCAN("&classvars",i," "));
               split(index_m)= current_class;
               %do j = 1 %to &count_classvars;                                  
                  %let classvars_word = %qscan (&classvars , &j , %str( ));         
                  %if &j = 1 %then if (current_class=UPCASE("&classvars_word")) then level(index_m)=&classvars_word %str(;);
                     %else else if (current_class=UPCASE("&classvars_word")) then level(index_m)=&classvars_word %str(;);
               %end;        
            end;            
         end;               
         do i = 1 to %eval(&maxway.+1); 
            if split(i)='' then split(i)= "ALL";
            if level(i)='' then level(i)= "ALL";
         end;     
         output; 
         
         *special case for when running univariates; 
         *the summarized dataset has only two split levels; 
         %if "&splits." ne "" %then %do; 
            if indexw("%upcase(%cmpres(&splits))",split1)>0 and 
               indexw("%upcase(%cmpres(&splits))",split2)>0 then do; 
            
              upVarName=split1;
              varValue=level1;
              split1=split2;
              level1=level2; 
              split2=upVarName;
              level2=varValue;
              
              output;
            end;
         %end;    
     run;

     proc sort data=out_&n. nodupkey;
        by split0 -- split&maxway level0 -- level&maxway;
     run;
     
     data split_values_all ;
        set out_&n. (where=(%do i = %eval(&relBaseLevel.+1) %to %eval(&maxway+1); 
                              split&i = 'ALL' and
                           %end; 1)                       
                    rename=%cmpres((%do i = 1 %to &count_sumvars.;
                                       %let sumvars_word = %qscan (&sumvars , &i , %str( ));
                                       &sumvars_word.=&sumvars_word.__a
                                    %end;))
                    );
     run;   
     
     data out_&n. ;
        %do i = 1 %to &count_sumvars.;
            %let sumvars_word = %qscan (&sumvars , &i , %str( ));
            %cmpres(&sumvars_word.__a)=.;
        %end;
        *load the "Split value = all" dataset into the hast object;
        if _N_ = 1 then do;
           declare hash h(dataset: "work.split_values_all", hashexp: 6);
           %do i = 0  %to &relBaseLevel;
              h.defineKey("split&i.");
              h.defineKey("level&i.");
           %end;
           %do i = 1 %to &count_sumvars.;
              %let sumvars_word = %qscan (&sumvars , &i , %str( ));
              h.defineData("%cmpres(&sumvars_word.__a)");
              drop %cmpres(&sumvars_word.__a);
           %end;   
           h.defineDone();
        end;
        set out_&n.;
        by split0 -- split&maxway level0 -- level&maxway;
        if %cmpres(first.level&relBaseLevel) then do; 
           %do j = 1 %to &count_pctvars;                                  
               %let pctvars_word = %qscan (&pctvars, &j , %str( ));         
               %cmpres(c_&pctvars_word.=&pctvars_word.;)  
               drop %cmpres(c_&pctvars_word.);
            %end; 
        end;
        else do; 
           %do j = 1 %to &count_pctvars;                                  
               %let pctvars_word = %qscan (&pctvars, &j , %str( ));       
               %cmpres(c_&pctvars_word. + &pctvars_word.;)
           %end;    
        end;   
        rc = h.find();
        drop rc;
        
     
        *calculate ratio and relativities; 
        %do i = 1 %to %eval(&numOfVars/3);
           %let ratio_word = %qscan (&ratio, &i , %str( )); 
           %let relativity_word = %qscan (&relativity, &i , %str( )); 
           %let numerator_word = %qscan (&numerator, &i , %str( )); 
           %let denom_word = %qscan (&denom, &i , %str( )); 
           
           call missing (&ratio_word,&relativity_word);
           
           if &denom_word not in (0,.) and &numerator_word ne . then do; 
              &ratio_word = &numerator_word / &denom_word;   
           end;                           
           
           if &denom_word not in (0,.) and 
              %cmpres(&numerator_word.__a) not in (0,.) and 
              &numerator_word not in (.) and
              &denom_word not in (.) then do;
              &relativity_word = (&numerator_word*%cmpres(&denom_word.__a)) /
                                 (%cmpres(&numerator_word.__a)*&denom_word) -1; 
           end;               
           format &ratio_word &relativity_word percent6.1;                   
        %end;
        
        *calculate percent vars;
        %do j = 1 %to &count_pctvars;
           %let pctvars_word = %CMPRES(%qscan ( &pctvars, &j , %str( )));
           call missing (pct_&pctvars_word.,cpct_&pctvars_word.);
           if %cmpres(&pctvars_word.__a) not in (0,.) and 
              &pctvars_word not in (.) then do;
              pct_&pctvars_word.=&pctvars_word/%cmpres(&pctvars_word.__a);
              cpct_&pctvars_word.=c_&pctvars_word/%cmpres(&pctvars_word.__a);
           end;
           format pct_&pctvars_word. cpct_&pctvars_word. percent6.1;
        %end;      
        
        format &sumvars comma15.;      
     run;
      
   %end;
   
   data &out (drop=split0 
                   level0 
                   %cmpres(split%eval(&maxway.+1))
                   %cmpres(level%eval(&maxway.+1))
                   &drop);   
      retain %do i = 1 %to &maxway.;
                split&i level&i
             %end;
             %do j = 1 %to &count_pctvars;    
                %scan(&pctvars,&j)                                 
                %cmpres(pct_%scan(&pctvars,&j)) 
                %cmpres(cpct_%scan(&pctvars,&j))
             %end;
             &sumvars 
             %do _iUni_ = 1 %to &numOfVars %by 3;
                 %scan( &ratioList, &_iUni_)
                 %cmpres(%scan( &ratioList, &_iUni_)_REL)
             %end;;
      set %do i = 1 %to &n.;
             out_&i.   
          %end;; 
      %do i = 1 %to &maxway.;
          level&i=left(level&i);
      %end;      
   run;
   
   %if &n > 1 %then %do;    
      proc sort data=&out nodupkey;
         by split1 -- split&maxway. level1 -- level&maxway;
      run;      
   %end; 
   
   %if "&keep" ne "" %then %do; 
      data &out;
         retain &keep;
         keep &keep;
         set &out;
      run;
   %end;   
   
   %if "&inCapFile" ne "" %then %do; 
      %if "&splits" ne "" %then %let start=1; 
         %else %let start=2;
      %do i = &start. %to &maxway;
         %doFormat(inCapFile=&inCapFile,
                   inUniFile=&out,
                   upVarName=Split&i,
                   varValue=Level&i);      
      %end;              
   %end; 
   
   %if "&outtxt" ne "" %then %do; 
   proc export data= &out
   outfile="&outtxt"
   dbms=dlm  replace;
   delimiter="@";
   run;

/*      %out_dlm (indsn=&out,   */
/*                dlm=@,            */
/*                outfile=&outtxt);        */
   %end;  
   
   options THREADS CPUCOUNT=&ini_cpucount;    
    
%mend fastuni2;

%macro print_summary (summary_file=,
                      format=,    
                      list=,
                      disp=,
                      inCapFile=,
                      upVarName=Split1,
                      varValue=Level1,
                      SHEET_NAME=n);
                        
   proc contents data=&summary_file. OUT=vars (keep=name type label varnum) noprint ; 
   run;                                                             
                                                                    
   proc sort data=vars out=vars(drop=varnum ); 
      by varnum;
   run;   
                                                                    
   data vars(index=(name/unique));                                  
      set vars;                                                     
      name=compress(upcase(name));                                  
   run;    
   
   %let disp = %cmpres(&disp);
        
   %if %str(&disp) ne %then %do; 
      filename in "&workPath/infile.txt" lrecl=2048;          
      
      data _null_;
         file in;     
         put "&disp";
      run;                                                 

      data disp;
         format name $32.
                label $50.;
         infile in dlm=' ' dsd;                
         input name $ label $ background$ @@;
         name=compress(upcase(name));
         set vars key=name / unique;                             
         if _IORC_ ne 0 then do;           
            _error_ = 0;
            delete;
         end;               
         xlformat = "#,##0";
      run;  
      x "rm &workPath/infile.txt";
   %end;
   %else %do; 
      data disp;
         set vars;
         xlformat = "#,##0";
         background="WHITE";
      run;     
   %end;                        

   proc sql noprint;
      select count(*)
      into: maxway
      separated by ' '       
      from vars
      where upcase(name)? "SPLIT";
   quit;              
         
   data vars; 
      set vars;
      %do i = 1 %to &maxway;
         if name in ("SPLIT&I.","LEVEL&I.") then delete;
      %end;
   run;    
      
   proc sql noprint;
      select upcase(name)
      into: printvars
      separated by ' '       
      from vars; 
   quit;    
   %put printvars=&printvars;
   
   data ctrl(keep=type start fmtname label);   
      length fmtname start $32.;
      set disp (drop=type) end=last;
      
      type = 'C';
      start=upcase(name);
      
      fmtname="LABEL";
      label=label;
      output;
      
      fmtname="XLFORMAT";
      label=XLFORMAT;
      output;
      
      fmtname="BACKGROUND";
      label=BACKGROUND;
      output;
   run; 
   
   proc sort data=ctrl;
      by fmtname start;
   run; 
   
   proc format library=work cntlin=ctrl;
   run;   
    
   data summary_file;
      set &summary_file; 
   run;    
   
   %if "&inCapFile" ne "" %then %do; 
      %doFormat(inUniFile=summary_file,
                   inCapFile=&inCapFile.,
                   upVarName=Split1,
                   varValue=Level1);
   %end;                      
                    
   proc sort data=summary_file out=summary_file NODUPKEY; 
      by split1 -- split&maxway. level1--level&maxway.;
   run;
   
   %let count_printvars=0;
   %do %while(%qscan(&printvars,&count_printvars+1,%str( )) ne %str());
      %let count_printvars = %eval(&count_printvars+1);
   %end;    
   
   %let count_list =0;
   %do %while(%qscan(&list,%eval(&count_list+1),%str( )) ne %str());
      %let count_list = %eval(&count_list+1);
   %end;
      
   %do i = 1 %to &count_list;               
      %let list_word = %UPCASE(%CMPRES(%qscan(&list, &i , %str( ))));          
      %let count_splits=0;
      %do %while(%qscan(&list_word,&count_splits+1,%str(*)) ne %str());
         %let count_splits = %eval(&count_splits+1);
      %end;   
      
      %let cond = ;
      %do j = 1 %to &count_splits;
          %let cond = &cond "%qscan(&list_word,&j,%str(*))",;
      %end;            
      
      data toprint; 
         set summary_file 
             (where=(%do j = 1 %to &count_splits;
                         INDEXW(upcase(%do k = 1 %to &count_splits;
                                          COMPRESS(split&k)||
                                       %end; ' '),
                                      "%upcase(%CMPRES(%qscan(&list_word,&j,%str(*))))")
                               >0 and  
                     %end; 1)) end=last_obs
              summary_file (where=(%do j = 1 %to &count_splits;
                                       upcase(COMPRESS(split&j))="ALL" and 
                                     %end;
                                     1));
              call symput ("last_obs",_n_);                                     
      run;         
     
      %if &last_obs < 100 %then %do;
         option ls=120;
         ods proclabel="&list_word.";
         %if %upcase(&SHEET_NAME)=Y %then ODS TAGSETS.EXCELXP OPTIONS(SHEET_NAME="&list_word.");;
               
         proc print data=toprint
                    WIDTH=MINIMUM
                    S=' '
                    noobs 
                    label
                    contents="";             
         
           var %do j = 1 %to &count_splits;
                   level&j
               %end;
               %str(;);
                                                    
            %if &disp ne %then %do j = 1 %to &count_printvars;
                %let printvars_word = %UPCASE(%CMPRES(%qscan( &printvars, &j , %str( ))));
                %let tagattr =%sysfunc(TRIM(&printvars_word),$xlformat.);
                %let background =%sysfunc(TRIM(&printvars_word),$background.);
                var &printvars_word / style (data) = {tagattr=%CMPRES("&tagattr.") 
                                                      %if %upcase(&background.) ne WHITE %then %do;
                                                         background=%CMPRES(&background.)
                                                      %end; 
                                                      borderwidth=2};
                %let mylabel= %TRIM(%sysfunc(TRIM(&printvars_word.),$label.));             
                %if "&mylabel" ne ""%then %do;
                   label &printvars_word="&mylabel.";
                %end;
            %end;
            %else var &printvars;;
            
            label %do j = 1 %to &count_splits;
                       level&j="%TRIM(%qscan(&list_word,&j,%str(*)))"
                  %end;;
            
            %if &format ne %then format &format %str(;);      
                   
         run;      
      %end;   
   %end;
%mend print_summary;
