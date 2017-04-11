
%let workPath = %sysfunc(pathname(work));

/***********************************************************
**** Macro creates QC Report in HTML format  **************
**** dst is the path/directory to store the output files **
************************************************************/

%Macro HTM_QCreport(libname, filename, dst, freqsize=40, samplesize=20);

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
