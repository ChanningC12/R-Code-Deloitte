/**************************************************************************
		combined_macros.sas
		
		Purpose: Includes definitions for a list of SAS macros that are 
		commonly used for data analysis.
		
		Included macros:
			1. %getCurrent: Creates global variables defining the directory 
			path and name of the current SAS code.
			2. %crtXLS: ODS Excel output macro.
			3. %crtPDF: ODS PDF output macro.
			4. %crtDataCheckReport: Creates a few statistical tables to 
			analyze data maturity.
		
		Date: 21st November, 2013 Last Modified: 25th November, 2013
		
		Authors: Abhyuday Das, Debayan Chandra, Priyam Banerjee, Ramya Puttur
***************************************************************************/

/**************************************************************************
	%getCurrent: This macro can be used to get the current file name, 
	directory name or a string containing both. Used in the %crtXLS and 
	%crtPDF macros.
***************************************************************************/
%macro getCurrent;
	 %global currentfile;
   %global FilePath;
   %global currentDirFileNm;
	 %global currentDir;
	 %global currentFile;

		%let FilePath=%sysget(SAS_EXECFILEPATH);
		%let loca = %eval(%length(&FilePath.) - %length(%scan(&FilePath.,-1,'\')));
		%let currentdir = %substr(&FilePath.,1,&loca.);
		%let currentfile = %substr(&FilePath.,&loca.+1,%length(&FilePath.)-4-&loca.);
		%let currentDirFileNm = %substr(&FilePath.,1,%length(&FilePath.)-4);
%mend;

/**************************************************************************
	%crtXLS: This macro can be used to get a print-out of analyses results in
	Microsoft Excel.
	
	Invocation example:
		1.%crtXLS; [This would take the program name as the name of the Excel
			file and save it in the same directory as the SAS program.]
				<analysis steps>
		2.%crtXLS(xlsout=C:\Users\pribanerjee\FreqReport)	[User defined filename.
			Please do not provide the extension '.xls' in the end.]
***************************************************************************/
%macro crtXLS(style=statistical,xlsout=);
	
	ods path sashelp.tmplmst(read);
	
	%if "&xlsout." = "" %then %do;
		%getCurrent;
		%let xlsout = &currentDirFileNm..xls;
	%end;
	
   ods tagsets.ExcelXP file="&xlsout." style=&style;   
   
%mend;

/**************************************************************************
	%crtPDF: This macro can be used to get a print-out of analyses results in
	a PDF format.
	
	Invocation example:
		1.%crtPDF; [This would take the program name as the name of the PDF
			file and save it in the same directory as the SAS program.]
				<analysis steps>
		2.%crtPDF(xlsout=C:\Users\pribanerjee\FreqReport)	[User defined filename.
			Please do not provide the extension '.pdf' in the end.]
***************************************************************************/
%macro crtPDF(style=sasweb,orientation=landscape,pdfout=);
	
	ods path sashelp.tmplmst(read);
	
	%if "&pdfout." = "" %then %do;
		%getCurrent;
		%let pdfout = &currentDirFileNm..pdf;
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
   
%mend;   

/**************************************************************************
	%crtDataCheckReport: This macro can be used to get a data maturity report
	for a SAS dataset. Usual reports: Contents of the file, frequency 
	distributions for character variables with <= 50 unique values and
	Univariate statistical reports for numeric variables.
	
	Invocation example:
		1.%crtDataCheckReport(dir=raw,infile=policy_details,odstyp=PDF); 
			[This command would print the report in a PDF format. It would take 
			the program name as the name of the PDF file and save it in the same
			directory as the SAS program. Default print format is .XLS.]
***************************************************************************/
%macro crtDataCheckReport(dir=,infile=,odstyp=XLS);

	/*** Get the contents of the file ***/	
	proc contents data = &dir..&infile. varnum noprint out = _contents_;
	run;
	
	data num char_long char_short;
		set _contents_;
		
		if type = 1 then output num;
		if type = 2 then do;
			if length > 100 then output char_long;
			if length <=100 then output char_short;
		end;
	run;
	
	proc sql;
		select name
		into: num_vars
		separated by ' '
		from num;
		
		select name
		into: char_short
		separated by ' '
		from char_short;
		
	quit;
	
	%let check_n = %symexist(num_vars);
	%let check_c = %symexist(char_short);
	
	%put[&check_n. &check_c.];
	
	/*** Start creating a combined file with frequencies for character variables ***/
	%macro run_freq;
		/*** Remove variables which have a large number of unique values ***/
			%let num_char_vars = %eval(1 + %length(%cmpres(&char_short.)) - %length(%sysfunc(compress(&char_short.))));
			
			%do i = 1 %to &num_char_vars.;
				%let _thisvar_ = %scan(&char_short.,&i.);
						proc freq data=&dir..&infile. noprint;
  					tables &_thisvar_./nocum nopercent missing out=a&i.;
						run;
						
						data a&i.;
							set a&i.;
							format var_name $32.;
							var_name = "&_thisvar_.";
						run;
						
						proc sql;
							create table uniqval_&i. as
							select var_name as vars, count(var_name) as uniq_val
							from a&i.
							group by var_name;
							
							drop table a&i.;
						quit;
						
			%end;
			
				data uniqvar_table;
					set uniqval_1 - uniqval_&num_char_vars.;
					if uniq_val <=50;
				run;		
				
				proc sql;
					select vars
					into: char_vars_filt
					separated by ' '
					from uniqvar_table;
				quit;
		/************************************************************************************/
		
		/*** Create combined table of frequencies ***/		
			ods output onewayfreqs=owf;
				proc freq data=&dir..&infile.;
  			tables &char_vars_filt./nocum missing ;
			run;
			
			data owf_1(drop=frequency)/view=owf_1;
				set owf ;
				count=frequency;
			run;
			
			data one_way_freq_table ;
				length variable_name variable_value $ 200;
				set owf_1;
				VARIABLE_NAME=scan(table,2);
				VARIABLE_VALUE=catx('',of &char_vars_filt.);
				keep variable_name variable_value count percent;
			run;
			
			proc sort data = one_way_freq_table; by variable_name descending percent; run;
		%mend;
		/************************************************************************************/

		/*** Create a combined file with univariate statistics for numeric variables ***/
		%macro run_UniRpt;
			%let num_numeric_vars = %eval(1 + %length(%cmpres(&num_vars.)) - %length(%sysfunc(compress(&num_vars.))));
			
			%do i = 1 %to &num_numeric_vars.;
				%let _thisvar_ = %scan(&num_vars.,&i.);
						proc univariate data = &dir..&infile. noprint;
						var &_thisvar_.;
						output out = pctls_&i. min=MIN_VAL max=MAX_VAL mean=MEAN sum=SUMTOTAL nmiss=MISSING_CNT n=CNT
						pctlpts = 10 25 50 75 90 95 100 pctlpre = Prctl_;
						run;
						
						data pctls_&i.;
							set pctls_&i.;
							format variable_name $35.;
							variable_name = "&_thisvar_.";
						run;
			%end;
	
				data continuous_vars_distn;
					retain VARIABLE_NAME MIN_VAL MAX_VAL MEAN SUMTOTAL MISSING_CNT
					CNT Prctl_10 Prctl_25 Prctl_50 Prctl_75 Prctl_90 Prctl_95 Prctl_100;
					set pctls_1 - pctls_&num_numeric_vars.;
				run;
 		%mend;

	%if &check_c. = 1 %then %do;
	%run_freq;
	%end;
	%if &check_n. = 1 %then %do;
	%run_UniRpt;
	%end;
 	
 	%macro run_PrtOut;
 		title "Contents Procedure for Data file: &dir..&infile";
 		proc print data = _contents_ noobs;
 		var name type length label 
 		format nobs engine crdate modate;
 		run;
 		
 		%if &check_c. = 1 %then %do;
		title "Frequency Procedure for Short Character Variables in Data file: &dir..&infile";
 		proc print data = one_way_freq_table noobs; run; %end;

 		%if &check_n. = 1 %then %do;
		title "Univariate Procedure for Numeric Variables in Data file: &dir..&infile";
 		proc print data = continuous_vars_distn noobs; run; %end;
	%mend;

 	%if &odstyp. = PDF %then %do;
 		%crtPDF;
 		%run_PrtOut;
 		ods pdf close;
 	%end;
 	%else %do;
 		%crtXLS;
 		%run_PrtOut;
 		ods tagsets.excelxp close;
 	%end;

			proc sql;
				drop table owf,uniqvar_table,num,char_short,_contents_,char_long,
				one_way_freq_table,continuous_vars_distn;
				drop view owf_1;
			quit;	
	
%mend;
