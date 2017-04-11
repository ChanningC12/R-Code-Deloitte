%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/combined_macros.sas";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/PREPROCESS/Var_list.sas";

%macro crtUni(varlstnm=);

%let _numvars_ = %sysfunc(countw(&varlstnm.));
%do i = 1 %to &_numvars_.;
	%let _thisvar_ = %scan(&varlstnm.,&i.);
	
	proc univariate data=pred.allvars_precap noprint;
	var &_thisvar_.;
	output out=a&i. pctlpts= 0 to 100 by 10 95 99 pctlpre=PCTL_;
	run;

	data uni&i.;
	  set a&i.;
	  format VARIABLE $32.;
	  VARIABLE = "&_thisvar_.";
	run;
%end;

data uni_all;
retain VARIABLE;
set
%do i = 1 %to &_numvars_.;
uni&i.
%end;
;
run;
%mend;

/*%crtUni(varlstnm=&KBM_CREDIT.);

%crtXLS(XLSOUT=/PROJECTS/GENWORTH_2016/02_CODES/UNI/Summarized_Credit_Frequency.xls);
proc print data=uni_all noobs;
run;*/

%crtUni(varlstnm=&KBM_ZAGG.);

%crtXLS(XLSOUT=/PROJECTS/GENWORTH_2016/02_CODES/UNI/KBM_ZAGG_Frequency.xls);
proc print data=uni_all noobs;
run;
