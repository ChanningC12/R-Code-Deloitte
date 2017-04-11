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
	  format PCTL_0_1 20.5
PCTL_10_1 20.5
PCTL_20_1 20.5
PCTL_30_1 20.5
PCTL_40_1 20.5
PCTL_50_1 20.5
PCTL_60_1 20.5
PCTL_70_1 20.5
PCTL_80_1 20.5
PCTL_90_1 20.5
PCTL_100_1 20.5
PCTL_95_1 20.5
PCTL_99_1 20.5
;
PCTL_0_1=PCTL_0;
PCTL_10_1=PCTL_10;
PCTL_20_1=PCTL_20;
PCTL_30_1=PCTL_30;
PCTL_40_1=PCTL_40;
PCTL_50_1=PCTL_50;
PCTL_60_1=PCTL_60;
PCTL_70_1=PCTL_70;
PCTL_80_1=PCTL_80;
PCTL_90_1=PCTL_90;
PCTL_100_1=PCTL_100;
PCTL_95_1=PCTL_95;
PCTL_99_1=PCTL_99;
drop 
PCTL_0
PCTL_10
PCTL_20
PCTL_30
PCTL_40
PCTL_50
PCTL_60
PCTL_70
PCTL_80
PCTL_90
PCTL_100
PCTL_95
PCTL_99
;
rename 
PCTL_0_1=PCTL_0
PCTL_10_1=PCTL_10
PCTL_20_1=PCTL_20
PCTL_30_1=PCTL_30
PCTL_40_1=PCTL_40
PCTL_50_1=PCTL_50
PCTL_60_1=PCTL_60
PCTL_70_1=PCTL_70
PCTL_80_1=PCTL_80
PCTL_90_1=PCTL_90
PCTL_100_1=PCTL_100
PCTL_95_1=PCTL_95
PCTL_99_1=PCTL_99
;

	
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

%crtUni(varlstnm=&KBM_CREDIT.);

%crtXLS(XLSOUT=/PROJECTS/GENWORTH_2016/02_CODES/UNI/Summarized_Credit_Frequency.xls);
proc print data=uni_all noobs;
run;

/*%crtUni(varlstnm=&KBM_ZAGG.);*/
/**/
/*%crtXLS(XLSOUT=/PROJECTS/GENWORTH_2016/02_CODES/UNI/KBM_ZAGG_Frequency1_s.xls);*/
/*proc print data=uni_all noobs;*/
/*run;*/
