%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/combined_macros.sas";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/UNI/varlist.sas";
options mprint;
options obs=MAX;
%MACRO CAP(VARIABLES=);
	DATA ALLVARS;
	SET PRED.ALLVARS_CAPPED;
	KEEP
	&VARIABLES.;
	RUN;
%MEND;

%CAP(VARIABLES=&HIGH_MED_LIST.);
PROC CONTENTS DATA=ALLVARS OUT=CONTENTS;RUN;

DATA CHARACTER NUMERIC;
SET CONTENTS;
IF TYPE =2 THEN OUTPUT CHARACTER;
ELSE OUTPUT NUMERIC;
RUN;

PROC SQL;
SELECT NAME INTO:CHAR_VARS SEPARATED BY " "
FROM CHARACTER;
QUIT;

PROC SQL;
SELECT NAME INTO:NUM_VARS SEPARATED BY " "
FROM NUMERIC;
QUIT;
	
%macro crtUni(varlstnm=,DS=,stat=,out_ds=);
data miss1;
set pred.allvars_capped;
run;
%let _numvars_ = %sysfunc(countw(&varlstnm.));
%do i = 1 %to &_numvars_.;
	%let _thisvar_ = %scan(&varlstnm.,&i.);
	
	proc univariate data=pred.allvars_capped noprint;
	var &_thisvar_.;
	output out=a&i. MODE=MODE median=median;
	run;

	data uni&i.;
	  set a&i.;
	  format VARIABLE $32.;
	  VARIABLE = "&_thisvar_.";
	  format MODE_1 20.5;
      format median_1 20.5
;
MODE_1=MODE;
median_1=median;

drop 
MODE
median

;
rename 
MODE_1=MODE
median_1=median

;

	
	run;

proc sql;
select mode into:mode separated by ""
from uni&i.;
quit;

proc sql;
select median into:median separated by ""
from uni&i.;
quit;

data miss&i.;
set miss&i.;
if missing(&_thisvar_.) then &_thisvar_.=&stat.;
run;

%let j=%sysfunc(SUM(&i.+1));
data miss&j.;
set miss&i.;
run;

%end;



data &DS.;
retain VARIABLE;
set
%do i = 1 %to &_numvars_.;
uni&i.
%end;
;
run;
data &out_ds.;
set miss&j.;

run;
%mend;

%crtUni(varlstnm=&CHAR_VARS.,DS=PRED.CHAR_UNI,stat=&mode.,out_ds=pred.final_char);



%crtUni(varlstnm=&NUM_VARS.,DS=PRED.NUM_UNI,stat=&median.,out_ds=pred.final_num);

data num;
set pred.final_num;
keep 
plcy_ref cust_ref
&num_vars.;
run;

proc sort data=pred.final_char;by plcy_ref cust_ref;run;
proc sort data=num;by plcy_ref cust_ref;run;

data pred.final_capped_miss;
merge pred.final_char(in=a) num(in=b);
by plcy_ref cust_ref;
if a;
run;








