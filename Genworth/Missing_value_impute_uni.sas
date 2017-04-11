%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/combined_macros.sas";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/UNI/varlist.sas";
options mprint;
options obs=MAX;
%let dateStamp = %sysfunc(putn(%sysfunc(TODAY()),yymmdd10.));

DATA ALLVARS_CAPPED;
SET PRED.ALLVARS_CAPPED;
DROP AGE_BIN;

RUN;

DATA ALLVARS_CAPPED;
SET ALLVARS_CAPPED;
	IF ISSUE_AGE LE 49 THEN
		AGE_BIN = 1;
	ELSE IF 49<ISSUE_AGE <=52  THEN
		AGE_BIN =2;
	ELSE IF 52<ISSUE_AGE <=54  THEN
		AGE_BIN =3;
	ELSE IF 52<ISSUE_AGE <=56  THEN
		AGE_BIN =4;
	ELSE IF 56<ISSUE_AGE <=58  THEN
		AGE_BIN =5;
	ELSE IF 58<ISSUE_AGE <=60  THEN
		AGE_BIN =6;
	ELSE IF 60<ISSUE_AGE <=62  THEN
		AGE_BIN =7;
	ELSE IF 62<ISSUE_AGE <=66  THEN
		AGE_BIN =8;
	ELSE IF 66<ISSUE_AGE   THEN
		AGE_BIN =9;
		RUN;

%MACRO CAP(VARIABLES=);
	DATA ALLVARS;
	SET ALLVARS_CAPPED;
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
	
%macro crtUni(varlstnm=,DS=);

%let _numvars_ = %sysfunc(countw(&varlstnm.));
%do i = 1 %to &_numvars_.;
	%let _thisvar_ = %scan(&varlstnm.,&i.);
	
	proc univariate data=allvars_capped noprint;
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

%end;



data &DS.;
retain VARIABLE;
set
%do i = 1 %to &_numvars_.;
uni&i.
%end;
;
run;

%mend;

%crtUni(varlstnm=&CHAR_VARS.,DS=CHAR_UNI);



%crtUni(varlstnm=&NUM_VARS.,DS=NUM_UNI);
data char_uni;
set char_uni;
var_type="char";
run;
data num_uni;
set num_uni;
var_type="num";
run;

data uni_all;
set char_uni
num_uni;
run;

proc export data = uni_all outfile = "/PROJECTS/GENWORTH_2016/02_CODES/UNI/CSV/missing_uni_imp_&dateStamp..csv"
	dbms=CSV replace;
run;






