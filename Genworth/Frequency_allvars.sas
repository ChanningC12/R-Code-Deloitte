options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/PREPROCESS/Var_list.sas";
	options obs=MAX;
%MACRO CAP(VARIABLES=);
	DATA ALLVARS;
	SET PRED.ALLVARS_PRECAP;
	KEEP
	&VARIABLES.;
	RUN;
%MEND;
/*%CAP(VARIABLES=&INTERNAL_VAR.);*/
/*%CAP(VARIABLES=&KBM_CREDIT.);*/
%CAP(VARIABLES=&INT_NEW.);
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
	



%macro freq(ds= ,out_ds=,varlist=);

	proc sql;
		drop table &out_ds.;
		quit;
	%do i = 1 %to %sysfunc(countw(&varlist.));
		%let _var_ = %scan(&varlist.,&i.);

		proc freq data  = &ds.;
			tables &_var_./missing list out =table;
		run;

		DATA TABLE_OUT;
			SET TABLE;
			FORMAT VARIABLE $100.;
			FORMAT VALUE $200.;
		 VALUE=&_VAR_.;
			VARIABLE = "&_VAR_.";
			DROP &_var_.;
		RUN;
	

		Proc append BASE = &out_ds.
			DATA = TABLE_OUT force;
		RUN;






	%end;
%MEND;
/*%freq(ds=ALLVARS ,out_ds=CHAR_VARS,varlist=&CHAR_VARS.);*/

%macro freq_num(ds= ,out_ds=,varlist=);

	proc sql;
		drop table &out_ds.;
		quit;
	%do i = 1 %to %sysfunc(countw(&varlist.));
		%let _var_ = %scan(&varlist.,&i.);

		proc freq data  = &ds.;
			tables &_var_./missing list out =table;
		run;

		DATA TABLE_OUT;
			SET TABLE;
			FORMAT VARIABLE $100.;
			FORMAT VALUE best32.;
		 VALUE=&_VAR_.;
			VARIABLE = "&_VAR_.";
			DROP &_var_.;
		RUN;
	

		Proc append BASE = &out_ds.
			DATA = TABLE_OUT force;
		RUN;






	%end;
%MEND;


%freq_num(ds=ALLVARS ,out_ds=NUM_VARS,varlist=&NUM_VARS.);

