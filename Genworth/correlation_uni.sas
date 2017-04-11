options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/UNI/varlist.sas";


%let dateStamp = %sysfunc(putn(%sysfunc(TODAY()),yymmdd10.));

options validvarname=any;
options obs=MAX;


%corrmatx(libnamex=pred, dsname=allvars_capped, var1=RBO_IND,vars=&varlist.,val=0,
outfile=/PROJECTS/GENWORTH_2016/02_CODES/UNI/CSV/corr_rbo_&dateStamp..csv);




%corrmatx(libnamex=pred, dsname=allvars_capped, var1=FULL_PAY_IND,vars=&varlist.,val=0,outfile=/PROJECTS/GENWORTH_2016/02_CODES/UNI/CSV/corr_FP_&dateStamp..csv);
%corrmatx(libnamex=pred, dsname=allvars_capped, var1=NFO_IND,vars=&varlist.,val=0,outfile=/PROJECTS/GENWORTH_2016/02_CODES/UNI/CSV/corr_NFO_&dateStamp..csv);
PROC SORT DATA= CORRCOL_RBO_IND;BY VARIABLE_NAME;RUN;
PROC SORT DATA= CORRCOL_FULL_PAY_IND;BY VARIABLE_NAME;RUN;
PROC SORT DATA= CORRCOL_NFO_IND;BY VARIABLE_NAME;RUN;
 DATA ALL_CORR(KEEP=VARIABLE_NAME RBO_IND FULL_PAY_IND NFO_IND);
 MERGE CORRCOL_RBO_IND CORRCOL_FULL_PAY_IND CORRCOL_NFO_IND;
 BY VARIABLE_NAME;
 RUN;

 DATA ALL_CORR1;
 SET ALL_CORR;
 SIGN_RBO_IND=SIGN(RBO_IND);
 SIGN_FULL_PAY_IND=SIGN(FULL_PAY_IND);
 SIGN_NFO_IND=SIGN(NFO_IND);
 RUN;



