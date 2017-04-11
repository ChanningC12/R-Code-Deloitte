/****************************************************************************************************************************
 program:      run_fastuni2.sas
 author:       Genworth Team
 date:         4 November, 2016
 purpose:      To create URS dataset
 Client:       Genworth

Note:
Input dataset(s):

Output dataset(s):

*****************************************************************************************************************************/
options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";


%let dateStamp = %sysfunc(putn(%sysfunc(TODAY()),yymmdd10.));

options validvarname=any;

options obs=MAX;

data base_URS_data;
	set pred.allvars_precap;
		TUOC_1=SUBSTR(TUOC,1,1);
    TUOCS_1=SUBSTR(TUOCS,1,1);
	CRRT_1=SUBSTR(CRRT,1,1);

	DROP CRRT TUOC  TUOCS ;
	RENAME CRRT_1=CRRT
	TUOC_1=TUOC
	TUOCS_1=TUOCS
;

run;

**** declare global variable to be used my various macros;
%let inCapFile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Capping_missing_all.sas;

%macro run_uni(infile = , outfile = );
**** Create a text file of variable capping def;
%getVarDef(inCapFile=&inCapFile,outTxtFile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/vardef.txt);
   
data allvars;
set &infile;
FULL_PAY_IND			 = (RESPONSE="FULL PAY");
NFO_IND      			 = (RESPONSE="NFO");
RBO_IND   			 = (RESPONSE="RBO");

run;


%doCapping(inDataLib=work,inDataFile=allvars,inCapFile=&inCapFile,outDataFile=pred.&outfile,outCapVarList=varlist_temp,runFreq=y,crtIndVar=y,assignFmt=n);






*Remove format assignments from the input dataset;
%unassignFmt(pred, &outfile);



%mend;

%run_uni(infile = base_URS_data, outfile=ALLVARS_CAPPED_MISS1);

DATA PRED.ALLVARS_CAPPED_MISS;
SET PRED.ALLVARS_CAPPED_MISS1;
	KEEP
	PLCY_REF
	CUST_REF
	&VARLIST01.
	FULL_PAY_IND
	NFO_IND
	RBO_IND
DECISION_STAGE
EFF_DT;
IF MISSING(BINARY_CARDIO_TRANS) THEN BINARY_CARDIO_TRANS=0;
IF MISSING(BINARY_DIAB_TRANS) THEN BINARY_DIAB_TRANS=0;
IF MISSING(BINARY_HERNIA_TRANS) THEN BINARY_HERNIA_TRANS=0;
IF MISSING(BINARY_HYPT_TRANS) THEN BINARY_HYPT_TRANS=0;
IF MISSING(BINARY_NF_TRANS) THEN BINARY_NF_TRANS=0;
IF MISSING(BINARY_NM_TRANS) THEN BINARY_NM_TRANS=0;
IF MISSING(BINARY_NO_TRANS) THEN BINARY_NO_TRANS=0;
IF MISSING(BINARY_PREG_TRANS) THEN BINARY_PREG_TRANS=0;
IF MISSING(SEV_DIAB_TRANS) THEN SEV_DIAB_TRANS=0;
IF MISSING(SEV_NT_TRANS) THEN SEV_NT_TRANS=0;
IF MISSING(BINARY_BP_TRANS) THEN BINARY_BP_TRANS=0;
IF MISSING(BINARY_CERV_TRANS) THEN BINARY_CERV_TRANS=0;
IF MISSING(BINARY_DRUG_TRANS) THEN BINARY_DRUG_TRANS=0;
IF MISSING(BINARY_NS_TRANS) THEN BINARY_NS_TRANS=0;
IF MISSING(BINARY_NT_TRANS) THEN BINARY_NT_TRANS=0;
IF MISSING(SEV_HYPT_TRANS) THEN SEV_HYPT_TRANS=0;
IF MISSING(SEV_RESP_TRANS) THEN SEV_RESP_TRANS=0;

RUN;



	


