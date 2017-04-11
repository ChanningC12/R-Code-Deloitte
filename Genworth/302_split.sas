%include "../library.txt";
%include "../common_macros.sas";
%include "/PROJECTS/SOURCE/MACROLIB/uni_macros.sas";
%include "/PROJECTS/SOURCE/MACROLIB/preModeling_macros.sas";
%include "/PROJECTS/SOURCE/MACROLIB/modeling_macros.sas";

*options obs=10000;

options nocenter ls = 250 ps = 1000;

data mod_data;
	set pred.modeling_data_capped;
  AGE_MALE   = (GENDER_CLNT="M") * AGE_CLNT;
 	AGE_FEMALE = (GENDER_CLNT="F") * AGE_CLNT;
 	KBM_ID=_n_;
run;

data allvars;
	set mod_data (keep = KBM_ID AGE_CLNT GENDER_CLNT client);
run;

*********************************************************************************************************************
CREATING VALIDATION SAMPLE BY STRATIFIED SAMPLING TECHNIQUE USING SRSWOR METHOD
*********************************************************************************************************************;
%let strata_var= %str(AGE_CLNT GENDER_CLNT CLIENT);
%stratsamp(infile=work.allvars, outfile=mreg.val);

*********************************************************************************************************************
CREATING TRNTST (THOSE RECORDS THAT WERE NOT SELECTED IN THE VALIDATION SAMPLE)
*********************************************************************************************************************;

proc sort data=allvars; by KBM_id; run;
proc sort data=mreg.val; by KBM_id; run;

data mreg.trntst;
   merge allvars(in= target) mreg.val(in=val);
   by KBM_id ;
   if target and not val;
run;

*********************************************************************************************
RUNNING THE DO RANDOM SPLITS MACRO ON TRNTST
*********************************************************************************************;

%doRandomSplit(infile=mreg.trnTst, outfile=mreg.trnTst, numSplits=5, SplitBy=KBM_ID, numBuckets=10);

******************************************************************************************************************
PREPARING MODELING DATA WITH SPLITS (RE ATTACHING ALL TRNTST AND VAL RECORDS 
******************************************************************************************************************;

%macro attachSplits;

  data allvars (keep = KBM_ID rsplit: );
   set mreg.trntst 
       mreg.val;

   Validation = 0;
   %do i = 1 %to 5;
  		if selectionProb>0 then do; RSPLIT&i = "VAL"; Validation = 1; end;
  	    	 else if random&i < 7 then RSPLIT&i = "TRN";
  	    	 else RSPLIT&i = "TST";
 	 %end;   
  run;
 
  proc sort data=allvars; by kbm_id; run;
	proc sort data=mod_data out=modeling_data; by kbm_id; run;

  data mreg.modeling_data_final;
     merge modeling_data(in= a) allvars ;
     by kbm_id ;
     if a;
	run;
	
	proc contents data = mreg.modeling_data_final; run;
	proc print data = mreg.modeling_data_final (obs = 1000); var KBM_id rsplit1-rsplit5; run;
	
	
%mend;

%attachSplits;


proc freq data = mreg.modeling_data_final;
tables rsplit1 - rsplit5/missing;
run;

