options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";


data mod_data;
	set  PRED.ALLVARS_CAPPED_MISS;
run;

%macro stratsamp(infile,outfile);

	proc sort data=&infile;
	by &strata_var;
	run;

	proc surveyselect data=&infile method=srs samprate=0.29 seed=1234 out=&outfile;
	strata &strata_var;
	run;

	proc sort data=&outfile;
	by &strata_var;
	run;

%mend stratsamp;

*********************************************************************************************************************
CREATING VALIDATION SAMPLE BY STRATIFIED SAMPLING TECHNIQUE USING SRSWOR METHOD
*********************************************************************************************************************;
%let strata_var= %str(HH_INCOME DECISION_STAGE RATE_INCR IFA_GROUP_STATE_PROD_FAWP IFA_GROUP_STATE_PROD_FA POLICYHOLDER_AGE);
%stratsamp(infile=work.mod_data, outfile=mreg.val_stratified);

*********************************************************************************************************************
CREATING TRNTST (THOSE RECORDS THAT WERE NOT SELECTED IN THE VALIDATION SAMPLE)
*********************************************************************************************************************;

proc sort data=mod_data; by PLCY_REF EFF_DT; run;
proc sort data=mreg.val_stratified; by PLCY_REF EFF_DT; run;

data mreg.trntst_stratified;
   merge mod_data(in= target) mreg.val_stratified(in=val);
   by PLCY_REF EFF_DT;
   if target and not val;
run;

*********************************************************************************************
RUNNING THE DO RANDOM SPLITS MACRO ON TRNTST
*********************************************************************************************;

%doRandomSplit(infile=mreg.trntst_stratified, outfile=mreg.trntst_stratified, numSplits=5, SplitBy=PLCY_REF EFF_DT, numBuckets=10);

******************************************************************************************************************
PREPARING MODELING DATA WITH SPLITS (RE ATTACHING ALL TRNTST AND VAL RECORDS 
******************************************************************************************************************;

%macro attachSplits;

  data mod_data (keep = PLCY_REF EFF_DT rsplit: );
   set mreg.trntst_stratified 
       mreg.val_stratified;

   Validation = 0;
   %do i = 1 %to 5;
  		if selectionProb>0 then do; RSPLIT&i = "VAL"; Validation = 1; end;
  	    	 else if random&i < 7 then RSPLIT&i = "TRN";
  	    	 else RSPLIT&i = "TST";
 	 %end;   
  run;
 
  proc sort data=mod_data; by PLCY_REF EFF_DT; run;
  /**chnage made by sakshi-confirm channing***/
	proc sort data=PRED.ALLVARS_CAPPED_MISS out=modeling_data; by PLCY_REF EFF_DT; run;

  data mreg.modeling_data_final;
     merge modeling_data(in= a) mod_data;
     by PLCY_REF EFF_DT;
     if a;
	 RESPONSE_TARGET = 1;
	 if RBO_IND = 1 then RESPONSE_TARGET = 2;
	 if NFO_IND = 1 then RESPONSE_TARGET = 3;
  run;
	
	proc contents data = mreg.modeling_data_final; run;
	proc print data = mreg.modeling_data_final (obs = 100); var PLCY_REF EFF_DT rsplit1-rsplit5; run;
	
	
%mend;

%attachSplits;


proc freq data = mreg.modeling_data_final;
tables rsplit1 * RESPONSE_TARGET
	    rsplit1 * rbo_ind
		rsplit1 * nfo_ind/missing;
run;

