options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";

%let obs=max;

data  allVars;            
	set mreg.modeling_data_final ;     	
   	weightVar = 1;	
   	pol_cnt=1;
   	zero=0;
/*	%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";*/
	POST_URS_VAR1 = ((ifa_group_state_prod_fa=1)+(benefit_pool_asset_ratio=10) = 2);
	POST_URS_VAR2 = ((ifa_group_state_prod_fa=1)+(income_split="LESS THAN $40,000") = 2);
	POST_URS_VAR3 = ((ifa_group_state_prod_fa=1)+(hh_income=2) = 2);

	TREE_NFO1A = 0;
	TREE_NFO3A = 0;
	IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=1 AND TOTL_COMMUNICATED_INCR_RATE>=4 
	then tree_NFO1A = 1;

	IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=0 AND STATE_MD=0 AND RATE_INCR>=4 
	then tree_NFO3A = 1;
 run;

%macro crtView(where=, view=);
   data  &view / view=&view;          
      set allVars (obs=&obs where=(&where /***%applyFilter***/));
   run;
%mend crtView; 
	
%let split=RSPLIT1;

   %let ALL		 = (&split in("TRN","TST","VAL"));
   %let TRNTST = (&split in("TRN","TST"));
   %let TRN 	 = (&split = "TRN");
   %let TST 	 = (&split = "TST");
   %let VAL 	 = (&split = "VAL");
	
    
/*   %crtView(view=ALL1,where=&ALL);*/
   %crtView(view=TRNTST,where=&TRNTST);
   %crtView(view=TRN,where=&TRN);
   %crtView(view=TST,where=&TST);
   %crtView(view=VAL,where=&VAL);

proc hpforest data=TRNTST seed=12345 vars_to_try=5 leafsize=1 maxtrees=500;
target nfo_ind/level=binary;
input &varlist_rf1. / level=interval;
ods output fitstatistics=fitstats;
save file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_noNUMD.bin";
run;

proc hp4score data=VAL;
id cust_ref eff_dt plcy_ref weightVar NFO_IND;
score file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_noNUMD.bin" out=TST_scored;
run;

%crtCentilesCode(TST_scored, p_nfo_ind1,weightVar,/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_noNUMD_NFO.sas);

data VAL_rf_scored_centile;
set TST_scored;
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_noNUMD_NFO.sas";
run;

