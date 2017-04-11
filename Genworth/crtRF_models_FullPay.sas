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
	PFA_IND = (NFO_IND+RBO_IND=0);
/*	%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";*/
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

%let varlist_int=%str(
ISSUE_AGE
IF_SPOUSE_DISC
SHARED_POLICY
POLICYHOLDER_AGE
CLAIM_COUNT_CONTACT_DYNAMIC
CLAIM_COUNT_CLOSED_DYNAMIC
TOTAL_AMOUNT_PAID
CLAIM_COUNT_DYNAMIC
RATE_INCR
RATE_INCR_AFFORD
IFA_GROUP_STATE_PROD_FA
IFA_GROUP_STATE_PROD_FAWP
POLICY_AGE_2
ANNL_PREM_BFOR
CNT_FPO_PREV1
CNT_RBO_PREV1
BENEFIT_POOL_ASSET_RATIO
DECISION_STAGE
BIO_AFTER_NO_BIO
NUM_DECISIONS
ORIG_DBA_AFTR
RATE_INCR_PRE
CLAIM_COUNT_NP_CLOSED_DYNAMIC
CLAIM_COUNT_OPEN_DYNAMIC
BENE_TRM_BFOR
BIO_BEFORE_5_COMPOUND
BIO_BEFORE_NO_BIO
FREQ_AFTR_M
FREQ_AFTR_Q
FREQ_BFOR_M
FREQ_BFOR_Q
ORIG_DBA_BFOR
PREM_PAID_TOT_2
STATE_AK
STATE_AR
STATE_AZ
STATE_CT
STATE_IA
STATE_IN
STATE_MD
STATE_MI
STATE_MN
STATE_OR
STATE_PARTNERSHIP
STATE_TX
STATE_UT
STATE_VA
STATE_WA
STATE_WI
STATE_WV
STATE_WY
TOTL_COMMUNICATED_INCR_RATE
);

proc hpforest data=TRNTST seed=12345 vars_to_try=20 leafsize=1 maxtrees=200;
target pfa_ind/level=binary;
input &varlist_int. / level=interval;
ods output fitstatistics=fitstats;
save file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_internalVars.bin";
run;

proc hp4score data=VAL;
id cust_ref plcy_ref weightVar PFA_IND;
score file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_internalVars.bin" out=VAL_scored;
run;

%crtCentilesCode(VAL_scored, p_pfa_ind1,weightVar,/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_internalVars_VAL_PFA.sas);

data VAL_rf_scored_centile;
set VAL_scored;
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_internalVars_VAL_PFA.sas";
keep plcy_ref cust_ref pfa_ind score10;
run;

