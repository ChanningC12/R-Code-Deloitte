%let sampleRun=NO;

options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";

data mod_data_trntst;
set mreg.modeling_data_final;
where rsplit1 in ("TRN","TST");
run;

data mod_tt_non_nfo mod_tt_nfo;
set mod_data_trntst;
if nfo_ind=1 then output mod_tt_nfo;
else output mod_tt_non_nfo;
run;

proc surveyselect data=mod_tt_non_nfo out=samp_non_nfo N=85853 method=srs seed=12345;
run;

proc freq data=samp_non_nfo;
tables nfo_ind rbo_ind/missing;
run;

data undersamp_nfo;
set samp_non_nfo mod_tt_nfo;
run;

proc sort; by plcy_ref eff_dt; run;

data resp_uncapped;
set raw.response_data;
TOTAL_COMM_INCR_RATE_UC = TOTL_COMMUNICATED_INCR_RATE;
RATE_INCR_UC = RATE_INCR;
ANNL_PREM_BFOR_UC = ANNL_PREM_BFOR;
ORIG_DBA_BFOR_UC = ORIG_DBA_BFOR;
ELIM_PRD_BFOR_UC = ELIM_PRD_BFOR;
BENE_TRM_BFOR_UC = BENE_TRM_BFOR;
IF_RED_DBA_UC = IF_RED_DBA;
IF_RED_BIO_UC = IF_RED_BIO;
IF_RED_BENPERIOD_UC = IF_RED_BENPERIOD;
IF_RED_ELIM_UC = IF_RED_ELIM;
RBO_NUM_CHANGES_UC = RBO_NUM_CHANGES;
BIO_BEFORE_5PCT_CPD = (BIO_BEFORE="5% Compound");
BIO_BEFORE_5PCT_SMP = (BIO_BEFORE="5% Simple");
STATE_MN = (STATE="MN");
STATE_MD = (STATE="MD");
STATE_AR = (STATE="AR");
STATE_LA = (STATE="LA");
STATE_SD = (STATE="SD");
STATE_VA = (STATE="VA");
STATE_GA = (STATE="GA");
STATE_ND = (STATE="ND");
STATE_KY = (STATE="KY");
keep plcy_ref eff_dt TOTAL_COMM_INCR_RATE_UC RATE_INCR_UC ANNL_PREM_BFOR_UC ORIG_DBA_BFOR_UC ELIM_PRD_BFOR_UC
BENE_TRM_BFOR_UC IF_RED_DBA_UC IF_RED_BIO_UC IF_RED_BENPERIOD_UC IF_RED_ELIM_UC RBO_NUM_CHANGES_UC BIO_BEFORE_5PCT_CPD
BIO_BEFORE_5PCT_SMP STATE_MN STATE_MD STATE_AR STATE_LA STATE_SD STATE_VA STATE_GA STATE_ND STATE_KY;
run;

proc sort data=resp_uncapped out=resp_sort;
by plcy_ref eff_dt;
run;

data mreg.modeling_data_final_undersamp; 
merge undersamp_nfo(in=a) resp_sort(in=b) ;
by plcy_ref eff_dt;
if a;	
weightVar = 1;	
pol_cnt=1;
zero=0;

/** Create some predictive composite vars **/
TREE_NFO1A = 0;
TREE_NFO1B = 0;
TREE_NFO3A = 0;
IF cnt_fpo_prev1=0 and IFA_GROUP_STATE_PROD_FAWP=1 AND TOTL_COMMUNICATED_INCR_RATE>=4 
then tree_NFO1A = 1;

IF cnt_fpo_prev1=1 and IFA_GROUP_STATE_PROD_FAWP=1 AND TOTL_COMMUNICATED_INCR_RATE<4 
then tree_NFO1B = 1;

IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=0 AND STATE_MD=0 AND RATE_INCR>=4 
then tree_NFO3A = 1;

POST_URS_VAR1 = ((ifa_group_state_prod_fa=1)+(benefit_pool_asset_ratio=10) = 2);
POST_URS_VAR2 = ((ifa_group_state_prod_fa=1)+(income_split="LESS THAN $40,000") = 2);
POST_URS_VAR3 = ((ifa_group_state_prod_fa=1)+(hh_income=2) = 2);

INTR_SPOUSE_SHARED =IF_SPOUSE_DISC*SHARED_POLICY;
INTR_FPO_FAWP=IFA_GROUP_STATE_PROD_FAWP*CNT_FPO_PREV1;
INTR1_SPOUSE_RATE_INCR = IF_SPOUSE_DISC * RATE_INCR_UC;
INTR2_RED_BEN = max(IF_RED_DBA_UC,IF_RED_BENPERIOD_UC,IF_RED_BIO_UC);
intr_rate_decision = rate_incr_pre*decision_stage;
intr_freq_bio = freq_bfor_q*BIO_BEFORE_NO_BIO;
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";
run;

data allVars;            
	set mreg.modeling_data_final ;     	
   	weightVar = 1;	
   	pol_cnt=1;
   	zero=0;
	/*%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";*/
	POST_URS_VAR1 = ((ifa_group_state_prod_fa=1)+(benefit_pool_asset_ratio=10) = 2);
	POST_URS_VAR2 = ((ifa_group_state_prod_fa=1)+(income_split="LESS THAN $40,000") = 2);
	POST_URS_VAR3 = ((ifa_group_state_prod_fa=1)+(hh_income=2) = 2);

	TREE_NFO1A = 0;
	TREE_NFO3A = 0;
	IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=1 AND TOTL_COMMUNICATED_INCR_RATE>=4 
	then tree_NFO1A = 1;

	IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=0 AND STATE_MD=0 AND RATE_INCR>=4 
	then tree_NFO3A = 1;

	intr_rate_decision = rate_incr_pre*decision_stage;
	intr_freq_bio = freq_bfor_q*BIO_BEFORE_NO_BIO;
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
    
   %crtView(view=TRNTST,where=&TRNTST);
   %crtView(view=TRN,where=&TRN);
   %crtView(view=TST,where=&TST);
   %crtView(view=VAL,where=&VAL);

proc hpforest data=mreg.modeling_data_final_undersamp seed=12345 vars_to_try=5 leafsize=1 maxtrees=500;
target nfo_ind/level=binary;
input &varlist_rf1. / level=interval;
ods output fitstatistics=fitstats;
save file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_undersamp.bin";
run;

proc hp4score data=VAL;
id cust_ref plcy_ref weightVar NFO_IND;
score file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_NFO_undersamp.bin" out=val_scored;
run;

%crtCentilesCode(val_scored, p_nfo_ind1,weightVar,/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_undersamp_NFO.sas);

data val_rf_scored_centile;
set val_scored;
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_undersamp_NFO.sas";
run;
