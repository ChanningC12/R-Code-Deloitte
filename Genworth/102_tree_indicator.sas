options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";

data tree_ind_rbo;
set mreg.modeling_data_final;
/*drop tree_var1 - tree_var3;*/
run;

data tree_ind_rbo;
set tree_ind_rbo;
if RATE_INCR< 5.5
	and CNT_RBO_PREV1< 0.5
	and DECISION_STAGE< 1.5
	and TOTL_COMMUNICATED_INCR_RATE>=3.5
	and TOTL_COMMUNICATED_INCR_RATE< 5.5
	and BENE_TRM_BFOR< 0.5
then tree_var1 = 1;
else tree_var1 = 0;

if RATE_INCR< 5.5
	and CNT_RBO_PREV1>=0.5
then tree_var2 = 1;
else tree_var2 = 0;

if RATE_INCR>=5.5
	and BENEFIT_POOL_ASSET_RATIO< 5.5
	and BENE_TRM_BFOR< 0.5
	and BENEFIT_POOL_ASSET_RATIO< 4.5
then tree_var3 = 1;
else tree_var3 = 0;

if RATE_INCR>=5.5
	and BENEFIT_POOL_ASSET_RATIO< 5.5
	and BENE_TRM_BFOR>= 0.5
	and ANNL_PREM_BFOR< 7.5
	and ANNL_PREM_BFOR>=3.5
then tree_var4 = 1;
else tree_var4 = 0;
run;


data tree_ind_rbo;
set tree_ind_rbo;
if tree_var1< 0.5 and tree_var3< 0.5 and RATE_INCR< 6.5 and CNT_RBO_PREV1< 0.5 and
    STATE_TX< 0.5 and ANNL_PREM_BFOR< 7.5 and DECISION_STAGE< 1.5 and BENEFIT_POOL_ASSET_RATIO< 5.5 and
    ANNL_PREM_BFOR>=2.5 and BENE_TRM_BFOR< 0.5 and BENEFIT_POOL_ASSET_RATIO>=4.5 and NETW<4.5
then tree_var_new1=1;
else tree_var_new1=0;

if tree_var1< 0.5 and tree_var3< 0.5 and RATE_INCR< 6.5 and CNT_RBO_PREV1< 0.5 and
    STATE_TX< 0.5 and ANNL_PREM_BFOR< 7.5 and DECISION_STAGE< 1.5 and BENEFIT_POOL_ASSET_RATIO< 5.5 and
    ANNL_PREM_BFOR>=2.5 and BENE_TRM_BFOR< 0.5 and BENEFIT_POOL_ASSET_RATIO<4.5
then tree_var_new2=1;
else tree_var_new2=0;

if  tree_var1< 0.5 and tree_var3< 0.5 and RATE_INCR< 6.5 and CNT_RBO_PREV1< 0.5 and
    STATE_TX< 0.5 and ANNL_PREM_BFOR>= 7.5 and DECISION_STAGE< 1.5 and BENEFIT_POOL_ASSET_RATIO< 5.5 and 
    BENE_TRM_BFOR< 0.5 and BENEFIT_POOL_ASSET_RATIO< 4.5 and POLICYHOLDER_AGE< 8.5
then tree_var_new3=1;
else tree_var_new3=0;

if tree_var1< 0.5 and tree_var3< 0.5 and RATE_INCR< 6.5 and CNT_RBO_PREV1< 0.5 and
    STATE_TX< 0.5 and ANNL_PREM_BFOR>= 7.5 and SHARED_POLICY>= 1.5 and PC_DS_NEO1_RBO< -0.447258
    /*NUM_DECISIONS< 1.5*/
then tree_var_new4=1;
else tree_var_new4=0;

if tree_var1< 0.5 and tree_var3< 0.5 and RATE_INCR< 6.5 and CNT_RBO_PREV1< 0.5 and
    STATE_TX>= 0.5 and BENE_TRM_BFOR< 0.5
then tree_var_new5=1;
else tree_var_new5=0;

if tree_var1< 0.5 and tree_var3>= 0.5 and NETW>=2 and POLICYHOLDER_AGE< 8.5 and ORIG_DBA_BFOR>=1.5
then tree_var_new6=1;
else tree_var_new6=0;

/* Priyam suggested */
if RATE_INCR>=6 and BENEFIT_POOL_ASSET_RATIO<6
then tree_var_new7=1;
else tree_var_new7=0;

/* Added on Jan 6th */
if RATE_INCR< 5.5 and CNT_RBO_PREV1< 0.5 and DECISION_STAGE< 1.5 and ANNL_PREM_BFOR< 6.5
and BENEFIT_POOL_ASSET_RATIO< 6.5 and BENE_TRM_BFOR< 0.5 and BENEFIT_POOL_ASSET_RATIO< 4.5
and NETW>=3.5
then tree_variable1 = 1;
else tree_variable1 = 0;

if RATE_INCR< 5.5 and CNT_RBO_PREV1< 0.5 and DECISION_STAGE< 1.5 and TOTL_COMMUNICATED_INCR_RATE>=3.5
and TOTL_COMMUNICATED_INCR_RATE< 5.5 and BENE_TRM_BFOR< 0.5
then tree_variable2 = 1;
else tree_variable2 = 0;

if RATE_INCR< 5.5 and CNT_RBO_PREV1>= 0.5
then tree_variable3 = 1;
else tree_variable3 = 0;

if RATE_INCR>= 5.5 and TOTL_COMMUNICATED_INCR_RATE>=3.5 and ANNL_PREM_BFOR>=6.5 and ANNL_PREM_BFOR>=9.5
then tree_variable4 = 1;
else tree_variable4 = 0;

if RATE_INCR>= 5.5 and TOTL_COMMUNICATED_INCR_RATE>=3.5 and ANNL_PREM_BFOR<6.5 and 
STATE_MI< 0.5 and ANNL_PREM_BFOR>=2.5 and RATE_INCR< 6.5 and BENE_TRM_BFOR< 0.5
then tree_variable5 = 1;
else tree_variable5 = 0;

if RATE_INCR>= 5.5 and TOTL_COMMUNICATED_INCR_RATE>=3.5 and ANNL_PREM_BFOR<6.5 and 
STATE_MI>= 0.5 and TOTL_COMMUNICATED_INCR_RATE< 5.5
then tree_variable6 = 1;
else tree_variable6 = 0;

if RATE_INCR>= 5.5 and TOTL_COMMUNICATED_INCR_RATE>=3.5 and ANNL_PREM_BFOR>=9.5 and 
ZAGG_VOTE>= 4.5 and BINARY_NF_TRANS>= 0.08103254
then tree_variable7 = 1;
else tree_variable7 = 0;

/* Add interaction variables */
INTR1_RBOPREV_DEC = CNT_RBO_PREV1*DECISION_STAGE;
INTR2_FAWP_DEC = IFA_GROUP_STATE_PROD_FAWP*DECISION_STAGE;
INTR3_RATE_FA = IFA_GROUP_STATE_PROD_FA*RATE_INCR;
INTR4_INCOME_PREM = ANNL_PREM_BFOR*HH_INCOME;
INTR5_BENEFIT_INCOME = BENEFIT_POOL_ASSET_RATIO*HH_INCOME;
INTR6_AFFORD_INCOME = RATE_INCR_AFFORD*HH_INCOME;
INTR7_FA_DEC = IFA_GROUP_STATE_PROD_FA*DECISION_STAGE;
run;






data mreg.modeling_data_final;
set tree_ind_rbo;
run;
