%let sampleRun=NO;

options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
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

proc sort data=mreg.modeling_data_final out=mod_data;
by plcy_ref eff_dt;
run;

data traintest;
	merge mod_data(in=a) resp_sort(in=b) ;
	by plcy_ref eff_dt;
	if a;	
	INTR1_SPOUSE_RATE_INCR = IF_SPOUSE_DISC * RATE_INCR_UC;
	INTR2_RED_BEN = max(IF_RED_DBA_UC,IF_RED_BENPERIOD_UC,IF_RED_BIO_UC);
	INTR_SPOUSE_SHARED =IF_SPOUSE_DISC*SHARED_POLICY;
	INTR_FPO_FAWP=IFA_GROUP_STATE_PROD_FAWP*
tree_NFO1*
CNT_FPO_PREV1;

	if rsplit1 in ("TRN","TST");
run;

************Principal Component Analysis for NFO Model ****************;

%let PCfile=mreg.princomp_NFO;
%let sascodeFile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas;

%let varList=%str(
ZAGG_ESTINC30_500
ZAGG_ESTINC30_035
ZAGG_C210EBI
ZAGG_ESTINC30_045
ZAGG_C210CIP
ZAGG_C210BPVT
ZAGG_BPAMEXY
)
;
%prinComp(inFile=traintest, prefix=pc_financial);

%let varList=%str(
ZAGG_MOBPLUS_M
ZAGG_OAMEN
ZAGG_OAGEN
ZAGG_OCHILDPR
ZAGG_OSTATION
ZAGG_OFURNITR
ZAGG_RC_M
ZAGG_OFOOD
ZAGG_RATEEN
ZAGG_BUYER
)
;
%prinComp(inFile=traintest, prefix=pc_buyer);



%let varList=%str(
HH_TOTALALLCREDIT_60DPD
NUM_BANKCARD_SEVEREDEROG
BAL_NONMTGCREDIT_60DPD

)
;
%prinComp(inFile=traintest, prefix=pc_Credit);

%let varList=%str(
BINARY_HYPT_TRANS
BINARY_DIAB_TRANS
POLICYHOLDER_AGE
)
;
%prinComp(inFile=traintest, prefix=pc_age_ds);

%let varList=%str(
HIGHCRD_HELOC_NEW
BAL_HELOC_NEW


)
;
%prinComp(inFile=traintest, prefix=pc_equity);

%let varList=%str(
HIGHCRD_AGENCYFIRSTMTG_NEW
BAL_1STMTGCREDIT_NEW


)
;
%prinComp(inFile=traintest, prefix=pc_MRTGAGE);


%let varList=%str(
NOC19
HHCOMP_A
)
;
%prinComp(inFile=traintest, prefix=pc_child);


%let varList=%str(
ZAGG_ISCA
ZAGG_IHGREEN
)
;
%prinComp(inFile=traintest, prefix=pc_SURVEY);
%let varList=%str(
ZAGG_HOMSTAT_Y
ZAGG_HOMSTAT_U
ZAGG_HOMSTAT_T
)
;
%prinComp(inFile=traintest, prefix=pc_HOMSTAT);

%let varList=%str(
RATE_INCR_PRE
DECISION_STAGE
CNT_FPO_PREV1
)
;
%prinComp(inFile=traintest, prefix=pc_rate);

%let varList=%str(
TOTAL_AMOUNT_PAID
CLAIM_COUNT_DYNAMIC
)
;
%prinComp(inFile=traintest, prefix=pc_claim);

%let varList=%str(
TOTL_COMMUNICATED_INCR_RATE
IFA_GROUP_STATE_PROD_FAWP

)
;
%prinComp(inFile=traintest, prefix=pc_response_rate);

%let varList=%str(
TREE_NFO3
NUM_DECISIONS
)
;
%prinComp(inFile=traintest, prefix=pc_dcsn_rate);

%let varList=%str(
TREE_NFO3
NUM_DECISIONS
RATE_INCR
IFA_GROUP_STATE_PROD_FAWP
)
;
%prinComp(inFile=traintest, prefix=PC_DCSN_RT_COMP);


%let varList=%str(
SHARED_POLICY
IF_SPOUSE_DISC)
;
%prinComp(inFile=traintest, prefix=PC_SHARED);
%let varList=%str(
NETW
RATE_INCR_AFFORD)
;
%prinComp(inFile=traintest, prefix=PC_rate_income);

%let varList=%str(
TREE_NFO3b
rate_incr
)
;
%prinComp(inFile=traintest, prefix=pc_dcsn_rt);

%let varList=%str(
RATE_INCR_UC
INTR1_SPOUSE_RATE_INCR
tree_NFO3B
IFA_GROUP_STATE_PROD_FA
)
;
%prinComp(inFile=traintest, prefix=pc_rate_incr_spouse);

%let varList=%str(
INTR2_RED_BEN
IF_RED_BENPERIOD_UC
IF_RED_DBA_UC
IF_RED_BIO_UC
)
;
%prinComp(inFile=traintest, prefix=pc_red_ben);

%let varList=%str(
SHARED_POLICY
IF_SPOUSE_DISC
BENE_TRM_BFOR_UC
)
;
%prinComp(inFile=traintest, prefix=pc_shared_pol);





