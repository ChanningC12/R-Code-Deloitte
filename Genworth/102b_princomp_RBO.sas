%let sampleRun=NO;

options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";

data traintest;
     set mreg.modeling_data_final;
	 if rsplit1 in ("TRN","TST");
	 run;

************Principal Component Analysis for RBO Model ****************;

%let PCfile=mreg.princomp_RBO;
%let sascodeFile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_RBO.sas;

%let varList=%str(
ZAGG_NETW30_1000
ZAGG_NETW30_0000
ZAGG_NETW30_0019
ZAGG_ESTINC30_500
ZAGG_ESTINC30_017
ZAGG_ESTINC30_045
)
;
%prinComp(inFile=traintest, prefix=PC_INCOME_ZAGG);
/*
%let varList=%str(
C210HMI
C210CIP
)
;
%prinComp(inFile=traintest, prefix=PC_INCOME_CEN);
*/

%let varList=%str(
BNKI
BANK
)
;
%prinComp(inFile=traintest, prefix=PC_BANK);

%let varList=%str(
BONDOLLR
BAVGONDL
)
;
%prinComp(inFile=traintest, prefix=PC_BUYER_IND);

%let varList=%str(
HIGHCRD_NONMTGCREDIT
HIGHCRD_HELOC_NEW
BAL_NONAGNFIRSTMTG
BAL_HELOC_NEW
BAL_1STMTGCREDIT
)
;
%prinComp(inFile=traintest, prefix=PC_HIGHBALANCE);

%let varList=%str(
NUM_NONMTGCREDIT_60DPD
HH_TOTALALLCREDIT_SEVEREDEROG
HH_BANKCARDCREDIT_60DPD
)
;
%prinComp(inFile=traintest, prefix=PC_SEVCREDIT);

%let varList=%str(
BINARY_NM_TRANS
BINARY_NF_TRANS
)
;
%prinComp(inFile=traintest, prefix=PC_DS_NEO);

%let varList=%str(
POLICYHOLDER_AGE
BINARY_HYPT_TRANS
BINARY_CARDIO_TRANS
BINARY_DIAB_TRANS
BINARY_NO_TRANS
SEV_NT_TRANS
BINARY_CERV_TRANS
)
;
%prinComp(inFile=traintest, prefix=PC_AGE_DS);

%let varList=%str(
NETW
HH_INCOME
RATE_INCR_AFFORD
C210HMI
C210CIP
C210HVA
)
;
%prinComp(inFile=traintest, prefix=PC_INC_AFFORD_CEN);

%let varList=%str(
DECISION_STAGE
CNT_FPO_PREV1
RATE_INCR_PRE
)
;
%prinComp(inFile=traintest, prefix=PC_DEC_RATE_INC);

%let varList=%str(
HH_TOTALALLCREDIT_60TO89DPD
HH_BANKCARD_60TO89DPD
)
;
%prinComp(inFile=traintest, prefix=PC_CREDIT_1V);

%let varList=%str(
/*tree_var3*/
CNT_RBO_PREV1
tree_var2
)
;
%prinComp(inFile=traintest, prefix=PC_PREV_RESP_RATE);
