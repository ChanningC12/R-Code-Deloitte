options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Modeling_Varlist_NFO.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/reg_summary_macros.sas";
%LET obs=MAX;


	%macro initialize;
		proc datasets library=work memtype=data nolist;
		delete _all_;
		quit;
		
		%global analysis		; 	%let analysis = MODELING;
		%global model_type	;  	%let model_type = NFO;
		
		   %global modelNo split modFile target varlist varLstNm  
           referenceModelParmFile regtype dist link weight modelOptions 
           refVarList renameInterceptAs drawliftcurve  
           varsInSummary  summaryVars  keepVars;
	
		%let refVarList=%str(modfile regType weight varLstNmbr target split);
		
		%let modelNo=;
		%let regtype=;
		%let weight=weightVar;
		%let comment=;
		%let link=;
		%let dist=;
		%let referenceModelParmFile=; 
		%let renameInterceptAs=;
		%let drawliftcurve = N; 
		%let modelOptions=;
		
		%let varsInSummary = %str(pol_cnt 
			 NFO_IND_ratio_REL NFO_IND_ratio NFO_IND
RBO_IND_ratio_REL RBO_IND_ratio RBO_IND);
																	  									
		%let summaryVars =  %str(pol_cnt NFO_IND RBO_IND FULL_PAY_IND );
		
	%mend;	

%initialize;

/* Drops the Existing RegSummary Table. Appends if commented out */
proc sql;
drop table mreg.Summary_&model_type._PB;
quit;

/*************************************
Prepare the data into train, test and other subsets 
of data needed for modeling and lift curve
**************************************/
%let prepareParms=;

%macro crtView(where=, view=);
   data  &view / view=&view;          
      set allVars (obs=&obs where=(&where /***%applyFilter***/));
   run;
%mend crtView; 

%macro prepareData;

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

proc sort data=mreg.modeling_data_final_smote_12 out=mod_data_smote;
by plcy_ref eff_dt;
run;

proc sort data=mreg.modeling_data_final out=mod_data;
by plcy_ref eff_dt;
run;

data  allVars_smote;            
merge mod_data_smote(in=a) resp_sort(in=b) ;
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

%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";
	INTR_SPOUSE_SHARED =IF_SPOUSE_DISC*SHARED_POLICY;
	INTR_FPO_FAWP=IFA_GROUP_STATE_PROD_FAWP*
CNT_FPO_PREV1;
INTR1_SPOUSE_RATE_INCR = IF_SPOUSE_DISC * RATE_INCR_UC;
INTR2_RED_BEN = max(IF_RED_DBA_UC,IF_RED_BENPERIOD_UC,IF_RED_BIO_UC);
run;

data  allVars;            
merge mod_data(in=a) resp_sort(in=b) ;
by plcy_ref eff_dt;
if a;	
weightVar = 1;	
pol_cnt=1;
zero=0;

/** Create some predictive composite vars **/
TREE_NFO1A = 0;
TREE_NFO3A = 0;
IF cnt_fpo_prev1=0 and IFA_GROUP_STATE_PROD_FAWP=1 AND TOTL_COMMUNICATED_INCR_RATE>=4 
then tree_NFO1A = 1;
intr_rate_decision = rate_incr_pre*decision_stage;
intr_freq_bio = freq_bfor_q*BIO_BEFORE_NO_BIO;
IF decision_stage=1 and IFA_GROUP_STATE_PROD_FAWP=0 AND STATE_MD=0 AND RATE_INCR>=4 
then tree_NFO3A = 1;

IF cnt_fpo_prev1=0 and IFA_GROUP_STATE_PROD_FAWP=0 AND STATE_MD=0 AND RATE_INCR>=4 
then tree_NFO3B = 1;

POST_URS_VAR1 = ((ifa_group_state_prod_fa=1)+(benefit_pool_asset_ratio=10) = 2);
POST_URS_VAR2 = ((ifa_group_state_prod_fa=1)+(income_split="LESS THAN $40,000") = 2);
POST_URS_VAR3 = ((ifa_group_state_prod_fa=1)+(hh_income=2) = 2);

INTR1_SPOUSE_RATE_INCR = IF_SPOUSE_DISC * RATE_INCR_UC;
INTR2_RED_BEN = max(IF_RED_DBA_UC,IF_RED_BENPERIOD_UC,IF_RED_BIO_UC);

%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";
	INTR_SPOUSE_SHARED =IF_SPOUSE_DISC*SHARED_POLICY;
	INTR_FPO_FAWP=IFA_GROUP_STATE_PROD_FAWP*
CNT_FPO_PREV1;
run;

   %let ALL		 = (&split in("TRN","TST","VAL"));
   %let TRNTST = (&split in("TRN","TST"));
   %let TRN 	 = (&split = "TRN");
   %let TST 	 = (&split = "TST");
   %let VAL 	 = (&split = "VAL");
    
   %crtView(view=TRNTST,where=&TRNTST);
   %crtView(view=TRN,where=&TRN);
   %crtView(view=TST,where=&TST);
   %crtView(view=VAL,where=&VAL);
    
%mend;

	%macro getlift(infile=);
		%drawLiftCurve(infile=&infile,
	                   varList=&varList,
	                   modelNo=&modelNo,
	                   referenceVarList=&refVarList,
	                   outSummaryFile=mreg.Summary_&model_type._PB,
	                   parmfile=mreg.regOut_&modelNo,
	                   summaryVars=&summaryVars,
	                   ratioList=&ratioList., 
	                   weightField=&weight);
	%mend;

%macro runRegModel;

/*   proc printto print="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/LST/reg_&modelNo..lst"*/
/*                  log="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/LOGS/reg_&modelNo..log"  new; run;*/
/*   title1 "Model=&modelNo Target=&target reg_type=&regtype  modelingFile=&modFile ";*/
   
   %prepareData;
   
   %let varlist = %str(&&varlist&varLstNm.);
   %let varLstNmbr= &model_type._varlist&varlstNm.;
   
   %let ratioList = %str(
		NFO_IND_ratio NFO_IND	pol_cnt 1
		RBO_IND_ratio RBO_IND   pol_cnt 1
		);
		
	 %if %upcase(&regtype.)=STEPWISE %then %do;
	 		proc reg data=&modFile ;
	 		model &target = &varList/ VIF selection=stepwise;
	 		ods output SelectionSummary = mreg.regOut_&modelNo(rename=(VarEntered=variable step=tValue VarRemoved=estimate));
	 		ods output ParameterEstimates = mreg.statout&modelNo;
	 		run;
	 %end;
   %else %if &link= and &dist= %then %do;
      %do_reg(inFile=&modFile,
              Weight=&weight,
              Target=&target,
              VarList=&varList,
              RegType=&regType,
              outRegOut=mreg.regOut_&modelNo, 
							outStatFile=mreg.statout&modelNo, 
              referenceModelParmFile=&referenceModelParmFile,
              modelOptions=&modelOptions);
   %end;
   %else %do;
      %do_genmod(inFile=&modFile,
              Weight=&weight,
              Target=&target,
              VarList=&varList,
		          link=&link,
              dist=&dist,
              outRegOut=mreg.regOut_&modelNo,              
              referenceModelParmFile=&referenceModelParmFile,
              outStatFile=mreg.statout&modelNo,
              modelOptions=&modelOptions);
   %end;          
   %wrtScoreCode(unwindParmFile = mreg.regOut_&modelNo,outSAScode=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/SCORE/reg_&model_type._glm_&modelNo..sas,yhatFldName= pred_&model_type._&modelNo.,mod=mod);
   
         %unwindParms(regParmFile=mreg.regOut_&modelNo,
	                princompFile=mreg.princomp_NFO,
	                outParmFile=parmFile);

   
/*   %getlift(infile=TRN);*/
/*   %getlift(infile=TST);*/
/*   %getlift(infile=TRNTST);*/
/*   %getlift(infile=TRNTST_OS);*/
   %getlift(infile=VAL);
/*   %getlift(infile=MREG.FINAL_NFO_IND);*/
/*   %getlift(infile=MREG.FINAL_VALIDATION);*/
   
   proc printto ;run;
  

%mend;

%macro runRandomSplits;
	%do  num = 1 %to 1;
		%let split= rsplit&num.; 
		%getModelNo;
		%runRegModel;
	%end;
%mend runRandomSplits;

/*Stepwise regression with all variables*/

********NFO;
%initialize;

%let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 01;
%let target=NFO_IND; 
/*%getModelNo;*/
/*		%runRegModel;*/

		%let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 02;
%let target=NFO_IND; 
/*%getModelNo;*/
/*		%runRegModel;*/

%let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 03;
%let target=NFO_IND; 
/*%getModelNo;*/
/*		%runRegModel;*/
   %let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 04;
%let target=NFO_IND; 
/*%getModelNo;*/
/*		%runRegModel;*/

    %let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 05;
%let target=NFO_IND; 
/*%getModelNo;*/
/*		%runRegModel;*/
    %let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 06;
%let target=NFO_IND; 
/*%getModelNo;*/
/*%runRandomSplits;*/

     %let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
/*%let split = rsplit1;*/
%let varLstNm= 07;
%let target=NFO_IND;
/*%getModelNo; */
/*%runRandomSplits;*/

       %let regtype=;
%let modFile=TRN;  
%let dist = BINOMIAL;  
%let link = logit; 
%let split = rsplit1;
%let varLstNm= 08;
%let target=NFO_IND; 
/*%getModelNo; */
/*%runRandomSplits;*/

%let modFile=TRN;  
%let varLstNm= 09;
/*%getModelNo; */
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 10;
/*%getModelNo; */
/*%runRandomSplits;*/
/**Removed correlatedvar**/
%let modFile=TRN;  
%let varLstNm= 11;
/*%getModelNo; */
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 12;
/*%getModelNo; */
/*%runRandomSplits;*/
/**added the vars removed in iteration 9 except zagg_vote**/
%let modFile=TRn;  
%let varLstNm= 13;
/*%getModelNo; */
/*%runRandomSplits;*/
%let modFile=TRNTST; 
%let split = rsplit1; 
%let varLstNm= 14;
*%getModelNo;
*%runRegModel;

/*** Add back significant state indicators and some pc's which had good results initially ***/
%let modFile=TRN;  
%let varLstNm= 15;
/*%getModelNo;*/
/*%runRandomSplits;*/
/*** Add back  some pc's which had good results initially and some other variables***/
%let modFile=TRN;  
%let varLstNm= 16;
*%getModelNo;
*%runRandomSplits;
%let modFile=TRN;  
%let varLstNm= 17;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 18;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 21;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 22;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 23;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;  
%let varLstNm= 24;
/*%getModelNo;*/
/*%runRandomSplits;*/
%let modFile=TRN;
%let split = rsplit1; 
*%let varLstNm= 25;
/*%let varLstNm= 26;*/
%let modFile=TRNTST;
*%let modFile=allvars_smote;  
*%let modFile=mreg.modeling_data_final_undersamp;  
%let split = rsplit1; 
/*%let varLstNm= 25;*/
/*%let varLstNm= 28;*/
*%let varLstNm= 61;
%let varLstNm= 63;
%getModelNo;
%runRandomSplits;

***************************************************************************
								OUTPUT REGRESSION SUMMARY REPORT
**************************************************************************;
     %getSummaryInExcel(inSummaryFile=mreg.Summary_&model_type._PB,
                   firstModelNo= 12000,
                   lastModelNo=  12999,
                   referenceVarList=&refVarList,
                   varsInSummary=&varsInSummary,
                   outCSVFile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/CSV/reg_&model_type._bin_logit_summary.csv);

       %xls(xlsout= /PROJECTS/GENWORTH_2016/02_CODES/MODELING/CSV/reg_&model_type._bin_logit_summary.xls);
	   %reg_summary(referenceVarList=modelNo infile fieldName &refVarList,doAvg5=N); 
	   %let corrFile=statout&modelno.; 
	   %xlsCorr(TRNTST,mreg,&modelno.);
	   %model_results_logistic(modelNo = %str(12631 /*12252 12253 12254 12255*/) ,
              princompFile=mreg.princomp_NFO,
              modLbl= %str(),
              sheetLabel= PEs);
