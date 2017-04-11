options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/reg_summary_macros.sas";
%LET obs=MAX;


	%macro initialize;
		
		
		%global analysis		; 	%let analysis = STEPWISE;
		%global model_type	;  	%let model_type = RBO;
		
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
			RBO_IND_ratio_REL RBO_IND_ratio RBO_IND NFO_IND_ratio_REL NFO_IND_ratio NFO_IND);
																	  									
		%let summaryVars =  %str(pol_cnt NFO_IND RBO_IND FULL_PAY_IND );
		
	%mend;	

%initialize;

/* Drops the Existing RegSummary Table. Appends if commented out */
/*proc sql;
drop table mreg.Summary_&model_type.;
quit;*/

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
   data  allVars;            
   	set mreg.modeling_data_final ;     	
   	weightVar = 1;	
   	pol_cnt=1;
   	zero=0;
   	   	
   	KEEP 
		PLCY_REF
	CUST_REF
	&VARLIST01.
DECISION_STAGE
EFF_DT FULL_PAY_IND RBO_IND NFO_IND POL_CNT ZERO WEIGHTVAR
RSPLIT:;


	 run;
		
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
   
%mend;

	%macro getlift(infile=);
		%drawLiftCurve(infile=&infile,
	                   varList=&varList,
	                   modelNo=&modelNo,
	                   referenceVarList=&refVarList,
	                   outSummaryFile=mreg.Summary_&model_type.,
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
		RBO_IND_ratio RBO_IND	pol_cnt 1
		NFO_IND_ratio NFO_IND	pol_cnt 1
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

********DECLINE;
%initialize;

%let regtype=STEPWISE;
%let modFile=TRNTST;   
%let split = rsplit1;
%let varLstNm= 01;
%let target=RBO_IND; 
%getModelNo;
		%runRegModel;



	
