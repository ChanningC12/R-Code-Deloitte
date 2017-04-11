options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";





*********************************************************************************************************************;
DATA MREG.VAL MREG.TRNTST;
SET PRED.ALLVARS_CAPPED_MISS;
IF YEAR(EFF_DT)>=2016 then output mreg.val;
else OUTPUT mreg.trntst;
run;



*********************************************************************************************
RUNNING THE DO RANDOM SPLITS MACRO ON TRNTST
*********************************************************************************************;

%doRandomSplit(infile=mreg.trnTst, outfile=mreg.trnTst, numSplits=5, SplitBy=PLCY_REF DECISION_STAGE, numBuckets=10);

******************************************************************************************************************
PREPARING MODELING DATA WITH SPLITS (RE ATTACHING ALL TRNTST AND VAL RECORDS 
******************************************************************************************************************;

%macro attachSplits;

  data allvars (keep = PLCY_REF DECISION_STAGE rsplit: );
   set mreg.trntst 
       mreg.val;

 
   %do i = 1 %to 5;
  		if missing(random&i.) then do; RSPLIT&i = "VAL";  end;
  	    	 else if random&i < 7 then RSPLIT&i = "TRN";
  	    	 else RSPLIT&i = "TST";
 	 %end;   
  run;
 
  proc sort data=allvars; by DECISION_STAGE PLCY_REF; run;
	proc sort data=pred.allvars_capped_miss out=modeling_data; by DECISION_STAGE PLCY_REF; run;

  data mreg.modeling_data_final;
     merge modeling_data(in= a) allvars ;
     by DECISION_STAGE PLCY_REF ;
     if a;
	run;
	
	proc contents data = mreg.modeling_data_final; run;
	proc print data = mreg.modeling_data_final (obs = 1000); var DECISION_STAGE PLCY_REF rsplit1-rsplit5; run;
	
	
%mend;

%attachSplits;


proc freq data = mreg.modeling_data_final;
tables rsplit1 - rsplit5/missing;
run;



