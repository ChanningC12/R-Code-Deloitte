options mprint;
OPTIONS OBS=MAX;
%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
/*%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Modeling_Varlist_RBO.sas";*/
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Stepwise_Varlist.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";

%let obs=max;

data  allVars;            
	set mreg.modeling_data_final ;     	
   	weightVar = 1;	
   	pol_cnt=1;
   	zero=0;
/*	%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_RBO.sas";*/
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

proc hpforest data=TRNTST seed=12345 vars_to_try=20 leafsize=1 maxtrees=250;
target rbo_ind/level=binary;
input &varlist_rf_internal. / level=interval;
ods output fitstatistics=fitstats;
save file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_RBO.bin";
run;

proc hp4score data=VAL;
id cust_ref plcy_ref eff_dt weightVar RBO_IND;
score file="/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/model_RBO.bin" out=VAL_scored;
run;

%crtCentilesCode(VAL_scored, p_rbo_ind1,weightVar,/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_VAL_RBO.sas);

data VAL_rf_scored_centile;
set VAL_scored;
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/MULTNOM/centiles_rf_VAL_RBO.sas";
run;


/*proc means data=VAL_rf_scored_centile;
var p_rbo_ind0 p_rbo_ind1;
class score10;
run;*/
