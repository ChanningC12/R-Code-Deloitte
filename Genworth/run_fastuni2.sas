/****************************************************************************************************************************
 program:      run_fastuni2.sas
 author:       Genworth Team
 date:         4 November, 2016
 purpose:      To create URS dataset
 Client:       Genworth

Note:
Input dataset(s):

Output dataset(s):

*****************************************************************************************************************************/
options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/UNI/varlist.sas";


%let dateStamp = %sysfunc(putn(%sysfunc(TODAY()),yymmdd10.));

options validvarname=any;

options obs=MAX;

data base_URS_data;
	set pred.allvars_precap_plv;
	TUOC_1=SUBSTR(TUOC,1,1);
    TUOCS_1=SUBSTR(TUOCS,1,1);
	CRRT_1=SUBSTR(CRRT,1,1);

	DROP CRRT TUOC  TUOCS ;
	RENAME CRRT_1=CRRT
	TUOC_1=TUOC
	TUOCS_1=TUOCS
;
	
	format AGE_SPLIT $20. GENDER_SPLIT $10. INCOME_SPLIT $20. DECISION_SPLIT $20.
IFA_GROUP_STATE_PROD_SPLIT $30. RATE_INCR_SPLIT $30. CUM_RATE_INCR_SPLIT $30.;

IF RATE_INCR<25 THEN RATE_INCR_SPLIT="< 25";
ELSE IF RATE_INCR>=25 THEN RATE_INCR_SPLIT=">= 25";
IF CUM_RATE_INCR>=0 AND CUM_RATE_INCR<20 THEN CUM_RATE_INCR_SPLIT="0-20";
/*
ELSE IF CUM_RATE_INCR>=20 AND CUM_RATE_INCR<40 THEN CUM_RATE_INCR_SPLIT="20-40";
ELSE IF CUM_RATE_INCR>=40  THEN CUM_RATE_INCR_SPLIT="40-High";
*/	
	AGE_SPLIT =AGE_BIN;

		IF UPCASE(IFA_GROUP_STATE_PROD) = 'FULL APPROVAL W/ PHASES'  THEN
		IFA_GROUP_STATE_PROD_SPLIT = 'FULL APPROVAL W/ PHASES';
		ELSE IF UPCASE(IFA_GROUP_STATE_PROD) = 'FULL APPROVAL' THEN
      IFA_GROUP_STATE_PROD_SPLIT = 'FULL APPROVAL';
	ELSE IF UPCASE(IFA_GROUP_STATE_PROD) = 'OTHER THAN FULL APPROVAL' THEN
		IFA_GROUP_STATE_PROD_SPLIT = 'OTHER THAN FULL APPROVAL';
	
	if missing(GENDER)or gender ="U" then GENDER_SPLIT = "UNKNOWN";
	else IF GENDER = "M" THEN GENDER_SPLIT="Male";
	else if gender = "F" then gender_split="Female";
	else if gender ="S" then gender_split="Shared";

	/*
	if missing(DECISION_STAGE) then DECISION_SPLIT = "Missing";
	else if DECISION_STAGE =1 then  DECISON_SPLIT ="FIRST_STAGE";
	else if DECISION_STAGE =2 then  DECISON_SPLIT ="SECOND_STAGE";
	else if DECISION_STAGE =3 then  DECISON_SPLIT ="THIRD_STAGE";
	*/

	
	if missing(ESTINC30) then INCOME_SPLIT = "UNKNOWN";
	else if ESTINC30 in ("A","B","C","D") then INCOME_SPLIT = "LESS THAN $40,000";
	else if ESTINC30 in ("E","F") then INCOME_SPLIT = "$40,000 - $59,999";
	else if ESTINC30 = ("G") then INCOME_SPLIT = "$60,000 - $74,999";
	else if ESTINC30 = ("H") then INCOME_SPLIT = "$75,000 - $99,999";
	else if ESTINC30 = ("I") then INCOME_SPLIT = "$100,000 - $124,999";
	else if ESTINC30 in ("J","K","L","M","N","O") then INCOME_SPLIT = "$125,000+";

	if missing(STATE)  then  STATE_SPLIT = "UNKNOWN";
	else if STATE="CA" then STATE_SPLIT = "CA";
	else if STATE="NY" then STATE_SPLIT = "NY";
	else if STATE="PA" then STATE_SPLIT = "PA";
	else if STATE="IL" then STATE_SPLIT = "IL";
	else if STATE="TX" then STATE_SPLIT = "TX";
	else STATE_SPLIT="OTHERS";
	drop
&DROPLIST.;
run;

**** declare global variable to be used my various macros;
%let inCapFile=/PROJECTS/GENWORTH_2016/02_CODES/UNI/Capping_all.sas;
**** Create a text file of variable capping def;
%getVarDef(inCapFile=&inCapFile,outTxtFile=/PROJECTS/GENWORTH_2016/02_CODES/UNI/vardef.txt);
   

%macro run_uni(infile = , outfile = , outtxt_file = );

%let outuni = uni;

%let splits = %str(ALL AGE_SPLIT GENDER_SPLIT 
 INCOME_SPLIT STATE_SPLIT /*DECISION_SPLIT*/ IFA_GROUP_STATE_PROD_SPLIT CUM_RATE_INCR_SPLIT
RATE_INCR_SPLIT); 

data all;
set &infile;
FULL_PAY_IND			 = (RESPONSE="FULL PAY");
NFO_IND      			 = (RESPONSE="NFO");
RBO_IND   			 = (RESPONSE="RBO");

run;

%doCapping(inDataLib=work,inDataFile=all,inCapFile=&inCapFile,outDataFile=pred.&outfile,outCapVarList=varlist_temp,runFreq=y,crtIndVar=y,assignFmt=n);


data final_allvars;
   set pred.&outfile;
   all = "ALL";
   num_pol=1;

   KEEP  num_pol all
FULL_PAY_IND NFO_IND RBO_IND &HIGH_MED_LIST. &splits.;
run;


*Run %fastUni2 that will generate a summarized output sas dataset;

%fastuni2(dataset=final_allvars,
          list= ALL &HIGH_MED_LIST.,
          splits=&splits,
          where=1,
    	  	
    	  	ratioList=
					FULL_PAY_RATIO  			FULL_PAY_IND NUM_POL
					NFO_RATIO        			NFO_IND       NUM_POL
					RBO_RATIO     			RBO_IND    NUM_POL
		
								,                    
          pctvars = num_pol,
          out=pred.&outuni._100pct,
          varsPerItr=55,
          
          keep= Split1
                Level1
                Split2
                Level2
                num_pol 
                pct_num_pol
                FULL_PAY_IND
				NFO_IND      
	         	RBO_IND   
								
                FULL_PAY_RATIO  			
				NFO_RATIO        
				RBO_RATIO 
                FULL_PAY_RATIO_REL  			
				NFO_RATIO_REL        
				RBO_RATIO_REL
             ,
          inCapFile=&inCapFile,
          outtxt=&outtxt_file.
          );

	
*Remove format assignments from the input dataset;
%unassignFmt(pred, &outfile);



%mend;

%run_uni(infile = base_URS_data, outfile=allvars_capped, outtxt_file = /PROJECTS/GENWORTH_2016/02_CODES/UNI/uni.txt);







PROC IMPORT OUT= pred.Variable_list
            DATAFILE= "/PROJECTS/GENWORTH_2016/02_CODES/UNI/Predictive_Variable_List_New.csv" 
            DBMS=DLM REPLACE;
     DELIMITER=","; 
     GETNAMES=YES;
     DATAROW=2; 
     GUESSINGROWS=32767;
 
RUN;



data uni_all;
set pred.uni_100pct;
rename Split2=Variable;
run;

proc sql;
create table pred.uni_all as select  a.Split1 as Split, a.Level1 as Split_Values, a.Variable, 
a.Level2 as Variable_Values, a.num_pol as Policy_Count,a.pct_num_pol as Pct_pol_Cnt, a.FULL_PAY_IND as FULL_PAY_Count, 
a.RBO_IND as RBO_Count, a.NFO_IND as NFO_Count,  a.FULL_PAY_RATIO, a.RBO_RATIO, a.NFO_RATIO, b.Description, b.Source,b.Ranking_NFO,
b.Ranking_RBO,

a.FULL_PAY_RATIO_rel as FULL_PAY_Rel, 
a.RBO_RATIO_rel as RBO_Rel, a.NFO_RATIO_rel as NFO_Rel
from uni_all as a left join pred.Variable_list as b on a.Variable=b.Variable; quit;
		
proc export data = pred.uni_all outfile = "/PROJECTS/GENWORTH_2016/02_CODES/UNI/CSV/uni_cc_&dateStamp..csv"
	dbms=CSV replace;
run;

*Create a zip file with the variable def uni txt fields;
*%zipIt(files=vardef.txt uni.txt zipfile = uni.zip);


