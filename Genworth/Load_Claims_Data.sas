%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";

/**/
/*proc import datafile='/PROJECTS/GENWORTH_2016/RAW/DELOITTE_CLAIMS_TAKE2.txt'*/
/*OUT=CLAIMS_DATA DBMS=DLM REPLACE; DELIMITER='09'x;GETNAMES=YES;RUN;*/

			
proc printto print="/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/LST/LOAD_CLAIMS_DATA_&dateStamp..lst"
log="/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/LOGS/LOAD_CLAIMS_DATA_&dateStamp..log"  new; run;
%let dateStamp = %sysfunc(putn(%sysfunc(TODAY()),yymmdd10.));


DATA RAW.CLAIMS_DATA;

	INFILE '/PROJECTS/GENWORTH_2016/RAW/DELOITTE_CLAIMS_TAKE2.txt' DELIMITER='09'X MISSOVER DSD LRECL=32767 FIRSTOBS=2 ;

informat	CUST_REF	best32.	;
informat	PLCY_REF	best32.	;
informat	CLAIM_REF	best32.	;
informat	Setup_Date	mmddyy10.	;
informat	Status_Group	$14.00 	;
informat	Amount_Paid	best32.	;
informat	End_Date	mmddyy10.	;
informat	Claim_Count	best32.	;
format	CUST_REF	best12.	;
format	PLCY_REF	best12.	;
format	CLAIM_REF	best12.	;
format	Setup_Date	mmddyy10.	;
format	Status_Group	$14.00 	;
format	Amount_Paid	best12.	;
format	End_Date	mmddyy10.	;
format	Claim_Count	best12.	;

INPUT			
CUST_REF	
PLCY_REF	
CLAIM_REF	
Setup_Date	
Status_Group	$
Amount_Paid	
End_Date	
Claim_Count	
		
;
RUN;






/*%HTM_QCreport(RAW,CLAIMS_DATA,/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/QC_CLAIMS_DATA/NEW , freqsize=40, samplesize=20);*/

PROC SQL;
SELECT CLAIM_REF INTO:CLAIM_REF
SEPARATED BY " "
FROM RAW.CLAIMS_DUPS_TO_REMOVE_1;
QUIT;

DATA RAW.CLAIMS_DATA;
SET RAW.CLAIMS_DATA;
IF CLAIM_REF IN (&CLAIM_REF.) THEN DELETE;
RUN;




