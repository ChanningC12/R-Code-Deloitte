%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";

/**/
/*proc import datafile='/PROJECTS/GENWORTH_2016/RAW/DELOITTE_RESPONSES_BY_RD_PHS_17OCT2016.txt'*/
/*OUT=CLAIMS_DATA DBMS=DLM REPLACE; DELIMITER='09'x;GETNAMES=YES;RUN;*/


proc printto print="/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/LST/LOAD_RESPONSE_DATA.lst"
log="/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/LOGS/LOAD_RESPONSE_DATA.log"  new; run;
DATA RAW.RESPONSE_DATA;

	INFILE '/PROJECTS/GENWORTH_2016/RAW/DELOITTE_RESPONSES_BY_RD_PHS_17OCT2016.txt' DELIMITER='09'X MISSOVER DSD LRECL=32767 FIRSTOBS=2 ;

	INFORMAT	PLCY_REF	BEST32.	;
	INFORMAT	STATE	$3.00 	;
	INFORMAT	PRODUCT_DESC	$24.00 	;
	INFORMAT	RESPONSE	$4.00   ;
	INFORMAT 	RATE_INCR	BEST12.  ;
	INFORMAT 	TOTL_COMMUNICATED_INCR_RATE 	BEST12.  ;
	INFORMAT	ANNL_PREM_BFOR		BEST12.  ;
	INFORMAT	ANNL_PREM_AFTR		BEST12.  ;
	INFORMAT 	LTR_DT		MMDDYY10.	;
	INFORMAT 	EFF_DT		MMDDYY10.	;
	INFORMAT 	ROUND		BEST12.  ;
	INFORMAT 	PHASE		BEST12.  ;
	INFORMAT 	BIO_BEFORE		$14.00	;
	INFORMAT 	BIO_AFTER		$14.00	;
	INFORMAT 	ORIG_DBA_BFOR	BEST12.  ;
	INFORMAT 	ORIG_DBA_AFTR	BEST12.  ;
	INFORMAT	ELIM_PRD_BFOR	BEST12.  ;
	INFORMAT 	ELIM_PRD_AFTR	BEST12.  ;
	INFORMAT	BENE_TRM_BFOR	BEST12.  ;
	INFORMAT 	BENE_TRM_AFTR	BEST12.  ;
	INFORMAT 	FREQ_BFOR		$1.00 	;
	INFORMAT 	FREQ_AFTR		$1.00 	;
	INFORMAT 	IF_RED_DBA		BEST12.	;
	INFORMAT 	IF_RED_BIO		BEST12.	;
	INFORMAT 	IF_RED_BENPERIOD	BEST12.	;
	INFORMAT 	IF_RED_ELIM		BEST12.	;
	INFORMAT 	RBO_NUM_CHANGES		BEST12.	;
	INFORMAT 	ISSUE_AGE		BEST12.	;
	INFORMAT 	IN_FORCE_DT		MMDDYY10.	;
	INFORMAT 	LANDING_SPOT	$5.00 	;
	INFORMAT	NY_Compare		$8.00 	;
	INFORMAT	IFA_Group_State_Prod	$24.00 	;
	INFORMAT 	NUM_DECISIONS		BEST12.	;



	FORMAT	PLCY_REF	BEST32.	;
	FORMAT	STATE	$3.00 	;
	FORMAT	PRODUCT_DESC	$24.00 	;
	FORMAT	RESPONSE	$4.00   ;
	FORMAT 	RATE_INCR	BEST12.  ;
	FORMAT 	TOTL_COMMUNICATED_INCR_RATE 	BEST12.  ;
	FORMAT	ANNL_PREM_BFOR		BEST12.  ;
	FORMAT	ANNL_PREM_AFTR		BEST12.  ;
	FORMAT 	LTR_DT		MMDDYY10.	;
	FORMAT 	EFF_DT		MMDDYY10.	;
	FORMAT 	ROUND		BEST12.  ;
	FORMAT 	PHASE		BEST12.  ;
	FORMAT 	BIO_BEFORE		$14.00	;
	FORMAT 	BIO_AFTER		$14.00	;
	FORMAT 	ORIG_DBA_BFOR	BEST12.  ;
	FORMAT 	ORIG_DBA_AFTR	BEST12.  ;
	FORMAT	ELIM_PRD_BFOR	BEST12.  ;
	FORMAT 	ELIM_PRD_AFTR	BEST12.  ;
	FORMAT	BENE_TRM_BFOR	BEST12.  ;
	FORMAT 	BENE_TRM_AFTR	BEST12.  ;
	FORMAT 	FREQ_BFOR		$1.00 	;
	FORMAT 	FREQ_AFTR		$1.00 	;
	FORMAT 	IF_RED_DBA		BEST12.	;
	FORMAT 	IF_RED_BIO		BEST12.	;
	FORMAT 	IF_RED_BENPERIOD	BEST12.	;
	FORMAT 	IF_RED_ELIM		BEST12.	;
	FORMAT 	RBO_NUM_CHANGES		BEST12.	;
	FORMAT 	ISSUE_AGE		BEST12.	;
	FORMAT 	IN_FORCE_DT		MMDDYY10.	;
	FORMAT 	LANDING_SPOT	$5.00 	;
	FORMAT	NY_Compare		$8.00 	;
	FORMAT	IFA_Group_State_Prod	$24.00 	;
	FORMAT 	NUM_DECISIONS		BEST12.	;

	INPUT			
	PLCY_REF
	STATE	$
	PRODUCT_DESC	$
	RESPONSE	$
	RATE_INCR
	TOTL_COMMUNICATED_INCR_RATE 
	ANNL_PREM_BFOR
	ANNL_PREM_AFTR	
	LTR_DT
	EFF_DT
	ROUND
	PHASE
	BIO_BEFORE	$
	BIO_AFTER	$
	ORIG_DBA_BFOR
	ORIG_DBA_AFTR
	ELIM_PRD_BFOR
	ELIM_PRD_AFTR
	BENE_TRM_BFOR
	BENE_TRM_AFTR
	FREQ_BFOR		$
	FREQ_AFTR		$
	IF_RED_DBA	
	IF_RED_BIO	
	IF_RED_BENPERIOD
	IF_RED_ELIM	
	RBO_NUM_CHANGES	
	ISSUE_AGE
	IN_FORCE_DT	
	LANDING_SPOT	$
	NY_Compare		$
	IFA_Group_State_Prod	$
	NUM_DECISIONS
	;	

RUN;

%HTM_QCreport(RAW,RESPONSE_DATA,/PROJECTS/GENWORTH_2016/02_CODES/DATALOAD/QC_RESPONSE_DATA , freqsize=40, samplesize=20);
