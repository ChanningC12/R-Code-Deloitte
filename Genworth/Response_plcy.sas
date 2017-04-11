DATA RESPONSE_P;
SET PRED.ALLVARS_PRECAP;
KEEP PLCY_REF RESPONSE;
RUN;

PROC TRANSPOSE DATA=RESPONSE_P OUT=TRANS_RESPONSE_P(DROP=_NAME_) PREFIX=RESPONSE;
BY PLCY_REF;
VAR RESPONSE;
RUN;

DATA TRANS_RESPONSE_P;
SET TRANS_RESPONSE_P;
IF RESPONSE1="NFO" OR RESPONSE2="NFO" OR RESPONSE3="NFO" THEN RESPONSE_IND="NFO";
ELSE IF RESPONSE1="RBO" OR RESPONSE2="RBO" OR RESPONSE3="RBO" THEN RESPONSE_IND="RBO";
ELSE RESPONSE_IND="FULL PAY";
RUN;

/* INDICATOR TABLE */
DATA PRED.RESPONSE_PLCY;
SET TRANS_RESPONSE_P;
KEEP PLCY_REF RESPONSE_IND;
RUN;

/* MERGE INDICATOR TO RESPONSE TABLE */
PROC SQL;
CREATE TABLE RESPONSE_PLCY_ALL AS
SELECT A.*,B.*
FROM RESPONSE_PLCY AS A
LEFT JOIN RAW.RESPONSE_DATA AS B
ON A.PLCY_REF = B.PLCY_REF;
RUN;

/* CHECK THE DISTINCT */
PROC SQL;
CREATE TABLE DIST_RESPONSE AS
SELECT DISTINCT 
PLCY_REF,
RESPONSE_IND,
STATE,
PRODUCT_DESC,
/* TOTL_COMMUNICATED_INCR_RATE */
NUM_DECISIONS,
LANDING_SPOT,
NY_COMPARE,
IFA_GROUP_STATE_PROD
FROM RESPONSE_PLCY_ALL;
RUN;



/******* CREATE UNIQUE EFF_DT, BIO_BFOR, BIO_AFTR, ELIM_PRD_BFOR, ELIM_PRD_AFTR, 
BENE_TRM_BFOR, BENE_TRM_AFTR, FREQ_BFOR, FREQ_AFTR********/
DATA RESPONSE_VARS;
SET PRED.ALLVARS_PRECAP;
KEEP PLCY_REF EFF_DT BIO_BFOR BIO_AFTR ELIM_PRD_BFOR ELIM_PRD_AFTR BENE_TRM_BFOR BENE_TRM_AFTR FREQ_BFOR FREQ_AFTR;
RUN;

DATA RESPONSE_VARS;
SET RESPONSE_VARS;
BY PLCY_REF;
IF LAST.PLCY_REF THEN DO;
EFF_DT_NEW = EFF_DT;
BIO_BFOR_NEW = BIO_BFOR;
BIO_AFTR_NEW = BIO_AFTR;
ELIM_PRD_BFOR_NEW = ELIM_PRD_BFOR;
ELIM_PRD_AFTR_NEW = ELIM_PRD_AFTR;
BENE_TRM_BFOR_NEW = BENE_TRM_BFOR;
BENE_TRM_AFTR_NEW = BENE_TRM_AFTR;
FREQ_BFOR_NEW = FREQ_BFOR;
FREQ_AFTR_NEW = FREQ_AFTR;
END;
KEEP PLCY_REF EFF_DT_NEW BIO_BFOR_NEW BIO_AFTR_NEW ELIM_PRD_BFOR_NEW 
ELIM_PRD_AFTR_NEW BENE_TRM_BFOR_NEW BENE_TRM_AFTR_NEW FREQ_BFOR_NEW FREQ_AFTR_NEW;
RUN;

DATA RESPONSE_VARS_FINAL;
SET RESPONSE_VARS;
IF EFF_DT_NEW NE .;
RENAME EFF_DT_NEW = EFF_DT;
RENAME BIO_BFOR_NEW = BIO_BFOR;
RENAME BIO_AFTR_NEW = BIO_AFTR;
RENAME ELIM_PRD_BFOR_NEW = ELIM_PRD_BFOR;
RENAME ELIM_PRD_AFTR_NEW = ELIM_PRD_AFTR;
RENAME BENE_TRM_BFOR_NEW = BENE_TRM_BFOR;
RENAME BENE_TRM_AFTR_NEW = BENE_TRM_AFTR;
RENAME FREQ_BFOR_NEW = FREQ_BFOR;
RENAME FREQ_AFTR_NEW = FREQ_AFTR;
RUN;

/* MERGE RESPONSE_VARS_FINAL WITH PRED.RESPONSE_PLCY */
PROC SQL;
CREATE TABLE PRED.RESPONSE_PLCY AS 
SELECT A.*, B.*
FROM PRED.RESPONSE_PLCY AS A
INNER JOIN RESPONSE_VARS_FINAL AS B
ON A.PLCY_REF = B.PLCY_REF;
QUIT;

