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
%MACRO SCORE(RBOModelNo,
NFOModelNo
);
DATA VALIDATION;
SET MREG.MODELING_DATA_FINAL;
IF RSPLIT1 = "VAL";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_NFO.sas";
%INCLUDE "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/PC/PC_RBO.sas";
	weightVar = 1;	
   	pol_cnt=1;
   	zero=0;
	IF NFO_IND=1 THEN ORIG_RESPONSE = "NFO";
	ELSE IF RBO_IND=1 THEN ORIG_RESPONSE="RBO";
	ELSE  ORIG_RESPONSE ="PFA";
IF ORIG_RESPONSE="PFA" THEN PFA_IND=1;
ELSE PFA_IND=0;
	RUN;
	PROC FREQ DATA=VALIDATION;TABLES NFO_IND RBO_IND ORIG_RESPONSE RESPONSE_TARGET/MISSING;RUN;
	DATA VALIDATION;
	SET VALIDATION;

   	%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/SCORE/reg_RBO_glm_&RBOModelNo..sas";
   	%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/SCORE/reg_NFO_glm_&NFOModelNo..sas"; 

		Score_RBO = exp(pred_RBO_&RBOModelNo.) / (1+exp(pred_RBO_&RBOModelNo.));
		Score_NFO = exp(pred_NFO_&NFOModelNo.) / (1+exp(pred_NFO_&NFOModelNo.));  
		if score_rbo<0.06 and score_nfo<0.0268 then pred_response="PFA";
		ELSE if score_rbo<0.06 and score_nfo>=0.0268 then pred_response="NFO";
		ELSE if score_rbo>=0.06 and score_nfo<0.0268 then pred_response="RBO";
		ELSE if score_rbo>=0.06 and score_nfo>=0.0268 AND
score_rbo>score_nfo then pred_response="RBO";
ELSE pred_response = "NFO";
if pred_response ="NFO" THEN PRED_NFO=1;
ELSE PRED_NFO=0;
if pred_response ="RBO" THEN PRED_RBO=1;
ELSE PRED_RBO=0;
if pred_response ="PFA" THEN PRED_PFA=1;
ELSE PRED_PFA=0;
RUN;
PROC FREQ DATA=VALIDATION;
TABLES RBO_IND*PRED_RBO NFO_IND*PRED_NFO ORIG_RESPONSE*PRED_RESPONSE PFA_IND*PRED_PFA/MISSING LIST;
RUN;

%MEND;
%SCORE(22471,12251)




