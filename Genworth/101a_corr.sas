%let sampleRun=NO;

options mprint;

%include "/PROJECTS/GENWORTH_2016/02_CODES/library.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/common_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/modeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/preModeling_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/uni_macros.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Modeling_Varlist_RBO.sas";
%include "/PROJECTS/GENWORTH_2016/02_CODES/MODELING/Modeling_Varlist_NFO.sas";

/*%correlation(libnamex=pred, dsname=allvars_capped_miss, vars=&varlist_RBO.*/
/*,val=0.5,outfile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/CSV/correlation_RBO.csv);*/

%correlation(libnamex=pred, dsname=allvars_capped_miss, vars=&varlist_NFO.
,val=0.6,outfile=/PROJECTS/GENWORTH_2016/02_CODES/MODELING/CSV/correlation_NFO.csv);
