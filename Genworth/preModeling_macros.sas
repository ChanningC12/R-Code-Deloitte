/****************************************************************************
   program:      preModeling_macros.sas
   author:       Ravi Kumar / Joe Brzezinski
   date:         May 19, 2005
   purpose:      List of all macros needed for the pre-modeling stage

Change Log

2007-12-07 rkumar
    - created a new macro called crtCappedTargetVars()
    - changed crtTargetVars to use the new crtCappedTargetvars()
    - changed crtSizeAdjRatios() to accept a new parameter called VarList
    changed crtSizeAdjVars() to accpet a new parameter called varList


*****************************************************************************/

%macro doRandomSplit(infile, outfile,numSplits=5, SplitBy=PolNum, numBuckets=10, premiumFld=manprem);
/****************************************************************************
   	Macro:      doRandomSplit
   	Author:    	Ravi Kumar / Joe Brzezinski
   	Date:      	May 19, 2005
   	Purpose:    Create a file associating &SplitBy variable with 
				&numSplits variables where the value in each variable
				is a random number from 1 to &numBuckets AND the sum
				of &premiumFld for each bucket is approximately the same.
	Parameters:	infile - Source file name of observations to be randomized
	  			outfile - Output file name containing the results
				numSplits - number of Random Variables to be created
				numBuckets - number of unique values for each Random Variable
					(e.g. if numBuckets is 10 each random variable will have
					a value from 1 to 10)
				premiumFld - Field used to weight summary statistics
*****************************************************************************/
    %if "&outfile" = "" %then %do;
      %let outfile = &infile;
    %end;
    
  
    proc sort  nodupkey data=&infile (keep= &Splitby )
     out=_splitKeys_; by &SplitBy ; run;

   	data _splitKeys_; set _splitKeys_; 
		drop testRandom1 testRandom;
     	%do i = 1 %to &numSplits;
         	testRandom1=ranuni(2**&i);
         	testRandom = floor(&numBuckets*testRandom1) + 1;
 			random&i = min(testRandom, &numBuckets);
     	%end;
	Title "Macro doRandomSplit: First 15 Results";
   	proc print data=_splitKeys_ (obs = 15);
   	run;
   	proc sort data=_splitKeys_; by &SplitBy; run;
   	proc sort data=&infile out=&outfile;by &SplitBy;
   	data &outfile;
     	merge &outfile (in=intrntst) _splitKeys_ (in=ok); by &SplitBy;
     	if intrntst;

%mend;




/****************************************************************************
   	Macro:      doTimeSerSplit
   	Author:    	Ravi Kumar / Joe Brzezinski
   	Date:      	May 19, 2005
   	Purpose:    Create a time series split variable 

	Parameters:	infile - Source file name of observations to be randomized
	  			outfile - Output file name containing the results
				numBuckets - number of unique values for each Random Variable
					(e.g. if numBuckets is 10 each random variable will have
					a value from 1 to 10)
				dateFld= the name of the date field that should be used to sort the dat in chronological order
				premiumFld - Field used to weight summary statistics
*****************************************************************************/
%macro doTimeSerSplit(infile, outFile,numberBuckets=10,dateFld=expDate,premiumFld=manprem);
    proc sort data=&infile out=&outFile; 
     by &dateFld;

    data &outfile;set &outfile nobs=nRecs;
    	timeSer = ceil(_n_ * &NumberBuckets/ nRecs );
    	timeser = max(min(timeser, &numberBuckets),1);

%mend;




/****************************************************************************
   	Macro:      crtCappedTargetVars
   	Author:    	Ravi Kumar
   	Date:      	Dec 12, 2007
   	Purpose:    Create additional capped variables for a target variable. 
                   For example, if the target variable is LR, this macro creates additional capped variables called LR90, LR95, LR99, LRlog and LR3iqr

	Parameters:	infile - input SAS filename which contains the target variable
	  	       outfile - Output SAS file name where the capped target variable will be created
			targetVarName - the name of the target variable
			grpBy - separate capping cut offs will be calculated for each by Group
                            Leave this blank if all capping cut offs are based on all data

	Sample usage:
 
**Create variables LR90, LR95, LR99, LRLOG and LR3iqr 
%crtCappedTargetVars(infile=trntst, outfile=trntst, targetVarName=LR, grpBy=);

**Create variables Loss90, Loss95, Loss99, LossLOG and Loss3iqr using different cut offs by State
%crtCappedTargetVars(infile=trntst, outfile=trntst, targetVarName=Loss, grpBy=State);


*****************************************************************************/
%macro crtCappedTargetVars(infile, outfile, targetVarName, grpBy=, suffix=);
   %if "&grpBy" = "" %then %do;
        proc univariate data= &infile (where=(&targetVarName >0)) noprint;by &grpBy;
   %end; %else %do;
        proc sort data=&infile out=&outfile; by &grpBy;
        proc univariate data= &outfile (where=(&targetVarName >0))noprint;by &grpBy;
   %end;
    var &targetVarName;
    output out=_uni_ pctlpts = 25 75 90 95 99 pctlpre = S ;
 
    data _uni_;set _uni_;
    iqr3 = S75 + 3 * (S75 - S25); 

    proc print data=_uni_;

   %if "&grpBy" = "" %then %do;
        data &outfile; set &infile(in=in_main);
           if _n_ = 1 then set _uni_;
           in_uni = 1;
           retain s90 s95 s99 iqr3;
   %end; %else %do;
        data &outfile; merge &outFile(in=in_main) _uni_ (in=in_uni);
          by &grpBy; if in_main;
   %end;

   drop s25 s75 s90 s95 s99 iqr3;
   
   if in_uni then do;
      &targetVarName.90&suffix=min(&targetVarName.,S90);
      &targetVarName.95&suffix=min(&targetVarName.,S95);
      &targetVarName.99&suffix=min(&targetVarName.,S99);
      &targetVarName.3iqr&suffix=min(&targetVarName.,IQR3);
   end;else do;
      &targetVarName.90&suffix=&targetVarName.;
      &targetVarName.95&suffix=&targetVarName.;
      &targetVarName.99&suffix=&targetVarName.;
      &targetVarName.3iqr&suffix=&targetVarName.;
   end;


   if &targetVarName.+1>0 then &targetVarName.log&suffix = log(&targetVarName. + 1);
   else &targetVarName.log&suffix = 0;    



%mend;






/****************************************************************************
   	Macro:      crtTargetVars
   	Author:    	Ravi Kumar
   	Purpose:    Create target variable LR, FREQ, LR90, LR95, LR99, LRlog and LR3iqr

	Parameters:	infile - input SAS filename which contains the target variable
	  	       outfile - Output SAS file name where the capped target variable will be created
			PremiumFld- the name of the premium field used in the denominator of loss ratio
			LossFld- the name of the loss field used in the numerator of loss ratio
			ClaimCntFld - the name of the claim count field used for calculating claim FREQ variable


	Sample usage:
 
**Create target variables freq,LR90, LR95, LR99, LRLOG and LR3iqr in trntst file
%crtTargetVars(infile=trntst, outfile=trntst, PremiumFld=manPrem, LossFld=ultLoss, ClaimCntFld=clmCnt)
*****************************************************************************/
%macro crtTargetVars(infile, outfile, PremiumFld, LossFld, ClaimCntFld);
	data &outfile ;set &infile;
              lr=0;lr100k=0;lr200k=0;lr300k=0;freq=0;
		if &PremiumFld>0 then do;
                   lr = sum(&LossFld/&PremiumFld,0);
                   lr100k = sum(&LossFld.100K/&PremiumFld,0) ; 
                   lr200k = sum(&LossFld.200K/&PremiumFld,0) ;
                   lr300k = sum(&LossFld.300K/&PremiumFld,0) ;
                   freq = &ClaimCntFld/&PremiumFld * 1000;
                end; 

  		if &ClaimCntFld = . then &ClaimCntFld= 0; _grpBy_ = 1; 

  	run;

       %crtCappedTargetVars(infile=&outfile, outfile=&outfile, targetVarName=lr, grpBy=);
%mend;




/****************************************************************************
   	Macro:      crtSizeAdjRatios
   	Author:    	Ravi Kumar
   	Purpose:    Create size bias adjustment ratios in a SAS file for the each targate variable specified in targetVarList
                   If no target variable is specified, the macro creates size bias adjustment factors for variables: lr90 lr95 lr99 lrlog lr
                   

	Parameters:	infile - input SAS filename which contains the target variables
			PremiumFld- the name of the premium field used as weight in calculating mean values for target variables
                                 Do not pass a premium field if you do not want the adjustments to be based on weighted means
			adjustBy- the name of the field(s) across which the size bias needs to be corrected
			outRatioFile - the name of the SAS file to which the size bias adjustments ratios should be written to.
                                    This file will be used by the crtSizeAdjVars() macro to create the size bias adjusted target variables
                    targetVarList - List of target variables for which size bid adjustments should be calculated
                                    If no variables are specified the macro defaults to : lr90 lr95 lr99 lrlog lr lr100k lr200k lr300k
                    baseVar : the name of the base variable whose distribution should be replicated. For example, he baseVar could be LR to make all adjusted
                              variables have the same bias as LR.


	Sample usage:
***Calculate size biad adjustment ratios for default target variable list
 %crtSizeAdjRatios(infile=trntst, PremiumFld=manprem, AdjustBy=PremSlice, outRatioFile=ratioFile);
***Calculate size biad adjustment ratios for target variables ultLoss, loss200K and loss100K
 %crtSizeAdjRatios(infile=trntst, PremiumFld=manprem, AdjustBy=PremSlice, outRatioFile=ratioFile,targetVarList=ultLoss loss200k loss100k);

*****************************************************************************/
%macro crtSizeAdjRatios(infile, PremiumFld=manprem, AdjustBy=PremSlice, outRatioFile=_lrratio_, targetVarlist=, baseVar=);
      %if "&targetVarlist" = "" %then %do;
           %let targetVarlist= lr90 lr95 lr99 lrlog lr lr100k lr200k lr300k;
      %end;
      %if "&PremiumFld" = "1" %then %do;
           %let PremiumFld=;
      %end;

     %let _numOfVars_ = %eval(1 + %length(%cmpres(&targetVarlist)) -
                             %length(%sysfunc(compress(&targetVarlist)));
   

 	proc summary data=&infile nway ;
   		class &AdjustBy;
   		var &targetVarlist;
   		%if "&premiumFld" ne "" %then %do; weight &PremiumFld; %end;
   		output out=&outRatioFile mean=%do _Icsar_ = 1 %to &_numOfVars_ ; %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  M&_thisVar_ %end;    ; run;
 
 	proc summary data=&infile nway ;
   		var &targetVarlist;
   		%if "&premiumFld" ne "" %then %do; weight &PremiumFld; %end;
   		output out=_overall_ mean=%do _Icsar_ = 1 %to &_numOfVars_ ; %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  MEAN&_thisVar_ %end;; run;
   		
   	data _overall_ ;set _overall_; drop _freq_; run;
   		
 	data &outRatioFile ;
 		set &outRatioFile;
 		if _n_ = 1 then set _overall_ ;


         %if "&baseVar" = "" %then %do;  
              %do _Icsar_ = 1 %to &_numOfVars_ ; 
                   %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  
                   adj_mult_&_thisVar_. = MEAN&_thisVar_./M&_thisVar_;
                   adj_add_&_thisVar_. = MEAN&_thisVar_. - M&_thisVar_;
              %end;
         %end; %else %do;
              %do _Icsar_ = 1 %to &_numOfVars_ ; 
                   %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  
                   adj_mult_&_thisVar_. = M&baseVar./M&_thisVar_;
                   adj_add_&_thisVar_. = M&baseVar. - M&_thisVar_;
              %end;
         %end;

   
	Title "crtSizeAdjRatios file &outRatioFile";
   	proc print data=&outRatioFile;
 %mend;
 






/****************************************************************************
   	Macro:      crtSizeAdjVars
   	Author:    	Ravi Kumar
   	Purpose:    Create size bias adjusted target variables for the each targate variable specified in targetVarList
                   If no target variable is specified, the macro creates size biased for variables: lr90 lr95 lr99 lrlog lr
                   

	Parameters:	infile - input SAS filename which contains the target variables
                    outFile - output SAS file which will contain the extra size bias adjusted target variables
                    inRatioFile - input SAS file which contains size bias adjustment factors. This file is created by crtSizeAdjRatios() macro. 
			PremiumFld- the name of the premium field for which additional logPrem variables are needed 
                                 Do not pass a premium field if you do not want the additiona addPrem variables
			adjustBy- the name of the field(s) across which the size bias needs to be corrected 
                     suffix - a common suffix that will added to all the size bias adjusted target variables that are created by this macro
                              Leave this parameter blank if you do not want a common suffix added every size bias adjusted target variable ame
                    targetVarList - List of target variables for which size bid adjustments should be calculated
                                    If no variables are specified the macro defaults to : lr90 lr95 lr99 lrlog lr


	Sample usage:
***Calculate size biad adjusted target variables for default target variable list
 %crtSizeAdjVars(infile=trntst, outfile=trntst, inRatioFile=ratiofile,  PremiumFld=manprem ,AdjustBy=PremSlice);

***Calculate size bias adjusted target variables for target variables ultLoss, loss200K and loss100K
 %crtSizeAdjVars(infile=trntst, outfile=trntst, inRatioFile=ratiofile,  PremiumFld=manprem ,AdjustBy=PremSlice,targetVarlist=ultLoss loss200K loss100K

*****************************************************************************/
 /*add a new parameter "suffix" on 11/06/2006 by Hua */  
 %macro crtSizeAdjVars(infile, outfile, inRatioFile=_lrratio_,  PremiumFld=manprem ,AdjustBy=PremSlice, suffix=,targetVarlist=);
      %if "&PremiumFld" = "1" %then %do;
           %let PremiumFld=;
      %end;

      %let crtLogPremvars=0;
      %if "&targetVarlist" = "" %then %do;
           %let targetVarlist = lr90 lr95 lr99 lrlog lr lr100k lr200k lr300k;
           %if "&PremiumFld" ne "" %then %do;
              %let crtLogPremvars=1;
           %end;
      %end;

     %let _numOfVars_ = %eval(1 + %length(%cmpres(&targetVarlist )) -
                             %length(%sysfunc(compress(&targetVarlist )));

      data &outFile;set &infile;
      %do _Icsar_ = 1 %to &_numOfVars_ ; 
           %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  
           adj_mult_&_thisVar_. = 0; drop adj_mult_&_thisVar_.;
           adj_add_&_thisVar_. = 0; drop adj_add_&_thisVar_.;
      %end;

 	proc sort data=&outfile;
 		by &AdjustBy;
 	data &outfile;
 		merge &outfile &inRatioFile;
 		by &AdjustBy;
      %do _Icsar_ = 1 %to &_numOfVars_ ; 
           %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  
   	    &_thisVar_._adj_add&suffix= &_thisVar_. +adj_add_&_thisVar_.;
   	    &_thisVar_._adj_mult&suffix= &_thisVar_. * adj_mult_&_thisVar_.;
           drop adj_add_&_thisVar_. adj_mult_&_thisVar_.; 
      %end;
      %if "&crtLogPremvars" = "1" %then %do;
   		log_prem_adj99&suffix = log(&PremiumFld/adj_mult_lr99);
   		log_prem_adj95&suffix = log(&PremiumFld/adj_mult_lr95);
   		log_prem_adj90&suffix = log(&PremiumFld/adj_mult_lr90);
   		log_prem_adjlr&suffix = log(&PremiumFld/adj_mult_lr);
      %end;

   
	proc means noprint data=&outfile; class &AdjustBy;
    	var &targetVarlist 
      %do _Icsar_ = 1 %to &_numOfVars_ ; 
           %let _thisVar_ = %scan( &targetVarlist , &_Icsar_);  
   	    &_thisVar_._adj_add&suffix &_thisVar_._adj_mult&suffix 
      %end;
      %if "&crtLogPremvars" = "1" %then %do;
   		log_prem_adj99&suffix log_prem_adj95&suffix log_prem_adj90&suffix log_prem_adjlr&suffix 
      %end;
		;
       %if "&premiumFld" ne "" %then %do; weight &PremiumFld; %end;
     	output out=_a_ mean=;
       	run;
	title "Macro crtSizeAdjVars Mean Values";
   	proc print data=_a_; run;
%mend;


%macro crtSlices(infile, outfile, inFld=manprem,outFld=premSlice, NumberBuckets=10);
   data &outFile;set &infile;
   temp_prem=0; _PremSum_=0; &outFld=0;
   if missing(&infld) then &infld=0; 
   drop temp_prem _PremSum_ &outFld;
 
   proc sort data=&outfile; by &inFld;  run;
   proc summary data=&outfile nway; var &inFld;
   output sum=_PremSum_ out=_prm_;run;
		
   data &outfile; set &outfile;
   if _n_=1 then do;
      set _prm_;
      retain temp_prem;	
      drop temp_prem _PremSum_;
     	temp_prem = 0.00;
   end;
   temp_prem=sum(0,temp_prem,&inFld);
   &outfld = ceil(temp_prem*&NumberBuckets/_PremSum_);
   &outfld = max(min(&outfld, &numberBuckets),1);
   		
   proc sort data=&outfile out =&outfile;by &outfld &infld;run;

   proc summary data=&outfile nway;
   class &outfld;var &infld  ;
   output out=_smySlices_ sum= ; 
   title "Macro crtSlices summary class &outfld, sum of &infld";
   proc print data=_smySlices_;     
%mend;


/***********************************************************************************
This macro does principal component analysis, saves the statistics to a SAS file and 
	writes the SAS code to calculate the principal components.

Macro variables to be passed to this macro:
        infile: the data file on which principal component [PC] analysis needs to be 
			performed
        prefix: the prefix for the principal component (variable) names
        numcomp: the number of components for which the principal component equation 
			need to be written to a file

Global macro variables to be set before calling this macro:
        varlist: the list of variables for which analysis need to be performed
        sascodeFile: The file to which the SAS code need to be written to. This 
			SAS code calculates the principal components.
        PCfile: The SAS datasets to which the PC weights wil be written to 

Input
        &infile

Output
        mfirreg.princomp_lev1 : This file contains all the principal component statistics.
                                This macro appends rows to this file. So, delete or empty this file
                                before first use.
		should this be			if you use permanent file name then run into problems 
		"&PCfile"				when multiple users run the same macro while another needs it?
***********************************************************************************/

%let _initPrinComp_ = 1;  /* ? -- only for include file if library nothing invokes it */
%macro prinComp(inFile, prefix=pc,weight=);
    title1 "Macro prinComp:  inFile=&infile   Prefix=&prefix";

    %let numComp = %eval(1 + %length(%cmpres(&varList)) -
                             %length(%sysfunc(compress(&varList)));


    %if &_initPrinComp_  =  1 %then %do;
         %if "&sasCodeFile" = "" %then %do;
         %end; %else %do;
               proc printto print="&sasCodeFile" new;run;
               proc printto;
         %end;
         %if "&PCfile" = "" %then %do;
         %end; %else %do;
               data &PCfile;set _null_; 
               proc sql;drop table &PCfile;
         %end;
    %end;

    %let _initPrinComp_ = 0;

    proc princomp data=&infile out=pcValues outstat=stats prefix=&prefix;
     	var &varlist;
    %if "&weight" = "" %then %do;
    %end;%else %do;
        WEIGHT &weight ; 
    %end;

    proc means data=pcValues;
    	var &prefix.1-&prefix&numComp;


 	data stats; set stats;
   		if _TYPE_ in ('SCORE' , 'EIGENVAL' , 'MEAN' , 'STD' );
   		if _name_ = "" then _name_ = _type_; drop _type_;
   	run;

   *** transpose stats file for ease of use in later steps ****;
   proc transpose data=stats out=stats name=_name_ ;id _name_;
   proc print data=stats;

   ****print the principal component equation for each principal component ***;
   %do i = 1 %to &numComp;
      	run;
      	data _null_; set stats end=lastobs;
      		file "&sasCodeFile" mod lrecl=32767;
      		if _n_ = 1 then do;
           		put "&prefix&i =";
           		put &prefix&i " * ( " _name_  " - " mean " ) / " std   ;
      		end; else do;
           		put " + " &prefix&i " * ( " _name_  " - " mean " ) / " std  ;
      		end;
      	if lastobs then do;
           	put ";";
      	end;

      ****Append the principal component stats to a sas file for later use ****;
      	%if "&PCfile" ne "" %then %do;
         	data stats1;set stats;
         		keep pcName varName eigen mean std;
         		format pcname varName $32.;
         		pcname = upcase("&prefix&i");
         		varname = upcase(_name_);
         		eigen = &prefix&i;
         	proc append data=stats1 base=&PCfile;
      	%end;
   	%end;

%mend;

%macro CORR (inLib, inFile, cutOff=0.5, CorrCoef=P,outCSVfile=corr.csv,upperTriangleOnly=0);
   %if %upcase("&CorrCoef") = "K" or  %upcase("&CorrCoef") = "S" %then %do;
   %end; %else %do;
         %let corrCoef=P;
   %end;  
   proc corr data=&inLib..&infile out&corrCoef=&infile._corr nosimple noprint;
   
   	/*Get a list of variables in the matrix by printing the contents to a file*/
   	proc contents data=&infile._corr out=var2 noprint;
   	proc sort data=var2;
   		by npos;

   	/*print the list for debugging purposes */
   	data _null_; set var2;
   		c=",";x=-1; 
   		file print notitle linesize=250 pagesize=500;
   		put name; 

   	/* assign a sequence number to every numeric variable */
   	data var2;set var2;
   		keep varname2 seq;
   		varname2 = name;
   		retain seq;
   		if _n_ = 1 then seq = 0;
   		if type=1; /*keep only numeric variables */
   		seq = seq+1;

   	/* write out the nXn matrix as a 'n-squared by 1' matrix */
   	data a; set &infile._corr;
   		array x(*) _NUMERIC_ ; /*make an array of all the numeric variables*/
   		keep varname1 seq corr; 
   		varname1 = _name_;
   		if varname1 ne '';
   		if _type_ = "CORR";
    	do seq = 1 to dim(x);
       		if x(seq) ne . then do;
          		corr = x(seq);
          		output;
       		end;
    	end;

   	proc sort data=a;
   		by seq;

   	/*merge with file var2 to get the name of the second variable */
   	data a ;merge a var2;
   		by seq;
   		if varname1 = varname2 then delete;
   		if substr(varname1,1) = "_" or substr(varname2,1,1) = "_" then delete;
   		if varname1 ne '';if varname2 ne '';
   		abscorr = abs(corr);
   		if abscorr > &cutOff; 
            %if "&upperTriangleOnly" = "1" %then %do; 
                if varname1 > varname2; 
            %end;

   	/* sort and print the correlation matrix */
   	proc sort data=a;
   		by descending abscorr;

		proc export data= a
OUTFILE = "&outCSVfile."
DBMS = csv
REPLACE;
run;
/*   	data _null_; set a;*/
/*   		c=",";x=-1; */
/*   		file "&outCSVfile";*/
/*   		if _n_ = 1 then do;*/
/*      		line1="obs,varname1,varname2,correlation";*/
/*      		put line1 ;*/
/*   		end;*/
/*   		varname1 = upcase(varname1);*/
/*   		varname2 = upcase(varname2);*/
/*/*   		put _n_ +x c +x varname1 +x c +x varname2 +x c +x corr ; */*/

%mend;






