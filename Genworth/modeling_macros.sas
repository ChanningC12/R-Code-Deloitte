
/******************************************************************************
        Program:        modeling_macros.sas
        Date:           May 6, 2009
        Author:         
        Purpose:        macros used to fit regression and GLM models
                              



******************************************************************************/



/********************************************************************
Macro: do_reg

Description:

Macro Parameters: 
   infile: the name of the SAS dataset to use for the regression 
   target: the name of the target variable (dependent variable)
   varList: List of independent variables to use in regression 
   outRegOut: the name of SAS dataset to which the regression parameters should be saved to
   weight: variable in the infile dataset with values that are relative weights for a weighted
           leased square fit. Typically, manual premium is used as a weight.
   RegType: Specifies model Selection method. Can have values of NONE, STEPWISE, FORWARD, BACKWARD,
            MAXR, MINR, RSQUARE, ADJRSQ or CP. The default when nothing is passed is NONE. 
   offSetVars: A list of variables for which model parameters will be derived but subsequently zeroed
               out for the scoring process
   referenceModelParmFile: parmfile containing the reference model parameter estimates
                           the regression model created by this macro will be a residual model 
   combineReferenceModel: 0 means not to combine the reference model with the new residual model
                          1 means the refernce model should be combined with the new residual model
   modelOptions: Additional model options to be specifieid for PROC REG
   outStatFile: name of the output SAS file to which model statistics will be written to 
                This file can be read by printModelStats() macro to compare t-statistics from different models
   renameReferenceInterceptAs: the reference intercept term will be renamed to this name
                               if this is empty, the reference intercept will be added to the intercept term 
                               of the combined full model
   
Input SAS files:
      &infile: The SAS dataset to use for the regression 
      &referenceModelParmFile: parmfile containing the referencemodel parameter estimates
Output SAS files:
      &outRegOut: Dataset containing parameter estimates 
      &outStatFile: dataset containing parameter estimates and T-statistics
Output SAS code:
      None

Sample Usage:  
 1. simple regression 
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList)

  2. weighted regression
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,weight=manprem)

  3. stepWise Regressoin
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,regType=stepwise)

  4. Using offset variables 
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,offSetVars=polYear polSize)

  5. Get regression parameters in a SAS file that can be used by %score macro
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,outRegOut=mreg.myParmFile)

  6. Get regression parameters and other statistics in a file used for display purposes
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,outStatFile=mreg.myStatFile)

  7. Create a residual model by passing a reference parm file
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,
           referenceModelParmFile=mreg.anyReferenceModel,combineReferenceModel=0)
  8. Create a residual model by passing a reference parm file,
      but then combine the two models to get a full model
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,
           referenceModelParmFile=mreg.anyReferenceModel,combineReferenceModel=1)
  9. Create a residual model by passing a reference parm file,
       then combine the two models to get a full model,
       but keep the reference model intercept term as a separate variable
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,
           referenceModelParmFile=mreg.anyReferenceModel,combineReferenceModel=1,
           renameReferenceInterceptAs=newRenewInd)



*********************************************************************/

%macro do_reg(inFile,Target,VarList,outRegOut=,Weight=,RegType=,offSetVars=,
              referenceModelParmFile=,combineReferenceModel=1,
              modelOptions=,outStatFile=statFile,
              renameReferenceInterceptAs=,crossValidVar=);

   %if "&RegType" ne "" %then %do;
       %let modelOptions=&modelOptions selection=&regType ; 
   %end;

   %let _infile_=&infile;
   %let _target_ = &target; run;

   %if "&referenceModelParmFile" ne "" %then %do;
      %score(infile=&infile,outfile=_infile_,varList=,parmFile=&referenceModelParmFile,crossValidVar=&crossValidVar);
      run;
      data _infile_(bufsize=128k);set _infile_;
       _residual_ = &target - model1; drop model1;run;

      %let _infile_=_infile_;
      %let _target_ = _residual_; run;
   %end;

   %if %upcase(&regtype.)=%upcase(stepwise) %then %do;
      ods output SelectionSummary=&outStatFile 
                 (rename=(VarEntered=variable step=tValue VarRemoved=estimate));
   %end; %else %do;                                          
      ods output ParameterEstimates=&outStatFile %str(); ;   
   %end;


   proc reg data=&_inFile_ outest=regout;
     %if "&weight" ne "" %then %do; weight &weight; %end;
          model1: model &_target_ = &varlist &offSetVars 
     %if "&modelOptions" ne "" %then %do; /&modelOptions %end;
   ;

   %if "&referenceModelParmFile" ne ""  and "&combineReferenceModel" = "1" %then %do;
        data regout;set regout;drop _model_ _type_ _depvar_ _rmse_ _residual_;
        if missing(intercept) then intercept=0;

        data _regout2_;set &referenceModelParmFile; _rmse_=0;
        %if "&renameReferenceInterceptAs" ne "" %then %do;
            if missing(intercept) then intercept = 0; 
            &renameReferenceInterceptAs = Intercept;
            intercept = 0; 
        %end;
        run;

        data regout;merge _regout2_(drop=_rmse_ rename=(INTERCEPT=INTERCEPT1))  regout;;
         INTERCEPT = INTERCEPT + INTERCEPT1;drop INTERCEPT1; run;
   %end;

   %if "&offSetVars" = "" %then %do;
   %end;%else %do;
       data regOut;set regOut;
          drop &offSetVars ;
   %end;

   %if "&outRegOut" ne "" %then %do;
       data &outRegOut ;set regout;
   %end;

   %if "&crossValidVar" ne "" %then %do;
       %do_reg_cv(inFile=&inFile,Target=&Target,VarList=&VarList,outRegOut=&outRegOut,Weight=&Weight,RegType=&RegType,offSetVars=&offSetVars,
              referenceModelParmFile=&referenceModelParmFile,combineReferenceModel=&combineReferenceModel,
              modelOptions=&modelOptions,outStatFile=&outStatFile,
              renameReferenceInterceptAs=&renameReferenceInterceptAs,crossValidVar=&crossValidVar);
   %end;

%mend;


/********************************************************************
Macro: do_genmod

Description:

Macro Parameters: 
   infile: the name of the SAS dataset to use for the regression 
   target: the name of the target variable (dependent variable)
   varList: List of independent variables to use in regression 
   outRegOut: the name of SAS dataset to which the regression parameters should be saved to
   outfile: the name of the SAS dataset which is the output of the PROC
   link: specifies the link function for the variable transformation
   offset: specifies the variable to offset the target variable
   dist: specifies the theoretical distribution the target variable is assumed to follow
   p: parameter is only applicable for tweedie distribution
   power: lamba if the power distribution is specified
   weight: variable in the infile dataset with values that are relative weights for a weighted
           leased square fit. Typically, manual premium is used as a weight.
   offSetVars: A list of variables for which model parameters will be derived but subsequently zeroed
               out for the scoring process
   referenceModelParmFile: parmfile containing the reference model parameter estimates
                           the regression model created by this macro will be a residual model 
   combineReferenceModel: 0 means not to combine the reference model with the new residual model
                          1 means the refernce model should be combined with the new residual model
   outStatFile: name of the output SAS file to which model statistics will be written to 
                This file can be read by printModelStats() macro to compare t-statistics from different models
   modelOptions: Additional model options to be specifieid for the PROC
   REPEATED: specifies the covariance structure on multivariate responses
   renameInterceptAs: the reference intercept term will be renamed to this name
   				if this is empty, the reference intercept will be added to the intercept term 
                of the combined full model
   ClASSvars: list of independent variables to use in regession as class variables
   crossValidVar: the Cross Validation Variable that will split the modeling dataset into
                  N-folds.  This variable must be numeric variable with values of 1 to 5
                  or 1 to 10, for example.
   

*************************************************************************/


%macro do_genmod(inFile,target,VarList,outRegOut,outFile,link=,offset=,dist=,p=,power=,weight=,
                 offSetVars=,referenceModelParmFile=,combineReferenceModel=1,outStatFile=,modelOptions=,REPEATED=,renameInterceptAs=Intercept,
                 CLASSvars=,crossValidVar=);

   %let _infile_=&infile;
   %let _target_ = &target; run;

   %if "&referenceModelParmFile" ne "" %then %do;
      proc sql; 
         drop table _infile_;
         drop view _infile_;
      quit;          

      %score(infile=&infile,outfile=_infile_,varList=,parmFile=&referenceModelParmFile,crossValidVar=&crossValidVar);
      run;
      
      data _infile_;set _infile_;
       %if "&link"="log" %then %do; 
            _residual_ = &target/exp(model1); 
       %end; %else %do;  
            _residual_ = &target - model1;  
       %end;
        drop model1; run; 


      %let _infile_=_infile_;
      %let _target_ = _residual_; run;
   %end;

   ods output ParameterEstimates=regout;

   proc genmod data=&_infile_ 
     %if %upcase(&link)= LOGIT  %then %str( descending ) ;
     NAMELEN=32 ;
     %if "&CLASSvars" ne "" %then %do; 
                class %str(&CLASSvars /missing order=freq descending ;);
     %end; %else %if "&REPEATED" ne "" %then %do;
           class %str(&REPEATED ;);                        
     %end;

     output pred=model1
    %if "&outfile" ne "" %then %do;
          out=&outFile
    %end;
      ;
	%if "&dist" eq "tweedie" %then %do;
		mu = _MEAN_;
	      	y    = _RESP_;
	        
              if y eq 0 then do; d = 2*mu**(2-&p)/(2-&p); end;
              if y ne 0 then do; d = 2*((y**(2-&p)-y*mu**(1-&p))/(1-&p) - (y**(2-&p)-mu**(2-&p))/(2-&p)); end;
	        
	        variance var   = mu**&p;
      	        deviance dev = d;
	     
		%if "&link" ne "power" %then %do;
      		%if "&offset" ne "" %then %do;
          			model &_target_= &varlist &offSetVars / link = &link  offset=&offset SCALE=PEARSON &modelOptions;
		      %end; %else %do;
          			model &_target_= &varlist &offSetVars / link = &link SCALE=PEARSON &modelOptions;
              	%end;
	    	%end; %else %do;
        		model &_target_= &varlist &offSetVars / link = power(&power) SCALE=PEARSON &modelOptions;
    		%end;
	%end;
	%else %do;

              %if "&link" ne "power" %then %do;
                 %if "&offset" ne "" %then %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = &link  offset=&offset SCALE=PEARSON &modelOptions;
                 %end; %else %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = &link SCALE=PEARSON &modelOptions;
                 %end;
              %end; %else %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = power(&power) &modelOptions;
              %end;
	%end;
    
    %if "&REPEATED" ne "" %then REPEATED SUBJECT=%str(&REPEATED ;);                        
    %if "&weight" ne "" %then %do; weight &weight; %end;
    run;
    
    data regout;set regout;
    if DF = 0 then delete;
    format variable $100.;variable=parameter; drop parameter; 


    %if "outStatFile" ne "" %then %do; 
       data &outStatFile; 
          set regout;
       run;
    %end;        
    




   %if "&offSetVars" = "" %then %do;
   %end;%else %do;
       data regOut;set regOut;
       %let _numOffsetVars_ = %eval(1 + %length(%cmpres(&offSetVars))- %length(%sysfunc(compress(&offSetVars)));
      %do _iCut_ = 1 %to &_numOffsetVars_;
           %let _thisVar_ = %scan( &offSetVars, &_iCut_,%str( ~ ) ); 
           if upcase("&_thisVar_") = upcase(variable) then delete;
      %end;
      %if "&renameInterceptAs" ne "" %then %do;
          if upcase("intercept") = upcase(variable) then variable = "&renameInterceptAs";
      %end;
   %end;

   ***If a reference model is given, combined it with the current model ***;
   %if "&referenceModelParmFile" ne ""  and "&combineReferenceModel" = "1" %then %do;
      %let _doTranspose_ = 0;
      data _null_;set &referenceModelParmFile nobs=reccnt;
      if _n_ > 1 then stop;
      if reccnt = 1 then call symput('_doTranspose_', "1" ) ;
      run;
      %if "&_doTranspose_" =  "1" %then %do;
        %_TransposeRegOut_(inRegParmFile=&referenceModelParmFile,outRegParmFile=_parmEst_); run;
      %end; %else %do;
          data _parmEst_;set &referenceModelParmFile;run;
      %end;

         data regout;set regout _parmEst_;
   %end;

   %if "&outRegOut" ne "" %then %do;
       data &outRegOut ;set regout;
   %end;
   
   %if "&crossValidVar" ne "" %then %do;
       %do_genmod_cv(inFile=&inFile,target=&target,VarList=&VarList,outRegOut=&outRegOut,outFile=&outFile,link=&link,offset=&offset,
                     dist=&dist,p=&p,power=&power,weight=&weight,offSetVars=&offSetVars,referenceModelParmFile=&referenceModelParmFile,
                     combineReferenceModel=&combineReferenceModel,outStatFile=&outStatFile,modelOptions=&modelOptions,REPEATED=&REPEATED,
                     renameInterceptAs=&renameInterceptAs,classVars=&classVars,crossValidVar=&crossValidVar);
   %end;
%mend;








%macro do_tweedie(inFile,target,VarList,outRegOut,outFile,link,offset,p,power);
      %do_genmod(inFile=&infile,target=&target,VarList=&varList,outRegOut=&outRegOut,outFile=&outFile,link=&link,offset=&offSet,p=&p,power=&power);
%mend;

/********************************************************************
Macro: do_glimmix
Last revised May 2014 by Seth Caughron (to make this work for logistic)

Description:

Macro Parameters: 
   infile: the name of the SAS dataset to use for the regression 
   target: the name of the target variable (dependent variable)
   varList: List of independent variables to use in regression 
   outRegOut: the name of SAS dataset to which the regression parameters should be saved to
   outfile: the name of the SAS dataset which is the output of the PROC
   link: specifies the link function for the variable transformation
   offset: specifies the variable to offset the target variable
   dist: specifies the theoretical distribution the target variable is assumed to follow
   p: parameter is only applicable for tweedie distribution
   power: lamba if the power distribution is specified
   weight: variable in the infile dataset with values that are relative weights for a weighted
           leased square fit. Typically, manual premium is used as a weight.
   offSetVars: A list of variables for which model parameters will be derived but subsequently zeroed
               out for the scoring process
   referenceModelParmFile: parmfile containing the reference model parameter estimates
                           the regression model created by this macro will be a residual model 
   combineReferenceModel: 0 means not to combine the reference model with the new residual model
                          1 means the refernce model should be combined with the new residual model
   outStatFile: name of the output SAS file to which model statistics will be written to 
                This file can be read by printModelStats() macro to compare t-statistics from different models
   modelOptions: Additional model options to be specifieid for the PROC
   renameInterceptAs: the reference intercept term will be renamed to this name
   				if this is empty, the reference intercept will be added to the intercept term 
                of the combined full model
   ClassVars: list of independent variables to use in regession as class variables
   BYVARS: specifies variables to obtain separate analyses on observations in the group
   blup: indicate Y or N for best linear unbiased predictors (BLUP);
				if Y then it uses the predictors of the random effects in computing the statistic
				if N then it does not
   ilink: indicate Y or N; if Y then it will compute compute the score on the scale side of the data
				if N then it will compute the score on the scale of the link function
   randEffects: accepts a string of SAS RANDOM statements
   technique: optimization technique which is specified in the NLOPTIONS statement
   method: specifies the estimation method used in the PROC
   crossValidVar: the Cross Validation Variable that will split the modeling dataset into
                  N-folds.  This variable must be numeric variable with values of 1 to 5
                  or 1 to 10, for example.
   maxiter: sets maximum number of iterations (added, Seth Caughron, May 2014)
   procOptions: specifies any additional options to pass to the PROC GLIMMIX

*************************************************************************/

%macro do_glimmix(inFile,target,VarList,outRegOut,outFile,link=,offset=,dist=,p=,power=,weight=,
			offSetVars=,referenceModelParmFile=,combineReferenceModel=1,outStatFile=,
			modelOptions=,renameInterceptAs=Intercept, CLASSvars=,BYVARS=,
			blup=Y,ilink=Y,randEffects=,technique=QUANEW,method=,crossValidVar=,procOptions=,maxiter=);


   %let _infile_=&infile;
   %let _target_ = &target; run;

   %if "&referenceModelParmFile" ne "" %then %do;
      proc sql; 
         drop table _infile_;
         drop view _infile_;
      quit;          

	%score(infile=&infile,outfile=_infile_,varList=,parmFile=&referenceModelParmFile,crossValidVar=&crossValidVar);

      run;
      
      data _infile_;set _infile_;
       %if "&link"="log" %then %do; 
            _residual_ = &target/exp(model1); 
       %end; %else %do;  
            _residual_ = &target - model1;  
       %end;
        drop model1; run; 
	
      %let offset = model1;
      %let _infile_=_infile_;
      %let _target_ = _residual_; run;
   %end;

   ods output ParameterEstimates=regout;
   ods output SolutionR=randout;

   * removed descending option for LOGIT ;
   proc glimmix data=&_infile_ 
     %if "&method" ne "" %then %str(method=&method);    
     NAMELEN=50 MAXOPT=200 NOCLPRINT=50 &procOptions.;
     %if "&BYVARS" ne "" %then %do; by &BYVARS; %end;
     %if "&CLASSvars" ne "" %then 	class %str(&CLASSvars;);
  

     	%if "&dist" eq "tweedie" %then %do;
		mu = _MEAN_;
	      	y    = _RESP_;
	        
              if y eq 0 then do; d = 2*mu**(2-&p)/(2-&p); end;
              if y ne 0 then do; d = 2*((y**(2-&p)-y*mu**(1-&p))/(1-&p) - (y**(2-&p)-mu**(2-&p))/(2-&p)); end;
	        
	        variance var   = mu**&p;
      	        deviance dev = d;
	     
		%if "&link" ne "power" %then %do;
      		%if "&offset" ne "" %then %do;
          			model &_target_= &varlist &offSetVars / link = &link  offset=&offset &modelOptions solution;
		      %end; %else %do;
          			model &_target_= &varlist &offSetVars / link = &link &modelOptions solution;
              	%end;
	    	%end; %else %do;
        		model &_target_= &varlist &offSetVars / link = power(&power) &modelOptions solution;
    		%end;
	%end;

	%else %do;
              %if "&link" ne "power" %then %do;
                 %if "&offset" ne "" %then %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = &link  offset=&offset &modelOptions solution;
                 %end; %else %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = &link &modelOptions solution;
                 %end;
              %end; %else %do;
                     model &_target_= &varlist &offSetVars / dist = &dist link = power(&power) &modelOptions solution;
              %end;
	%end;

        %if ("&technique" ne "" or "&maxiter" ne "") %then %do;
	    nloptions
            %if "&technique" ne "" %then %do; technique=&technique; %end;
            %if "&maxiter" ne "" %then %do; maxiter=&maxiter.; %end;
        %end;

	%if "%upcase(&blup)" ne "Y" %then %do; %let _blup_ = noblup; %end;
		%else %do; %let _blup_ = blup; %end;
	%if "%upcase(&ilink)" ne "Y" %then %do; %let _ilink_ = noilink; %end;
		%else %do; %let _ilink_ = ilink; %end;
	%if "&outfile" ne "" %then %do;
		output out=&outFile pred(&_blup_ &_ilink_)=model1; %end;

	%if "&randEffects" ne "" %then %do; random &randEffects; %end; 
  
	%if "&weight" ne "" %then %do; weight &weight; %end;
   
    run;
    
	%let _numCLASSvars_ = %eval(1 + %length(%cmpres(&CLASSvars)) - %length(%sysfunc(compress(&CLASSvars)));

    data regout;set regout;
	    if DF = . then delete;
	    informat variable $100. effect $50.;
	    variable=effect; 
	    effect="FixedEffect";
	    
    %if "&randEffects" ne "" and %sysfunc(exist(randout))=1 %then %do;
		proc sql noprint; select distinct scan(subject,1,' ') into :_subjects_ separated by " " from randout;

		%let _numSubjects_ = %eval(1 + %length(%cmpres(&_subjects_)) - %length(%sysfunc(compress(&_subjects_)));

		data randout (drop=subject); set randout;
			rename StdErrPred=StdErr;
			format variable $100. effect $50.;
			variable = scan(subject,1,' ');
			%do i=1 %to &_numSubjects_;
				if variable = "%scan(&_subjects_, &i," ")" then do;
					%scan(&_subjects_, &i," ") = %str(input(substr(subject,find(subject," ")+1),best12.););
				end;
			%end;
			
		data regout; set regout randout;
	%end;

    %if "outStatFile" ne "" %then %do; 
       data &outStatFile; retain variable;
          set regout;
    %end;

   %if "&offSetVars" = "" %then %do;
   %end;%else %do;
       data regOut;set regOut;
       %let _numOffsetVars_ = %eval(1 + %length(%cmpres(&offSetVars))- %length(%sysfunc(compress(&offSetVars)));
      %do _iCut_ = 1 %to &_numOffsetVars_;
           %let _thisVar_ = %scan( &offSetVars, &_iCut_,%str( ~ ) ); 
           if upcase("&_thisVar_") = upcase(variable) then delete;
      %end;
      %if "&renameInterceptAs" ne "" %then %do;
          if upcase("intercept") = upcase(variable) then variable = "&renameInterceptAs";
      %end;
   %end;

   ***If a reference model is given, combined it with the current model ***;
   %if "&referenceModelParmFile" ne ""  and "&combineReferenceModel" = "1" %then %do;
      %let _doTranspose_ = 0;
      data _null_;set &referenceModelParmFile nobs=reccnt;
      if _n_ > 1 then stop;
      if reccnt = 1 then call symput('_doTranspose_', "1" ) ;
      run;
      %if "&_doTranspose_" =  "1" %then %do;
        %_TransposeRegOut_(inRegParmFile=&referenceModelParmFile,outRegParmFile=_parmEst_); run;
      %end; %else %do;
          data _parmEst_;set &referenceModelParmFile;run;
      %end;

         data regout;set regout _parmEst_;
   %end;

   %if "&outRegOut" ne "" %then %do;
       data &outRegOut ; retain variable; set regout;
   %end;

   %if "&crossValidVar" ne "" %then %do;
       %do_glimmix_cv(inFile=&inFile,target=&target,VarList=&VarList,outRegOut=&outRegOut,outFile=&outFile,link=&link,
			offset=&offset,dist=&dist,p=&p,power=&power,weight=&weight,
			offSetVars=&offSetVars,referenceModelParmFile=&referenceModelParmFile,combineReferenceModel=&combineReferenceModel,outStatFile=&outStatFile,
			modelOptions=&modelOptions,renameInterceptAs=&renameInterceptAs, CLASSvars=&CLASSvars,BYVARS=&BYVARS,
			blup=&blup,ilink=&ilink,randEffects=&randEffects,technique=&technique,method=&method,crossValidVar=&crossValidVar);
   %end;
%mend do_glimmix;




/********************************************************************
Macro: score

Description:

Macro Parameters: 
   infile: the name of the input SAS dataset to score  
   outFile: output dataset that will have the scores 
   varList: List of independent variables
   parmFile: Dataset containing parameter estimates 

Input SAS files:
   &infile: the name of the input SAS dataset to score  
   &parmFile: Dataset containing parameter estimates 

Output SAS files:
   &outFile: output dataset that will have the scores 
Output SAS code:
      None

Sample Usage:  
   %do_reg(inFile=trnData,Target=lr99,VarList=&varList,
        outRegOut=mreg.regOut&modelNo,Weight=manPrem,RegType=); 

*********************************************************************/
%macro score(infile , outfile,varList,parmFile=regout,crossValidVar=);
   %let workPath = %sysfunc(pathname(work)) ; 

   %wrtRegEquation(regParmFile=&parmFile,outSAScode=&workpath./_sasCode_.sas,yhatFldName=model1,
                   crossValidVar=&crossValidVar,inFile=&inFile);
   run;
  data &outFile;set &infile;
  %include "&workpath./_sasCode_.sas";
  run;
%mend;









%macro makeSlice(infile, outFile, inField, weightField,numBuckets,outSliceField,cutoffs);
   %if "&cutOffs" ne "" %then %do;
        %let numBuckets=100;
   %end;
   %let _totprem_=1000;
   %if "&weightField" = "" or "&weightField" = "1" %then %do;
     %let weightField=1;
     data _null_;set &infile nobs=reccnt;
     if _n_ > 1 then stop;
     call symput('_TOTPREM_', reccnt ) ;
   %end;%else %do;
      proc summary data=&infile nway;
       var &weightField;
      output sum=_TOTPREM_ out=_tempTotal_;
      data _null_;set _tempTotal_;
      call symput('_TOTPREM_', _totprem_ ) ;
      run;
   %end;

   proc sort data=&infile out=&outFile;by &inField; run;


   data &outFile;
    set &outFile nobs=reccnt;
   retain _cumPrem_; drop _cumPrem_;
   if _N_=1 then _cumPrem_=0;
   _cumPrem_=_cumPrem_+ &weightField ;
   num_pol=1;
   &outSliceField= round((_cumPrem_/&_totprem_ * &numBuckets)+0.5 ,1); 
   &outSliceField = max(1,min(&outSliceField,&numBuckets));
   %if "&cutOffs" ne "" %then %do;
      %let _numCutOffs_ = %eval(1 + %length(%cmpres(&cutOffs))
                     - %length(%sysfunc(compress(&cutOffs)));
      %do _iCut_ = 1 %to &_numCutOffs_;
           if &outSliceField > 0 then _tempSliceField_ = &_iCut_;
           %let _thisCutOff_ = %scan( &cutOffs, &_iCut_); 
           &outSliceField = &outSliceField - &_thisCutOff_;
      %end;
      if &outSliceField > 0 then _tempSliceField_ = &_numCutOffs_ + 1;
       &outSliceField = _tempSliceField_;drop _tempSliceField_;

   %end;

%mend;









%macro _getAreaUnderCurve_(infile,score100Field,isDescending,
               numBuckets,bucketFrom,bucketTo,lossVars,outFile=);
  %if "&lossVars" = "" %then %do;
     %let _numLossVars_=0; %goto endOfMacro;
  %end; %else %do;
      %let _numLossVars_ = %eval(1 + %length(%cmpres(&lossVars))
                     - %length(%sysfunc(compress(&lossVars)));
  %end;

   %if "&bucketFrom" = "" %then %do;
       %let bucketFrom=1;
   %end;
   %if "&bucketTo" = "" %then %do;
       %let bucketTo=&numBuckets;
   %end;

    proc sort data=&infile out=_areaTemp_;
      by _type_ 
      %if "&isDescending" = "1" %then descending;
      &score100Field;

    data _areaTemp_;set _areaTemp_;
    %do _iArea_ = 1 %to &_numLossVars_;
        %let _thisLossVar_ = %scan( &lossVars, &_iArea_); 
        retain tot_&_thisLossVar_  cum_&_thisLossVar_ ;
       if _type_ = 0 then do;
          tot_&_thisLossVar_  = &_thisLossVar_ ;
          cum_&_thisLossVar_ = 0;
       end;
   %end;

   if _type_ = 0 then delete;

    _thisScore_ = &score100Field;
    %if "&isDescending" = "1" %then %do;
        _thisScore_ = &numBuckets - &score100Field + 1;
    %end;

    %do _iArea_ = 1 %to &_numLossVars_;
        %let _thisLossVar_ = %scan( &lossVars, &_iArea_); 
      cum_&_thisLossVar_  = sum(cum_&_thisLossVar_, &_thisLossVar_) ;
      &_thisLossVar_  = (cum_&_thisLossVar_ /tot_&_thisLossVar_ );
      &_thisLossVar_  = &_thisLossVar_  - (_thisScore_/&numBuckets);

      &_thisLossVar_  = &_thisLossVar_  /&numBuckets;
      &_thisLossVar_  = round(&_thisLossVar_ , .00009);

   %end;
   keep &score100Field &lossVars;

  data _areaSubSet_;set _areaTemp_;
  if &score100Field < &bucketFrom or 
     &score100Field > &bucketTo  then delete;
      
   proc summary data=_areaSubSet_;
    var &lossVars;
   output out=_areaSubSet_ sum=&lossVars;

   title5 "Area under '&score100Field' between &bucketFrom and &bucketTo ";
   proc print data=_areaSubSet_;
   var &LossVars;
   run;title5;run;

   %if "&outFile" = "" %then %do;
   %end; %else %do;
        data &outFile;set _areaTemp_;
        run;
   %end;
%endOfMacro:
%mend;









%macro getAreaUnderCurve(infile,score100Field,isDescending,
             numBuckets,bucketFrom,bucketTo,lossVars,outFile);

   proc summary data=&infile ; 
    class &score100Field;
    var &lossVars; 
    output out=_tempSummary_ sum=;

   %_getAreaUnderCurve_(infile=_tempSummary_,score100Field=&score100Field,isDescending=&isDescending,
                 numBuckets=&numBuckets,bucketFrom=&bucketFrom,bucketTo=&bucketTo,
                 lossVars=&lossVars,outFile=&outFile);


%mend;










%macro drawLiftCurve(infile,varList,modelNo,referenceVarList,outSummaryFile,outScoredFile,parmfile=regout,
                 summaryVars=,ratioList=, weightField=manPrem,crossValidVar=);
   title3  "Data Source = &infile ";

   %let _version_=;

   %if "&weightField" = "" %then %do;
      %let weightField=manPrem;
   %end;

   %if "&summaryVars"="" %then %do;
        %let _version_=old;

        %let summaryVars =  num_pol actPrem manPrem loss loss200k loss300k clmcnt; 
   %end;

   %if "&ratioList"="" %then %do;
     %let ratioList = %str(
     freq_a    clmcnt actPrem      1000
     lr_a      loss   actPrem      1
     lr200_a   loss200k actPrem    1
     lr300_a   loss300k actPrem    1
     freq_m    clmcnt   manPrem    1000
     lr_m      loss     manPrem    1
     lr200_m   loss200k manPrem    1
     lr300_m   loss300k manPrem    1
     );
   %end;

   %if "&outScoredFile" =  "" %then %do;
       %let outScoredFile=_temp_;
   %end;
   
   %let _cvInd_=;
   %if "&crossValidVar" ne "" %then %let _cvInd_=_cv;


   %score(infile=&infile,outFile=&outScoredFile,varList=&varList,parmfile=&parmFile,crossValidVar=&crossValidVar);


   data _null_;
     call symput("fdate",left(put("&sysdate"d,yymmdd.)));
     call symput ("ftime", left(right("&systime")) );
   run;

   %makeSlice(infile=&outScoredFile, outFile=&outScoredFile, 
              inField=model1&_cvIND_, weightField=&weightField,numBuckets=100,outSliceField=slice);
/*Added by Sakshi***/
DATA &outScoredFile;
			  SET &outScoredFile;
			  SLICE_1=101-SLICE;
			  DROP SLICE;
			  RENAME SLICE_1=SLICE;
			  RUN;
			  
   proc means data=&outScoredFile noprint; class slice;
   var &summaryVars;
   output out=_temp_ sum=;
 
   data _temp_;set _temp_;
   drop slice;
   slicePerc="             ";sortOrder=.;
   if slice=. then do;
      slicePerc='Total   '; sortOrder=10; output;return;
   end;

   if      slice < 11 then slicePerc="00%-10%";
   else if slice < 21 then slicePerc="10%-20%";
   else if slice < 31 then slicePerc="20%-30%";
   else if slice < 41 then slicePerc="30%-40%";
   else if slice < 51 then slicePerc="40%-50%";
   else if slice < 61 then slicePerc="50%-60%";
   else if slice < 71 then slicePerc="60%-70%";
   else if slice < 81 then slicePerc="70%-80%";
   else if slice < 91 then slicePerc="80%-90%";
   else slicePerc="90%-100%";
   sortOrder=2; output;

   if      slice < 26 then slicePerc="00%-25%";
   else if slice < 51 then slicePerc="25%-50%";
   else if slice < 76 then slicePerc="50%-75%";
   else slicePerc="75%-100%";
   sortOrder=1; output;

   if slice >90 and slice < 96 then do;
      slicePerc="90%-95%"; sortOrder=3; output;
   end;
   if slice >95 then do;
      slicePerc="95%-100%";sortOrder=3; output;
   end;
   if slice = 96 then do;
      slicePerc="95%-96%";sortOrder=4; output;
   end;
   if slice = 97 then do;
      slicePerc="96%-97%";sortOrder=4; output;
   end;
   if slice = 98 then do;
      slicePerc="97%-98%";sortOrder=4; output;
   end;
   if slice = 99 then do;
      slicePerc="98%-99%";sortOrder=4; output;
   end;
   if slice = 100 then do;
      slicePerc="99%-100%";sortOrder=4; output;
   end;

   proc means noprint nway; class sortOrder slicePerc;
   var &summaryVars;
   output out=_temp_ sum=;


   proc sort data=_temp_;by descending sortOrder slicePerc;


   ***Separate ratioList to 4 separate lists *****;
    %let _ratioVars_=;
    %let _relativityVars_=;
    %let _numeratorVars_=;
    %let _denomVars_=; 
    %let _scaleVars_=; 

    %let _numOfVars_ = %eval(1 + %length(%cmpres(&ratioList)) -
                             %length(%sysfunc(compress(&ratioList)));
    %do _iLift_ = 1 %to &_numOfVars_ %by 4;
          %let _thisVar_ = %scan( &ratioList, &_iLift_);
          %let _ratioVars_= &_ratioVars_ &_thisVar_;  
          %let _relativityVars_=&_relativityVars_ &_thisVar_._REL ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+1));
          %let _numeratorVars_= &_numeratorVars_  &_thisVar_; 

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+2));
          %let _denomVars_= &_denomVars_ &_thisVar_ ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+3));
          %let _scaleVars_= &_scaleVars_ &_thisVar_ ;      
    
    %end;


    ***Create list of 'Percentage' and 'Total' variable names ****;
    %let _PERCvars_=;
    %let _TOTvars_=;
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
    %do _iLift_ = 1 %to &_numOfVars_;
          %let _thisVar_ = %scan( &summaryVars, &_iLift_);
          %let _PERCvars_=&_PERCvars_ &_thisVar_._PERC ;  
    %end;

   data _temp_;
     set _temp_;

    %let _numOfSummaryVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
   ***Calculate percentages of totals ***;
    %do _iRatio_ = 1 %to &_numOfSummaryVars_;
          %let _thisSummaryVar_ = %scan( &summaryVars, &_iRatio_); 
          &_thisSummaryVar_._PERC =0;
        if sortOrder = 10 then do;
            t_&_thisSummaryVar_ = &&_thisSummaryVar_ ; retain t_&_thisSummaryVar_ ; drop t_&_thisSummaryVar_ ;
        end; 
        if not missing(t_&_thisSummaryVar_.) then do;
           &_thisSummaryVar_._PERC   =  &_thisSummaryVar_.   / t_&_thisSummaryVar_. ;
        end;
    %end;

    %let _numOfRatioVars_ = %eval(1 + %length(%cmpres(&_denomVars_)) -
                             %length(%sysfunc(compress(&_denomVars_)));

   ***Calculate ratios***;
    %do _iRatio_ = 1 %to &_numOfRatioVars_;
          %let _thisRatioVar_ = %scan( &_ratioVars_, &_iRatio_); 
          %let _thisNumeratorVar_ = %scan( &_numeratorVars_, &_iRatio_); 
          %let _thisDenomVar_ = %scan( &_denomVars_, &_iRatio_); 
          %let _thisScaleVar_ = %scan( &_scaleVars_, &_iRatio_); 

         &_thisRatioVar_ = 0;&_thisRatioVar_._REL=0;
         if &_thisDenomVar_ ne 0 then do;
            &_thisRatioVar_ = &_thisNumeratorVar_  * &_thisScaleVar_ / &_thisDenomVar_ ; 
         end;
        if sortOrder = 10 then do;
            t_&_thisRatioVar_ = &&_thisRatioVar_ ; retain t_&_thisRatioVar_ ; drop t_&_thisRatioVar_ ;
        end; 
        if not missing(t_&_thisRatioVar_.) then do;
            &_thisRatioVar_._REL   =  &_thisRatioVar_.   / t_&_thisRatioVar_. - 1;
        end;

    %end;

   format &summaryVars comma14.0 &_ratioVars_ comma7.3 &_relativityVars_ &_PERCvars_  percent6.1;



proc sort data=_temp_;by sortOrder slicePerc;run;

proc print data=_temp_; id slicePerc;
  var &summaryVars ; 
;run;

proc print data=_temp_; id slicePerc;
  var &_PERCvars_ ; 
;run;

proc print data=_temp_; id slicePerc;
  var &&_ratioVars_ 
;run;
proc print data=_temp_; id slicePerc;
  var &_relativityVars_ 
;run;

   %if "&outSummaryFile" ne "" %then %do;
      %if "&referenceVarList" = "" %then %do;
         %let _numwords_=0;
      %end; %else %do;
         %let _numwords_ = %eval(1 + %length(%cmpres(&referenceVarList))
                        - %length(%sysfunc(compress(&referenceVarList )));
      %end;
      data _temp_;set _temp_;
      modelno = &modelno;
      format runtime infile &referenceVarList $30.;
      runtime = "&fdate &ftime";infile = "&infile";
      %do _iLift_ = 1 %to &_numwords_;
        %let _thisWord_ = %scan( &referenceVarList , &_iLift_); 
        &_thisWord_="&&&_thisWord_"; 
      %end; 
      %if "&_version_" = "old" %then %do;
          pcnt_pol = num_pol_PERC; drop &_PERCvars_ ;
      %end;

      proc append base=&outSummaryFile data=_temp_  force;
   %end;
   run;title3;run;
%mend;












%macro getSummaryInExcel(inSummaryFile,firstModelNo,lastModelNo,
           referenceVarList,varsInSummary,outCSVFile,otherModelId=, calcAveragesBy=);
  %if "&varsInSummary" = "" %then %do;
      %let varsInSummary=%str(lr_m_rel lr200_m_rel freq_m_rel pcnt_pol);
  %end;

  %if "&referenceVarList" = "" %then %do;
     %let _numwords_=0;
  %end; %else %do;
      %let _numwords_ = %eval(1 + %length(%cmpres(&referenceVarList))
                     - %length(%sysfunc(compress(&referenceVarList )));
  %end;

   %let _refVarList1_=;
   %let _refVarList2_=;
   %do _iExcel_ = 1 %to &_numwords_;
     %let _thisWord_ = %scan( &referenceVarList , &_iExcel_); 
     %let _refVarList1_=&_refVarList1_ &_thisWord_ @ ; 
     %let _refVarList2_=&_refVarList2_ &_thisWord_ c ; 
   %end; 

  proc sort data=&inSummaryFile out=_temp_;
     by &otherModelId modelNo descending runtime;

   data _temp_;set _temp_;
     by &otherModelId modelno;
    retain h_runtime;
    if first.modelno then do;
         h_runtime = runtime;
    end;
    if runtime = h_runtime;
    %if "&firstModelNo" ne "" %then %do;
         if modelNo ge &firstModelNo ;
    %end;
    %if "&lastModelNo" ne "" %then %do;
         if modelNo le &lastModelNo ;
    %end;


   proc sort data=_temp_;by &otherModelId modelNo infile;

   %if "&varsInSummary" = "" %then %do;
      %let _numwords_=0;
   %end; %else %do;
      %let _numwords_ = %eval(1 + %length(%cmpres(&varsInSummary))
                     - %length(%sysfunc(compress(&varsInSummary)));
   %end;

   %do _iExcel_ = 1 %to &_numwords_;
     %let _thisWord_ = %scan( &varsInSummary, &_iExcel_); 
     data _tempOne_;set _temp_;
       by &otherModelId modelno infile ;
     retain s00_10 s10_20 s20_30 s30_40 s40_50 s50_60 s60_70 s70_80 s80_90 s90_100
           s90_95 s95_100 
           s00_25 s25_50 s50_75 s75_100;
             
     if first.infile then do;
           s00_10=.; s10_20=.; s20_30=.; s30_40=.; s40_50=.; s50_60=.; s60_70=.; s70_80=.; s80_90=.; s90_100=.;
           s90_95=.; s95_100=.; 
           s00_25=.; s25_50=.; s50_75=.; s75_100=.;
     end;
     

     if slicePerc="00%-10%" then s00_10=&_thisWord_ ;
     if slicePerc="10%-20%" then s10_20=&_thisWord_ ;
     if slicePerc="20%-30%" then s20_30=&_thisWord_ ;
     if slicePerc="30%-40%" then s30_40=&_thisWord_ ;
     if slicePerc="40%-50%" then s40_50=&_thisWord_ ;
     if slicePerc="50%-60%" then s50_60=&_thisWord_ ;
     if slicePerc="60%-70%" then s60_70=&_thisWord_ ;
     if slicePerc="70%-80%" then s70_80=&_thisWord_ ;
     if slicePerc="80%-90%" then s80_90=&_thisWord_ ;
     if slicePerc="90%-100%" then s90_100=&_thisWord_ ;

     if slicePerc="00%-25%"  then s00_25=&_thisWord_ ;
     if slicePerc="25%-50%"  then s25_50=&_thisWord_ ;
     if slicePerc="50%-75%"  then s50_75=&_thisWord_ ;
     if slicePerc="75%-100%" then s75_100=&_thisWord_ ;
 
     if slicePerc="95%-100%" then s95_100=&_thisWord_ ;
     if slicePerc="90%-95%" then s90_95=&_thisWord_ ;

     format fieldName $30.; fieldName="&_thisWord_";

     if last.infile then output;
     run;

     proc append base=_tempAll_ data=_tempOne_ force;
   %end; 

   %if "&calcAveragesBy" ne "" %then %do;
           proc sort data=_tempAll_ ;by &calcAveragesBy infile fieldName;
           PROC SUMMARY data=_tempAll_ nway missing;
             by &calcAveragesBy infile fieldName;
          var s00_10 s10_20 s20_30 s30_40 s40_50 s50_60 s60_70 s70_80 s80_90 s90_100 s00_25 s25_50 s50_75 s75_100 s95_100 s90_95;
          output out=_averagesFile_ mean=;
          data _averagesFile_;set _averagesFile_; if _freq_ = 1 then delete; drop _freq_;
         
          data _minMaxFile_;set _tempAll_;
             by &calcAveragesBy infile fieldName;
          %do _iExcel_ = 1 %to &_numwords_;
             %let _thisWord_ = %scan( &referenceVarList, &_iExcel_); 
             if first.fieldName then &_thisWord_._h = &_thisWord_; 
             retain &_thisWord_._h; drop &_thisWord_._h;
             if &_thisWord_._h ne &_thisWord_ then &_thisWord_._h = "";
             &_thisWord_  = &_thisWord_._h; 
          %end;
          keep &calcAveragesBy infile fieldName &referenceVarList;
          if last.fieldName and not first.fieldName then output;
          return;

          data _averagesFile_;merge _averagesFile_ _minMaxFile_;
          by &calcAveragesBy infile fieldName;

          proc append base=_tempAll_ data=_averagesFile_ force;


   %end;

   proc sort data=_tempAll_;
   by &calcAveragesBy modelNo infile fieldName;

   data _null_;set _tempAll_;
   c="@";x=-1;
   file "&outCSVFile" lrecl=32767 ;


   if _n_ = 1 then do;
      line1="model@infile@fieldName@";
      line2="&_refVarList1_";

      line3="00%-25%@25%-50%@50%-75%@75%-100%@ @";
      line4="00%-10%@90%-100%@ @";
      line5="95%-100%@ @";
      put "sep=@";
      put line1 line2 line3 line4 line5;
   end;

 put modelno +x c +x infile +x c+x fieldName +x c +x
     &_refVarList2_ 
     s00_25 +x c +x s25_50 +x c +x s50_75 +x c +x s75_100 +x c +x  c 
     s00_10 +x c +x s90_100 +x c +x c
     s95_100 +x c +x c
    ;
   *modified by zbs 6/20/06
   *proc sql;drop table _tempAll_; 
%mend;










%macro _TransposeRegOut_(inRegParmFile=regOut,outRegParmFile=parmEst);
   **Find the name of dependent variable ***;
   %let DEPVAR=;
   data _null_;set &inRegParmFile;
   call symput('DEPVAR', _depvar_ ) ;run;  

   proc contents data=&inRegParmFile
        noprint out=_allVars_;  run;
   data _null_;set _allvars_;
   file  "&workpath./_sasCode_.sas"  notitle linesize=250 pagesize=500;
   if _n_ = 1 then do;
     put " data &outRegParmFile;set &inRegParmFile;";
     put "keep variable estimate sortseq;";
     put "format variable $100.;";
   end;
   if substr(name,1,1) ne "_"  and 
      compress(upcase("&DEPVAR")) ne compress(upcase(name)) then do;
      put " variable = '" name "';" ;  
      put " estimate = " name ";" ; 
      if compress(upcase(name)) = "INTERCEPT" then do;
         put " sortSeq = 0;";
      end;else do;
         put " sortSeq = 1;";
      end;
      put "output;";
   end;
   run;
   %include "&workpath./_sasCode_.sas"  ;

   proc sort data=&outRegParmFile;by sortseq variable ;
   
   data &outRegParmFile;set &outRegParmFile;
   if estimate = . then estimate = 0;
   variable = compress(upcase(variable ));

%mend;












%macro wrtRegEquation(regParmFile,outSAScode,yhatFldName,crossValidVar=,inFile=);

   %if "&yhatFldName" = "" %then %do;
       %let yhatFldName=yhat;
   %end;
  

   %let _doTranspose_ = 0;
   data _null_;set &regParmFile nobs=reccnt;
     if _n_ > 1 then stop;
     if reccnt = 1 then call symput('_doTranspose_', "1" ) ;
   run;
   %if "&_doTranspose_" =  "1" %then %do;
     %_TransposeRegOut_(inRegParmFile=&regParmFile,outRegParmFile=parmEst); run;
     %let regParmFile = parmEst; 
   %end;

  %wrtScoreCode(unwindParmFile=&regParmFile,outSAScode=&outSAScode,yhatFldName=&yhatFldName,
                mod=,crossValidVar=&crossValidVar,inFile=&inFile);  

%mend;












/********************************************************************
Macro: unwindParms

This macro un-winds the regression parameter estimates from PC variable to raw variables.  
The unwound parameters are written to a SAS file given by &outParmFile. 
Also, this macro writes a new macro given by &scoreMacroNm. This new macro contains the scoring 
formula based on the unwound parameters. 

Macro Parameters: 
   &regParmFile: Name of the parameters file created by the outest= statement in PROC REG.
   &princompfile: Name of the SAS file containing Principal component specifications. 
   &outWindFile: Name of the SAS dataset to which the unwound parameters should be written to.
   &outSAScode: The name of the SAS program that should be created that will contain the scoring formula
                  based on unwound parameters.

Input SAS files:
    &regParmFile, &princompFile

Output SAS files:
   &outParmFile: Contains unwound parameter estimates. Contains two columns, namely: variable, estimate
                   'variable' column contains the raw variable name. 
                   'Estimate' column contains the corresponding variable's unwound parameter estimate.

 Output SAS code:

	This macro also writes out SAS code in a file called &outSAScode. This SAS code contains the scoring logic
      based on the unwound parameters. 


Sample Usage:

    %unwindParms(regParmFile=mautReg.regout2_2001,
                 princompFile=mautReg.princomp,
                 outParmFile=parm2001,
                 outSAScode=unwind2001.sas)

    data a;set mautcap.modeling_data_auto (obs=5000);
    %include "PCcode.sas";
    %include "unwind2001.sas" ; model2=yhat;

    proc score data=a score=mautReg.regout2_2001 out=a type=parms; var &varlist; run;

    data a;set a; diff = model1 - model2;
    proc univariate data=a;var diff;

    

    The above call to the macro unwinds the paramters for model number 2001. The above code also shows you
    how you can do a QC to check if the unwound parameters generate the same score compared to PROC SCORE. 
    In the above example, 'model2' is score based on unwound parameters and 'model1' is score based on 
    original parameters. The PROC UNIVARIATE shows the distribution of the difference between these two scores.
    If everything worked right, this difference should be all very very small.


    If you want to compare parameter estimates from two or more models, use the following sample code:

    %unwindParms(regParmFile=mautReg.regout2_2001,
                 princompFile=mautReg.princomp,
                 outParmFile=parm2001,
                 outSAScode=unwind2001.sas)
    %unwindParms(regParmFile=mautReg.regout2_2002,
                 princompFile=mautReg.princomp,
                 outParmFile=parm2002,
                 outSAScode=unwind2002.sas)
    %unwindParms(regParmFile=mautReg.regout2_2003,
                 princompFile=mautReg.princomp,
                 outParmFile=parm2003,
                 outSAScode=unwind2003.sas)

     proc sort data=parm2001(rename=(estimate=estimate_2001));by variable;
     proc sort data=parm2002(rename=(estimate=estimate_2002));by variable;
     proc sort data=parm2003(rename=(estimate=estimate_2003));by variable;

     data allModels;merge parm2001 parm2002 parm2003;
     by variable;  

     proc export data=allModels outFile="reg_allModels.csv"
        DBMS=CSV REPLACE;run; 

 
*********************************************************************/
%macro unwindParms(regParmFile,princompFile,outParmFile,outSAScode=,yhatFldName=yhat,crossValidVar=,inFile=);
   %let workPath = %sysfunc(pathname(work)) ; ** The SAS work directory ***;

   **Find the name of dependent variable ***;
   %let DEPVAR=;
   data _null_;set &regParmFile;
   call symput('DEPVAR', _depvar_ ) ;run;    

   %let _doTranspose_ = 0;
   data _null_;set &regParmFile nobs=reccnt;
     if _n_ > 1 then stop;
     if reccnt = 1 then call symput('_doTranspose_', "1" ) ;
   run;
   %if "&_doTranspose_" =  "1" %then %do;
     %_TransposeRegOut_(inRegParmFile=&regParmFile,outRegParmFile=parmEst); run;
     %let regParmFile = parmEst; 
   %end; %else %do;
      data parmEst;set &regParmFile; variable = compress(upcase(variable ));run;
   %end;





   ****Get number of level fields in the file *****;
   %let _NUMlevels_=0;
   proc contents data=parmEst
        noprint out=_allVars_;  run;
   data _allvars_;set _allvars_;
   if substr(upcase(name),1,5) = "LEVEL";

    data _null_; set _allvars_ nobs=reccnt;
    if _n_ > 1 then stop;
    call symput('_NUMlevels_', reccnt ) ;
      run;
   run;  
   %let _NUMlevels_=&_NUMlevels_;

   %if &_NUMlevels_ > 0 %then %do;
       data classVars;set parmEst;
       array levels level1-level&_NUMlevels_;
       array flags flag1-flag&_NUMlevels_;

       do i = 1 to &_NUMlevels_;
          flags(i)=0;
          if not missing(levels(i)) then flags(i) = 1;
       end;
       keep variable flag1-flag&_NUMlevels_;
   
       proc summary data=classVars nway;
       class variable; var flag1-flag&_NUMlevels_;
       output out=classVars MAX=;

       proc sort nodupkey data=classVars;by variable;

       data classVars;set classVars;
       array flags flag1-flag&_NUMlevels_;
       numOfLevels = 0; do i = 1 to &_NUMlevels_; numOfLevels = numOfLevels + flags(i); end;
       if numOfLevels > 0 ; keep variable;

       proc sort data=parmEst;by Variable;

       data parmEst (keep=variable estimate) classEst(keep= variable estimate level1-level&_NUMlevels_);
       merge parmEst classVars (in=in_class);by variable;
       if in_class then do;
            output classEst;
       end; else do;
            output parmEst; 
       end;
   %end; 







   *** Bring in the principal component file ***;
   data princomp;set &princompFile;
   keep pcName varName eigen mean std;
   pcName = compress(upcase(pcName));
   varName = compress(upcase(varName)); 


   ***Merge principal component file with variable estimate file ***;
   proc sql noprint; create table work.final as 
   select est.variable  ,est.estimate
          ,pc.pcname ,pc.varname ,pc.eigen ,pc.mean ,pc.std
   from work.parmEst as est left join princomp  as pc
         on pc.pcname = est.variable; run;
   

   ***Calculate the unwound parameter estimates ***;
   data final;set final;
   keep variable estimate; 
   if eigen =  . then do;
      output;
   end; else do;
      variable = varName;
      estimate = estimate * eigen / std ; 
      output;
      variable = "INTERCEPT";
      ****estimate has already been modified above. ***;
      ****So, to get the intercept term we only need to multiply by (-)mean ****;
      estimate =  (-1) * estimate * mean ; 
      output;
   end; 
   proc summary data=final nway;
   class variable;
   var estimate;
   output out=&outParmFile sum=;

   %if &_NUMlevels_ > 0 %then %do;
      data &outParmFile;set classEst &outParmFile;
      proc sort data=&outParmFile;by variable;
   %end;

   %wrtScoreCode(unwindParmFile=&outParmFile,outSAScode=&outSAScode,yhatFldName=&yhatFldName);
   
   %if "&crossValidVar" ne "" and "&inFile" ne "" %then %do;
      %wrtScoreCode_cv(unwindParmFile=&regParmFile,outSAScode=&outSAScode,yhatFldName=&yhatFldName,
                       crossValidVar=&crossValidVar,inFile=&inFile);
   %end;

%mend;








/********************************************************************
Macro: wrtScoreCode

This macro writes the Scoring equation as a SAS code  

Macro Parameters: 
   &unwindParmFile: Name of the transposed parameters file 
   &outSAScode: The name of the SAS program that should be created that will contain the scoring formula
                  based on unwound parameters.
   &yhatFldName: This is name of the yhat field name to be written in the SAS code

Input SAS files:
    &unwindParmFile

Output SAS files: none

 Output SAS code:

	This macro writes out SAS code in a file called &outSAScode. This SAS code contains the scoring logic
      based on the unwound parameters. 

****************************************************/
%macro wrtScoreCode(unwindParmFile,outSAScode=,yhatFldName=yhat,mod=,crossValidVar=,inFile=);

   %if "&yhatFldName" = "" %then %do;
       %let yhatFldName=yhat;
   %end;

   %if "&outSAScode" = "" %then %do;
       %goto endOfMacro;
   %end;

   ****Get number of level fields in the file *****;
   %let _NUMlevels_=0;
   proc contents data=&unwindParmFile
        noprint out=_allVars_;

   %let _GLIMMIX_ = 0;

   data _null_; set _allVars_;
   	if name="Effect" then do;
		call symput('_GLIMMIX_', '1');
	end;
   run;
	%put &_GLIMMIX_;
   %if &_GLIMMIX_ eq 1 %then %do;
   	%wrtScoreCode_Glimmix(unwindParmFile=&unwindParmFile,outSAScode=&outSAScode,
		yhatFldName=&yhatFldName,mod=&mod,crossValidVar=&crossValidVar,inFile=&inFile);
	%goto endOfMacro;
   %end;
   run;

   data _allvars_;set _allvars_;
   if substr(upcase(name),1,5) = "LEVEL";

    data _null_; set _allvars_ nobs=reccnt;
    if _n_ > 1 then stop;
    call symput('_NUMlevels_', reccnt ) ;
      run;
   run;  
   %let _NUMlevels_=&_NUMlevels_;

   %if &_NUMlevels_ > 0 %then %do;
       data classVars;set &unwindParmFile;
       array levels level1-level&_NUMlevels_;
       array flags flag1-flag&_NUMlevels_;

       do i = 1 to &_NUMlevels_;
          flags(i)=0;
          if not missing(levels(i)) then flags(i) = 1;
       end;
       keep variable flag1-flag&_NUMlevels_;
   
       proc summary data=classVars nway;
       class variable; var flag1-flag&_NUMlevels_;
       output out=classVars MAX=;

       proc sort nodupkey data=classVars;by variable;

       proc sort data=&unwindParmFile out=temp;by Variable;

       data temp;merge temp classVars (in=in_class);by variable;
   %end; %else %do;
       data temp;set &unwindParmFile;
   %end;

   filename delfile "&outSAScode.";
   data _null_;
   rc=fdelete('delfile');
   run;

   data temp;set temp;
   sortSeq = 1;
   if upcase(variable) = "INTERCEPT" then sortSeq=0;
   proc sort data=temp;by sortSeq variable; 

   data _null_;set temp end=lastobs;
   file  "&outSAScode"  notitle &mod linesize=250 pagesize=500;
   if _n_ = 1 then do;
     put "/*This code was created by wrtScoreCode macro    */";
     put "   &yhatFldName = 0 ";
   end;
   if upcase (variable)= "INTERCEPT" then do;
     put " + (" estimate ")"  ;  return;
   end;

   if &_NUMlevels_ = 0 then do;
      put " + " variable " * (" estimate ")"  ;  return;
   end;

   array levels level1-level&_NUMlevels_; 
   array flags flag1-flag&_NUMlevels_;
 
  i=1; do while (not missing(scan(variable,i)));  i=i+1; end;
  numOfVariables=i-1;

  numOfLevels = 0; do i = 1 to &_NUMlevels_; numOfLevels = numOfLevels + flags(i); end;

  if numOfLevels = 0 then do; put " + " variable " * (" estimate ")"  ;  return;  end;

  if numOfLevels > numOfVariables then do; put " + thisIsJunk * " variable " * (" estimate ")"  ;  return;  end;

   put " + 1 " @;x=-1; firstLevelIndex = numOfVariables - numOfLevels + 1;
   format thisVar $50.;
   do i = 1 to numOfVariables;
       thisVar = scan(variable,i);
       if thisVar = "" then leave;
       j = i - firstLevelIndex +1; 
       if i < firstLevelIndex then do;
           put " * (" thisVar ")" @; 
       end; else do;
           if missing(levels(j)) then do;
              put " * (" thisVar "='')" @;  
           end;else do;
              if compress(levels(j),"1234567890.") = "" then do; 
                 put " * (" thisVar "=" levels(j) ")" @;  
              end;else do;
                 put " * (" thisVar "='" levels(j) +x "')" @;  
              end;
           end;
       end;      
   end;
   put " * (" estimate ")"  ;  

   

   data _null_;set temp end=lastobs;
   file  "&outSAScode"  mod notitle linesize=250 pagesize=500;   
   put '   ;'; stop;


      run;
      
   %if "&crossValidVar" ne "" and "&inFile" ne "" %then %do;
       %wrtScoreCode_cv(unwindParmFile=&unwindParmFile,outSAScode=&outSAScode,yhatFldName=&yhatFldName,
                        crossValidVar=&crossValidVar,inFile=&inFile);
   %end;      

%endOfMacro:
%mend;



%macro _getVarIndex_(varname=,iterations=);
	do j=1 to &iterations;
		if &varname = numericVars(j) then leave;
	end;
  if &varname = numericVars(j) then index =j; else index=0;
%mend _getVarIndex_;


%macro wrtScoreCode_GLIMMIX(unwindParmFile,outSAScode=,yhatFldName=yhat,mod=,crossValidVar=,inFile=);

	%if "&yhatFldName" = "" %then %do;
		%let yhatFldName=yhat;
	%end;

	%if "&outSAScode" = "" %then %do;
		%goto endOfMacro;
	%end;

	proc sql; select count(*) into :_numOfTerms_ from &unwindPArmfile; quit;
	%let _numOfTerms_ = %cmpres(&_numOfTerms_); %put "Number of Terms in the model = &_numOfTerms_";

	proc contents data=&unwindPArmfile noprint out=_varList_; data _varList_; set _varList_; if type=1;
	proc sort data=_varList_; by varnum;

	proc sql; select count(*) into :_numOfTerms_ from _varList_; quit;
	%let _numOfTerms_ = %cmpres(&_numOfTerms_); %put "Number of Terms in the model = &_numOfTerms_";


	data _varList_ ; set _varList_ end=lastobs;
		array numericVars(*) $32. num1-num&_numOfTerms_; 
		if _n_ =1 then i = 0;
		retain i num1-num&_numOfTerms_;
		i=i+1;
		numericVars(i) = name;
		keep num1-num&_numOfTerms_; if lastobs then output;

	data _null_; set &unwindParmFile;
		file "&outSAScode" notitle &mod linesize=250 pagesize=500;
		
		array specialVals{*} _numeric_;
		array numericVars(*) $32. num1-num30;

		if _n_ = 1 then do;
			set  _varList_; retain num1-num&_numOfTerms_;
			
			put "/*	This code was created by wrtScoreCode_GLIMMIX macro	*/";
			put "	&yhatFldName = 0 ";
		end;
		if upcase(variable)="INTERCEPT" then do;
			put " + (" estimate ")"  ; return;
		end;

		put " + 1 * " @;

		do i=1 to 10;
			thisVar = scan(variable,i,"*");
			if missing(thisVar) then leave;
			%_getVarIndex_(varname=thisVar,iterations=&_numOfTerms_);
			if index=0 then do;
				put " (" thisVar ") * " @;
			end; else do;
				if effect eq "FixedEffect" then do;
					put " (" thisVar " = " specialVals(index) " ) * " @; end;
				else do;
					index2=index;
					%_getVarIndex_(varname=effect,iterations=&_numOfTerms_);
					if index=0 then do; 
						put " (" thisVar " = " specialVals(index2) " ) * (" effect " ) * " @;
					end; else do;
						put " (" thisVar " = " specialVals(index2) " ) * (" effect " = " specialVals(index) ") * " @;
					end;
				end;
			end;
		end;

		put "(" estimate ")";
	data _null_; set &unwindParmFile end=lastobs;
		file "&outSAScode" mod notitle linesize=50 pagesize=500;
		put '	;'; stop;
	run;
	



%endOfMacro:
%mend wrtScoreCode_GLIMMIX;



%macro _getStat_(infile, classVar, varList, stat, outFile);
   proc means noprint data=&infile; class &classVar;
           var &varlist;
           output out=&outFile &stat=;
   run;
   data &outFile; set &outFile; stat="        "; stat="&stat"; drop _type_; 
%mend;


%macro crtMeanBySliceReport(inFile, yhatFieldName, 
             weightField, meanVarList, outMeanBySliceFile=meanBySliceFile, statList=MIN MAX MEAN);

    data _tempCMBSR_;set &infile(obs=5000);cnt=1;
    proc summary data=_tempCMBSR_ nway missing;
     class &yhatFieldName; var cnt;
    output out=_tempCMBSR_ sum=;
    %let _numUnique_=0;
    data _null_;set _tempCMBSR_ nobs=reccnt;
     if _n_ > 1 then stop;
     call symput('_numUnique_', reccnt ) ;run;
    %if &_numUnique_ > 20 %then %do;
       %makeSlice(infile=&inFile, outFile=_scoredFile_, 
        inField=&yhatFieldName, weightField=&weightField,numBuckets=10,outSliceField=score10);run;
       %let _FieldName_=score10;
       %let _sourceFile_ = _scoredFile_;
       run;
    %end; %else %do;
       %let _FieldName_=&yhatFieldName;
       %let _sourceFile_ = &inFile;
    %end;

  %let _numStats_ = %eval(1 + %length(%cmpres(&statList))
                     - %length(%sysfunc(compress(&statList)));

  %do _iStatVars_ = 1 %to &_numStats_;
       %let _thisStatVar_ = %scan( &statList, &_iStatVars_); 

      %_getStat_(infile=&_sourceFile_, classVar=&_FieldName_, varList=&meanVarList, 
        stat=&_thisStatVar_, outFile=_statFile_);

       data _statFile_;set _statFile_;
       format inFile fieldName fieldValue $50.;
       infile = "&infile";
       fieldName = "&_FieldName_";
       fieldValue = &_FieldName_; drop &_FieldName_;
       proc append data=_statFile_ base=&outMeanBySliceFile;

  %end;


%mend; 

%macro prtMeanBySliceReport(inMeanBySliceFile, meanVarList, outCSVfile);
   %if "&meanVarList" = "" %then %do;
      %let _numMeanVars_=0;
   %end; %else %do;
      %let _numMeanVars_ = %eval(1 + %length(%cmpres(&meanVarList))
                     - %length(%sysfunc(compress(&meanVarList)));
   %end;


   proc sort data=&inMeanBySliceFile;
     by infile stat fieldName fieldValue ;


  data _null_;set &inMeanBySliceFile;
   file "&outCSVfile" lrecl=32767;
   x=-1;c="@";
  if _n_ = 1 then do;
    put "sep=@";
    put "Infile@Stat@fieldName@fieldValue @_freq_@" @;
    %do _iMeanVars_ = 1 %to &_numMeanVars_;
       %let _thisMeanVar_ = %scan( &meanVarList, &_iMeanVars_); 
       put "&_thisMeanVar_ @" @; 
    %end;
    put;
  end;
  put infile +x c +x stat +x c +x fieldName +x c +x fieldValue  +x c +x _freq_ +x c +x 
    %do _iMeanVars_ = 1 %to &_numMeanVars_;
       %let _thisMeanVar_ = %scan( &meanVarList, &_iMeanVars_); 
        &_thisMeanVar_  +x c +x  
    %end;
  ; 

  run;


   
   
%mend;



%macro crtSlicesReport(infile, score100Field, splitVars, meanVarList, outSlicesFile,
                 summaryVars=,ratioList=);

   %if "&summaryVars"="" %then %do;
        %let summaryVars =  num_pol actPrem manPrem loss loss200k loss300k clmcnt; 
   %end;

   %if "&ratioList"="" %then %do;
     %let ratioList = %str(
     freq_a    clmcnt actPrem      1000
     lr_a      loss   actPrem      1
     lr200_a   loss200k actPrem    1
     lr300_a   loss300k actPrem    1
     freq_m    clmcnt   manPrem    1000
     lr_m      loss     manPrem    1
     lr200_m   loss200k manPrem    1
     lr300_m   loss300k manPrem    1
     );
   %end;


   %if "&meanVarList" = "" %then %do;
      %let _numMeanVars_=0;
   %end; %else %do;
      %let _numMeanVars_ = %eval(1 + %length(%cmpres(&meanVarList))
                     - %length(%sysfunc(compress(&meanVarList)));
   %end;


   %if "&splitVars" = "" %then %do;
      %let _numSplitVars_=0;
   %end; %else %do;
      %let _numSplitVars_ = %eval(1 + %length(%cmpres(&splitVars))
                     - %length(%sysfunc(compress(&splitVars)));
   %end;


   proc sort data=&infile out=_temp_;
   by &splitVars &score100Field;

   proc summary data=_temp_ nway missing;
   by &splitVars &score100Field;
   var &summaryVars &meanVarList;
   output out=_temp_ sum=;

   proc summary data=_temp_ ;
   var &summaryVars &meanVarList;
   output out=_tempTotal_ sum=;

   data _tempTotal_;set _tempTotal_;
   slicePerc="             ";sortOrder=.;
   slicePerc='Total   '; sortOrder=10; 
   


   data _temp_;set _temp_ ;
   drop slice;slice = &score100Field;
   slicePerc="             ";sortOrder=.;

   if      slice < 11 then slicePerc="00%-10%";
   else if slice < 21 then slicePerc="10%-20%";
   else if slice < 31 then slicePerc="20%-30%";
   else if slice < 41 then slicePerc="30%-40%";
   else if slice < 51 then slicePerc="40%-50%";
   else if slice < 61 then slicePerc="50%-60%";
   else if slice < 71 then slicePerc="60%-70%";
   else if slice < 81 then slicePerc="70%-80%";
   else if slice < 91 then slicePerc="80%-90%";
   else slicePerc="90%-100%";
   sortOrder=2; output;

   if      slice < 26 then slicePerc="00%-25%";
   else if slice < 51 then slicePerc="25%-50%";
   else if slice < 76 then slicePerc="50%-75%";
   else slicePerc="75%-100%";
   sortOrder=1; output;

   if slice >90 and slice < 96 then do;
      slicePerc="90%-95%"; sortOrder=3; output;
   end;
   if slice >95 then do;
      slicePerc="95%-100%";sortOrder=3; output;
   end;
   if slice = 96 then do;
      slicePerc="95%-96%";sortOrder=4; output;
   end;
   if slice = 97 then do;
      slicePerc="96%-97%";sortOrder=4; output;
   end;
   if slice = 98 then do;
      slicePerc="97%-98%";sortOrder=4; output;
   end;
   if slice = 99 then do;
      slicePerc="98%-99%";sortOrder=4; output;
   end;
   if slice = 100 then do;
      slicePerc="99%-100%";sortOrder=4; output;
   end;

   

   %do _iSplitVars_ = 1 %to &_numSplitVars_;
     %let _thisSplitVar_ = %scan( &splitVars, &_iSplitVars_); 
     proc means data=_temp_ noprint nway missing; class &_thisSplitVar_ sortOrder slicePerc;
     var &summaryVars &meanVarList;
     output out=_ThisTemp_ sum=;

     data _ThisTempSubTotal_;set _temp_;
     if sortOrder = 1; 
     proc means data=_ThisTempSubTotal_ noprint nway missing; class &_thisSplitVar_ ;
     var &summaryVars &meanVarList;
     output out=_ThisTempSubTotal_ sum=;

    data _ThisTempSubTotal_;set _ThisTempSubTotal_  nobs=reccnt;
    slicePerc="             ";sortOrder=.;
    sortOrder=9; slicePerc="sub Total";
    if recCnt = 1 then delete;

    
     data _thisTemp_;set _tempTotal_   _thisTemp_  _ThisTempSubTotal_ ;
     if upcase(compress("&_thisSplitVar_")) = "WHOLEBOOK" then do;
     end; else do;
         if sortOrder in (3,4) then delete;
     end;
     run;
   
     proc sort data=_ThisTemp_;by descending sortOrder slicePerc &_thisSplitVar_  ;


   ***Separate ratioList to 4 separate lists *****;
    %let _ratioVars_=;
    %let _relativityVars_=;
    %let _numeratorVars_=;
    %let _denomVars_=; 
    %let _scaleVars_=; 

    %let _numOfVars_ = %eval(1 + %length(%cmpres(&ratioList)) -
                             %length(%sysfunc(compress(&ratioList)));
    %do _iLift_ = 1 %to &_numOfVars_ %by 4;
          %let _thisVar_ = %scan( &ratioList, &_iLift_);
          %let _ratioVars_= &_ratioVars_ &_thisVar_;  
          %let _relativityVars_=&_relativityVars_ &_thisVar_._REL ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+1));
          %let _numeratorVars_= &_numeratorVars_  &_thisVar_; 

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+2));
          %let _denomVars_= &_denomVars_ &_thisVar_ ;  

          %let _thisVar_ = %scan( &ratioList, %eval(&_iLift_+3));
          %let _scaleVars_= &_scaleVars_ &_thisVar_ ;      
    
    %end;


    ***Create list of 'Percentage' and 'Total' variable names ****;
    %let _PERCvars_=;
    %let _TOTvars_=;
    %let _numOfVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
    %do _iLift_ = 1 %to &_numOfVars_;
          %let _thisVar_ = %scan( &summaryVars, &_iLift_);
          %let _PERCvars_=&_PERCvars_ &_thisVar_._PERC ;  
    %end;

     data _ThisTemp_;
     set _ThisTemp_;

    %let _numOfSummaryVars_ = %eval(1 + %length(%cmpres(&summaryVars)) -
                             %length(%sysfunc(compress(&summaryVars)));
   ***Calculate percentages of totals ***;
    %do _iRatio_ = 1 %to &_numOfSummaryVars_;
          %let _thisSummaryVar_ = %scan( &summaryVars, &_iRatio_); 
          &_thisSummaryVar_._PERC =0;
        if sortOrder = 10 then do;
            t_&_thisSummaryVar_ = &&_thisSummaryVar_ ; retain t_&_thisSummaryVar_ ; drop t_&_thisSummaryVar_ ;
        end; 
        if not missing(t_&_thisSummaryVar_.) then do;
           &_thisSummaryVar_._PERC   =  &_thisSummaryVar_.   / t_&_thisSummaryVar_. ;
        end;
    %end;

    %let _numOfRatioVars_ = %eval(1 + %length(%cmpres(&_denomVars_)) -
                             %length(%sysfunc(compress(&_denomVars_)));

   ***Calculate ratios***;
    %do _iRatio_ = 1 %to &_numOfRatioVars_;
          %let _thisRatioVar_ = %scan( &_ratioVars_, &_iRatio_); 
          %let _thisNumeratorVar_ = %scan( &_numeratorVars_, &_iRatio_); 
          %let _thisDenomVar_ = %scan( &_denomVars_, &_iRatio_); 
          %let _thisScaleVar_ = %scan( &_scaleVars_, &_iRatio_); 

         &_thisRatioVar_ = 0;&_thisRatioVar_._REL=0;
         if &_thisDenomVar_ ne 0 then do;
            &_thisRatioVar_ = &_thisNumeratorVar_  * &_thisScaleVar_ / &_thisDenomVar_ ; 
         end;
        if sortOrder = 10 then do;
            t_&_thisRatioVar_ = &&_thisRatioVar_ ; retain t_&_thisRatioVar_ ; drop t_&_thisRatioVar_ ;
        end; 
        if not missing(t_&_thisRatioVar_.) then do;
            &_thisRatioVar_._REL   =  &_thisRatioVar_.   / t_&_thisRatioVar_. - 1;
        end;

    %end;

   format &summaryVars comma14.0 &_ratioVars_ comma7.3 &_relativityVars_ &_PERCvars_  percent6.1;

    %do _iMeanVars_ = 1 %to &_numMeanVars_;
       %let _thisMeanVar_ = %scan( &meanVarList, &_iMeanVars_); 
       avg_&_thisMeanVar_ = &_thisMeanVar_ / num_pol;
    %end;

     format infile varName varValue score100Field $50.;
     varName = "&_thisSplitVar_";
     varValue = &_thisSplitVar_;
     infile = "&infile";
     score100Field = "&score100Field";
     drop &_thisSplitVar_;
     proc append data=_thisTemp_ base=_allSplits_;
  %end;

  proc append force data=_allSplits_ base=&outSlicesFile; 
  
  proc sql;drop table _allSplits_;

%mend;


%macro prtSlicesReport(inSlicesFile, meanVarList, outCSVfile,varsInReport);
   %if "&meanVarList" = "" %then %do;
      %let _numMeanVars_=0;
   %end; %else %do;
      %let _numMeanVars_ = %eval(1 + %length(%cmpres(&meanVarList))
                     - %length(%sysfunc(compress(&meanVarList)));
   %end;

   %if "&varsInReport" = "" %then %do;
       %let varsInReport = %str(
      num_pol num_pol_PERC actPrem manPrem clmcnt loss loss200k loss300k
      freq_a lr_a lr200_a LR300_A 
      freq_m lr_m lr200_m lr300_m
      freq_a_rel lr_a_rel lr200_a_rel lr300_a_rel
      freq_m_rel lr_m_rel lr200_m_rel lr300_m_rel 
       );
   %end;
   %let _numVarsInReport_ = %eval(1 + %length(%cmpres(&varsInReport))
                     - %length(%sysfunc(compress(&varsInReport)));



  proc sort data=&inSlicesFile;
  by infile score100Field varName varValue sortOrder slicePerc;

  data _null_;set &inSlicesFile ;
   file "&outCSVfile" lrecl=32767;
   x=-1;c="@";
  if _n_ = 1 then do;
    put "sep=@";
    put "Infile@score100Field@Split@Split Value@Slice Type@Slice@" @;
    %do _iVars_ = 1 %to &_numVarsInReport_;
       %let _thisVar_ = %scan( &varsInReport, &_iVars_); 
       put "&_thisVar_ @" @; 
    %end;
    %do _iMeanVars_ = 1 %to &_numMeanVars_;
       %let _thisMeanVar_ = %scan( &meanVarList, &_iMeanVars_); 
       put "Average &_thisMeanVar_ @" @; 
    %end;
    put;
  end;
  put infile +x c +x score100Field +x c +x varName +x c +x varValue +x c +x sortOrder +x c +c slicePerc +x c +x 
    %do _iVars_ = 1 %to &_numVarsInReport_;
       %let _thisVar_ = %scan( &varsInReport, &_iVars_); 
       &_thisVar_ +x c +x  
    %end;
    %do _iMeanVars_ = 1 %to &_numMeanVars_;
       %let _thisMeanVar_ = %scan( &meanVarList, &_iMeanVars_); 
        avg_&_thisMeanVar_  +x c +x  
    %end;
  ; 

  run;
%mend;



%macro calcBetaMu(cappedFile,parmFile,outBetaMuFile);
    data _temp_;set &cappedFile;run;
   

    data _parmFile_;set &parmFile;
    if upcase(variable)= "INTERCEPT" then delete;
    format varname $32.;
    varName = compress(variable);drop variable;
    

    data _null_;set _parmFile_ end=lastobs;
    file  "&workpath./_sasCode_.sas"  lrecl=32767;
    x = -1;
    if _n_ = 1 then do;
      put "proc summary data=_temp_; ";
      put " var ";
    end;
    put varName;
    if lastobs then put ";";
    
    data _null_;set _parmFile_ end=lastobs;
    file  "&workpath./_sasCode_.sas" mod lrecl=32767;
    x = -1;
    if _n_ = 1 then do;
       put " output out=_parmTemp_ Mean=";
    end;
    put "mu_" varName; 

    data _null_;set _parmFile_ end=lastobs;
    file  "&workpath./_sasCode_.sas" mod lrecl=32767;
    x = -1;
    if _n_ = 1 then do;
       put " STD=";
    end;
    put "std_" varName; 
    if lastobs then put ";";

    data _null_;set _parmFile_ end=lastobs;
    file  "&workpath./_sasCode_.sas" mod lrecl=32767;
    x = -1;
    if _n_ = 1 then do;
      put "data  _parmtemp_ (keep=varName  varMean sigma);";
      put " set _parmtemp_ end=lastobs;";
      put "format varName  $32.;";
    end;

    put "varName  = '" varName  +x "';";
    put "varMean = mu_" varName  ";";
    put "sigma = std_" varName  ";";
    put "output;";


    run;
    %include "&workpath./_sasCode_.sas" ;run;

    data _parmtemp_;set _parmtemp_;
    varName  = compress(upcase(varName ));
    proc sort data=_parmtemp_;by varName ;

   data _parmFile_;set _parmFile_; 
   varName  = compress(upcase(varName ));
   proc sort data=_parmFile_;by varName ;
   data &outBetaMuFile;merge _parmtemp_ _parmFile_ (in=in_parm);
     by varName ;
    if in_parm;
    ********estimate = abs(estimate);
   run;
%mend;





%macro genReasonCode(inBetaMuFile,outSAScode);
   %if "&outSAScode" = "" %then %do; %goto endOfMacro; %end;
      
   data _null_;set &inBetaMuFile  end=lastobs;
   file  "&outSAScode"  lrecl=32767;
   x=-1;
   if _n_ = 1 then do;
     put "/*This code was created by genReasonCode macro    */";
     put;put;
   end;
   put 'x' _n_ '=' varName '; mu' _n_  '=' varMean '; beta' _n_ '=' estimate '; sig' _n_ '=' sigma '; fldName' _n_ '="' varName +x '";' ; 
 
   if not lastobs then return;

   put;put;
   put '%global numPredVars;';
   put '%let numPredVars = ' _n_  ';';
   put;put;
   put 'array rz(&numPredVars) rz1 - rz&numPredVars;';
   put 'array x(&numPredVars) x1 - x&numPredVars;';
   put 'array beta(&numPredVars) beta1 - beta&numPredVars;';
   put 'array mu(&numPredVars) mu1 - mu&numPredVars;';
   put 'do i = 1 to &numPredVars;';
   put '   rz(i) = beta(i) * (x(i) - mu(i));';
   put 'end;';

   run;
    
%endOfMacro:
%mend;




%macro genConfCode(inBetaMuFile,outSAScode);
   %if "&outSAScode" = "" %then %do;
   %end; %else %do;
      data _null_;set &inBetaMuFile end=lastobs;
      file  "&outSAScode"  lrecl=32767;
      x=-1;
      estimate = abs(estimate);
      if _n_ = 1 then do;
        put "/*This code was created by genConfCode macro   */";
        put;put;
         put "numerator = 0";
      end;
      put " + " estimate "* " varMean "* " varName  +x "_m" ;
      if lastobs then put ";";

      data _null_;set &inBetaMuFile end=lastobs;
      file  "&outSAScode"  mod lrecl=32767;
      x=-1;
      estimate = abs(estimate);
      if _n_ = 1 then do;
         put "denominator = 0";
      end;
      put " + " estimate "* " varMean  ;
      if not lastobs then return;
      put ";";
      put "confidence = round(1 - (numerator/denominator),.001);";
      put "format confRange $20.;";
      put "if confidence ge .99 then confRange= '3: 99+';";
      put "if confidence ge .80 and confidence lt .99 then confRange= '2: 80 - 98';";
      put "if confidence lt .80 then confRange= '1: 79 and below';";
      put "if missing(confidence) then confRange= '4: missing';";

      run;
   %end;

    

%mend;




%macro crtCentilesCode(infile, rawScoreField,weightField,outSAScode);
 %let iCentile = 1;
 proc univariate noprint data= &infile; var &rawScoreField;
   freq &weightField;
   output out=b
   pctlpts = 1 to 99 by 1
   pctlpre = S
   pctlname =
      %do %until ( &iCentile  > 99) ;  s&iCentile 
      %let iCentile  = %eval( &iCentile  + 1 );
      %end;;


  data _null_; set b;
   semi = ';' ; x = -1; perc = '%';
   file "&outSAScode" lrecl=32767;
          put "/*This code was created by crtCentilesCode macro    */";
          put  "    score100 = 0"  semi ;
          put  "         if &rawScoreField < "  Ss1  " then score100 = 1"  semi ;
      %let iCentile  = 2;
      %do %until ( &iCentile  > 99) ;
          put  "    else if &rawScoreField < "  Ss&iCentile  " then score100 = &iCentile "  semi ;
          %let iCentile  = %eval( &iCentile  + 1 );
      %end;
          put  "    else                              score100 = 100"  semi ;
      put "score10=1;";
      put "if score100 > 10 then score10=2;";
      put "if score100 > 20 then score10=3;";
      put "if score100 > 30 then score10=4;";
      put "if score100 > 40 then score10=5;";
      put "if score100 > 50 then score10=6;";
      put "if score100 > 60 then score10=7;";
      put "if score100 > 70 then score10=8;";
      put "if score100 > 80 then score10=9;";
      put "if score100 > 90 then score10=10;";

      put "score4=1;";
      put "if score100 > 25 then score4=2;";
      put "if score100 > 50 then score4=3;";
      put "if score100 > 75 then score4=4;";

      put "score11=score10;";
      put "if score100>95 then score11=11;";

run;
/*   return;*/
%mend;





%macro mergeWithNextKey(infile, outFile, key, nextKey, varList);

  %let _numVars_ = %eval(1 + %length(%cmpres(&varList))
                     - %length(%sysfunc(compress(&varList)));



   proc sql noprint noerrorstop;
   create index &key on &infile ( &key );

   proc sql noprint noerrorstop;
   create index &nextkey on &infile ( &nextKey );


   proc sql noprint ; create table &outFile as
   select A.&key  as &key  
   %do _iMWNK_ = 1 %to &_numVars_;
      %let _thisVar_ = %scan( &varList, &_iMWNK_); 
        , A.&_thisVar_ as &_thisVar_
        , B.&_thisVar_ as &_thisVar_._next
   %end;
     from &inFile A, &inFile B
    where A.&nextKey = B.&key;
  quit ;

   proc sql noprint noerrorstop; drop index &key on &infile  ;
   proc sql noprint noerrorstop; drop index &nextKey on &infile  ;



%mend;


%macro crtDisruptionReport(inFile,outCSVfile,disruptionVarList,varsInReport);

  %let _numVarsInReport_ = %eval(1 + %length(%cmpres(&varsInReport))
                     - %length(%sysfunc(compress(&varsInReport)));
  %let _numDisruptionVars_ = %eval(1 + %length(%cmpres(&disruptionVarList))
                     - %length(%sysfunc(compress(&disruptionVarList)));


   %let _fileName_ = &infile; run;
   %if "&varsInReport" = "" %then %do;
     data _tempOut_;set &_fileName_;
     cnt=1; run;
      %let _fileName_ = _tempOut_;
      %let varsInReport=cnt;
   %end;
   run;

   proc summary data=&_fileName_ nway missing;
   class &disruptionVarList; 
   var &varsInReport;
   output out=_tempOut_ sum= N=polCnt; 

   proc summary data=_tempOut_ nway missing;
   var polCnt &varsInReport;
   output out=_tempTotal_ sum=polCnt_tot
    %do _iCDR_ = 1 %to &_numVarsInReport_;
      %let _thisVar_ = %scan( &varsInReport, &_iCDR_); 
       &_thisVar_._tot  
   %end;
   ; 

   data _tempOut_;set _tempOut_;
   if _n_ = 1 then set _tempTotal_;
   retain polCnt_tot;drop polCnt_tot;
   polCnt_perc = round(polCnt * 100000/ polCnt_tot,9);
   %do _iCDR_ = 1 %to &_numVarsInReport_;
      %let _thisVar_ = %scan( &varsInReport, &_iCDR_); 
       retain &_thisVar_._tot ; drop &_thisVar_._tot ; 
       &_thisVar_._perc = round(&_thisVar_ * 100000/ &_thisVar_._tot, 9);
   %end;
   


   data _null_;set _tempOut_ ;
   c="@";x=-1;
   file "&outCSVfile" lrecl=32767;
   if _n_ = 1 then do;
        put "sep=@";
        put "infile" c +x
        %do _iCDR_ = 1 %to &_numDisruptionVars_;
            %let _thisVar_ = %scan( &disruptionVarList, &_iCDR_); 
            "&_thisVar_" c +x 
        %end;
        "polCnt" c +x "polCnt_perc" c +x 
        %do _iCDR_ = 1 %to &_numVarsInReport_;
           %let _thisVar_ = %scan( &varsInReport, &_iCDR_); 
           "&_thisVar_" c +x "&_thisVar_._perc" c +x 
       %end;
       ;
   end;

   put "&infile"  c +x 
   %do _iCDR_ = 1 %to &_numDisruptionVars_;
      %let _thisVar_ = %scan( &disruptionVarList, &_iCDR_); 
      &_thisVar_ +x c +x 
  %end;
   polCnt +x c +x polCnt_perc +x c +x 
   %do _iCDR_ = 1 %to &_numVarsInReport_;
      %let _thisVar_ = %scan( &varsInReport, &_iCDR_); 
      &_thisVar_ +x c +x  &_thisVar_._perc +x c +x 
  %end;
  ;


%mend;




/*******************************************************************************************************
This macro produces the SAS code to rescale one variable to the scale as another variable's mean and STD.
This macro will be used when you want to rescale new business raw scores to Renewl levels.
******************************************************************************************************/
%macro crtRescaleCode(srcVar, rescaleToVar, srcVarFile,rescaleToVarFile, outSAScode);
   proc summary data=&srcVarFile; 
    var &srcVar;
   output out=_SRC_ 
      mean(&srcVar) = src_mean std(&srcVar) = src_std;

    proc summary data=&rescaleToVarFile; 
    var &rescaleToVar;
   output out=_TGT_ 
      mean(&rescaleToVar) = tgt_mean std(&rescaleToVar) = tgt_std;

   data _null_;set _SRC_; set _TGT_;
   file "&outSAScode";
   put "&srcVar = (&srcVar - " src_mean ") / " src_std ";" ; 
   put "&srcVar = (&srcVar * " tgt_std ") + " tgt_mean ";" ; 
%mend;


/*******************************************************************************************************
The macro will:
•	Merge parameter estimate datasets from various models
•	Calculate average absolute t-value 
•	Sort the merged dataset by ascending order of absolute t-value
•	And finally print the dataset into a CSV file.
******************************************************************************************************/
%macro printModelStats(inStatFileList=,outCSVfile=);
   
    %compare_pe(dsns=&inStatFileList) ; 
    %if "&outCSVfile" ne "" %then %do;
        proc export data=allPE outfile="&outCSVfile" replace;
    %end;
    
%mend;


/********************************************************************
Macro: model_results
 
This macro outputs regression summary information to an excel spreadsheet
for easy review.  There are two tabs output for each call of the macro,
winded and unwinded parameters.  The macro can also calculate score
averages and/or predictive variable means for every level of a specified
variable.  The macro is intended to be called after initializing an
excel spreadsheet using the %xls macro.
 
For adding regression summaries to the spreadsheet, see the %reg_summary macro.

Last updated May2014.  Code added to make this macro compatible with do_genmod as well as do_reg
 
Macro Parameters: 
   &modelNo: List of model numbers for summarization.  Each modelNo listed will appear as columns in the spreadsheet.
    List of Numeric items.
   &modLbl: List of definitions supplied for &modelNo param.  Labels will output in headers of columns.
    Will default to &modelNo.
   &sheetLabel: Label for each tab.  Useful when macro called multiple times.
   &getUni: Use the fastuni2 macro to summarize predicted scores for each level of variables of interest.
    Variable list specified with param &uniVars. Y/N.  Default=N.
   &inFile: Dataset on which to generate summarization for getUni=Y.  SAS Dataset name.
   &varMeans: Calculate variable means for variables of interest. Variable list specified with param
    &meansVars.  Y/N. Default=N.
   &uniVars: List of variables on which to summarize for with getUni.  Will default to all model vars.
    List of variables from &inFile.
   &meansVars: List of variables on which to generate averages.  Will default to all model vars. List of variables
    from &inFile.
   &ratioList: List of ratios to output in summary if getUni=Y.  Should be in 3 column format: output, numerator, denominator.
   &princompfile: Name of the SAS file containing Principal component specifications.
   &pctvars: Variables to summarize as percentage of total if &getUni=Y
   &drop: List of variables to exclude from variable means calculation when varmeans=Y.
    Defaults to %str(actPrem manprem clmcnt ultloss200k ultloss).
   &weight: Weight variable from &inFile for generation of calibrated score in &getUni summary.  Default=manprem.
   &format: List of formats for getUni summary.  Optional.
 
Input SAS files:
    mreg.regout_&modelno (one for each item in &modelno)
    &inFile (optional)
 
Output XML Spreadsheet files:
    Will append to open ODS output XML Spreadsheet.
 
Output SAS files:
    None.
 
Output SAS code:
    None.
 
 
 
Sample Usage:
 
    Note that the macro should be called after calling the %xls macro to initiate the spreadsheet.
    The following call will generate a spreadsheet with 10 tabs (2 for each modelno).  One tab
    for PE and one tab for UnwindedPE.
 
    %xls(xlsout=myxml.xls);
    %model_results2 (modelNo =
        %str(610101711
        610101712
        610101713
        610101714
        610101715),
        modLbl= %str(1 2 3 4 5),
        sheetLabel= LR99_adj_mult,
        );
    
    This version will generate an extra 3 tabs (13 total), one for each variable in uniVars.
    Each tab will have a summary of the mreg.modeling_data dataset for each variable along
    with variable means for the 4 variables listed in meansVars.
    
    %xls(xlsout=myxml.xls);
    %model_results2 (modelNo =
        %str(610101711
        610101712
        610101713
        610101714
        610101715),
        modLbl= %str(1 2 3 4 5),
        sheetLabel= LR99_adj_mult,
        getUni=Y,
        var_means=Y,
        princompfile=mreg.princomp,
        uniVars=%str(cscorepl state ageofbus),
        meansVars=%str(pol_age freq_prev1 freq_prev2 lr_cprev3),
        infile=mreg.modeling_data,
        ratioList=%str(freq_a        clmCnt1000        actPrem
                       lr_a          ultloss           actPrem
                       lr200_a       ultloss200k       actPrem 
                       freq_m        clmCnt1000        adjprem           
                       lr_m          ultloss           adjprem            
                       lr200_m       ultloss200k       adjprem)
        );
 
 
 

 **********************************************************************************/
 
%macro model_results (modelNo=&modelNo,
                      modLbl=,
                      sheetLabel=summary,
                      getUni=N, 
                      inFile=,
                      var_means=N,
                      uniVars=,
                      meansVars=,
                      princompFile=mreg.prinComp, 
                      pctvars =num_pol manprem,
                      ratiolist=, 
                      drop=actPrem
                           manprem
                           clmCnt
                           ultloss200k
                           ultloss,
                      weight=manprem,
                      FORMAT=
                      );
    %local modelNo;
   
    %let _count_modelNo_=0;
    %do %while(%qscan(&modelNo,&_count_modelno_+1,%str( )) ne %str());
        %let _count_modelno_ = %eval(&_count_modelno_+1);
        %end;
    
    %if "&modLbl"="" %then %do;
        %let modLbl=&modelno;
        %end;


    *Compare PE for print out;
    %compare_pe(dsns= %do _i_ = 1 %to &_count_modelno_;
    %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
    %cmpres(mreg.model&thismodelNo._pe)
        %end;,noprint=Y
        %if "&regtype"="stepwise" %then %do;
           , statlist=tvalue
        %end;
        %if "&link" ne "" %then %do;
            ,statlist=estimate chisq
            %end;
        %if "&regtype"="" and "&link" = "" %then %do;
           , statlist=estimate tvalue
        %end;

        );

    data _allpe_rename_;
        set allpe;
            %if "&regtype"="stepwise" %then %do;
                modelsPicked=0;
                if missing(variable) then delete;
                %end;
        %do _i_ = 1 %to &_count_modelno_;
            %let thismodelNo = %scan(&modelNo,&_i_,%str( ));
            %let thismodLbl = %scan(&modLbl,&_i_,%str( ));
            %if "&regtype"="" %then %do;
            Estimate_&thismodLbl._pe=Estimate_model&thismodelno._pe;
            %end;
            %if "&link"="" %then %do;
            tValue_&thismodLbl._pe=tValue_model&thismodelno._pe;
            %end;
            %else %do;                
            ChiSq_&thismodLbl._pe=ChiSq_model&thismodelno._pe;
            %end;
            drop
            %if "&regtype"="" %then %do;
                Estimate_model&thismodelno._pe
            %end;
            %if "&link"="" %then %do;
                tValue_model&thismodelno._pe
            %end;
            %else %do;
                ChiSq_model&thismodelno._pe
            %end;
                ;
            %if "&regtype"="stepwise" %then %do;
                if tValue_&thismodLbl._pe ~=. then modelsPicked=modelsPicked+1;
                %end;
            %end;
    run;

    %if "&regtype"="stepwise" %then %do;
       proc sort data=_allpe_rename_;
            by abstavg_tvalue;
        run;
        %end;

    *Unwinded params for comparison - only run for non-stepwise to avoid Princomp issue;
    %if "&regtype"="" %then %do;
        %compare_pe_uw(dsns= %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        mreg.regOut_&thismodelNo
            %end;,
            princompFile= &princompFile,noprint=Y
            );

         data _allpe_rename_uw_;
             set allpe;
             %do _i_ = 1 %to &_count_modelno_;
                 %let thismodelNo = %scan(&modelNo,&_i_,%str( ));
                 %let thismodLbl = %scan(&modLbl,&_i_,%str( ));
                 UnWind_&thismodLbl.=Estimate_regOut_&thismodelno.;
                 drop Estimate_regOut_&thismodelno.;
                 %end;
          run;
          %end;
    
    proc sql noprint;
        select upcase(variable)
        into: _varlist_
        separated by ' '      
        from %cmpres(allpe)
        where(upcase(variable) not in ("INTERCEPT","LOG_MANPREM")); 
        quit;
    %if "&uniVars"="" %then %do;
        %let uniVars=&_varlist_;
        %end;
    %if "&meansVars"="" %then %do;
        %let meansVars=&_varlist_;
        %end;

    %if "&regtype"="" %then %do;    
    %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        %unwindParms(regParmFile=%cmpres(mreg.regOut_&thismodelNo.),
            princompFile= &princompFile, 
            outParmFile=outParmFile,
            outSAScode=&workpath./score&thismodelNo..sas,
            yhatFldName=score_&thismodelNo.);
        %end;
     %end;
    *Generate XLS output for review;
    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="&sheetLabel PE");  
    proc print data=_allpe_rename_ noobs width=min;run;

    %if "&regtype"="" %then %do;        
    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="&sheetLabel Unwinded PE");
    proc print data=_allpe_rename_uw_ noobs width=min;run;    
    %end;



    %if %upcase(&getUNI)=Y %then %do; 
        %let obs=max;
      *Generate univariates;
        data _scored_ / view=_scored_;
            set &inFile; 
            
            %do _i_ = 1 %to &_count_modelno_;
                %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
                %include "&workpath./score&thismodelNo..sas";
                %cmpres(score_&thismodelNo.=score_&thismodelNo.*&weight);
                %end;
            %do _i_=1 %to %eval(1 + %length(%cmpres(&ratioList)) -
                %length(%sysfunc(compress(&ratioList)));
                %if %sysfunc(mod(&_i_,3))=1 %then %do;
                    %let thisvar=%scan(&ratioList,&_i_,%str( ));
                    &thisvar=%scan(&ratioList,%eval(&_i_+1),%str( ))
                        / %scan(&ratioList,%eval(&_i_+2),%str( ));
                    %end;
                %end;
            keep %do _i_ = 1 %to &_count_modelno_;
            %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
            %cmpres(score_&thismodelNo.)          
                %end;
            &uniVars &ratioList &meansVars &weight &pctvars;  
        run;
        

        %fastuni2(dataset=_scored_,
            list=&uniVars,
            ratioList=&ratioList
            %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        avg_score_&thismodelNo score_&thismodelNo &weight
            %end;,    
            pctvars =&pctvars,          
            drop=&drop
            %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        %cmpres(score_&thismodelNo.)
            %end;,
            out=_uni_,
            varsPerItr=100,
            %if %upcase(&var_means)=Y %then %do; 
            statsWgt=&weight,
                stats=mean,
                statVars=&meansVars,
                %end;   
            obs=&obs);
            %print_summary (summary_file=_uni_,
                list=&uniVars,
                format=&format,
                upVarName=Split1,
                varValue=Level1,
                sheet_name=Y);
            %end;                
    %mend model_results;
 

/********************************************************************
Macro: reg_summary
 
This macro outputs regression summary information to an excel spreadsheet
for easy review. Note that this macro MUST be called after the
%getSummaryinExcel macro.  The macro is intended to be called after initializing an
excel spreadsheet using the %xls macro.  Also, this macro is well suited
to run before the model_results macro, as it will summarize the lift
information on the first tab.
 

Macro Parameters: 
   &firstmodelNo: Minimum model number to print into summary
   &lastmodelNo: Maximum model number to print into summary
   &referenceVarList: Variables to print out to Excel (default=macro variable &refVarList)
    NOTE: variables listed here should be the same as for the %getSummaryInExcel macro
 
    
Input SAS files:
    _tempall_: File generated by the %getSummaryInExcel macro.
 
Output XML Spreadsheet files:
    Will append to open ODS output XML Spreadsheet.
 
Output SAS files:
    None.
 
Output SAS code:
    None.
 
 
 
Sample Usage:
 
    Note that the macro should be called after calling the %xls macro to initiate the spreadsheet.
    The following call will generate a spreadsheet with 10 tabs (2 for each modelno).  One tab
    for PE and one tab for UnwindedPE.
 
    %getSummaryInExcel(inSummaryFile=mreg.SummaryFile,
                   firstModelNo=0,
                   lastModelNo=999999999,
                   referenceVarList=&refVarList,
                   varsInSummary=&varsInSummary, 
                   outCSVFile=reg_summary.csv);
    %xls(xlsout=myxml.xls);
    %reg_summary(firstmodelno=0,lastmodelno=999999999,referenceVarList=&refVarList);
    
 
 **********************************************************************************/
 
    
%macro reg_summary(firstmodelno=0,lastmodelno=999999999,referenceVarList=&refVarList);
    %if "&referenceVarList"="" %then %do;
        %let referenceVarList=%str(modelno infile fieldName varLstNm target split);
        %end;
 
    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="reg_summary");  
    proc print data=_tempAll_ noobs;
        where modelno >= &firstmodelno and modelno <= &lastmodelno;
        var &referenceVarList
            s00_25 s25_50 s50_75 s75_100 s00_10 s90_100 s95_100; run;
%mend reg_summary; 
 





%macro _CrtLevelBasedVars_(inFile,outFile);

   ****Get number of level fields in the file *****;
   %let _NUMlevels_=0;
   proc contents data=&inFile
        noprint out=_allVars_;  run;
   data _allvars_;set _allvars_;
   if substr(upcase(name),1,5) = "LEVEL";

    data _null_; set _allvars_ nobs=reccnt;
    if _n_ > 1 then stop;
    call symput('_NUMlevels_', reccnt ) ;
      run;
   run;  
   %let _NUMlevels_=&_NUMlevels_;

  %if "&_numLossVars_" = "0" %then %do; %goto endOfMacro; %end;

  data &outFile;set &infile;
  variable = trim(variable) 
          %do _Icrtlevel_ = 1 %to &_NUMlevels_;
               || " " || trim(level&_Icrtlevel_)
          %end;
  ;


  run;
%endOfMacro:
%mend;


%macro compare_pe(dsns=,outcsv=,noprint=N, statList=Estimate tValue);
   
    /* noprint option added by Mike Greene 7/2007 */
   %let count=0;
   %do %while(%qscan(&dsns,&count+1,%str( )) ne %str());
      %let count = %eval(&count+1);
   %end;

   %let _numOfVars_ = %eval(1 + %length(%cmpres(&statList)) -
                             %length(%sysfunc(compress(&statList)));
   
   %do i = 1 %to &count;
      %let myDsn=%qscan(&dsns,&i,%str( ));     
      %let myDsn_suffix=%qscan(&myDsn,2,%str(.));
      %if "&myDsn_suffix" = "" %then %do;
          %let myDsn_suffix = %CMPRES(&myDsn); 
      %end;
      %_CrtLevelBasedVars_(inFile=&mydsn,outFile=&myDsn_suffix.)
      proc sort data=&myDsn_suffix.(keep=Variable &statList)
                out=&myDsn_suffix.;
         by Variable;
      run;
   %end;   
     
   data allpe;
      
      length Variable $100.; 
      merge %do i = 1 %to &count;
                %let myDsn=%qscan(&dsns,&i,%str( ));
                %let myDsn_suffix=%CMPRES(%qscan(&myDsn,2,%str(.)));
                %if "&myDsn_suffix" = "" %then %do;
                    %let myDsn_suffix = %CMPRES(&myDsn); 
                %end;
                &myDsn_suffix. (rename=(
                       %do _J_ = 1 %to &_numOfVars_;
                             %let _thisVar_ = %scan( &statList , &_J_); 
                             &_thisVar_ = &_thisVar_._&myDsn_suffix.
                       %end;
                               ))  
             %end; 
             ;                                  
      by Variable;    

     %do _J_ = 1 %to &_numOfVars_;
         %let _thisVar_ = %scan( &statList , &_J_);                                           
         abstavg_&_thisVar_ =mean( %do i = 1 %to &count;
                        %let myDsn=%qscan(&dsns,&i,%str( ));
                        %let myDsn_suffix=%CMPRES(%qscan(&myDsn,2,%str(.)));
                        %if "&myDsn_suffix" = "" %then %do;
                            %let myDsn_suffix = %CMPRES(&myDsn); 
                        %end;
                        abs(&_thisVar_._&myDsn_suffix.),  
                    %end; 
                   .);
      %end;
   run;                                                      
                                                          
    

   %if %upcase("&noprint")="N" %then %do;
        proc print data=allpe noobs;
        run;
   %end;

    %if "&outCSV" ne "" %then %do;
        proc export data=allPE outfile="&outCSV" replace;
    %end;
       
%mend compare_pe;

%macro compare_pe_uw(dsns=,outCSV=,
                     princompFile=mod_h.prinComp,noprint=N);
   
    /* noprint option added by Mike Greene 7/2007 */
   %let count=0;
   %do %while(%qscan(&dsns,&count+1,%str( )) ne %str());
      %let count = %eval(&count+1);
   %end;
   
   %do i = 1 %to &count;
      %let myDsn=%qscan(&dsns,&i,%str( ));      
      %let myDsn_suffix=%qscan(&myDsn,2,%str(.));
      %if "&myDsn_suffix" = "" %then %do;
          %let myDsn_suffix = %CMPRES(&myDsn); 
      %end;
      
      %unwindParms(regParmFile=&myDsn,             
   	               princompFile=&princompFile,    
                   outParmFile=&myDsn_suffix);
      %_CrtLevelBasedVars_(inFile=&myDsn_suffix.,outFile=&myDsn_suffix.)                   
      proc sort data=&myDsn_suffix(keep=Variable Estimate);
         by Variable;
      run;
   %end;
     
   data allpe;       
      merge %do i = 1 %to &count;
                %let myDsn=%qscan(&dsns,&i,%str( ));
                %let myDsn_suffix=%CMPRES(%qscan(&myDsn,2,%str(.)));
                %if "&myDsn_suffix" = "" %then %do;
                    %let myDsn_suffix = %CMPRES(&myDsn); 
                %end;
                &myDsn_suffix. (rename=(Estimate = Estimate_&myDsn_suffix.))  
             %end; 
             ;                                  
      by Variable;                                              
   run; 
   
   data allpe;
      set allpe (where=(upcase(Variable)="INTERCEPT"))
          allpe (where=(upcase(Variable) ne "INTERCEPT"));
   run;
   %if %upcase("&noprint")="N" %then %do;       
        proc print data=allpe noobs WIDTH=MIN;
        run;
   %end;       

    %if "&outCSV" ne "" %then %do;
        proc export data=allPE outfile="&outCSV" replace;
    %end;

%mend compare_pe_uw;

/********************************************************************
Macro: do_reg_cv

Description:

Macro Parameters: 
   crossValidVar: the Cross Validation Variable that will split the modeling dataset into
                  N-folds.  This variable must be numeric variable with values of 1 to 5
                  or 1 to 10, for example.
   
   All Other Inputs: as described in do_reg macro

********************************************************************/
%macro do_reg_cv(inFile,Target,VarList,outRegOut=,Weight=,RegType=,offSetVars=,
              referenceModelParmFile=,combineReferenceModel=1,
              modelOptions=,outStatFile=statFile,
              renameReferenceInterceptAs=,crossValidVar=);

   proc sql noprint;
      select distinct &crossValidVar
      into: _crossValidVals_
      separated by ' '      
      from &inFile
      ;
   quit;
   
   %let _numCrossValid_ = %eval(1 + %length(%cmpres(&_crossValidVals_))
                   - %length(%sysfunc(compress(&_crossValidVals_)));

   %do _iCV_ = 1 %to &_numCrossValid_;
   
     %let _thisCVval_ = %scan( &_crossValidVals_, &_iCV_ );
   
     title "Cross Validation on &crossValidVar Equal to &_thisCVval_";

     data modfile&_thisCVval_ ;
        set &inFile (where=( &crossValidVar <> &_thisCVval_ ));
     run;
     
     %let outRegOut_CV=;
     %if "&outRegOut" ne "" %then
         %let outRegOut_CV=&outRegOut._&_thisCVval_;
         
              
     %let referenceModelParmFile_CV=;
     %if "&referenceModelParmFile" ne "" %then
         %let referenceModelParmFile_CV=&referenceModelParmFile._&_thisCVval_;
         
     %let outStatFile_CV=;
     %if "&outStatFile" ne "" %then
         %let outStatFile_CV=&outStatFile._&_thisCVval_;         
     
     %do_reg(inFile=modfile&_thisCVval_,Target=&target,VarList=&VarList,outRegOut=&outRegOut_CV,Weight=&Weight,RegType=&RegType,offSetVars=&offSetVars,
              referenceModelParmFile=&referenceModelParmFile_CV,combineReferenceModel=&combineReferenceModel,
              modelOptions=&modelOptions,outStatFile=&outStatFile_CV,
              renameReferenceInterceptAs=&renameReferenceInterceptAs);
   %end;
   
   title;
 
%mend do_reg_cv;

/********************************************************************
Macro: do_genmod_cv

Description:

Macro Parameters: 
   crossValidVar: the Cross Validation Variable that will split the modeling dataset into
                  N-folds.  This variable must be numeric variable with values of 1 to 5
                  or 1 to 10, for example.
   
   All Other Inputs: as described in do_genmod macro

********************************************************************/
%macro do_genmod_cv(inFile,target,VarList,outRegOut,outFile,link=,offset=,dist=,p=,power=,weight=,
                    offSetVars=,referenceModelParmFile=,combineReferenceModel=1,outStatFile=,modelOptions=,
                    REPEATED=,renameInterceptAs=Intercept,classVars=,crossValidVar=);

   proc sql noprint;
      select distinct &crossValidVar
      into: _crossValidVals_
      separated by ' '      
      from &inFile
      ;
   quit;
   
   %let _numCrossValid_ = %eval(1 + %length(%cmpres(&_crossValidVals_))
                   - %length(%sysfunc(compress(&_crossValidVals_)));

   %do _iCV_ = 1 %to &_numCrossValid_;
   
     %let _thisCVval_ = %scan( &_crossValidVals_, &_iCV_ );
   
     title "Cross Validation on &crossValidVar Equal to &_thisCVval_";

     data modfile&_thisCVval_ ;
        set &inFile (where=( &crossValidVar <> &_thisCVval_ ));
     run;
     
     %let outRegOut_CV=;
     %if "&outRegOut" ne "" %then
         %let outRegOut_CV=&outRegOut._&_thisCVval_;
         
     %let outFile_CV=;
     %if "&outFile" ne "" %then
         %let outFile_CV=&outFile._&_thisCVval_;
         
     %let referenceModelParmFile_CV=;
     %if "&referenceModelParmFile" ne "" %then
         %let referenceModelParmFile_CV=&referenceModelParmFile._&_thisCVval_;
         
     %let outStatFile_CV=;
     %if "&outStatFile" ne "" %then
         %let outStatFile_CV=&outStatFile._&_thisCVval_;         
     
     %do_genmod(inFile=modfile&_thisCVval_,target=&target,VarList=&VarList,outRegOut=&outRegOut_CV,
                outFile=&outFile_CV,link=&link,offset=&offset,dist=&dist,p=&p,power=&power,weight=&weight,
                offSetVars=&offSetVars,referenceModelParmFile=&referenceModelParmFile_CV,
                combineReferenceModel=&combineReferenceModel,outStatFile=&outStatFile_CV,
                modelOptions=&modelOptions,REPEATED=&REPEATED,renameInterceptAs=&renameInterceptAs,classVars=&classVars);
   
   %end;
   
   title;
 
%mend do_genmod_cv;



%macro wrtScoreCode_cv(unwindParmFile,outSAScode=,yhatFldName=yhat,crossValidVar=,inFile=);

   %if "&yhatFldName" = "" %then %do;
       %let yhatFldName=yhat_cv;
   %end; %else %do;
       %let yhatFldName=&yhatFldName._cv;
   %end;
   
   %if "&outSAScode" = "" %then %do;
       %goto endOfMacro;
   %end;
      
   proc sql noprint;
      select distinct &crossValidVar
      into: _crossValidVals_
      separated by ' '      
      from &inFile
      ;
   quit;
   
   %let _numCrossValid_ = %eval(1 + %length(%cmpres(&_crossValidVals_))
                   - %length(%sysfunc(compress(&_crossValidVals_)));

   %do _iCV_ = 1 %to &_numCrossValid_;
   
     %let _thisCVval_ = %scan( &_crossValidVals_, &_iCV_ );

     data _null_;set &inFile end=lastobs;
       file  "&outSAScode"  mod notitle linesize=250 pagesize=500;
       if _n_ = 1 then do;   
         put '   ';
         put "if &crossValidVar = &_thisCVval_ then do;";
         put '   ';
       end; else do;
         stop;
       end;
     run;

     %wrtScoreCode(unwindParmFile=&unwindParmFile._&_thisCVval_,outSAScode=&outSAScode,
                   yhatFldName=&yhatFldName,mod=MOD);
     
     data _null_;set &inFile end=lastobs;
       file  "&outSAScode"  mod notitle linesize=250 pagesize=500;   
       put '   ';
       put 'end;'; stop;
     run;
   %end;

   %endOfMacro:
   
%mend wrtScoreCode_cv;


/********************************************************************
Macro: do_glimmix_cv

Description:

Macro Parameters: 
   crossValidVar: the Cross Validation Variable that will split the modeling dataset into
                  N-folds.  This variable must be numeric variable with values of 1 to 5
                  or 1 to 10, for example.
   
   All Other Inputs: as described in do_glimmix macro

********************************************************************/
%macro do_glimmix_cv(inFile,target,VarList,outRegOut,outFile,link=,offset=,dist=,p=,power=,weight=,
			offSetVars=,referenceModelParmFile=,combineReferenceModel=1,outStatFile=,
			modelOptions=,renameInterceptAs=Intercept, CLASSvars=,BYVARS=,
			blup=Y,ilink=N,randEffects=,technique=QUANEW,method=,crossValidVar=);

   proc sql noprint;
      select distinct &crossValidVar
      into: _crossValidVals_
      separated by ' '      
      from &inFile
      ;
   quit;
   
   %let _numCrossValid_ = %eval(1 + %length(%cmpres(&_crossValidVals_))
                   - %length(%sysfunc(compress(&_crossValidVals_)));

   %do _iCV_ = 1 %to &_numCrossValid_;
   
     %let _thisCVval_ = %scan( &_crossValidVals_, &_iCV_ );
   
     title "Cross Validation on &crossValidVar Equal to &_thisCVval_";

     data modfile&_thisCVval_ ;
        set &inFile (where=( &crossValidVar <> &_thisCVval_ ));
     run;
     
     %let outRegOut_CV=;
     %if "&outRegOut" ne "" %then
         %let outRegOut_CV=&outRegOut._&_thisCVval_;
         
     %let outFile_CV=;
     %if "&outFile" ne "" %then
         %let outFile_CV=&outFile._&_thisCVval_;
         
     %let referenceModelParmFile_CV=;
     %if "&referenceModelParmFile" ne "" %then
         %let referenceModelParmFile_CV=&referenceModelParmFile._&_thisCVval_;
         
     %let outStatFile_CV=;
     %if "&outStatFile" ne "" %then
         %let outStatFile_CV=&outStatFile._&_thisCVval_;         
     
     %do_glimmix(inFile=modfile&_thisCVval_,target=&target,VarList=&VarList,outRegOut=&outRegOut_CV,outFile=&outFile_CV,
			link=&link,offset=&offset,dist=&dist,p=&p,power=&power,weight=&weight,
			offSetVars=&offSetVars,referenceModelParmFile=&referenceModelParmFile_CV,
			combineReferenceModel=&combineReferenceModel,outStatFile=&outStatFile_CV,
			modelOptions=&modelOptions,renameInterceptAs=&renameInterceptAs, CLASSvars=&CLASSvars,BYVARS=&BYVARS,
			blup=&blup,ilink=&ilink,randEffects=&randEffects,technique=&technique,method=&method);
   %end;
   
   title;
 
%mend do_glimmix_cv;



/**********************************************************************
 Change Log:
 
 Macro            Alterations
          
 do_genmod        1. Added additional input parameter named "crossValidVar"
                  2. At end of macro, added call to "do_genmod_cv" macro based on conditional logic
                  
 do_genmod_cv     1. Created.
 
 wrtScoreCode     1. Added 3 additional input parameters named "mod", "crossValidVar" and "inFile"
                  2. Added "mod" input parameter in the outSAScode file statement
                  3. At end of macro, added call to "wrtScoreCode_cv" macro based on conditional logic
                  
 wrtScoreCode_cv  1. Created.
 
 unwindParms      1. Added 2 additional input parameters named "crossValidVar" and "inFile"
                  2. At end of macro, added call to "wrtScoreCode_cv" macro based on conditional logic
                  
 score            1. Added additional input parameter named "crossValidVar"
                  2. In call to "wrtRegEquation" macro, added 2 additional parameters named "crossValidVar" and "inFile"
                  
 wrtRegEquation   1. Added 2 additional input parameters named "crossValidVar" and "inFile"
                  2. In call to "wrtScoreCode" macro, added 3 additional parameters named "mod", "crossValidVar" and "inFile"
                  
 drawLiftCurve    1. Added additional input parameter named "crossValidVar"
                  2. In call to "score" macro, added addtional parameter named "crossValidVar"
                  3. Added conditional logic for _cvInd_ and added macro variable in call to "makeSlice" macro
 
**********************************************************************/
