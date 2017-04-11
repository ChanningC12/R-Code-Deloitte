/*********************************************************************************
Macro Name : GETMODELNO

Automated Procedure to generate model numbers (Used from Model 2 - Using KBM data)
**********************************************************************************/

%macro getModelNo;
	

  %let DigitOne = 99;

  %if %upcase(%cmpres(&model_type))= NFO %then %let DigitOne = 1;
  %if %upcase(%cmpres(&model_type))= RBO %then %let DigitOne = 2;
  %if %upcase(%cmpres(&model_type))= HYBRID %then %let DigitOne = 3;



  
  %let DigitModel = 9; 
  %if %upcase(%cmpres(&analysis))= STEPWISE %then %let DigitModel = 1;
  %if %upcase(%cmpres(&analysis))= MODELING %then %let DigitModel = 2;

  %let DigitVarlistNum = 09; 
  %let DigitVarlistNum = &varLstNm.;
  
  %let DigitSplit = 9;
  %let DigitSplit = %substr(%cmpres(&split),7,1);
   
  %let modelNo = &DigitOne.&DigitModel.&DigitVarlistNum.&DigitSplit.;

  %put modelNo= [&modelNo];
  
%mend;



/***********************************************************************************************
Macro Name : MODEL_RESULTS_LOGISTIC

Print Model Results customised for Logistic Regression (showing F-values and unwound parameters
***********************************************************************************************/

%macro model_results_logistic (modelNo=&modelNo,
                      modLbl=,
                      sheetLabel=summary,
                      inFile=,
                      princompFile=, 
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
    %compare_pe_PFG(statlist = Estimate chisq,noprint=y,dsns= %do _i_ = 1 %to &_count_modelno_;
    %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
    %cmpres(mreg.statout&thismodelNo.)
        %end;);
 
    data _allpe_rename_;
        set allpe;
        %do _i_ = 1 %to &_count_modelno_;
            %let thismodelNo = %scan(&modelNo,&_i_,%str( ));
            %let thismodLbl = %scan(&modLbl,&_i_,%str( ));
            Estimate_&thismodLbl._pe=Estimate_statout&thismodelno.;
            chisq_&thismodLbl._pe=chisq_statout&thismodelno.;
            drop Estimate_statout&thismodelno. chisq_statout&thismodelno.;
            %end;
    run;
 
    *Unwinded params for comparison;
    %if "&princompFile" ^= "" %then %do; 
      %compare_pe_uw_PFG(statlist =, noprint=y,dsns= %do _i_ = 1 %to &_count_modelno_;
      %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
      mreg.regOut_&thismodelNo.
          %end;,
          princompFile= &princompFile);
 
      data _allpe_rename_uw_;
          set allpe;
          %do _i_ = 1 %to &_count_modelno_;
              %let thismodelNo = %scan(&modelNo,&_i_,%str( ));
              %let thismodLbl = %scan(&modLbl,&_i_,%str( ));
              /*UnWind_&thismodLbl.=Estimate_regOut_kbm_&thismodelno.;
              drop Estimate_regOut_kbm_&thismodelno.;*/
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
     
     %if "&princompFile" ^= "" %then %do; 

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

   %if "&princompFile" ^= "" %then %do; 
      ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="&sheetLabel Unwinded PE");
      proc print data=_allpe_rename_uw_ noobs width=min;run;    
   %end;
%mend model_results_logistic;

/***********************************************************************************************
Macro Name : REG_SUMMARY

Print Regression summary with average lifts over 5 random splits
***********************************************************************************************/
%macro reg_summary(firstmodelno=0,lastmodelno=999999999,referenceVarList=&refVarList,doAvg5=N);
    %if "&referenceVarList"="" %then %do;
        %let referenceVarList=%str(modelno infile fieldName varLstNm target split);
        %end;

    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="reg_summary");  

        %if "&doAvg5"="Y" %then %do;
		
			data temp_6th_rw;
				set _tempAll_(rename=(modelno=modelno_temp));
				modelno = INT(modelno_temp/10)*10;
			run;
	 		
	 		proc summary data = temp_6th_rw nway noprint;
	 			var s00_25 s25_50 s50_75 s75_100 s00_10 s10_20 s20_30 s30_40 s40_50 s50_60 s60_70 s70_80 s80_90 s90_100 s95_100;
	 			class modelno modfile infile fieldname;
	 			output out = split_6th_rw mean=;
	 		run;
	 		
	 		data model_temp;
	 			set temp_6th_rw;
	 			drop split fieldname infile;
	 		run;
	 		
	 		proc sort data = model_temp nodupkey; by modelno modfile; run;
	 		
	 		 data split_temp;
            merge split_6th_rw (in=s) model_temp (in=ma);
            by modelno modfile;
            split='AVG5';
       run;
            
            
        data split_temp3;
            set _tempAll_ split_temp;
        run;
        
	 %end;
			
			proc print data = 
						%if "&doAvg5"="N" %then %do; _tempAll_ %end;
        %else %do; split_temp3 %end; 
			
    	noobs;
        where modelno >= &firstmodelno and modelno <= &lastmodelno;
        var &referenceVarList
            s00_25 s25_50 s50_75 s75_100
            s00_10 s10_20 s20_30 s30_40 s40_50 s50_60 s60_70 s70_80 s80_90 s90_100
            s95_100; run;
		
		proc datasets library=work memtype=data nolist;
		delete _all_;
		quit;	

    %mend reg_summary; 

/***********************************************************************************************
GENMOD Procedure for binary case. Macro from AXA project.
***********************************************************************************************/
    
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

/*
       %if "&link"="log" %then %do; 
            _residual_ = &target/exp(model1); 
       %end; %else %do;  
            _residual_ = &target - model1;  
       %end;
*/
       %if "&offset" ne "" %then %do;
           &offset = model1 ;
           _residual_ = &target;
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

/*** Correlation macro with some updates ***/
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

   	data _null_; set a;
   		c=",";x=-1; 
   		file "&outCSVfile";
   		if _n_ = 1 then do;
      		line1="obs,varname1,varname2,correlation";
      		put line1 ;
   		end;
   		varname1 = upcase(varname1);
   		varname2 = upcase(varname2);
   		put _n_ +x c +x varname1 +x c +x varname2 +x c +x corr ; 

%mend;

/********COMPARE_PE AND COMPARE_PE_UW ************/



%macro compare_pe_PFG(dsns=,outcsv=,noprint=N, statList=Estimate tValue);
   
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
       
%mend compare_pe_PFG;






%macro compare_pe_uw_PFG(statList=,dsns=,outCSV=,
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

%mend compare_pe_uw_PFG;

/*** Correlation macro with some updates ***/
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

   	data _null_; set a;
   		c=",";x=-1; 
   		file "&outCSVfile";
   		if _n_ = 1 then do;
      		line1="obs,varname1,varname2,correlation";
      		put line1 ;
   		end;
   		varname1 = upcase(varname1);
   		varname2 = upcase(varname2);
   		put _n_ +x c +x varname1 +x c +x varname2 +x c +x corr ; 

%mend;


%macro xlsCorr(inFile,lib,modelNo,cutoff=0,outfile=&workPath./corr.csv);
    *Correlation for model variables;
        
    proc sql noprint;
    select upcase(variable)
    into:_corrvarlist_
    separated by ' '      
    from &lib..&corrFile.
    where(upcase(variable) not in ("INTERCEPT")) and (substr(upcase(variable),1,9) not in ("INTERCEPT"));
    quit;

    data _corrView_ / view = _corrView_;
    set &infile(keep = &_corrvarlist_.);
    run;
    /**added by sakshi***/
	%correlation(libnamex=work, dsname=_corrView_, vars=&_corrvarlist_.
,val=&cutoff.,outfile=&outfile.);
	proc print data=corrcol noobs;
	run;
/*    %corr(inLib=work,infile= _corrView_,cutOff= &cutoff., upperTriangleOnly=1,outCSVfile=&workPath./corr.csv);*/

    *Generate XLS output for correlation;
/*    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="Correlation");  */
/*    proc print data=a noobs width=min; var varname1 varname2 corr; run;*/
%mend;

/********************************************************************************************
Updates to %model_results macro to account for user defined libraries rather than MREG only
********************************************************************************************/

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
    %cmpres(&reglib..model&thismodelNo._pe)
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
    %if "&princompFile" ^= "" %then %do; 
    %if "&regtype"="" %then %do;
        %compare_pe_uw(dsns= %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        &reglib..regout_&model_type._&thismodelNo
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

    %if "&princompFile" ^= "" %then %do; 
		%if "&regtype"="" %then %do;    
    %do _i_ = 1 %to &_count_modelno_;
        %let thismodelNo =%qscan(&modelNo,&_i_,%str( ));
        %unwindParms(regParmFile=%cmpres(&reglib..regout_&model_type._&thismodelNo.),
            princompFile= &princompFile, 
            outParmFile=outParmFile,
            outSAScode=&workpath./score&thismodelNo..sas,
            yhatFldName=score_&thismodelNo.);
        %end;
     %end;
    %end;
    *Generate XLS output for review;
    ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="&sheetLabel PE");  
    proc print data=_allpe_rename_ noobs width=min;run;

   %if "&princompFile" ^= "" %then %do; 
    %if "&regtype"="" %then %do;        
      ODS TAGSETS.EXCELXP OPTIONS(zoom='75' AutoFilter = 'All' SHEET_NAME="&sheetLabel Unwinded PE");
      proc print data=_allpe_rename_uw_ noobs width=min;run;    
    %end;
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

%macro getSummaryInExcel_LPM(inSummaryFile,firstModelNo,lastModelNo,
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
      line4="00%-10%@10%-20%@20%-30%@30%-40%@40%-50%@50%-60%@60%-70%@70%-80%@80%-90%@90%-100%@ @";
      line5="95%-100%@ @";
      put "sep=@";
      put line1 line2 line3 line4 line5;
   end;

 put modelno +x c +x infile +x c+x fieldName +x c +x
     &_refVarList2_ 
     s00_25 +x c +x s25_50 +x c +x s50_75 +x c +x s75_100 +x c +x  c 
     s00_10 +x c +x s10_20 +x c +x s20_30 +x c +x s30_40 +x c +x s40_50 +x c +x s50_60 +x c +x s60_70 +x c +x s70_80 +x c +x s80_90 +x c +x s90_100 +x c +x c
     s95_100 +x c +x c
    ;
   *modified by zbs 6/20/06
   *proc sql;drop table _tempAll_; 
%mend;
