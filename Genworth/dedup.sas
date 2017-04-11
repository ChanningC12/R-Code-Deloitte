%macro dedup(stringw=,passyes=No);
      %global _unique__;
      %local _count_;
      %let _count_=1;
      %let _unique__=;

      %do %while(%quote(%scan(&stringw,&_count_,%str( ))) ne);
          %if %qsysfunc(indexw(%upcase(&_unique__),%upcase(%scan(&stringw,&_count_,%str( )))))=0 %then 
          %let _unique__=&_unique__ %scan(&stringw,&_count_,%str( )); 
          %let _count_=%eval(&_count_+1);
      %end;
      
      %if %upcase(&passyes)=YES %then
         %do;
              &_unique__ %* generate count *;
         %end;
%mend dedup;
