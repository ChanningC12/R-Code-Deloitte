# test imputing missing values
df = data.frame(a = c(1,2,3,4,5,NA,NA,4,2,1,3,6),b=c(1:10,NA,NA),c=c(rep("A",4),NA,NA,rep("B",6)))
str(df)
df$b = as.numeric(df$b)
df$c = as.character(df$c)

# Impute missing values: numerical to median, character to mode
Mode = function (x, na.rm) {
  xtab = table(x)
  xmode = names(which(xtab == max(xtab)))
  if (length(xmode) > 1)
    xmode = ">1 mode"
  return(xmode)
}


  for (x in 1:ncol(df)) {
  if(class(df[,x])=="numeric"){
    df[is.na(df[,x]),x] = median(df[,x],na.rm=T)
  }
  else if(class(df[,x])=="character"){
    df[is.na(df[,x]),x] = Mode(df[,x],na.rm=T)
  }
  }