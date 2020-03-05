size <- function(year = str()){
  lookup <- paste("/Users/patricksimpson/Desktop/Test/pbpdata ", year, ".csv", sep = "")
  mycsv <- read.csv(lookup)
  mylength <- ncol(mycsv)
  return(mylength)
}


library(purrr)
years <- c("2018-19", "2017-18", "2016-17", "2015-16", "2014-15",
           "2013-14", "2012-13", "2011-12", "2010-11", "2009-10",
           "2008-09", "2007-08", "2006-07", "2005-06", "2004-05",
           "2003-04", "2002-03", "2001-02", "2000-01")


for(i in 1:length(years)){
  mydf <- cbind(years[i], size(year = years[i]))
  
  if(i == 1){
    masterdf <- mydf
  } else{
    helper <- mydf
    masterdf <- rbind(masterdf, helper)
  }
  i = i + 1
}
masterdf <- as.data.frame(masterdf)
colnames(masterdf) <- c("Year", "Measures")
remove(mydf, helper)
