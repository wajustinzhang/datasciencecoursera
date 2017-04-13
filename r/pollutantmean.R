pollutantmean<-function(directory, pollutant, id=1:332) {
  fs <- list.files(directory, pattern = "\\.csv", full.names=TRUE)
  totalData <-do.call(rbind, lapply(fs[id], read.csv))
  if(pollutant == "sulfate"){
    mean(totalData$sulfate[!is.na(totalData$sulfate)])
  }else if(pollutant == "nitrate"){
    mean(totalData$nitrate[!is.na(totalData$nitrate)])
  }
}

complete<-function(directory, id=1:332) {
  fs <- list.files(directory, pattern = "\\.csv", full.names=TRUE)
  totalData <-do.call(rbind, lapply(fs[id], read.csv))
  
  df <- data.frame(id=NA, nobs=NA)
  if(length(id) == 1) {
    item = c(id, nrow(totalData[totalData$ID==id&!is.na(totalData$sulfate) & !is.na(totalData$nitrate),]))
    df<-rbind(df,item)
  }
  else if(length(id) > 1) {
    for(idx in id) {
      item = c(idx, nrow(totalData[totalData$ID==idx&!is.na(totalData$sulfate) & !is.na(totalData$nitrate),]))
      df<-rbind(df,item)
    }
  }
  
  df<-na.omit(df)
  df
}


corr<- function(directory, threshold=0){
   cdata<-complete(directory) ##"specdata"
   ids <-cdata$id[cdata$nobs>threshold]
   
   fs <- list.files(directory, pattern = "\\.csv", full.names=TRUE)
   totalData <-do.call(rbind, lapply(fs[ids], read.csv))
   
   corv=vector();
   if(length(ids) == 1) {
     item <-totalData[totalData$ID==ids&!is.na(totalData$sulfate) & !is.na(totalData$nitrate),]
     corv<-c(corv, cor(item$sulfate, item$nitrate))
     
   }
   else if(length(ids) > 1) {
     for(idx in ids) {
       item <-totalData[totalData$ID==idx&!is.na(totalData$sulfate) & !is.na(totalData$nitrate),]
       corv<-c(corv, cor(item$sulfate, item$nitrate))
    }
  }
   
   corv
}

