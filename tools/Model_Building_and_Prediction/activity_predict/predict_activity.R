##########
args <- commandArgs(T)
arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]
#source("~/galaxy-dist/tools/mpdstoolsV2/tool3/Preold.R")
#pre(arg1,arg2,arg3
set.seed(1)
pre <- function(args1,args2,args3){
#args <- commandArgs(TRUE)
nTrain <- read.csv(args1,row.names= 1, header = T) # example nTrain.csv file of unknown activity
#save(nTrain,file = "nTrain.RData")
#load("nTrain.RData")
load(args2) # model generated from  previous programn  
newdata <- nTrain
modelFit <- Fit
###########
# input csv file must contaion the exact same column as used in model building #
# Also do pre-proccessing by means of centering and scaling 
## problem in s4 object so first check that the given model has s4 object in
## >isS4(Fit$finalmodel) if it is s4 than add in with elseif loop 
## eg . isS4(plsFit$finalModel) == TRUE
f=function(x){
   x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
   x[is.na(x) | is.nan(x) | is.infinite(x)] = median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}

f2=function(x){
               all(is.na(x))
                }


fop <- apply(newdata,2,f2)
allcolumnmissing <- which(fop)
if (length(allcolumnmissing) > 0){
newdata[,allcolumnmissing] <- 0
newdata[,allcolumnmissing] <- newdata[,allcolumnmissing] + runif(3,0,0.00001) ### add noise}
}

library(caret)

#if(as.character(!isS4(Fit$finalModel == "TRUE")))
if(exists('ppInfo')){
#if(as.character(!isS4(Fit$finalModel == "TRUE")))
if((Fit$method != "svmRadial") && (Fit$method != "svmLinear"))
{
        reqcol <- Fit$finalModel$xNames
        newdata <- newdata[,reqcol]
        newdata <- apply(newdata,2,f)
        #newdata <- newdata + runif(3,0,0.0001) ### add noise to overcome from NZV error
        #newdata1 <- preProcess(newdata, method = c("center", "scale"))
        #newdata1 <- preProcess(newdata, ppInfo)
        newdata11 <- predict(ppInfo,newdata)
###########
        library(stats)
        testpredict <- predict(modelFit,newdata11)
        Label <- levels(testpredict)
        a1 <- Label[1]
        a2 <- Label[2]
        probpredict <- predict(modelFit,newdata11,type="prob")
        names <- as.data.frame(rownames(nTrain))
        colnames(names) <- "COMPOUND"
        activity <- as.data.frame(testpredict)
        colnames(activity) <- "PREDICTED ACTIVITY"
        colnames(probpredict) <- c(eval(a1),eval(a2))
        Prob <- as.data.frame(probpredict)
        dw <- format(cbind(names,Prob,activity),justify="centre")
        write.table(dw,file=args3,row.names=FALSE,sep="\t")
        
        
        
} else if((Fit$method == "svmRadial") | (Fit$method == "svmLinear")){       
        library(stats)
        reqcol <- colnames(Fit$trainingData)
        reqcol <- reqcol[1:length(reqcol)-1]
        newdata <- newdata[,reqcol]

        newdata <- apply(newdata,2,f)
        #newdata <- newdata + runif(3,0,0.0001) ### add little noise to overcome from NZV problem
        #newdata1 <- preProcess(newdata, method = c("center", "scale"))
        #newdata1 <- preProcess(newdata,ppInfo)
        newdata11 <- predict(ppInfo,newdata)
        testpredict <- predict(modelFit,newdata11)
        Label <- levels(testpredict)
        a1 <- Label[1]
        a2 <- Label[2]
        probpredict <- predict(modelFit,newdata11,type="prob")
        names <- as.data.frame(rownames(nTrain))
        colnames(names) <- "COMPOUND"
        activity <- as.data.frame(testpredict)
        colnames(activity) <- "PREDICTED ACTIVITY"
        colnames(probpredict) <- c(eval(a1),eval(a2))
        Prob <- as.data.frame(probpredict)
        dw <- format(cbind(names,Prob,activity),justify="centre")
        write.table(dw,file=args3,row.names=FALSE,sep="\t")
}else {
      dw <- "There is something wrong in data or model"
	 write.csv(dw,file=args3,row.names=FALSE)
}
} else{

#if(as.character(!isS4(Fit$finalModel == "TRUE")))
if((Fit$method != "svmRadial") && (Fit$method != "svmLinear"))
{
        reqcol <- Fit$finalModel$xNames
        newdata <- newdata[,reqcol]
        newdata <- apply(newdata,2,f)
        #newdata <- newdata + runif(3,0,0.0001) ### add noise to overcome from NZV error
       
###########
        library(stats)
        testpredict <- predict(modelFit,newdata)
        Label <- levels(testpredict)
        a1 <- Label[1]
        a2 <- Label[2]
        probpredict <- predict(modelFit,newdata,type="prob")
        names <- as.data.frame(rownames(nTrain))
        colnames(names) <- "COMPOUND"
        activity <- as.data.frame(testpredict)
        colnames(activity) <- "PREDICTED ACTIVITY"
        colnames(probpredict) <- c(eval(a1),eval(a2))
        Prob <- as.data.frame(probpredict)
        dw <- format(cbind(names,Prob,activity),justify="centre")
        write.table(dw,file=args3,row.names=FALSE,sep="\t")
        
        
        
} else if((Fit$method == "svmRadial") | (Fit$method == "svmLinear")){       
        library(stats)
        reqcol <- colnames(Fit$trainingData)
        reqcol <- reqcol[1:length(reqcol)-1]
        newdata <- newdata[,reqcol]

        newdata <- apply(newdata,2,f)
        #newdata <- newdata + runif(3,0,0.0001) ### add little noise to overcome from NZV problem
              
        testpredict <- predict(modelFit,newdata)
        Label <- levels(testpredict)
        a1 <- Label[1]
        a2 <- Label[2]
        probpredict <- predict(modelFit,newdata,type="prob")
        names <- as.data.frame(rownames(nTrain))
        colnames(names) <- "COMPOUND"
        activity <- as.data.frame(testpredict)
        colnames(activity) <- "PREDICTED ACTIVITY"
        colnames(probpredict) <- c(eval(a1),eval(a2))
        Prob <- as.data.frame(probpredict)
        dw <- format(cbind(names,Prob,activity),justify="centre")
        write.table(dw,file=args3,row.names=FALSE,sep="\t")
}else {
      dw <- "There is something wrong in data or model"
	 write.csv(dw,file=args3,row.names=FALSE)
}
}
}
pre(arg1,arg2,arg3)
