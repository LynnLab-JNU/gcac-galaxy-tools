args <- commandArgs(T)

arg1 <- args[1]
arg2 <- args[2]
arg3 <- args[3]
arg4 <- args[4]
arg5 <- args[5]
arg6 <- args[6]
arg7 <- args[7]
arg8 <- args[8]
arg9 <- args[9]
arg10 <- args[10]
library(caret)
library(doMC)
load(arg1)

#RAWDATA <- dataX
#RAWDATA$outcome <- dataY


###########################
Smpling <- arg9

if(Smpling=="downsampling")
{
dwnsmpl <- downSample(dataX,dataY)
RAWDATA <- dwnsmpl[,1:length(dwnsmpl)-1]
RAWDATA$outcome <- dwnsmpl[,length(dwnsmpl)]
dataX <- RAWDATA[,1:length(dwnsmpl)-1]
dataY <- RAWDATA[,"outcome"]
remove("dwnsmpl")
}else if(Smpling=="upsampling"){
upsmpl <- upSample(dataX,dataY)
RAWDATA <- upsmpl[,1:length(upsmpl)-1]
RAWDATA$outcome <- upsmpl[,length(upsmpl)]
dataX <- RAWDATA[,1:length(upsmpl)-1]
dataY <- RAWDATA[,"outcome"]
remove("upsmpl")
}else { 
RAWDATA <- dataX
RAWDATA$outcome <- dataY
}




##########################


rawData <- dataX
predictorNames <- names(rawData)

isNum <- apply(rawData[,predictorNames, drop = FALSE], 2, is.numeric)
if(any(!isNum)) stop("all predictors in rawData should be numeric")

colRate <- apply(rawData[, predictorNames, drop = FALSE],
                 2, function(x) mean(is.na(x)))
colExclude <- colRate > 0.1
	if(any(colExclude)){
				predictorNames <- predictorNames[-which(colExclude)]
				rawData <- RAWDATA[, c(predictorNames,"outcome")]
				 } else {
	                        rawData <- RAWDATA 
						}  
                		rowRate <- apply(rawData[, predictorNames, drop = FALSE],
                 		1, function(x) mean(is.na(x)))
			

rowExclude <- rowRate > 0
	if(any(rowExclude)){
  				rawData <- rawData[!rowExclude, ]
    				##hasMissing <- apply(rawData[, predictorNames, drop = FALSE],
                        	##1, function(x) mean(is.na(x)))
                   
############################################################################
                                                                      
            
###############################################################################                        	
                    } else {  
                    		rawData <- rawData[complete.cases(rawData),]

                    		} 
                    
set.seed(2)

#print(dim(dataX))
#print(dim(rawData))
#print(length(dataY))

nzv <- nearZeroVar(rawData[,1:(length(rawData) - 1)])
	  if(length(nzv) > 0)  {
    				#nzvVars <- names(rawData)[nzv]
    				rawData <- rawData[,-nzv]
   				#rawData$outcome <- dataY
    				} 
    
predictorNames <- names(rawData)[names(rawData) != "outcome"]
   
dx <- rawData[,1:length(rawData)-1]
dy <- rawData[,length(rawData)]
corrThresh <- as.numeric(arg8)
highCorr <- findCorrelation(cor(dx, use = "pairwise.complete.obs"),corrThresh)
dx <- dx[, -highCorr]
subsets <- seq(1,length(dx),by=5)
normalization <- preProcess(dx)
dx <- predict(normalization, dx)
dx <- as.data.frame(dx)

if (arg4 == "lmFuncs"){
ctrl1 <- rfeControl(functions = lmFuncs,
                   method = arg5 ,
                   repeats = as.numeric(arg6),
                   number = as.numeric(arg7),
                   verbose = FALSE)
} else if(arg4 == "rfFuncs"){
ctrl1 <- rfeControl(functions = rfFuncs,
                   method = arg5 ,
                   repeats = as.numeric(arg6),
                   number = as.numeric(arg7),
                   verbose = FALSE)
}else if (arg4 == "treebagFuncs"){
ctrl1 <- rfeControl(functions = treebagFuncs,
                   method = arg5 ,
                   repeats = as.numeric(arg6),
                   number = as.numeric(arg7),
                   verbose = FALSE)
}else {

ctrl1 <- rfeControl(functions = nbFuncs,
                   method = arg5 ,
                   repeats = as.numeric(arg6),
                   number = as.numeric(arg7),
                   verbose = FALSE)
}



if (as.numeric(arg10) == 1){ 
Profile <- rfe(dx, dy,sizes = subsets,rfeControl = ctrl1)

pred11 <- predictors(Profile)
save(Profile,file=arg2)
dataX <- rawData[,pred11]
dataY <- rawData$outcome

save(dataX,dataY,file=arg3)
rm(dataX)
rm(dataY)
} else if (as.numeric(arg10) > 1){
registerDoMC(cores = as.numeric(arg10))

Profile <- rfe(dx, dy,sizes = subsets,rfeControl = ctrl1)

pred11 <- predictors(Profile)
save(Profile,file=arg2)
dataX <- rawData[,pred11]
dataY <- rawData$outcome

save(dataX,dataY,file=arg3)
rm(dataX)
rm(dataY)
} else { stop("something went wrong. please see the parameters")}  


