def __template4Rnw():

	template4Rnw = r'''%% Classification Modeling Script 
%% Max Kuhn (max.kuhn@pfizer.com, mxkuhn@gmail.com)
%% Version: 1.00
%% Created on: 2010/10/02
%% 
%% The originla file hs been improved by 
%% Deepak Bharti, Andrew M. Lynn , Anmol J. Hemrom 
%% Version : 1.01
%% created on : 2014/08/12
%% This is an Sweave template for building and describing
%% classification models. It mixes R and LaTeX code. The document can
%% be processing using R's Sweave function to produce a tex file.  
%%
%% The inputs are:
%% - the initial data set in a data frame called 'rawData' 
%% - a factor column in the data set called 'class'. this should be the
%%    outcome variable 
%% - all other columns in rawData should be predictor variables
%% - the type of model should be in a variable called 'modName'.
%% 
%% The script attempts to make some intelligent choices based on the
%% model being used. For example, if modName is "pls", the script will
%% automatically center and scale the predictor data. There are
%% situations where these choices can (and should be) changed.   
%%
%% There are other options that may make sense to change. For example,
%% the user may want to adjust the type of resampling. To find these
%% parts of the script, search on the string 'OPTION'. These parts of
%% the code will document the options. 

\documentclass[14pt]{report}
\usepackage{amsmath}
\usepackage[pdftex]{graphicx}
\usepackage{color}
\usepackage{ctable}
\usepackage{xspace}
\usepackage{fancyvrb}
\usepackage{fancyhdr}
\usepackage{lastpage}
\usepackage{longtable} 
\usepackage{algorithm2e}
\usepackage[
         colorlinks=true,
         linkcolor=blue,
         citecolor=blue,
         urlcolor=blue]
           {hyperref}
\usepackage{lscape}
\usepackage{Sweave}
\SweaveOpts{keep.source = TRUE}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% define new colors for use
\definecolor{darkgreen}{rgb}{0,0.6,0}
\definecolor{darkred}{rgb}{0.6,0.0,0}
\definecolor{lightbrown}{rgb}{1,0.9,0.8}
\definecolor{brown}{rgb}{0.6,0.3,0.3}
\definecolor{darkblue}{rgb}{0,0,0.8}
\definecolor{darkmagenta}{rgb}{0.5,0,0.5}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\bld}[1]{\mbox{\boldmath $$#1$$}}
\newcommand{\shell}[1]{\mbox{$$#1$$}}
\renewcommand{\vec}[1]{\mbox{\bf {#1}}}

\newcommand{\ReallySmallSpacing}{\renewcommand{\baselinestretch}{.6}\Large\normalsize}
\newcommand{\SmallSpacing}{\renewcommand{\baselinestretch}{1.1}\Large\normalsize}

\newcommand{\halfs}{\frac{1}{2}}

\setlength{\oddsidemargin}{-.25 truein}
\setlength{\evensidemargin}{0truein}
\setlength{\topmargin}{-0.2truein}
\setlength{\textwidth}{7 truein}
\setlength{\textheight}{8.5 truein}
\setlength{\parindent}{0.20truein}
\setlength{\parskip}{0.10truein}

\setcounter{LTchunksize}{50}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\pagestyle{fancy}
\lhead{}
%% OPTION Report header name
\chead{Classification Model Script}
\rhead{}
\lfoot{}
\cfoot{}
\rfoot{\thepage\ of \pageref{LastPage}}
\renewcommand{\headrulewidth}{1pt}
\renewcommand{\footrulewidth}{1pt}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OPTION Report title and modeler name
\title{Classification Model Script using $METHOD}
\author{"Lynn Group with M. Kuhn, SCIS, JNU, New Delhi"} 

\begin{document}

\maketitle

\thispagestyle{empty}
<<dummy, eval=TRUE, echo=FALSE, results=hide>>=
# sets values for variables used later in the program to prevent the \Sexpr error on parsing with Sweave
numSamples=''
classDistString=''
missingText=''
numPredictors=''
numPCAcomp=''
pcaText=''
nzvText=''
corrText=''
ppText=''
varText=''
splitText="Dummy Text"
nirText="Dummy Text"
# pctTrain is a variable that is initialised in Data splitting, and reused later in testPred
pctTrain=0.8
Smpling=''
nzvText1=''
classDistString1=''
dwnsmpl=''
upsmpl=''

@	
<<startup, eval= TRUE, results = hide, echo = FALSE>>=
library(Hmisc)
library(caret)
library(pROC)
versionTest <- compareVersion(packageDescription("caret")$$Version, 
                              "4.65")
if(versionTest < 0) stop("caret version 4.65 or later is required")

library(RColorBrewer)


listString <- function (x, period = FALSE, verbose = FALSE) 
{
  if (verbose)   cat("\n      entering listString\n")
  flush.console()
  if (!is.character(x)) 
    x <- as.character(x)
  numElements <- length(x)
  out <- if (length(x) > 0) {
    switch(min(numElements, 3), x, paste(x, collapse = " and "), 
           {
             x <- paste(x, c(rep(",", numElements - 2), " and", ""), sep = "")
             paste(x, collapse = " ")
           })
  }
  else ""
  if (period)  out <- paste(out, ".", sep = "")
  if (verbose)  cat("      leaving  listString\n\n")
  flush.console()
  out
}

resampleStats <- function(x, digits = 3)
  {
    bestPerf <- x$$bestTune
    colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
    out <- merge(x$$results, bestPerf)
    out <- out[, colnames(out) %in% x$$perfNames]
    names(out) <- gsub("ROC", "area under the ROC curve", names(out), fixed = TRUE)
    names(out) <- gsub("Sens", "sensitivity", names(out), fixed = TRUE)
    names(out) <- gsub("Spec", "specificity", names(out), fixed = TRUE)
    names(out) <- gsub("Accuracy", "overall accuracy", names(out), fixed = TRUE)
    names(out) <- gsub("Kappa", "Kappa statistics", names(out), fixed = TRUE)
    
    out <- format(out, digits = digits)
    listString(paste(names(out), "was", out))
  }

twoClassNoProbs <- function (data, lev = NULL, model = NULL) 
{
  out <- c(sensitivity(data[, "pred"], data[, "obs"], lev[1]), 
           specificity(data[, "pred"], data[, "obs"], lev[2]),
           confusionMatrix(data[, "pred"], data[, "obs"])$$overall["Kappa"])
  
  names(out) <- c("Sens", "Spec", "Kappa")
  out
}



##OPTION: model name: see ?train for more values/models
modName <- "$METHOD"


load("$RDATA")
rawData <- dataX
rawData$$outcome <- dataY

@ 


\section*{Data Sets}\label{S:data}

%% OPTION: provide some background on the problem, the experimental
%% data, how the compounds were selected etc

<<getDataInfo, eval = $GETDATAINFOEVAL, echo = $GETDATAINFOECHO, results = $GETDATAINFORESULT>>=
if(!any(names(rawData) == "outcome")) stop("a variable called outcome should be in the data set")
if(!is.factor(rawData$$outcome)) stop("the outcome should be a factor vector")

## OPTION: when there are only two classes, the first level of the 
##         factor is used as the "positive" or "event" for calculating
##         sensitivity and specificity. Adjust the outcome factor accordingly.
numClasses <- length(levels(rawData$$outcome))
numSamples <- nrow(rawData)
numPredictors <- ncol(rawData) - 1
predictorNames <- names(rawData)[names(rawData) != "outcome"]

isNum <- apply(rawData[,predictorNames, drop = FALSE], 2, is.numeric)
if(any(!isNum)) stop("all predictors in rawData should be numeric")

classTextCheck <- all.equal(levels(rawData$$outcome), make.names(levels(rawData$$outcome)))
if(!classTextCheck) warning("the class levels are not valid R variable names; this may cause errors")

## Get the class distribution
classDist <- table(rawData$$outcome)
classDistString <- paste("``",
                         names(classDist),
                         "'' ($$n$$=",
                         classDist,
                         ")",
                         sep = "")
classDistString <- listString(classDistString)
@ 

<<missingFilter, eval = $MISSINGFILTEREVAL, echo = $MISSINGFILTERECHO, results = $MISSINGFILTERRESULT>>=
colRate <- apply(rawData[, predictorNames, drop = FALSE],
                 2, function(x) mean(is.na(x)))

##OPTION thresholds can be changed
colExclude <- colRate > $MISSINGFILTERTHRESHC

missingText <- ""

if(any(colExclude))
  {
    missingText <- paste(missingText,
                         ifelse(sum(colExclude) > 1,
                                " There were ",
                                " There was "),
                         sum(colExclude),
                         ifelse(sum(colExclude) > 1,
                                " predictors ",
                                " predictor "),
                         "with an excessive number of ",
                         "missing data. ",
                         ifelse(sum(colExclude) > 1,
                                " These were excluded. ",
                                " This was excluded. "))
    predictorNames <- predictorNames[!colExclude]
    rawData <- rawData[, names(rawData) %in% c("outcome", predictorNames), drop = FALSE]
  }


rowRate <- apply(rawData[, predictorNames, drop = FALSE],
                 1, function(x) mean(is.na(x)))

rowExclude <- rowRate > $MISSINGFILTERTHRESHR


if(any(rowExclude)) {
    missingText <- paste(missingText,
                         ifelse(sum(rowExclude) > 1,
                                " There were ",
                                " There was "),
                         sum(colExclude),
                         ifelse(sum(rowExclude) > 1,
                                " samples ",
                                " sample "),
                         "with an excessive number of ",
                         "missing data. ",
                         ifelse(sum(rowExclude) > 1,
                                " These were excluded. ",
                                " This was excluded. "),
                         "After filtering, ",
                         sum(!rowExclude),
                         " samples remained.")
    rawData <- rawData[!rowExclude, ]
    hasMissing <- apply(rawData[, predictorNames, drop = FALSE],
                        1, function(x) mean(is.na(x)))
  } else {
        hasMissing <- apply(rawData[, predictorNames, drop = FALSE],
                        1, function(x) any(is.na(x)))
        missingText <- paste(missingText,
                             ifelse(missingText == "",
                                "There ",
                                "Subsequently, there "),
                             ifelse(sum(hasMissing) == 1,
                                    "was ",
                                    "were "),
                             ifelse(sum(hasMissing) > 0, 
                                    sum(hasMissing), 
                                    "no"),
                             ifelse(sum(hasMissing) == 1,
                                    "sample ",
                                    "samples "),
                             "with missing values.")                            
    
  rawData <- rawData[complete.cases(rawData),]

  }

rawData1 <- rawData[,1:length(rawData)-1]
rawData2 <- rawData[,length(rawData)]

set.seed(222)
nzv1 <- nearZeroVar(rawData1)
  if(length(nzv1) > 0)
  {
    nzvVars1 <- names(rawData1)[nzv1]
    rawData <- rawData1[, -nzv1]
    rawData$outcome <- rawData2
    nzvText1 <- paste("There were ",
                     length(nzv1),
                     " predictors that were removed from original data due to",
                     " severely unbalanced distributions that",
                     " could negatively affect the model fit",
                     ifelse(length(nzv1) > 10,
                            ".",
                            paste(": ",
                                  listString(nzvVars1),
                                  ".",
                                  sep = "")),
                     sep = "")

 } else {
rawData <- rawData1
rawData$outcome <- rawData2
nzvText1 <- ""

}

remove("rawData1")
remove("rawData2")

@ 

  The initial data set consisted of \Sexpr{numSamples} samples and
\Sexpr{numPredictors} predictor variables. The breakdown of the
outcome data classes were: \Sexpr{classDistString}. 

 \Sexpr{missingText}
 
 \Sexpr{nzvText1}

<<pca, eval= $PCAEVAL, echo = $PCAECHO, results = $PCARESULT>>=

predictorNames <- names(rawData)[names(rawData) != "outcome"]
numPredictors <- length(predictorNames)
predictors <- rawData[, predictorNames, drop = FALSE]
## PCA will fail with predictors having less than 2 unique values
isZeroVar <- apply(predictors, 2, 
                   function(x) length(unique(x)) < 2)
if(any(isZeroVar)) predictors <- predictors[, !isZeroVar, drop = FALSE]
## For whatever, only the formula interface to prcomp 
## handles missing values
pcaForm <- as.formula(
                      paste("~",
                            paste(names(predictors), collapse = "+")))
pca <- prcomp(pcaForm, 
              data = predictors,
              center = TRUE, 
              scale. = TRUE,
              na.action = na.omit)
## OPTION: the number of components plotted/discussed can be set
numPCAcomp <- $PCACOMP
pctVar <- pca$$sdev^2/sum(pca$$sdev^2)*100
pcaText <- paste(round(pctVar[1:numPCAcomp], 1),
                 "\\\\%", 
                 sep = "")
pcaText <- listString(pcaText)
@

 To get an initial assessment of the separability of the classes,
 principal component analysis (PCA) was used to distill the
 \Sexpr{numPredictors} predictors down into \Sexpr{numPCAcomp}
 surrogate variables (i.e. the principal components) in a manner that
 attempts to maximize the amount of information preserved from the
 original predictor set. Figure \ref{F:inititalPCA} contains plots of
 the first \Sexpr{numPCAcomp} components, which accounted for
 \Sexpr{pcaText} percent of the variability in the original predictors
 (respectively).  


%% OPTION: remark on how well (or poorly) the data separated

 \setkeys{Gin}{width = 0.8\textwidth}
 \begin{figure}[p]
 \begin{center}

<<pcaPlot, eval = $PCAPLOTEVAL, echo = $PCAPLOTECHO, results = $PCAPLOTRESULT, fig = $PCAPLOTFIG, width = 8, height = 8>>=
trellis.par.set(caretTheme(), warn = TRUE)
if(numPCAcomp == 2)
  {
    axisRange <- extendrange(pca$$x[, 1:2])
    print(
          xyplot(PC1 ~ PC2, 
                 data = as.data.frame(pca$$x),
                 type = c("p", "g"),
                 groups = rawData$$outcome,
                 auto.key = list(columns = 2),
                 xlim = axisRange,
                 ylim = axisRange))
  } else {
    axisRange <- extendrange(pca$$x[, 1:numPCAcomp])
    print(
          splom(~as.data.frame(pca$$x)[, 1:numPCAcomp],
                type = c("p", "g"),
                groups = rawData$$outcome,
                auto.key = list(columns = 2),
                as.table = TRUE,
                prepanel.limits = function(x) axisRange
                ))      
    
      } 

@
 
   \caption[PCA Plot]{A plot of the first \Sexpr{numPCAcomp}
   principal components for the original data set.}
   \label{F:inititalPCA}         
 \end{center}
 \end{figure}  



<<initialDataSplit, eval = $INITIALDATASPLITEVAL, echo = $INITIALDATASPLITECHO, results = $INITIALDATASPLITRESULT>>=

  ## OPTION: in small samples sizes, you may not want to set aside a
  ## training set and focus on the resampling results.   

set.seed(1234)
dataX <- rawData[,1:length(rawData)-1]
dataY <- rawData[,length(rawData)]

 Smpling <- "$SAAMPLING"

if(Smpling=="downsampling")
{
dwnsmpl <- downSample(dataX,dataY)
rawData <- dwnsmpl[,1:length(dwnsmpl)-1]
rawData$outcome <- dwnsmpl[,length(dwnsmpl)]
remove("dwnsmpl")
remove("dataX")
remove("dataY")
}else if(Smpling=="upsampling"){
upsmpl <- upSample(dataX,dataY)
rawData <- upsmpl[,1:length(upsmpl)-1]
rawData$outcome <- upsmpl[,length(upsmpl)]
remove("upsmpl")
remove("dataX")
remove("dataY")
}else{remove("dataX")
remove("dataY")
}



numSamples <- nrow(rawData)

predictorNames <- names(rawData)[names(rawData) != "outcome"]
numPredictors <- length(predictorNames)


classDist1 <- table(rawData$outcome)
classDistString1 <- paste("``",
                         names(classDist1),
                         "'' ($n$=",
                         classDist1,
                         ")",
                         sep = "")
classDistString1 <- listString(classDistString1)

  pctTrain <- $PERCENT

if(pctTrain < 1)
  {
    ## OPTION: seed number can be changed
    set.seed(1)
    inTrain <- createDataPartition(rawData$$outcome,
                                   p = pctTrain,
                                   list = FALSE)
    trainX <- rawData[ inTrain, predictorNames]
    testX  <- rawData[-inTrain, predictorNames]
    trainY <- rawData[ inTrain, "outcome"]
    testY  <- rawData[-inTrain, "outcome"]
    splitText <- paste("The original data were split into ",
                       "a training set ($$n$$=",
                       nrow(trainX),
                       ") and a test set ($$n$$=",
                       nrow(testX),
                       ") in a manner that preserved the ",
                       "distribution of the classes.",
                       sep = "")
    isZeroVar <- apply(trainX, 2, 
                       function(x) length(unique(x)) < 2)
    if(any(isZeroVar))
      {
        trainX <- trainX[, !isZeroVar, drop = FALSE]  
        testX <- testX[, !isZeroVar, drop = FALSE]
      }
    
  } else {
    trainX <- rawData[, predictorNames]
    testX  <- NULL
    trainY <- rawData[, "outcome"]
    testY  <- NULL 
    splitText <- "The entire data set was used as the training set."
  }
trainDist <- table(trainY)
nir <- max(trainDist)/length(trainY)*100
niClass <- names(trainDist)[which.max(trainDist)]
nirText <- paste("The non--information rate is the accuracy that can be ",
                 "achieved by predicting all samples using the most ",
                 "dominant class. For these data, the rate is ",
                 round(nir, 2), "\\\\% using the ``",
                 niClass,
                 "'' class.",
                 sep = "")

remove("rawData")

if((!is.null(testX)) && (!is.null(testY))){
#save(trainX,trainY,testX,testY,file="datasets.RData")
} else {
save(trainX,trainY,file="datasets.RData")
}

@ 

 \Sexpr{splitText} 

 \Sexpr{nirText}

The data set for model building consisted of \Sexpr{numSamples} samples and
\Sexpr{numPredictors} predictor variables. The breakdown of the
outcome data classes were: \Sexpr{classDistString1}.

<<nzv, eval= $NZVEVAL, results = $NZVRESULT, echo = $NZVECHO>>=
## OPTION: other pre-processing steps can be used
ppSteps <- caret:::suggestions(modName)

set.seed(2)
if(ppSteps["nzv"])
  {
    nzv <- nearZeroVar(trainX)
    if(length(nzv) > 0) 
      {
        nzvVars <- names(trainX)[nzv]
        trainX <- trainX[, -nzv]
        nzvText <- paste("There were ",
                         length(nzv),
                         " predictors that were removed from train set due to",
                         " severely unbalanced distributions that",
                         " could negatively affect the model",
                         ifelse(length(nzv) > 10, 
                                ".",
                                paste(": ",
                                      listString(nzvVars),
                                      ".",
                                      sep = "")),
                         sep = "") 
        testX <- testX[, -nzv]
      } else nzvText <- ""
  } else nzvText <- ""
@ 

\Sexpr{nzvText}


<<corrFilter, eval = $CORRFILTEREVAL, results = $CORRFILTERRESULT, echo = $CORRFILTERECHO>>=
if(ppSteps["corr"])
  {
    ## OPTION: 
    corrThresh <- $THRESHHOLDCOR
    highCorr <- findCorrelation(cor(trainX, use = "pairwise.complete.obs"), 
                                corrThresh)
    if(length(highCorr) > 0) 
      {
        corrVars <- names(trainX)[highCorr]
        trainX <- trainX[, -highCorr]
        corrText <- paste("There were ",
                         length(highCorr),
                         " predictors that were removed due to",
                         " large between--predictor correlations that",
                         " could negatively affect the model fit",
                         ifelse(length(highCorr) > 10, 
                                ".",
                                paste(": ",
                                      listString(highCorr),
                                      ".",
                                      sep = "")),
                          " Removing these predictors forced",
                          " all pair--wise correlations to be",
                          " less than ",
                          corrThresh,
                          ".",
                          sep = "") 
        testX <- testX[, -highCorr]
      } else corrText <- "No correlation among data on given threshold"
  }else corrText <- ""
@

 \Sexpr{corrText}

<<preProc, eval = $PREPROCEVAL, echo = $PREPROCECHO, results = $PREPROCRESULT>>=
ppMethods <- NULL
if(ppSteps["center"]) ppMethods <- c(ppMethods, "center")
if(ppSteps["scale"]) ppMethods <- c(ppMethods, "scale")
if(any(hasMissing) > 0) ppMethods <- c(ppMethods, "knnImpute")
##OPTION other methods, such as spatial sign, can be added to this list

if(length(ppMethods) > 0)
  {
    ppInfo <- preProcess(trainX, method = ppMethods)
    trainX <- predict(ppInfo, trainX)
    if(pctTrain < 1) testX <- predict(ppInfo, testX)   
    ppText <- paste("The following pre--processing methods were",
                    " applied to the training",
                    ifelse(pctTrain < 1, " and test", ""),
                    " data: ",
                    listString(ppMethods),
                    ".",
                    sep = "")
    ppText <- gsub("center", "mean centering", ppText)
    ppText <- gsub("scale", "scaling to unit variance", ppText)
    ppText <- gsub("knnImpute", 
                   paste(ppInfo$$k, "--nearest neighbor imputation", sep = ""), 
                   ppText)
    ppText <- gsub("spatialSign", "the spatial sign transformation", ppText)
    ppText <- gsub("pca", "principal component feature extraction", ppText)
    ppText <- gsub("ica", "independent component feature extraction", ppText)
    } else {
      ppInfo <- NULL
      ppText <- ""
    }

predictorNames <- names(trainX)
if(nzvText != "" | corrText != "" | ppText != "")
  {
    varText <- paste("After pre--processing, ",
                     ncol(trainX),
                     "predictors remained for modeling.")
  } else varText <- ""
  
@ 

 \Sexpr{ppText} 
 \Sexpr{varText}

\clearpage

\section*{Model Building}

<<setupWorkers, eval = TRUE, echo = $SETUPWORKERSECHO, results = $SETUPWORKERSRESULT>>=
numWorkers <- $NUMWORKERS
##OPTION: turn up numWorkers to use MPI
if(numWorkers > 1)
  {
    mpiCalcs <- function(X, FUN, ...)
      {
        theDots <- list(...)
        parLapply(theDots$$cl, X, FUN)
      }

    library(snow)
    cl <- makeCluster(numWorkers, "MPI")
  }
@ 

<<setupResampling, echo = $SETUPRESAMPLINGECHO, results = $SETUPRESAMPLINGRESULT>>=
##OPTION: the resampling options can be changed. See
##        ?trainControl for details

resampName <- "$RESAMPNAME" 
resampNumber <- $RESAMPLENUMBER
numRepeat <- $NUMREPEAT
resampP <- $RESAMPLENUMBERPERCENT

modelInfo <- modelLookup(modName)

if(numClasses == 2)
  {
    foo <- if(any(modelInfo$$probModel)) twoClassSummary else twoClassNoProbs
  } else foo <- defaultSummary
   
set.seed(3)
ctlObj <- trainControl(method = resampName,
                       number = resampNumber,
                       repeats = numRepeat,
                       p = resampP,
                       classProbs = any(modelInfo$$probModel),
                       summaryFunction = foo)


##OPTION select other performance metrics as needed
optMetric <- if(numClasses == 2 & any(modelInfo$$probModel)) "ROC" else "Kappa"

if(numWorkers > 1)
  {
    ctlObj$$workers <- numWorkers
    ctlObj$$computeFunction <- mpiCalcs
    ctlObj$$computeArgs <- list(cl = cl)
  }
@ 

<<setupGrid, results = $SETUPGRIDRESULT, echo = $SETUPGRIDECHO>>=
##OPTION expand or contract these grids as needed (or
##       add more models

gridSize <- $SETUPGRIDSIZE

if(modName %in% c("svmPoly", "svmRadial", "svmLinear", "lvq", "ctree2", "ctree")) gridSize <- 5
if(modName %in% c("earth", "fda")) gridSize <- 7
if(modName %in% c("knn", "rocc", "glmboost", "rf", "nodeHarvest")) gridSize <- 10

if(modName %in% c("nb")) gridSize <- 2
if(modName %in% c("pam", "rpart")) gridSize <- 15
if(modName %in% c("pls")) gridSize <- min(20, ncol(trainX))

if(modName == "gbm")
  {
    tGrid <- expand.grid(.interaction.depth = -1 + (1:5)*2 ,
                         .n.trees = (1:10)*20,
                         .shrinkage = .1)
  }

if(modName == "nnet")
  {
    tGrid <- expand.grid(.size = -1 + (1:5)*2 ,
                         .decay = c(0, .001, .01, .1))
  }

if(modName == "ada")
 {
  tGrid <- expand.grid(.maxdepth = 1, .iter = c(100,200,300,400), .nu = 1 )

 }


@ 

<<fitModel, results = $FITMODELRESULT, echo = $FITMODELECHO, eval = $FITMODELEVAL>>=
##OPTION alter as needed

set.seed(4)
modelFit <- switch(modName,                  
                   gbm = 
                   {
                     mix <- sample(seq(along = trainY))  
                     train(
                           trainX[mix,], trainY[mix], modName, 
                           verbose = FALSE,
                           bag.fraction = .9, 
                           metric = optMetric,
                           trControl = ctlObj, 
                           tuneGrid = tGrid)
                   },
                   
                   multinom =
                   {
                     train(
                           trainX, trainY, modName, 
                           trace = FALSE, 
                           metric = optMetric,
                           maxiter = 1000, 
                           MaxNWts = 5000,
                           trControl = ctlObj, 
                           tuneLength = gridSize)   
                   },
                   
                   nnet =
                   {
                     train(
                           trainX, trainY, modName, 
                           metric = optMetric,
                           linout = FALSE,
                           trace = FALSE, 
                           maxiter = 1000, 
                           MaxNWts = 5000,
                           trControl = ctlObj, 
                           tuneGrid = tGrid)  
                     
                   }, 
                   
                   svmRadial =, svmPoly =, svmLinear = 
                   {
                     train(
                           trainX, trainY, modName,
                           metric = optMetric,
                           scaled = TRUE,
                           trControl = ctlObj, 
                           tuneLength = gridSize)    
                   },
                   {
                     train(trainX, trainY, modName, 
                           trControl = ctlObj, 
                           metric = optMetric,
                           tuneLength = gridSize)
                   })

@ 

<<modelDescr, echo = $MODELDESCRECHO, results = $MODELDESCRRESULT>>=
summaryText <- ""

resampleName <- switch(tolower(modelFit$$control$$method),
                       boot = paste("the bootstrap (", length(modelFit$$control$$index), " reps)", sep = ""),
                       boot632 = paste("the bootstrap 632 rule (", length(modelFit$$control$$index), " reps)", sep = ""),
                       cv = paste("cross-validation (", modelFit$$control$$number, " fold)", sep = ""),
                       repeatedcv = paste("cross-validation (", modelFit$$control$$number, " fold, repeated ",
                         modelFit$$control$$repeats, " times)", sep = ""),
                       lgocv = paste("repeated train/test splits (", length(modelFit$$control$$index), " reps, ",
                         round(modelFit$$control$$p, 2), "$$\\%$$)", sep = ""))

tuneVars <- latexTranslate(tolower(modelInfo$$label))
tuneVars <- gsub("\\#", "the number of ", tuneVars, fixed = TRUE)
if(ncol(modelFit$$bestTune) == 1 && colnames(modelFit$$bestTune) == ".parameter")
  {
    summaryText <- paste(summaryText,
                         "\n\n",
                         "There are no tuning parameters associated with this model.",
                         "To characterize the model performance on the training set,",
                         resampleName,
                         "was used.",
                         "Table \\\\ref{T:resamps} and Figure \\\\ref{F:profile}",
                         "show summaries of the resampling results. ")

  } else {
    summaryText <- paste("There",
                         ifelse(nrow(modelInfo) > 1, "are", "is"),
                         nrow(modelInfo),
                         ifelse(nrow(modelInfo) > 1, "tuning parameters", "tuning parameter"),
                         "associated with this model:",
                         listString(tuneVars, period = TRUE))



    paramNames <- gsub(".", "", names(modelFit$$bestTune), fixed = TRUE)
    ## (i in seq(along = paramNames))
    ##  {
     ##   check <- modelInfo$$parameter %in% paramNames[i]
     ##   if(any(check))
     ##     {
     ##       paramNames[i] <- modelInfo$$label[which(check)]          
     ##     }
     ## }

    paramNames <- gsub("#", "the number of ", paramNames, fixed = TRUE)
    ## Check to see if there was only one combination fit
    summaryText <- paste(summaryText,
                         "To choose",
                         ifelse(nrow(modelInfo) > 1,
                                "appropriate values of the tuning parameters,",
                                "an appropriate value of the tuning parameter,"),
                         resampleName,
                         "was used to generated a profile of performance across the",
                         nrow(modelFit$$results),
                         ifelse(nrow(modelInfo) > 1,
                                "combinations of the tuning parameters.",
                                "candidate values."),
                         
                         "Table \\\\ref{T:resamps} and Figure \\\\ref{F:profile} show",
                         "summaries of the resampling profile. ",                                                                                         "The final model fitted to the entire training set was:",
                         listString(paste(latexTranslate(tolower(paramNames)), "=", modelFit$$bestTune[1,]), period = TRUE))

  }
@ 

\Sexpr{summaryText}

<<resampTable, echo = $RESAMPTABLEECHO, results = $RESAMPTABLERESULT>>=
tableData <- modelFit$$results

if(all(modelInfo$$parameter == "parameter") && resampName == "boot632")
    {
    tableData <- tableData[,-1, drop = FALSE]
    colNums <- c( length(modelFit$$perfNames), length(modelFit$$perfNames), length(modelFit$$perfNames))
    colLabels <- c("Mean", "Standard Deviation","Apparant")
    constString <- ""
    isConst <- NULL
    } else if (all(modelInfo$$parameter == "parameter") && (resampName == "boot" | resampName == "cv" | resampName == "repeatedcv" )){
    tableData <- tableData[,-1, drop = FALSE]
    colNums <- c(length(modelFit$$perfNames), length(modelFit$$perfNames))
    colLabels <- c("Mean", "Standard Deviation")
    constString <- ""
    isConst <- NULL
    } else if (all(modelInfo$$parameter == "parameter") && resampName == "LOOCV" ){
   tableData <- tableData[,-1, drop = FALSE]
   colNums <- length(modelFit$$perfNames)
   colLabels <- c("Measures")
   constString <- ""
   isConst <- NULL
} else  {
 if (all(modelInfo$$parameter != "parameter") && resampName == "boot632" ){
 isConst <- apply(tableData[, modelInfo$$parameter, drop = FALSE],
                     2, 
                     function(x) length(unique(x)) == 1)

    numParamInTable <- sum(!isConst)

    if(any(isConst))
      {
        constParam <- modelInfo$$parameter[isConst]
        constValues <- format(tableData[, constParam, drop = FALSE], digits = 4)[1,,drop = FALSE]
        tableData <- tableData[, !(names(tableData) %in% constParam), drop = FALSE]
        constString <- paste("The tuning",
                             ifelse(sum(isConst) > 1,
                                    "parmeters",
                                    "parameter"),
                             listString(paste("``", names(constValues), "''", sep = "")),
                             ifelse(sum(isConst) > 1,
                                    "were",
                                    "was"),
                             "held constant at",
                             ifelse(sum(isConst) > 1,
                                    "a value of",
                                    "values of"),
                             listString(constValues[1,]))
        
      } else constString <- ""

    cn <- colnames(tableData)
    ## for(i in seq(along = cn))
    ##  {
    ##    check <- modelInfo$$parameter %in% cn[i]
    ##    if(any(check))
    ##      {
    ##        cn[i] <- modelInfo$$label[which(check)]          
    ##     }
    ##  }
    ## colnames(tableData) <- cn

    colNums <- c(numParamInTable, 
                 length(modelFit$$perfNames),
                 length(modelFit$$perfNames),
                 length(modelFit$$perfNames))
    colLabels <- c("", "Mean", "Standard Deviation", "Apparant")

}else if (all(modelInfo$$parameter != "parameter") && (resampName == "boot" | resampName == "repeatedcv" | resampName == "cv") ){
 isConst <- apply(tableData[, modelInfo$$parameter, drop = FALSE],
                     2,
                     function(x) length(unique(x)) == 1)

    numParamInTable <- sum(!isConst)

    if(any(isConst))
      {
        constParam <- modelInfo$$parameter[isConst]
        constValues <- format(tableData[, constParam, drop = FALSE], digits = 4)[1,,drop = FALSE]
        tableData <- tableData[, !(names(tableData) %in% constParam), drop = FALSE]
        constString <- paste("The tuning",
                             ifelse(sum(isConst) > 1,
                                    "parmeters",
                                    "parameter"),
                             listString(paste("``", names(constValues), "''", sep = "")),
                             ifelse(sum(isConst) > 1,
                                    "were",
                                    "was"),
                             "held constant at",
                             ifelse(sum(isConst) > 1,
                                    "a value of",
                                    "values of"),
                             listString(constValues[1,]))

      } else constString <- ""

    cn <- colnames(tableData)
    ## for(i in seq(along = cn))
    ##  {
    ##    check <- modelInfo$$parameter %in% cn[i]
    ##    if(any(check))
    ##      {
    ##        cn[i] <- modelInfo$$label[which(check)]
    ##      }
    ##  }
    ## colnames(tableData) <- cn

    colNums <- c(numParamInTable,
                 length(modelFit$$perfNames),
                  length(modelFit$$perfNames))
    colLabels <- c("", "Mean", "Standard Deviation")

}
else if (all(modelInfo$$parameter != "parameter") && resampName == "LOOCV"){
 isConst <- apply(tableData[, modelInfo$$parameter, drop = FALSE],
                     2,
                     function(x) length(unique(x)) == 1)

    numParamInTable <- sum(!isConst)

    if(any(isConst))
      {
        constParam <- modelInfo$$parameter[isConst]
        constValues <- format(tableData[, constParam, drop = FALSE], digits = 4)[1,,drop = FALSE]
        tableData <- tableData[, !(names(tableData) %in% constParam), drop = FALSE]
        constString <- paste("The tuning",
                             ifelse(sum(isConst) > 1,
                                    "parmeters",
                                    "parameter"),
                             listString(paste("``", names(constValues), "''", sep = "")),
                             ifelse(sum(isConst) > 1,
                                    "were",
                                    "was"),
                             "held constant at",
                             ifelse(sum(isConst) > 1,
                                    "a value of",
                                    "values of"),
                             listString(constValues[1,]))

      } else constString <- ""

    cn <- colnames(tableData)
##    for(i in seq(along = cn))
##      {
##        check <- modelInfo$$parameter %in% cn[i]
##        if(any(check))
##          {
##            cn[i] <- modelInfo$$label[which(check)]
##          }
##      }
##    colnames(tableData) <- cn

    colNums <- c(numParamInTable,
                  length(modelFit$$perfNames))
    colLabels <- c("", "Measures")

}

}



colnames(tableData) <- gsub("SD$$", "", colnames(tableData))
colnames(tableData) <- gsub("Apparent$$", "", colnames(tableData))
colnames(tableData) <- latexTranslate(colnames(tableData))
rownames(tableData) <- latexTranslate(rownames(tableData))

latex(tableData,
      rowname = NULL,
      file = "",
      cgroup = colLabels,
      n.cgroup = colNums,
      where = "h!",
      digits = 4,
      longtable = nrow(tableData) > 30,
      caption = paste(resampleName, "results from the model fit.", constString),
      label = "T:resamps")
@ 

  \setkeys{Gin}{ width = 0.9\textwidth}
  \begin{figure}[b]
  \begin{center}

<<profilePlot, echo = $PROFILEPLOTECHO, fig = $PROFILEPLOTFIG, width = 8, height = 6>>=
  trellis.par.set(caretTheme(), warn = TRUE)
if(all(modelInfo$$parameter == "parameter") | all(isConst) | modName == "nb")
  {
    resultsPlot <- resampleHist(modelFit)
    plotCaption <- paste("Distributions of model performance from the ",
                         "training set estimated using ",
                         resampleName)
  } else {
    if(modName %in% c("svmPoly", "svmRadial", "svmLinear"))
      {
        resultsPlot <- plot(modelFit, 
                            metric = optMetric,                          
                            xTrans = function(x) log10(x))
        resultsPlot <- update(resultsPlot,
                              type = c("g", "p", "l"),
                              ylab = paste(optMetric, " (", resampleName, ")", sep = ""))

      } else {
        resultsPlot <- plot(modelFit,                         
                            metric = optMetric) 
        resultsPlot <- update(resultsPlot,
                              type = c("g", "p", "l"),
                              ylab = paste(optMetric, " (", resampleName, ")", sep = ""))     
      }  
   plotCaption <- paste("A plot of the estimates of the",
                        optMetric,
                        "values calculated using",
                        resampleName)
  }
print(resultsPlot)
@ 
   \caption[Performance Plot]{\Sexpr{plotCaption}.}
    \label{F:profile}         
  \end{center}
 \end{figure}  


<<stopWorkers, echo = $STOPWORKERSECHO, results = $STOPWORKERSRESULT>>=
if(numWorkers > 1) stopCluster(cl)
@ 

<<testPred, results = $TESTPREDRESULT, echo = $TESTPREDECHO>>=
  if((!is.null(testX)) && (!is.null(testY))){
   save(trainX,trainY,testX,testY,file="datasets.RData")
   } else {
         save(trainX,trainY,file="datasets.RData")
          }
	  
  if(pctTrain < 1) 
  {
    cat("\\clearpage\n\\section*{Test Set Results}\n\n")
    classPred <- predict(modelFit, testX)
    cm <- confusionMatrix(classPred, testY)
    values <- cm$$overall[c("Accuracy", "Kappa", "AccuracyPValue", "McnemarPValue")]
    
    values <- values[!is.na(values) & !is.nan(values)]
    values <- c(format(values[1:2], digits = 3),
                format.pval(values[-(1:2)], digits = 5))
    nms <- c("the overall accuracy", "the Kappa statistic", 
                       "the $$p$$--value that accuracy is greater than the no--information rate",
                       "the $$p$$--value of concordance from McNemar's test")
    nms <- nms[seq(along = values)]
    names(values) <- nms
    
    if(any(modelInfo$$probModel))
      {
        classProbs <- extractProb(list(fit = modelFit), 
                                  testX = testX,
                                  testY = testY)
        classProbs <- subset(classProbs, dataType == "Test")  
        if(numClasses == 2)
          {
            tmp <- twoClassSummary(classProbs, lev = levels(classProbs$$obs))
            tmp <- c(format(tmp, digits = 3))
           names(tmp) <- c("the area under the ROC curve", "the sensitivity", "the specificity")
      
            values <- c(values, tmp)
            
          }
        probPlot <- plotClassProbs(classProbs)
      }
    testString <- paste("Based on the test set of",
                        nrow(testX),
                        "samples,",
                        listString(paste(names(values), "was", values), period = TRUE),
                        "The confusion matrix for the test set is shown in Table",
                        "\\\\ref{T:cm}.")
    testString <- paste(testString,
                        " Using ", resampleName,
                        ", the training set estimates were ",
                        resampleStats(modelFit),
                        ".", 
                        sep = "")
    
    if(any(modelInfo$$probModel)) testString <- paste(testString,
                                                     "Histograms of the class probabilities",
                                                     "for the test set samples are shown in",
                                                     "Figure \\\\ref{F:probs}",
                                                     ifelse(numClasses == 2,
                                                            " and the test set ROC curve is in Figure \\\\ref{F:roc}.",
                                                            "."))
    
    
    
    latex(cm$$table,
          title = "",
          file = "",
          where = "h",
          cgroup = "Observed Values",
          n.cgroup = numClasses,
          caption = "The confusion matrix for the test set",
          label = "T:cm")
    
  } else testString <- ""
@ 
\Sexpr{testString}


<<classProbsTex, results = $CLASSPROBSTEXRESULT, echo = $CLASSPROBSTEXECHO>>=
 if(any(modelInfo$probModel) && pctTrain < 1 )  {
    cat(
        paste("\\begin{figure}[p]\n",
              "\\begin{center}\n",
              "\\includegraphics{classProbs}",
              "\\caption[PCA Plot]{Class probabilities",
              "for the test set. Each panel contains ",
            "separate classes}\n",
              "\\label{F:probs}\n",
              "\\end{center}\n",
              "\\end{figure}"))
  }
  if(any(modelInfo$$probModel) & numClasses == 2  & pctTrain < 1 )
  {
    cat(
        paste("\\begin{figure}[p]\n",
              "\\begin{center}\n",
              "\\includegraphics[clip, width = .8\\textwidth]{roc}",
              "\\caption[ROC Plot]{ROC Curve",
              "for the test set.}\n",
              "\\label{F:roc}\n",
              "\\end{center}\n",
              "\\end{figure}"))
  } else {
cat (paste(""))
}

@ 
<<classProbsTex, results = $CLASSPROBSTEXRESULT1, echo = $CLASSPROBSTEXECHO1 >>=
 if(any(modelInfo$probModel) && pctTrain < 1)  {
    pdf("classProbs.pdf", height = 7, width = 7)
    trellis.par.set(caretTheme(), warn = FALSE)
    print(probPlot)
    dev.off()
  }
 if(any(modelInfo$probModel) & numClasses == 2 & pctTrain < 1) { 
    resPonse<-testY
    preDictor<-classProbs[, levels(trainY)[1]]
    pdf("roc.pdf", height = 8, width = 8)
# from pROC example at http://web.expasy.org/pROC/screenshots.htm   
    plot.roc(resPonse, preDictor, # data
         percent=TRUE, # show all values in percent
         partial.auc=c(100, 90), partial.auc.correct=TRUE, # define a partial AUC (pAUC)
         print.auc=TRUE, #display pAUC value on the plot with following options:
         print.auc.pattern="Corrected pAUC (100-90%% SP):\n%.1f%%", print.auc.col="#1c61b6",
         auc.polygon=TRUE, auc.polygon.col="#1c61b6", # show pAUC as a polygon
         max.auc.polygon=TRUE,     max.auc.polygon.col="#1c61b622", # also show the 100% polygon
         main="Partial AUC (pAUC)")
    plot.roc(resPonse, preDictor,
         percent=TRUE, add=TRUE, type="n", # add to plot, but don't re-add the ROC itself (useless)
         partial.auc=c(100, 90), partial.auc.correct=TRUE,
         partial.auc.focus="se", # focus pAUC on the sensitivity
         print.auc=TRUE, print.auc.pattern="Corrected pAUC (100-90%% SE):\n%.1f%%", print.auc.col="#008600",
         print.auc.y=40, # do not print auc over the previous one
         auc.polygon=TRUE, auc.polygon.col="#008600",
         max.auc.polygon=TRUE, max.auc.polygon.col="#00860022")
    dev.off()
  } else {
cat("")
  }

@ 

\section*{Versions}

<<versions, echo = FALSE, results = tex>>=
toLatex(sessionInfo())

@ 

<<save-data, echo = $SAVEDATAECHO, results = $SAVEDATARESULT>>=
## change this to the name of modName....
Fit <- modelFit
if(exists('ppInfo') && !is.null(ppInfo)){
save(Fit,ppInfo,cm,file="$METHOD-Fit.RData")
} else {save(Fit,cm,file="$METHOD-Fit.RData")}

@
The model was built using $METHOD and is saved as $METHOD Model for reuse. This contains the variable Fit.

\end{document}'''

	return template4Rnw
