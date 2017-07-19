args <- commandArgs(TRUE)

csv2rdatatrain <- function(arg1,arg2)
{
  file <- read.csv(arg1,row.names =1, header=T)
  col <- ncol(file)
  stopifnot(is.null(file) | col > 2 )

  #cat("the Outcome column is not a factor vector.\n",file=stderr())
  stopifnot(is.factor(file[,col]))

  if(levels(file[,col])[1] != ""){
    dataX <- file[,1:(col-1)]
    dataY <- file[,col]
    stopifnot(nrow(dataX) == length(dataY))
    save(dataX,dataY,file=arg2)
  }
  else{
     cat("the Outcome column has less number of entry than number of compounds.please check input file.\n",file=stderr())
  }
  }

csv2rdatatrain(args[1],args[2])





