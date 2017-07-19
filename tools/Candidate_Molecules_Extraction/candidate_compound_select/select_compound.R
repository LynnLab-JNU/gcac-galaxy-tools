args <- commandArgs(T)
arg1 <- args[1] ## Reae Input prediction file
arg2 <- args[2] ## Less Than  
arg3 <- args[3] ## Greater Than or equal too
arg4 <- args[4] ## Active / Inactive
arg5 <- args[5] ## define output file name
asd <- read.table(arg1,row.names=1,header=T)
if (arg4 == "Active") {
refined <- asd[asd[,1] >= as.numeric(arg3) & asd[,1] <= as.numeric(arg2),] 
compound  <- rownames(refined)
refined <- cbind(compound,refined)
} else if((arg4 == "Inactive") ){
#refined <- asd[asd[,1] <= as.numeric(arg2),] 
refined <- asd[asd[,2] >= as.numeric(arg3) & asd[,2] <= as.numeric(arg2),]
compound  <- rownames(refined)
refined <- cbind(compound,refined)}
###write.table(dw,file=args3,row.names=FALSE,sep="\t")
write.table(refined,file=arg5,row.names=FALSE,sep="\t")

