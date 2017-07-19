args <- commandArgs(TRUE)


res1 <- read.csv(args[1])
#res11 <- duplicated(res1[,1:length(res1) - 1])
res11 <- duplicated(res1[,1])
result <- res1[!res11,]
names1 <- result[,1]
rownames(result) <- names1
result$Name <- NULL
write.csv(result,file=args[2])
