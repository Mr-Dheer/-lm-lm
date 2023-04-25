Probability = .50
Size = seq(100,1000,100)

chap3a = function(Probability,Size)  {

# R Infinite Probability Function
# Change Probability value for different true probability
# Change Size for different sample size (default is 100 to 1000 by 100)

# Input Probability and Size values

# Process
SampleFreqs = NULL
for (SampleSize in Size) {
SampleFreqs = c(SampleFreqs,sum(sample(0:1,prob=c((1-Probability),Probability),size=SampleSize, replace=T))/SampleSize)
}

# Output
plot(Size,SampleFreqs,type="l",ylim=c(Probability-.1,Probability+.1),xlab="Sample Size",ylab="Proportion", 
main="Sample Proportion by Sample Size")
#
SampleMatrix = NULL
for (SampleFreq in SampleFreqs) {
        SampleMatrix = rbind(SampleMatrix,c(round(SampleFreq,digits=3),format(Probability,digits=3),
round(SampleFreq-Probability,digits=3)))
}
dimnames(SampleMatrix) = list(paste("sample size =",Size),c("Sample %","Population %","Error"))
out = data.frame(SampleMatrix)
names(out) = cbind("Sample %","Pop %","Error")

cat("\n")
cat("\n")
cat("PROGRAM OUTPUT","\n")
cat("\n")
#print(SampleMatrix)
print(out)
}

chap3a(Probability,Size)
