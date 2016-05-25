library(splitstackshape)
library(plyr)
library(vioplot)

toPdf <- FALSE
fileName <- "exec_time_issues.pdf"

#to read the files the path of the folder Experiments has to be written in place of "."

results2<-read.table(file="./Experiments/UrbanMobilitySystem/resultsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatments2<-read.table(file="./Experiments/UrbanMobilitySystem/treatmentsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")

universe <- merge(treatments2, results2,by="id")
universe$dv1 <- universe$dv1 / 1000000 # in seconds
universe$dv2 <- universe$dv2 / (1024 * 1024) # in Megabyte

if(toPdf) {
  setupPdf()
}

exec_time_issues <- function() {
  x1 <- universe$dv1[universe$iv1==1]
  x2 <- universe$dv1[universe$iv1==250]
  x3 <- universe$dv1[universe$iv1==500]
  x4 <- universe$dv1[universe$iv1==750]
  x5 <- universe$dv1[universe$iv1==1000]

  p <- boxplot(x1, x2, x3, x4, x5, names=c("1", "250", "500", "750", "1000"), col="grey", drawRect=TRUE, wex=0.5)
  text(y=c(1100,8000,17600,27000,36600), labels=c(round(mean(x1), 2), round(mean(x2), 2), round(mean(x3), 2), round(mean(x4), 2), round(mean(x5), 2)), x = c(1,2,3,4,5), col="red")
  title("Execution time per number of raised issues (in milliseconds)")
}

exec_time_issues()

if(toPdf) {
  dev.off()
}

setupPdf <- function() {
  margin <- 4
  pdf(fileName)
  par(mar=c(margin, margin, margin, margin))
  par(mfrow=c(1, 1))
  par(las=1)
}
