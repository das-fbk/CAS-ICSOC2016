library(splitstackshape)
library(plyr)
library(vioplot)

toPdf <- FALSE
fileName <- "exec_time_per_roles.pdf"

#to read the files the path of the folder Experiments has to be written in place of "."

results2<-read.table(file="./Experiments/SurveillanceSystem/resultsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatments2<-read.table(file="./Experiments/SurveillanceSystem/treatmentsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")

universe <- merge(treatments2, results2,by="id")
universe$dv1 <- universe$dv1 / 1000000 # in seconds
universe$dv7 <- universe$dv7 	 # numbers of involved roles

if(toPdf) {
  setupPdf()
}

exec_time_per_roles <- function() {

	#here we consider only the treatments of 500 issues, 
	#and and only the roles involved in this subset of treatments (7, 8, 9, 10, 11)

	x2 <- universe$dv1[universe$dv7==7 & universe$iv1==500]
	x3 <- universe$dv1[universe$dv7==8 & universe$iv1==500]
	x4 <- universe$dv1[universe$dv7==9 & universe$iv1==500]
	x5 <- universe$dv1[universe$dv7==10 & universe$iv1==500]
	x6 <- universe$dv1[universe$dv7==11 & universe$iv1==500]

	y2 <- length(x2)
	y3 <- length(x3)
	y4 <- length(x4)
	y5 <- length(x5)
	y6 <- length(x6)
	
	value <- c(round(mean(x2), 2), round(mean(x3), 2), round(mean(x4), 2), round(mean(x5), 2), round(mean(x6), 2))

	 p <- boxplot(x2, x3, x4, x5, x6,  names=c("7", "8", "9", "10", "11"), col="grey", drawRect=TRUE, wex=0.5)
       text(y=c(13500,13700,13900,14100,14500), labels=c(round(mean(x2), 2), round(mean(x3), 2), round(mean(x4), 2), round(mean(x5), 2), round(mean(x6), 2)), x = c(1,2,3,4,5), col="red")
       title("Execution time per number of involved roles (in milliseconds)")

}

exec_time_per_roles()

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
