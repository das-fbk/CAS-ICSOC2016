library(splitstackshape)
library(plyr)
library(vioplot)

toPdf <- FALSE
fileName <- "distribution_over_roles.pdf"

#to read the files the path of the folder Experiments has to be written in place of "."

resultsM<-read.table(file="./Experiments/UrbanMobilitySystem/resultsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatmentsM<-read.table(file="./Experiments/UrbanMobilitySystem/treatmentsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")

resultsD<-read.table(file="./Experiments/SurveillanceSystem/resultsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatmentsD<-read.table(file="./Experiments/SurveillanceSystem/treatmentsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")

universeM <- merge(treatmentsM, resultsM,by="id")
universeM$iv1 <- universeM$iv1 
universeM$dv7 <- universeM$dv7 	 # numbers of involved roles in the UMS scenario

universeD <- merge(treatmentsD, resultsD,by="id")
universeD$iv1 <- universeD$iv1 
universeD$dv7 <- universeD$dv7 	 # numbers of involved roles in the UAV scenario


if(toPdf) {
  setupPdf()
}

distribution_over_roles <- function() {

	#from the data, the number of involved roles are in the range between 3 and 13 (dv7 in the file results)

	x1 <- universeM$iv1[universeM$dv7==3]
	x2 <- universeM$iv1[universeM$dv7==4]
	x3 <- universeM$iv1[universeM$dv7==5]
	x4 <- universeM$iv1[universeM$dv7==6]
	x5 <- universeM$iv1[universeM$dv7==7]
	x6 <- universeM$iv1[universeM$dv7==8]
	x7 <- universeM$iv1[universeM$dv7==9]
	x8 <- universeM$iv1[universeM$dv7==10]
	x9 <- universeM$iv1[universeM$dv7==11]
	x10 <- universeM$iv1[universeM$dv7==12]
	x11 <- universeM$iv1[universeM$dv7==13]

	x1D <- universeD$iv1[universeD$dv7==3]
	x2D <- universeD$iv1[universeD$dv7==4]
	x3D <- universeD$iv1[universeD$dv7==5]
	x4D <- universeD$iv1[universeD$dv7==6]
	x5D <- universeD$iv1[universeD$dv7==7]
	x6D <- universeD$iv1[universeD$dv7==8]
	x7D <- universeD$iv1[universeD$dv7==9]
	x8D <- universeD$iv1[universeD$dv7==10]
	x9D <- universeD$iv1[universeD$dv7==11]
	x10D <- universeD$iv1[universeD$dv7==12]
	x11D <- universeD$iv1[universeD$dv7==13]


	y1 <- length(x1)
	y2 <- length(x2)
	y3 <- length(x3)
	y4 <- length(x4)
	y5 <- length(x5)
	y6 <- length(x6)
	y7 <- length(x7)
	y8 <- length(x8)
	y9 <- length(x9)
	y10 <- length(x10)
	y11 <- length(x11)

	y1D <- length(x1D)
	y2D <- length(x2D)
	y3D <- length(x3D)
	y4D <- length(x4D)
	y5D <- length(x5D)
	y6D <- length(x6D)
	y7D <- length(x7D)
	y8D <- length(x8D)
	y9D <- length(x9D)
	y10D <- length(x10D)
	y11D <- length(x11D)

	array_num_roles <- c(y1, y1D, y2, y2D, y3, y3D, y4, y4D, y5, y5D, y6, y6D, y7, y7D, y8, y8D, y9, y9D, y10, y10D, y11, y11D)

	leg.txt <- c("Urban Mobility System", "Surveillance System")
	
	z <- barplot(array_num_roles , 
			names.arg = c("3", "", "4", "", "5", "", "6", "", "7", "", "8", "", "9", "", "10", "", "11", "", "12", "", "13", ""), col=c("lightblue", "orange"), axes = TRUE, ylim = c(0, 200),
	         xpd = FALSE, ylab = "Number of treatments", xlab = "Number of involved roles", main = "Distribution over roles of the collective issue resolutions. ")
  	text(z, array_num_ruoli, array_num_ruoli, pos = 3, cex = 1, col = "red")
	lines(density(array_num_roles ), col=c("blue", "red"))
	legend("topright", legend=leg.txt, fill=c("lightblue", "orange"))
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
