library(splitstackshape)
library(plyr)
library(vioplot)

toPdf <- FALSE
fileName <- "distribution_over_ensembles.pdf"

#to read the files the path of the folder Experiments has to be written in place of "."

resultsM<-read.table(file="./Experiments/UrbanMobilitySystem/resultsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatmentsM<-read.table(file="./Experiments/UrbanMobilitySystem/treatmentsMobility.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")

resultsD<-read.table(file="./Experiments/SurveillanceSystem/resultsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")
treatmentsD<-read.table(file="./Experiments/SurveillanceSystem/treatmentsDrones.csv", header=TRUE, sep=",", na.strings = "NA", dec=".")


universeM <- merge(treatmentsM, resultsM,by="id")
universeM$iv1 <- universeM$iv1 
universeM$dv6 <- universeM$dv6 	 # numbers of involved ensembles in the UMS scenario

universeD <- merge(treatmentsD, resultsD,by="id")
universeD$iv1 <- universeD$iv1 
universeD$dv6 <- universeD$dv6 	 # numbers of involved ensembles in the UAV scenario


if(toPdf) {
  setupPdf()
}

distribution_over_ensembles<- function() {
	
	#from the data the number of involved ensembles are 1, 2, 3, 4

	x1 <- universeM$iv1[universeM$dv6==1]
	x2 <- universeM$iv1[universeM$dv6==2]
	x3 <- universeM$iv1[universeM$dv6==3]
	x4 <- universeM$iv1[universeM$dv6==4]

	x1D <- universeD$iv1[universeD$dv6==1]
	x2D <- universeD$iv1[universeD$dv6==2]
	x3D <- universeD$iv1[universeD$dv6==3]
	x4D <- universeD$iv1[universeD$dv6==4]

	
	y1 <- length(x1)
	y2 <- length(x2)
	y3 <- length(x3)
	y4 <- length(x4)

	y1D <- length(x1D)
	y2D <- length(x2D)
	y3D <- length(x3D)
	y4D <- length(x4D)


	value <- c(y1, y2, y3, y4)

	valueD <- c(y1D, y2D, y3D, y4D)

	values <- c(y1, y1D, y2, y2D, y3, y3D, y4, y4D)

	leg.txt <- c("Urban Mobility System (U)", "Surveillance System (S)")
	
	z <- barplot(values,
			names.arg = c("1U ", "1S ", "2U ", "2S ", "3U ", "3S ", "4U ", "4S "), col=c("lightblue","orange"), axes = TRUE, ylim = c(0, 450),
	        xpd = FALSE, ylab = "Number of treatments", xlab = "Number of ensembles", main = "Distribution over ensembles of the collective issue resolutions. " )
  	text(z, values, values, pos = 3, cex = 1, col = "red")
	legend("topleft", legend=leg.txt, fill=c("lightblue", "orange"))

}


distribution_over_ensembles()

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
