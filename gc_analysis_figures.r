###################################################################################
########################  2016-2017 SIBERIA SHRUB SAPFLOW  ########################
########################  Low Density and German Site      ########################
###################################################################################
######################## extraction of heat balance sensor ########################
######################## measurements and the calculation  ########################
######################## of sap flow for measurements at   ########################
######################## in a floodplain (german site)     ########################
######################## and a low density upland forest   ########################
###################################################################################
######################## R version 3.4.3                   ######################## 
###################################################################################
######################## outputs:                          ########################
######################## sensor (list) sensor info         ########################
######################## sapFlow (list) sapflow g m-2 s-1  ########################
######################## specFlow (list) species averaged  ########################
########################     sapflow g m-2 s-1             ########################
######################## specTday (list) species averaged  ########################
########################     total daily transpiration     ########################
########################     L m-2 s-1                     ########################
######################## gcSpec (list) species averaged    ########################
########################     stomatal conductance          ########################
########################     mol m-2 s-1                   ########################
###################################################################################


##################################
# read in data                   #
##################################
source("/Users/hkropp/Documents/GitHub/ch_shrub_sap/gc_analysis_script.r")


##################################
# set up directories             #
##################################
plotDir <- "/Users/hkropp/Library/CloudStorage/GoogleDrive-hkropp@hamilton.edu/My Drive/research/projects/shrub_sapflow/manuscript/figures"



##################################
# Vegetation colors for plotting #
##################################

coli <- c(rgb(0,114,178, maxColorValue = 255), #alnus floodplain
			rgb(213,94,0, maxColorValue = 255), #salix floodplain
			rgb(0,158,115, maxColorValue = 255), #betula upland
			rgb(230,159,0, maxColorValue = 255)) #salix upland
			
			
colDF <- data.frame(spsID= seq(1,4), 
		species=c("Alnus","Salix","Betula","Salix"), 
		siteid=c(1,1,2,2),
		coli=coli,
		col2=c(rgb(0,114,178,127, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,127, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,127, maxColorValue = 255), #betula upland
				rgb(230,159,0,127, maxColorValue = 255)), #salix upland
		col3=c(rgb(0,114,178,50, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,50, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,50, maxColorValue = 255), #betula upland
				rgb(230,159,0,50, maxColorValue = 255)), #salix upland	

		col4=c(rgb(0,114,178,80, maxColorValue = 255), #alnus floodplain
				rgb(213,94,0,80, maxColorValue = 255), #salix floodplain
				rgb(0,158,115,80, maxColorValue = 255), #betula upland
				rgb(230,159,0,80, maxColorValue = 255)) #salix upland	
				
		)

##################################
# plots                          #
##################################

#################################################################
################## Daily Transpiration   ########################
#################################################################
metDaily$Prday_cm <- metDaily$Prday/10

wd <- 45
hd <- 10
xl <- 180
xh <- 240.5
yl2 <- 0
yh2 <- 5

yl <- 0
yh <- 25
prMax <- 35
prScale <- yh/prMax
Dscale <- yh/3.5

xseq <- seq(180, 240, by=5)
yseq <- seq(0,25, by=5)
yseq2 <- seq(0,35, by=5)*prScale
yseq3 <- seq(0,5, by=1)
#tick width
tlw <- 3
#axis tick label size
alc <- 2
#axis width 
alw <- 2
#axis label size
llc <- 3
#point size
pcx <- 3

png(paste0(plotDir,"/Tday.png"), width = 59, height = 28, units = "cm", res=300)
	layout(matrix(c(1,2),ncol=1), width=lcm(wd),height=rep(lcm(hd),2))
	par(mai=c(0.25,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)
	#add precip
	for(i in 1:nrow(metDaily)){
		polygon(c(metDaily$doy[i]-0.25,metDaily$doy[i]-0.25,metDaily$doy[i]+0.25,metDaily$doy[i]+0.25),
			c(0,metDaily$Prday_cm[i]*prScale,metDaily$Prday_cm[i]*prScale,0), border=NA, col=rgb(115,194,251,100,maxColorValue=255))
	}	
	#air temp
	points(metDaily$doy[metDaily$siteid == 2], metDaily$aveTemp[metDaily$siteid == 2], pch=19, type="b", cex=pcx)
	points(metDaily$doy[metDaily$siteid == 1], metDaily$aveVPD[metDaily$siteid == 1]*Dscale, pch=19, type="b", col=rgb(.74,.74,.74,.75), cex=pcx)
	points(metDaily$doy[metDaily$siteid == 2], metDaily$aveVPD[metDaily$siteid == 2]*Dscale, pch=19, type="b", col=rgb(.82,.7,.54,.75), cex=pcx)
	axis(1, xseq, rep(" ", length(xseq)), lwd.ticks=tlw, lwd=alw)
	axis(2, yseq, rep(" ", length(yseq)), lwd.ticks=tlw, lwd=alw)
	axis(4, yseq2, rep(" ", length(yseq2)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq, at=yseq, side=2, line=2, cex=alc, las=2)
	mtext(seq(0,35, by=5)/10, at=yseq2, side=4, line=2, cex=alc, las=2)
	mtext(expression(paste("Air temperature")), side=2, line=9, cex=llc)
	mtext(expression(paste("(",degree,"C)")), side=2, line=5, cex=llc)	
	mtext("Precipitation", side=4, line=6, cex=llc)
	mtext("Vapor pressure deficit (VPD)", side=4, line=9, cex=llc)
	mtext("(cm, kPa)", side=4, line=12, cex=llc)
	legend("topleft", c("Temperature upland", "Precipitation"), pch=c(19, 15), 
		col=c("black",rgb(115,194,251,100,maxColorValue=255)), bty="n", cex=2)
	legend("topright", c( "VPD floodplain","VPD upland"), pch=c(19,19), 
		col=c(col=rgb(.74,.74,.74,.75), col=rgb(.82,.7,.54,.75)), bty="n", cex=2)
	par(mai=c(0,0,0.25,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl2,yh2), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)
	polygon(c(189.75,189.75,190.25,190.25), c(0,5,5,0), border=NA, col=rgb(237,237,237, maxColorValue=255))
	polygon(c(221.75,221.75,222.25,222.25), c(0,5,5,0), border=NA, col=rgb(237,237,237, maxColorValue=255))
	polygon(c(229.75,229.75,230.25,230.25), c(0,5,5,0), border=NA, col=rgb(237,237,237, maxColorValue=255))
	for(i in 1:4){	
		points(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i], pch=19, col=coli[i],
			type="b", cex=pcx)
		arrows(tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]-tdayDF$L.m2.daySD[tdayDF$spsID==i],
		tdayDF$doy[tdayDF$spsID==i],tdayDF$L.m2.day[tdayDF$spsID==i]+tdayDF$L.m2.daySD[tdayDF$spsID==i],code=0, col=rgb(0.5,0.5,0.5,0.5),lwd=3)
	}
	axis(1, xseq, rep(" ", length(xseq)), lwd.ticks=tlw, lwd=alw)
	mtext(xseq, at=xseq, side=1, line=2, cex=alc)
	axis(2, yseq3, rep(" ", length(yseq3)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq3, at=yseq3, side=2, line=2, cex=alc, las=2)
	mtext(expression(paste("Transpiration ")), side=2, line=9, cex=llc)
	mtext(expression(paste("(L m"^"-2","day"^"-1",")")), side=2, line=5, cex=llc)
	mtext("Day of Year", side=1, line=5, cex=llc)
	legend("topleft", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix))), pch=19, col=c(coli[1],coli[2]), bty="n", cex=2)
	legend("topright", c(expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))), pch=19, col=c(coli[3],coli[4]), bty="n", cex=2)
dev.off()	
	
	
	
		
	
#################################################################
################## Whole plant T         ########################
#################################################################	

tdayDF$jitteri <- runif(nrow(tdayDF),0,0.5)


#get quantiles
tquant <- list()
for(i in 1:4){
	tquant[[i]] <- quantile(tdayDF$L.plant.day[tdayDF$spsID == i], prob=c(0.025,0.25,0.50,0.75,0.975))

}

wd <- 13
hd <- 13
xl <- .75
xh <- 4.75
yl <- 0
yh <- 0.7

#tick width
tlw <- 2
#axis tick label size
alc <- 1.5
#axis width 
alw <- 2
#axis label size
llc <- 2
#point size
pcx <- 2
#line thickness
llw <- 3
xseq <- seq(1,4)+0.25
xseqA <- seq(0,5)+0.25


png(paste0(plotDir,"\\T_ramet.png"), width = 20, height = 20, units = "cm", res=300)
	layout(matrix(c(1),ncol=1), width=lcm(wd),height=lcm(hd))
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
		xlab= " ", ylab=" ", axes=FALSE)	
	for(i in 1:4){
	points(tdayDF$spsID[tdayDF$spsID == i] +tdayDF$jitteri[tdayDF$spsID == i], tdayDF$L.plant.day[tdayDF$spsID == i], pch=19,col=as.character(colDF$col4[i]))
	arrows(i+0.25,tquant[[i]][1],i+0.25,tquant[[i]][5], code=0, lwd=2, col=as.character(colDF$col3[i]))
	}
	
	for(i in 1:4){
	polygon(c(i,i,i+0.5,i+0.5),
			c(tquant[[i]][2],tquant[[i]][4],tquant[[i]][4],tquant[[i]][2]), border=NA, col=as.character(colDF$col3[i]))
	
	arrows(i,tquant[[i]][3],i+0.5,tquant[[i]][3], code=0, lwd=5, col=coli[i])
	}
	axis(1, xseqA, rep(" ", length(xseqA)), lwd.ticks=tlw, lwd=alw)
	axis(2, yseq, rep(" ", length(yseq)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq, at=yseq, side=2, line=1, cex=alc, las=2)
	mtext(c("Floodplain","Floodplain","Upland","Upland"), at=xseq, side=1, line=1, cex=alc)	
	mtext(c(expression(italic(Alnus)),expression(italic(Salix)),expression(italic(Betula)),expression(italic(Salix))), at=xseq, side=1, line=2, cex=alc)
	mtext(expression(paste("Transpiration")), side=2, line=5, cex=llc)
	mtext(expression(paste("(L ramet"^"-1","day"^"-1",")")), side=2, line=3, cex=llc)
	mtext("Position and Genus", side=1, line=4, cex=llc)
	
	
	dev.off()	

	
#################################################################
################## Patterns in stomatal conductance##############
#################################################################	
#unlist gc

#fit regression
fit <- list()
Int <- numeric()
Slope <- numeric()
minD <- numeric(0)
maxD <- numeric(0)

for(i in 1:nrow(gcDayN)){
	fit[[i]] <- lm(gcDF2$gc.mol.m2.s[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]] ~ 
					log(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]]))
	Int[i] <- coefficients(fit[[i]])[1]
	Slope[i] <- coefficients(fit[[i]])[2] 
	minD[i] <- min(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]])
	maxD[i] <- max(gcDF2$D[gcDF2$doy == gcDayN$doy[i] & gcDF2$spsID == gcDayN$spsID[i]])
}

gcDayN$slope <- Slope
gcDayN$Int <- Int
gcDayN$S <- abs(gcDayN$slope)/gcDayN$Int
plot(gcDayN$spsID, gcDayN$S)
wd <- 9
hd <- 9
xl <- 0.5
xh <- 1.75


yl <- 0
yh <- 0.75


xseq <- seq(0.5,1.75, by=0.25)
yseq <- seq(0,0.7, by =0.1)
#tick width
tlw <- 2
#axis tick label size
alc <- 1.5
#axis width 
alw <- 2
#axis label size
llc <- 2
#point size
pcx <- 2
#line thickness
llw <- 3
#legend size
lcx <- 1.25



##########first day ##########
doy1 <- 190

gcDay1N1 <- which(gcDayN$spsID == 1 & gcDayN$doy == doy1) 
gcDay1N2 <- which(gcDayN$spsID == 2 & gcDayN$doy == doy1) 
gcDay1N3 <- which(gcDayN$spsID == 3 & gcDayN$doy == doy1) 
gcDay1N4 <- which(gcDayN$spsID == 4 & gcDayN$doy == doy1) 

png(paste0(plotDir,"\\gcDay1.png"), width = 16, height = 15, units = "cm", res=300)
	layout(matrix(c(1),ncol=1), width=lcm(wd),height=lcm(hd))
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
			xlab= " ", ylab=" ", axes=FALSE)	
		#add all points
			points(gcDF2$D[gcDF2$doy == doy1 & gcDF2$spsID == 1], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy1 & gcDF2$spsID == 1], pch=19, col=as.character(colDF$col3[1]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy1 & gcDF2$spsID == 2], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy1 & gcDF2$spsID == 2], pch=19, col=as.character(colDF$col3[2]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy1 & gcDF2$spsID == 3], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy1 & gcDF2$spsID == 3], pch=19, col=as.character(colDF$col3[3]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy1 & gcDF2$spsID == 4], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy1 & gcDF2$spsID == 4], pch=19, col=as.character(colDF$col3[4]), cex=pcx)	
		#add slope
			points(seq(minD[gcDay1N1],maxD[gcDay1N1], length.out=100), Int[gcDay1N1] + (Slope[gcDay1N1]*log(seq(minD[gcDay1N1],maxD[gcDay1N1], length.out=100))), 
				type="l", col=as.character(colDF$coli[1]),
				lwd=llw)
			points(seq(minD[gcDay1N2],maxD[gcDay1N2], length.out=100), Int[gcDay1N2] + (Slope[gcDay1N2]*log(seq(minD[gcDay1N2],maxD[gcDay1N2], length.out=100))), 
				type="l", col=as.character(colDF$coli[2]),
				lwd=llw)
			points(seq(minD[gcDay1N3],maxD[gcDay1N3], length.out=100), Int[gcDay1N3] + (Slope[gcDay1N3]*log(seq(minD[gcDay1N3],maxD[gcDay1N3], length.out=100))), 
				type="l", col=as.character(colDF$coli[3]),
				lwd=llw)
				
			points(seq(minD[gcDay1N4],maxD[gcDay1N4], length.out=100), Int[gcDay1N4] + (Slope[gcDay1N4]*log(seq(minD[gcDay1N4],maxD[gcDay1N4], length.out=100))), 
				type="l", col=as.character(colDF$coli[4]),
				lwd=llw)	
		axis(1, xseq, rep(" ", length(xseq)), lwd.ticks=tlw, lwd=alw)
	axis(2, yseq, rep(" ", length(yseq)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq, at=yseq, side=2, line=1, cex=alc, las=2)
	mtext(xseq, at=xseq, side=1, line=1, cex=alc)
	mtext(expression(paste("Canopy stomatal conductance ")), side=2, line=5, cex=llc)
	mtext(expression(paste("(mol m"^"-2","s"^"-1",")")), side=2, line=3, cex=llc)
	mtext("Vapor pressure deficit (kPa)", side=1, line=3, cex=llc)
	mtext(paste("Day of year",doy1), side=3, line=3, cex=llc)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=19,
					col=c(as.character(colDF$col3[1]),
							as.character(colDF$col3[2]),
							as.character(colDF$col3[3]),
							as.character(colDF$col3[4])), bty="n", cex=lcx)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=NA,lwd=llw,
					col=c(as.character(colDF$coli[1]),
							as.character(colDF$coli[2]),
							as.character(colDF$coli[3]),
							as.character(colDF$coli[4])), bty="n", cex=lcx)	
dev.off()	



##########second day ##########
doy2 <- 222

gcDay2N1 <- which(gcDayN$spsID == 1 & gcDayN$doy == doy2) 
gcDay2N2 <- which(gcDayN$spsID == 2 & gcDayN$doy == doy2) 
gcDay2N3 <- which(gcDayN$spsID == 3 & gcDayN$doy == doy2) 
gcDay2N4 <- which(gcDayN$spsID == 4 & gcDayN$doy == doy2) 

png(paste0(plotDir,"\\gcDay2.png"), width = 16, height = 15, units = "cm", res=300)
	layout(matrix(c(1),ncol=1), width=lcm(wd),height=lcm(hd))
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
			xlab= " ", ylab=" ", axes=FALSE)	
		#add all points
			points(gcDF2$D[gcDF2$doy == doy2 & gcDF2$spsID == 1], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy2 & gcDF2$spsID == 1], pch=19, col=as.character(colDF$col3[1]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy2 & gcDF2$spsID == 2], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy2 & gcDF2$spsID == 2], pch=19, col=as.character(colDF$col3[2]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy2 & gcDF2$spsID == 3], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy2 & gcDF2$spsID == 3], pch=19, col=as.character(colDF$col3[3]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy2 & gcDF2$spsID == 4], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy2 & gcDF2$spsID == 4], pch=19, col=as.character(colDF$col3[4]), cex=pcx)	
		#add slope
			points(seq(minD[gcDay2N1],maxD[gcDay2N1], length.out=100), Int[gcDay2N1] + (Slope[gcDay2N1]*log(seq(minD[gcDay2N1],maxD[gcDay2N1], length.out=100))), 
				type="l", col=as.character(colDF$coli[1]),
				lwd=llw)
			points(seq(minD[gcDay2N2],maxD[gcDay2N2], length.out=100), Int[gcDay2N2] + (Slope[gcDay2N2]*log(seq(minD[gcDay2N2],maxD[gcDay2N2], length.out=100))), 
				type="l", col=as.character(colDF$coli[2]),
				lwd=llw)
			points(seq(minD[gcDay2N3],maxD[gcDay2N3], length.out=100), Int[gcDay2N3] + (Slope[gcDay2N3]*log(seq(minD[gcDay2N3],maxD[gcDay2N3], length.out=100))), 
				type="l", col=as.character(colDF$coli[3]),
				lwd=llw)
				
			points(seq(minD[gcDay2N4],maxD[gcDay2N4], length.out=100), Int[gcDay2N4] + (Slope[gcDay2N4]*log(seq(minD[gcDay2N4],maxD[gcDay2N4], length.out=100))), 
				type="l", col=as.character(colDF$coli[4]),
				lwd=llw)	
		axis(1, xseq, rep(" ", length(xseq)), lwd.ticks=tlw, lwd=alw)
	axis(2, yseq, rep(" ", length(yseq)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq, at=yseq, side=2, line=1, cex=alc, las=2)
	mtext(xseq, at=xseq, side=1, line=1, cex=alc)
	mtext(expression(paste("Canopy stomatal conductance ")), side=2, line=5, cex=llc)
	mtext(expression(paste("(mol m"^"-2","s"^"-1",")")), side=2, line=3, cex=llc)
	mtext("Vapor pressure deficit (kPa)", side=1, line=3, cex=llc)
	mtext(paste("Day of year",doy2), side=3, line=3, cex=llc)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=19,
					col=c(as.character(colDF$col3[1]),
							as.character(colDF$col3[2]),
							as.character(colDF$col3[3]),
							as.character(colDF$col3[4])), bty="n", cex=lcx)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=NA,lwd=llw,
					col=c(as.character(colDF$coli[1]),
							as.character(colDF$coli[2]),
							as.character(colDF$coli[3]),
							as.character(colDF$coli[4])), bty="n", cex=lcx)	
dev.off()	



##########third day ##########
doy3 <- 230

gcDay3N1 <- which(gcDayN$spsID == 1 & gcDayN$doy == doy3) 
gcDay3N2 <- which(gcDayN$spsID == 2 & gcDayN$doy == doy3) 
gcDay3N3 <- which(gcDayN$spsID == 3 & gcDayN$doy == doy3) 
gcDay3N4 <- which(gcDayN$spsID == 4 & gcDayN$doy == doy3) 

png(paste0(plotDir,"\\gcDay3.png"), width = 16, height = 15, units = "cm", res=300)
	layout(matrix(c(1),ncol=1), width=lcm(wd),height=lcm(hd))
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xaxs="i",yaxs="i",
			xlab= " ", ylab=" ", axes=FALSE)	
		#add all points
			points(gcDF2$D[gcDF2$doy == doy3 & gcDF2$spsID == 1], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy3 & gcDF2$spsID == 1], pch=19, col=as.character(colDF$col3[1]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy3 & gcDF2$spsID == 2], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy3 & gcDF2$spsID == 2], pch=19, col=as.character(colDF$col3[2]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy3 & gcDF2$spsID == 3], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy3 & gcDF2$spsID == 3], pch=19, col=as.character(colDF$col3[3]), cex=pcx)
			points(gcDF2$D[gcDF2$doy == doy3 & gcDF2$spsID == 4], 
				gcDF2$gc.mol.m2.s[gcDF2$doy == doy3 & gcDF2$spsID == 4], pch=19, col=as.character(colDF$col3[4]), cex=pcx)	
		#add slope
			points(seq(minD[gcDay3N1],maxD[gcDay3N1], length.out=100), Int[gcDay3N1] + (Slope[gcDay3N1]*log(seq(minD[gcDay3N1],maxD[gcDay3N1], length.out=100))), 
				type="l", col=as.character(colDF$coli[1]),
				lwd=llw)
			points(seq(minD[gcDay3N2],maxD[gcDay3N2], length.out=100), Int[gcDay3N2] + (Slope[gcDay3N2]*log(seq(minD[gcDay3N2],maxD[gcDay3N2], length.out=100))), 
				type="l", col=as.character(colDF$coli[2]),
				lwd=llw)
			points(seq(minD[gcDay3N3],maxD[gcDay3N3], length.out=100), Int[gcDay3N3] + (Slope[gcDay3N3]*log(seq(minD[gcDay3N3],maxD[gcDay3N3], length.out=100))), 
				type="l", col=as.character(colDF$coli[3]),
				lwd=llw)
				
			points(seq(minD[gcDay3N4],maxD[gcDay3N4], length.out=100), Int[gcDay2N4] + (Slope[gcDay3N4]*log(seq(minD[gcDay3N4],maxD[gcDay3N4], length.out=100))), 
				type="l", col=as.character(colDF$coli[4]),
				lwd=llw)	
		axis(1, xseq, rep(" ", length(xseq)), lwd.ticks=tlw, lwd=alw)
	axis(2, yseq, rep(" ", length(yseq)), lwd.ticks=tlw, lwd=alw)
	mtext(yseq, at=yseq, side=2, line=1, cex=alc, las=2)
	mtext(xseq, at=xseq, side=1, line=1, cex=alc)
	mtext(expression(paste("Canopy stomatal conductance ")), side=2, line=5, cex=llc)
	mtext(expression(paste("(mol m"^"-2","s"^"-1",")")), side=2, line=3, cex=llc)
	mtext("Vapor pressure deficit (kPa)", side=1, line=3, cex=llc)
	mtext(paste("Day of year",doy3), side=3, line=3, cex=llc)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=19,
					col=c(as.character(colDF$col3[1]),
							as.character(colDF$col3[2]),
							as.character(colDF$col3[3]),
							as.character(colDF$col3[4])), bty="n", cex=lcx)
	legend("topright", c(expression("Floodplain"~italic(Alnus)), expression("Floodplain"~italic(Salix)),
						expression("Upland"~italic(Betula)), expression("Upland"~italic(Salix))),
			pch=NA,lwd=llw,
					col=c(as.character(colDF$coli[1]),
							as.character(colDF$coli[2]),
							as.character(colDF$coli[3]),
							as.character(colDF$coli[4])), bty="n", cex=lcx)	
dev.off()	



#check for days with all species for comparision
s1 <- gcDayN[gcDayN$spsID == 1,]
s2 <- gcDayN[gcDayN$spsID == 2,]
s3 <- gcDayN[gcDayN$spsID == 3,]
s4 <- gcDayN[gcDayN$spsID == 4,]

a1 <- inner_join(s1,s2, by="doy")
a2 <- inner_join(a1, s3, by="doy")
a3 <- inner_join(a2, s4, by="doy")


