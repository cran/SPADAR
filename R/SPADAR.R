
require(mapproj)
require(RCEIM)

createAllSkyScatterPlotChart <- function(x, y, pointcol=rgb(0,0,0,0.5), pointsize=0.5, dataCoordSys="equatorial", 
	mainGrid="equatorial", eqCol="red", eclCol="blue", galCol="green", eqLty=1, eclLty=2, galLty=3, 
	eqLwd=1, eclLwd=1, galLwd=1, eqDraw=TRUE, eclDraw=TRUE, galDraq=TRUE, 
	projname="aitoff", projparam=NULL, projorient=NULL, nGridpoints=100, 
  addLab=TRUE, label.cex=0.6, 
	main=paste("All-Sky Scatter Plot (",projname,")",sep=""), ...) {
	
	# If the data coordinate system is different from the grid main coordinate system
	# perform the necessary coordinate system conversion
	if(dataCoordSys != mainGrid) {
		if((mainGrid == 'equatorial') && (dataCoordSys == 'galactic')) { nT <- 2 }
		if((mainGrid == 'equatorial') && (dataCoordSys == 'ecliptic')) { nT <- 4 }
		if((mainGrid == 'ecliptic') && (dataCoordSys == 'equatorial')) { nT <- 3 }
		if((mainGrid == 'ecliptic') && (dataCoordSys == 'galactic')) { nT <- 6 }
		if((mainGrid == 'galactic') && (dataCoordSys == 'equatorial')) { nT <- 1 }
		if((mainGrid == 'galactic') && (dataCoordSys == 'ecliptic')) { nT <- 5 }
		pl <- myEuler(x, y, nT)
		x <- pl$ao
		y <- pl$bo
	}

	# Compute the projection of the points
	ppoints <- mapproj::mapproject(x, y, projname, parameters=projparam, orientation=projorient)

	# Prepare the plot
	par(mar=c(2,0,3,0))
	plot(ppoints$x, ppoints$y, type="n", asp=1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="", main=main)

	# Add astronomical objects
	points(ppoints$x, ppoints$y, cex=pointsize, col=pointcol, ...)
	
	# Create the grid on top of the points
	createAllSkyGridChart(mainGrid=mainGrid, eqCol=eqCol, eclCol=eclCol, galCol=galCol, 
						 eqLty=eqLty, eclLty=eclLty, galLty=galLty, 
						 eqLwd=eqLwd, eclLwd=eclLwd, galLwd=galLwd, 
						 eqDraw=eqDraw, eclDraw=eclDraw, galDraq=galDraq, 
						 projname=projname, projparam=projparam, projorient=projorient, 
						 npoints=nGridpoints, addLab=addLab, label.cex=label.cex, 
             overplot=TRUE)
	
	par(mar=(c(5,4,4,2)+0.1))	
}

overplotScatterPlotInAllSkyGridChart <- function(x, y, pointcol=rgb(0,0,0,0.5), pointsize=0.5, dataCoordSys="equatorial", 
	mainGrid="equatorial", projname="aitoff", projparam=NULL, projorient=NULL, ...) {

	# If the data coordinate system is different from the grid main coordinate system
	# perform the necessary coordinate system conversion
	if(dataCoordSys != mainGrid) {
		if((mainGrid == 'equatorial') && (dataCoordSys == 'galactic')) { nT <- 2 }
		if((mainGrid == 'equatorial') && (dataCoordSys == 'ecliptic')) { nT <- 4 }
		if((mainGrid == 'ecliptic') && (dataCoordSys == 'equatorial')) { nT <- 3 }
		if((mainGrid == 'ecliptic') && (dataCoordSys == 'galactic')) { nT <- 6 }
		if((mainGrid == 'galactic') && (dataCoordSys == 'equatorial')) { nT <- 1 }
		if((mainGrid == 'galactic') && (dataCoordSys == 'ecliptic')) { nT <- 5 }
		pl <- myEuler(x, y, nT)
		x <- pl$ao
		y <- pl$bo
	}

	# Compute the projection of the points
	ppoints <- mapproject(x, y, projname, parameters=projparam, orientation=projorient)

	# Add astronomical objects
	points(ppoints$x, ppoints$y, cex=pointsize, col=pointcol, ...)

}

createAllSkyGridChart <- function(longitude=c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
								  latitude=c(-75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75), 
								  mainGrid="equatorial", eqCol="red", eclCol="blue", galCol="green", 
								  eqLty=1, eclLty=2, galLty=3, eqLwd=1, eclLwd=1, galLwd=1, 
								  eqDraw=TRUE, eclDraw=TRUE, galDraq=TRUE, projname="aitoff", 
								  projparam=NULL, projorient=NULL, npoints=50, overplot=FALSE, 
								  addLab=TRUE, label.cex=0.6, main=paste("All-Sky Grid (",projname,")",sep=""), ...) {
	
	if(overplot==FALSE) {
		# Compute the projection of the points
		x <- runif(10000, min=0, max=360)
		y <- runif(10000, min=-90, max=+90)
		ppoints <- mapproject(x, y, projection=projname, parameters=projparam, orientation=projorient)
		
		# Prepare the plot
		par(mar=c(2,0,3,0))
		plot(ppoints$x, ppoints$y, type="n", asp=1, xaxt="n", yaxt="n", bty="n", xlab="", ylab="", main=main)
	}
	
	# Add Equatorial Coordinate Grid
	if( eqDraw ) {
		addEquatorialGrid(longitude=longitude, latitude=latitude,
					npoints=npoints, projname=projname, projparam=projparam, 
					projorient=projorient, mainGrid=mainGrid, 
					color=eqCol, lty=eqLty, addLab=addLab, label.cex=label.cex, lwd=eqLwd, ...)
	}
	
	# Add Ecliptic Coordinate Grid
	if( eclDraw ) {
		addEclipticGrid(longitude=longitude, latitude=latitude,
					npoints=npoints, projname=projname, projparam=projparam, 
					projorient=projorient, mainGrid=mainGrid, 
					color=eclCol, lty=eclLty, addLab=addLab, label.cex=label.cex, lwd=eclLwd, ...)
	}
	
	# Add Galactic Coordinate Grid
	if( galDraq ) {
		addGalacticGrid(longitude=longitude, latitude=latitude,
					npoints=npoints, projname=projname, projparam=projparam, 
					projorient=projorient, mainGrid=mainGrid, 
					color=galCol, lty=galLty,  addLab=addLab, label.cex=label.cex, lwd=galLwd, ...)
	}

	if(overplot==FALSE) {
		par(mar=(c(5,4,4,2)+0.1))
	}
}

# Equatorial (J2000)
addEquatorialGrid <- function(longitude=c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
						    latitude=c(-60, -30, 0, 30, 60), npoints=1000, projname, projparam=projparam, 
							projorient=projorient, mainGrid='equatorial', color, lty, addLab=TRUE, label.cex=0.6, lwd=1, ...) {
	if(mainGrid=='equatorial') {
		nT <- 0
	}
	if(mainGrid=='ecliptic') {
		nT <- 3
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	if(mainGrid=='galactic') {
		nT <- 1
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	addLatLongLinesLabAvoid(pos=longitude, avoidPos=latitude, latitudeLines=FALSE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
	addLatLongLinesLabAvoid(pos=latitude, avoidPos=longitude, latitudeLines=TRUE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
}

# Galactic
addGalacticGrid <- function(longitude=c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
						    latitude=c(-60, -30, 0, 30, 60), npoints=1000, projname, projparam=projparam, 
							projorient=projorient, mainGrid='equatorial', color, lty, addLab=TRUE, label.cex=0.6, lwd=1, ...) {
	if(mainGrid=='equatorial') {
		nT <- 2
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	if(mainGrid=='ecliptic') {
		nT <- 6
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	if(mainGrid=='galactic') {
		nT <- 0
	}
	addLatLongLinesLabAvoid(pos=longitude, avoidPos=latitude, latitudeLines=FALSE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
	addLatLongLinesLabAvoid(pos=latitude, avoidPos=longitude, latitudeLines=TRUE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
}

# Ecliptic
addEclipticGrid <- function(longitude=c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
						    latitude=c(-60, -30, 0, 30, 60), npoints=1000, projname, projparam=projparam, 
							projorient=projorient, mainGrid='equatorial', color, lty, addLab=TRUE, label.cex=0.6, lwd=1, ...) {
	if(mainGrid=='equatorial') {
		nT <- 4
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	if(mainGrid=='ecliptic') {
		nT <- 0
	}
	if(mainGrid=='galactic') {
		nT <- 5
		if( (0%in%longitude) && (360%in%longitude)) { longitude <- longitude[-which(longitude==360)] }
	}
	addLatLongLinesLabAvoid(pos=longitude, avoidPos=latitude, latitudeLines=FALSE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
	addLatLongLinesLabAvoid(pos=latitude, avoidPos=longitude, latitudeLines=TRUE, addLab=addLab, nCoordTransf=nT, npoints=npoints, projname=projname, projparam=projparam, projorient=projorient, col=color, lty=lty, lwd=lwd, label.cex=label.cex, ...)
}

# if lat = TRUE  : pos will include latitude lines
# if lat = FALSE : pos will include longitude lines
addLatLongLinesLabAvoid <- function(pos=c(-60, -30, 0, 30, 60), avoidPos=c(0, 45, 90, 135, 180, 225, 270, 315, 360), latitudeLines=TRUE, addLab=FALSE, nCoordTransf=2, npoints=1000, projname, projparam=NULL, projorient=NULL, col, lty, label.col=col, label.cex=0.6, lwd=1, ...) {
	if(latitudeLines) {
		xx <- seq(from=0, to=360, length.out=npoints) 
		yy <- pos

		xx_avoid <- avoidPos
		yy_avoid <- seq(from=-90, to=90, length.out=npoints) 
		
		xxx <- c()
		yyy <- c()
		xxx_avoid <- c()
		yyy_avoid <- c()
		for(i in 1:length(yy)){
			yyy <- c(yyy, rep(yy[i], npoints))
			xxx <- c(xxx, xx)
		}
		for(i in 1:length(xx_avoid)){
			xxx_avoid <- c(xxx_avoid, rep(xx_avoid[i], npoints))
			yyy_avoid <- c(yyy_avoid, yy_avoid)	
		}
	} else {
		xx <- pos
		yy <- seq(from=-90, to=90, length.out=npoints) 

		xx_avoid <- seq(from=0, to=360, length.out=npoints) 
		yy_avoid <- avoidPos 
		
		xxx <- c()
		yyy <- c()
		xxx_avoid <- c()
		yyy_avoid <- c()
		for(i in 1:length(xx)){
			xxx <- c(xxx, rep(xx[i], npoints))
			yyy <- c(yyy, yy)
		}
		for(i in 1:length(yy_avoid)){
			yyy_avoid <- c(yyy_avoid, rep(yy_avoid[i], npoints))
			xxx_avoid <- c(xxx_avoid, xx_avoid)					
		}
	}
	if(nCoordTransf != 0) {
		pl <- myEuler(xxx, yyy, nCoordTransf)
		pl_avoid <- myEuler(xxx_avoid, yyy_avoid, nCoordTransf)
		lpoints <- mapproj::mapproject(x=pl$ao, y=pl$bo, projname, parameters=projparam, orientation=projorient)
		lpoints_avoid <- mapproj::mapproject(x=pl_avoid$ao, y=pl_avoid$bo, projname, parameters=projparam, orientation=projorient)
	} else {
		lpoints <- mapproj::mapproject(x=xxx, y=yyy, projname, parameters=projparam, orientation=projorient)
		lpoints_avoid <- mapproj::mapproject(x=xxx_avoid, y=yyy_avoid, projname, parameters=projparam, orientation=projorient)
	}
	xs <- split(lpoints$x, ceiling(seq_along(lpoints$x)/npoints))
	ys <- split(lpoints$y, ceiling(seq_along(lpoints$y)/npoints))
	xs_avoid <- split(lpoints_avoid$x, ceiling(seq_along(lpoints_avoid$x)/npoints))
	ys_avoid <- split(lpoints_avoid$y, ceiling(seq_along(lpoints_avoid$y)/npoints))
		
	if(addLab) {
			if(latitudeLines) {
				tex <- "latitude lines..."
			} else {
				tex <- "longitude lines..."
			}
		idx_x <- which(!is.na(xs))
		idx_y <- which(!is.na(ys))
		idx <- intersect(idx_x, idx_y)
		xs <- xs[idx]
		ys <- ys[idx]	
		tryXs <- xs
		tryYs <- ys		
		for(i in 1:length(tryXs)) {
			tryXs[[i]] <- as.vector(na.omit(unlist(tryXs[i])))
			tryYs[[i]] <- as.vector(na.omit(unlist(tryYs[i])))
		}
		iidx <- c()
		xsbckp <- xs
		ysbckp <- ys
		for(i in 1:(length(xs)-1)) {
			nnaX <- xs[[i]]
			nnaY <- ys[[i]]
			idx_x <- which(!is.na(nnaX))
			idx_y <- which(!is.na(nnaY))
			idx <- intersect(idx_x, idx_y)
			nnaXp <- xs[[i+1]]
			nnaYp <- ys[[i+1]]
			idx_xp <- which(!is.na(nnaXp))
			idx_yp <- which(!is.na(nnaYp))
			idxp <- intersect(idx_xp, idx_yp)
			midx <- intersect(idx, idxp)
			if(i == 1) { 
				iidx <- midx 
			} else {
				iidx <- intersect(iidx, midx)
			}
		}
		for(i in 1:length(xs)) {
			tx <- as.vector(unlist(xs[i]))
			xs[[i]] <- tx[iidx]
			
			ty <- as.vector(unlist(ys[i]))
			ys[[i]] <- ty[iidx]
		}

		for(i in 1:length(xs_avoid)) {
			xs_avoid[[i]] <- as.vector(na.omit(unlist(xs_avoid[i])))
			ys_avoid[[i]] <- as.vector(na.omit(unlist(ys_avoid[i])))
			
		}
			pppx <- is.na(unlist(xs))
			pppy <- is.na(unlist(ys))
		for(i in 1:length(unlist(xs))){
			if(pppx[i]) {print("SOME xs is NA!")}
			if(pppy[i]) {print("SOME ys is NA!")}
		}
		optPos <- tryCatch(
			expr = ceimOpt(OptimFunction = "computePotentialForUniqueIdxAvoid", maxIter=10, Ntot=50, 
							  epsilon=0.1, nParams=1, boundaries=t(c(1, npoints)), xs=xs, ys=ys, xs_avoid=xs_avoid, ys_avoid=ys_avoid)
			, 
			error = function(e) {ceimOpt(OptimFunction = "computePotentialForUniqueIdxAvoid", maxIter=10, Ntot=50, 
							  epsilon=0.1, nParams=1, boundaries=t(c(1, npoints)), xs=tryXs, ys=tryYs, xs_avoid=xs_avoid, ys_avoid=ys_avoid)
			})
		xs <- xsbckp
		ys <- ysbckp
	}
			
	for(i in 1:length(xs)) {
		xst <- as.vector(unlist(xs[i]))
		yst <- as.vector(unlist(ys[i]))
		# If labels should be added		
		if(addLab) {
			drawLinesInProjection(xst, yst, label=pos[i], labelPos=round(optPos$BestMember[1]), col=col, lwd=lwd, lty=lty, label.col=label.col, label.cex=label.cex, ...)			
		} else {
			drawLinesInProjection(xst, yst, label=NULL, col=col, lwd=lwd, lty=lty, ...)
		}
	}
}

computePotentialForUniqueIdx <- function(pos, xs, ys) {
	
	posVec <- round(pos)
	
	for(i in 1:length(xs)) {
		xst <- as.vector(unlist(xs[i]))
		yst <- as.vector(unlist(ys[i]))
		xVec[i] <- xst[pos]
		yVec[i] <- yst[pos]
	}

	return(computePotentialForUnitaryCharges(xVec, yVec))
}

computePotentialForUniqueIdxAvoid <- function(pos, xs, ys, xs_avoid=0, ys_avoid=0) {
	
	posVec <- round(pos)
	
	xVec <- c()
	yVec <- c()
	xVec_avoid <- c()
	yVec_avoid <- c()
	
	for(i in 1:length(xs)) {
		xst <- as.vector(unlist(xs[i]))
		yst <- as.vector(unlist(ys[i]))
		xVec[i] <- xst[pos]
		yVec[i] <- yst[pos]
	}
	if(length(xs_avoid) > 1)
	for(i in 1:length(xs_avoid)) {
		xst_avoid <- as.vector(unlist(xs_avoid[i]))
		yst_avoid <- as.vector(unlist(ys_avoid[i]))
		xVec_avoid[i] <- xst_avoid[pos]
		yVec_avoid[i] <- yst_avoid[pos]
	}
	
	return(computePotentialForUnitaryCharges(c(xVec, xVec_avoid), c(yVec, yVec_avoid)))
}


computePotentialForIdx <- function(posVec, xs, ys) {
	
	posVec <- round(posVec)
	
	for(i in 1:length(xs)) {
		xst <- as.vector(unlist(xs[i]))
		yst <- as.vector(unlist(ys[i]))
		xVec[i] <- xst[posVec[i]]
		yVec[i] <- yst[posVec[i]]
	}

	return(computePotentialForUnitaryCharges(xVec, yVec))
}

computePotentialForUnitaryCharges <- function(xVec, yVec) {
	pot <- 0
	idxs <- combn(length(xVec), 2)
	for(i in 1:length(xVec)) {
		pot <- pot + 1/sqrt( (xVec[idxs[2,i]]-xVec[idxs[1,i]])^2 + (yVec[idxs[2,i]]-yVec[idxs[1,i]])^2 )
	}
	
	if(is.na(pot)) {
		pot <- 1e+555
	}	
	
	return(pot)
}

drawLinesInProjection <- function(x, y, label=NULL, labelPos, eps=0.3, label.col, label.cex=0.5, labspex=2, ...) {
	# A small function to avoid drawing lines crossing the projection due to the projection effects
	# and to avoid drawing over the line label
	if(is.null(label) == FALSE) {
		xC <- labelPos 
		if(xC+2 >= length(x)) {
			xC <- xC - 2
		} 
		xCbckp <- xC
		signm <- -1
		while(is.na(x[xC+2]) || is.na(y[xC+2]) || is.na(x[xC]) || is.na(y[xC]) ) {
			xC <- xC + 2 * signm
			if(xC <= 1) { 
				xC <- xCbckp
				signm <- +1
			}
		}
		ang <- atan2((y[xC+2]-y[xC]),(x[xC+2]-x[xC]))*180/pi 
		text(x[xC], y[xC], label, srt=ang, cex=label.cex, col=label.col)

	# Now we need to determine where to start drawing and where to stop drawing
	stringHalfSize <- strwidth(label)*label.cex/labspex
	i <- 1
	distance <- 0
	while(distance < stringHalfSize) {
		if( (xC+i) < length(x)) {
			distance <- sqrt((x[xC+i] - x[xC])^2 + (y[xC+i] - y[xC])^2)
		} else {
			distance <- sqrt((x[xC-i] - x[xC])^2 + (y[xC-i] - y[xC])^2)
		}
		if(is.na(distance)) {
			distance <- 0
		}
		i <- i + 1
	}
	spacing <- i
	} else {
		xC <- 0
		spacing <- 0
	}

	asdf <- sqrt( (x[2:length(x)] - x[1:(length(x)-1)])^2 + (y[2:length(y)] - y[1:(length(y)-1)])^2)
	if(max(asdf, na.rm = TRUE) > eps) {
		pMax <- which(asdf>eps)
		if(length(pMax) == 1) {
			if(xC<pMax) {
				if((xC-spacing) > 1) {
					lines(x[1:(xC-spacing)], y[1:(xC-spacing)], ...)
				}
				if((xC+1+spacing) < pMax) {
					lines(x[(xC+1+spacing):pMax], y[(xC+1+spacing):pMax], ...)
				}
				lines(x[(pMax+1):length(x)], y[(pMax+1):length(y)], ...)
			} else {
				lines(x[1:pMax], y[1:pMax], ...)
				if((pMax+1) < (xC-spacing)) {
					lines(x[(pMax+1):(xC-spacing)], y[(pMax+1):(xC-spacing)], ...)
				}
				if((xC+1+spacing) < length(x)) {
					lines(x[(xC+1+spacing):length(x)], y[(xC+1+spacing):length(x)], ...)
				}
			}
		} else {
			if(xC<=pMax[1]) {
				if((xC-spacing) > 1) {
					lines(x[1:(xC-spacing)], y[1:(xC-spacing)], ...)
				}
				if((xC+1+spacing) < pMax[1]) {
					lines(x[(xC+1+spacing):pMax[1]], y[(xC+1+spacing):pMax[1]], ...)
				}
				if((pMax[1]+1) < pMax[2]) {
					lines(x[(pMax[1]+1):pMax[2]], y[(pMax[1]+1):pMax[2]], ...)
				}
				if((pMax[2]+1) < length(x)) {
					lines(x[(pMax[2]+1):length(x)], y[(pMax[2]+1):length(y)], ...)				
				}
			} else if(xC<=pMax[2]) {
				if(pMax[1] != 1) {
					lines(x[1:pMax[1]], y[1:pMax[1]], ...)
				}
				if((pMax[1]+1) < (xC-spacing)) {
					lines(x[(pMax[1]+1):(xC-spacing)], y[(pMax[1]+1):(xC-spacing)], ...)
				}
				if((xC+1+spacing) < pMax[2]) {
					lines(x[(xC+1+spacing):pMax[2]], y[(xC+1+spacing):pMax[2]], ...)
				}
				if((pMax[2]+1) < length(x)) {
					lines(x[(pMax[2]+1):length(x)], y[(pMax[2]+1):length(y)], ...)				
				}
			} else {			
				if(pMax[1] != 1) {
					lines(x[1:pMax[1]], y[1:pMax[1]], ...)
				}
				if((pMax[1]+1) < pMax[2]) {				
					lines(x[(pMax[1]+1):pMax[2]], y[(pMax[1]+1):pMax[2]], ...)		
				}
				if((pMax[2]+1) < (xC-spacing)) {
					lines(x[(pMax[2]+1):(xC-spacing)], y[(pMax[2]+1):(xC-spacing)], ...)				
				}
				if((xC+1+spacing) < length(x)) {
					lines(x[(xC+1+spacing):length(x)], y[(xC+1+spacing):length(y)], ...)				
				}
			}
		}
	} else {
		if((xC-spacing) > 1) {
			lines(x[1:(xC-spacing)], y[1:(xC-spacing)], ...)
		}
		lines(x[(xC+1+spacing):length(x)], y[(xC+1+spacing):length(y)], ...)		
	}

}

ceimOpt <- function (OptimFunction = "testFunOptimization", nParams = 1, 
    minimize = TRUE, Ntot = 1000, N_elite = floor(Ntot/4), N_super = 1, 
    alpha = 1, epsilon = 0.1, q = 2, maxIter = 50, waitGen = maxIter, 
    boundaries = t(matrix(rep(c(-10, 10), nParams), ncol = nParams)), 
    plotConvergence = FALSE, chaosGen = maxIter, handIterative = FALSE, 
    verbose = FALSE, plotResultDistribution = FALSE, parallelVersion = FALSE, ...) 
{
    if (dim(boundaries)[1] != nParams) {
        stop("Boundaries dimensions (n. rows) are not compatible with the number of parameters.")
    }
    if (dim(boundaries)[2] != 2) {
        stop("Boundaries dimensions (n. cols) are not valid. It should be two (min, max)!")
    }
    if (sum(boundaries[, 1] < boundaries[, 2]) != length(boundaries[, 
        1])) {
        stop("Trying to use invalid boundaries, with Min > Max!")
    }
    if (plotConvergence) {
        dev.new()
    }
    if (minimize) {
        mfactor <- 1
    }
    else {
        mfactor <- -1
    }
    convergenceStatsBest <- vector("double", maxIter)
    convergenceStatsMean <- vector("double", maxIter)
    convergenceStatsSdev <- vector("double", maxIter)
    Sfunc <- match.fun(OptimFunction)
    param_Estimated <- matrix(0, nrow = Ntot, ncol = nParams)
    for (i in 1:nParams) {
        param_Estimated[, i] <- runif(Ntot, min = boundaries[i, 
            1], max = boundaries[i, 2])
    }
    mu <- vector("double", nParams)
    sig <- vector("double", nParams)
    for (i in 1:nParams) {
        mu[i] <- mean(param_Estimated[, i])
        sig[i] <- sd(param_Estimated[, i])
    }
    fracsig <- 10 * epsilon
    iter <- 1
    waitGenCounter <- 0
    chaosCounter <- 0
    oldEliteS <- 0
    Svals <- vector("double", Ntot)
    Crit <- "Temp"
    while ((iter < maxIter) && (epsilon < fracsig) && (waitGenCounter <= 
        waitGen) && (Crit != "SAME")) {
        if (verbose) {
            for (i in 1:nParams) {
                cat(paste("[", iter, " - ", i, "] - ", round(mu[i], 
                  4), " - ", round(fracsig, 4), "\n"))
            }
        }
        if (parallelVersion) {
            if (requireNamespace("parallel", quietly = TRUE)) {
                pp <- parallel::mclapply(1:Ntot, function(x) {
                  Sfunc(param_Estimated[x, ], ...)
                })
                for (i in 1:Ntot) {
                  Svals[i] <- mfactor * pp[[i]]
                }
            }
        }
        else {
            for (i in 1:Ntot) {
                Svals[i] <- mfactor * Sfunc(param_Estimated[i, 
                  ], ...)
            }
        }
        dfsel <- data.frame(param_Estimated, S = Svals)
        dfsel <- RCEIM::sortDataFrame(dfsel, "S")
        sN <- length(dfsel$S)
        elite <- dfsel[1:N_elite, ]
        if (verbose) {
            cat(paste("[", iter, "      ] -                       ", 
                round(elite[1, (length(elite))], 3), "\n"))
        }
        convergenceStatsBest[iter] <- elite$S[1]
        convergenceStatsMean[iter] <- mean(elite$S)
        convergenceStatsSdev[iter] <- sd(elite$S)
        if (abs(oldEliteS - elite$S[1]) > 0) {
            waitGenCounter <- 0
            chaosGenCounter <- 0
        }
        else {
            waitGenCounter <- waitGenCounter + 1
            chaosGenCounter <- chaosGenCounter + 1
        }
        oldEliteS <- elite$S[1]
        if (verbose) {
            cat(paste("[", iter, " - ", maxIter, "] -  CONVERGENCE STATUS \t\t\t ", 
                round(convergenceStatsBest[iter], 3), " - ", 
                round(convergenceStatsMean[iter], 3), " - ", 
                round(convergenceStatsSdev[iter], 3), "\n"))
        }
        if (convergenceStatsBest[iter] == convergenceStatsMean[iter]) {
            Crit <- "SAME"
        }
        if (epsilon < min(sig)) {
            alpha_d <- alpha - alpha * (1 - 1/iter)^q
            for (i in 1:nParams) {
                mu[i] <- alpha * mean(elite[, i]) + (1 - alpha) * 
                  mu[i]
                sig[i] <- alpha_d * sd(elite[, i]) + (1 - alpha_d) * 
                  sig[i]
            }
            param_Estimated <- matrix(0, nrow = Ntot, ncol = nParams)
            Nnew <- Ntot - N_super
            for (i in 1:nParams) {
                param_Estimated[1:Nnew, i] <- rnorm(Nnew, mu[i], 
                  sig[i])
            }
            if (N_super > 0) {
                for (i in 1:nParams) {
                  param_Estimated[Ntot:(Nnew + 1), i] <- elite[1:N_super, 
                    i]
                }
            }
            maxSigIdx <- which(sig == max(sig))
            fracsig <- sig[maxSigIdx]/abs(mu[maxSigIdx])
        }
        if (chaosGenCounter >= chaosGen) {
            if (verbose) {
                cat(paste("[", iter, " - ", i, "] -  Adding CHAOS! \n"))
            }
            for (i in 1:nParams) {
                param_Estimated[1:Nnew, i] <- rnorm(Nnew, mean = mu[i], 
                  sd = (boundaries[i, 2] - boundaries[i, 1])/2)
            }
            if (N_super > 0) {
                for (i in 1:nParams) {
                  param_Estimated[Ntot:(Nnew + 1), i] <- elite[1:N_super, 
                    i]
                }
            }
            chaosGenCounter <- 0
        }
        param_Estimated <- RCEIM::enforceDomainOnParameters(param_Estimated, 
            boundaries)
        if (plotConvergence) {
            plot(1:iter, convergenceStatsMean[1:iter], type = "n", 
                xlab = "Iteration", ylab = "Fval", ylim = c(min(c(convergenceStatsBest, 
                  convergenceStatsMean)), max(convergenceStatsMean)), 
                main = "Convergence status")
            grid()
            overPlotErrorPolygon(1:iter, convergenceStatsMean[1:iter], 
                convergenceStatsSdev[1:iter], col = rgb(0, 0, 
                  1, 0.25), logPlot = FALSE, border = NA)
            lines(1:iter, convergenceStatsMean[1:iter], col = "black", 
                lwd = 2)
            lines(1:iter, convergenceStatsBest[1:iter], col = "red", 
                lwd = 2)
        }
        if (handIterative) {
            cat(" Press enter for the next iteration... ")
            readline()
        }
        iter <- iter + 1
    }
    if (iter >= maxIter) {
        Convergence <- FALSE
        Crit <- "Maximum iterations reached."
    }
    else if (waitGenCounter >= waitGen) {
        Convergence <- FALSE
        Crit <- "Maximum wait generations reached without changes in the best individual."
    }
    else if (Crit == "SAME") {
        Convergence <- TRUE
        Crit <- "Convergence :: (Mean-Best) are the same, and the stdev is small."
    }
    else {
        Convergence <- TRUE
        Crit <- "Convergence criteria reached :: Max(sigma/mu) is smaller than the requested value."
    }
    if (plotResultDistribution) {
        plotEliteDistrib(elite)
    }
    return(list(BestMember = unlist(elite[1, 1:(nParams + 1)]), 
        Convergence = Convergence, Criteria = Crit, Iterations = iter, 
        EliteMembers = elite))
}

myEuler <- function (ai, bi, select, fk4 = F, radian = F) {
# this function was modified from the astrolibR package. 
# the browser() funciton call inside it was preventing its use
# in this package
    twopi = 2 * pi
    fourpi = 4 * pi
    rad_to_deg = 180/pi
    if (fk4) {
      equinox = "(b1950)"
      psi = c(0.57595865315, 4.9261918136, 0, 0, 0.11129056012, 
            4.7005372834)
      stheta = c(0.88781538514, -0.88781538514, 0.39788119938, 
               -0.39788119938, 0.86766174755, -0.86766174755)
      ctheta = c(0.46019978478, 0.46019978478, 0.9174369467, 
               0.9174369467, 0.49715499774, 0.49715499774)
      phi = c(4.9261918136, 0.57595865315, 0, 0, 4.7005372834, 
            0.11129056012)
    }
    else {
      equinox = "(j2000)"
      psi = c(0.574770433, 4.9368292465, 0, 0, 0.11142137093, 
            4.71279419371)
      stheta = c(0.88998808748, -0.88998808748, 0.39777715593, 
               -0.39777715593, 0.86766622025, -0.86766622025)
      ctheta = c(0.45598377618, 0.45598377618, 0.91748206207, 
               0.91748206207, 0.49714719172, 0.49714719172)
      phi = c(4.9368292465, 0.574770433, 0, 0, 4.71279419371, 
            0.11142137093)
    }
    i = select
    if (radian) {
      ao = ai - phi[i]
      bo = bi
    }
    else {
      ao = ai/rad_to_deg - phi[i]
      bo = bi/rad_to_deg
    }
    sb = sin(bo)
    cb = cos(bo)
    cbsa = cb * sin(ao)
    bo = -stheta[i] * cbsa + ctheta[i] * sb
    tmp = bo
    tmp[tmp > 1] = 1
    bo = asin(tmp)
    if (!radian) 
      bo = bo * rad_to_deg
    ao = atan2(ctheta[i] * cbsa + stheta[i] * sb, cb * cos(ao))
    ao = ((ao + psi[i] + fourpi)%%twopi)
    if (!radian) 
      ao = ao * rad_to_deg
    return(list(ao = ao, bo = bo))
}