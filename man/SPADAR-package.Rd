\name{SPADAR-package}
\alias{SPADAR-package}
\alias{SPADAR}
\docType{package}
\title{
Spherical Projections of Astronomical Data
}
\description{
Provides easy to use functions to create all-sky grid plots of widely used astronomical coordinate systems (equatorial, ecliptic, galactic) and scatter plots of data on any of these systems including on-the-fly coordinate system conversion. It supports any type of spherical projection defined by the mapproj package.
}
\details{
\tabular{ll}{
Package: \tab SPADAR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2017-04-29\cr
License: \tab GPL (>= 3)\cr
}
The SPADAR package provides simple functions to create spherical projection plots of astronomical data. It enables the creation of coordinate system grids (as backgrounds or overlays) in equatorial, ecliptic and galactic coordinates and also supports the conversion between these coordinate systems (J2000). The plots can adopt multiple spherical projections to the plane (aitoff, mollweide, mercator using the default projection parameters are tested). Coordinate system conversions are performed by a modified version of the euler function from the astrolibR package, and plane projections by the mapproj package.

\if{html}{\figure{uhecrauger2014-aitoff-galeq.jpg}{UHECR-Auger-Events-2014}\figure{astrocoordsystems-aitoff.jpg}{CommonAstronomicalCoordinateSystems}}
\if{latex}{\figure{uhecrauger2014-aitoff-galeq.pdf}{options: width=2.65in}\figure{astrocoordsystems-aitoff.pdf}{options: width=2.65in}}

This package comes with a small dataset containing Ultra High Energy Cosmic Ray events (from the \href{https://www.auger.org/index.php/document-centre/finish/115-data/2354-list-of-ultra-high-energy-cosmic-ray-events-2014}{Pierre Auger Observatory 2014 event list}), for example and test purposes.
}
\author{
Alberto Krone-Martins

Maintainer: Alberto Krone-Martins <algol@sim.ul.pt>
}
\keyword{ package }
\seealso{
  \code{\link{createAllSkyScatterPlotChart}},
  \code{\link{createAllSkyGridChart}},
  \code{\link{overplotScatterPlotInAllSkyGridChart}}
}
\examples{
# Creates an all sky chart with Equatorial, Ecliptic and Galactic coordinate systems
# superposed. Equatorial as the main system in black, the Ecliptic in blue and the 
# Galactic in red
createAllSkyGridChart(mainGrid="equatorial", eqDraw=TRUE, eclDraw=TRUE, galDraq=TRUE, 
  eqCol="black", eclCol=rgb(0,0,1,0.5), galCol=rgb(1,0,0,0.5), eqLty=1, eclLty=2, galLty=2)

# Creates an all sky scatter plot of the Pierre Auger Observatory 2014 public event list 
# (arXiv:1411.6111). Equatorial and Galactic coordinate systems superposed;
# Galactic as the main system in black and the Equatorial in red.
data(uhecrauger2014)
createAllSkyScatterPlotChart(uhecrauger2014$RA, uhecrauger2014$DEC, mainGrid="galactic",
  dataCoordSys="equatorial", pointcol="black", pch=19, 
  pointsize=uhecrauger2014$Eev/max(uhecrauger2014$Eev),
  eqDraw=TRUE, eclDraw=FALSE, galDraq=TRUE, galCol="black", eqLty=2, galLty=1, 
  eqCol=rgb(1,0,0,0.5))
  
# Creates a grid with the Galactic Coordinate system and overplots data
createAllSkyGridChart(mainGrid="galactic", eqDraw=FALSE, eclDraw=FALSE, galDraq=TRUE, 
  galCol=rgb(0,0,0,0.5), galLty=1)
overplotScatterPlotInAllSkyGridChart(uhecrauger2014$RA, uhecrauger2014$DEC, 
  mainGrid="galactic", dataCoordSys="equatorial", pointcol="black", pch=19, 
  pointsize=uhecrauger2014$Eev/max(uhecrauger2014$Eev))
}
