\name{overplotScatterPlotInAllSkyGridChart}
\alias{overplotScatterPlotInAllSkyGridChart}
\title{
Overplots positions in some coordinate system on top of all-sky chart plots.
}
\description{

A function to overplot points represented some astronomical coordinate system on top of all-sky charts in any coordinate system and projection. The function will perform the convertion of the data coordinate system to match the chart coordinate system, but this must be specified by the user. Any spherical projection supported by the mapproj package is supported (but only the Aitoff, Mollweide and Mercator projections are routinely tested).

}
\usage{
overplotScatterPlotInAllSkyGridChart(x, y, pointcol = rgb(0, 0, 0, 0.5), 
pointsize = 0.5, dataCoordSys = "equatorial", mainGrid = "equatorial", 
projname = "aitoff", projparam = NULL, projorient = NULL, ...)
}
\arguments{
  \item{x}{A vector with the data in degrees for the first coordinate (Right Ascention, for data in the Equatorial system, Ecliptic Longitude for the Ecliptic system or Galactic Longitude for the Galactic System).}
  \item{y}{A vector with the data in degrees for the second coordinate (Declination, for data in the Equatorial system, Ecliptic Latitude for the Ecliptic system or Galactic Latitude for the Galactic System).}
  \item{pointcol}{A scalar or a vector with, the same size of the x and y vectors, containing the color of the points.}
  \item{pointsize}{A scalar or a vector with, the same size of the x and y vectors, containing the sizes of the points.
}
  \item{dataCoordSys}{String. The name of the coordinate system of the x and y vector. It can take the following values: "equatorial", "ecliptic" or "galactic". It defaults to "equatorial".}
  \item{mainGrid}{String. The name of the the main coordinate system of the plot. It can take the following values: "equatorial", "ecliptic" or "galactic". It defaults to "equatorial".}
  \item{projname}{A string with the spherical projection of the plot. It can take as argument any projection supported by the mapproj package, but only "aitoff", "mollweide" and "mercator" are tested automatically. It defaults to "aitoff".}
  \item{projparam}{Parameters to configure the projection. It defaults to "NULL", as they are not needed for "aitoff".}
  \item{projorient}{The orientation of the projection.}
  \item{\dots}{Additional parameters to be passed to \code{points}.}
}
\author{
Alberto Krone-Martins
}
\seealso{
  \code{\link{createAllSkyScatterPlotChart}},
  \code{\link{createAllSkyGridChart}}
}
\examples{
# Creates a grid with the Galactic Coordinate system.

createAllSkyGridChart(mainGrid="galactic", eqDraw=FALSE, eclDraw=FALSE, galDraq=TRUE, 
  galCol=rgb(0,0,0,0.5), galLty=1)

# Overplots the Pierre Auger Observatory 2014 public event list (arXiv:1411.6111).

data(uhecrauger2014)

overplotScatterPlotInAllSkyGridChart(uhecrauger2014$RA, uhecrauger2014$DEC, 
  mainGrid="galactic", dataCoordSys="equatorial", pointcol="black", pch=19, 
  pointsize=uhecrauger2014$Eev/max(uhecrauger2014$Eev))
}
\keyword{ aplot }
\keyword{ dplot }
\keyword{ hplot }