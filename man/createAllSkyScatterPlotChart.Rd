\name{createAllSkyScatterPlotChart}
\alias{createAllSkyScatterPlotChart}
\title{
Creates all-sky scatter plots
}
\description{
A function to create all-sky scatter plots with superposed gridlines of one or more astronomical coordinate systems.

The function will perform the convertion of the coordinate system to place the data and the grid on the same system. Please, note that you must specify which coordinate system the data is, as well as which is the coordinate system you want the plot to be. The function can create gridlines for the equatorial, ecliptic or galactic coordinate systems. Any spherical projection supported by the mapproj package is supported (but only the Aitoff, Mollweide and Mercator projections are routinely tested).
}
\usage{
createAllSkyScatterPlotChart(x, y, pointcol = rgb(0, 0, 0, 0.5), pointsize = 0.5, 
dataCoordSys = "equatorial", mainGrid = "equatorial", eqCol = "red", eclCol = "blue", 
galCol = "green", eqLty = 1, eclLty = 2, galLty = 3, eqLwd = 1, eclLwd = 1, galLwd = 1, 
eqDraw = TRUE, eclDraw = TRUE, galDraq = TRUE, projname = "aitoff", projparam = NULL, 
projorient = NULL, nGridpoints = 100, addLab=TRUE, label.cex=0.6, 
main = paste("All-Sky Scatter Plot (", projname, ")", sep = ""), ...)
}
\arguments{
  \item{x}{A vector with the data in degrees for the first coordinate (Right Ascention, for data in the Equatorial system, Ecliptic Longitude for the Ecliptic system or Galactic Longitude for the Galactic System).}
  \item{y}{A vector with the data in degrees for the second coordinate (Declination, for data in the Equatorial system, Ecliptic Latitude for the Ecliptic system or Galactic Latitude for the Galactic System).}
  \item{pointcol}{A scalar or a vector with, the same size of the x and y vectors, containing the color of the points.}
  \item{pointsize}{A scalar or a vector with, the same size of the x and y vectors, containing the sizes of the points.
}
  \item{dataCoordSys}{String. The name of the coordinate system of the x and y vector. It can take the following values: "equatorial", "ecliptic" or "galactic". It defaults to "equatorial".}
  \item{mainGrid}{String. The name of the the main coordinate system of the plot. It can take the following values: "equatorial", "ecliptic" or "galactic". It defaults to "equatorial".}
  \item{eqCol}{String. The color of the Equatorial coordinate system lines.}
  \item{eclCol}{String. The color of the Ecliptic coordinate system lines.}
  \item{galCol}{String. The color of the Galactic coordinate system lines.}
  \item{eqLty}{Numeric. The line type of the Equatorial coordinate system lines.}
  \item{eclLty}{Numeric. The line type of the Ecliptic coordinate system lines.}
  \item{galLty}{Numeric. The line type of the Galactic coordinate system lines.}
  \item{eqLwd}{Numeric. The line width of the Equatorial coordinate system lines.}
  \item{eclLwd}{Numeric. The line width of the Ecliptic coordinate system lines.}
  \item{galLwd}{Numeric. The line width of the Galactic coordinate system lines.}
  \item{eqDraw}{Logical. A boolean to indicate if the Equatorial coordinate system lines should be draw.}
  \item{eclDraw}{Logical. A boolean to indicate if the Ecliptic coordinate system lines should be draw.}
  \item{galDraq}{Logical. A boolean to indicate if the Galactic coordinate system lines should be draw.}
  \item{projname}{String. The spherical projection of the plot. It can take as argument any projection supported by the mapproj package, but only "aitoff", "mollweide" and "mercator" are tested automatically. It defaults to "aitoff".}
  \item{projparam}{Parameters to configure the projection. It defaults to NULL.}
  \item{projorient}{The orientation of the projection. It defaults to NULL.}
  \item{nGridpoints}{Numeric. The number of points used to draw each grid line. Defaults to 50.}
  \item{addLab}{Logical. A boolean to indicate if the labels should be added.}
  \item{label.cex}{Numeric. The size of the labels.}
\item{main}{String. The main title of the plot.}
  \item{\dots}{Additional parameters to be passed to \code{\link{points}}.}
}
\author{
Alberto Krone-Martins
}
\seealso{
  \code{\link{createAllSkyGridChart}},
  \code{\link{overplotScatterPlotInAllSkyGridChart}},
  \code{\link[graphics]{points}},
  \code{\link[mapproj]{mapproject}}.
}
\examples{
# Creates an all sky scatter plot of the Pierre Auger Observatory 2014 public event list 
# (arXiv:1411.6111), with Equatorial and Galactic coordinate systems superposed, and using
# the Galactic as the main system in black and the Equatorial in red with 0.5 transparency
# and dashed lines. The point sizes encode the energy of the cosmic ray.

data(uhecrauger2014)

createAllSkyScatterPlotChart(uhecrauger2014$RA, uhecrauger2014$DEC, mainGrid="galactic",
  dataCoordSys="equatorial", pointcol="black", pch=19, 
  pointsize=uhecrauger2014$Eev/max(uhecrauger2014$Eev),
  eqDraw=TRUE, eclDraw=FALSE, galDraq=TRUE, galCol="black", eqLty=2, galLty=1, 
  eqCol=rgb(1,0,0,0.5))
}
\keyword{ aplot }
\keyword{ dplot }
\keyword{ hplot }