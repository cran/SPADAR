\name{createAllSkyGridChart}
\alias{createAllSkyGridChart}
\title{
Creates all-sky grid charts
}
\description{
A function to create all-sky charts with gridlines of one or more astronomical coordinate systems. 

The function can create gridlines for the equatorial, ecliptic or galactic coordinate systems. Any spherical projection supported by the mapproj package is supported (but only the Aitoff, Mollweide and Mercator projections routinely are tested).
}
\usage{
createAllSkyGridChart(longitude = c(0, 45, 90, 135, 180, 225, 270, 315, 360), 
latitude = c(-75, -60, -45, -30, -15, 0, 15, 30, 45, 60, 75), mainGrid = "equatorial", 
eqCol = "red", eclCol = "blue", galCol = "green", eqLty = 1, eclLty = 2, galLty = 3, 
eqLwd = 1, eclLwd = 1, galLwd = 1, eqDraw = TRUE, eclDraw = TRUE, galDraq = TRUE, 
projname = "aitoff", projparam = NULL, projorient = NULL, npoints = 50, 
overplot = FALSE, addLab = TRUE, label.cex = 0.6, 
main = paste("All-Sky Grid (", projname, ")", sep = ""), ...)
}
\arguments{
  \item{longitude}{A vector with the positions of the first coordinate lines in degrees (Right Ascention, for data in the Equatorial system, Ecliptic Longitude for the Ecliptic system or Galactic Longitude for the Galactic System).}
  \item{latitude}{A vector with the positions of the second coordinate lines in degrees (Declination, for data in the Equatorial system, Ecliptic Latitude for the Ecliptic system or Galactic Latitude for the Galactic System).}
  \item{mainGrid}{String. The main coordinate system of the plot. It can take the following values: "equatorial", "ecliptic" or "galactic". It defaults to "equatorial".}
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
  \item{npoints}{Numeric. The number of points used to draw each grid line. Defaults to 50.}
  \item{overplot}{Logical. A boolean to indicate if the grid should be overploted or a new plot should be created.}
  \item{addLab}{Logical. A boolean to indicate if the labels should be added.}
  \item{label.cex}{Numeric. The size of the labels.}
  \item{main}{String. The main title of the plot.}
  \item{\dots}{Additional parameters to be passed to \code{\link{lines}}.}
}
\author{
Alberto Krone-Martins
}
\seealso{
  \code{\link{createAllSkyScatterPlotChart}},
  \code{\link{overplotScatterPlotInAllSkyGridChart}},
  \code{\link[graphics]{lines}},
  \code{\link[mapproj]{mapproject}}.
}
\examples{
# Creates an all sky chart with Equatorial, Ecliptic and Galactic coordinate systems superposed, 
# with the Equatorial as the main system in black, the Ecliptic in blue with 0.5 transparency and 
# dashed lines and the Galactic in red with 0.5 transparency and dashed lines
createAllSkyGridChart(mainGrid="equatorial", eqDraw=TRUE, eclDraw=TRUE, galDraq=TRUE, 
  eqCol="black", eclCol=rgb(0,0,1,0.5), galCol=rgb(1,0,0,0.5), eqLty=1, eclLty=2, galLty=2)

# Equatorial and Galactic coordinate systems represented in a polyconic projection
createAllSkyGridChart(mainGrid="equatorial", eqDraw=TRUE, eclDraw=FALSE, galDraq=TRUE, 
  eqCol="black", galCol=rgb(1,0,0,0.5), eqLty=1, galLty=2, projname="polyconic")

# The same as above, but in non-standard Aitoff projection with the South Pole 
# in the center og the projection
createAllSkyGridChart(mainGrid="equatorial", eqDraw=TRUE, eclDraw=TRUE, galDraq=TRUE, 
  eqCol="black", eclCol=rgb(0,0,1,0.5), galCol=rgb(1,0,0,0.5), eqLty=1, eclLty=2, 
  galLty=2, projname="aitoff", projorient=c(0,0,0))
}
\keyword{ aplot }
\keyword{ dplot }
\keyword{ hplot }