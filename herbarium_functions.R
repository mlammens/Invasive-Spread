## ******************************************************************** ##
## Define some user-created functions here
## ******************************************************************** ##

## -------------------------------------------------------------------- ##
## Rounding function, used to assign decade values to records. This
## function is from the following post on stackoverflow
## http://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
round.up <- function(x,to=10) {
  to*(x%/%to + as.logical(x%%to))
}
## -------------------------------------------------------------------- ##
## Calculate the distance of 1 degree of Longitude for the 
## min and max latitudes in my extent
dist.lon <- function(lat.deg) {
  # Distance in km of one degree of latitude at the equator
  lat.1deg <- 111
  # Convert degrees to radians
  lat.rad <- lat.deg * (pi/180)
  # According to the following website, one degree of longitude
  # is the distance of one degree of latitude at the equator multiplied
  # by the cosine of the latitude:
  # http://www.colorado.edu/geography/gcraft/warmup/aquifer/html/distance.html
  lon.1deg <- lat.1deg * cos(lat.rad)
  return(lon.1deg)
}
## -------------------------------------------------------------------- ##
## Make a two column data.frame of the cumulative number of grid cells
## occupied for each year represented in a dataset
make.cummgrids.dfYrs <- function( record.df ) {
  ## Get year of first introduction for each grid cell that is 
  ## occupied
  Grid.Frst.Yr <- ddply(.data=record.df,
                        .variable=c("GridID"),
                        Grid.Frst.Yr=min(CollectionYear),
                        summarize)
  ## Remove NA Rows (points that dont fall into a grid cell)
  if ( length(which(is.na(Grid.Frst.Yr$GridID))) > 0 ) {
    Grid.Frst.Yr <- Grid.Frst.Yr[-which(is.na(Grid.Frst.Yr$GridID)),]
  }
  ## Sort Grid.Frst.Yr by year
  Grid.Frst.Yr <- Grid.Frst.Yr[order(Grid.Frst.Yr$Grid.Frst.Yr),]
  ## Add a column of the total grid cells occupied at that time point. Note
  ## that at this point, there is more than one value for each year, as there
  ## are more than one new grid cells occupied (thus adding to the cumulative
  ## total) per year for *some* years.
  Grid.Frst.Yr$CummGrids <- cumsum(rep(1,times=length(Grid.Frst.Yr$Grid.Frst.Yr)))
  ## Get the max number of occupied grid cells for any given year
  Grid.CummGrids.Yr <- ddply(.data=Grid.Frst.Yr,
                             .variable=c("Grid.Frst.Yr"),
                             Max.Grid=max(CummGrids),
                             summarize)
  return( Grid.CummGrids.Yr )
}
## -------------------------------------------------------------------- ##
## Create a new data.frame were there is a value for the number
## of grid cells occupied cumualtively through time 
make.cummgrids.allYrs <- function( cumm.grid.df ){
  # Convert first year to numeric value
  cumm.grid.df$Grid.Frst.Yr <- as.numeric( cumm.grid.df$Grid.Frst.Yr )
  first.year <- min( cumm.grid.df$Grid.Frst.Yr )
  last.year <- max( cumm.grid.df$Grid.Frst.Yr )
  Years <- first.year:last.year
  # This will yield a vector of length n-1, where n = length of cumm.grid.df$Grid.Frst.Yr
  diff.btw.obsYears <- diff( cumm.grid.df$Grid.Frst.Yr )
  # Add 1 to the end of this vector
  diff.btw.obsYears <- c(diff.btw.obsYears,1)
  OccGrids <- rep( cumm.grid.df$Max.Grid, times=diff.btw.obsYears )
  return(data.frame(Years=Years,OccGrids=OccGrids))
}
## -------------------------------------------------------------------- ##
## Create a new data.frame were there is a value for the number
## of records cumulatively through time 
make.cummrecs.allYrs <- function( cumm.recs.df ){
  # Convert Collection year to numeric value
  cumm.recs.df$CollectionYear <- as.numeric( cumm.recs.df$CollectionYear )
  first.year <- min( cumm.recs.df$CollectionYear )
  last.year <- max( cumm.recs.df$CollectionYear )
  Years <- first.year:last.year
  # This will yield a vector of length n-1, where n = length of cumm.recs.df$CollectionYear
  diff.btw.obsYears <- diff( cumm.recs.df$CollectionYear )
  # Add 1 to the end of this vector
  diff.btw.obsYears <- c(diff.btw.obsYears,1)
  CummRecs <- rep( cumm.recs.df$Cumm.Recs, times=diff.btw.obsYears )
  return(data.frame(Years=Years,CummRecs=CummRecs))
}
## -------------------------------------------------------------------- ##
## Make a two column data.frame of the cumulative number of **counties**
## occupied for each year represented in a dataset
make.cummCntys.dfYrs <- function( cnty.df ) {
  ## Get year of first introduction for each grid cell that is 
  ## occupied
  Cnty.Frst.Yr <- ddply(.data=cnty.df,
                        .variable=c("Admin2"),
                        Cnty.Frst.Yr=min(CollectionYear),
                        summarize)
  ## Sort Cnty.Frst.Yr by year
  Cnty.Frst.Yr <- Cnty.Frst.Yr[order(Cnty.Frst.Yr$Cnty.Frst.Yr),]
  ## Add a column of the total counties occupied at that time point. Note
  ## that at this point, there is more than one value for each year, as there
  ## are more than one new counties occupied (thus adding to the cumulative
  ## total) per year for *some* years.
  Cnty.Frst.Yr$CummCnty <- cumsum(rep(1,times=length(Cnty.Frst.Yr$Cnty.Frst.Yr)))
  ## Get the max number of occupied counties for any given year
  Cumm.Cnty.Yr <- ddply(.data=Cnty.Frst.Yr,
                        .variable=c("Cnty.Frst.Yr"),
                        Max.Cnty=max(CummCnty),
                        summarize)
  return( Cumm.Cnty.Yr )
}
## -------------------------------------------------------------------- ##
## Create a new data.frame were there is a value for the number
## of **counties** occupied cumualtively through time for every year
make.cummCntys.allYrs <- function( cumm.cnty.df ){
  # Convert first year to numeric value
  cumm.cnty.df$Cnty.Frst.Yr <- as.numeric( cumm.cnty.df$Cnty.Frst.Yr )
  first.year <- min( cumm.cnty.df$Cnty.Frst.Yr )
  last.year <- max( cumm.cnty.df$Cnty.Frst.Yr )
  Years <- first.year:last.year
  # This will yield a vector of length n-1, where n = length of cumm.cnty.df$Cnty.Frst.Yr
  diff.btw.obsYears <- diff( cumm.cnty.df$Cnty.Frst.Yr )
  # Add 1 to the end of this vector
  diff.btw.obsYears <- c(diff.btw.obsYears,1)
  OccCounties <- rep( cumm.cnty.df$Max.Cnty, times=diff.btw.obsYears )
  return(data.frame(Years=Years,OccCounties=OccCounties))
}
## -------------------------------------------------------------------- ##
## Write a function to calculate 'annual' slope values for the cubic linear fits
cube.growth <- function( cubic.mod, year ) {
  mod.coef = cubic.mod$coefficients
  slope = mod.coef[2] + (2*mod.coef[3]*year) + (3*mod.coef[4]*(year^2))
  slope = exp(slope)
  return(slope)
}
