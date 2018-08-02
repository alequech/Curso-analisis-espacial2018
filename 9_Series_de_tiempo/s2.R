library(bfastSpatial)
data(tura)
head(getZ(tura))



targcell <- 3492
bfm <- bfmPixel(tura, cell=targcell, start=c(2009, 1))
bfm$bfm
plot(bfm$bfm)



tura2 <- tura[[which(getZ(tura) >= as.Date("1999-12-31"))]]


bfm <- bfmPixel(tura2, cell=targcell, start=c(2009, 1))
bfm$bfm
plot(bfm$bfm)


bfm1 <- bfmPixel(tura, cell=targcell, start=c(2009, 1), 
                 formula=response~harmon, plot=TRUE)

bfm2 <- bfmPixel(tura, cell=targcell, start=c(2009, 1), 
                 formula=response~harmon, order=1, plot=TRUE)

bfm3 <- bfmPixel(tura, cell=targcell, start=c(2009, 1), 
                 formula=response~trend, plot=TRUE)


bfm4 <- bfmPixel(tura, cell=targcell, start=c(2009, 1), 
                 monend=c(2010, 1), plot=TRUE)

bfm5 <- bfmPixel(tura, cell=targcell, start=c(2009, 1), 
                 sensor="ETM+", plot=TRUE)



#t1 <- system.time(
  bfm <- bfmSpatial(tura, start=c(2009, 1), order=1)
#)
# plot the result
plot(bfm)



#t2 <- system.time(
  bfm <- bfmSpatial(tura2, start=c(2009, 1), order=1, mc.cores=1)
#)
# compare processing time with previous run
#t1 - t2


plot(bfm)

change <- raster(bfm, 1)


months <- changeMonth(change)

monthlabs <- c("jan", "feb", "mar", "apr", "may", "jun", 
               "jul", "aug", "sep", "oct", "nov", "dec")
cols <- rainbow(12)
plot(months, col=cols, breaks=c(1:12), legend=FALSE)

legend("bottomright", legend=monthlabs, cex=0.5, fill=cols, ncol=2)

# extract magn raster
magn <- raster(bfm, 2)
# make a version showing only breakpoing pixels
magn_bkp <- magn
magn_bkp[is.na(change)] <- NA
op <- par(mfrow=c(1, 2))
plot(magn_bkp, main="Magnitude: breakpoints")
plot(magn, main="Magnitude: all pixels")

par(op)

magn <- raster(bfm, 2) / 10000



months <- changeMonth(change)
# set up labels and colourmap for months
monthlabs <- c("jan", "feb", "mar", "apr", "may", "jun", 
               "jul", "aug", "sep", "oct", "nov", "dec")
cols <- rainbow(12)
plot(months, col=cols, breaks=c(1:12), legend=FALSE)
# insert custom legend
legend("bottomright", legend=monthlabs, cex=0.5, fill=cols, ncol=2)


magn <- raster(bfm, 2)
# make a version showing only breakpoing pixels
magn_bkp <- magn
magn_bkp[is.na(change)] <- NA
op <- par(mfrow=c(1, 2))
plot(magn_bkp, main="Magnitude: breakpoints")
plot(magn, main="Magnitude: all pixels")


bfm09 <- bfmSpatial(tura, start=c(2009, 1), monend=c(2010, 1), order=1)
# extract change
change09 <- raster(bfm09, 1)
# extract and rescale magnitude
magn09 <- raster(bfm09, 2) / 10000
# remove all non-breakpoints
magn09[is.na(change09)] <- NA


# extract and rescale magnitude and apply a -500 threshold
magn09thresh <- magn09
magn09thresh[magn09 > -0.05] <- NA
# compare
op <- par(mfrow=c(1, 2))
plot(magn09, main="magnitude")
plot(magn09thresh, main="magnitude < -0.05")

par(op)

magn09_sieve <- areaSieve(magn09thresh, thresh=1800)

magn09_areasieve <- areaSieve(magn09thresh)

magn09_as_rook <- areaSieve(magn09thresh, directions=4)
# compare all magn rasters
op <- par(mfrow=c(2, 2))
plot(magn09thresh, main="magnitude")
plot(magn09_sieve, main="pixel sieve")
plot(magn09_areasieve, main="0.5ha sieve")
plot(magn09_as_rook, main="0.5ha sieve, rook's case")

par(op)

changeSize_queen <- clumpSize(magn09_areasieve)
changeSize_rook <- clumpSize(magn09_areasieve, directions=4)
# compare
op <- par(mfrow=c(1, 2))
plot(changeSize_queen, col=bpy.colors(50), main="Clump size: Queen's case")
plot(changeSize_rook, col=bpy.colors(50), main="Clump size: Rook's case")

par(op)

changeSize <- clumpSize(magn09_areasieve, f=900/10000)
plot(changeSize, col=bpy.colors(50), main="Clump size (hectares)")
print(changeSize$stats)


changeSize <- clumpSize(magn09_areasieve, f=900/10000, stats=TRUE)
print(changeSize$stats)


