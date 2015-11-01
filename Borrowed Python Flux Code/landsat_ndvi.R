#### Robert Paul

stack_and_ndvi_landsat <- function(landsat8_dir, map_extent)
{
	require("sp")
	require("rgdal")
	require("raster")
	require("foreach")
	
	# Bands we want from Landsat 8 data
	bandsL8 <- c('B2','B3','B4','B5','B6','B7')
	
	stackIt <- function(targetDir, bands)
	{
		# Build a list of files with our bands
		files <- foreach(i=1:length(bands)) %do%
			dir(path=targetDir, pattern=paste(bands[i], ".TIF", sep=""), full.names=T)
		
		# Stack can accept a list of filenames
		result <- stack(files, quick=T)
		result <- crop(result, map_extent)
		# We have some negative values, convert them to NA
		result[result<0] <- NA
		return(result)
	}
	
	# Calculates and returns NDVI from red and NIR rasters
	ndvi <- function(red, nir)
	{
		return((nir-red)/(nir+red))
	}
	
	stackL8 <- stackIt(landsat8_dir, bandsL8)
	
	# The stack bands are arranged in the order of our bandsL* character vector,
	# therefore we can recall them from the stack of raster layers using which()
	# Landsat 8, red = band 4, nir = band 5
	ndviL8 <- ndvi(red=stackL8[[which(bandsL8 == 'B4')]],
		nir=stackL8[[which(bandsL8 == 'B5')]])
	
	return(list(stackL8, ndviL8))
}
