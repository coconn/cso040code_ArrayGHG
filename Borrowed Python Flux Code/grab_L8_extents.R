myExt <- extent(390000, 430000, 4450000, 4490000)
myDir <- c("/Users/exnihilo/gis/LC80220322013144LGN00/", "/Users/exnihilo/gis/LC80220322013176LGN00/", "/Users/exnihilo/gis/LC80230322013247LGN00/")

earlyL8 <- stack_and_ndvi_landsat(myDir[1], myExt)
midL8 <- stack_and_ndvi_landsat(myDir[2], myExt)
lateL8 <- stack_and_ndvi_landsat(myDir[3], myExt)