library(spgrass6)

  initGRASS(gisBase ='/path/to/grass/gcc/6.4.4/grass-6.4.4', location = 'MCD15A3_SMAX', mapset = 'PERMANENT', gisDbase = '/path/to/HydroVeg', override = TRUE)
  mapsets = c("bardo", "bystrzyca", "klodzko", "ladek", "miedzylesie", "scinawka", "szalejow", "szczytna", "zelazno")
  
  SMAX_try = execGRASS("g.mlist", type="rast", mapset=mapsets[1], pattern="SMAX.*")
  SMAX = attributes(SMAX_try)$resOut
  cat(SMAX)

  for (mapset in mapsets)
  {
    ## SMAX
    for (raster in SMAX)
    {
      execGRASS("g.mapset", mapset=paste(mapset), flags=c("quiet"))
      
      execGRASS("r.out.gdal", input=paste(raster), format="GTiff", type="Float64", output=paste("/path/to/HydroVeg/Rastry_S_max/",mapset,"/",raster,"_",mapset,".tif",sep=""), flags=c("quiet"))
      
      execGRASS("g.mapset", mapset="PERMANENT", flags=c("quiet"))
    }
  }
