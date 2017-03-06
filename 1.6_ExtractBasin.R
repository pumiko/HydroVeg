library(spgrass6)

# Funckja podajaca daty
SMAX_date = function(SMAX)
{ 
  substr(SMAX, 6, 15)
}

# Funckja do docinania rastrow Smax do zlewni
ExtractBasin = function()
{
  initGRASS(gisBase ='/path/to/grass/gcc/6.4.4/grass-6.4.4', location = 'MCD15A3_SMAX', mapset = 'RAW', gisDbase = '/path/to/HydroVeg', override = TRUE)
  
  execGRASS("g.region", rast="wzor")
  
  SMAX_try = execGRASS("g.mlist", parameters = list(type = "rast", pattern = "SMAX.*"))
  SMAX = attributes(SMAX_try)$resOut
  
  basins_try = execGRASS("g.mlist", parameters = list(type = "rast", pattern = "z_*", mapset="PERMANENT"))
  basins = attributes(basins_try)$resOut
  
  # Przycinanie rastrow LAI i FPAR
  for (basin in basins)
  {
    for (raster in SMAX)
    {	
      mapset_name=substr(basin, 3, 15)
      output=paste("SMAX.",SMAX_date(raster),"_",mapset_name,sep="")
      copied_name=paste("SMAX.",SMAX_date(raster),sep="")
      copied=paste(output,"@RAW,",copied_name, sep="")		
      
      execGRASS("g.region", rast = "wzor")
      execGRASS("r.mask", flags=c("o"), input = paste(basin))
      execGRASS("r.resample", flags=c("overwrite"), input=paste(raster), output=output)
      cat(copied_name)

      execGRASS("g.mapset", mapset=paste(mapset_name), flags=c("quiet"))
      execGRASS("g.copy", rast=paste(copied))
      execGRASS("g.mapset", mapset="RAW", flags=c("quiet")) 
      execGRASS("g.remove", rast=paste(output), flags=c("quiet"))
    }   
  }
  
  # Usuniecie maski
  execGRASS("r.mask", flags=c("r"))
}  

# Uruchomienie funkcji
ExtractBasin()