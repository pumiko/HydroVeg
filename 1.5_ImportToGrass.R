library(spgrass6)

TIF_date = function(file) 
{ 
  date = substr(file, 6, 15)
  gsub('-', '.', date)
}

## Funkcja importujaca pliki tiff do GRASSA
ModisImport = function()
{
  # Wybierz pliki ktore nalezy zaimportowac do GRASS
  directory = "/path/to/HydroVeg/S_max/"
  tif_files = list.files(paste(directory),pattern = "tif")
  
  # Zaimportuj pliki do GRASS i nadaj im odpowiednie nazwy
  for (tif in tif_files)
  {
    raster_date = TIF_date(tif)
    input=paste(directory,tif,sep="")
    
    initGRASS(gisBase ='/path/to/grass/gcc/6.4.4/grass-6.4.4', location = 'MCD15A3_SMAX', mapset = 'RAW', gisDbase = '/path/to/HydroVeg', override = TRUE)
      
    parameters = list(input=input, output=paste("SMAX.",raster_date,sep=""))
    execGRASS("r.in.gdal", parameters = parameters, flags=c("overwrite","quiet"))  

  }
}

ModisImport()
