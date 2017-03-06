## Przeliczanie LAI na wartoscc S_max

library(rgdal)
library(raster)

wd = "/path/to/HydroVeg"
lai_wd = "/path/to/HydroVeg/pliki_lai_fit"
output_wd = "/path/to/HydroVeg/S_max"

lista = list.files(lai_wd, "*.rst", full.names = T)
lista.aux.xml = list.files(lai_wd, "*.rst.aux.xml", full.names = T)
lista = setdiff(lista,lista.aux.xml)

## Proba
pokrycie_terenu = raster(paste(wd,"/f.tif",sep=""))

#plik = lista[2]
#raster = raster(plik)

for (plik in lista) {
  raster = raster(plik)
  raster_1 = raster * 0.1 #obliczenie faktycznej wartosci LAI
  raster_2 = raster_1 + 1 #nawias
  raster_logarytm = log(raster_2) #logarytm
  S_max = raster_logarytm * pokrycie_terenu #wspolczynnik f
  nazwa = paste("/SMAX",substr(plik,57,67),".tif",sep="")
  writeRaster(S_max, filename=paste(output_wd,nazwa,sep=""), format="GTiff")
}
