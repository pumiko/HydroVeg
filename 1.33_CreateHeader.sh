#!/bin/bash

#Przeksztalcanie plikow wyjsciowych z porgramu TIMESAT na IDRISI image
#Tworzenie Header files
#Konwersja pliku binarnego na plik IDRISI

dirname=/path/to/HydroVeg/pliki_lai_fit/proba
filelist=`ls ${dirname}/Lai_*.rst` 
suffix=.rst

for file in $filelist
do
filename=${file%$suffix}
echo  "file format : Idrisi Raster A.1
file title  :
data type   : byte
file type   : binary
columns     : 83
rows        : 83
ref. system : utm-33n
ref. units  : m
unit dist.  : 1
min. X      : 573000.0000000
max. X      : 656000.0000000
min. Y      : 5544500.0000000
max. Y      : 5627500.0000000
pos'n error : unspecified
resolution  : 1000.0000000
min. value  : 0
max. value  : 254
display min : 0
display max : 254
value units : unspecified
value error : unspecified
flag value  : 255
flag def'n  : none
legend cats : 0
lineage     :
comment     :" > ${filename}.rdc


echo "<PAMDataset>
  <PAMRasterBand band="1">
    <Metadata>
        <MDI key="STATISTICS_MINIMUM">0</MDI>
        <MDI key="STATISTICS_MAXIMUM">254</MDI>
        <MDI key="STATISTICS_MEAN">0</MDI>
        <MDI key="STATISTICS_STDDEV">0</MDI>
    </Metadata>
  </PAMRasterBand>
</PAMDataset>" > ${filename}.rst.aux.xml 
done


