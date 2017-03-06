#!/bin/bash
module load gdal 

# PART I - File Management
dirname=/path/to/MODIS/MCD15A3/
dirname_raw_lai=/path/to/MODIS/MCD15A3/Lai/
dirname_raw_qa=/path/to/MODIS/MCD15A3/FparLai_QC/
dirname_lai=/path/to/MODIS/MCD15A3/pliki_lai/
dirname_qa=/path/to/MODIS/MCD15A3/pliki_qa/
dirname_hdf=/path/to/MODIS/MCD15A3/HDF/
dirname_work=/path/to/MODIS/

lai_list=`ls ${dirname_raw_lai}MCD15A3_h19v03_*`
qa_list=`ls ${dirname_raw_qa}MCD15A3_h19v03_*`

for file in $lai_list
do
  file_date=${file#${dirname_raw_lai}MCD15A3_h19v03_}
  file_date=${file_date%".Lai_1km.tif"}
  echo $file_date
  gdal_translate -q -of RST "$file" "$dirname_lai"Lai_"$file_date".rst
done

for file in $qa_list
do
  file_date=${file#${dirname_raw_qa}MCD15A3_h19v03_}
  file_date=${file_date%".FparLai_QC.tif"}
  echo $file_date
  gdal_translate -q -of RST "$file" "$dirname_qa"QA_"$file_date".rst 
done

dirname_lai=/path/to/pliki_lai/
dirname_qa=/path/to/pliki_qa/
dirname_work=/path/to/
dirname_lai_fit=/path/to/pliki_lai_fit/

# PART II - Create File List
ls ${dirname_lai}*.rst | wc -l > ${dirname_work}lai_list.txt
ls ${dirname_qa}*.rst | wc -l > ${dirname_work}qa_list.txt
ls -d -1 ${dirname_lai}*.rst >> ${dirname_work}lai_list.txt
ls -d -1 ${dirname_qa}*.rst >> ${dirname_work}qa_list.txt

# PART III - Use TIMESAT
# step 1
/path/to/timesat311/timesat_fortran/main/TSF_process.x64 HydroVeg.set

# step 2
/path/to/timesat311/timesat_fortran/tools/TSF_fit2img.x64 "/path/to/Lai_fit.tts" 255 Lai 1 -1

# PART IV - Give file names based on dates
Rscript ./Give_names.R

# PART V Create Header Files
# Skrypt







