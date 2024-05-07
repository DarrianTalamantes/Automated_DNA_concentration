# This ensures the proper directories are made
Home=$(pwd)
int_files=$Home/int_files
output=$Home/output
if [ ! -e $int_files ] ; then mkdir $int_files; fi
if [ ! -e $output ] ; then mkdir $output; fi

read -p "Input .csv of 4 columns, Well, Sample, Intensity, Concentration." infile
infile2="${infile:1:-1}"
echo 
echo 
read -p "Input dilution factor, 1 to " dilution
echo 
echo "Running analysis now"

./Concentration_finder.R $infile2 $dilution 
mv *.png output/
mv DNA_Concentrations.csv output/
