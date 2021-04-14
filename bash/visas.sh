#!/bin/bash

# $ ovisas.sh input_file output_file

# Converts provided file to text
pdftotext -layout $1 $2

# Removes empty lines
sed -i '/^$/d' $2 

# Removes extra spaces
sed -i 's/[ ]\+/ /g' $2

# Removes leading spaces
sed -i 's/^[ ]//g' $2 

# Removes commas
sed -i 's/,//g' $2 

# Removes repeating header lines
sed -i '/^.*Visa.*$/d' $2 
sed -i '/FY/d' $2 
sed -i '/Page/d' $2 

# Adds comma to last space in line
# Need four to cover all the variations
sed -E -i "s/\s([0-9]+$)/,\1/" $2
sed -E -i "s/\s([A-Z]*.,.*$)/,\1/" $2
sed -E -i "s/\s([A-Z]*.\/[A-Z].*,.*$)/,\1/" $2
sed -E -i "s/\s([A-Z][0-9][A-Z].*,.*$)/,\1/" $2



