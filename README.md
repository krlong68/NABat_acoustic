# NABat_acoustic

This repository contains a script for transforming metadata from Bats
Northwest's mobile surveys into a format ready to upload to NABat as mobile
transect metadata. This script is run after performing preliminary analysis and
metadata adjustments in Sonobat.

### How to Use

1. Clone this repository. Alternatively, you can download
`reformat.R` individually and specify the relevant input and output directories
within the script.
2. Place input TXT files into the `sonobat_txt/` directory under year and
grid-specific subdirectories. Refer to `sonobat_txt/README.md` for detailed
organization instructions. These TXT files will have been prepared following
Bats Northwest's mobile survey processing procedures up through Sonobat
attributing and noise scrubbing, Sonobatch automatic identification, and Sonovet
manual vetting.
3. Prepare year and grid-specific subdirectories under the `out_csv/` directory
for output files. Refer to `out_csv/README.md` for detailed organization
instructions.
4. Edit the specified variables in `reformat.R`.
5. Run the script: this can be done line-by-line using an IDE such as RStudio or
as a single run command in the terminal.
    ```
    $ Rscript reformat.R
    ```
6. Find your newly created output files in the directory created in step 3.
These files are ready to upload to NABat.