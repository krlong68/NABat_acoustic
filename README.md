# NABat_acoustic

This repository contains a script for transforming metadata from Bats
Northwest's mobile surveys into a format ready to upload to NABat as mobile
transect metadata. This script is run after performing preliminary analysis and
metadata adjustments in SonoBat.

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
  - Working directory
  - Year the survey was performed
  - ID of the grid the survey was performed in
  - Species list used for both automatic and manual species identification
5. Run the script: this can be done line-by-line using an IDE such as RStudio or
as a single run command in the terminal.
    ```
    $ Rscript reformat.R
    ```
6. Find your newly created output files in the directory created in step 3.

### Checking the Output File

This script automatically checks for the following date-related errors. If any
of these errors are found, they will be recorded in an `Errors` column and the
output file name will be prefixed with `Errors_`.

1. Do all date-time values occur within the provided year?
2. Do all date-time values occur on the same date?
3. Do all date-time values occur in the correct order?
(Survey Start -> Recording Timestamp -> Survey End)

If any of these errors are noted in the output file, you can either correct them
in the input file and re-run the script, or correct them in the output file and
delete the `Errors` column. Then the file is ready to be uploaded to NABat.