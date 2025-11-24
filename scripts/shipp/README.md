# SHIPP

These scripts generate the SHIPP tool. You will need the Avenir Health Dropbox datasets synced on your machine. There are 3 scripts here

1. `generate_shipp_tools.R` - used to extract data from naomi.resources and fill into the SHIPP workbook template.
2. `open_and_calculate.ps1` - powershell script which will open a workbook, allow the calculations to run, save it and close it
3. `batch_processor.ps1` - powershell script which will run an arbitrary script across all excel files in a directory. Use to open, calculate and save all workbooks.


## open_and_calculate.ps1

To use the open and calculate script, open powershell and change directory via `cd` to this location. Run the script 
on a single file with `.\open_and_calculate.ps1 <path to file>`. To get the path, open the SHIPP directory
in file explorer, right click in the address bar and click "Copy address as text". Ensure it is quoted in the script e.g.

```
.\open_and_calculate.ps1 "C:\Users\Test\Avenir Health Dropbox\Avenir Shared Drive\DataSets\UNAIDS\2025 Estimates\SHIPP\AGO_2025_shipp.xlsx"
```

## batch_processor.ps1

To open and calculate all workbook, use the batch processor. This will run a script for all xlsx files in a directory.

```
.\batch_processor.ps1 "C:\Users\Test\Avenir Health Dropbox\Avenir Shared Drive\DataSets\UNAIDS\2025 Estimates\SHIPP\" .\open_and_calculate.ps1
```

When the batch processor completes it will save out a log file to the SHIPP directory showing success/failure for each file.
