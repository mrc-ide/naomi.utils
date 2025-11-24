# Batch Excel File Processor
# Processes all Excel files in a directory using the Excel processor script

param(
    [Parameter(Mandatory=$true)]
    [string]$DirectoryPath,
    
    [Parameter(Mandatory=$true)]
    [string]$ProcessorScriptPath,
    
    [Parameter(Mandatory=$false)]
    [int]$MaxWaitMinutes = 20,
    
    [Parameter(Mandatory=$false)]
    [switch]$Recurse
)

# Verify directory exists
if (-not (Test-Path $DirectoryPath -PathType Container)) {
    Write-Error "Directory not found: $DirectoryPath"
    exit 1
}

# Verify processor script exists
if (-not (Test-Path $ProcessorScriptPath -PathType Leaf)) {
    Write-Error "Processor script not found: $ProcessorScriptPath"
    exit 1
}

# Get all Excel files
$searchParams = @{
    Path = $DirectoryPath
    Filter = "*.xlsx"
}

if ($Recurse) {
    $searchParams.Recurse = $true
}

$excelFiles = Get-ChildItem @searchParams

if ($excelFiles.Count -eq 0) {
    Write-Warning "No .xlsx files found in: $DirectoryPath"
    exit 0
}

Write-Host "Found $($excelFiles.Count) Excel file(s) to process"
Write-Host "========================================"

# Initialize counters
$successful = 0
$failed = 0
$results = @()

# Process each file
foreach ($file in $excelFiles) {
    Write-Host ""
    Write-Host "Processing: $($file.Name)"
    Write-Host "----------------------------------------"
    
    $startTime = Get-Date
    
    try {
        # Run the processor script for this file
        & $ProcessorScriptPath -FilePath $file.FullName -MaxWaitMinutes $MaxWaitMinutes
        
        if ($LASTEXITCODE -eq 0 -or $null -eq $LASTEXITCODE) {
            $successful++
            $status = "Success"
            Write-Host "Successfully processed: $($file.Name)" -ForegroundColor Green
        }
        else {
            $failed++
            $status = "Failed"
            Write-Host "Failed to process: $($file.Name)" -ForegroundColor Red
        }
    }
    catch {
        $failed++
        $status = "Failed"
        Write-Host "Error processing: $($file.Name)" -ForegroundColor Red
        Write-Host "  Error: $_" -ForegroundColor Red
    }
    
    $elapsed = (Get-Date) - $startTime
    
    # Store result
    $results += [PSCustomObject]@{
        FileName = $file.Name
        FilePath = $file.FullName
        Status = $status
        TimeElapsed = $elapsed.ToString("mm\:ss")
    }
}

# Display summary
Write-Host ""
Write-Host "========================================"
Write-Host "PROCESSING SUMMARY"
Write-Host "========================================"
Write-Host "Total files: $($excelFiles.Count)"
Write-Host "Successful: $successful" -ForegroundColor Green
Write-Host "Failed: $failed" -ForegroundColor $(if ($failed -gt 0) { "Red" } else { "White" })
Write-Host ""
Write-Host "Detailed Results:"
$results | Format-Table -AutoSize

# Export results to CSV
$logPath = Join-Path $DirectoryPath "processing_log_$(Get-Date -Format 'yyyyMMdd_HHmmss').csv"
$results | Export-Csv -Path $logPath -NoTypeInformation
Write-Host ""
Write-Host "Results exported to: $logPath"

if ($failed -gt 0) {
    exit 1
}
else {
    exit 0
}