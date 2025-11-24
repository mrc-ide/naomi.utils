# Excel File Processor
# Opens an Excel file, waits for calculations to complete, saves and closes

param(
    [Parameter(Mandatory=$true)]
    [string]$FilePath,
    
    [Parameter(Mandatory=$false)]
    [int]$MaxWaitMinutes = 30
)

# Verify file exists
if (-not (Test-Path $FilePath)) {
    Write-Error "File not found: $FilePath"
    exit 1
}

# Get absolute path
$FilePath = Resolve-Path $FilePath

Write-Host "Opening Excel file: $FilePath"

# Create Excel COM object
$excel = New-Object -ComObject Excel.Application

try {
    # Make Excel visible (optional - set to $false for background processing)
    $excel.Visible = $true
    
    # Disable alerts to prevent popup dialogs
    $excel.DisplayAlerts = $false
    
    # Open the workbook
    $workbook = $excel.Workbooks.Open($FilePath)
    
    Write-Host "Workbook opened. Waiting for calculations to complete..."
    
    # Wait for calculations to complete
    $startTime = Get-Date
    $timeout = New-TimeSpan -Minutes $MaxWaitMinutes
    
    while ($excel.CalculationState -ne [Microsoft.Office.Interop.Excel.XlCalculationState]::xlDone) {
        Start-Sleep -Seconds 30
        
        $elapsed = (Get-Date) - $startTime
        if ($elapsed -gt $timeout) {
            Write-Warning "Timeout reached after $MaxWaitMinutes minutes. Proceeding to save anyway."
            break
        }
        
        Write-Host "Still calculating... (Elapsed: $([math]::Round($elapsed.TotalMinutes, 1)) minutes)"
    }
    
    Write-Host "Calculations complete. Saving file..."
    
    # Save the workbook
    $workbook.Save()
    
    Write-Host "File saved successfully."
    
    # Close the workbook
    $workbook.Close($false)
    
    Write-Host "Workbook closed."
}
catch {
    Write-Error "An error occurred: $_"
    exit 1
}
finally {
    # Quit Excel and clean up COM objects
    if ($excel) {
        $excel.Quit()
        [System.Runtime.Interopservices.Marshal]::ReleaseComObject($excel) | Out-Null
    }
    
    # Force garbage collection to ensure Excel process is released
    [System.GC]::Collect()
    [System.GC]::WaitForPendingFinalizers()
    
    Write-Host "Excel closed and resources released."
}

Write-Host "Script completed successfully."
exit 0