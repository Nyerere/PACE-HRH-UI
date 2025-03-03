library(openxlsx)

# Produce all input files from region and scenario combinations
# NB: Each excel file must be manually opened and saved.

# Read the Excel file
wb <- loadWorkbook("config/master_input_sheet.xlsx")

# Get lists from Lookup sheet
lookup_sheet <- "Lookup"
scenarios <- read.xlsx(wb, sheet = lookup_sheet, cols = 14:14, rows = 1:20)  # Adjust rows as needed
scenarios
regions <- read.xlsx(wb, sheet = lookup_sheet, cols = 21:21, rows = 1:20)    # Adjust rows as needed
regions
# Remove any NA values
scenarios <- scenarios[!is.na(scenarios)]
regions <- regions[!is.na(regions)]
# Get the target worksheet for modifications
region_target_sheet <- "RegionSelect"
scenario_target_sheet <- "Scenarios"

#Loop through each region and scenario
for (region in regions) {
  for (scenario in scenarios) {
    cat(paste0(region,'_',scenario,'\n'))
    # modify region
    run_name <- paste0(region,'_',scenario)
    dir.create(file.path("config/region", run_name), showWarnings = TRUE)
    writeData(wb, sheet = region_target_sheet, x = region, xy = c(2, 2))
    writeData(wb, sheet = scenario_target_sheet, x = scenario, xy = c(1, 2))
    # Save after each modification
    filename <- sprintf("config/region/%s/mod_%s.xlsx", run_name, run_name)
    saveWorkbook(wb, filename, overwrite = TRUE)
    cat(sprintf("Processing Region: %s, Scenario: %s\n", region, scenario))
  }
}
cat("Processing complete!\n")

