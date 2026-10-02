# Run once before launching the app
required <- c("shiny", "bslib", "plotly", "DT", "dplyr")
missing  <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) install.packages(missing)
cat("All required packages are installed.\n")
