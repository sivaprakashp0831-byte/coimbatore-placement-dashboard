# Coimbatore Higher Education Placement Insights Dashboard

A premium R Shiny dashboard (bslib + plotly + DT) with a dark sidebar, four tabs and
global filters for placement year, institution tier and stream.

## Requirements
- R 4.1 or newer
- Packages: shiny, bslib (>= 0.7), plotly, DT, dplyr

## Run
1. Install packages (once): `source("install_packages.R")`
2. Launch from the project folder: `shiny::runApp()`

## Data
`simulated_data.csv` is generated automatically on first launch with a fixed seed
(14 Coimbatore colleges x streams x 2023-2026), so results are reproducible.
Delete the file to regenerate it, or replace it with your own data using the same columns:
record_id, college, short_name, institution_type, annual_fee_lakh, year, stream,
students, placed, placement_rate, avg_package, highest_package.
Streams must be one of: IT/CS, Core Engineering, Commerce.

All values are simulated for demonstration and are not official college statistics.

## Tabs
1. Executive Summary: KPI cards, package trend, placement rate by tier, top 5 colleges, stream mix.
2. Placement Insights: boxplots, density curves, stream salary summary, tier vs stream comparison.
3. College Comparison: choose 2 to 3 colleges for side-by-side charts and scorecard.
4. Top Performers: top 5/10 leaderboard with budget, minimum placement rate and ranking filters.