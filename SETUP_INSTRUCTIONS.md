# Setup Instructions

Welcome to the Women's Basketball March Madness Bradley-Terry Model Analysis project!

## Prerequisites

### Required Software

1. **R** (version 4.0 or higher)
   - Download from: https://cran.r-project.org/

2. **RStudio** (recommended but optional)
   - Download from: https://posit.co/download/rstudio-desktop/

3. **Git** (for version control)
   - Download from: https://git-scm.com/downloads

### Optional Software

- **LaTeX** (for PDF report generation)
  - Windows: MiKTeX (https://miktex.org/)
  - Mac: MacTeX (https://tug.org/mactex/)
  - Linux: TeX Live (`sudo apt-get install texlive-full`)

## Installation Steps

### Step 1: Clone or Download the Project

If using Git:
```bash
cd ~/Desktop/stat479
git clone <repository-url> WBB-March-Madness-Model
cd WBB-March-Madness-Model
```

If downloaded as ZIP:
- Extract to `~/Desktop/stat479/WBB-March-Madness-Model`

### Step 2: Install Required R Packages

Open R or RStudio and run:

```r
# Install CRAN packages
install.packages(c(
  "tidyverse",
  "here",
  "devtools",
  "ggplot2",
  "patchwork",
  "knitr",
  "rmarkdown",
  "kableExtra",
  "lubridate"
))

# Install GitHub packages
devtools::install_github("snestler/wncaahoopR")
devtools::install_github("hturner/BradleyTerry2")
```

**Note:** The master script `run_all.R` will also check and install packages automatically.

### Step 3: Verify Directory Structure

Ensure the following directories exist:
```
WBB-March-Madness-Model/
├── data/
│   ├── raw/
│   └── processed/
├── scripts/
├── results/
│   ├── figures/
│   └── tables/
└── reports/
```

If any are missing, create them:
```r
dir.create("data/raw", recursive = TRUE)
dir.create("data/processed", recursive = TRUE)
dir.create("results/figures", recursive = TRUE)
dir.create("results/tables", recursive = TRUE)
```

## Running the Analysis

### Option 1: Run Complete Pipeline (Recommended)

Open R/RStudio in the project directory and run:

```r
source("run_all.R")
```

This will:
1. Collect and clean data
2. Fit Bradley-Terry models
3. Analyze 8-12 seed probabilities
4. Run Monte Carlo simulations (5000 tournaments)
5. Generate visualizations
6. Create final report

**Expected Runtime:** 15-30 minutes (depending on your computer and data size)

### Option 2: Run Scripts Individually

If you want more control or need to troubleshoot:

```r
# Step 1: Data Collection
source("scripts/01_data_collection.R")

# Step 2: Model Fitting
source("scripts/02_bradley_terry_model.R")

# Step 3: Seed Analysis
source("scripts/03_seed_analysis.R")

# Step 4: Tournament Simulation
source("scripts/04_tournament_simulation.R")

# Step 5: Visualization
source("scripts/05_visualization.R")

# Step 6: Generate Report
rmarkdown::render("reports/final_report.Rmd")
```

## Troubleshooting

### Issue: wncaahoopR Package Not Found

The `wncaahoopR` package may have specific function names that differ from what we've used. Check the package documentation:

```r
help(package = "wncaahoopR")
```

You may need to adjust the function names in `01_data_collection.R` based on the actual package API.

### Issue: Data Collection Fails

If the wncaahoopR package is not working as expected:

1. **Check the package repository:** https://github.com/snestler/wncaahoopR
2. **Look for alternative data sources** or use pre-downloaded CSV files
3. **Contact the package maintainer** for support

**Workaround:** If you have existing game data in CSV format:
- Place it in `data/raw/`
- Modify `01_data_collection.R` to read from your CSV files instead

### Issue: BradleyTerry2 Installation Fails

Try installing from CRAN instead:
```r
install.packages("BradleyTerry2")
```

### Issue: Simulations Take Too Long

In `04_tournament_simulation.R`, reduce the number of simulations:
```r
N_SIMS <- 1000  # Instead of 5000
```

### Issue: Report Generation Fails

If PDF generation fails (usually due to missing LaTeX):
- Just use the HTML report: `final_report.html`
- Or install LaTeX (see prerequisites)

### Issue: Memory Errors

If you encounter memory issues during simulation:
1. Close other applications
2. Reduce `N_SIMS` in `04_tournament_simulation.R`
3. Comment out saving of `all_simulations.rds` (large file)

## Customization

### Changing Seasons

Edit `01_data_collection.R`:
```r
SEASONS <- c(2023, 2024, 2025)  # Modify years
```

### Changing Seed Focus

To analyze different seeds (e.g., 5-7 instead of 8-12):
- Edit filter conditions in `03_seed_analysis.R`
- Update `seed >= 8 & seed <= 12` to your range

### Adjusting Visualizations

All plot settings are in `05_visualization.R`:
- Colors: Modify `oi_colors` vector
- Size: Change `width` and `height` in `ggsave()`
- Theme: Change `theme_set()` at the top

## Getting Help

### Project-Specific Help

- Review the README.md
- Check comments in individual scripts
- Examine intermediate output files in `data/processed/`

### R Package Help

```r
?BTm  # Bradley-Terry model help
?ggplot  # Visualization help
?dplyr  # Data manipulation help
```

### External Resources

- Bradley-Terry Models: https://cran.r-project.org/web/packages/BradleyTerry2/
- NCAA WBB Stats: https://www.ncaa.com/stats/basketball-women/d1
- R for Data Science: https://r4ds.had.co.nz/

## Contact

For questions about this project:
- Instructor: STAT 479 professor
- Office Hours: [Check course schedule]

## License

MIT License - Free to use and modify for educational purposes.

---

**Good luck with your analysis!** 🏀📊

