# Women's Basketball March Madness: Bradley-Terry Model Analysis

## Project Overview
This project applies Bradley-Terry models to analyze the NCAA Women's Basketball March Madness tournament, with a focus on understanding mid-tier seed advancement probabilities.

## Research Questions
1. **Primary Question**: How many of the 8-12 seeds should we expect to advance past the second round?
2. **Secondary Question**: Given that a team ranked 8-12 advanced past the second round, what is the chance they make it all the way to the finals or win the championship?

## Methodology
We use the Bradley-Terry model to estimate latent team strengths from game results. The model asserts that:

P(i beats j) = 1 / (1 + exp(-(λᵢ - λⱼ)))

Where λᵢ and λⱼ represent the latent strengths of teams i and j.

## Data Source
- **Package**: `wehoop` - Women's Basketball data package
- **Repository**: https://github.com/sportsdataverse/wehoop
- **Scope**: Regular season games (2019, 2021-2024 seasons)
  - Note: 2020 season excluded due to COVID-19 disruptions
  - Multi-season data provides more robust team strength estimates
  - Tournament seeding based on most recent season only

## Important Note: Tournament Seeding Methodology

⚠️ **Tournament Seeds**: By default, this analysis uses **model-based rankings** (not actual NCAA committee seeds).

### Current Implementation:
- **Seeds assigned by**: Regular-season win percentage (top 64 teams ranked 1-64, grouped as seeds 1-16 in 4 regions)
- **Interpretation**: Results show "what if tournament was seeded purely by model strength estimates"
- **NOT**: Analysis of actual NCAA committee decisions or historical tournament matchups

### Why This Matters:
- NCAA committee considers: strength of schedule, conference performance, "eye test", injuries, geography
- Our model considers: only regular-season game outcomes
- **Result**: Model seeds ≠ NCAA seeds (often significantly different)

### Using Real NCAA Seeds:
To analyze actual NCAA tournament performance:
1. Create `data/raw/ncaa_seeds_historical.csv` with columns: `season, team, ncaa_seed, region`
2. Ensure team names match wehoop data exactly
3. Re-run pipeline - code automatically detects and uses real seeds

### Interpretation Guidelines:
- With **model-based seeds**: Treat as predictive "what-if" analysis
- With **real NCAA seeds**: Can analyze actual committee seeding vs. model predictions
- All probabilities represent expected performance under Bradley-Terry model assumptions

## Project Structure
```
WBB-March-Madness-Model/
├── README.md                           # This file
├── scripts/
│   ├── 01_data_collection.R           # Data scraping and preparation
│   ├── 02_bradley_terry_model.R       # Model fitting and estimation
│   ├── 03_seed_analysis.R             # Analysis of 8-12 seed probabilities
│   ├── 04_tournament_simulation.R     # Monte Carlo tournament simulations
│   └── 05_visualization.R             # Plotting and results visualization
├── data/
│   ├── raw/                           # Raw scraped data
│   └── processed/                     # Cleaned and processed data
├── results/
│   ├── figures/                       # Generated plots
│   └── tables/                        # Summary statistics tables
└── reports/
    └── final_report.Rmd               # Final project report
```

## Installation and Setup

### Required R Packages
```r
# Install required packages
install.packages(c("tidyverse", "BradleyTerry2", "here"))

# Install wncaahoopR from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("snestler/wncaahoopR")
```

### Running the Analysis
1. Collect data: `source("scripts/01_data_collection.R")`
2. Fit Bradley-Terry model: `source("scripts/02_bradley_terry_model.R")`
3. Analyze seed probabilities: `source("scripts/03_seed_analysis.R")`
4. Run simulations: `source("scripts/04_tournament_simulation.R")`
5. Generate visualizations: `source("scripts/05_visualization.R")`

## Key Components

### Bradley-Terry Model
- Estimates latent team strength (λ) for each team
- Uses regular season game results
- Accounts for all head-to-head matchups

### Tournament Simulation
- Monte Carlo simulation of March Madness brackets
- Probability calculations for specific seed advancement
- Analysis of upset scenarios

### Focus on 8-12 Seeds
We specifically examine mid-tier seeds because:
- They represent competitive teams with realistic upset potential
- Not automatic first-round exits (like 13-16 seeds)
- Not overwhelming favorites (like 1-4 seeds)
- Interesting sweet spot for competitive analysis

## Expected Outputs
1. Team strength estimates (λ) for all D1 women's basketball teams
2. Probability distributions for 8-12 seed advancement by round
3. Expected number of 8-12 seeds in Sweet 16, Elite 8, Final Four
4. Conditional probabilities of championship given second-round advancement
5. Visualizations of team strengths and tournament simulations

## Course Information
- **Course**: STAT 479 - Sports Analytics
- **Institution**: University of Wisconsin-Madison
- **Project**: Project 2 - Bradley-Terry Models

## Authors
[Kalynn, Jasmine, Ellie]

## References
- Nesbitt, S. (2024). wncaahoopR: Women's NCAA Basketball Data Package. https://github.com/snestler/wncaahoopR
- Turner, H. & Firth, D. (2020). BradleyTerry2: Bradley-Terry Models in R. R package.
- Bradley, R.A. and Terry, M.E. (1952). Rank analysis of incomplete block designs. Biometrika, 39, 324-345.

## License
MIT License - Feel free to use and modify for educational purposes.

