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
- **Scope**: Regular season games (2023-2024 seasons)

## Important Note: Synthetic Seeding

⚠️ **Tournament Seeds**: This analysis uses a **synthetic bracket** seeded by regular-season win percentage, not actual NCAA committee seeds. 

- Seeds are assigned purely from win% ranking (top 64 teams)
- Seed-specific results (e.g., "8-seed vs 9-seed matchups") are **model-based what-ifs**, not historical reproductions
- All conditional summaries should be interpreted as illustrative projections based on team strength estimates
- To use real NCAA tournament seeds, replace `tournament_seeds.csv` with actual bracket data

For research purposes, the **simulation-based conditional probabilities** (Script 04) are the primary results. The analytical estimates (Script 03) use heuristic opponent assumptions and serve as exploratory checks only.

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

