# Implementation Plan for Model Fixes

## Priority 1: CRITICAL FIXES (Must Complete)

### 1A. SYNTHETIC SEEDS - Option 1: Get Real NCAA Data (RECOMMENDED)

**Impact**: Fixes circular logic, makes research meaningful  
**Effort**: Low if data available, High if need to manually collect  
**Timeline**: 2-4 hours

**Implementation Steps**:

1. **Collect Real NCAA Tournament Seeds**
   - Sources:
     - ESPN: https://www.espn.com/womens-college-basketball/tournament/bracket
     - NCAA: https://www.ncaa.com/brackets/basketball-women/d1/
     - Her Hoop Stats: https://herhoopstats.com/
   
2. **Create Data File**: `data/raw/ncaa_seeds_historical.csv`
   ```csv
   season,team,ncaa_seed,region,round_reached
   2024,South Carolina Gamecocks,1,Portland,Championship Winner
   2024,Iowa Hawkeyes,1,Albany,Championship Runner-up
   2024,NC State Wolfpack,1,Spokane,Final Four
   2024,UConn Huskies,2,Portland,Sweet 16
   ...
   ```

3. **Modify Script 01** (`scripts/01_data_collection_UPDATED.R`)
   
   Replace lines 272-306 with:
   ```r
   # Load real NCAA tournament seeds if available
   ncaa_seed_file <- here("data", "raw", "ncaa_seeds_historical.csv")
   
   if (file.exists(ncaa_seed_file)) {
       cat("✓ Using REAL NCAA tournament seeds\n")
       tournament_seeds <- read_csv(ncaa_seed_file, show_col_types = FALSE) %>%
           filter(season == max(SEASONS)) %>%  # Use most recent season
           select(team, seed = ncaa_seed, region) %>%
           # Normalize team names to match our data
           mutate(team = str_trim(team))
       
       # Verify teams exist in our model
       tournament_seeds <- tournament_seeds %>%
           filter(team %in% unique(c(games_cleaned$home_team, games_cleaned$away_team)))
       
       cat(sprintf("  Loaded %d real tournament teams\n", nrow(tournament_seeds)))
       
   } else {
       warning("NCAA seed file not found - using MODEL-BASED SEEDS (for demonstration only)")
       # Keep existing synthetic seed code as fallback
       tournament_seeds <- team_perf %>%
           mutate(
               seed = rep(1:16, length.out = n()),
               region = rep(c("Portland", "Albany", "Spokane", "Wichita"), length.out = n())
           ) %>%
           select(team, seed, region)
   }
   ```

4. **Update README.md** to reflect real seeds:
   ```markdown
   ## Tournament Seeds
   
   This analysis uses **actual NCAA tournament committee seeds** from [seasons].
   Seeds reflect the committee's assessment of team quality, strength of schedule,
   conference performance, and other factors beyond raw win-loss records.
   ```

5. **Update Script 02** to use real seeds (lines 225-246):
   - Remove the "re-seeding based on lambda" section
   - Use the tournament_seeds from Script 01 as-is

---

### 1B. SYNTHETIC SEEDS - Option 2: Reframe Research Question

**If real seeds unavailable, change the question**:

**OLD Research Question**:
> How many 8-12 seeds advance past the second round?

**NEW Research Question**:
> Among teams ranked 32nd-48th in Bradley-Terry strength, what is their probability of reaching the Sweet 16?

**Changes Required**:
1. Update README.md title and description
2. Change all references from "8-12 seeds" to "mid-tier ranked teams (32-48)"
3. Add explicit note: "This is a model-based ranking, not NCAA committee seeding"
4. Reframe as a "what-if" analysis: "If tournament was seeded purely by BT strength..."

---

### 2. HOME ADVANTAGE - Fix Game-Level Modeling

**Impact**: Fixes biased predictions for non-neutral games  
**Effort**: Medium (requires refactoring data prep)  
**Timeline**: 3-4 hours

**Implementation Steps**:

1. **Create New Script**: `scripts/02a_bradley_terry_game_level.R`

   ```r
   # Game-level Bradley-Terry model (proper home advantage estimation)
   
   library(tidyverse)
   library(here)
   library(BradleyTerry2)
   
   games_cleaned <- readRDS(here("data", "processed", "games_cleaned.rds"))
   
   # Fit game-level models per season (proper home advantage)
   seasons <- sort(unique(games_cleaned$season))
   all_season_abilities <- list()
   beta_home_by_season <- tibble()
   
   for (season_year in seasons) {
       cat(sprintf("\n=== Fitting GAME-LEVEL model for %d ===\n", season_year))
       
       games_season <- games_cleaned %>%
           filter(season == season_year) %>%
           mutate(
               # Convert to 1-game format for BTM
               home.wins = as.integer(home_winner),
               away.wins = as.integer(1 - home_winner),
               home_advantage = 1 - neutral_site  # 1 = home game, 0 = neutral
           )
       
       # Ensure factors have all teams
       all_teams <- sort(unique(c(games_season$home_team, games_season$away_team)))
       games_season <- games_season %>%
           mutate(
               home.team = factor(home_team, levels = all_teams),
               away.team = factor(away_team, levels = all_teams)
           )
       
       # Fit game-level BTM
       bt_model_season <- BTm(
           outcome = cbind(home.wins, away.wins),
           player1 = home.team,
           player2 = away.team,
           formula = ~ team + home_advantage,  # Game-level covariate
           id = "team",
           contrasts = list(team = "contr.sum"),
           data = games_season
       )
       
       # Extract abilities
       abilities <- BTabilities(bt_model_season) %>%
           as.data.frame() %>%
           rownames_to_column(var = "team") %>%
           rename(lambda = ability, se = s.e.) %>%
           mutate(season = season_year)
       
       all_season_abilities[[as.character(season_year)]] <- abilities
       
       # Extract home advantage coefficient
       coef_summary <- summary(bt_model_season)$coefficients
       if ("home_advantage" %in% rownames(coef_summary)) {
           beta_home_by_season <- bind_rows(
               beta_home_by_season,
               tibble(
                   season = season_year,
                   beta_home = coef_summary["home_advantage", "Estimate"],
                   se = coef_summary["home_advantage", "Std. Error"],
                   z_value = coef_summary["home_advantage", "z value"],
                   p_value = coef_summary["home_advantage", "Pr(>|z|)"]
               )
           )
       }
       
       cat(sprintf("  Converged: %s\n", bt_model_season$converged))
       if (!bt_model_season$converged) {
           warning("Model did not converge for season ", season_year)
       }
   }
   
   # Meta-analysis of home advantage across seasons
   cat("\n=== Home Advantage Across Seasons ===\n")
   print(beta_home_by_season)
   
   # Compute weighted average home advantage
   beta_home_meta <- with(beta_home_by_season, {
       weighted.mean(beta_home, w = 1 / se^2)
   })
   
   cat(sprintf("\nMeta-estimate of home advantage: %.4f (log-odds)\n", beta_home_meta))
   cat(sprintf("  → Home win prob (equal teams): %.1f%%\n", plogis(beta_home_meta) * 100))
   
   # Continue with z-scoring and aggregation as before...
   # [Include all the z-scoring code from Script 02, lines 77-97]
   
   # Save results
   saveRDS(beta_home_meta, here("data", "processed", "home_advantage_meta.rds"))
   saveRDS(beta_home_by_season, here("data", "processed", "home_advantage_by_season.rds"))
   ```

2. **Update Script 02** to use game-level results:
   - Load home advantage from 02a
   - Remove pair-level aggregation code (lines 256-269 in Script 01)
   - Update comments to reflect game-level modeling

3. **Update Script 04** (tournament sims) to use proper home advantage:
   ```r
   # Tournament is neutral site, so beta_home = 0
   # But we have proper estimate now if needed for other predictions
   ```

---

## Priority 2: HIGH-PRIORITY FIXES

### 3. SE SCALING IN Z-SCORE NORMALIZATION

**Impact**: Corrects uncertainty estimates  
**Effort**: Low  
**Timeline**: 30 minutes

**Implementation**:

**In Script 02** (lines 77-85), change:
```r
# OLD (WRONG):
all_abilities_scaled <- all_abilities %>%
    group_by(season) %>%
    mutate(
        lambda_z = (lambda - mean(lambda, na.rm = TRUE)) / sd(lambda, na.rm = TRUE),
        se = pmax(se, 1e-6)  # ← Not scaled!
    ) %>%
    ungroup()

# NEW (CORRECT):
all_abilities_scaled <- all_abilities %>%
    group_by(season) %>%
    mutate(
        season_sd = sd(lambda, na.rm = TRUE),
        lambda_z = (lambda - mean(lambda, na.rm = TRUE)) / season_sd,
        se_z = se / season_sd,  # ← Scale SE too!
        se_z = pmax(se_z, 1e-6)  # Prevent division by zero
    ) %>%
    ungroup()

# Then use se_z in aggregation:
team_abilities_df <- all_abilities_scaled %>%
    group_by(team) %>%
    summarise(
        lambda = weighted.mean(lambda_z, w = 1 / (se_z^2), na.rm = TRUE),
        se = sqrt(1 / sum(1 / (se_z^2), na.rm = TRUE)),
        n_seasons = n(),
        seasons_played = paste(season, collapse = ", "),
        .groups = "drop"
    ) %>%
    arrange(desc(lambda))
```

**Apply same fix to**:
- `scripts/08_model_validation.R` (lines 87-102)

---

### 4. UPDATE DOCUMENTATION TO MATCH CODE

**Impact**: Prevents misleading readers  
**Effort**: Low  
**Timeline**: 1 hour

**Implementation**:

1. **Update `reports/final_report.Rmd`** (lines 177-216):
   - Replace the direct aggregation code with the z-scoring version
   - Add explicit commentary about why z-scoring is necessary

2. **Update `reports/quarto_report.qmd`** (lines 145-211):
   - Same changes as Rmd

3. **Add methodological note to README**:
   ```markdown
   ## Multi-Season Aggregation Methodology
   
   Since Bradley-Terry models are only identified up to an additive constant within
   each season, we standardize team abilities within season before aggregating:
   
   1. Fit separate BT models for each season (2019, 2021-2024)
   2. Z-score λ within each season: λ_z = (λ - μ_season) / σ_season
   3. Aggregate z-scored abilities using inverse-variance weighting
   4. Final λ represents team strength relative to average team across all seasons
   ```

---

## Priority 3: MEDIUM-PRIORITY IMPROVEMENTS

### 5. UNCERTAINTY PROPAGATION IN SIMULATIONS

**Impact**: More realistic confidence intervals  
**Effort**: Medium  
**Timeline**: 2-3 hours

**Implementation**:

**Create new script**: `scripts/04b_simulation_with_uncertainty.R`

```r
library(tidyverse)
library(here)
library(MASS)  # for mvrnorm

# Load model
bt_model <- readRDS(here("data", "processed", "bt_model.rds"))
team_abilities <- readRDS(here("data", "processed", "team_abilities_with_seeds.rds"))

# Option 1: Simple approach - Sample lambdas from N(λ, SE^2) independently
simulate_tournament_with_uncertainty <- function(sim_id) {
    set.seed(479 + sim_id)
    
    # Sample team abilities from their distributions
    team_abilities_sampled <- team_abilities %>%
        mutate(
            lambda_sampled = rnorm(n = n(), mean = lambda, sd = se)
        )
    
    # Build lambda matrix with sampled values
    # [Rest of simulation code using lambda_sampled instead of lambda]
    
    # ... return results
}

# Option 2: Advanced approach - Sample from full covariance matrix
# (Accounts for correlation between team estimates)
# Requires re-parameterizing to get full vcov matrix
# See BTM documentation for extracting vcov(bt_model)

# Run simulations
N_SIMS <- 5000
simulation_results_uncertain <- mclapply(
    X = 1:N_SIMS,
    FUN = simulate_tournament_with_uncertainty,
    mc.cores = detectCores() - 1
)

# Compare to original simulations
# Width of confidence intervals should be wider (more realistic)
```

**Add comparison plot** showing:
- Original CI (no uncertainty propagation)
- Updated CI (with uncertainty propagation)
- Label: "Updated CIs more accurately reflect model uncertainty"

---

### 6. SEASON WEIGHTING (RECENT PRIORITY)

**Impact**: Better reflects current team strength  
**Effort**: Low-Medium  
**Timeline**: 1-2 hours

**Implementation**:

**In Script 02** (lines 88-97), add exponential decay weighting:

```r
# Define recency weights (exponential decay)
most_recent_season <- max(seasons)
recency_weights <- all_abilities_scaled %>%
    mutate(
        years_ago = most_recent_season - season,
        recency_weight = exp(-0.3 * years_ago)  # Decay rate: 0.3
        # 0 years ago: weight = 1.0
        # 1 year ago: weight = 0.74
        # 2 years ago: weight = 0.55
        # 3 years ago: weight = 0.41
        # 5 years ago: weight = 0.22
    )

# Aggregate with both inverse-variance and recency weighting
team_abilities_df <- recency_weights %>%
    group_by(team) %>%
    summarise(
        lambda = weighted.mean(
            lambda_z, 
            w = recency_weight / (se_z^2),  # Combined weight
            na.rm = TRUE
        ),
        se = sqrt(1 / sum(recency_weight / (se_z^2), na.rm = TRUE)),
        n_seasons = n(),
        avg_recency = mean(recency_weight),
        seasons_played = paste(season, collapse = ", "),
        .groups = "drop"
    ) %>%
    arrange(desc(lambda))

cat("\nSeason weighting applied: recent seasons weighted more heavily\n")
```

**Add sensitivity analysis**: Try decay rates of 0.2, 0.3, 0.5 and compare results

---

## Priority 4: MINOR FIXES

### 7. IN-SAMPLE ACCURACY WARNING

**In Script 02** (line 352), change:

```r
# OLD:
cat(sprintf("Prediction accuracy: %.2f%%\n", 100 * accuracy))

# NEW:
cat(sprintf("Training accuracy: %.2f%% (OPTIMISTICALLY BIASED - in-sample)\n", 100 * accuracy))
cat("For unbiased accuracy, see validation results (Script 08)\n")
```

---

### 8. CONVERGENCE CHECKS

**In Script 02** (after line 68), add:

```r
if (!bt_model_season$converged) {
    stop(sprintf(
        "Bradley-Terry model failed to converge for %d season after %d iterations.\n",
        season_year, bt_model_season$iter,
        "Check for separation issues or reduce model complexity."
    ))
}

if (bt_model_season$iter > 50) {
    warning(sprintf(
        "Model for %d season took %d iterations (>50). May indicate convergence issues.",
        season_year, bt_model_season$iter
    ))
}
```

---

## IMPLEMENTATION ORDER

### Phase 1: Critical Fixes (Complete First)
**Estimated Time: 6-10 hours**

1. **Synthetic Seeds Decision** (2-4 hours)
   - [ ] Decide: Real seeds vs. Reframe question
   - [ ] If real seeds: Collect data for 2023-2024
   - [ ] Implement chosen solution
   - [ ] Update README and all documentation

2. **Home Advantage Fix** (3-4 hours)
   - [ ] Create Script 02a with game-level modeling
   - [ ] Test convergence and compare to pair-level
   - [ ] Update Script 02 to use game-level results
   - [ ] Document methodology changes

3. **SE Scaling Fix** (30 min)
   - [ ] Update Script 02 z-scoring code
   - [ ] Update Script 08 validation code
   - [ ] Verify results still reasonable

4. **Documentation Sync** (1 hour)
   - [ ] Update reports/final_report.Rmd
   - [ ] Update reports/quarto_report.qmd
   - [ ] Add methodology section to README

### Phase 2: Important Improvements (Do Next)
**Estimated Time: 4-6 hours**

5. **Uncertainty Propagation** (2-3 hours)
   - [ ] Implement Script 04b
   - [ ] Compare CI widths
   - [ ] Update visualizations

6. **Season Weighting** (1-2 hours)
   - [ ] Add recency weights to aggregation
   - [ ] Sensitivity analysis on decay rate
   - [ ] Document rationale

### Phase 3: Polish (Do Last)
**Estimated Time: 1 hour**

7. **Minor Fixes** (30 min)
   - [ ] Update accuracy labels
   - [ ] Add convergence checks
   - [ ] Fix variable naming

8. **Final Validation** (30 min)
   - [ ] Run complete pipeline with all fixes
   - [ ] Compare old vs new results
   - [ ] Document changes in CHANGELOG.md

---

## TESTING CHECKLIST

After implementing each fix:

- [ ] Run affected scripts without errors
- [ ] Check that lambda ranges are still reasonable (-6 to +6)
- [ ] Verify SEs are reasonable (< 10)
- [ ] Confirm simulations produce expected distributions
- [ ] Compare key results to pre-fix baseline
- [ ] Update any hardcoded values or assumptions
- [ ] Re-generate all figures
- [ ] Re-run reports

---

## EXPECTED RESULTS CHANGES

**After fixes, expect to see**:

1. **Different Seeds**: Real NCAA seeds vs. your lambda rankings
   - Some mismatches expected (committee considers more than strength)
   
2. **Different Probabilities**: 
   - More uncertainty (wider CIs) from uncertainty propagation
   - Slightly different values from proper SE scaling
   
3. **Valid Home Advantage**: 
   - Likely positive (home teams win more)
   - Typical range: 0.1 to 0.4 on log-odds scale
   - Translates to ~52-60% home win rate for equal teams

4. **Research Question Changes**:
   - From "8-12 seeds" to "actual NCAA 8-12 seeds"
   - More interesting results (may show model predictions vs. reality)

---

## VALIDATION APPROACH

**Before-After Comparison**:

1. **Run Current Model** → Save all results to `results_old/`
2. **Implement All Fixes**
3. **Run Fixed Model** → Save all results to `results_new/`
4. **Create Comparison Report**:
   - Side-by-side lambda comparisons
   - CI width comparisons
   - Probability distribution comparisons
   - Document all changes and rationale

---

## QUESTIONS TO DECIDE

Before starting implementation, decide:

1. **Seeds**: Real data or reframe? (Affects entire analysis)
2. **Season weighting**: Decay rate? (0.2, 0.3, or 0.5?)
3. **Uncertainty**: Simple (independent) or advanced (correlated)?
4. **Scope**: Fix everything or just critical issues?

---

## FILES THAT NEED CHANGES

### Must Edit:
- `scripts/01_data_collection_UPDATED.R` (seeds)
- `scripts/02_bradley_terry_model.R` (SE scaling, home adv)
- `scripts/08_model_validation.R` (SE scaling)
- `reports/final_report.Rmd` (documentation)
- `reports/quarto_report.qmd` (documentation)
- `README.md` (methodology, disclaimers)

### Should Create:
- `scripts/02a_bradley_terry_game_level.R` (new)
- `scripts/04b_simulation_with_uncertainty.R` (new)
- `CHANGELOG.md` (document all changes)
- `data/raw/ncaa_seeds_historical.csv` (if using real seeds)

### Will Be Regenerated:
- All files in `data/processed/`
- All files in `results/tables/`
- All files in `results/figures/`

---

**Total Estimated Time**: 11-17 hours depending on choices made

**Recommended Timeline**: 
- Week 1: Phase 1 (Critical Fixes)
- Week 2: Phase 2 (Improvements) + Validation
- Week 3: Phase 3 (Polish) + Documentation

