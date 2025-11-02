# Covariate Analysis Summary

## Original Model
The baseline Bradley-Terry model uses:
- **Team strength (λ)**: Estimated from win/loss outcomes
- **Home advantage**: Binary indicator (1 = home game, 0 = neutral site)

## Covariates Tested

### 1. Offensive/Defensive Ratings
- **Offensive rating**: Points scored per game
- **Defensive rating**: Points allowed per game  
- **Net rating**: Point differential per game

**Result**: AIC identical to base model (Δ = 0.0)
**Reason**: Team abilities already capture offensive/defensive strength through win/loss patterns

### 2. Margin of Victory (MOV)
Tested two approaches:

#### Approach A: Game-level weights
- Used `log(MOV+1)` as weights in the model
- **Result**: AIC **increased** by ~4000-4500 (much worse)
- **Reason**: Over-weighting blowouts creates overfitting and numerical instability

#### Approach B: Team-level MOV differential
- Calculated average MOV per team
- Used as covariate: `mov_diff = home_mov_z - away_mov_z`
- **Result**: AIC identical to base model (Δ = 0.0)
- **Reason**: Average MOV is highly correlated with team strength (r = 0.79 with λ)

## Key Finding

**The simple Bradley-Terry model (team + home advantage) is optimal for this dataset.**

All performance metrics we tested (offensive rating, defensive rating, MOV) are **already encoded** in the team strength parameters (λ). The Bradley-Terry model extracts maximum information from win/loss outcomes alone.

## Correlations

Between team ability (λ) and various metrics:
- Offensive rating: **0.768**
- Net rating: **0.794**
- Defensive rating: **-0.289**
- Average MOV: **~0.79**

These strong correlations explain why adding these as covariates provides no additional predictive power.

## Model Specifications

All models fit separately per season, then aggregated across seasons using inverse-variance weighting.

**Base model (optimal)**:
```r
BTm(outcome ~ team + home_advantage, 
    id = "team", 
    contrasts = list(team = "contr.sum"))
```

**Models tested (all equivalent to base)**:
- `team + home_advantage + net_diff`
- `team + home_advantage + off_diff + def_diff`
- `team + home_advantage + mov_diff`
- `team + home_advantage + net_diff + mov_diff`

## Recommendation

**Use the base Bradley-Terry model** with:
- Team strength parameters (λ)
- Home advantage adjustment
- Season-specific fitting with z-score normalization

This provides optimal prediction without overfitting or unnecessary complexity.

## Files Generated

- `team_off_def_by_season.rds`: Offensive/defensive ratings per team-season
- `team_off_def_aggregated.rds`: Ratings aggregated across seasons
- `team_mov_ratings_by_season.rds`: MOV ratings per team-season
- `team_abilities_with_ratings.rds`: Team abilities with all metrics for analysis

These files are useful for **descriptive analysis** but not for improving the model.
