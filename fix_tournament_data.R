library(tidyverse)
library(here)

# Load existing data
tournament_seeds <- readRDS("data/processed/tournament_seeds.rds")

# Check current size
cat("Current tournament_seeds has", nrow(tournament_seeds), "rows\n")

# If it's only 16 teams, expand to 64
if (nrow(tournament_seeds) < 64) {
    top_16 <- tournament_seeds %>% 
        distinct(team, .keep_all = TRUE) %>%
        head(16)
    
    tournament_seeds_64 <- tibble(
        season = "2024",
        team = rep(top_16$team, times = 4),
        seed = rep(1:16, times = 4),
        region = rep(c("Portland", "Albany", "Spokane", "Wichita"), each = 16)
    )
    
    saveRDS(tournament_seeds_64, "data/processed/tournament_seeds.rds")
    write_csv(tournament_seeds_64, "data/processed/tournament_seeds.csv")
    
    cat("✓ Expanded to", nrow(tournament_seeds_64), "teams\n")
    
    # Also update mid_tier_seeds
    mid_tier_seeds_64 <- tournament_seeds_64 %>%
        filter(seed >= 8 & seed <= 12) %>%
        mutate(seed_category = "8-12 seeds")
    
    saveRDS(mid_tier_seeds_64, "data/processed/mid_tier_seeds.rds")
    write_csv(mid_tier_seeds_64, "data/processed/mid_tier_seeds.csv")
    
    cat("✓ Updated mid_tier_seeds:", nrow(mid_tier_seeds_64), "teams\n")
}
