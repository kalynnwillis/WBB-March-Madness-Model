# =============================================================================
# Helper Functions
# Purpose: Utility functions used across multiple scripts
# =============================================================================

# =============================================================================
# Statistical Functions
# =============================================================================

# Inverse logit function (logistic)
inv_logit <- function(x) {
    1 / (1 + exp(-x))
}

# Logit function
logit <- function(p) {
    log(p / (1 - p))
}

# Calculate win probability from lambda difference
win_prob <- function(lambda1, lambda2) {
    inv_logit(lambda1 - lambda2)
}

# =============================================================================
# Data Manipulation Functions
# =============================================================================

# Safe join that warns about unmatched rows
safe_left_join <- function(x, y, by, warn = TRUE) {
    result <- dplyr::left_join(x, y, by = by)

    if (warn) {
        n_na <- sum(is.na(result))
        if (n_na > 0) {
            warning(sprintf("Join resulted in %d NA values", n_na))
        }
    }

    return(result)
}

# Create summary table with common statistics
summarize_numeric <- function(x, var_name = "variable") {
    tibble::tibble(
        variable = var_name,
        n = length(x),
        mean = mean(x, na.rm = TRUE),
        median = median(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        q25 = quantile(x, 0.25, na.rm = TRUE),
        q75 = quantile(x, 0.75, na.rm = TRUE)
    )
}

# =============================================================================
# Tournament Structure Functions
# =============================================================================

# Get opponent seed based on standard bracket structure
get_opponent_seed <- function(seed) {
    matchups <- c(
        "1" = 16, "16" = 1,
        "2" = 15, "15" = 2,
        "3" = 14, "14" = 3,
        "4" = 13, "13" = 4,
        "5" = 12, "12" = 5,
        "6" = 11, "11" = 6,
        "7" = 10, "10" = 7,
        "8" = 9, "9" = 8
    )

    return(matchups[as.character(seed)])
}

# Get second round opponent seed
get_second_round_opponent <- function(seed) {
    # After winning first round, typical second round matchups
    matchups <- c(
        "1" = 1, "16" = 1, # Winner of 1 vs 16 faces winner of 8 vs 9
        "8" = 1, "9" = 1,
        "2" = 2, "15" = 2, # Winner of 2 vs 15 faces winner of 7 vs 10
        "7" = 2, "10" = 2,
        "3" = 3, "14" = 3, # Winner of 3 vs 14 faces winner of 6 vs 11
        "6" = 3, "11" = 3,
        "4" = 4, "13" = 4, # Winner of 4 vs 13 faces winner of 5 vs 12
        "5" = 4, "12" = 4
    )

    return(matchups[as.character(seed)])
}

# =============================================================================
# Simulation Helper Functions
# =============================================================================

# Simulate a single game between two teams
simulate_single_game <- function(lambda1, lambda2, seed = NULL) {
    prob_team1_wins <- win_prob(lambda1, lambda2)

    if (!is.null(seed)) set.seed(seed)

    outcome <- rbinom(n = 1, size = 1, prob = prob_team1_wins)
    return(outcome)
}

# Simulate multiple games
simulate_games <- function(lambda1_vec, lambda2_vec, seed = NULL) {
    if (!is.null(seed)) set.seed(seed)

    probs <- mapply(win_prob, lambda1_vec, lambda2_vec)
    outcomes <- rbinom(n = length(probs), size = 1, prob = probs)

    return(outcomes)
}

# =============================================================================
# Formatting Functions
# =============================================================================

# Format probability as percentage
fmt_pct <- function(x, digits = 1) {
    sprintf(paste0("%.", digits, "f%%"), x * 100)
}

# Format number with specified decimal places
fmt_num <- function(x, digits = 2) {
    sprintf(paste0("%.", digits, "f"), x)
}

# Create pretty confidence interval string
fmt_ci <- function(lower, upper, digits = 2) {
    sprintf("[%s, %s]", fmt_num(lower, digits), fmt_num(upper, digits))
}

# =============================================================================
# Plotting Helper Functions
# =============================================================================

# Okabe-Ito colorblind-friendly palette
get_oi_colors <- function() {
    c(
        "#E69F00", "#56B4E9", "#009E73", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )
}

# Create custom ggplot theme
theme_march_madness <- function(base_size = 12) {
    ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = base_size * 1.2),
            plot.subtitle = ggplot2::element_text(size = base_size * 0.9),
            axis.title = ggplot2::element_text(face = "bold"),
            legend.position = "bottom"
        )
}

# =============================================================================
# Validation Functions
# =============================================================================

# Check if data has required columns
validate_columns <- function(data, required_cols) {
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
        stop(sprintf(
            "Missing required columns: %s",
            paste(missing_cols, collapse = ", ")
        ))
    }

    invisible(TRUE)
}

# Check for NA values in key columns
check_na <- function(data, cols, warn = TRUE) {
    for (col in cols) {
        n_na <- sum(is.na(data[[col]]))
        if (n_na > 0 && warn) {
            warning(sprintf(
                "Column '%s' has %d NA values (%.1f%%)",
                col, n_na, 100 * n_na / nrow(data)
            ))
        }
    }

    invisible(TRUE)
}

# =============================================================================
# Progress Reporting
# =============================================================================

# Print a section header
print_section <- function(title, width = 70) {
    cat("\n")
    cat(paste(rep("=", width), collapse = ""), "\n")
    cat(title, "\n")
    cat(paste(rep("=", width), collapse = ""), "\n")
    cat("\n")
}

# Print a subsection header
print_subsection <- function(title, width = 70) {
    cat("\n")
    cat(paste(rep("-", width), collapse = ""), "\n")
    cat(title, "\n")
    cat(paste(rep("-", width), collapse = ""), "\n")
    cat("\n")
}

# Print progress indicator
print_progress <- function(current, total, prefix = "Progress") {
    pct <- round(100 * current / total)
    cat(sprintf("\r%s: %d/%d (%d%%)", prefix, current, total, pct))
    if (current == total) cat("\n")
    flush.console()
}

# =============================================================================
# File I/O Helpers
# =============================================================================

# Save both RDS and CSV versions
save_dual <- function(data, base_path, name) {
    rds_path <- here::here(base_path, paste0(name, ".rds"))
    csv_path <- here::here(base_path, paste0(name, ".csv"))

    saveRDS(data, file = rds_path)
    readr::write_csv(data, file = csv_path)

    cat(sprintf("✓ Saved: %s (.rds and .csv)\n", name))

    invisible(list(rds = rds_path, csv = csv_path))
}

# Load RDS file with error handling
load_rds <- function(path, name = NULL) {
    if (is.null(name)) name <- basename(path)

    if (!file.exists(path)) {
        stop(sprintf("File not found: %s", path))
    }

    tryCatch(
        {
            data <- readRDS(path)
            cat(sprintf("✓ Loaded: %s\n", name))
            return(data)
        },
        error = function(e) {
            stop(sprintf("Error loading %s: %s", name, e$message))
        }
    )
}

# =============================================================================
# End of Helper Functions
# =============================================================================

cat("✓ Helper functions loaded\n")
