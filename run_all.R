# =============================================================================
# Master Script: Run Complete Analysis Pipeline
# Purpose: Execute all analysis scripts in sequence
# =============================================================================

# Set working directory to project root
setwd(here::here())

cat("\n")
cat("================================================================================\n")
cat("  Women's Basketball March Madness: Bradley-Terry Model Analysis\n")
cat("  Master Script - Running Complete Analysis Pipeline\n")
cat("================================================================================\n")
cat("\n")

# Record start time
start_time <- Sys.time()

# =============================================================================
# Install Required Packages
# =============================================================================

cat("Checking and installing required packages...\n")

required_packages <- c(
    "tidyverse",
    "here",
    "devtools",
    "ggplot2",
    "patchwork",
    "knitr",
    "rmarkdown",
    "kableExtra",
    "lubridate"
)

# Check and install CRAN packages
for (pkg in required_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat(sprintf("  Installing %s...\n", pkg))
        install.packages(pkg, dependencies = TRUE)
    }
}

# Install GitHub packages
if (!require("wncaahoopR", quietly = TRUE)) {
    cat("  Installing wncaahoopR from GitHub...\n")
    devtools::install_github("snestler/wncaahoopR")
}

if (!require("BradleyTerry2", quietly = TRUE)) {
    cat("  Installing BradleyTerry2 from GitHub...\n")
    devtools::install_github("hturner/BradleyTerry2")
}

cat("✓ All required packages installed\n\n")

# =============================================================================
# Script Execution
# =============================================================================

scripts <- c(
    "01_data_collection_UPDATED.R",
    "02_bradley_terry_model.R",
    "03_seed_analysis.R",
    "04_tournament_simulation.R",
    "05_visualization.R",
    "06_individual_team_probabilities.R",
    "07_individual_team_viz.R",
    "08_model_validation.R",
    "09_model_comparison.R"
)

script_names <- c(
    "Data Collection",
    "Bradley-Terry Model Fitting",
    "Seed Analysis",
    "Tournament Simulation",
    "Visualization",
    "Individual Team Probabilities",
    "Individual Team Visualizations",
    "Model Validation",
    "Model Comparison (Predictive Performance)"
)

# Execute each script
for (i in seq_along(scripts)) {
    cat("\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat(sprintf("STEP %d/%d: %s\n", i, length(scripts), script_names[i]))
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("\n")

    script_start <- Sys.time()

    tryCatch(
        {
            source(here::here("scripts", scripts[i]))

            script_end <- Sys.time()
            elapsed <- difftime(script_end, script_start, units = "secs")

            cat("\n")
            cat(sprintf(
                "✓ %s completed successfully (%.1f seconds)\n",
                script_names[i], elapsed
            ))
        },
        error = function(e) {
            cat("\n")
            cat(sprintf("✗ Error in %s:\n", script_names[i]))
            cat(sprintf("  %s\n", e$message))
            cat("\n")
            cat("Pipeline execution stopped due to error.\n")
            cat("Please fix the error and run again.\n")
            stop(e)
        }
    )
}

# =============================================================================
# Generate Final Report
# =============================================================================

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("STEP 6/6: Generating Final Report\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

report_start <- Sys.time()

tryCatch(
    {
        # Render HTML report
        cat("Rendering HTML report...\n")
        rmarkdown::render(
            input = here::here("reports", "final_report.Rmd"),
            output_format = "html_document",
            output_file = "final_report.html",
            output_dir = here::here("reports")
        )

        cat("✓ HTML report generated\n")

        # Try to render PDF report (requires LaTeX)
        tryCatch(
            {
                cat("Rendering PDF report...\n")
                rmarkdown::render(
                    input = here::here("reports", "final_report.Rmd"),
                    output_format = "pdf_document",
                    output_file = "final_report.pdf",
                    output_dir = here::here("reports")
                )
                cat("✓ PDF report generated\n")
            },
            error = function(e) {
                cat("⚠ PDF report generation skipped (LaTeX not available)\n")
            }
        )

        report_end <- Sys.time()
        elapsed <- difftime(report_end, report_start, units = "secs")

        cat(sprintf("\n✓ Report generation completed (%.1f seconds)\n", elapsed))
    },
    error = function(e) {
        cat("\n")
        cat("⚠ Warning: Report generation encountered an error:\n")
        cat(sprintf("  %s\n", e$message))
        cat("  However, all analysis scripts completed successfully.\n")
        cat("  You can manually generate the report later if needed.\n")
    }
)

# =============================================================================
# Summary
# =============================================================================

end_time <- Sys.time()
total_elapsed <- difftime(end_time, start_time, units = "mins")

cat("\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("ANALYSIS PIPELINE COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

cat(sprintf("Total execution time: %.1f minutes\n", total_elapsed))
cat("\n")

cat("Generated Outputs:\n")
cat("\n")
cat("Data Files (data/processed/):\n")
cat("  - games_cleaned.rds/csv\n")
cat("  - bt_data.rds/csv\n")
cat("  - team_abilities.rds/csv\n")
cat("  - tournament_seeds.rds/csv\n")
cat("  - bt_model.rds\n")
cat("\n")

cat("Results (results/tables/):\n")
cat("  - first_round_summary.csv\n")
cat("  - second_round_summary.csv\n")
cat("  - deeper_runs_summary.csv\n")
cat("  - conditional_summary.csv\n")
cat("  - simulation_summary.csv\n")
cat("  - seed_performance_sim.csv\n")
cat("  - championship_winners.csv\n")
cat("\n")

cat("Visualizations (results/figures/):\n")
cat("  - 00_summary_dashboard.png\n")
cat("  - 01_team_strength_by_seed.png\n")
cat("  - 02_all_team_strengths.png\n")
cat("  - 03_first_round_probabilities.png\n")
cat("  - 04_expected_advancement.png\n")
cat("  - 05_simulation_distributions.png\n")
cat("  - 06_analytical_vs_simulation.png\n")
cat("  - 07_conditional_probabilities.png\n")
cat("  - 08_seed_performance_heatmap.png\n")
cat("\n")

cat("Reports (reports/):\n")
cat("  - final_report.html\n")
if (file.exists(here::here("reports", "final_report.pdf"))) {
    cat("  - final_report.pdf\n")
}
cat("\n")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("Thank you for using the WBB March Madness Analysis Pipeline!\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
cat("\n")

# =============================================================================
# Optional: Open results
# =============================================================================

cat("Would you like to:\n")
cat("  1. Open the HTML report\n")
cat("  2. Open the results folder\n")
cat("  3. View summary dashboard\n")
cat("  4. Exit\n")
cat("\n")

# Uncomment the following lines if you want automatic opening:
# browseURL(here::here("reports", "final_report.html"))
# system(paste("open", here::here("results", "figures", "00_summary_dashboard.png")))
