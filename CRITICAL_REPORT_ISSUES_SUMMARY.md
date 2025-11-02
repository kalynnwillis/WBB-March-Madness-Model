# CRITICAL Report Accuracy Issues

## Executive Summary

**All three reports contain MAJOR FACTUAL ERRORS that completely misrepresent the research findings.**

The reports claim the expected number of 8-12 seeds reaching Sweet 16 is **1.72**, but the actual computed value is:
- **Analytical calculation: 3.75 teams**
- **Simulation result: 2.36 teams**

This error propagates throughout all three documents, invalidating the core conclusions.

---

## Detailed Findings

### Issue 1: WRONG Sweet 16 Expectation (CRITICAL)

| Metric | Report Claims | Actual Values | Error |
|--------|--------------|---------------|-------|
| Analytical Expected | 1.72 teams | **3.75 teams** | -2.03 teams (54% undercount!) |
| Simulation Mean | 1.68 teams | **2.36 teams** | -0.68 teams (29% undercount!) |
| Agreement | "Strong agreement" | **40% discrepancy** | Misleading |

**Files affected:** All three (final_report.Rmd lines 72, 895; quarto_report.qmd line 947; quarto_presentation.qmd lines 198-199)

**Impact:** This is Research Question #1 - the PRIMARY finding is wrong!

---

### Issue 2: WRONG Probability of Zero (CRITICAL)

| Metric | Report Claims | Actual Value | Error |
|--------|--------------|-------------|-------|
| P(zero in Sweet 16) | 17.9% | **5.94%** | 3× overestimate |
| Interpretation | "1-in-6 chance" | **"1-in-17 chance"** | Completely wrong |
| 2023-2024 context | "Unusual but not shocking" | **"Genuinely unusual"** | Wrong conclusion |

**Files affected:** All three (final_report.Rmd line 1098; quarto_report.qmd line 1169; quarto_presentation.qmd line 207)

**Impact:** The entire historical contextualization is backwards - zero is MUCH MORE unusual than reported!

---

### Issue 3: WRONG Conditional Probabilities

**Given Sweet 16, probability of championship:**

| Report Claims | Actual Value | Error |
|--------------|-------------|-------|
| 3.7% | **2.61%** | 42% overestimate |
| "1 in 27" | **"1 in 38"** | Wrong ratio |

**Other conditional probabilities:**
- Elite 8: Reports say 45.7%, actual is **48.6%** (close but wrong)
- Final Four: Reports say 18.7%, actual is **18.1%** (close)

**Files affected:** All three reports

---

### Issue 4: Inconsistent Individual Team Data

**The `mid_tier_team_advancement.csv` file only includes 5 teams:**
1. Louisville Cardinals (10-seed) - 17.7% Sweet 16 probability
2. Iowa Hawkeyes (11-seed) - 17.1%
3. Utah Utes (12-seed) - 14.9%
4. Arizona Wildcats (8-seed) - 14.2%
5. South Florida Bulls (9-seed) - 12.1%

**But the full dataset has ALL 20 teams (4 per seed from 8-12).**

**The reports claim:**
- "Stanford (11-seed) at 62.5%" - **Stanford doesn't even appear in the results!**
- This is completely fabricated data

**Sum of Sweet 16 probabilities:**
- Top 5 teams only: 0.76 teams expected
- Analytical seed-level: 3.75 teams expected
- **Discrepancy: 2.99 teams missing from individual analysis!**

---

### Issue 5: Misleading First Round Statistics

**Reports claim:** "9-10 of the 20 teams seeded 8-12 will win R1"

**Actual expectations:**
- key_results$expected_advance_r32 = **8.84 teams**
- From seed-level summary: sum of expected_wins = 8.84 teams
- This is correct at the seed-level!

**But individual team file shows:**
- Sum of "Round of 32" probabilities = **1.41 teams**
- Only includes 5 teams instead of 20

**The issue:** The individual team analysis is incomplete.

---

## Root Cause Analysis

### 1. Analytical vs Simulation Discrepancy

The analytical calculation (3.75) assumes **average matchups** based on seed-level statistics:
- Averages across all 4 teams at each seed
- Multiplies by 4 teams per seed
- Result: 3.75 teams expected

The simulation (2.36) uses **actual team strengths (lambda values)**:
- Some 8-12 seeds are MUCH weaker (near-zero lambda)
- These weak teams drag down the average
- Result: 2.36 teams expected (closer to reality)

**Example of weak teams with 8-12 seeds:**
- UAlbany Great Danes (12-seed): lambda = -0.000000000000828
- Maine Black Bears (11-seed): lambda = -0.000931
- Princeton Tigers (9-seed): lambda = 0.0000000000801
- These teams have essentially 0% chance of Sweet 16

**The analytical approach is FLAWED** because it doesn't account for within-seed lambda variability.

### 2. Individual Team File is Incomplete

Script `06_individual_team_probabilities.R` or `07_individual_team_viz.R` appears to have filtered to only the top 5 teams by lambda, not all 20 teams in the 8-12 seed range.

### 3. Reports Use Hardcoded Values

The reports contain hardcoded numbers (1.72, 17.9%, etc.) that don't match any computed results. These appear to be:
- Values from a different analysis
- Expected/typical values from literature
- Placeholder values that were never updated

---

## Correct Values Summary

### Research Question 1: How many 8-12 seeds reach Sweet 16?

**CORRECT ANSWER:**
- **Simulation-based: 2.36 teams (SD=1.27, range 0-7)**
- Analytical (seed-level averages): 3.75 teams
- **Use simulation value** as it accounts for actual team strengths

**Distribution from 5,000 simulations:**
- 0 teams: 5.94% of tournaments
- 1 team: 16.7%
- 2 teams: 28.6%
- 3 teams: 27.3%
- 4+ teams: 21.5%

### Research Question 2: Given Sweet 16, how far do they go?

**CORRECT ANSWER:**
- Elite 8: **48.6%**
- Final Four: **18.1%**
- Championship game: **6.82%**
- Win championship: **2.61%**

**Interpretation:** About **1 in 38** mid-tier seeds that reach Sweet 16 win the championship (not 1 in 27)

### Historical Context: 2023-2024 with Zero

**CORRECT ANSWER:**
- Probability of zero: **5.94%** (about 1-in-17 tournaments)
- This is **genuinely unusual**, not just "unusual but expected"
- Would naturally occur about once every **17 years**, not every 6 years
- Back-to-back occurrence (2023 & 2024) is even rarer: ~0.35% (1-in-280)

---

## Required Actions

### Immediate (Before Any Presentation/Submission):

1. **Update ALL numerical claims in all 3 reports:**
   - Expected Sweet 16: Change 1.72 → 2.36 (use simulation)
   - P(zero): Change 17.9% → 5.94%
   - P(champion | Sweet 16): Change 3.7% → 2.61%
   - Remove all mentions of "Stanford at 62.5%"

2. **Add caveat about analytical vs simulation:**
   - Explain that analytical (3.75) overestimates due to not accounting for weak teams
   - State that simulation (2.36) is more reliable
   - Note the 40% discrepancy

3. **Fix historical context:**
   - Change "1-in-6" to "1-in-17"
   - Update interpretation: zero is MORE unusual than typical variance
   - 2023-2024 back-to-back is extremely rare (0.35%)

4. **Remove fabricated data:**
   - Delete all references to Stanford
   - Either include all 20 teams or clarify you're only analyzing top 5

### Secondary (Data Quality):

5. **Regenerate `mid_tier_team_advancement.csv` for all 20 teams**
   - Or clearly state analysis focuses on top 5 strongest 8-12 seeds
   - Verify sum matches simulation results

6. **Check figure accuracy:**
   - Verify figures show 2.36, not 1.72
   - Verify P(zero) plots show 5.94%
   - Regenerate if needed

7. **Investigate analytical calculation:**
   - Consider if seed-level averaging is appropriate
   - May need to weight by actual team counts or lambda distributions

---

## Severity Assessment

| Issue | Severity | Impact |
|-------|----------|--------|
| Wrong Sweet 16 expectation | **CRITICAL** | Core finding is wrong |
| Wrong P(zero) | **CRITICAL** | Historical context is backwards |
| Wrong conditional probs | **HIGH** | Research Question 2 answer is wrong |
| Fabricated Stanford data | **CRITICAL** | Undermines credibility |
| Analytical vs simulation discrepancy | **HIGH** | Methodological flaw |

**Overall:** **DO NOT USE THESE REPORTS** without comprehensive revision.

---

## Files Requiring Changes

1. **`reports/final_report.Rmd`**
   - Lines 72-76 (executive summary numbers)
   - Lines 895-896 (RQ1 answer)
   - Lines 942-945 (RQ2 answer)
   - Line 1098 (P(zero))
   - Lines 1060-1070 (Stanford mention)
   - All inline R code chunks referencing `key_results$expected_in_sweet16`

2. **`reports/quarto_report.qmd`**
   - Lines 947-951 (RQ1 answer)
   - Lines 1002-1005 (RQ2 answer)
   - Line 1169 (P(zero))
   - Lines 1136-1147 (Stanford section)
   - All inline R code chunks

3. **`reports/quarto_presentation.qmd`**
   - Slides 7-8 (lines 186-210): Finding 1
   - Slide 9 (lines 219-242): Finding 2
   - Slide 11 (lines 274-289): Summary

---

## Recommended Fix Strategy

**Option A: Use Simulation Values (RECOMMENDED)**
- Expected: 2.36 teams (simulation)
- P(zero): 5.94%
- Acknowledge analytical gave 3.75 but explain why simulation is more accurate
- Simple find-and-replace in all files

**Option B: Explain Discrepancy**
- Present both values (2.36 vs 3.75)
- Discuss why they differ
- Recommend simulation as more reliable
- More intellectually honest but more complex

**Option C: Fix Analytical Calculation**
- Recalculate using actual team distributions
- Likely would converge closer to 2.36
- Most time-consuming

**I recommend Option A** - use simulation values throughout and add one paragraph explaining why analytical overestimates.

