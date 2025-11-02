# Report Accuracy Issues - CRITICAL

## Summary
All three report files (final_report.Rmd, quarto_report.qmd, quarto_presentation.qmd) contain **significant inaccuracies** that misrepresent the actual results.

---

## MAJOR ISSUES

### 1. Expected Sweet 16 Advancement - WRONG

**All reports claim:**
- Expected 8-12 seeds in Sweet 16: **1.72 teams**
- Simulation mean: **1.68 teams**

**ACTUAL RESULTS:**
- Analytical calculation: **3.75 teams** (from key_results$expected_in_sweet16)
- Simulation mean: **2.36 teams** (from simulation_summary.csv)

**Impact:** This is the PRIMARY RESEARCH QUESTION and the answer is completely wrong throughout all reports!

---

### 2. Number of 8-12 Seeds - WRONG

**Reports claim:**
- "20 teams seeded 8-12"
- "Collectively, we expect about 9-10 of the 20 teams seeded 8-12 to win their first-round game"

**ACTUAL DATA:**
- Only **5 teams** seeded 8-12 (not 20!)
  - Louisville Cardinals (10-seed)
  - Iowa Hawkeyes (11-seed)
  - Utah Utes (12-seed)
  - Arizona Wildcats (8-seed)
  - South Florida Bulls (9-seed)

**Why:** The model-based seeding created a synthetic bracket with only 5 mid-tier seeds, not the standard 4 regions × 5 seeds = 20 teams.

---

### 3. Probability of Zero in Sweet 16 - WRONG

**Reports claim:**
- Probability of zero 8-12 seeds in Sweet 16: **17.9%** (or 18%)
- Interpretation: "about 1-in-6 chance"

**ACTUAL RESULTS:**
- Probability of zero: **5.94%** (from simulation)
- This is about **1-in-17**, not 1-in-6!

**Impact:** The entire historical contextualization of 2023-2024 is incorrect.

---

### 4. Conditional Probabilities - SLIGHTLY WRONG

**Reports claim (given Sweet 16):**
- Final Four: 18.7%
- Championship: 3.7%

**ACTUAL RESULTS:**
- Final Four: **18.1%** (close but slightly off)
- Elite 8: **48.6%** (reports say 45.7%)
- Championship: **2.61%** (reports say 3.7% - significantly different!)

---

### 5. First Round Statistics - MISLEADING

**Reports claim:**
- 8 seeds: ~49% win probability
- 9 seeds: ~51% win probability
- 10 seeds: ~34% win probability
- etc.

**ACTUAL RESULTS (from first_round_summary.csv):**
- 8 seeds: **48.7%** (close)
- 9 seeds: **43.4%** (NOT 51%!)
- 10 seeds: **42.5%** (NOT 34%!)
- 11 seeds: **42.5%**
- 12 seeds: **43.9%**

**Why misleading:** These don't match typical NCAA matchups because of the model-based seeding with only 5 teams.

---

## SPECIFIC FILE ISSUES

### final_report.Rmd

**Lines with errors:**
- Line 72-75: Wrong expected_in_sweet16 value
- Line 73: Wrong conditional probabilities
- Line 75: Wrong highest Sweet 16 probability claim
- Line 895: "approximately 1.72"
- Line 942-945: All conditional probabilities wrong
- Line 1098: "17.9%" probability of zero

### quarto_report.qmd

**Lines with errors:**
- Line 947: "approximately 1.72 teams"
- Line 1002-1005: Wrong conditional probabilities
- Line 1169: "17.9%" probability
- Line 914: Wrong claim about "9-10 of the 20 teams"

### quarto_presentation.qmd

**Lines with errors:**
- Line 198-199: "Expected: 1.7 teams", "Analytical: 1.72"
- Line 207: "Probability: 17.9%"
- Line 231-234: Wrong conditional probabilities
- Line 278: "~1.7 mid-tier seeds"

---

## ROOT CAUSES

1. **Model-based seeding limitation:** Only 5 teams got seeds 8-12 instead of the expected 20 (4 regions × 5 seeds)

2. **Hardcoded values:** Reports have hardcoded numbers that don't match actual results

3. **Inconsistent key_results.rds:** The analytical calculation (3.75) doesn't match simulation (2.36), suggesting an issue with how the analytical approach summed probabilities

4. **Copy-paste from template:** Appears reports were written with expected/typical values rather than actual computed values

---

## WHAT NEEDS TO BE FIXED

### Priority 1: Fix all numerical claims
- Update expected_in_sweet16 to match actual results (3.75 analytical, 2.36 simulation)
- Explain the discrepancy between analytical and simulation
- Update probability of zero to 5.94%
- Update conditional probabilities to match actual values

### Priority 2: Address the 5 vs 20 teams issue
- Either explain why only 5 teams got 8-12 seeds (model-based seeding quirk)
- Or acknowledge this limitation
- Remove claims about "20 teams seeded 8-12"
- Adjust first-round win expectations accordingly

### Priority 3: Update interpretation
- Change "1-in-6 chance" to "1-in-17 chance" for zero seeds
- Revise the 2023-2024 contextualization
- Note that zero is MORE unusual than currently claimed

### Priority 4: Verify figure accuracy
- Check if figures match the corrected values
- Regenerate if necessary

---

## RECOMMENDATION

**DO NOT USE THESE REPORTS AS-IS.** The core findings are misrepresented. All three files need comprehensive revision with actual computed values before presentation or submission.

The discrepancy between analytical (3.75) and simulation (2.36) also suggests a potential issue in the analytical calculation method that should be investigated.

