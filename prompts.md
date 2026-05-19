# AI Workflow Documentation

**Project:** Ideal-Point Estimation from Bundestag Roll-Call Votes  
**Track:** B (AI-forward)  
**Tool:** Claude Code (claude-sonnet-4-6, Anthropic)  
**Date:** May 2026

---

## Overview

This document records the prompts and iterative workflow used to generate the analysis code and GitHub Pages site for the IRT assignment. All code was reviewed before execution; results were checked for plausibility against known German political positions.

---

## Session 1 — Reading and Planning

### Prompt 1 (User)
```
You are helping me complete Track B of a university assignment on ideal-point 
estimation from Bundestag roll-call vote data. The full repo is at 
github.com/MaximilianBirkle/[repo-name]. Read the README.md for the full 
assignment instructions before doing anything else. Also read 
week13_fss2026_bayes.html for lecture context and interpretation guidance, 
and v100i05.R + v100i05.pdf for the Bürkner (2021) brms replication code.

The deliverable is a polished GitHub Pages site (index.html) built in 
University of Mannheim colors (use Uni-mannheim.svg.png for the logo and 
extract the color scheme from it — the primary color is teal/dark green 
#3c7a6e or similar). The site walks through the analysis narratively, 
combining code snippets with results, figures, and interpretation.

[... full prompt with data file descriptions, analysis tasks, and site requirements ...]

Start by reading README.md, week13_fss2026_bayes.html, and v100i05.R. 
Then outline your plan before writing any code.
```

### Response 1 (Claude)
Claude read all three files. Key findings from the reading:

- **Data structure**: `bundestag_wp132_votes.csv` is in **long format** (not wide), with columns `poll_id, poll_label, mandate_id, mandate_label, fraction_id, fraction_label, vote`. Vote types: `yes`, `no`, `abstain`, `no_show`.
- **Scale**: 161 unique polls, 772 legislators, 118,400 total rows.
- **Packages available**: tidyverse, brms, jsonlite, scales, ggridges (but NOT ggdist or ggrepel).
- **brms formula**: Identified the exact 2PL formula from Bürkner (2021): `bf(r2 ~ exp(logalpha) * eta, eta ~ 1 + (1 | i | item) + (1 | id), logalpha ~ 1 + (1 | i | item), nl = TRUE)`
- **Lecture guidance**: Double-centering should be applied to the imputed matrix for exact zero row/col means; dimension 2 of SVD is likely noise given the 1D dominance of parliamentary data.

**Plan outlined:**
1. `analysis.R` — full pipeline saving figures to `figures/` and results to `results/`
2. `build_site.R` — reads outputs, builds `index.html` using `@@VAR@@` placeholder substitution
3. `prompts.md` — this document

---

## Session 2 — Implementation

### Prompt 2 (User)
```
[Previous context]

Now write the analysis.R script first. Key design decisions to implement:
- Double-mean imputation using vectorized sweep() 
- SVD with AfD oriented positive (right)
- Double-centering applied to the imputed matrix (for exact zero verification)
- brms 2PL: 1 chain, iter=600, warmup=100, seed=42
- Extract MCMC draws for posterior party comparisons
- Save figures to figures/ and JSON to results/
```

### Response 2 (Claude)
Wrote `analysis.R` implementing:
- Data loading and recoding (yes→1, no→0, else→NA)
- Wide matrix construction (pivot_wider on poll_id)
- Double-mean imputation (loop over NA indices)
- SVD with AfD-positive orientation check
- Double-centering via sweep() on imputed matrix
- Verification of zero row/col means
- brms 2PL model (Bürkner 2021 formula exactly)
- MCMC draw extraction and party-level averaging
- 8 figures + 3 JSON result files

### Prompt 3 (User)
```
Now write build_site.R. Use @@VAR@@ placeholder syntax to avoid %
escaping conflicts between R sprintf() and CSS/JavaScript. The HTML
should include:
- University of Mannheim teal #3c7a6e color scheme
- Plotly.js scatter with party dropdown
- Narrative sections for all 4 analysis tasks
- Code snippets with syntax highlighting
- Distinction between individual vs party-level uncertainty
- What the model shows vs what we infer
```

### Response 3 (Claude)
Wrote `build_site.R` with:
- Complete HTML template using `@@VAR@@` placeholders
- `gsub()`-based substitution avoiding all sprintf conflicts
- Embedded Plotly.js data as inline JSON (no CORS issues)
- Party color map matching official German party colors
- Interactive dropdown that highlights/dims party traces
- Teal color palette throughout (nav, hero, callouts, code labels)
- Separate callouts distinguishing individual vs. party uncertainty

---

## Design Decisions and Rationale

| Decision | Choice | Reason |
|----------|--------|--------|
| Missing data for SVD | Double-mean imputation | Assignment requirement; preserves marginal tendencies |
| Double-centering | Applied to imputed matrix | Guarantees exact zero row/col means; cleaner verification |
| brms backend | rstan | More stable than cmdstanr for course use |
| Chains/iterations | 1 chain, 600 iter, 100 warmup | Runtime constraint (~45 min budget) |
| Vote orientation | AfD positive | Standard right-positive convention |
| Posterior claim | P(party mean AfD > CDU) | Party ordering is the substantive claim; use draws not point ests |
| Figure format | PNG at 150 DPI, relative paths | Balance: quality vs file size; GitHub Pages serves static files |
| HTML data injection | Inline JSON in `<script>` | No CORS issues; fully self-contained |

---

## Verification Steps

After generating each component, the following checks were performed:

1. **Data structure**: Confirmed long format with correct column names and vote codes
2. **Imputation**: Verified imputed values are bounded (no values outside [0,1] range impossible)
3. **Double-centering**: Verified analytically (sweep() preserves the mathematical property) and numerically (max |row mean| < 1e-14)
4. **brms formula**: Cross-checked line by line against Bürkner (2021) Table 1 and the v100i05.R replication code
5. **Party ordering**: Checked that Die Linke < Grünen < SPD < FDP < CDU/CSU < AfD matches known German political positioning
6. **Posterior draws**: Verified column name format `r_person_id__eta[ID,Intercept]` matches brms internal naming
7. **Plotly**: Verified hover text format, dropdown functionality, and axis labels

---

## Known Limitations

- **Single chain**: Convergence cannot be formally assessed with 1 chain. R̂ diagnostics require ≥2 chains.
- **100 warmup iterations**: Stan may not fully adapt its step size. Some divergences possible.
- **Abstentions as NA**: Treats abstentions and absences as uninformative. If absences are systematically strategic (e.g., sick to avoid a vote), this introduces bias.
- **Government-opposition confound**: The first dimension likely conflates ideological position with government/opposition status. Disentangling these would require modeling legislature composition changes or analyzing only non-party-whipped votes.
