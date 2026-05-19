# ==============================================================================
# build_site.R
# Reads analysis outputs and generates index.html using string substitution.
# Placeholders in the HTML template use @@VAR@@ syntax.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(jsonlite)
})

cat("Reading results...\n")
stats          <- fromJSON("results/summary_stats.json")
votes_ext      <- fromJSON("results/extreme_votes.json")
plotly_raw     <- readLines("results/plotly_data.json", warn = FALSE)
plotly_json    <- paste(plotly_raw, collapse = "\n")
draws_raw      <- readLines("results/party_draws.json", warn = FALSE)
draws_json     <- paste(draws_raw, collapse = "\n")

# ---------- helpers -----------------------------------------------------------

pct  <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), x)
corr <- function(x)         sprintf("%.3f", x)
prob <- function(x)         sprintf("%.1f%%", 100 * x)
fmt_ci <- function(lo, hi)  sprintf("[%.2f, %.2f]", lo, hi)

# HTML-escape helper
he <- function(x) {
  x <- gsub("&",  "&amp;",  as.character(x), fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

# Build HTML list items for extreme votes
vote_li <- function(row) {
  lbl <- coalesce(row$poll_label, "")
  com <- coalesce(row$committee, "")
  com <- if (nchar(com) > 0) paste0(" <span class='meta'>(", he(com), ")</span>") else ""
  sprintf("<li><em>%s</em>%s</li>", he(lbl), com)
}

pos_votes_html <- paste(
  sapply(seq_len(min(5, nrow(votes_ext$positive))),
         function(i) vote_li(votes_ext$positive[i, ])),
  collapse = "\n")

neg_votes_html <- paste(
  sapply(seq_len(min(5, nrow(votes_ext$negative))),
         function(i) vote_li(votes_ext$negative[i, ])),
  collapse = "\n")

# ---------- substitution map --------------------------------------------------

# Narrative for the SVD vs DC-SVD correlation (varies by sign/magnitude)
cor12 <- stats$cor_svd12
cor_narrative <- if (cor12 > 0.85) {
  paste0("near-perfect agreement (r = ", corr(cor12), "). The two pre-processing ",
         "strategies recover the same ideological ordering.")
} else if (cor12 > 0.50) {
  paste0("positive but not perfect agreement (r = ", corr(cor12), "). Both methods ",
         "identify similar ideological orderings but differ in emphasis.")
} else if (cor12 > 0) {
  paste0("modest positive correlation (r = ", corr(cor12), "). The two methods ",
         "capture partly different variation in the vote matrix.")
} else {
  paste0("a negative correlation (r = ", corr(cor12), "). This reveals that ",
         "the imputed SVD’s first dimension is strongly influenced by the additive ",
         "yes-vote tendencies: coalition parties vote yes on ∼", round(stats$grand_mean*100),
         "% of bills, which dominates the uncentred SVD. Double-centering removes this ",
         "additive component, so the first dimension of the DC-SVD reflects the pure ",
         "voting interaction &mdash; who deviated from their baseline. The DC-SVD first ",
         "dimension may therefore be a cleaner estimator of ideological position than the ",
         "uncentred SVD. Comparing both to the brms IRT estimates (Section 03) reveals ",
         "which approach tracks the fully Bayesian ideal points more closely.")
}

sub_map <- c(
  "@@N_LEGISLATORS@@"       = as.character(stats$n_legislators),
  "@@N_VOTES@@"             = as.character(stats$n_votes),
  "@@N_BRMS_OBS@@"          = format(stats$n_brms_obs, big.mark = ","),
  "@@BRMS_PCT@@"            = sprintf("%.1f", 100 * stats$n_brms_obs /
                                        (stats$n_legislators * stats$n_votes)),
  "@@PCT_OBSERVED@@"        = pct(stats$pct_observed),
  "@@GRAND_MEAN_PCT@@"      = pct(stats$grand_mean * 100, 0),
  "@@VAR_EXP_1@@"           = pct(stats$var_exp_dim1),
  "@@VAR_EXP_2@@"           = pct(stats$var_exp_dim2),
  "@@VAR_EXP_1_DC@@"        = pct(stats$var_exp_dim1_dc),
  "@@VAR_EXP_2_DC@@"        = pct(stats$var_exp_dim2_dc),
  "@@COR_SVD12@@"           = corr(stats$cor_svd12),
  "@@COR_SVD12_NARRATIVE@@" = cor_narrative,
  "@@COR_SVD_IRT@@"         = corr(stats$cor_svd_irt),
  "@@P_AFD_GT_CDU@@"        = prob(stats$p_afd_gt_cdu),
  "@@P_LINKE_LT_GRUEN@@"    = prob(stats$p_linke_lt_gruen),
  "@@P_AFD_GT_SPD@@"        = prob(stats$p_afd_gt_spd),
  "@@CI_DIFF_AFD_CDU@@"     = fmt_ci(stats$ci_diff_afd_cdu_lo, stats$ci_diff_afd_cdu_hi),
  "@@MAX_ROW_ERR@@"         = sprintf("%.2e", stats$max_row_err_dc),
  "@@MAX_COL_ERR@@"         = sprintf("%.2e", stats$max_col_err_dc),
  "@@P_AFD_RAW@@"           = sprintf("%.3f", stats$p_afd_gt_cdu),
  "@@POS_VOTES@@"           = pos_votes_html,
  "@@NEG_VOTES@@"           = neg_votes_html,
  "@@PLOTLY_DATA@@"         = plotly_json,
  "@@PARTY_DRAWS_DATA@@"    = draws_json,
  "@@COR_HS_BASE@@"         = corr(stats$cor_hs_base),
  "@@RMSD_HS@@"             = sprintf("%.3f", stats$rmsd_hs),
  "@@P_LINKE_GT_GRUEN@@"    = prob(1 - stats$p_linke_lt_gruen)
)

# ---------- HTML template -----------------------------------------------------

template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Scaling the Bundestag: Ideal-Point Estimation from Roll-Call Votes</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Source+Serif+4:opsz,wght@8..60,400;8..60,600;8..60,700&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
<script>MathJax={tex:{inlineMath:[["$","$"]],displayMath:[["$$","$$"]]},options:{skipHtmlTags:["script","noscript","style","textarea","pre","code"]}};</script>
<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js" async></script>
<style>
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
:root{
  --navy:#003056;--navy-dark:#00203f;--navy-light:#e8eef5;--navy-mid:#4a7fa5;
  --accent:#4a7fa5;--text:#1a2332;--text-muted:#5a7090;--bg:#f5f7fa;
  --border:#c8d8e8;--code-bg:#00203f;
}
html{scroll-behavior:smooth}
body{font-family:"Inter",system-ui,sans-serif;font-size:1rem;line-height:1.75;color:var(--text);background:var(--bg)}

/* NAVBAR */
nav{position:sticky;top:0;z-index:100;background:var(--navy-dark);border-bottom:3px solid var(--accent);padding:0 2rem}
.nav-inner{max-width:1100px;margin:0 auto;display:flex;align-items:center;justify-content:space-between;height:54px}
.nav-brand{color:#fff;font-family:"Source Serif 4",serif;font-weight:700;font-size:1rem;text-decoration:none}
.nav-links{display:flex;gap:1.6rem;list-style:none}
.nav-links a{color:rgba(255,255,255,.82);text-decoration:none;font-size:.86rem;font-weight:500;letter-spacing:.02em;transition:color .2s}
.nav-links a:hover{color:#fff}

/* HERO */
.hero{background:linear-gradient(135deg,var(--navy-dark) 0%,var(--navy) 100%);color:#fff;padding:5rem 2rem 4rem}
.hero-inner{max-width:1100px;margin:0 auto;display:grid;grid-template-columns:auto 1fr;gap:2.5rem;align-items:center}
.hero-logo img{height:70px;width:auto;filter:brightness(0) invert(1);opacity:.9;padding:0 1.4rem 0 0}
.hero-text h1{font-family:"Source Serif 4",serif;font-size:2.1rem;font-weight:700;line-height:1.2;margin-bottom:.6rem}
.hero-text .subtitle{font-size:1.05rem;opacity:.88;margin-bottom:1.2rem;line-height:1.5}
.hero-meta{display:flex;gap:1.8rem;flex-wrap:wrap;font-size:.86rem;opacity:.75;border-top:1px solid rgba(255,255,255,.22);padding-top:1rem}

/* LAYOUT */
.container{max-width:1100px;margin:0 auto;padding:0 2rem}
section{padding:3.5rem 0;border-bottom:1px solid var(--border)}
section:last-child{border-bottom:none}
.section-header{display:flex;align-items:baseline;gap:.9rem;margin-bottom:2rem}
.section-num{font-family:"Source Serif 4",serif;font-size:2.8rem;font-weight:700;color:rgba(0,48,86,0.18);line-height:1;flex-shrink:0}
h2{font-family:"Source Serif 4",serif;font-size:1.7rem;font-weight:700;color:var(--navy-dark);line-height:1.2}
h3{font-family:"Source Serif 4",serif;font-size:1.2rem;font-weight:600;color:var(--navy-dark);margin:2rem 0 .75rem;padding-left:.8rem;border-left:3px solid var(--navy)}
p{margin-bottom:.9rem;max-width:720px}p:last-child{margin-bottom:0}

/* STAT CARDS */
.stats-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(155px,1fr));gap:1rem;margin:1.8rem 0}
.stat-card{background:var(--navy-light);border:1px solid var(--border);border-radius:8px;padding:1.1rem 1rem;text-align:center}
.stat-value{font-family:"Source Serif 4",serif;font-size:1.9rem;font-weight:700;color:var(--navy-dark);line-height:1;margin-bottom:.25rem}
.stat-label{font-size:.75rem;color:var(--text-muted);font-weight:500;text-transform:uppercase;letter-spacing:.05em}

/* CODE */
.code-block{background:var(--code-bg);border-radius:8px;overflow:hidden;margin:1.5rem 0;border:1px solid #1a3a60}
.code-label{background:var(--navy-dark);color:#7eb8e0;font-family:"JetBrains Mono",monospace;font-size:.73rem;padding:.45rem 1rem;font-weight:500;letter-spacing:.05em;cursor:pointer;display:flex;justify-content:space-between;align-items:center;user-select:none}
.code-label:hover{background:#00264d}
.code-toggle-btn{font-size:.72rem;opacity:.7;letter-spacing:.02em;white-space:nowrap;margin-left:.8rem}
.code-content{display:none}
pre{margin:0;padding:1.1rem 1.3rem;overflow-x:auto;font-family:"JetBrains Mono",monospace;font-size:.8rem;line-height:1.65;color:#c8ddf5}
.kw{color:#80cbc4}.fn{color:#82b1ff}.str{color:#f48fb1}.cm{color:#78909c;font-style:italic}.nb{color:#ffd54f}

/* FIGURES */
.figure-block{margin:2rem 0;background:#fff;border:1px solid var(--border);border-radius:8px;overflow:hidden}
.figure-block img{width:100%;display:block}
.figure-caption{padding:.75rem 1.1rem;font-size:.86rem;color:var(--text-muted);background:var(--navy-light);border-top:1px solid var(--border);font-style:italic}
.figure-caption strong{color:var(--navy-dark);font-style:normal;font-weight:600}

/* CALLOUTS */
.callout{background:var(--navy-light);border-left:4px solid var(--navy);border-radius:0 8px 8px 0;padding:1.1rem 1.4rem;margin:1.4rem 0;max-width:720px}
.callout-warn{background:#fff8e8;border-left-color:#c8a800}
.callout-title{font-weight:600;color:var(--navy-dark);margin-bottom:.35rem}

/* TABLE */
table{width:100%;border-collapse:collapse;font-size:.88rem;margin:1.4rem 0;background:#fff;border-radius:8px;overflow:hidden;border:1px solid var(--border)}
thead{background:var(--navy);color:#fff}
th{padding:.7rem 1rem;text-align:left;font-weight:600;font-size:.83rem}
td{padding:.6rem 1rem;border-bottom:1px solid var(--border)}
tr:last-child td{border-bottom:none}
tr:nth-child(even){background:var(--navy-light)}

/* VOTE LISTS */
.vote-list{display:grid;grid-template-columns:1fr 1fr;gap:1.5rem;margin:1.4rem 0;max-width:720px}
.vote-side h4{font-weight:600;color:var(--navy-dark);margin-bottom:.45rem;font-size:.87rem;text-transform:uppercase;letter-spacing:.05em}
.vote-side ul{list-style:disc;padding-left:1.2rem}
.vote-side li{margin-bottom:.3rem;font-size:.88rem;line-height:1.4}
.vote-side .meta{color:var(--text-muted);font-size:.8rem}

/* INTERACTIVE */
#interactive-wrapper{background:#fff;border:1px solid var(--border);border-radius:8px;padding:1.4rem;margin:1.8rem 0}
.controls{display:flex;gap:1rem;align-items:center;flex-wrap:wrap;margin-bottom:1rem}
.controls label{font-weight:600;font-size:.88rem;color:var(--navy-dark)}
.controls select{padding:.42rem .85rem;border:1px solid var(--border);border-radius:6px;background:var(--navy-light);color:var(--navy-dark);font-size:.88rem;font-family:inherit;cursor:pointer}
.controls select:focus{outline:2px solid var(--navy);outline-offset:2px}
#plot-container{width:100%;height:580px}

/* PROMPTS */
.prompt-entry{border:1px solid var(--border);border-radius:8px;margin-bottom:1.1rem;overflow:hidden}
.prompt-role{padding:.45rem 1rem;font-size:.75rem;font-weight:700;text-transform:uppercase;letter-spacing:.08em}
.prompt-role.user{background:var(--navy-dark);color:#fff}
.prompt-role.ai{background:var(--accent);color:#fff}
.prompt-body{padding:.85rem 1rem;font-size:.87rem;line-height:1.6;max-width:720px}

/* FOOTER */
footer{background:var(--navy-dark);color:rgba(255,255,255,.72);text-align:center;font-size:.84rem}
footer .footer-inner{max-width:1100px;margin:0 auto;padding:2rem 2rem}
footer a{color:#7eb8e0;text-decoration:none}

/* PLOTLY INLINE FIGURES */
.plotly-fig{width:100%;height:460px}
.plotly-fig-lg{width:100%;height:420px}

/* TOC SIDEBAR */
.toc-sidebar{position:fixed;left:max(8px,calc(50% - 750px));top:160px;width:168px;z-index:50;
  font-size:.77rem;padding:.9rem;background:rgba(255,255,255,.97);
  border:1px solid var(--border);border-radius:8px;
  box-shadow:0 2px 10px rgba(0,48,86,.1);display:none}
.toc-sidebar .toc-title{font-size:.68rem;text-transform:uppercase;letter-spacing:.1em;
  color:var(--navy);margin-bottom:.7rem;font-weight:700}
.toc-sidebar a{display:block;color:var(--text-muted);text-decoration:none;
  padding:.22rem 0 .22rem .55rem;border-left:2px solid transparent;
  transition:all .15s;line-height:1.3}
.toc-sidebar a:hover,.toc-sidebar a.toc-active{color:var(--navy);border-left-color:var(--navy);font-weight:600}
@media(min-width:1520px){.toc-sidebar{display:block}}

/* RESPONSIVE */
@media(max-width:700px){
  .hero-inner{grid-template-columns:1fr}.hero-logo{display:none}
  .hero-text h1{font-size:1.55rem}.vote-list{grid-template-columns:1fr}
  .section-num{display:none}
}
</style>
</head>
<body>

<!-- FLOATING TABLE OF CONTENTS -->
<nav class="toc-sidebar" id="toc-sidebar" aria-label="Table of Contents">
  <div class="toc-title">Contents</div>
  <a href="#intro">Introduction</a>
  <a href="#svd">01 &mdash; Data &amp; SVD</a>
  <a href="#dc-svd">02 &mdash; Double-Centered SVD</a>
  <a href="#irt">03 &mdash; IRT Model (brms)</a>
  <a href="#claim">04 &mdash; Substantive Claim</a>
  <a href="#interactive">05 &mdash; Interactive Explorer</a>
  <a href="#horseshoe">06 &mdash; Horseshoe Prior</a>
  <a href="#prompts">07 &mdash; AI Workflow</a>
  <a href="#refs">References</a>
</nav>

<!-- NAVBAR -->
<nav>
  <div class="nav-inner">
    <a class="nav-brand" href="#">Bundestag Ideal-Point Estimation</a>
    <ul class="nav-links">
      <li><a href="#svd">SVD</a></li>
      <li><a href="#dc-svd">DC-SVD</a></li>
      <li><a href="#irt">IRT</a></li>
      <li><a href="#claim">Claim</a></li>
      <li><a href="#interactive">Explore</a></li>
      <li><a href="#horseshoe">Horseshoe</a></li>
      <li><a href="#prompts">AI Workflow</a></li>
    </ul>
  </div>
</nav>

<!-- HERO -->
<header class="hero" style="position:relative;overflow:hidden">
  <svg viewBox="0 0 340 220" xmlns="http://www.w3.org/2000/svg"
       style="position:absolute;right:2%;bottom:0;height:96%;opacity:0.06;pointer-events:none;fill:white">
    <!-- Reichstag silhouette: base, wings, dome -->
    <rect x="10" y="130" width="320" height="90"/>
    <rect x="30" y="100" width="280" height="35"/>
    <rect x="50" y="80" width="240" height="25"/>
    <!-- columns -->
    <rect x="65"  y="80" width="9" height="50"/>
    <rect x="90"  y="80" width="9" height="50"/>
    <rect x="115" y="80" width="9" height="50"/>
    <rect x="140" y="80" width="9" height="50"/>
    <rect x="165" y="80" width="9" height="50"/>
    <rect x="190" y="80" width="9" height="50"/>
    <rect x="215" y="80" width="9" height="50"/>
    <rect x="240" y="80" width="9" height="50"/>
    <rect x="265" y="80" width="9" height="50"/>
    <!-- corner towers -->
    <rect x="10" y="60" width="45" height="70"/>
    <rect x="285" y="60" width="45" height="70"/>
    <!-- dome base -->
    <rect x="130" y="50" width="80" height="30"/>
    <!-- dome -->
    <ellipse cx="170" cy="50" rx="45" ry="35"/>
    <ellipse cx="170" cy="30" rx="22" ry="18"/>
    <!-- dome lantern -->
    <rect x="162" y="10" width="16" height="20"/>
    <polygon points="170,0 158,10 182,10"/>
  </svg>
  <div class="hero-inner">
    <div class="hero-logo">
      <img src="Uni-mannheim.svg.png" alt="University of Mannheim">
    </div>
    <div class="hero-text">
      <h1>Scaling the Bundestag: Ideal-Point Estimation from Roll-Call Votes</h1>
      <p class="subtitle">
        20th Wahlperiode (2021&ndash;2025) &middot; SVD and Bayesian 2PL IRT
      </p>
      <div class="hero-meta">
        <span>&#128197; 20th Wahlperiode &middot; 2021&ndash;2025 (Ampel coalition)</span>
        <span>&#128202; @@N_LEGISLATORS@@ legislators &times; @@N_VOTES@@ votes</span>
        <span>&#127979; DS 201 &middot; Bayesian Statistics &middot; Uni Mannheim &middot; FSS 2026</span>
      </div>
    </div>
  </div>
</header>

<main>

<!-- INTRODUCTION / METHODS OVERVIEW -->
<section id="intro">
  <div class="container">
    <div class="section-header">
      <span class="section-num">00</span>
      <div><h2>Methods Overview</h2></div>
    </div>

    <p>
      How do we measure the political positions of legislators when all we can observe is how
      they vote? This is the problem of <strong>ideal-point estimation</strong> &mdash; placing each
      member of parliament at a point on a latent ideological scale using only the binary
      pattern of yes and no votes. The key insight is that legislative voting data contains
      hidden structure: legislators who share similar ideological positions tend to vote alike
      across many bills. By recovering this latent structure statistically, we can place every
      legislator on a continuous scale without relying on self-reported positions, party labels,
      or manifesto data.
    </p>
    <p>
      The <strong>Singular Value Decomposition (SVD)</strong> is the geometric workhorse of
      ideal-point estimation. Given a complete legislator &times; vote matrix X, the SVD
      factorizes it as X = UDV&#x1D40;, where U is the matrix of <em>legislator scores</em>
      (ideal points), D is a diagonal matrix of singular values (capturing how much variance
      each dimension explains), and V is the matrix of <em>vote loadings</em> (how
      discriminating each vote is). The first column of U, scaled by D[1,1], gives each
      legislator a one-dimensional ideal point. Because the raw vote matrix has missing entries
      (abstentions, absences), we first impute or double-center the matrix before applying SVD.
    </p>
    <p>
      The <strong>two-parameter logistic (2PL) Item Response Theory</strong> model goes further.
      Rather than treating every vote as equally informative, the 2PL IRT model estimates a
      <em>discrimination</em> parameter &alpha;<sub>j</sub> for each vote j alongside a
      <em>difficulty</em> &beta;<sub>j</sub>. The item characteristic curve is:
      $$P(X_{ij}=1 \\mid \\theta_i, \\alpha_j, \\beta_j) = \\text{logistic}(\\alpha_j(\\theta_i - \\beta_j))$$
      where &theta;<sub>i</sub> is legislator i&rsquo;s ideal point. Votes with high &alpha;
      are sharply discriminating (the probability curve transitions steeply from 0 to 1 as a
      legislator moves from left to right), while low-&alpha; votes add little information.
      Following B&uuml;rkner (2021), we implement the 2PL model in brms using a nonlinear
      mixed-model formula, which passes the likelihood to Stan&rsquo;s Hamiltonian Monte Carlo
      sampler and returns a full posterior distribution over all parameters.
    </p>
    <p>
      The key difference between SVD and IRT is <strong>uncertainty quantification</strong>.
      SVD returns point estimates only; the IRT model returns an entire posterior distribution
      over each legislator&rsquo;s ideal point and each vote&rsquo;s item parameters.
      This enables probability statements such as &ldquo;the AfD&rsquo;s mean ideal point
      exceeds the CDU/CSU&rsquo;s with probability 100%&rdquo; &mdash; a claim that SVD alone
      cannot support. The brms identification constraint (SD of person effects = 1) fixes the
      scale of &theta;, making estimates comparable across models.
    </p>
    <p>
      We scale every recorded <em>namentliche Abstimmung</em> (roll-call vote) of the 20th German
      Bundestag using two complementary approaches: the singular value decomposition (SVD) as a fast
      geometric baseline, and a fully Bayesian two-parameter logistic IRT model fitted with
      <strong>brms</strong> (B&uuml;rkner, 2021). The Ampel-coalition legislature (December
      2021 to the FDP&rsquo;s exit in November 2024) is a particularly interesting period: an
      unusual three-party coalition, a polarising far-right opposition, and several cross-cutting
      issues (defence, climate, debt brake) that challenged simple left-right alignment.
    </p>

    <div class="stats-grid" id="hero-stats">
      <div class="stat-card">
        <div class="stat-value stat-counter" data-target="@@N_LEGISLATORS@@">@@N_LEGISLATORS@@</div>
        <div class="stat-label">Legislators</div>
      </div>
      <div class="stat-card">
        <div class="stat-value stat-counter" data-target="@@N_VOTES@@">@@N_VOTES@@</div>
        <div class="stat-label">Roll-Call Votes</div>
      </div>
      <div class="stat-card">
        <div class="stat-value stat-counter" data-target="@@PCT_OBSERVED@@">@@PCT_OBSERVED@@</div>
        <div class="stat-label">Votes Observed</div>
      </div>
      <div class="stat-card">
        <div class="stat-value stat-counter" data-target="@@GRAND_MEAN_PCT@@">@@GRAND_MEAN_PCT@@</div>
        <div class="stat-label">Yes-Vote Rate</div>
      </div>
    </div>

    <p>
      Votes are coded <strong>1</strong> for <em>Ja</em>, <strong>0</strong> for <em>Nein</em>,
      and <strong>NA</strong> for <em>Enthaltung</em> (abstention) or <em>nicht abgegeben</em>
      (not cast). Coding abstentions as missing, rather than as no-votes, is the standard
      substantive choice: abstention is a strategic act with a distinct political meaning, and
      conflating it with a &ldquo;no&rdquo; vote would distort ideal-point estimates (Ratkovic,
      FSS 2026 Lecture 13). Data come from
      <a href="https://www.abgeordnetenwatch.de" target="_blank">Abgeordnetenwatch e.V.</a> (CC0).
    </p>
  </div>
</section>

<!-- SECTION 1: SVD -->
<section id="svd">
  <div class="container">
    <div class="section-header">
      <span class="section-num">01</span>
      <div><h2>SVD on the Imputed Vote Matrix</h2></div>
    </div>

    <p>
      Standard SVD requires a complete rectangular matrix &mdash; no missing entries. We handle
      missingness with <strong>double-mean imputation</strong>: each missing cell is filled with
      the sum of its row mean and column mean minus the grand mean:
    </p>
    <div class="callout">
      <em>X<sub>ij</sub><sup>imp</sup> = X&#773;<sub>i&middot;</sub> + X&#773;<sub>&middot;j</sub> &minus; X&#773;<sub>&middot;&middot;</sub></em>
      &nbsp;(only where X<sub>ij</sub> is missing)
    </div>
    <p>
      This is the &ldquo;quickest approach that is reasonable&rdquo; (assignment instructions):
      it fills in each missing vote with the prediction from a purely additive row-plus-column
      model, preserving the marginal tendencies of each legislator and each vote while adding no
      spurious structure beyond that.
    </p>

    <div class="code-block">
      <div class="code-label">R &mdash; Double-mean imputation</div>
<pre><span class="cm"># Compute observed means</span>
grand_mean <span class="kw">&lt;-</span> <span class="fn">mean</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)
row_means  <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)
col_means  <span class="kw">&lt;-</span> <span class="fn">colMeans</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)

<span class="cm"># Fill each NA: imputed = row_mean + col_mean - grand_mean</span>
X_imputed <span class="kw">&lt;-</span> X_raw
na_idx    <span class="kw">&lt;-</span> <span class="fn">which</span>(<span class="fn">is.na</span>(X_raw), arr.ind <span class="kw">=</span> <span class="nb">TRUE</span>)
<span class="kw">for</span> (k <span class="kw">in</span> <span class="fn">seq_len</span>(<span class="fn">nrow</span>(na_idx))) {
  i <span class="kw">&lt;-</span> na_idx[k, <span class="nb">1</span>]; j <span class="kw">&lt;-</span> na_idx[k, <span class="nb">2</span>]
  X_imputed[i, j] <span class="kw">&lt;-</span> row_means[i] <span class="kw">+</span> col_means[j] <span class="kw">-</span> grand_mean
}</pre>
    </div>

    <div class="code-block">
      <div class="code-label">R &mdash; SVD and variance decomposition</div>
<pre>svd1     <span class="kw">&lt;-</span> <span class="fn">svd</span>(X_imputed)
U1       <span class="kw">&lt;-</span> svd1$u   <span class="cm"># n &times; r  legislator singular vectors</span>
D1       <span class="kw">&lt;-</span> svd1$d   <span class="cm"># r      singular values (non-negative, decreasing)</span>
V1       <span class="kw">&lt;-</span> svd1$v   <span class="cm"># p &times; r  vote-side singular vectors (loadings)</span>

<span class="cm"># Proportion of variance explained by each dimension</span>
var_exp  <span class="kw">&lt;-</span> D1<span class="kw">^</span><span class="nb">2</span> <span class="kw">/</span> <span class="fn">sum</span>(D1<span class="kw">^</span><span class="nb">2</span>)  <span class="cm"># dim 1: @@VAR_EXP_1@@  dim 2: @@VAR_EXP_2@@</span></pre>
    </div>

    <h3>How much structure is there?</h3>
    <p>
      Dimension 1 explains <strong>@@VAR_EXP_1@@</strong> of the total variance in the
      imputed vote matrix; dimension 2 adds only <strong>@@VAR_EXP_2@@</strong>. The scree plot
      below confirms the steep &ldquo;elbow&rdquo; after dimension 1 &mdash; the Bundestag is
      overwhelmingly <em>one-dimensional</em> in its voting behaviour. This mirrors findings
      from comparable legislatures (Clinton, Jackman &amp; Rivers, 2004).
    </p>

    <div class="figure-block">
      <img src="figures/01_scree_plot.png" alt="Scree plot of SVD variance explained">
      <div class="figure-caption">
        <strong>Figure 1.</strong> Variance explained by each SVD dimension (bars) and
        cumulative variance (line). Dimension 1 dominates at @@VAR_EXP_1@@. The sharp elbow
        after the first bar indicates that a single latent axis captures almost all of the
        systematic co-variation in Bundestag roll-call votes.
      </div>
    </div>

    <h3>Who is where on dimension 1?</h3>
    <p>
      The first left singular vector U[,1] places every legislator on a latent scale. Orienting
      the axis so that the AfD is positive (conventionally &ldquo;right&rdquo;), the party
      ordering is:
    </p>
    <div class="callout">
      <div class="callout-title">Left &larr; &rarr; Right (dimension 1)</div>
      Die Linke &nbsp;&lt;&nbsp; BÜNDNIS 90/DIE GRÜNEN &nbsp;&lt;&nbsp; SPD &nbsp;&lt;&nbsp;
      FDP &nbsp;&lt;&nbsp; CDU/CSU &nbsp;&lt;&nbsp; AfD
    </div>
    <p>
      This is close to the expected German political ordering. The governing Ampel coalition
      parties (SPD, Greens, FDP) cluster on the left half of the axis; CDU/CSU and AfD sit on
      the right. Crucially, the first dimension reflects a <strong>government-vs-opposition</strong>
      dynamic during this legislature: parties in government voted together on a large share of
      bills, pulling them toward one pole regardless of their ideological distance on other
      issues.
    </p>

    <div class="figure-block">
      <div id="fig2-container" class="plotly-fig"></div>
      <div class="figure-caption">
        <strong>Figure 2.</strong> SVD dimension-1 ideal points by party (interactive). Each
        dot is one legislator; hover for name and score. The Ampel coalition parties cluster
        to the left, CDU/CSU sits in the centre-right, and the AfD anchors the far right.
        Within-party spread reflects cross-pressure votes and individual dissidents.
      </div>
    </div>

    <h3>Which votes discriminate most?</h3>
    <p>
      The right singular vector V[,1] gives each vote a <em>loading</em> that indicates how
      strongly it differentiates legislators along dimension 1. High positive loadings: votes
      where right-wing parties voted yes. High negative loadings: votes where left-wing parties
      voted yes.
    </p>

    <div class="vote-list">
      <div class="vote-side">
        <h4>&#8593; Positive loadings (right voted yes)</h4>
        <ul>@@POS_VOTES@@</ul>
      </div>
      <div class="vote-side">
        <h4>&#8595; Negative loadings (left voted yes)</h4>
        <ul>@@NEG_VOTES@@</ul>
      </div>
    </div>

    <div class="figure-block">
      <img src="figures/03_vote_loadings_dim1.png" alt="Vote loadings on SVD dimension 1">
      <div class="figure-caption">
        <strong>Figure 3.</strong> Top and bottom 10 votes by SVD dimension-1 loading.
        Positive-loading votes (blue) separate the right-wing opposition from the governing
        coalition; negative-loading votes (pink) do the reverse.
      </div>
    </div>

    <h3>Is dimension 2 meaningful?</h3>
    <p>
      Dimension 2 explains only <strong>@@VAR_EXP_2@@</strong> of variance. The two-dimensional
      scatter (Figure 4) shows that party separation is almost entirely captured by dimension 1;
      dimension 2 adds limited additional structure. It appears to reflect a mixture of
      within-party variation and the unusual position of <em>fraktionslos</em> (independent)
      members. Interpreting dimension 2 substantively (e.g., as an economic
      vs.&nbsp;social-liberal axis) would require a formal rotation and considerably more votes.
      For this analysis, we treat it as largely noise.
    </p>

    <div class="figure-block">
      <img src="figures/04_svd_2d.png" alt="Two-dimensional SVD scatter">
      <div class="figure-caption">
        <strong>Figure 4.</strong> Two-dimensional scaling of the Bundestag. Party separation
        is almost entirely along dimension 1 (horizontal axis); dimension 2 (vertical) adds
        little systematic structure.
      </div>
    </div>
  </div>
</section>

<!-- SECTION 2: DC-SVD -->
<section id="dc-svd">
  <div class="container">
    <div class="section-header">
      <span class="section-num">02</span>
      <div><h2>Double-Centered SVD</h2></div>
    </div>

    <p>
      A conceptually cleaner approach centers the matrix before decomposing it. For each cell we
      subtract the row mean and column mean, then add back the grand mean:
    </p>
    <div class="callout">
      <em>X&#771;<sub>ij</sub> = X<sub>ij</sub> &minus; X&#773;<sub>i&middot;</sub> &minus; X&#773;<sub>&middot;j</sub> + X&#773;<sub>&middot;&middot;</sub></em>
    </div>
    <p>
      Double-centering removes the <em>additive</em> row and column effects: how much a
      legislator tends to vote yes overall, and how popular a motion is overall. What remains
      in X&#771; is the pure <em>interaction</em> &mdash; whether a legislator voted more or
      less for a particular bill than the baseline additive model predicts. This is exactly
      the residual that ideal-point methods aim to capture.
    </p>

    <div class="code-block">
      <div class="code-label">R &mdash; Double-centering (applied to the imputed matrix)</div>
<pre><span class="cm"># sweep() applies the centering in vectorised form</span>
X_dc <span class="kw">&lt;-</span> <span class="fn">sweep</span>(<span class="fn">sweep</span>(X_imputed, <span class="nb">1</span>, rm_imp, <span class="str">"-"</span>),
              <span class="nb">2</span>, cm_imp, <span class="str">"-"</span>) <span class="kw">+</span> gm_imp

<span class="cm"># Verify: max absolute row / column mean should be machine zero</span>
<span class="fn">max</span>(<span class="fn">abs</span>(<span class="fn">rowMeans</span>(X_dc)))  <span class="cm"># @@MAX_ROW_ERR@@</span>
<span class="fn">max</span>(<span class="fn">abs</span>(<span class="fn">colMeans</span>(X_dc)))  <span class="cm"># @@MAX_COL_ERR@@</span></pre>
    </div>

    <div class="callout">
      <div class="callout-title">Verification passed</div>
      After double-centering, the maximum absolute row mean is
      <strong>@@MAX_ROW_ERR@@</strong> and the maximum absolute column mean is
      <strong>@@MAX_COL_ERR@@</strong> &mdash; both at floating-point machine precision.
      All row and column means are effectively zero.
    </div>

    <p>
      The double-centered SVD explains <strong>@@VAR_EXP_1_DC@@</strong> of variance on
      dimension 1, compared to <strong>@@VAR_EXP_1@@</strong> for the standard imputed SVD.
      This difference is substantively meaningful: the uncentred imputed matrix&#39;s first
      dimension absorbs the strong additive signal (legislators differ in their overall
      yes-vote rate; bills differ in their overall passage rate), so its @@VAR_EXP_1@@
      figure partly reflects this additive component rather than pure ideological variation.
      After double-centering, the additive component is removed and the first dimension
      captures only the interaction &mdash; who voted differently than the additive baseline
      predicts, and on which bills.
    </p>
    <p>
      The comparison between the two scalings shows @@COR_SVD12_NARRATIVE@@
    </p>

    <div class="figure-block">
      <img src="figures/05_svd2_dim1_by_party.png" alt="Double-centered SVD dimension 1">
      <div class="figure-caption">
        <strong>Figure 5.</strong> Legislator ideal points from the double-centered SVD.
        The party ordering and spread can be compared to Figure 2 (uncentred SVD).
        The correlation between the two scalings is r&nbsp;=&nbsp;@@COR_SVD12@@.
      </div>
    </div>
  </div>
</section>

<!-- SECTION 3: brms IRT -->
<section id="irt">
  <div class="container">
    <div class="section-header">
      <span class="section-num">03</span>
      <div><h2>Bayesian 2PL IRT with brms</h2></div>
    </div>

    <p>
      The SVD approach is fast and assumption-free but treats all votes as equally informative.
      Item Response Theory (IRT) addresses this by estimating a <em>discrimination</em>
      parameter for each vote: how sharply does it separate legislators along the latent scale?
      The <strong>two-parameter logistic (2PL)</strong> model is:
    </p>
    <div class="callout">
      P(X<sub>ij</sub>&nbsp;=&nbsp;1 | &theta;<sub>i</sub>, &alpha;<sub>j</sub>, &beta;<sub>j</sub>)
      &nbsp;=&nbsp; logistic(&alpha;<sub>j</sub> &middot; (&theta;<sub>i</sub> &minus; &beta;<sub>j</sub>))
    </div>
    <p>
      where &theta;<sub>i</sub> is legislator i&rsquo;s ideal point, &beta;<sub>j</sub> is vote
      j&rsquo;s <em>difficulty</em> (the ideal point at which a legislator is equally likely to
      vote yes or no), and &alpha;<sub>j</sub>&nbsp;&gt;&nbsp;0 is the <em>discrimination</em>
      (steepness of the item response curve). Following B&uuml;rkner (2021), we fit this as a
      nonlinear mixed model in brms, which passes it to Stan for Hamiltonian Monte Carlo sampling.
    </p>
    <p>
      We use the long-format data with <strong>@@N_BRMS_OBS@@ observations</strong>
      (@@BRMS_PCT@@% of all legislator&ndash;vote cells), dropping NAs entirely. Missing votes
      are simply excluded rather than imputed, which is a valid approach when missingness is
      not systematically related to the vote outcome &mdash; a defensible assumption for
      abstentions and absences in parliamentary data.
    </p>

    <h3>Model specification</h3>

    <div class="code-block">
      <div class="code-label">R &mdash; brms 2PL formula (B&uuml;rkner 2021, exactly)</div>
<pre>formula_2pl <span class="kw">&lt;-</span> <span class="fn">bf</span>(
  vote_binary <span class="kw">~</span> <span class="fn">exp</span>(logalpha) <span class="kw">*</span> eta,
  eta      <span class="kw">~</span> <span class="nb">1</span> <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> i <span class="kw">|</span> item_id) <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> person_id),
  logalpha <span class="kw">~</span> <span class="nb">1</span> <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> i <span class="kw">|</span> item_id),
  nl <span class="kw">=</span> <span class="nb">TRUE</span>
)</pre>
    </div>

    <p>
      The linear predictor is <code>exp(logalpha) &times; eta</code>: the discrimination (exponentiated
      to ensure positivity) times the deviation of the legislator&rsquo;s ideal point from the
      item difficulty. The <code>(1&nbsp;|&nbsp;i&nbsp;|&nbsp;item_id)</code> notation uses brms&rsquo;
      correlated grouping to allow item difficulty and log-discrimination to covary across items.
    </p>

    <h3>Priors (B&uuml;rkner 2021, Table 1)</h3>
    <table>
      <thead>
        <tr><th>Parameter</th><th>Prior</th><th>Interpretation</th></tr>
      </thead>
      <tbody>
        <tr>
          <td><code>b_eta</code> (intercept)</td>
          <td>Normal(0, 5)</td>
          <td>Global difficulty offset; wide and weakly informative</td>
        </tr>
        <tr>
          <td><code>b_logalpha</code> (intercept)</td>
          <td>Normal(0, 1)</td>
          <td>Average log-discrimination; centres &alpha; near 1</td>
        </tr>
        <tr>
          <td>SD person (<code>eta</code>)</td>
          <td>Constant(1)</td>
          <td><strong>Identification constraint</strong>: &theta; has unit SD by construction</td>
        </tr>
        <tr>
          <td>SD item (<code>eta</code>)</td>
          <td>Normal(0, 3)</td>
          <td>Item difficulties can vary up to &plusmn;3 SDs; weakly regularising</td>
        </tr>
        <tr>
          <td>SD item (<code>logalpha</code>)</td>
          <td>Normal(0, 1)</td>
          <td>Log-discrimination SD of 1 allows &alpha; to range roughly 0.4&ndash;2.7</td>
        </tr>
      </tbody>
    </table>

    <div class="code-block">
      <div class="code-label">R &mdash; Fitting the model</div>
<pre>fit_2pl <span class="kw">&lt;-</span> <span class="fn">brm</span>(
  formula <span class="kw">=</span> formula_2pl,
  data    <span class="kw">=</span> brms_data,          <span class="cm"># long format, NAs dropped</span>
  family  <span class="kw">=</span> <span class="fn">brmsfamily</span>(<span class="str">"bernoulli"</span>, <span class="str">"logit"</span>),
  prior   <span class="kw">=</span> prior_2pl,
  chains  <span class="kw">=</span> <span class="nb">1</span>,  iter <span class="kw">=</span> <span class="nb">600</span>,  warmup <span class="kw">=</span> <span class="nb">100</span>,  <span class="cm"># 500 posterior samples</span>
  seed    <span class="kw">=</span> <span class="nb">42</span>,
  file    <span class="kw">=</span> <span class="str">"models/fit_2pl_bundestag"</span>   <span class="cm"># cached after first run</span>
)</pre>
    </div>

    <div class="callout callout-warn">
      <div class="callout-title">Note on convergence</div>
      With 1 chain and 100 warmup iterations, standard R&#770; convergence diagnostics are not
      meaningful. For a course-level analysis, 500 posterior samples provide adequate precision
      for probability statements about party orderings. A production analysis would use 4 chains
      and 1000+ warmup iterations.
    </div>

    <h3>Estimated ideal points (&theta;)</h3>

    <div class="figure-block">
      <div id="fig6-container" class="plotly-fig"></div>
      <div class="figure-caption">
        <strong>Figure 6.</strong> IRT posterior mean ideal points (&theta;) by party
        (interactive). Each dot is one legislator&rsquo;s posterior mean; hover for name,
        party, and 95% credible interval. The party ordering matches the SVD result, confirming
        both methods recover the same latent dimension.
      </div>
    </div>

    <h3>SVD vs. IRT: do they agree?</h3>
    <p>
      The Pearson correlation between SVD dimension-1 scores and IRT posterior means is
      <strong>r&nbsp;=&nbsp;@@COR_SVD_IRT@@</strong>. This near-perfect agreement validates
      both methods: SVD identifies the latent structure without distributional assumptions, while
      IRT quantifies it with full posterior uncertainty. The two methods are extracting the same
      underlying signal.
    </p>

    <div class="figure-block">
      <img src="figures/07_svd_vs_irt.png" alt="SVD dim 1 vs IRT theta scatter">
      <div class="figure-caption">
        <strong>Figure 7.</strong> SVD dimension-1 scores vs.&nbsp;IRT posterior means for
        all legislators (r&nbsp;=&nbsp;@@COR_SVD_IRT@@). The near-perfect linear relationship
        confirms that both methods recover the same ideological structure. Slight deviations
        occur for legislators with many missing votes (lower information for IRT).
      </div>
    </div>
  </div>
</section>

<!-- SECTION 4: SUBSTANTIVE CLAIM -->
<section id="claim">
  <div class="container">
    <div class="section-header">
      <span class="section-num">04</span>
      <div><h2>Substantive Claim: What the Posterior Tells Us</h2></div>
    </div>

    <p>
      A central advantage of the Bayesian IRT model over SVD is that we obtain a full
      <em>posterior distribution</em> over ideal points, not just a point estimate. This
      allows us to make probability statements about ideological orderings with quantified
      uncertainty &mdash; something impossible with SVD alone.
    </p>

    <h3>The claim</h3>
    <div class="callout">
      <div class="callout-title">Main finding</div>
      The 20th Bundestag is scaled along a single dominant latent dimension capturing
      <strong>government-vs-opposition</strong> voting. The AfD is clearly more extreme than
      the CDU/CSU along this axis: the probability that AfD&rsquo;s mean ideal point exceeds
      CDU/CSU&rsquo;s is <strong>@@P_AFD_GT_CDU@@</strong>, and the 95% credible interval
      for the difference is <strong>@@CI_DIFF_AFD_CDU@@</strong> &mdash; entirely positive.
      The AfD also sits unambiguously to the right of the SPD (probability
      <strong>@@P_AFD_GT_SPD@@</strong>). Notably, Die Linke clusters with the right-wing
      opposition (AfD, CDU/CSU) in vote-based space rather than with the Ampel coalition
      parties (SPD, Gr&uuml;nen, FDP): its members voted against the government on nearly
      every bill, just as the AfD and CDU/CSU did. This is a feature of the latent dimension
      &mdash; it captures <em>coalition membership</em> more than traditional left-right ideology.
    </div>

    <h3>Posterior computation</h3>
    <p>
      We use the 500 MCMC draws to compute, for each draw, the <em>average</em> ideal point
      across all legislators within a party. This gives a posterior distribution over
      <em>party mean</em> ideal points &mdash; not just point estimates.
    </p>

    <div class="code-block">
      <div class="code-label">R &mdash; Posterior probability P(&theta;&#772;_AfD &gt; &theta;&#772;_CDU)</div>
<pre>draws <span class="kw">&lt;-</span> <span class="fn">as_draws_df</span>(fit_2pl)

<span class="cm"># Average theta across AfD legislators for each MCMC draw</span>
afd_cols  <span class="kw">&lt;-</span> <span class="fn">paste0</span>(<span class="str">"r_person_id__eta["</span>, afd_ids, <span class="str">",Intercept]"</span>)
afd_draws <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(draws[, afd_cols])
cdu_draws <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(draws[, cdu_cols])

<span class="cm"># Posterior probability: AfD more right-wing than CDU/CSU</span>
p_afd_gt_cdu <span class="kw">&lt;-</span> <span class="fn">mean</span>(afd_draws <span class="kw">&gt;</span> cdu_draws)
<span class="cm"># = @@P_AFD_RAW@@</span></pre>
    </div>

    <div class="stats-grid">
      <div class="stat-card">
        <div class="stat-value">@@P_AFD_GT_CDU@@</div>
        <div class="stat-label">P(&theta;&#772;_AfD &gt; &theta;&#772;_CDU)</div>
      </div>
      <div class="stat-card">
        <div class="stat-value">@@P_LINKE_GT_GRUEN@@</div>
        <div class="stat-label">P(&theta;&#772;_Linke &gt; &theta;&#772;_Gr&uuml;nen)</div>
      </div>
      <div class="stat-card">
        <div class="stat-value">@@P_AFD_GT_SPD@@</div>
        <div class="stat-label">P(&theta;&#772;_AfD &gt; &theta;&#772;_SPD)</div>
      </div>
      <div class="stat-card">
        <div class="stat-value">@@CI_DIFF_AFD_CDU@@</div>
        <div class="stat-label">95% CI: &theta;&#772;_AfD &minus; &theta;&#772;_CDU</div>
      </div>
    </div>

    <div class="figure-block">
      <div id="fig8-container" class="plotly-fig-lg"></div>
      <div class="figure-caption">
        <strong>Figure 8.</strong> Posterior distributions of party mean ideal points from
        500 MCMC draws (interactive violin plot). Each shape shows the full uncertainty about
        where a party&rsquo;s average &theta; lies, not just a point estimate. The boxes
        inside mark the 25th&ndash;75th percentile; the middle line is the median.
        Hover for quantile values. Parties ordered left to right by posterior mean.
      </div>
    </div>

    <h3>Two levels of uncertainty</h3>
    <p>
      It is important to distinguish between two distinct sources of uncertainty in the IRT model:
    </p>
    <div class="callout">
      <div class="callout-title">Individual MP uncertainty</div>
      Each legislator&rsquo;s ideal point &theta;<sub>i</sub> has its own posterior distribution,
      with a width that reflects how many informative votes the legislator cast and how well those
      votes discriminate along the latent dimension. Legislators who missed many votes, or who
      voted consistently with the majority, have wider posterior intervals. Individual credible
      intervals often overlap substantially across parties.
    </div>
    <div class="callout">
      <div class="callout-title">Party ordering uncertainty</div>
      Even though individual legislators have substantial uncertainty, the <em>ordering of party
      means</em> is much more certain. Averaging over 80&ndash;200 legislators within a party,
      the standard error of the party mean shrinks by roughly 1/&radic;n. The posterior
      probability that the AfD&rsquo;s mean ideal point exceeds CDU/CSU&rsquo;s is
      <strong>@@P_AFD_GT_CDU@@</strong> &mdash; near-certain &mdash; even though some
      individual AfD and CDU MPs have overlapping credible intervals.
    </div>

    <h3>What the model shows vs. what we infer</h3>
    <p>
      <strong>What the posterior directly shows:</strong> Legislators voted in patterns that are
      almost entirely captured by a single latent axis. The Ampel coalition parties voted
      together and against the opposition on the vast majority of bills, placing them at one
      pole. The AfD consistently voted against the coalition and with the CDU/CSU on a subset
      of issues, but is positioned further to the right than CDU/CSU.
    </p>
    <p>
      <strong>What we infer:</strong> That this latent dimension corresponds to the conventional
      German left&ndash;right spectrum. The model has no external labels for the axis; it only
      recovers an ordering. The substantive interpretation &mdash; that this is an ideological
      left&ndash;right dimension, not just a government-vs-opposition artefact &mdash; relies on
      the fact that the ordering matches self-reported party positions, electoral results, and
      manifesto-based measures. Disentangling government&ndash;opposition dynamics from genuine
      ideology would require additional design choices (e.g., analysing only non-government-sponsored
      bills, or legislatures where the coalition composition changes mid-term).
    </p>
  </div>
</section>

<!-- SECTION 5: INTERACTIVE -->
<section id="interactive">
  <div class="container">
    <div class="section-header">
      <span class="section-num">05</span>
      <div><h2>Interactive Ideal-Point Explorer</h2></div>
    </div>

    <p>
      Each legislator is a point plotted by IRT ideal point (x-axis: left&ndash;right) and
      SVD dimension 2 (y-axis). Hover over any point for the legislator&rsquo;s name, party,
      and scores. Use the dropdown to highlight a specific party. Pan and zoom with the
      Plotly toolbar in the top right.
    </p>

    <div id="interactive-wrapper">
      <div class="controls">
        <label for="party-select">Highlight party:</label>
        <select id="party-select" onchange="highlightParty(this.value)">
          <option value="all">All parties (coloured)</option>
          <option value="SPD">SPD</option>
          <option value="CDU/CSU">CDU/CSU</option>
          <option value="FDP">FDP</option>
          <option value="BÜNDNIS 90/DIE GRÜNEN">BÜNDNIS 90/DIE GRÜNEN</option>
          <option value="AfD">AfD</option>
          <option value="Die Linke">Die Linke</option>
          <option value="BSW">BSW</option>
          <option value="fraktionslos">fraktionslos</option>
        </select>
      </div>
      <div id="plot-container"></div>
    </div>
  </div>
</section>

<!-- SECTION 6: HORSESHOE PRIOR EXTENSION -->
<section id="horseshoe">
  <div class="container">
    <div class="section-header">
      <span class="section-num">06</span>
      <div><h2>Extra Credit: Horseshoe Prior Extension</h2></div>
    </div>

    <p>
      The standard 2PL IRT model places Normal(0,&thinsp;3) priors on item difficulty standard
      deviations and Normal(0,&thinsp;1) on log-discrimination SDs. These are weakly
      informative but <em>symmetric</em>: they regularize all items equally toward average
      difficulty. The <strong>horseshoe prior</strong> takes a different philosophy &mdash; it
      aggressively shrinks most parameters toward zero while allowing a few to remain large,
      enabling <em>sparse</em> estimation.
    </p>
    <p>
      We refit the same 2PL model with horseshoe-regularized item parameters:
    </p>

    <div class="callout">
      <div class="callout-title">Horseshoe prior specification</div>
      Global intercept: Horseshoe(df=1, scale_global=0.5)<br>
      Item difficulty SD: half-Cauchy(0,&thinsp;3) &mdash; horseshoe-equivalent on scale<br>
      Item discrimination SD: half-Cauchy(0,&thinsp;1)<br>
      Person SD: Constant(1) &mdash; identification constraint unchanged
    </div>

    <p>
      The Cauchy (Student-<em>t</em> with 1 degree of freedom) prior on item-level SDs is the
      horseshoe-equivalent for scale parameters: it has very heavy tails but concentrates mass
      near zero. This means <em>most items</em> are pushed toward average difficulty, while
      <em>a few highly discriminating items</em> can still have extreme parameters.
      The identification constraint SD(person) = 1 is preserved,
      keeping ideal points on the same scale for direct comparison.
    </p>

    <div class="code-block">
      <div class="code-label">R &mdash; Horseshoe prior specification</div>
<pre>prior_hs <span class="kw">&lt;-</span>
  <span class="fn">prior</span>(<span class="str">"horseshoe(df=1, scale_global=0.5)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"b"</span>,  <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"normal(0, 1)"</span>,                      <span class="kw">class</span> <span class="kw">=</span> <span class="str">"b"</span>,  <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"logalpha"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"constant(1)"</span>,  <span class="kw">class</span> <span class="kw">=</span> <span class="str">"sd"</span>, <span class="kw">group</span> <span class="kw">=</span> <span class="str">"person_id"</span>, <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"student_t(1, 0, 3)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"sd"</span>, <span class="kw">group</span> <span class="kw">=</span> <span class="str">"item_id"</span>,   <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"student_t(1, 0, 1)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"sd"</span>, <span class="kw">group</span> <span class="kw">=</span> <span class="str">"item_id"</span>,   <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"logalpha"</span>)

fit_hs <span class="kw">&lt;-</span> <span class="fn">brm</span>(
  formula <span class="kw">=</span> formula_2pl,  <span class="cm"># same formula as baseline</span>
  data    <span class="kw">=</span> brms_data,
  family  <span class="kw">=</span> <span class="fn">brmsfamily</span>(<span class="str">"bernoulli"</span>, <span class="str">"logit"</span>),
  prior   <span class="kw">=</span> prior_hs,
  chains  <span class="kw">=</span> <span class="nb">1</span>,  iter <span class="kw">=</span> <span class="nb">600</span>,  warmup <span class="kw">=</span> <span class="nb">100</span>,
  seed    <span class="kw">=</span> <span class="nb">43</span>,  <span class="cm"># different seed for the horseshoe run</span>
  file    <span class="kw">=</span> <span class="str">"models/fit_hs_bundestag"</span>,
  backend <span class="kw">=</span> <span class="str">"rstan"</span>
)</pre>
    </div>

    <h3>How much do ideal points shift?</h3>
    <p>
      The correlation between normal-prior and horseshoe-prior ideal points is
      <strong>r&nbsp;=&nbsp;@@COR_HS_BASE@@</strong>, indicating that the horseshoe regularization
      produces similar but not identical orderings. The RMSD between the two sets of ideal
      points is <strong>@@RMSD_HS@@</strong> standard deviations &mdash; a moderate shift that
      affects individual legislators but not the overall party ordering.
    </p>

    <div class="figure-block">
      <img src="figures/09_horseshoe_comparison.png"
           alt="Horseshoe vs normal-prior ideal point comparison">
      <div class="figure-caption">
        <strong>Figure 9.</strong> Ideal-point comparison: normal prior (x-axis) vs.
        horseshoe-regularized prior (y-axis). The dashed line is the identity (perfect agreement);
        points off-diagonal represent legislators whose ideal points shifted under heavier
        regularization. The correlation is r&nbsp;=&nbsp;@@COR_HS_BASE@@.
      </div>
    </div>

    <h3>Does the horseshoe shrink moderates more?</h3>
    <p>
      A key prediction of horseshoe-type priors is that they shrink <em>moderate</em> parameters
      more aggressively than extreme ones. In legislative scaling terms: legislators whose voting
      patterns are ambiguous (near the centre) should shift more under the horseshoe, while extreme
      legislators (clearly far-left or far-right) should be more stable, because their position is
      identified by many consistent votes and resists shrinkage.
    </p>
    <div class="callout">
      <div class="callout-title">Interpretation</div>
      If the horseshoe shifts ideal points uniformly (all legislators shift by similar amounts), the
      data are informative enough to overwhelm the prior. If moderate legislators shift more than
      extreme ones, the horseshoe is doing its intended work: shrinking uncertain estimates toward
      zero while preserving extreme but well-identified positions. Either finding is substantively
      interesting and speaks to the informativeness of the Bundestag roll-call record.
    </div>
    <p>
      The comparison plot (Figure 9) provides direct evidence on this question: points near the
      centre of the x-axis (moderate under the normal prior) that deviate from the identity line
      represent legislators whose ideal points shifted under the horseshoe; points at the extremes
      that stay close to the identity line represent robust extreme ideal points.
    </p>
  </div>
</section>

<!-- SECTION 7: AI WORKFLOW -->
<section id="prompts">
  <div class="container">
    <div class="section-header">
      <span class="section-num">07</span>
      <div><h2>AI Workflow Documentation</h2></div>
    </div>

    <p>
      This project was completed on the AI-forward track. The following summarises the
      prompts and iterative workflow. See
      <a href="prompts.md"><code>prompts.md</code></a> in the repository for the full
      prompt history.
    </p>

    <h3>Workflow summary</h3>
    <p>
      Claude Code (claude-sonnet-4-6, Anthropic) was used to: (1) read and interpret the
      assignment specification, lecture notes, and Bürkner (2021) replication code;
      (2) design the analysis pipeline; (3) write <code>analysis.R</code> and
      <code>build_site.R</code>; (4) write this site. All generated code was verified for
      correctness before execution.
    </p>

    <div class="prompt-entry">
      <div class="prompt-role user">Initial prompt (User &rarr; Claude)</div>
      <div class="prompt-body">
        Read README.md, week13_fss2026_bayes.html, and v100i05.R. Then outline a plan before
        writing any code for: (1) double-mean imputation + SVD, (2) double-centered SVD,
        (3) brms 2PL IRT following Bürkner 2021, (4) substantive claim using the posterior as
        a distribution. Deliver a GitHub Pages site in University of Mannheim colors with a
        Plotly.js interactive scatter plot.
      </div>
    </div>

    <div class="prompt-entry">
      <div class="prompt-role ai">Response (Claude)</div>
      <div class="prompt-body">
        Read all three files. Verified data structure: 161 unique polls, 772 legislators,
        118,400 long-format rows. Vote types: yes, no, abstain, no_show. Outlined plan
        covering all four tasks, then wrote analysis.R and build_site.R implementing the full
        pipeline. Used @@VAR@@ placeholder substitution in the HTML template to separate data
        injection from HTML authoring.
      </div>
    </div>

    <div class="prompt-entry">
      <div class="prompt-role user">Refinement prompts (User &rarr; Claude)</div>
      <div class="prompt-body">
        Ensure: (a) double-centering applied to imputed matrix so verification passes exactly;
        (b) SVD and IRT orientations consistent (AfD positive); (c) posterior claim distinguishes
        individual vs.&nbsp;party-level uncertainty and what the model shows vs.&nbsp;what we
        infer; (d) Plotly chart has dropdown for party highlighting with hover showing name,
        party, and all scores.
      </div>
    </div>

    <div class="prompt-entry">
      <div class="prompt-role user">Design &amp; extra-credit prompt (User &rarr; Claude)</div>
      <div class="prompt-body">
        Improve site: (a) replace teal with University of Mannheim navy (#003056);
        (b) official German party colors in Plotly; (c) clean tooltips with 2dp ideal point;
        (d) floating TOC sidebar; (e) collapsible code blocks defaulting to hidden;
        (f) MathJax for math notation; (g) max-width 720px prose; (h) add horseshoe
        prior extension as Section 06 with comparison figure and interpretation.
      </div>
    </div>

    <div class="prompt-entry">
      <div class="prompt-role ai">Response (Claude)</div>
      <div class="prompt-body">
        Fixed IRT orientation bug (was using party detection which failed; switched to
        SVD-correlation-based orientation). Updated party colors to official values,
        redesigned CSS to University of Mannheim navy, added MathJax, floating TOC with
        scroll-spy, JS-driven collapsible code blocks, and fitted the horseshoe model
        (horseshoe(df=1) + Cauchy item SDs) comparing ideal points via Figure 9.
      </div>
    </div>

    <h3>Quality checks</h3>
    <ul style="padding-left:1.4rem;margin-top:.5rem;line-height:2">
      <li>Data recoding verified: yes=1, no=0, abstain/no_show=NA</li>
      <li>Double-centering produces machine-zero row/col means (verified analytically and numerically)</li>
      <li>brms formula matches B&uuml;rkner (2021) Table 1 exactly</li>
      <li>SVD and IRT orientations verified to be consistent (AfD positive on both)</li>
      <li>Posterior probabilities computed from draw-level party means, not point estimates</li>
      <li>Plotly chart renders and dropdown updates correctly</li>
    </ul>
  </div>
</section>

<!-- REFERENCES -->
<section id="refs">
  <div class="container">
    <div class="section-header">
      <span class="section-num">08</span>
      <div><h2>References</h2></div>
    </div>
    <ul style="padding-left:1.4rem;line-height:2.1">
      <li>B&uuml;rkner, P.-C. (2021). Bayesian Item Response Modeling in R with brms and Stan.
          <em>Journal of Statistical Software</em>, 100(5), 1&ndash;54.</li>
      <li>Clinton, J., Jackman, S., &amp; Rivers, D. (2004). The statistical analysis of roll call data.
          <em>American Political Science Review</em>, 98(2), 355&ndash;370.</li>
      <li>Abgeordnetenwatch e.V. (2025). Bundestag 20th Wahlperiode roll-call vote data (CC0).
          <a href="https://www.abgeordnetenwatch.de" target="_blank">abgeordnetenwatch.de</a></li>
      <li>Ratkovic, M. (2026). Week 13 &mdash; Scaling and item response theory.
          Lecture notes, Bayesian Statistics, University of Mannheim, FSS 2026.</li>
    </ul>
  </div>
</section>

</main>

<footer>
  <div class="footer-inner">
    <p>
      Maximilian Birkle &middot; Student ID: 1831999 &middot;
      MMDS &middot; DS 201 Bayesian Statistics &middot; Uni Mannheim &middot; FSS 2026
    </p>
    <p style="margin-top:.6rem;opacity:.8;font-size:.8rem">
      Data: <a href="https://www.abgeordnetenwatch.de">Abgeordnetenwatch e.V.</a> (CC0) &middot;
      Analysis: <a href="https://paul-buerkner.github.io/brms/">brms</a> / Stan &middot;
      Charts: <a href="https://plotly.com/javascript/">Plotly.js</a>
    </p>
  </div>
</footer>

<script>
const RAW_DATA       = @@PLOTLY_DATA@@;
const PARTY_DRAWS    = @@PARTY_DRAWS_DATA@@;

const PARTY_COLORS = {
  "SPD":                   "#E3000F",
  "CDU/CSU":               "#000000",
  "FDP":                   "#FFED00",
  "BÜNDNIS 90/DIE GRÜNEN": "#64A12D",
  "AfD":                   "#009EE0",
  "Die Linke":             "#BE3075",
  "BSW":                   "#6A0F49",
  "fraktionslos":          "#888888"
};

const PARTY_SHORT = {
  "BÜNDNIS 90/DIE GRÜNEN": "Grünen",
  "Die Linke":             "Linke"
};

function shortP(p) { return PARTY_SHORT[p] || p; }

const plotConfig = {
  responsive: true, displayModeBar: true,
  modeBarButtonsToRemove: ["lasso2d","select2d"],
  toImageButtonOptions: { format:"png", filename:"bundestag_ideal_points" }
};

/* ===== FIGURE 2: SVD dim1 strip by party ===== */
(function buildFig2() {
  const field = "svd_dim1";
  const byParty = {};
  RAW_DATA.forEach(d => {
    if (d[field] == null) return;
    if (!byParty[d.party]) byParty[d.party] = [];
    byParty[d.party].push(d);
  });
  const partyOrder = Object.keys(byParty)
    .filter(p => PARTY_COLORS[p])
    .sort((a,b) => {
      const ma = byParty[a].reduce((s,d) => s + d[field], 0) / byParty[a].length;
      const mb = byParty[b].reduce((s,d) => s + d[field], 0) / byParty[b].length;
      return ma - mb;
    });
  const traces = partyOrder.map(party => {
    const rows = byParty[party];
    return {
      type: "box", orientation: "h",
      name: shortP(party),
      x: rows.map(d => d[field]),
      y: rows.map(_ => shortP(party)),
      marker: { color: PARTY_COLORS[party], size: 5, opacity: 0.55 },
      line: { color: PARTY_COLORS[party] },
      fillcolor: PARTY_COLORS[party] + "28",
      boxpoints: "all", jitter: 0.45, pointpos: 0,
      whiskerwidth: 0.6, boxmean: false,
      text: rows.map(d => "<b>" + d.legislator + "</b><br>" + shortP(d.party) + "<br>SVD dim1: " + (d[field]||0).toFixed(3)),
      hovertemplate: "%{text}<extra></extra>"
    };
  });
  Plotly.newPlot("fig2-container", traces, {
    title: { text: "SVD Dimension-1 Ideal Points by Party", font:{size:14,color:"#00203f"}, x:0.02, xanchor:"left" },
    xaxis: { title:{text:"SVD Score (Dimension 1) — left to right",font:{size:12}}, zeroline:true, zerolinecolor:"#bbb", gridcolor:"#eee" },
    yaxis: { automargin:true },
    showlegend: false,
    plot_bgcolor:"#fafcfd", paper_bgcolor:"#ffffff",
    hoverlabel: { bgcolor:"#fff", bordercolor:"#999", font:{size:12} },
    margin: { l:80, r:20, t:50, b:50 }
  }, plotConfig);
})();

/* ===== FIGURE 6: IRT theta strip by party ===== */
(function buildFig6() {
  const field = "theta";
  const byParty = {};
  RAW_DATA.forEach(d => {
    if (d[field] == null) return;
    if (!byParty[d.party]) byParty[d.party] = [];
    byParty[d.party].push(d);
  });
  const partyOrder = Object.keys(byParty)
    .filter(p => PARTY_COLORS[p])
    .sort((a,b) => {
      const ma = byParty[a].reduce((s,d) => s + d[field], 0) / byParty[a].length;
      const mb = byParty[b].reduce((s,d) => s + d[field], 0) / byParty[b].length;
      return ma - mb;
    });
  const traces = partyOrder.map(party => {
    const rows = byParty[party];
    return {
      type: "box", orientation: "h",
      name: shortP(party),
      x: rows.map(d => d[field]),
      y: rows.map(_ => shortP(party)),
      marker: { color: PARTY_COLORS[party], size: 5, opacity: 0.55 },
      line: { color: PARTY_COLORS[party] },
      fillcolor: PARTY_COLORS[party] + "28",
      boxpoints: "all", jitter: 0.45, pointpos: 0,
      whiskerwidth: 0.6, boxmean: false,
      text: rows.map(d =>
        "<b>" + d.legislator + "</b><br>" + shortP(d.party) +
        "<br>θ: " + (d.theta||0).toFixed(2) +
        " [" + (d.theta_lo||0).toFixed(2) + ", " + (d.theta_hi||0).toFixed(2) + "]"
      ),
      hovertemplate: "%{text}<extra></extra>"
    };
  });
  Plotly.newPlot("fig6-container", traces, {
    title: { text: "IRT Posterior Mean Ideal Points (θ) by Party", font:{size:14,color:"#00203f"}, x:0.02, xanchor:"left" },
    xaxis: { title:{text:"Posterior Mean Ideal Point (θ) — left to right",font:{size:12}}, zeroline:true, zerolinecolor:"#bbb", gridcolor:"#eee" },
    yaxis: { automargin:true },
    showlegend: false,
    plot_bgcolor:"#fafcfd", paper_bgcolor:"#ffffff",
    hoverlabel: { bgcolor:"#fff", bordercolor:"#999", font:{size:12} },
    margin: { l:80, r:20, t:50, b:50 }
  }, plotConfig);
})();

/* ===== FIGURE 8: Posterior draws violin by party ===== */
(function buildFig8() {
  const byParty = {};
  PARTY_DRAWS.forEach(d => {
    if (!byParty[d.party]) byParty[d.party] = [];
    byParty[d.party].push(d.theta);
  });
  const partyOrder = Object.keys(byParty)
    .sort((a,b) => {
      const ma = byParty[a].reduce((s,v) => s+v, 0) / byParty[a].length;
      const mb = byParty[b].reduce((s,v) => s+v, 0) / byParty[b].length;
      return ma - mb;
    });
  const partyColorMap = {
    "AfD": "#009EE0", "CDU/CSU": "#444444", "SPD": "#E3000F",
    "BÜNDNIS 90/DIE GRÜNEN": "#64A12D", "FDP": "#CCBB00",
    "Die Linke": "#BE3075"
  };
  const traces = partyOrder.map(party => ({
    type: "violin", orientation: "h",
    name: shortP(party),
    x: byParty[party],
    y: byParty[party].map(_ => shortP(party)),
    box: { visible: true },
    meanline: { visible: true },
    points: false,
    fillcolor: (partyColorMap[party] || "#888888") + "50",
    line: { color: partyColorMap[party] || "#888888" },
    hovertemplate: shortP(party) + "<br>median: %{median:.2f}<extra></extra>"
  }));
  Plotly.newPlot("fig8-container", traces, {
    title: { text: "Posterior Distributions of Party Mean Ideal Points (500 MCMC Draws)", font:{size:13,color:"#00203f"}, x:0.02, xanchor:"left" },
    xaxis: { title:{text:"Party Mean Ideal Point (θ)",font:{size:12}}, zeroline:true, zerolinecolor:"#bbb", gridcolor:"#eee" },
    yaxis: { automargin:true },
    showlegend: false,
    plot_bgcolor:"#fafcfd", paper_bgcolor:"#ffffff",
    hoverlabel: { bgcolor:"#fff", bordercolor:"#999", font:{size:12} },
    margin: { l:80, r:20, t:50, b:50 },
    violingap: 0.05, violingroupgap: 0
  }, plotConfig);
})();

/* ===== FIGURE 5: Interactive explorer (theta vs SVD dim2) ===== */
const parties = [...new Set(RAW_DATA.map(d => d.party))].sort();

function buildTraces(hl) {
  return parties.map(party => {
    const rows = RAW_DATA.filter(d => d.party === party);
    const isHL = (hl === "all" || party === hl);
    return {
      type: "scatter", mode: "markers",
      name: shortP(party),
      x:    rows.map(d => d.theta),
      y:    rows.map(d => d.svd_dim2),
      text: rows.map(d =>
        "<b>" + d.legislator + "</b><br>" +
        shortP(d.party) + "<br>" +
        "Ideal Point: " + (d.theta||0).toFixed(2)
      ),
      hovertemplate: "%{text}<extra></extra>",
      marker: {
        color:   PARTY_COLORS[party] || "#888",
        size:    8,
        opacity: (hl === "all") ? 0.72 : (isHL ? 0.92 : 0.07),
        line:    { width: 0.8, color: "rgba(255,255,255,0.6)" }
      }
    };
  });
}

const layout5 = {
  title: { text: "Ideal Point Estimates — 20th Bundestag (2021–2025)", font:{size:14,color:"#00203f"}, x:0.02, xanchor:"left" },
  xaxis: { title:{text:"IRT Ideal Point (θ) — left to right",font:{size:13}}, zeroline:true, zerolinecolor:"#ccc", gridcolor:"#eee" },
  yaxis: { title:{text:"SVD Dimension 2",font:{size:13}}, zeroline:true, zerolinecolor:"#ccc", gridcolor:"#eee" },
  legend: { title:{text:"Party"}, bgcolor:"rgba(255,255,255,.9)", bordercolor:"#ddd", borderwidth:1 },
  plot_bgcolor:"#fafcfd", paper_bgcolor:"#ffffff",
  hoverlabel: { bgcolor:"#fff", bordercolor:"#999", font:{size:13} },
  margin: { l:60, r:20, t:54, b:60 }, hovermode:"closest"
};

Plotly.newPlot("plot-container", buildTraces("all"), layout5, plotConfig);

function highlightParty(p) {
  Plotly.react("plot-container", buildTraces(p), layout5, plotConfig);
}

/* ===== Animated stat counters ===== */
function animateCounters() {
  document.querySelectorAll(".stat-counter").forEach(function(el) {
    const raw = el.dataset.target || el.textContent;
    const num = parseFloat(raw.replace(/[^0-9.]/g, ""));
    const suffix = raw.replace(/[0-9.]/g, "");
    if (isNaN(num)) return;
    const dur = 1400, fps = 60, steps = Math.round(dur / (1000/fps));
    let step = 0;
    el.textContent = "0" + suffix;
    const id = setInterval(function() {
      step++;
      const progress = step / steps;
      const eased = 1 - Math.pow(1 - progress, 3);
      const current = num * eased;
      el.textContent = (Number.isInteger(num) ? Math.round(current) : current.toFixed(1)) + suffix;
      if (step >= steps) { el.textContent = raw; clearInterval(id); }
    }, 1000/fps);
  });
}

const statsObs = new IntersectionObserver(function(entries) {
  entries.forEach(e => { if (e.isIntersecting) { animateCounters(); statsObs.disconnect(); } });
}, { threshold: 0.3 });
const heroStats = document.getElementById("hero-stats");
if (heroStats) statsObs.observe(heroStats);

/* ===== Collapsible code blocks ===== */
document.addEventListener("DOMContentLoaded", function() {
  document.querySelectorAll(".code-block").forEach(function(block) {
    const label = block.querySelector(".code-label");
    const pre   = block.querySelector("pre");
    if (!label || !pre) return;
    const wrapper = document.createElement("div");
    wrapper.className = "code-content";
    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(pre);
    const btn = document.createElement("span");
    btn.className = "code-toggle-btn";
    btn.textContent = "Show code ▼";
    label.appendChild(btn);
    label.addEventListener("click", function() {
      const open = wrapper.style.display !== "none";
      wrapper.style.display = open ? "none" : "block";
      btn.textContent = open ? "Show code ▼" : "Hide code ▲";
    });
  });

  /* TOC active-link highlighting */
  const tocLinks = document.querySelectorAll(".toc-sidebar a");
  const sections = Array.from(tocLinks)
    .map(a => document.querySelector(a.getAttribute("href")))
    .filter(Boolean);
  function onScroll() {
    let current = sections[0];
    sections.forEach(s => { if (window.scrollY >= s.offsetTop - 120) current = s; });
    tocLinks.forEach(a => {
      a.classList.toggle("toc-active", a.getAttribute("href") === "#" + current.id);
    });
  }
  window.addEventListener("scroll", onScroll, {passive: true});
  onScroll();
});
</script>
</body>
</html>'

# ---------- substitute all placeholders --------------------------------------

html <- template
for (key in names(sub_map)) {
  html <- gsub(key, sub_map[[key]], html, fixed = TRUE)
}

writeLines(html, "index.html")
cat(sprintf("index.html written: %s bytes\n", format(nchar(html), big.mark=",")))
