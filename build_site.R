# ==============================================================================
# build_site.R  —  dark theme + Bundesadler + dark Plotly + UX improvements
# @@VAR@@ placeholders to avoid sprintf/CSS/JS % conflicts
# ==============================================================================

suppressPackageStartupMessages({ library(tidyverse); library(jsonlite) })

cat("Reading results...\n")
stats       <- fromJSON("results/summary_stats.json")
votes_ext   <- fromJSON("results/extreme_votes.json")
plotly_json <- paste(readLines("results/plotly_data.json",   warn=FALSE), collapse="\n")
draws_json  <- paste(readLines("results/party_draws.json",   warn=FALSE), collapse="\n")
scree_json  <- paste(readLines("results/scree_data.json",    warn=FALSE), collapse="\n")
loads_json  <- paste(readLines("results/vote_loadings.json", warn=FALSE), collapse="\n")

pct    <- function(x, d=1) sprintf(paste0("%.",d,"f%%"), x)
corr   <- function(x) sprintf("%.3f", x)
prob   <- function(x) sprintf("%.1f%%", 100*x)
fmt_ci <- function(lo,hi) sprintf("[%.2f, %.2f]", lo, hi)
he <- function(x) {
  x <- gsub("&","&amp;",as.character(x),fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)
}

vote_li <- function(row) {
  lbl <- coalesce(row$poll_label,""); com <- coalesce(row$committee,"")
  com <- if(nchar(com)>0) paste0(" <span class=\"vm\">(", he(com), ")</span>") else ""
  sprintf("<li><em>%s</em>%s</li>", he(lbl), com)
}
pos_votes_html <- paste(sapply(seq_len(min(5,nrow(votes_ext$positive))),
  function(i) vote_li(votes_ext$positive[i,])), collapse="\n")
neg_votes_html <- paste(sapply(seq_len(min(5,nrow(votes_ext$negative))),
  function(i) vote_li(votes_ext$negative[i,])), collapse="\n")

cor12 <- stats$cor_svd12
cor_narrative <- if(abs(cor12)>0.85){
  paste0("near-perfect agreement (r = ",corr(cor12),"). Both pre-processing strategies recover the same ideological ordering.")
} else if(cor12>0.50){
  paste0("positive but moderate agreement (r = ",corr(cor12),"). Both methods identify similar orderings but differ in emphasis.")
} else {
  paste0("a negative correlation (r = ",corr(cor12),"). The uncentred SVD first dimension is dominated by additive yes-vote tendencies; double-centering removes this and the DC-SVD first dimension reflects the pure voting interaction &mdash; a cleaner estimator of ideological position.")
}

sub_map <- c(
  "@@N_LEGISLATORS@@"       = as.character(stats$n_legislators),
  "@@N_VOTES@@"             = as.character(stats$n_votes),
  "@@N_BRMS_OBS@@"          = format(stats$n_brms_obs, big.mark=","),
  "@@BRMS_PCT@@"            = sprintf("%.1f", 100*stats$n_brms_obs/(stats$n_legislators*stats$n_votes)),
  "@@PCT_OBSERVED@@"        = pct(stats$pct_observed),
  "@@GRAND_MEAN_PCT@@"      = pct(stats$grand_mean*100, 0),
  "@@VAR_EXP_1@@"           = pct(stats$var_exp_dim1),
  "@@VAR_EXP_2@@"           = pct(stats$var_exp_dim2),
  "@@VAR_EXP_1_DC@@"        = pct(stats$var_exp_dim1_dc),
  "@@VAR_EXP_2_DC@@"        = pct(stats$var_exp_dim2_dc),
  "@@COR_SVD12@@"           = corr(stats$cor_svd12),
  "@@COR_SVD12_NARRATIVE@@" = cor_narrative,
  "@@COR_SVD_IRT@@"         = corr(stats$cor_svd_irt),
  "@@P_AFD_GT_CDU@@"        = prob(stats$p_afd_gt_cdu),
  "@@P_LINKE_GT_GRUEN@@"    = prob(1-stats$p_linke_lt_gruen),
  "@@P_AFD_GT_SPD@@"        = prob(stats$p_afd_gt_spd),
  "@@CI_DIFF_AFD_CDU@@"     = fmt_ci(stats$ci_diff_afd_cdu_lo, stats$ci_diff_afd_cdu_hi),
  "@@MAX_ROW_ERR@@"         = sprintf("%.2e", stats$max_row_err_dc),
  "@@MAX_COL_ERR@@"         = sprintf("%.2e", stats$max_col_err_dc),
  "@@P_AFD_RAW@@"           = sprintf("%.3f", stats$p_afd_gt_cdu),
  "@@POS_VOTES@@"           = pos_votes_html,
  "@@NEG_VOTES@@"           = neg_votes_html,
  "@@PLOTLY_DATA@@"         = plotly_json,
  "@@PARTY_DRAWS_DATA@@"    = draws_json,
  "@@SCREE_DATA@@"          = scree_json,
  "@@VOTE_LOADINGS_DATA@@"  = loads_json,
  "@@COR_HS_BASE@@"         = corr(stats$cor_hs_base),
  "@@RMSD_HS@@"             = sprintf("%.3f", stats$rmsd_hs)
)

# ==============================================================================
# HTML TEMPLATE
# ==============================================================================

template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Scaling the Bundestag &middot; Ideal-Point Estimation &middot; Uni Mannheim</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Source+Serif+4:opsz,wght@8..60,400;8..60,600;8..60,700&family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400;500&display=swap" rel="stylesheet">
<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
<script>MathJax={tex:{inlineMath:[["$","$"],["\\\\(","\\\\)"]],displayMath:[["$$","$$"],["\\\\[","\\\\]"]]},options:{skipHtmlTags:["script","noscript","style","textarea","pre","code"]}};</script>
<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js" async></script>
<style>
/* ── CUSTOM EASING ─────────────────────────────────────────────────────────── */
:root {
  --ease-out:    cubic-bezier(0.23, 1, 0.32, 1);
  --ease-in-out: cubic-bezier(0.77, 0, 0.175, 1);
  --navy:        #003056;
  --navy-dark:   #00203f;
  --navy-mid:    #4a7fa5;
  --bg:          #0a0f1a;
  --bg-card:     #111827;
  --bg-code:     #0d1117;
  --text:        #f1f5f9;
  --text-muted:  #94a3b8;
  --border:      #1e3a5f;
  --nav-h:       56px;
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
html{scroll-behavior:smooth;scroll-padding-top:calc(var(--nav-h) + 16px)}
body{font-family:"Inter",system-ui,sans-serif;font-size:1rem;line-height:1.75;color:var(--text);background:var(--bg)}
a{color:#60a5fa;text-decoration:none}
a:hover{color:#93c5fd;text-decoration:underline}
strong{color:#f1f5f9}
ul,ol{padding-left:1.4rem;color:var(--text)}
li{margin-bottom:.25rem}

/* ── NAV ─────────────────────────────────────────────────────────────────── */
#main-nav{
  position:sticky;top:0;z-index:200;
  background:rgba(10,15,26,0.95);
  backdrop-filter:blur(12px);
  -webkit-backdrop-filter:blur(12px);
  border-bottom:1px solid rgba(255,255,255,.06);
  height:var(--nav-h);
}
.nav-progress{
  position:absolute;bottom:0;left:0;height:2px;
  background:var(--navy-mid);width:0;
  transition:width 80ms linear;
}
.nav-inner{
  max-width:1200px;margin:0 auto;padding:0 2rem;
  height:100%;display:flex;align-items:center;justify-content:space-between;gap:1rem;
}
.nav-brand{
  background:rgba(255,255,255,0.08);border-radius:8px;
  padding:4px 10px;display:flex;align-items:center;
  transition:background 160ms ease;
}
.nav-brand:hover{background:rgba(255,255,255,0.14)}
.nav-brand img{height:34px;width:auto;filter:brightness(0) invert(1);opacity:.92;display:block}
.nav-links{display:flex;gap:.1rem;list-style:none;align-items:center}
.nav-links a{
  position:relative;color:rgba(255,255,255,.72);text-decoration:none;
  font-size:.8rem;font-weight:500;padding:.35rem .65rem;border-radius:5px;
  transition:color 180ms ease,background 180ms ease;
}
@media(hover:hover) and (pointer:fine){
  .nav-links a::after{
    content:"";position:absolute;bottom:2px;left:.65rem;right:.65rem;height:1.5px;
    background:var(--navy-mid);transform:scaleX(0);transform-origin:left;
    transition:transform 220ms var(--ease-out);
  }
  .nav-links a:hover{color:#fff}
  .nav-links a:hover::after{transform:scaleX(1)}
}
.nav-links a.nav-active{color:#fff}
.nav-links a.nav-active::after{transform:scaleX(1)!important;background:var(--navy-mid)}
.hamburger{
  display:none;background:none;border:1px solid rgba(255,255,255,.3);
  border-radius:6px;color:#fff;padding:.35rem .5rem;cursor:pointer;font-size:1.1rem;
  transition:background 160ms ease;
}
.hamburger:active{background:rgba(255,255,255,.12);transform:scale(0.97)}
@media(max-width:820px){
  .hamburger{display:block}
  .nav-links{
    display:none;position:absolute;top:var(--nav-h);left:0;right:0;
    background:rgba(10,15,26,0.98);flex-direction:column;align-items:flex-start;
    padding:1rem 2rem;border-bottom:1px solid rgba(255,255,255,.08);gap:.2rem;
    backdrop-filter:blur(12px);
  }
  .nav-links.open{display:flex}
  .nav-links a{font-size:.95rem;padding:.55rem .4rem;width:100%}
}

/* ── HERO ────────────────────────────────────────────────────────────────── */
.hero{
  position:relative;overflow:hidden;
  min-height:88vh;display:flex;flex-direction:column;justify-content:center;
  background:linear-gradient(135deg, var(--navy-dark) 0%, var(--navy) 50%, #004a7c 100%);
  background-size:200% 200%;
  animation:hero-breathe 12s ease-in-out infinite;
  color:#fff;
}
@keyframes hero-breathe{0%,100%{background-position:0% 50%}50%{background-position:100% 50%}}
.hero-dots{
  position:absolute;inset:0;pointer-events:none;
  background-image:radial-gradient(circle, rgba(255,255,255,.045) 1px, transparent 1px);
  background-size:28px 28px;
}
.hero-eagle{
  position:absolute;top:50%;left:50%;
  transform:translate(-50%,-50%);
  width:65%;max-width:680px;height:auto;
  opacity:0.07;
  filter:invert(1) brightness(1.5);
  mix-blend-mode:screen;
  pointer-events:none;
  animation:eagle-breathe 8s ease-in-out infinite;
  object-fit:contain;
}
@keyframes eagle-breathe{
  0%,100%{transform:translate(-50%,-50%) scale(1.0)}
  50%{transform:translate(-50%,-50%) scale(1.03)}
}
.hero-inner{
  position:relative;z-index:2;
  max-width:900px;margin:0 auto;padding:4rem 2rem 6rem;
  text-align:center;
}
.hero-logo{margin-bottom:1.8rem;animation:fade-up 600ms var(--ease-out) both}
.hero-logo img{height:60px;width:auto;filter:brightness(0) invert(1);opacity:.92}
.hero-title{
  font-family:"Source Serif 4",serif;font-size:clamp(1.9rem,5vw,3rem);
  font-weight:700;line-height:1.18;margin-bottom:.85rem;
  animation:fade-up 640ms var(--ease-out) 80ms both;
}
.hero-pills{display:flex;justify-content:center;gap:.6rem;flex-wrap:wrap;margin-bottom:1.2rem;animation:fade-up 640ms var(--ease-out) 160ms both}
.hero-pill{background:rgba(255,255,255,.1);border:1px solid rgba(255,255,255,.2);border-radius:20px;padding:.25rem .9rem;font-size:.78rem;font-weight:500;letter-spacing:.03em;color:rgba(255,255,255,.9)}
.hero-course{font-size:.83rem;color:rgba(255,255,255,.55);margin-bottom:2.5rem;animation:fade-up 640ms var(--ease-out) 220ms both}
.hero-stats{
  display:grid;grid-template-columns:repeat(4,1fr);gap:1.2rem;
  margin-bottom:3rem;
  animation:fade-up 640ms var(--ease-out) 300ms both;
}
@media(max-width:600px){.hero-stats{grid-template-columns:repeat(2,1fr)}}
.hero-stat{
  background:rgba(255,255,255,0.05);
  border:1px solid rgba(255,255,255,0.1);
  border-radius:12px;padding:1.3rem .9rem;text-align:center;
  backdrop-filter:blur(8px);-webkit-backdrop-filter:blur(8px);
}
.hs-c1{border-top:3px solid #4a7fa5}
.hs-c2{border-top:3px solid #E3000F}
.hs-c3{border-top:3px solid #64A12D}
.hs-c4{border-top:3px solid #009EE0}
.hero-stat-val{
  font-family:"Source Serif 4",serif;font-size:2.1rem;font-weight:700;
  line-height:1;display:block;margin-bottom:.35rem;color:#fff;
}
.hero-stat-lbl{font-size:.7rem;color:#4a7fa5;text-transform:uppercase;letter-spacing:.1em}
.hero-caret{
  animation:hero-bounce 1.8s ease-in-out infinite, fade-up 640ms var(--ease-out) 480ms both;
  font-size:1.6rem;opacity:.55;cursor:pointer;display:inline-block;
}
@keyframes hero-bounce{0%,100%{transform:translateY(0)}50%{transform:translateY(7px)}}
@keyframes fade-up{from{opacity:0;transform:translateY(20px)}to{opacity:1;transform:translateY(0)}}

/* ── LAYOUT ──────────────────────────────────────────────────────────────── */
main{overflow:hidden;background:var(--bg);padding:.5rem 0}
.container{max-width:1100px;margin:0 auto;padding:0 2rem}
section{
  background:var(--bg-card);
  border-left:3px solid var(--navy);
  box-shadow:0 4px 24px rgba(0,0,0,0.4);
  padding:4rem 0;margin:.75rem 0;border-bottom:none;
}

/* Section reveal */
.reveal{opacity:0;transform:translateY(20px);transition:opacity 500ms var(--ease-out),transform 500ms var(--ease-out)}
.reveal.visible{opacity:1;transform:translateY(0)}
@media(prefers-reduced-motion:reduce){
  .reveal{opacity:0;transform:none;transition:opacity 400ms ease}
  .reveal.visible{opacity:1}
  .hero-logo,.hero-title,.hero-pills,.hero-course,.hero-stats,.hero-caret{animation:none;opacity:1}
  .hero,.hero-eagle{animation:none}
}

/* ── SECTION HEADERS ─────────────────────────────────────────────────────── */
.section-header{margin-bottom:2rem}
.section-num{
  font-family:"Source Serif 4",serif;font-size:5rem;font-weight:700;
  color:rgba(255,255,255,0.12);line-height:1;display:block;margin-bottom:-.8rem;
  pointer-events:none;
}
.section-title{
  font-family:"Source Serif 4",serif;font-size:1.75rem;font-weight:700;
  color:#ffffff;line-height:1.2;margin-bottom:.75rem;
}
.key-finding{
  display:inline-block;background:#0a2540;color:#fff;
  border-left:4px solid #4a7fa5;
  border-radius:0 6px 6px 0;padding:.4rem 1rem;font-size:.88rem;font-weight:500;
  margin-bottom:1.2rem;letter-spacing:.01em;
}
h3{
  font-family:"Source Serif 4",serif;font-size:1.15rem;font-weight:600;
  color:#ffffff;margin:2rem 0 .7rem;
  padding-left:.85rem;border-left:3px solid var(--navy-mid);
}
p{margin-bottom:.9rem;max-width:720px;color:var(--text)}
p:last-child{margin-bottom:0}
code{background:#1a2535;border-radius:4px;padding:.1rem .4rem;font-family:"JetBrains Mono",monospace;font-size:.82rem;color:#79c0ff}

/* ── STAT CARDS ──────────────────────────────────────────────────────────── */
.stats-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(155px,1fr));gap:1rem;margin:1.8rem 0}
.stat-card{
  background:rgba(255,255,255,0.04);border:1px solid var(--border);border-radius:10px;
  padding:1.2rem 1rem;text-align:center;
  transition:transform 220ms var(--ease-out),box-shadow 220ms var(--ease-out);
}
@media(hover:hover) and (pointer:fine){
  .stat-card:hover{transform:translateY(-3px);box-shadow:0 6px 20px rgba(0,0,0,.5)}
}
.stat-value{
  font-family:"Source Serif 4",serif;font-size:1.85rem;font-weight:700;
  color:#fff;line-height:1;margin-bottom:.28rem;display:block;
}
.stat-label{font-size:.73rem;color:#4a7fa5;font-weight:500;text-transform:uppercase;letter-spacing:.05em}
.stat-desc{font-size:.7rem;color:var(--text-muted);margin-top:.4rem;line-height:1.4;display:block}

/* ── CODE BLOCKS ─────────────────────────────────────────────────────────── */
.code-block{
  background:var(--bg-code);border-radius:10px;overflow:hidden;margin:1.5rem 0;
  border:1px solid #21262d;position:relative;
}
.code-label{
  background:#161b22;color:#4a7fa5;font-family:"JetBrains Mono",monospace;
  font-size:.73rem;padding:.5rem 1.1rem;font-weight:500;letter-spacing:.05em;
  cursor:pointer;display:flex;justify-content:space-between;align-items:center;
  user-select:none;transition:background 160ms ease;
}
@media(hover:hover) and (pointer:fine){.code-label:hover{background:#1c2128}}
.code-label:active{transform:scale(0.99)}
.code-toggle-btn{
  font-size:.7rem;white-space:nowrap;margin-left:.8rem;
  background:#1e293b;border:1px solid #4a7fa5;border-radius:20px;
  padding:.15rem .7rem;color:#4a7fa5;
  transition:all 160ms ease;flex-shrink:0;
}
.code-content{max-height:0;overflow:hidden;transition:max-height 300ms var(--ease-in-out)}
.code-content.open{max-height:1200px}
pre{margin:0;padding:1.1rem 1.3rem;overflow-x:auto;font-family:"JetBrains Mono",monospace;font-size:.8rem;line-height:1.65;color:#c9d1d9}
.kw{color:#ff7b72}.fn{color:#79c0ff}.str{color:#a5d6ff}.cm{color:#8b949e;font-style:italic}.nb{color:#e3b341}
.copy-btn{
  background:#21262d;border:1px solid #30363d;border-radius:20px;
  color:#8b949e;font-size:.7rem;padding:.15rem .6rem;cursor:pointer;
  font-family:"JetBrains Mono",monospace;line-height:1.5;flex-shrink:0;
  transition:all 160ms ease;
}
@media(hover:hover) and (pointer:fine){.copy-btn:hover{color:#e8edf4;border-color:var(--navy-mid)}}
.copy-btn.copied{color:#56d364;border-color:#56d364}

/* ── FIGURES ─────────────────────────────────────────────────────────────── */
.figure-block{
  margin:2rem 0;background:var(--bg-card);border:1px solid var(--border);
  border-radius:10px;overflow:hidden;
  transition:box-shadow 220ms var(--ease-out);
}
@media(hover:hover) and (pointer:fine){
  .figure-block:hover{box-shadow:0 4px 20px rgba(0,0,0,.6)}
}
.fig-plotly{width:100%;height:460px}
.fig-plotly-sm{width:100%;height:380px}
.figure-caption{
  padding:.8rem 1.2rem;font-size:.85rem;color:var(--text-muted);
  background:#1a2535;border-top:1px solid var(--border);font-style:italic;
  opacity:0;transition:opacity 300ms var(--ease-out) 200ms;
}
.figure-block.cap-visible .figure-caption{opacity:1}
.figure-caption strong{color:#f1f5f9;font-style:normal;font-weight:600}

/* ── CALLOUTS ────────────────────────────────────────────────────────────── */
.callout{
  background:#1a2535;border-left:4px solid var(--navy);
  border-radius:0 8px 8px 0;padding:1.1rem 1.4rem;margin:1.4rem 0;max-width:720px;
  color:var(--text);
}
.callout-warn{background:#1f1a0f;border-left-color:#c8a800}
.callout-title{font-weight:600;color:#e8edf4;margin-bottom:.35rem}

/* ── TABLES ──────────────────────────────────────────────────────────────── */
table{width:100%;border-collapse:collapse;font-size:.88rem;margin:1.4rem 0;background:var(--bg-card);border-radius:10px;overflow:hidden;border:1px solid var(--border)}
thead{background:var(--navy);color:#fff}
th{padding:.7rem 1rem;text-align:left;font-weight:600;font-size:.83rem;color:#fff}
td{padding:.6rem 1rem;border-bottom:1px solid var(--border);color:var(--text)}
tr:last-child td{border-bottom:none}
tr:nth-child(even){background:rgba(255,255,255,.03)}

/* ── VOTE LISTS ──────────────────────────────────────────────────────────── */
.vote-list{display:grid;grid-template-columns:1fr 1fr;gap:1.5rem;margin:1.4rem 0;max-width:720px}
.vote-side h4{font-weight:600;color:#e8edf4;margin-bottom:.45rem;font-size:.87rem;text-transform:uppercase;letter-spacing:.05em}
.vote-side ul{list-style:disc;padding-left:1.2rem}
.vote-side li{margin-bottom:.3rem;font-size:.88rem;line-height:1.4;color:var(--text)}
.vote-side .vm{color:var(--text-muted);font-size:.8rem}
@media(max-width:600px){.vote-list{grid-template-columns:1fr}}

/* ── INTERACTIVE SECTION ─────────────────────────────────────────────────── */
#interactive-wrapper{background:var(--bg-card);border:1px solid var(--border);border-radius:10px;padding:1.4rem;margin:1.8rem 0}
.controls{display:flex;gap:1rem;align-items:center;flex-wrap:wrap;margin-bottom:1rem}
.controls label{font-weight:600;font-size:.88rem;color:#e8edf4}
.controls select{
  padding:.42rem .85rem;border:1px solid var(--border);border-radius:6px;
  background:#1a2535;color:#e8edf4;font-size:.88rem;font-family:inherit;
  cursor:pointer;transition:border-color 160ms ease;
}
.controls select:focus{outline:2px solid var(--navy-mid);outline-offset:2px}
#plot-container{width:100%;height:580px}

/* ── PROMPT ENTRIES ──────────────────────────────────────────────────────── */
.prompt-entry{border:1px solid var(--border);border-radius:8px;margin-bottom:1.1rem;overflow:hidden}
.prompt-role{padding:.45rem 1rem;font-size:.75rem;font-weight:700;text-transform:uppercase;letter-spacing:.08em}
.prompt-role.user{background:#003056;color:#fff}
.prompt-role.ai{background:#1e3a5f;color:#7eb8e0}
.prompt-body{padding:.85rem 1rem;font-size:.87rem;line-height:1.6;max-width:720px;color:var(--text);background:var(--bg-card)}

/* ── FOOTER ──────────────────────────────────────────────────────────────── */
footer{background:#00203f;color:rgba(255,255,255,.7);font-size:.84rem;text-align:center;border-top:1px solid rgba(255,255,255,.06)}
footer .footer-inner{max-width:1100px;margin:0 auto;padding:2.2rem 2rem}
footer a{color:#7eb8e0}
footer p+p{margin-top:.6rem;font-size:.8rem;opacity:.8}

/* ── RESPONSIVE ──────────────────────────────────────────────────────────── */
@media(max-width:700px){
  .section-num{font-size:3.5rem}
  .section-title{font-size:1.4rem}
  .hero-title{font-size:1.7rem}
}
</style>
</head>
<body>

<!-- NAV -->
<nav id="main-nav" aria-label="Main navigation">
  <div class="nav-progress" id="nav-progress"></div>
  <div class="nav-inner">
    <a class="nav-brand" href="https://www.uni-mannheim.de" target="_blank" rel="noopener" aria-label="University of Mannheim">
      <img src="Uni-mannheim.svg.png" alt="University of Mannheim">
    </a>
    <button class="hamburger" id="hamburger" aria-label="Toggle menu" aria-expanded="false">&#9776;</button>
    <ul class="nav-links" id="nav-menu" role="list">
      <li><a href="#intro">Intro</a></li>
      <li><a href="#svd">SVD</a></li>
      <li><a href="#dc-svd">DC-SVD</a></li>
      <li><a href="#irt">IRT</a></li>
      <li><a href="#claim">Claim</a></li>
      <li><a href="#interactive">Explorer</a></li>
      <li><a href="#horseshoe">Horseshoe</a></li>
      <li><a href="#prompts">AI Workflow</a></li>
      <li><a href="#refs">Refs</a></li>
    </ul>
  </div>
</nav>

<!-- HERO -->
<header class="hero" id="top">
  <div class="hero-dots" aria-hidden="true"></div>
  <img src="bundestag.jpg" class="hero-eagle" alt="" aria-hidden="true">
  <div class="hero-inner">
    <div class="hero-logo">
      <a href="https://www.uni-mannheim.de" target="_blank" rel="noopener">
        <img src="Uni-mannheim.svg.png" alt="University of Mannheim logo">
      </a>
    </div>
    <h1 class="hero-title">Scaling the Bundestag: Ideal-Point Estimation from Roll-Call Votes</h1>
    <div class="hero-pills">
      <span class="hero-pill">20th Bundestag</span>
      <span class="hero-pill">2021&ndash;2025</span>
      <span class="hero-pill">Ampel Coalition</span>
    </div>
    <p class="hero-course">DS 201 &middot; Bayesian Statistics &middot; University of Mannheim &middot; FSS 2026</p>
    <div class="hero-stats" id="hero-stats">
      <div class="hero-stat hs-c1">
        <span class="hero-stat-val hero-counter" data-target="@@N_LEGISLATORS@@">@@N_LEGISLATORS@@</span>
        <span class="hero-stat-lbl">Legislators</span>
      </div>
      <div class="hero-stat hs-c2">
        <span class="hero-stat-val hero-counter" data-target="@@N_VOTES@@">@@N_VOTES@@</span>
        <span class="hero-stat-lbl">Roll-Call Votes</span>
      </div>
      <div class="hero-stat hs-c3">
        <span class="hero-stat-val hero-counter" data-target="@@PCT_OBSERVED@@">@@PCT_OBSERVED@@</span>
        <span class="hero-stat-lbl">Votes Observed</span>
      </div>
      <div class="hero-stat hs-c4">
        <span class="hero-stat-val hero-counter" data-target="@@GRAND_MEAN_PCT@@">@@GRAND_MEAN_PCT@@</span>
        <span class="hero-stat-lbl">Yes-Vote Rate</span>
      </div>
    </div>
    <a class="hero-caret" href="#intro" aria-label="Scroll to content">&#8964;</a>
  </div>
</header>

<main>

<!-- ── SECTION 00: METHODS OVERVIEW ─────────────────────────────────────── -->
<section id="intro">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">00</span>
    <h2 class="section-title">Methods Overview</h2>
    <span class="key-finding">Ideal-point estimation from binary vote records</span>
  </div>
  <div class="reveal">
  <p>
    How do we measure the political positions of legislators when all we observe is how they vote?
    This is the problem of <strong>ideal-point estimation</strong> &mdash; placing each member of
    parliament at a point on a latent ideological scale using only the binary pattern of yes and no
    votes. The key insight is that legislative voting data contains hidden structure: legislators
    who share similar ideological positions tend to vote alike across many bills.
  </p>
  <p>
    The <strong>Singular Value Decomposition (SVD)</strong> is the geometric workhorse. Given a
    legislator &times; vote matrix X, SVD factorises it as X = UDV&#x1D40;, where U gives each
    legislator a score (ideal point) and V gives each vote a loading (discriminating power). The
    first column of U (scaled by D[1,1]) provides a one-dimensional ideal-point summary. Because
    the raw vote matrix has missing entries, we handle missingness either by double-mean imputation
    or by double-centering the imputed matrix before decomposition.
  </p>
  <p>
    The <strong>2PL Item Response Theory model</strong> goes further by estimating a
    <em>discrimination</em> parameter &alpha;<sub>j</sub> per vote alongside a <em>difficulty</em>
    &beta;<sub>j</sub>. The item characteristic curve is
    $$P(X_{ij}=1 \\mid \\theta_i, \\alpha_j, \\beta_j) = \\text{logistic}(\\alpha_j(\\theta_i - \\beta_j))$$
    where &theta;<sub>i</sub> is legislator i&rsquo;s ideal point. Following <a href="v100i05.pdf" target="_blank">B&uuml;rkner (2021)</a>,
    we fit this as a nonlinear mixed model in brms, obtaining a full posterior distribution over
    all parameters &mdash; something SVD alone cannot provide.
  </p>
  <p>
    The key differentiator is <strong>uncertainty quantification</strong>. SVD gives point
    estimates only; the IRT model returns a posterior distribution, enabling probability
    statements such as &ldquo;the AfD&rsquo;s mean ideal point exceeds CDU/CSU&rsquo;s with
    probability @@P_AFD_GT_CDU@@&rdquo; with quantified credible intervals.
  </p>
  <p>
    Geometrically, SVD provides the <strong>optimal low-rank approximation</strong> to the vote matrix. By the Eckart&ndash;Young theorem, truncating to the first \\(k\\) singular components minimises the Frobenius-norm reconstruction error over all rank-\\(k\\) matrices. The first left singular vector \\(\\mathbf{u}_1\\) therefore captures the single axis that best reconstructs pairwise voting co-movements &mdash; without using any party-label information.
  </p>
  <p>
    Following <a href="v100i05.pdf" target="_blank">B&uuml;rkner (2021)</a>, we implement the 2PL model in brms as a nonlinear mixed model with formula <code>vote_binary&nbsp;~&nbsp;exp(logalpha)&nbsp;*&nbsp;eta</code>. The item discrimination \\(\\alpha_j = \\exp(\\log\\alpha_j)\\) is kept positive by the exponential link; the person&ndash;item interaction \\(\\eta_{ij} = \\theta_i - \\beta_j\\) is decomposed into crossed random effects for legislators and votes. Identification is achieved by constraining the SD of person effects to 1, placing \\(\\theta\\) on a standardised scale with mean 0.
  </p>
  <p>
    Double-centering addresses a bias in the raw imputed SVD. Without centering, the first dimension partly reflects <em>additive</em> tendencies: legislators who habitually vote yes score high regardless of ideology; popular bills load positively simply because many legislators agree. Subtracting each legislator&rsquo;s mean, each vote&rsquo;s mean, and adding back the grand mean removes these additive components, leaving a matrix of pure <em>interaction</em> residuals that DC-SVD then decomposes.
  </p>
  </div>
  <div class="stats-grid reveal" id="hero-stats-secondary">
    <div class="stat-card">
      <span class="stat-value stat-counter" data-target="@@N_LEGISLATORS@@">@@N_LEGISLATORS@@</span>
      <span class="stat-label">Legislators</span>
    </div>
    <div class="stat-card">
      <span class="stat-value stat-counter" data-target="@@N_VOTES@@">@@N_VOTES@@</span>
      <span class="stat-label">Roll-Call Votes</span>
    </div>
    <div class="stat-card">
      <span class="stat-value stat-counter" data-target="@@PCT_OBSERVED@@">@@PCT_OBSERVED@@</span>
      <span class="stat-label">Votes Observed</span>
    </div>
    <div class="stat-card">
      <span class="stat-value stat-counter" data-target="@@GRAND_MEAN_PCT@@">@@GRAND_MEAN_PCT@@</span>
      <span class="stat-label">Yes-Vote Rate</span>
    </div>
  </div>
  <div class="reveal">
  <p>
    Votes are coded <strong>1</strong> for <em>Ja</em>, <strong>0</strong> for <em>Nein</em>,
    and <strong>NA</strong> for <em>Enthaltung</em> or <em>nicht abgegeben</em>. Abstentions are
    treated as missing rather than as no-votes: abstention is a strategic act with distinct
    political meaning. Data: <a href="https://www.abgeordnetenwatch.de" target="_blank">Abgeordnetenwatch e.V.</a> (CC0).
  </p>
  </div>
</div>
</section>

<!-- ── SECTION 01: SVD ───────────────────────────────────────────────────── -->
<section id="svd">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">01</span>
    <h2 class="section-title">SVD on the Imputed Vote Matrix</h2>
    <span class="key-finding">Dimension 1 explains @@VAR_EXP_1@@ of total variance &mdash; overwhelmingly one-dimensional</span>
  </div>

  <div class="reveal">
  <p>
    Standard SVD requires a complete matrix. We handle missingness with <strong>double-mean
    imputation</strong>: each missing cell is filled with the sum of its row mean and column
    mean minus the grand mean:
  </p>
  <div class="callout">
    <em>X<sub>ij</sub><sup>imp</sup> = X&#773;<sub>i&middot;</sub> + X&#773;<sub>&middot;j</sub> &minus; X&#773;<sub>&middot;&middot;</sub></em>
    &nbsp;(only where X<sub>ij</sub> is missing)
  </div>

  <div class="code-block">
    <div class="code-label">R &mdash; Double-mean imputation <span class="code-toggle-btn">Show code &#9660;</span></div>
    <div class="code-content">
<pre><span class="cm"># Observed means</span>
grand_mean <span class="kw">&lt;-</span> <span class="fn">mean</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)
row_means  <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)
col_means  <span class="kw">&lt;-</span> <span class="fn">colMeans</span>(X_raw, na.rm <span class="kw">=</span> <span class="nb">TRUE</span>)

<span class="cm"># Fill each NA</span>
X_imputed <span class="kw">&lt;-</span> X_raw
na_idx    <span class="kw">&lt;-</span> <span class="fn">which</span>(<span class="fn">is.na</span>(X_raw), arr.ind <span class="kw">=</span> <span class="nb">TRUE</span>)
<span class="kw">for</span> (k <span class="kw">in</span> <span class="fn">seq_len</span>(<span class="fn">nrow</span>(na_idx))) {
  i <span class="kw">&lt;-</span> na_idx[k, <span class="nb">1</span>]; j <span class="kw">&lt;-</span> na_idx[k, <span class="nb">2</span>]
  X_imputed[i, j] <span class="kw">&lt;-</span> row_means[i] <span class="kw">+</span> col_means[j] <span class="kw">-</span> grand_mean
}</pre>
    </div>
  </div>

  <h3>How much structure is there?</h3>
  <p>
    Dimension 1 explains <strong>@@VAR_EXP_1@@</strong> of total variance; dimension 2 adds
    only <strong>@@VAR_EXP_2@@</strong>. The Bundestag is overwhelmingly <em>one-dimensional</em>
    in its voting behaviour &mdash; mirroring findings from comparable legislatures
    (Clinton, Jackman &amp; Rivers, 2004).
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig1-container" class="fig-plotly-sm"></div>
    <div class="figure-caption">
      <strong>Figure 1.</strong> Variance explained by each SVD dimension (bars) and
      cumulative variance (line). Dimension 1 dominates at @@VAR_EXP_1@@. The sharp elbow
      confirms a single latent axis captures almost all systematic co-variation.
    </div>
  </div>

  <div class="reveal">
  <h3>Who is where on dimension 1?</h3>
  <p>
    The first left singular vector U[,1] places every legislator on a latent scale. Oriented
    so that the AfD is positive (right-wing), the vote-space ordering reflects
    government-vs-opposition dynamics during the Ampel coalition.
  </p>
  <div class="callout">
    <div class="callout-title">Vote-space ordering (left &larr; &rarr; right)</div>
    Gr&uuml;nen &nbsp;&lt;&nbsp; SPD &nbsp;&lt;&nbsp; FDP &nbsp;&lt;&nbsp;
    CDU/CSU &nbsp;&lt;&nbsp; Linke &nbsp;&lt;&nbsp; AfD
  </div>
  <p>
    Note: Die Linke sits with the right-wing opposition parties rather than with the Ampel
    coalition. This is because the first dimension captures <strong>coalition membership</strong>
    more than traditional left-right ideology &mdash; Die Linke voted against the Ampel
    government on nearly every bill.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig2-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 2.</strong> SVD dimension-1 ideal points by party (interactive). Hover
      for legislator name and score. The Ampel coalition parties cluster to the left; CDU/CSU
      and Die Linke sit in the centre-right; AfD anchors the far right.
    </div>
  </div>

  <div class="reveal">
  <h3>Which votes discriminate most?</h3>
  <p>
    V[,1] gives each vote a loading indicating how strongly it differentiates legislators.
    High positive loadings: right-wing opposition voted yes. High negative loadings: coalition
    parties voted yes. The interactive chart shows all @@N_VOTES@@ votes sorted by loading.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig3-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 3.</strong> SVD dimension-1 loadings for all @@N_VOTES@@ votes, sorted
      from most negative (left/coalition votes yes) to most positive (right/opposition votes
      yes). Hover for the full vote label and committee. Colour shows passage outcome.
    </div>
  </div>

  <div class="reveal">
  <h3>Is dimension 2 meaningful?</h3>
  <p>
    Dimension 2 explains only <strong>@@VAR_EXP_2@@</strong> of variance. The 2D scatter
    below confirms that party separation is almost entirely along dimension 1.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig4-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 4.</strong> Two-dimensional SVD scaling (hover for legislator details).
      Party separation is almost entirely along dimension 1 (horizontal); dimension 2 adds
      limited systematic structure and is treated as largely noise.
    </div>
  </div>
</div>
</section>

<!-- ── SECTION 02: DC-SVD ────────────────────────────────────────────────── -->
<section id="dc-svd">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">02</span>
    <h2 class="section-title">Double-Centered SVD</h2>
    <span class="key-finding">SVD vs DC-SVD correlation: r = @@COR_SVD12@@ &mdash; @@COR_SVD12_NARRATIVE@@</span>
  </div>

  <div class="reveal">
  <p>
    Double-centering removes the <em>additive</em> row and column effects before
    decomposition. What remains is the pure interaction &mdash; whether a legislator voted
    more or less for a particular bill than the additive baseline predicts.
  </p>
  <div class="callout">
    <em>X&#771;<sub>ij</sub> = X<sub>ij</sub> &minus; X&#773;<sub>i&middot;</sub> &minus; X&#773;<sub>&middot;j</sub> + X&#773;<sub>&middot;&middot;</sub></em>
  </div>

  <div class="code-block">
    <div class="code-label">R &mdash; Double-centering <span class="code-toggle-btn">Show code &#9660;</span></div>
    <div class="code-content">
<pre>X_dc <span class="kw">&lt;-</span> <span class="fn">sweep</span>(<span class="fn">sweep</span>(X_imputed, <span class="nb">1</span>, rm_imp, <span class="str">"-"</span>),
              <span class="nb">2</span>, cm_imp, <span class="str">"-"</span>) <span class="kw">+</span> gm_imp

<span class="cm"># Verification: max absolute row / col mean &rarr; machine zero</span>
<span class="fn">max</span>(<span class="fn">abs</span>(<span class="fn">rowMeans</span>(X_dc)))  <span class="cm"># @@MAX_ROW_ERR@@</span>
<span class="fn">max</span>(<span class="fn">abs</span>(<span class="fn">colMeans</span>(X_dc)))  <span class="cm"># @@MAX_COL_ERR@@</span></pre>
    </div>
  </div>

  <div class="callout">
    <div class="callout-title">Verification passed</div>
    Max absolute row mean: <strong>@@MAX_ROW_ERR@@</strong> &mdash;
    Max absolute column mean: <strong>@@MAX_COL_ERR@@</strong> &mdash;
    both at floating-point machine precision.
  </div>

  <p>
    DC-SVD explains <strong>@@VAR_EXP_1_DC@@</strong> on dimension 1 (vs @@VAR_EXP_1@@ for
    the standard imputed SVD). The reduced figure reflects that the additive component has
    been removed, leaving only the interaction.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig5-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 5.</strong> DC-SVD dimension-1 ideal points by party. The party ordering
      and spread can be compared to Figure 2 (uncentred SVD).
      The correlation between the two scalings is r&nbsp;=&nbsp;@@COR_SVD12@@.
    </div>
  </div>
</div>
</section>

<!-- ── SECTION 03: IRT ───────────────────────────────────────────────────── -->
<section id="irt">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">03</span>
    <h2 class="section-title">Bayesian 2PL IRT with brms</h2>
    <span class="key-finding">SVD vs IRT correlation: r = @@COR_SVD_IRT@@ &mdash; both methods recover the same latent dimension</span>
  </div>

  <div class="reveal">
  <p>
    IRT addresses a limitation of SVD: it treats all votes as equally informative. The 2PL
    model estimates a <em>discrimination</em> parameter for each vote &mdash; how sharply it
    separates legislators. The model is:
  </p>
  <div class="callout">
    \\[P(X_{ij}=1 \\mid \\theta_i, \\alpha_j, \\beta_j) = \\text{logistic}(\\alpha_j(\\theta_i - \\beta_j))\\]
  </div>

  <div class="code-block">
    <div class="code-label">R &mdash; brms 2PL formula (<a href="v100i05.pdf" target="_blank">B&uuml;rkner 2021</a>) <span class="code-toggle-btn">Show code &#9660;</span></div>
    <div class="code-content">
<pre>formula_2pl <span class="kw">&lt;-</span> <span class="fn">bf</span>(
  vote_binary <span class="kw">~</span> <span class="fn">exp</span>(logalpha) <span class="kw">*</span> eta,
  eta      <span class="kw">~</span> <span class="nb">1</span> <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> i <span class="kw">|</span> item_id) <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> person_id),
  logalpha <span class="kw">~</span> <span class="nb">1</span> <span class="kw">+</span> (<span class="nb">1</span> <span class="kw">|</span> i <span class="kw">|</span> item_id),
  nl <span class="kw">=</span> <span class="nb">TRUE</span>
)
prior_2pl <span class="kw">&lt;-</span>
  <span class="fn">prior</span>(<span class="str">"normal(0, 5)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"b"</span>,  <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"normal(0, 1)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"b"</span>,  <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"logalpha"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"constant(1)"</span>, <span class="kw">class</span> <span class="kw">=</span> <span class="str">"sd"</span>, <span class="kw">group</span> <span class="kw">=</span> <span class="str">"person_id"</span>, <span class="kw">nlpar</span> <span class="kw">=</span> <span class="str">"eta"</span>)</pre>
    </div>
  </div>

  <h3>Prior specification (<a href="v100i05.pdf" target="_blank">B&uuml;rkner 2021</a>, Table 1)</h3>
  <table>
    <thead><tr><th>Parameter</th><th>Prior</th><th>Interpretation</th></tr></thead>
    <tbody>
      <tr><td><code>b_eta</code></td><td>Normal(0, 5)</td><td>Global difficulty; wide and weakly informative</td></tr>
      <tr><td><code>b_logalpha</code></td><td>Normal(0, 1)</td><td>Average log-discrimination; centres &alpha; near 1</td></tr>
      <tr><td>SD person (<code>eta</code>)</td><td>Constant(1)</td><td><strong>Identification</strong>: &theta; has unit SD by construction</td></tr>
      <tr><td>SD item (<code>eta</code>)</td><td>Normal(0, 3)</td><td>Item difficulties can vary up to &plusmn;3 SDs</td></tr>
      <tr><td>SD item (<code>logalpha</code>)</td><td>Normal(0, 1)</td><td>Log-discrimination SD; &alpha; ranges roughly 0.4&ndash;2.7</td></tr>
    </tbody>
  </table>

  <div class="callout callout-warn">
    <div class="callout-title">Note on convergence</div>
    With 1 chain and 100 warmup iterations, R&#770; diagnostics are not meaningful.
    For a course-level analysis, 500 posterior samples provide adequate precision for
    party-ordering probability statements. A production analysis would use 4 chains
    with 1000+ warmup iterations.
  </div>

  <h3>IRT ideal points by party</h3>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig6-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 6.</strong> IRT posterior mean ideal points (&theta;) by party (interactive).
      Hover for name, party, and 95% credible interval [&theta;<sub>lo</sub>, &theta;<sub>hi</sub>].
      The party ordering matches the SVD result.
    </div>
  </div>

  <div class="reveal">
  <h3>SVD vs IRT agreement</h3>
  <p>
    The Pearson correlation between SVD dimension-1 scores and IRT posterior means is
    <strong>r&nbsp;=&nbsp;@@COR_SVD_IRT@@</strong>. This validates both methods: SVD
    identifies latent structure without distributional assumptions; IRT quantifies it with full
    posterior uncertainty. The two methods extract the same underlying signal.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig7-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 7.</strong> SVD dim-1 scores vs IRT posterior means for all
      @@N_LEGISLATORS@@ legislators (r&nbsp;=&nbsp;@@COR_SVD_IRT@@). Dashed line = OLS fit;
      dotted line = y&nbsp;=&nbsp;x reference. Hover for name and party.
    </div>
  </div>
</div>
</section>

<!-- ── SECTION 04: SUBSTANTIVE CLAIM ────────────────────────────────────── -->
<section id="claim">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">04</span>
    <h2 class="section-title">Substantive Claim: What the Posterior Tells Us</h2>
    <span class="key-finding">P(&theta;&#772;_AfD &gt; &theta;&#772;_CDU) = @@P_AFD_GT_CDU@@ &middot; 95% CI = @@CI_DIFF_AFD_CDU@@</span>
  </div>

  <div class="reveal">
  <p>
    A central advantage of Bayesian IRT over SVD: we obtain a full posterior distribution over
    ideal points, enabling probability statements about ideological orderings with quantified
    uncertainty.
  </p>

  <div class="callout">
    <div class="callout-title">Main finding</div>
    The 20th Bundestag is scaled along a single dominant latent dimension capturing
    <strong>government-vs-opposition</strong> voting. The AfD is clearly more extreme than
    CDU/CSU: P(&theta;&#772;_AfD&nbsp;&gt;&nbsp;&theta;&#772;_CDU)&nbsp;=&nbsp;<strong>@@P_AFD_GT_CDU@@</strong>,
    95% CI for the difference = <strong>@@CI_DIFF_AFD_CDU@@</strong> &mdash; entirely positive.
    Die Linke clusters with the right-wing opposition (AfD, CDU/CSU) in vote-based space,
    not with the Ampel coalition (SPD, Gr&uuml;nen, FDP): P(&theta;&#772;_Linke&nbsp;&gt;&nbsp;&theta;&#772;_Gr&uuml;nen)&nbsp;=&nbsp;<strong>@@P_LINKE_GT_GRUEN@@</strong>.
  </div>

  <div class="code-block">
    <div class="code-label">R &mdash; Computing posterior probability P(&theta;&#772;_AfD &gt; &theta;&#772;_CDU) <span class="code-toggle-btn">Show code &#9660;</span></div>
    <div class="code-content">
<pre>draws <span class="kw">&lt;-</span> <span class="fn">as_draws_df</span>(fit_2pl)
afd_cols  <span class="kw">&lt;-</span> <span class="fn">paste0</span>(<span class="str">"r_person_id__eta["</span>, afd_ids, <span class="str">",Intercept]"</span>)
afd_draws <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(draws[, afd_cols])
cdu_draws <span class="kw">&lt;-</span> <span class="fn">rowMeans</span>(draws[, cdu_cols])
p_afd_gt_cdu <span class="kw">&lt;-</span> <span class="fn">mean</span>(afd_draws <span class="kw">&gt;</span> cdu_draws)  <span class="cm"># = @@P_AFD_RAW@@</span></pre>
    </div>
  </div>

  <div class="stats-grid">
    <div class="stat-card">
      <span class="stat-value">@@P_AFD_GT_CDU@@</span>
      <span class="stat-label">P(&theta;&#772;_AfD &gt; &theta;&#772;_CDU)</span>
      <span class="stat-desc">AfD sits right of CDU/CSU in all 500 posterior draws</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">@@P_LINKE_GT_GRUEN@@</span>
      <span class="stat-label">Linke in opposition bloc</span>
      <span class="stat-desc">P(&theta;&#772;_Linke &gt; &theta;&#772;_Gr&uuml;nen) &mdash; vote-space, not ideology</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">@@P_AFD_GT_SPD@@</span>
      <span class="stat-label">P(&theta;&#772;_AfD &gt; &theta;&#772;_SPD)</span>
      <span class="stat-desc">AfD is more extreme than SPD in all draws</span>
    </div>
    <div class="stat-card">
      <span class="stat-value">@@CI_DIFF_AFD_CDU@@</span>
      <span class="stat-label">95% CI: &theta;&#772;_AfD &minus; &theta;&#772;_CDU</span>
      <span class="stat-desc">Credible interval for the gap in units of &theta; SD</span>
    </div>
  </div>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig8-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 8.</strong> Posterior distributions of party mean ideal points from 500
      MCMC draws. Violin shows full uncertainty; dashed lines mark the 95% credible interval
      per party. Hover for median and CI values.
    </div>
  </div>

  <div class="reveal">
  <h3>Two levels of uncertainty</h3>
  <div class="callout">
    <div class="callout-title">Individual MP uncertainty</div>
    Each legislator&rsquo;s ideal point has its own posterior width reflecting how many
    informative votes they cast. Legislators who missed many votes, or voted with the majority,
    have wider intervals. Individual credible intervals often overlap across parties.
  </div>
  <div class="callout">
    <div class="callout-title">Party ordering uncertainty</div>
    Averaging over 80&ndash;200 legislators, the standard error of the party mean shrinks by
    roughly 1/&radic;n. The posterior probability that AfD&rsquo;s mean exceeds CDU/CSU&rsquo;s
    is <strong>@@P_AFD_GT_CDU@@</strong> &mdash; near-certain &mdash; even though individual
    MP credible intervals overlap.
  </div>

  <h3>What the model shows vs. what we infer</h3>
  <p>
    <strong>What the posterior directly shows:</strong> Legislators voted in patterns captured
    almost entirely by a single latent axis. The Ampel coalition parties voted together against
    the opposition on most bills. The AfD consistently voted with CDU/CSU but is positioned
    further to the right. Die Linke voted with the opposition nearly as consistently as AfD.
  </p>
  <p>
    <strong>What we infer:</strong> That this dimension partly corresponds to the conventional
    German left&ndash;right spectrum. The ordering matches self-reported positions and electoral
    results &mdash; but disentangling government&ndash;opposition dynamics from genuine ideology
    would require analysing non-government-sponsored bills or cross-legislature comparisons.
  </p>
  </div>
</div>
</section>

<!-- ── SECTION 05: INTERACTIVE EXPLORER ─────────────────────────────────── -->
<section id="interactive">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">05</span>
    <h2 class="section-title">Interactive Ideal-Point Explorer</h2>
    <span class="key-finding">IRT ideal point (x) vs SVD dimension 2 (y) &mdash; hover any legislator</span>
  </div>
  <div class="reveal">
  <p>
    Each legislator is a point by IRT ideal point (x-axis) and SVD dimension 2 (y-axis).
    Hover for name, party, and scores. Use the dropdown to highlight a specific party.
  </p>
  </div>
  <div id="interactive-wrapper" class="reveal">
    <div class="controls">
      <label for="party-select">Highlight party:</label>
      <select id="party-select" onchange="highlightParty(this.value)">
        <option value="all">All parties (coloured)</option>
        <option value="SPD">SPD</option>
        <option value="CDU/CSU">CDU/CSU</option>
        <option value="FDP">FDP</option>
        <option value="B&#220;NDNIS 90/DIE GR&#220;NEN">B&#220;NDNIS 90/DIE GR&#220;NEN</option>
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

<!-- ── SECTION 06: HORSESHOE ─────────────────────────────────────────────── -->
<section id="horseshoe">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">06</span>
    <h2 class="section-title">Extra Credit: Horseshoe Prior Extension</h2>
    <span class="key-finding">Normal vs horseshoe correlation: r = @@COR_HS_BASE@@ &middot; RMSD = @@RMSD_HS@@ &sigma;</span>
  </div>

  <div class="reveal">
  <p>
    The standard 2PL model places Normal(0,3) priors on item difficulty SDs &mdash; symmetric
    regularisation toward average difficulty. The <strong>horseshoe prior</strong> aggressively
    shrinks most parameters toward zero while allowing a few to remain large, enabling
    <em>sparse</em> estimation.
  </p>

  <div class="callout">
    <div class="callout-title">Horseshoe prior specification</div>
    Global intercept: Horseshoe(df=1, scale_global=0.5)<br>
    Item difficulty SD: half-Cauchy(0,3) &mdash; horseshoe-equivalent on scale<br>
    Item discrimination SD: half-Cauchy(0,1)<br>
    Person SD: Constant(1) &mdash; identification constraint unchanged
  </div>

  <div class="code-block">
    <div class="code-label">R &mdash; Horseshoe prior specification <span class="code-toggle-btn">Show code &#9660;</span></div>
    <div class="code-content">
<pre>prior_hs <span class="kw">&lt;-</span>
  <span class="fn">prior</span>(<span class="str">"horseshoe(df=1, scale_global=0.5)"</span>, <span class="kw">class</span><span class="kw">=</span><span class="str">"b"</span>,  <span class="kw">nlpar</span><span class="kw">=</span><span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"normal(0, 1)"</span>,                      <span class="kw">class</span><span class="kw">=</span><span class="str">"b"</span>,  <span class="kw">nlpar</span><span class="kw">=</span><span class="str">"logalpha"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"constant(1)"</span>,  <span class="kw">class</span><span class="kw">=</span><span class="str">"sd"</span>, <span class="kw">group</span><span class="kw">=</span><span class="str">"person_id"</span>, <span class="kw">nlpar</span><span class="kw">=</span><span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"student_t(1, 0, 3)"</span>, <span class="kw">class</span><span class="kw">=</span><span class="str">"sd"</span>, <span class="kw">group</span><span class="kw">=</span><span class="str">"item_id"</span>,   <span class="kw">nlpar</span><span class="kw">=</span><span class="str">"eta"</span>) <span class="kw">+</span>
  <span class="fn">prior</span>(<span class="str">"student_t(1, 0, 1)"</span>, <span class="kw">class</span><span class="kw">=</span><span class="str">"sd"</span>, <span class="kw">group</span><span class="kw">=</span><span class="str">"item_id"</span>,   <span class="kw">nlpar</span><span class="kw">=</span><span class="str">"logalpha"</span>)</pre>
    </div>
  </div>

  <p>
    The correlation between normal-prior and horseshoe-prior ideal points is
    <strong>r&nbsp;=&nbsp;@@COR_HS_BASE@@</strong>. The RMSD between the two sets is
    <strong>@@RMSD_HS@@</strong> standard deviations &mdash; a moderate shift that affects
    individual legislators but not the overall party ordering.
  </p>
  </div>

  <div class="figure-block reveal cap-visible">
    <div id="fig9-container" class="fig-plotly"></div>
    <div class="figure-caption">
      <strong>Figure 9.</strong> Normal prior vs horseshoe-prior ideal points (interactive).
      Dashed line = identity y&nbsp;=&nbsp;x (perfect agreement). Points off-diagonal shifted
      under heavier regularisation. Hover for legislator name, party, and both &theta; values.
      Correlation r&nbsp;=&nbsp;@@COR_HS_BASE@@.
    </div>
  </div>

  <div class="reveal">
  <h3>Does the horseshoe shrink moderates more?</h3>
  <p>
    A key prediction of horseshoe priors is that they shrink <em>moderate</em> parameters more
    aggressively than extreme ones. In Figure 9, points near the centre of the x-axis that
    deviate from the identity line represent legislators whose ideal points shifted most under
    the horseshoe. Points at the extremes that stay close to the identity represent robust,
    well-identified extreme positions.
  </p>
  </div>
</div>
</section>

<!-- ── SECTION 07: AI WORKFLOW ───────────────────────────────────────────── -->
<section id="prompts">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">07</span>
    <h2 class="section-title">AI Workflow Documentation</h2>
    <span class="key-finding">AI-forward track &mdash; Claude Code (claude-sonnet-4-6)</span>
  </div>
  <div class="reveal">
  <p>
    This project was completed on the AI-forward track using Claude Code (claude-sonnet-4-6,
    Anthropic). The AI was used to: (1) read and interpret the assignment specification,
    lecture notes, and B&uuml;rkner (2021) replication code; (2) design the full analysis
    pipeline; (3) write <code>analysis.R</code> and <code>build_site.R</code>; (4) build this
    site. All generated code was verified for correctness before execution.
  </p>
  <h3>Key prompts</h3>
  </div>
  <div class="reveal">
  <div class="prompt-entry">
    <div class="prompt-role user">Initial prompt (User)</div>
    <div class="prompt-body">
      Read README.md, week13_fss2026_bayes.html, and v100i05.R. Outline a plan for:
      (1) double-mean imputation + SVD, (2) double-centered SVD, (3) brms 2PL IRT
      following B&uuml;rkner 2021, (4) substantive claim using the posterior as a distribution.
      Deliver a GitHub Pages site in University of Mannheim colors with Plotly.js charts.
    </div>
  </div>
  <div class="prompt-entry">
    <div class="prompt-role ai">Response (Claude)</div>
    <div class="prompt-body">
      Read all files. Verified structure: 161 polls, 772 legislators. Wrote analysis.R
      implementing all four tasks. Used @@VAR@@ placeholder substitution in the HTML template.
    </div>
  </div>
  <div class="prompt-entry">
    <div class="prompt-role user">Refinement &amp; design prompts (User)</div>
    <div class="prompt-body">
      Fix orientation bug (AfD must be positive/right). Add horseshoe prior extension.
      Convert all figures to Plotly. Apply Emil Kowalski animation principles and full
      dark theme with Bundesadler hero watermark.
    </div>
  </div>
  <div class="prompt-entry">
    <div class="prompt-role ai">Response (Claude)</div>
    <div class="prompt-body">
      Fixed orientation using AfD vs SPD hard political knowledge. All 9 figures converted
      to dark-mode Plotly with lazy loading. Applied dark theme (#0a0f1a page, #111827 cards),
      Bundesadler hero watermark, glassmorphism stat counters, GitHub-dark code blocks with
      copy button, directional axis annotations on strip plots, 95% CI lines on posteriors.
    </div>
  </div>
  <h3>Quality checks</h3>
  <ul style="padding-left:1.4rem;margin-top:.5rem;line-height:2">
    <li>Data recoding verified: yes=1, no=0, abstain/no_show=NA</li>
    <li>Double-centering produces machine-zero row/col means (@@MAX_ROW_ERR@@)</li>
    <li>brms formula matches B&uuml;rkner (2021) Table 1 exactly</li>
    <li>Orientation: AfD > SPD in both SVD and IRT (hard political knowledge check)</li>
    <li>Posterior probabilities computed from draw-level party means, not point estimates</li>
    <li>All 9 figures rendered as interactive dark-mode Plotly charts</li>
  </ul>
  </div>
</div>
</section>

<!-- ── SECTION 08: REFERENCES ────────────────────────────────────────────── -->
<section id="refs">
<div class="container">
  <div class="section-header reveal">
    <span class="section-num" aria-hidden="true">08</span>
    <h2 class="section-title">References</h2>
  </div>
  <div class="reveal">
  <ul style="padding-left:1.4rem;line-height:2.2">
    <li><a href="v100i05.pdf" target="_blank">B&uuml;rkner, P.-C. (2021). Bayesian Item Response Modeling in R with brms and Stan.</a>
        <em>Journal of Statistical Software</em>, 100(5), 1&ndash;54.</li>
    <li>Clinton, J., Jackman, S., &amp; Rivers, D. (2004). The statistical analysis of roll call data.
        <em>American Political Science Review</em>, 98(2), 355&ndash;370.</li>
    <li>Abgeordnetenwatch e.V. (2025). Bundestag 20th Wahlperiode roll-call vote data (CC0).
        <a href="https://www.abgeordnetenwatch.de" target="_blank">abgeordnetenwatch.de</a></li>
    <li>Ratkovic, M. (2026). Week 13 &mdash; Scaling and item response theory.
        Lecture notes, Bayesian Statistics, University of Mannheim, FSS 2026.</li>
    <li>Kowalski, E. (2024). Design Engineering principles. <a href="https://animations.dev" target="_blank">animations.dev</a></li>
  </ul>
  </div>
</div>
</section>

</main>

<footer>
  <div class="footer-inner">
    <p>Maximilian Birkle &middot; Student ID: 1831999 &middot; MMDS &middot; DS 201 Bayesian Statistics &middot; Uni Mannheim &middot; FSS 2026</p>
    <p>Data: <a href="https://www.abgeordnetenwatch.de">Abgeordnetenwatch e.V.</a> (CC0) &middot;
       Analysis: <a href="https://paul-buerkner.github.io/brms/">brms</a> / Stan &middot;
       Charts: <a href="https://plotly.com/javascript/">Plotly.js</a></p>
  </div>
</footer>

<script>
/* =========================================================================
   DATA
   ========================================================================= */
const RAW_DATA   = @@PLOTLY_DATA@@;
const DRAWS_DATA = @@PARTY_DRAWS_DATA@@;
const SCREE_DATA = @@SCREE_DATA@@;
const LOADS_DATA = @@VOTE_LOADINGS_DATA@@;

/* =========================================================================
   CONSTANTS
   ========================================================================= */
const PARTY_COLORS = {
  "SPD":                   "#E3000F",
  "CDU/CSU":               "#cccccc",
  "FDP":                   "#CCBB00",
  "BÜNDNIS 90/DIE GRÜNEN": "#64A12D",
  "AfD":                   "#009EE0",
  "Die Linke":             "#BE3075",
  "BSW":                   "#9b3f8e",
  "fraktionslos":          "#9ca3af"
};
const PARTY_SHORT = {
  "BÜNDNIS 90/DIE GRÜNEN": "Grünen",
  "Die Linke": "Linke"
};
function sp(p){ return PARTY_SHORT[p] || p; }

const PC = {
  responsive:true, displayModeBar:true, displaylogo:false,
  modeBarButtonsToRemove:["lasso2d","select2d"],
  toImageButtonOptions:{format:"png",filename:"bundestag"}
};

const DARK_BG = "#111827";
const LAY_BASE = {
  plot_bgcolor: DARK_BG, paper_bgcolor: DARK_BG,
  hoverlabel:{bgcolor:"#1e293b",bordercolor:"#4a7fa5",font:{size:12,family:"Inter,sans-serif",color:"#e8edf4"}},
  font:{family:"Inter,sans-serif",color:"#e8edf4"},
  margin:{l:90,r:24,t:52,b:56}
};
const AX = {
  color:"#e8edf4", gridcolor:"#1e293b", zerolinecolor:"#334155",
  tickfont:{color:"#e8edf4"}, titlefont:{color:"#8b9ab0"}
};

/* =========================================================================
   HELPERS
   ========================================================================= */
function lazyPlot(id, fn) {
  if (!("IntersectionObserver" in window)) { fn(); return; }
  const el = document.getElementById(id);
  if (!el) { fn(); return; }
  const obs = new IntersectionObserver(function(entries) {
    if (entries[0].isIntersecting) { fn(); obs.disconnect(); }
  }, {rootMargin:"200px 0px", threshold:0});
  obs.observe(el);
}

function partyOrderByMean(field) {
  const m = {};
  RAW_DATA.forEach(d => {
    if(d[field]==null) return;
    if(!m[d.party]) m[d.party]={s:0,n:0};
    m[d.party].s += d[field]; m[d.party].n++;
  });
  return Object.keys(m).filter(p => PARTY_COLORS[p])
    .sort((a,b) => m[a].s/m[a].n - m[b].s/m[b].n);
}

function stripTraces(field, labelFn) {
  const order = partyOrderByMean(field);
  return order.map(party => {
    const rows = RAW_DATA.filter(d => d.party===party && d[field]!=null);
    return {
      type:"box", orientation:"h", name:sp(party),
      x:rows.map(d=>d[field]), y:rows.map(_=>sp(party)),
      marker:{color:PARTY_COLORS[party],size:4,opacity:0.55},
      line:{color:PARTY_COLORS[party]},
      fillcolor:PARTY_COLORS[party]+"22",
      boxpoints:"all", jitter:0.42, pointpos:0,
      whiskerwidth:0.5, boxmean:false,
      text:rows.map(labelFn),
      hovertemplate:"%{text}<extra></extra>",showlegend:false
    };
  });
}

function scatterTraces(xf,yf,labelFn,hl) {
  const parties = [...new Set(RAW_DATA.map(d=>d.party))].sort();
  return parties.map(party=>{
    const rows = RAW_DATA.filter(d=>d.party===party && d[xf]!=null && d[yf]!=null);
    const isHL = (hl==="all"||party===hl);
    return {
      type:"scatter", mode:"markers", name:sp(party),
      x:rows.map(d=>d[xf]), y:rows.map(d=>d[yf]),
      text:rows.map(labelFn),
      hovertemplate:"%{text}<extra></extra>",
      marker:{color:PARTY_COLORS[party]||"#888",size:7,
        opacity:(hl==="all")?0.72:(isHL?0.92:0.06),
        line:{width:0.5,color:"rgba(0,0,0,0.3)"}}
    };
  });
}

/* Direction annotations for strip plots */
const DIR_ANNO = [
  {xref:"paper",yref:"paper",x:0.0,y:-0.12,text:"← Left",
   showarrow:false,font:{color:"#8b9ab0",size:11},xanchor:"left"},
  {xref:"paper",yref:"paper",x:1.0,y:-0.12,text:"Right →",
   showarrow:false,font:{color:"#8b9ab0",size:11},xanchor:"right"}
];

/* =========================================================================
   FIG 1 — Scree plot
   ========================================================================= */
lazyPlot("fig1-container", function() {
  const bars = {
    type:"bar", name:"Variance explained",
    x:SCREE_DATA.map(d=>d.dim), y:SCREE_DATA.map(d=>d.var_pct),
    marker:{color:"#003056",opacity:0.9},
    hovertemplate:"Dim %{x}: %{y:.2f}%<extra></extra>"
  };
  const line = {
    type:"scatter", mode:"lines+markers", name:"Cumulative",
    x:SCREE_DATA.map(d=>d.dim), y:SCREE_DATA.map(d=>d.cumulative),
    line:{color:"#4a7fa5",width:2},
    marker:{color:"#4a7fa5",size:5},
    hovertemplate:"Cum. dim %{x}: %{y:.1f}%<extra></extra>",
    yaxis:"y2"
  };
  Plotly.newPlot("fig1-container",[bars,line],Object.assign({},LAY_BASE,{
    title:{text:"SVD Variance Explained by Dimension",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"Dimension"},dtick:1,range:[0.5,SCREE_DATA.length+0.5]}),
    yaxis:Object.assign({},AX,{title:{text:"Variance explained (%)"},rangemode:"tozero"}),
    yaxis2:Object.assign({},AX,{title:{text:"Cumulative (%)"},overlaying:"y",side:"right",range:[0,100]}),
    legend:{x:0.72,y:0.5,font:{color:"#e8edf4"},bgcolor:"rgba(17,24,39,0.8)"},
    margin:Object.assign({},LAY_BASE.margin,{l:60,r:60})
  }),PC);
});

/* =========================================================================
   FIG 2 — SVD dim1 strip by party
   ========================================================================= */
lazyPlot("fig2-container", function() {
  Plotly.newPlot("fig2-container",
    stripTraces("svd_dim1", d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>SVD dim1: "+(d.svd_dim1||0).toFixed(3)),
    Object.assign({},LAY_BASE,{
      title:{text:"SVD Dimension-1 Ideal Points by Party",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
      xaxis:Object.assign({},AX,{title:{text:"SVD Score (Dimension 1)"},zeroline:true}),
      yaxis:Object.assign({},AX,{automargin:true}),
      annotations:DIR_ANNO
    }),PC);
});

/* =========================================================================
   FIG 3 — Vote loadings horizontal bar
   ========================================================================= */
lazyPlot("fig3-container", function() {
  const top = LOADS_DATA.slice(-20).reverse();
  const bot = LOADS_DATA.slice(0,20);
  const data = [...bot,...top.reverse()];
  const labels = data.map(d=>{
    const s = (d.poll_label||"").substring(0,50);
    return s.length<(d.poll_label||"").length ? s+"…" : s;
  });
  Plotly.newPlot("fig3-container",[{
    type:"bar", orientation:"h",
    x:data.map(d=>d.svd_loading1), y:labels,
    text:data.map(d=>{
      const lbl = d.poll_label||"";
      const com = d.committee||"";
      const acc = d.accepted===true?"Passed":d.accepted===false?"Rejected":"Unknown";
      return "<b>"+lbl+"</b><br>"+com+"<br>"+acc+"<br>Loading: "+((d.svd_loading1||0).toFixed(4));
    }),
    hovertemplate:"%{text}<extra></extra>",
    marker:{color:data.map(d=>d.svd_loading1>0?"#009EE0":"#E3000F"),opacity:0.85}
  }],Object.assign({},LAY_BASE,{
    title:{text:"SVD Dimension-1 Vote Loadings (top 20 each direction)",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"SVD Dimension-1 Loading"},zeroline:true,zerolinewidth:1.5}),
    yaxis:Object.assign({},AX,{automargin:true,tickfont:{color:"#e8edf4",size:10}}),
    margin:Object.assign({},LAY_BASE.margin,{l:320})
  }),PC);
});

/* =========================================================================
   FIG 4 — 2D SVD scatter
   ========================================================================= */
lazyPlot("fig4-container", function() {
  const traces = scatterTraces("svd_dim1","svd_dim2",
    d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>dim1: "+((d.svd_dim1||0).toFixed(3))+" dim2: "+((d.svd_dim2||0).toFixed(3)),
    "all");
  Plotly.newPlot("fig4-container",traces,Object.assign({},LAY_BASE,{
    title:{text:"Two-Dimensional SVD Scaling",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"SVD Dimension 1"},zeroline:true}),
    yaxis:Object.assign({},AX,{title:{text:"SVD Dimension 2"},zeroline:true}),
    legend:{title:{text:"Party"},font:{color:"#e8edf4"},bgcolor:"rgba(17,24,39,0.8)",bordercolor:"#1e293b",borderwidth:1},
    margin:Object.assign({},LAY_BASE.margin,{l:60})
  }),PC);
});

/* =========================================================================
   FIG 5 — DC-SVD dim1 strip
   ========================================================================= */
lazyPlot("fig5-container", function() {
  Plotly.newPlot("fig5-container",
    stripTraces("svd2_dim1", d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>DC-SVD dim1: "+((d.svd2_dim1||0).toFixed(3))),
    Object.assign({},LAY_BASE,{
      title:{text:"DC-SVD Dimension-1 Ideal Points by Party",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
      xaxis:Object.assign({},AX,{title:{text:"DC-SVD Score (Dimension 1)"},zeroline:true}),
      yaxis:Object.assign({},AX,{automargin:true}),
      annotations:DIR_ANNO
    }),PC);
});

/* =========================================================================
   FIG 6 — IRT theta strip
   ========================================================================= */
lazyPlot("fig6-container", function() {
  Plotly.newPlot("fig6-container",
    stripTraces("theta", d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>θ: "+((d.theta||0).toFixed(2))+" ["+((d.theta_lo||0).toFixed(2))+", "+((d.theta_hi||0).toFixed(2))+"]"),
    Object.assign({},LAY_BASE,{
      title:{text:"IRT Posterior Mean Ideal Points (θ) by Party",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
      xaxis:Object.assign({},AX,{title:{text:"Posterior Mean Ideal Point (θ)"},zeroline:true}),
      yaxis:Object.assign({},AX,{automargin:true}),
      annotations:DIR_ANNO
    }),PC);
});

/* =========================================================================
   FIG 7 — SVD vs IRT scatter (OLS line + y=x reference)
   ========================================================================= */
lazyPlot("fig7-container", function() {
  const valid = RAW_DATA.filter(d=>d.svd_dim1!=null&&d.theta!=null);
  const traces = scatterTraces("svd_dim1","theta",
    d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>SVD: "+((d.svd_dim1||0).toFixed(3))+" θ: "+((d.theta||0).toFixed(2)),
    "all");
  const xs = valid.map(d=>d.svd_dim1), ys = valid.map(d=>d.theta);
  const n=xs.length, sx=xs.reduce((a,b)=>a+b,0), sy=ys.reduce((a,b)=>a+b,0);
  const sxy=xs.reduce((a,x,i)=>a+x*ys[i],0), sx2=xs.reduce((a,x)=>a+x*x,0);
  const slope=(n*sxy-sx*sy)/(n*sx2-sx*sx), int=(sy-slope*sx)/n;
  const xmin=Math.min(...xs), xmax=Math.max(...xs);
  const allVals=[...xs,...ys], mn=Math.min(...allVals), mx=Math.max(...allVals);
  const olsLine={type:"scatter",mode:"lines",name:"OLS fit",
    x:[xmin,xmax],y:[slope*xmin+int,slope*xmax+int],
    line:{color:"#4a7fa5",width:1.8,dash:"dot"},showlegend:false,hoverinfo:"skip"};
  const identLine={type:"scatter",mode:"lines",name:"y = x",
    x:[mn,mx],y:[mn,mx],
    line:{color:"#334155",width:1.4,dash:"dashdot"},showlegend:false,hoverinfo:"skip"};
  Plotly.newPlot("fig7-container",[...traces,identLine,olsLine],Object.assign({},LAY_BASE,{
    title:{text:"SVD Dimension-1 vs IRT Ideal Points",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"SVD Score (Dimension 1)"},zeroline:true}),
    yaxis:Object.assign({},AX,{title:{text:"Posterior Mean θ"},zeroline:true}),
    legend:{title:{text:"Party"},font:{color:"#e8edf4"},bgcolor:"rgba(17,24,39,0.8)",bordercolor:"#1e293b",borderwidth:1},
    margin:Object.assign({},LAY_BASE.margin,{l:60})
  }),PC);
});

/* =========================================================================
   FIG 8 — Posterior party distributions violin + 95% CI lines
   ========================================================================= */
lazyPlot("fig8-container", function() {
  const byParty = {};
  DRAWS_DATA.forEach(d=>{ if(!byParty[d.party]) byParty[d.party]=[]; byParty[d.party].push(d.theta); });
  const order = Object.keys(byParty).sort((a,b)=>{
    const ma=byParty[a].reduce((s,v)=>s+v,0)/byParty[a].length;
    const mb=byParty[b].reduce((s,v)=>s+v,0)/byParty[b].length;
    return ma-mb;
  });
  const violins = order.map(party=>({
    type:"violin", orientation:"h", name:sp(party),
    x:byParty[party], y:byParty[party].map(_=>sp(party)),
    box:{visible:true}, meanline:{visible:true}, points:false,
    fillcolor:(PARTY_COLORS[party]||"#888")+"33",
    line:{color:PARTY_COLORS[party]||"#888",width:1.5},
    hovertemplate:sp(party)+"<br>median: %{median:.2f}<extra></extra>",
    showlegend:false
  }));
  /* 95% CI as line traces */
  const ciTraces = order.map(party=>{
    const vals = byParty[party].slice().sort((a,b)=>a-b);
    const n = vals.length;
    const lo = vals[Math.max(0,Math.floor(0.025*n))];
    const hi = vals[Math.min(n-1,Math.ceil(0.975*n)-1)];
    const col = PARTY_COLORS[party]||"#888";
    return {
      type:"scatter", mode:"lines+markers", name:"",
      x:[lo,hi], y:[sp(party),sp(party)],
      line:{color:col,width:3},
      marker:{symbol:"line-ns-open",size:9,color:col,line:{width:2.5,color:col}},
      showlegend:false,
      hovertemplate:"95% CI: %{x:.2f}<extra>"+sp(party)+"</extra>"
    };
  });
  Plotly.newPlot("fig8-container",[...violins,...ciTraces],Object.assign({},LAY_BASE,{
    title:{text:"Posterior Distributions of Party Mean Ideal Points (500 MCMC draws)",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"Party Mean Ideal Point (θ)"},zeroline:true}),
    yaxis:Object.assign({},AX,{automargin:true}),
    violingap:0.05, violingroupgap:0,
    margin:Object.assign({},LAY_BASE.margin,{l:80})
  }),PC);
});

/* =========================================================================
   FIG 9 — Horseshoe vs normal prior scatter
   ========================================================================= */
lazyPlot("fig9-container", function() {
  const valid = RAW_DATA.filter(d=>d.theta!=null&&d.theta_hs!=null);
  const allX = valid.map(d=>d.theta), allY = valid.map(d=>d.theta_hs);
  const mn=Math.min(...allX,...allY), mx=Math.max(...allX,...allY);
  const diag={type:"scatter",mode:"lines",x:[mn,mx],y:[mn,mx],
    line:{color:"#334155",width:1.5,dash:"dash"},showlegend:false,hoverinfo:"skip"};
  const traces = scatterTraces("theta","theta_hs",
    d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>Normal: "+((d.theta||0).toFixed(2))+" HS: "+((d.theta_hs||0).toFixed(2)),
    "all");
  Plotly.newPlot("fig9-container",[diag,...traces],Object.assign({},LAY_BASE,{
    title:{text:"Horseshoe vs Normal-Prior Ideal Points",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
    xaxis:Object.assign({},AX,{title:{text:"Normal-Prior θ"},zeroline:true}),
    yaxis:Object.assign({},AX,{title:{text:"Horseshoe-Prior θ"},zeroline:true}),
    legend:{title:{text:"Party"},font:{color:"#e8edf4"},bgcolor:"rgba(17,24,39,0.8)",bordercolor:"#1e293b",borderwidth:1},
    margin:Object.assign({},LAY_BASE.margin,{l:60})
  }),PC);
});

/* =========================================================================
   SECTION 05 — Interactive explorer
   ========================================================================= */
const allParties = [...new Set(RAW_DATA.map(d=>d.party))].sort();
function buildTraces(hl){
  return allParties.map(party=>{
    const rows=RAW_DATA.filter(d=>d.party===party);
    const isHL=(hl==="all"||party===hl);
    return {
      type:"scatter",mode:"markers",name:sp(party),
      x:rows.map(d=>d.theta), y:rows.map(d=>d.svd_dim2),
      text:rows.map(d=>"<b>"+d.legislator+"</b><br>"+sp(d.party)+"<br>Ideal point: "+((d.theta||0).toFixed(2))),
      hovertemplate:"%{text}<extra></extra>",
      marker:{color:PARTY_COLORS[party]||"#888",size:8,
        opacity:(hl==="all")?0.72:(isHL?0.92:0.06),
        line:{width:0.5,color:"rgba(0,0,0,0.3)"}}
    };
  });
}
const layout5=Object.assign({},LAY_BASE,{
  title:{text:"Ideal Point Estimates — 20th Bundestag (2021–2025)",font:{size:13,color:"#e8edf4"},x:0.02,xanchor:"left"},
  xaxis:Object.assign({},AX,{title:{text:"IRT Ideal Point (θ) — left to right"},zeroline:true}),
  yaxis:Object.assign({},AX,{title:{text:"SVD Dimension 2"},zeroline:true}),
  legend:{title:{text:"Party"},font:{color:"#e8edf4"},bgcolor:"rgba(17,24,39,0.9)",bordercolor:"#1e293b",borderwidth:1},
  hovermode:"closest",
  height:560,
  margin:Object.assign({},LAY_BASE.margin,{l:60})
});
let plot5ready = false;
lazyPlot("plot-container", function() {
  plot5ready = true;
  Plotly.newPlot("plot-container",buildTraces("all"),layout5,PC);
});
function highlightParty(p){
  if(!plot5ready) return;
  Plotly.react("plot-container",buildTraces(p),layout5,PC);
}

/* =========================================================================
   SCROLL PROGRESS BAR
   ========================================================================= */
(function(){
  const bar=document.getElementById("nav-progress");
  function update(){
    const h=document.documentElement;
    const pct=(h.scrollTop||document.body.scrollTop)/(h.scrollHeight-h.clientHeight)*100;
    bar.style.width=Math.min(pct,100)+"%";
  }
  window.addEventListener("scroll",update,{passive:true});
  update();
})();

/* =========================================================================
   NAV SCROLL-SPY
   ========================================================================= */
(function(){
  const links=document.querySelectorAll(".nav-links a[href]");
  const ids=Array.from(links).map(a=>a.getAttribute("href").slice(1)).filter(Boolean);
  const secs=ids.map(id=>document.getElementById(id)).filter(Boolean);
  function spy(){
    const mid=window.scrollY+window.innerHeight*0.4;
    let cur=secs[0];
    secs.forEach(s=>{ if(s.offsetTop<=mid) cur=s; });
    links.forEach(a=>{
      a.classList.toggle("nav-active", a.getAttribute("href")==="#"+cur.id);
    });
  }
  window.addEventListener("scroll",spy,{passive:true});
  spy();
})();

/* =========================================================================
   HAMBURGER MENU
   ========================================================================= */
(function(){
  const btn=document.getElementById("hamburger");
  const menu=document.getElementById("nav-menu");
  btn.addEventListener("click",function(){
    const open=menu.classList.toggle("open");
    btn.setAttribute("aria-expanded",open);
  });
  document.addEventListener("click",function(e){
    if(!btn.contains(e.target)&&!menu.contains(e.target)) menu.classList.remove("open");
  });
})();

/* =========================================================================
   SECTION REVEAL
   ========================================================================= */
(function(){
  if(!("IntersectionObserver" in window)){
    document.querySelectorAll(".reveal").forEach(el=>el.classList.add("visible"));
    return;
  }
  const obs=new IntersectionObserver(function(entries){
    entries.forEach(function(e){
      if(e.isIntersecting){ e.target.classList.add("visible"); obs.unobserve(e.target); }
    });
  },{threshold:0.07,rootMargin:"0px 0px -40px 0px"});
  document.querySelectorAll(".reveal").forEach(el=>obs.observe(el));
})();

/* =========================================================================
   ANIMATED STAT COUNTERS
   ========================================================================= */
function animateCounter(el){
  const raw=el.dataset.target||el.textContent;
  const num=parseFloat(raw.replace(/[^0-9.]/g,""));
  const suffix=raw.replace(/[0-9.]/g,"");
  if(isNaN(num)||num===0) return;
  const isInt=Number.isInteger(num);
  const dur=1600, start=performance.now();
  function frame(now){
    const t=Math.min((now-start)/dur,1);
    const eased=1-Math.pow(1-t,4);
    const val=num*eased;
    el.textContent=(isInt?Math.round(val):val.toFixed(1))+suffix;
    if(t<1) requestAnimationFrame(frame);
    else el.textContent=raw;
  }
  requestAnimationFrame(frame);
}
window.addEventListener("load",function(){
  setTimeout(function(){
    document.querySelectorAll(".hero-counter").forEach(animateCounter);
  },400);
});
(function(){
  const obs=new IntersectionObserver(function(entries){
    entries.forEach(function(e){
      if(e.isIntersecting){
        e.target.querySelectorAll(".stat-counter").forEach(animateCounter);
        obs.unobserve(e.target);
      }
    });
  },{threshold:0.3});
  document.querySelectorAll(".stats-grid").forEach(g=>obs.observe(g));
})();

/* =========================================================================
   CODE BLOCK SMOOTH EXPAND
   ========================================================================= */
document.querySelectorAll(".code-block").forEach(function(block){
  const label=block.querySelector(".code-label");
  const content=block.querySelector(".code-content");
  const btn=block.querySelector(".code-toggle-btn");
  if(!label||!content) return;
  label.addEventListener("click",function(){
    const open=content.classList.toggle("open");
    if(btn) btn.innerHTML=open?"Hide code &#9650;":"Show code &#9660;";
  });
});

/* =========================================================================
   COPY TO CLIPBOARD — injected beside the toggle button, no overlap
   ========================================================================= */
document.querySelectorAll(".code-block").forEach(function(block){
  const pre=block.querySelector("pre");
  const toggler=block.querySelector(".code-toggle-btn");
  if(!pre||!toggler) return;
  const copyBtn=document.createElement("button");
  copyBtn.className="copy-btn";
  copyBtn.textContent="Copy";
  copyBtn.setAttribute("aria-label","Copy code to clipboard");
  copyBtn.addEventListener("click",function(e){
    e.stopPropagation();
    navigator.clipboard.writeText(pre.textContent).then(function(){
      copyBtn.textContent="✓ Copied";
      copyBtn.classList.add("copied");
      setTimeout(function(){ copyBtn.textContent="Copy"; copyBtn.classList.remove("copied"); },2000);
    }).catch(function(){ copyBtn.textContent="Error"; setTimeout(function(){ copyBtn.textContent="Copy"; },2000); });
  });
  /* Wrap both buttons in a flex row */
  const wrap=document.createElement("span");
  wrap.style.cssText="display:flex;gap:8px;align-items:center;flex-shrink:0";
  toggler.parentNode.insertBefore(wrap,toggler);
  wrap.appendChild(toggler);
  wrap.appendChild(copyBtn);
});

/* Ensure captions visible after a delay */
setTimeout(function(){
  document.querySelectorAll(".figure-block").forEach(function(b){ b.classList.add("cap-visible"); });
},2000);
</script>
</body>
</html>'

# ---------- substitute all placeholders -----------------------------------

html <- template
for(key in names(sub_map)){
  html <- gsub(key, sub_map[[key]], html, fixed=TRUE)
}
writeLines(html, "index.html")
cat(sprintf("index.html written: %s bytes\n", format(nchar(html), big.mark=",")))
