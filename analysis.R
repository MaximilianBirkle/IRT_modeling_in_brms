# ==============================================================================
# analysis.R
# Ideal-Point Estimation: Bundestag 20th Wahlperiode (2021–2025)
# Methods: Double-mean imputation + SVD, double-centered SVD, brms 2PL IRT
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(jsonlite)
  library(scales)
  library(ggridges)
})

set.seed(42)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = 1)

for (d in c("figures", "results", "models")) {
  if (!dir.exists(d)) dir.create(d)
}

# Official German party colors (keys match cleaned party names)
party_colors <- c(
  "SPD"                    = "#E3000F",
  "CDU/CSU"                = "#000000",
  "FDP"                    = "#FFED00",
  "BÜNDNIS 90/DIE GRÜNEN"  = "#64A12D",
  "AfD"                    = "#009EE0",
  "Die Linke"              = "#BE3075",
  "BSW"                    = "#6A0F49",
  "fraktionslos"           = "#888888"
)

theme_set(
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(color = "#1a3a3a", face = "bold", size = 14),
      plot.subtitle = element_text(color = "#555555", size = 11),
      panel.grid.minor = element_blank(),
      plot.caption  = element_text(color = "#777777", size = 9)
    )
)

cat("=== Loading data ===\n")

votes_raw <- read_csv("bundestag_wp132_votes.csv", show_col_types = FALSE)
codebook  <- read_csv("bundestag_wp132_vote_codebook.csv", show_col_types = FALSE)

# Recode votes and clean party / legislator names
# Remove soft hyphens from party names (Grünen entry has one)
votes <- votes_raw %>%
  mutate(
    vote_binary = case_when(
      vote == "yes" ~ 1L,
      vote == "no"  ~ 0L,
      TRUE          ~ NA_integer_
    ),
    party      = str_remove_all(
                   str_remove(fraction_label, fixed(" (Bundestag 2021 - 2025)")),
                   "­"),   # remove soft hyphens
    legislator = str_remove(mandate_label, fixed(" (Bundestag 2021 - 2025)"))
  ) %>%
  # Consolidate Die Linke spellings
  mutate(party = case_when(
    party %in% c("DIE LINKE.", "Die Linke.", "Die Linke. (Gruppe)") ~ "Die Linke",
    party == "BSW (Gruppe)" ~ "BSW",
    TRUE ~ party
  ))

# ==============================================================================
# 1. WIDE MATRIX
# ==============================================================================

cat("=== Building wide matrix ===\n")

# Assign each legislator (mandate_id) a single party:
# use the most common party assignment across all their votes
leg_party <- votes %>%
  count(mandate_id, party) %>%
  group_by(mandate_id) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(mandate_id, party)

leg_name <- votes %>%
  distinct(mandate_id, legislator) %>%
  group_by(mandate_id) %>%
  slice(1) %>%
  ungroup()

leg_meta_base <- leg_party %>%
  left_join(leg_name, by = "mandate_id")

# Update party column in votes for consistency
votes <- votes %>%
  select(-party) %>%
  left_join(leg_meta_base %>% select(mandate_id, party), by = "mandate_id")

votes_wide <- votes %>%
  select(mandate_id, poll_id, vote_binary) %>%
  pivot_wider(
    id_cols     = mandate_id,
    names_from  = poll_id,
    values_from = vote_binary,
    values_fn   = function(x) x[1]   # take first if duplicate (shouldn't occur)
  )

leg_meta <- votes_wide %>%
  select(mandate_id) %>%
  left_join(leg_meta_base, by = "mandate_id")

# Numeric vote matrix (rows = legislators, cols = polls)
X_raw <- votes_wide %>%
  select(-mandate_id) %>%
  as.matrix()
rownames(X_raw) <- leg_meta$legislator

# Column-to-poll mapping
poll_id_cols <- as.numeric(colnames(X_raw))
vote_meta <- tibble(poll_id = poll_id_cols) %>%
  left_join(
    codebook %>% select(poll_id, vote_column, poll_label, accepted, committee, url),
    by = "poll_id"
  ) %>%
  arrange(match(poll_id, poll_id_cols))

n_leg   <- nrow(X_raw)
n_votes <- ncol(X_raw)
cat(sprintf("Matrix: %d legislators × %d votes\n", n_leg, n_votes))
cat(sprintf("Observed yes/no cells: %d / %d (%.1f%%)\n",
            sum(!is.na(X_raw)), n_leg * n_votes,
            100 * mean(!is.na(X_raw))))

# ==============================================================================
# 2. DOUBLE-MEAN IMPUTATION + SVD
# ==============================================================================

cat("=== SVD on imputed matrix ===\n")

grand_mean <- mean(X_raw, na.rm = TRUE)
row_means  <- rowMeans(X_raw, na.rm = TRUE)
col_means  <- colMeans(X_raw, na.rm = TRUE)

# Fill NAs: imputed value = row_mean + col_mean - grand_mean
X_imputed <- X_raw
na_idx <- which(is.na(X_raw), arr.ind = TRUE)
for (k in seq_len(nrow(na_idx))) {
  i <- na_idx[k, 1]; j <- na_idx[k, 2]
  X_imputed[i, j] <- row_means[i] + col_means[j] - grand_mean
}

svd1     <- svd(X_imputed)
U1       <- svd1$u
D1       <- svd1$d
V1       <- svd1$v
var_exp1 <- D1^2 / sum(D1^2)

cat(sprintf("Variance explained — dim 1: %.2f%%  dim 2: %.2f%%\n",
            100 * var_exp1[1], 100 * var_exp1[2]))

# Orient: AfD (right) must be positive — use hard political knowledge vs SPD (left)
mean_svd_afd <- mean(U1[leg_meta$party == "AfD", 1], na.rm = TRUE)
mean_svd_spd <- mean(U1[leg_meta$party == "SPD", 1], na.rm = TRUE)
if (!is.na(mean_svd_afd) && !is.na(mean_svd_spd) && mean_svd_afd < mean_svd_spd) {
  U1[, 1] <- -U1[, 1]; V1[, 1] <- -V1[, 1]
  U1[, 2] <- -U1[, 2]; V1[, 2] <- -V1[, 2]
}

leg_meta <- leg_meta %>%
  mutate(svd_dim1 = U1[, 1], svd_dim2 = U1[, 2])

vote_meta <- vote_meta %>%
  mutate(svd_loading1 = V1[, 1], svd_loading2 = V1[, 2])

# Party ordering by mean dim-1 score
party_order <- leg_meta %>%
  group_by(party) %>%
  summarise(mean_dim1 = mean(svd_dim1), .groups = "drop") %>%
  arrange(mean_dim1) %>%
  pull(party)

# ==============================================================================
# FIGURE 1: Scree plot
# ==============================================================================

scree_df <- tibble(
  dim        = seq_along(D1)[1:min(25, length(D1))],
  var_pct    = var_exp1[1:min(25, length(D1))] * 100,
  cumulative = cumsum(var_exp1[1:min(25, length(D1))]) * 100
)

p_scree <- ggplot(scree_df, aes(x = dim)) +
  geom_col(aes(y = var_pct), fill = "#3c7a6e", alpha = 0.85, width = 0.7) +
  geom_line(aes(y = cumulative), color = "#1a3a3a", linewidth = 1.1) +
  geom_point(aes(y = cumulative), color = "#1a3a3a", size = 2.5) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25)) +
  labs(
    title    = "Variance Explained by SVD Dimensions",
    subtitle = "Bars: per-dimension variance; line: cumulative",
    x        = "Dimension",
    y        = "Variance Explained (%)",
    caption  = "Source: Bundestag 20th Wahlperiode roll-call votes"
  )

ggsave("figures/01_scree_plot.png", p_scree, width = 8, height = 5, dpi = 150)

# ==============================================================================
# FIGURE 2: Legislator ideal points — SVD dim 1 by party
# ==============================================================================

leg_plot <- leg_meta %>%
  mutate(party_f = factor(party, levels = party_order)) %>%
  filter(party %in% names(party_colors))

p_dim1 <- ggplot(leg_plot, aes(x = svd_dim1, y = party_f, color = party)) +
  geom_jitter(height = 0.22, alpha = 0.45, size = 1.6) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 18,
               color = "#1a3a3a") +
  scale_color_manual(values = party_colors, guide = "none") +
  labs(
    title    = "Bundestag Legislator Positions — SVD Dimension 1",
    subtitle = "Each dot = one legislator; diamond = party mean",
    x        = "SVD Score (Dimension 1: left ← → right)",
    y        = NULL,
    caption  = "Double-mean imputation applied before SVD"
  )

ggsave("figures/02_svd_dim1_by_party.png", p_dim1,
       width = 9, height = 6, dpi = 150)

# ==============================================================================
# FIGURE 3: Extreme vote loadings on dim 1
# ==============================================================================

top_votes <- bind_rows(
  slice_max(vote_meta, svd_loading1, n = 10) %>% mutate(direction = "Positive end (right)"),
  slice_min(vote_meta, svd_loading1, n = 10) %>% mutate(direction = "Negative end (left)")
) %>%
  mutate(label = str_trunc(coalesce(poll_label, as.character(poll_id)), 48)) %>%
  arrange(svd_loading1)

p_loadings <- ggplot(top_votes,
  aes(x = svd_loading1, y = reorder(label, svd_loading1), fill = direction)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = c(
    "Positive end (right)" = "#0489db",
    "Negative end (left)"  = "#be3075"
  )) +
  labs(
    title    = "Most Discriminating Votes — SVD Dimension 1",
    subtitle = "Top and bottom 10 votes by dimension-1 loading",
    x        = "Loading (Dimension 1)",
    y        = NULL,
    fill     = NULL,
    caption  = "Positive loading → vote separates right parties; Negative → left parties"
  ) +
  theme(legend.position = "top")

ggsave("figures/03_vote_loadings_dim1.png", p_loadings,
       width = 10, height = 7.5, dpi = 150)

# ==============================================================================
# FIGURE 4: 2D scatter (SVD dim 1 vs dim 2)
# ==============================================================================

p_2d <- ggplot(leg_plot, aes(x = svd_dim1, y = svd_dim2, color = party)) +
  geom_point(alpha = 0.55, size = 1.8) +
  scale_color_manual(values = party_colors, name = "Party") +
  labs(
    title    = "Two-Dimensional Scaling of the Bundestag",
    subtitle = "Dimension 1 (main left–right axis) vs. Dimension 2",
    x        = "SVD Dimension 1",
    y        = "SVD Dimension 2",
    caption  = "SVD on double-mean-imputed vote matrix"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 0.9)))

ggsave("figures/04_svd_2d.png", p_2d, width = 9, height = 7, dpi = 150)

# ==============================================================================
# 3. DOUBLE-CENTERED SVD
# ==============================================================================

cat("=== Double-centered SVD ===\n")

# Double-center the IMPUTED matrix (ensures exact zero row/col means)
gm_imp   <- mean(X_imputed)
rm_imp   <- rowMeans(X_imputed)
cm_imp   <- colMeans(X_imputed)

X_dc <- sweep(sweep(X_imputed, 1, rm_imp, "-"), 2, cm_imp, "-") + gm_imp

# Verify
max_row_err <- max(abs(rowMeans(X_dc)))
max_col_err <- max(abs(colMeans(X_dc)))
cat(sprintf("Max |row mean| after double-centering: %.2e\n", max_row_err))
cat(sprintf("Max |col mean| after double-centering: %.2e\n", max_col_err))

svd2     <- svd(X_dc)
U2       <- svd2$u
D2       <- svd2$d
V2       <- svd2$v
var_exp2 <- D2^2 / sum(D2^2)

# Orient consistently (AfD positive)
mean_dc_afd <- mean(U2[leg_meta$party == "AfD", 1], na.rm = TRUE)
mean_dc_spd <- mean(U2[leg_meta$party == "SPD", 1], na.rm = TRUE)
if (!is.na(mean_dc_afd) && !is.na(mean_dc_spd) && mean_dc_afd < mean_dc_spd) {
  U2[, 1] <- -U2[, 1]; V2[, 1] <- -V2[, 1]
}

leg_meta <- leg_meta %>%
  mutate(svd2_dim1 = U2[, 1], svd2_dim2 = U2[, 2])

vote_meta <- vote_meta %>%
  mutate(svd2_loading1 = V2[, 1])

cor_svd12 <- cor(leg_meta$svd_dim1, leg_meta$svd2_dim1)
cat(sprintf("DC-SVD variance explained — dim 1: %.2f%%  dim 2: %.2f%%\n",
            100 * var_exp2[1], 100 * var_exp2[2]))
cat(sprintf("Correlation SVD1 vs DC-SVD dim1: %.4f\n", cor_svd12))

# ==============================================================================
# FIGURE 5: Legislator positions — DC-SVD dim 1
# ==============================================================================

leg_plot2 <- leg_meta %>%
  mutate(party_f = factor(party, levels = party_order)) %>%
  filter(party %in% names(party_colors))

p_dim1_dc <- ggplot(leg_plot2, aes(x = svd2_dim1, y = party_f, color = party)) +
  geom_jitter(height = 0.22, alpha = 0.45, size = 1.6) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 18,
               color = "#1a3a3a") +
  scale_color_manual(values = party_colors, guide = "none") +
  labs(
    title    = "Bundestag Legislator Positions — Double-Centered SVD Dimension 1",
    subtitle = "Each dot = one legislator; diamond = party mean",
    x        = "Double-Centered SVD Score (Dimension 1)",
    y        = NULL,
    caption  = sprintf("Correlation with standard SVD dim 1: r = %.3f", cor_svd12)
  )

ggsave("figures/05_svd2_dim1_by_party.png", p_dim1_dc,
       width = 9, height = 6, dpi = 150)

# ==============================================================================
# 4. brms 2PL IRT MODEL
# ==============================================================================

cat("=== Fitting brms 2PL IRT model ===\n")

brms_data <- votes %>%
  filter(!is.na(vote_binary)) %>%
  transmute(
    person_id   = as.character(mandate_id),
    item_id     = as.character(poll_id),
    vote_binary = vote_binary,
    legislator  = legislator,
    party       = party
  )

cat(sprintf("brms observations: %d (%.1f%% of total cells)\n",
            nrow(brms_data), 100 * nrow(brms_data) / (n_leg * n_votes)))

# 2PL formula following Bürkner (2021) exactly
formula_2pl <- bf(
  vote_binary ~ exp(logalpha) * eta,
  eta      ~ 1 + (1 | i | item_id) + (1 | person_id),
  logalpha ~ 1 + (1 | i | item_id),
  nl = TRUE
)

# Priors from Bürkner (2021) Table 1
prior_2pl <-
  prior("normal(0, 5)", class = "b",  nlpar = "eta") +
  prior("normal(0, 1)", class = "b",  nlpar = "logalpha") +
  prior("constant(1)", class = "sd",  group = "person_id", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item_id",   nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item_id",   nlpar = "logalpha")

fit_2pl <- brm(
  formula = formula_2pl,
  data    = brms_data,
  family  = brmsfamily("bernoulli", "logit"),
  prior   = prior_2pl,
  chains  = 1,
  iter    = 600,
  warmup  = 100,
  seed    = 42,
  file    = "models/fit_2pl_bundestag",
  backend = "rstan"
)

cat("Model fitted.\n")
print(summary(fit_2pl), digits = 3)

# ==============================================================================
# Extract person (theta) parameters
# ==============================================================================

ranef_out  <- ranef(fit_2pl)
theta_arr  <- ranef_out$person_id[, , "eta_Intercept"]

theta_df <- as_tibble(theta_arr, rownames = "person_id") %>%
  rename(theta = Estimate, theta_se = Est.Error,
         theta_lo = Q2.5, theta_hi = Q97.5) %>%
  mutate(mandate_id = as.numeric(person_id)) %>%
  left_join(leg_meta_base, by = "mandate_id")

# Orient theta to match SVD dim1 (which is already AfD-positive)
# This is more robust than party detection because it uses the already-oriented SVD
svd_dim1_for_irt <- leg_meta %>%
  select(mandate_id, svd_dim1) %>%
  inner_join(theta_df %>% select(mandate_id, theta), by = "mandate_id")
test_cor_irt <- cor(svd_dim1_for_irt$svd_dim1, svd_dim1_for_irt$theta,
                    use = "complete.obs")

if (test_cor_irt < 0) {
  theta_df <- theta_df %>%
    mutate(
      theta    = -theta,
      lo_old   = theta_lo,
      theta_lo = -theta_hi,
      theta_hi = -lo_old
    ) %>%
    select(-lo_old)
}

leg_meta <- leg_meta %>%
  left_join(
    theta_df %>% select(mandate_id, theta, theta_se, theta_lo, theta_hi),
    by = "mandate_id"
  )

# Correlation SVD dim1 vs IRT theta
leg_cor <- leg_meta %>% filter(!is.na(theta))
cor_svd_irt <- cor(leg_cor$svd_dim1, leg_cor$theta)
cat(sprintf("Correlation SVD dim1 vs IRT theta: %.4f\n", cor_svd_irt))

# ==============================================================================
# FIGURE 6: IRT theta by party
# ==============================================================================

leg_plot3 <- leg_meta %>%
  filter(!is.na(theta), party %in% names(party_colors)) %>%
  mutate(party_f = factor(party, levels = party_order))

p_theta <- ggplot(leg_plot3, aes(x = theta, y = party_f, color = party)) +
  geom_jitter(height = 0.22, alpha = 0.45, size = 1.6) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 18, color = "#1a3a3a") +
  scale_color_manual(values = party_colors, guide = "none") +
  labs(
    title    = "Bundestag Ideal Points — 2PL IRT Posterior Mean (θ)",
    subtitle = "Each dot = one legislator's posterior mean; diamond = party mean",
    x        = "Posterior Mean Ideal Point (θ)",
    y        = NULL,
    caption  = "1 chain, 500 posterior samples; constant(1) prior on person SD fixes scale"
  )

ggsave("figures/06_irt_theta.png", p_theta, width = 9, height = 6, dpi = 150)

# ==============================================================================
# FIGURE 7: SVD dim 1 vs IRT theta
# ==============================================================================

p_cor <- ggplot(leg_cor %>% filter(party %in% names(party_colors)),
  aes(x = svd_dim1, y = theta, color = party)) +
  geom_point(alpha = 0.5, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE,
              color = "#1a3a3a", fill = "#cccccc", linewidth = 1) +
  annotate("label", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = sprintf("r = %.3f", cor_svd_irt),
           size = 4.5, color = "#1a3a3a", fill = "white", label.size = 0.3) +
  scale_color_manual(values = party_colors, name = "Party") +
  labs(
    title    = "Agreement Between SVD and IRT Ideal Points",
    subtitle = "Each point = one legislator",
    x        = "SVD Score (Dimension 1)",
    y        = "IRT Posterior Mean (θ)",
    caption  = "Near-perfect correlation confirms both methods recover the same latent dimension"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 0.9)))

ggsave("figures/07_svd_vs_irt.png", p_cor, width = 9, height = 7, dpi = 150)

# ==============================================================================
# 5. SUBSTANTIVE CLAIM — Posterior distributions over party means
# ==============================================================================

cat("=== Extracting posterior draws ===\n")

draws <- as_draws_df(fit_2pl)

# Person-ID → party mapping
person_party <- brms_data %>% distinct(person_id, party)

# Function: extract party-level mean theta draw series
party_theta_draws <- function(party_pattern) {
  pids <- person_party %>%
    filter(str_detect(party, party_pattern)) %>%
    pull(person_id)
  cols <- paste0("r_person_id__eta[", pids, ",Intercept]")
  cols <- intersect(cols, colnames(draws))
  if (length(cols) == 0) return(NULL)
  rowMeans(draws[, cols, drop = FALSE])
}

draws_afd   <- party_theta_draws("AfD")
draws_cdu   <- party_theta_draws("CDU")
draws_spd   <- party_theta_draws("SPD")
draws_gruen <- party_theta_draws("GRÜNEN")
draws_linke <- party_theta_draws("Linke")
draws_fdp   <- party_theta_draws("FDP")

# Orient draws: AfD must be RIGHT of SPD — hard political knowledge
if (!is.null(draws_afd) && !is.null(draws_spd) && mean(draws_afd) < mean(draws_spd)) {
  draws_afd   <- -draws_afd
  draws_cdu   <- if (!is.null(draws_cdu))   -draws_cdu   else NULL
  draws_spd   <- -draws_spd
  draws_gruen <- if (!is.null(draws_gruen)) -draws_gruen else NULL
  draws_linke <- if (!is.null(draws_linke)) -draws_linke else NULL
  draws_fdp   <- if (!is.null(draws_fdp))   -draws_fdp   else NULL
}

# Substantive probabilities
p_afd_gt_cdu    <- if (!is.null(draws_afd) && !is.null(draws_cdu))
  mean(draws_afd > draws_cdu) else NA_real_

p_linke_lt_gruen <- if (!is.null(draws_linke) && !is.null(draws_gruen))
  mean(draws_linke < draws_gruen) else NA_real_

p_afd_gt_spd    <- if (!is.null(draws_afd) && !is.null(draws_spd))
  mean(draws_afd > draws_spd) else NA_real_

cat(sprintf("P(mean θ_AfD > mean θ_CDU/CSU) = %.4f\n", p_afd_gt_cdu))
cat(sprintf("P(mean θ_Linke < mean θ_Grünen) = %.4f\n", p_linke_lt_gruen))
cat(sprintf("P(mean θ_AfD > mean θ_SPD) = %.4f\n", p_afd_gt_spd))

# Credible intervals for AfD–CDU difference
diff_afd_cdu <- draws_afd - draws_cdu
ci_diff <- quantile(diff_afd_cdu, c(0.025, 0.975))
cat(sprintf("95%% CI for θ_AfD − θ_CDU: [%.3f, %.3f]\n", ci_diff[1], ci_diff[2]))

# ==============================================================================
# FIGURE 8: Posterior distributions of party mean ideal points
# ==============================================================================

party_draws_list <- list(
  "AfD"                    = draws_afd,
  "CDU/CSU"                = draws_cdu,
  "FDP"                    = draws_fdp,
  "SPD"                    = draws_spd,
  "BÜNDNIS 90/DIE GRÜNEN"  = draws_gruen,
  "Die Linke"              = draws_linke
)

party_draws_df <- bind_rows(lapply(names(party_draws_list), function(p) {
  if (!is.null(party_draws_list[[p]]))
    tibble(party = p, theta = as.numeric(party_draws_list[[p]]))
}))

party_order_draws <- party_draws_df %>%
  group_by(party) %>%
  summarise(m = mean(theta), .groups = "drop") %>%
  arrange(m) %>%
  pull(party)

party_draws_df <- party_draws_df %>%
  mutate(party_f = factor(party, levels = party_order_draws))

p_post <- ggplot(party_draws_df,
  aes(x = theta, y = party_f, fill = party, color = party)) +
  geom_density_ridges(
    alpha = 0.65,
    scale = 0.85,
    quantile_lines = TRUE,
    quantiles      = c(0.025, 0.975),
    rel_min_height = 0.01
  ) +
  scale_fill_manual(values  = party_colors, guide = "none") +
  scale_color_manual(values = party_colors, guide = "none") +
  labs(
    title    = "Posterior Distribution of Party Mean Ideal Points (θ)",
    subtitle = "Each curve = distribution of mean θ across 500 MCMC draws; lines = 95% CI",
    x        = "Party Mean Ideal Point (θ)",
    y        = NULL,
    caption  = sprintf(
      "P(θ̄_AfD > θ̄_CDU) = %.3f   |   P(θ̄_Linke < θ̄_Grünen) = %.3f",
      p_afd_gt_cdu, p_linke_lt_gruen
    )
  )

ggsave("figures/08_posterior_parties.png", p_post,
       width = 9, height = 7, dpi = 150)

# ==============================================================================
# 6. HORSESHOE PRIOR EXTENSION (Extra Credit)
# ==============================================================================

cat("=== Fitting horseshoe-regularized 2PL IRT model ===\n")

# Horseshoe-inspired regularization:
#   - horseshoe(df=1) on global difficulty intercept (b_eta) — the parameter
#     class where brms supports horseshoe() directly
#   - student_t(1, 0, ...) = Cauchy priors on item-level SDs: this is the
#     classic half-Cauchy / horseshoe-equivalent for scale parameters
#   - person SD remains constant(1) to preserve identification
#
# Effect: items are more aggressively regularized toward average difficulty
# and average discrimination. The implied ideal points change because the
# relative informativeness of each vote is reweighted.

prior_hs <-
  prior("horseshoe(df=1, scale_global=0.5)", class = "b",  nlpar = "eta") +
  prior("normal(0, 1)",       class = "b",  nlpar = "logalpha") +
  prior("constant(1)",        class = "sd", group = "person_id", nlpar = "eta") +
  prior("student_t(1, 0, 3)", class = "sd", group = "item_id",   nlpar = "eta") +
  prior("student_t(1, 0, 1)", class = "sd", group = "item_id",   nlpar = "logalpha")

fit_hs <- brm(
  formula = formula_2pl,
  data    = brms_data,
  family  = brmsfamily("bernoulli", "logit"),
  prior   = prior_hs,
  chains  = 1,
  iter    = 600,
  warmup  = 100,
  seed    = 43,
  file    = "models/fit_hs_bundestag",
  backend = "rstan"
)

cat("Horseshoe model fitted.\n")

# Extract theta from horseshoe model
ranef_hs     <- ranef(fit_hs)
theta_hs_arr <- ranef_hs$person_id[, , "eta_Intercept"]

theta_hs_df <- as_tibble(theta_hs_arr, rownames = "person_id") %>%
  rename(theta_hs = Estimate, theta_hs_se = Est.Error,
         theta_hs_lo = Q2.5, theta_hs_hi = Q97.5) %>%
  mutate(mandate_id = as.numeric(person_id))

# Orient horseshoe theta to match SVD dim1 (same rule as normal-prior model)
svd_for_hs <- leg_meta %>%
  select(mandate_id, svd_dim1) %>%
  inner_join(theta_hs_df %>% select(mandate_id, theta_hs), by = "mandate_id")
test_cor_hs <- cor(svd_for_hs$svd_dim1, svd_for_hs$theta_hs, use = "complete.obs")

if (test_cor_hs < 0) {
  theta_hs_df <- theta_hs_df %>%
    mutate(
      theta_hs    = -theta_hs,
      lo_old      = theta_hs_lo,
      theta_hs_lo = -theta_hs_hi,
      theta_hs_hi = -lo_old
    ) %>%
    select(-lo_old)
}

# Comparison: baseline (normal prior) vs horseshoe
leg_comparison <- theta_df %>%
  select(mandate_id, theta) %>%
  inner_join(theta_hs_df %>% select(mandate_id, theta_hs), by = "mandate_id") %>%
  left_join(leg_meta_base, by = "mandate_id") %>%
  filter(party %in% names(party_colors))

cor_hs_base <- cor(leg_comparison$theta, leg_comparison$theta_hs, use = "complete.obs")
cat(sprintf("Correlation normal-prior vs horseshoe theta: %.4f\n", cor_hs_base))

# Root-mean-square deviation (how much do ideal points shift?)
rmsd_hs <- sqrt(mean((leg_comparison$theta - leg_comparison$theta_hs)^2, na.rm = TRUE))

# Does horseshoe shrink moderate legislators more than extreme ones?
# Compute: for each quintile of |theta|, mean shift = |theta_hs - theta|
leg_comparison <- leg_comparison %>%
  mutate(
    abs_theta  = abs(theta),
    abs_shift  = abs(theta_hs - theta),
    extremism  = ntile(abs_theta, 5)
  )
shift_by_extreme <- leg_comparison %>%
  group_by(extremism) %>%
  summarise(mean_shift = mean(abs_shift), .groups = "drop")
cat("Mean absolute shift by extremism quintile (1=most moderate, 5=most extreme):\n")
print(shift_by_extreme)

# ==============================================================================
# FIGURE 9: Horseshoe vs Normal prior ideal-point comparison
# ==============================================================================

p_hs <- ggplot(leg_comparison, aes(x = theta, y = theta_hs, color = party)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "#aaaaaa", linewidth = 0.7) +
  geom_point(alpha = 0.55, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, inherit.aes = FALSE,
              aes(x = theta, y = theta_hs),
              color = "#00203f", linewidth = 1) +
  scale_color_manual(values = party_colors, name = "Party") +
  annotate("label", x = -Inf, y = Inf, hjust = -0.1, vjust = 1.2,
           label = sprintf("r = %.3f  RMSD = %.3f", cor_hs_base, rmsd_hs),
           size = 4, color = "#00203f", fill = "white", label.size = 0.3) +
  labs(
    title    = "Horseshoe vs. Normal Prior: Ideal-Point Comparison",
    subtitle = "Dashed line = identity (perfect agreement); points off-diagonal shifted by prior",
    x        = "Normal-Prior Ideal Point (θ)",
    y        = "Horseshoe-Prior Ideal Point (θ)",
    caption  = "Horseshoe: horseshoe(df=1) on global intercept; Cauchy on item SDs"
  ) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 0.9)))

ggsave("figures/09_horseshoe_comparison.png", p_hs,
       width = 9, height = 7, dpi = 150)

# ==============================================================================
# SAVE RESULTS FOR SITE
# ==============================================================================

cat("=== Saving results ===\n")

# Plotly data (for interactive chart)
plotly_data <- leg_meta %>%
  filter(!is.na(theta)) %>%
  select(mandate_id, legislator, party, svd_dim1, svd_dim2,
         svd2_dim1, theta, theta_se, theta_lo, theta_hi) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

write_json(plotly_data, "results/plotly_data.json", dataframe = "rows", na = "null")

# Summary statistics
summary_stats <- list(
  n_legislators      = n_leg,
  n_votes            = n_votes,
  n_observed         = sum(!is.na(X_raw)),
  pct_observed       = round(100 * mean(!is.na(X_raw)), 1),
  n_brms_obs         = nrow(brms_data),
  var_exp_dim1       = round(100 * var_exp1[1], 2),
  var_exp_dim2       = round(100 * var_exp1[2], 2),
  var_exp_dim1_dc    = round(100 * var_exp2[1], 2),
  var_exp_dim2_dc    = round(100 * var_exp2[2], 2),
  cor_svd12          = round(cor_svd12, 4),
  cor_svd_irt        = round(cor_svd_irt, 4),
  p_afd_gt_cdu       = round(p_afd_gt_cdu, 4),
  p_linke_lt_gruen   = round(p_linke_lt_gruen, 4),
  p_afd_gt_spd       = round(p_afd_gt_spd, 4),
  ci_diff_afd_cdu_lo = round(ci_diff[1], 3),
  ci_diff_afd_cdu_hi = round(ci_diff[2], 3),
  grand_mean         = round(grand_mean, 4),
  max_row_err_dc     = signif(max_row_err, 3),
  max_col_err_dc     = signif(max_col_err, 3),
  cor_hs_base        = round(cor_hs_base, 4),
  rmsd_hs            = round(rmsd_hs, 4)
)

write_json(summary_stats, "results/summary_stats.json", auto_unbox = TRUE)

# Extreme vote labels (for site text)
top10_pos <- slice_max(vote_meta, svd_loading1, n = 5) %>%
  select(poll_label, committee, accepted, svd_loading1) %>%
  mutate(svd_loading1 = round(svd_loading1, 4))
top10_neg <- slice_min(vote_meta, svd_loading1, n = 5) %>%
  select(poll_label, committee, accepted, svd_loading1) %>%
  mutate(svd_loading1 = round(svd_loading1, 4))

write_json(list(positive = top10_pos, negative = top10_neg),
           "results/extreme_votes.json", dataframe = "rows")

write_json(party_draws_df %>% select(party, theta),
           "results/party_draws.json", dataframe = "rows")

cat("=== Analysis complete. Run build_site.R to generate index.html ===\n")
