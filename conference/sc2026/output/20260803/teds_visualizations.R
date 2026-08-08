################################################################################
## Visualizations -- TEDS Replication
##
## Run after teds_bn_pipeline.R.
##
## Design decisions that differ from the US figures:
##
##   Colour. The US version used the ggplot default red/teal. That palette is
##   actively misleading in Taiwan, where blue and green ARE the parties. All
##   partisan figures use pan-Green (#1B9E45) and pan-Blue (#1F5FA9) with TPP
##   teal for the third force. Observed-vs-synthetic comparisons use a
##   separate neutral palette so the two encodings never collide on one panel.
##
##   Third party. Ko took 26.5% in 2024. The US figures could bury Other in a
##   residual; here it needs equal visual weight.
##
##   China factor. Four figures with no US counterpart: the identity-by-cohort
##   gradient, the tondu distribution, the identity and tondu interventions,
##   and the threat-salience contrast.
##
##   ROC annotation. The diagonal is the target. The US figure said so in a
##   subtitle; here it is annotated on the panel, because the inverted reading
##   is the most common misreading of this result.
################################################################################

library(tidyverse)
library(patchwork)
library(pROC)
library(scales)

OUT_DIR <- "output"
FIG_DIR <- file.path(OUT_DIR, "figures")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
## PALETTES AND THEME
################################################################################

pal_party <- c("DPP" = "#1B9E45", "KMT" = "#1F5FA9",
               "Other" = "#28C8C8", "NoVote" = "#B8B8B8")

pal_media <- c("Green" = "#1B9E45", "Blue" = "#1F5FA9", "None" = "#B8B8B8")

## Identity and tondu are ordered, so they use ramps rather than categorical
## colour. Both run green (Taiwan-leaning) to warm (China-leaning) so the two
## figures read consistently side by side.
pal_identity <- c("Taiwanese" = "#0B7A3B", "Both" = "#7FB069",
                  "Chinese" = "#C4522E")

pal_tondu <- c("Independence" = "#0B7A3B", "StatusQuo" = "#9AA5B1",
               "Unification" = "#C4522E")

pal_partyid <- c("Green" = "#1B9E45", "Blue" = "#1F5FA9",
                 "White" = "#28C8C8", "Independent" = "#B8B8B8")

pal_source <- c("Observed" = "#2D3142", "Synthetic" = "#E8A33D")

pal_regime_var <- c("MediaExposure" = "#1F5FA9", "Identity" = "#0B7A3B",
                    "Tondu" = "#7A5C9E")

theme_teds <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle    = element_text(colour = "grey35", size = rel(0.9)),
      plot.caption     = element_text(colour = "grey45", size = rel(0.72),
                                      hjust = 0),
      strip.text       = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

save_fig <- function(p, name, w = 9, h = 5.5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(FIG_DIR, paste0(name, ".pdf")), p,
         width = w, height = h, bg = "white")
  invisible(p)
}

################################################################################
## FIG 1: DAG
################################################################################
## Draw so the Taiwan-specific difference is the visually obvious thing:
## Identity and Tondu sit on the spine and everything downstream passes
## through them.

library(ggraph)
library(tidygraph)

edges <- tribble(
  ~from,           ~to,
  "Age",           "Identity",     "Sex",       "Identity",
  "Education",     "Identity",     "Region",    "Identity",
  "Identity",      "Tondu",        "Age",       "Tondu",
  "Education",     "Tondu",        "Region",    "Tondu",
  "Age",           "LatentType",   "Sex",       "LatentType",
  "Education",     "LatentType",   "Region",    "LatentType",
  "Identity",      "LatentType",   "Tondu",     "LatentType",
  "Identity",      "MediaExposure","Tondu",     "MediaExposure",
  "LatentType",    "MediaExposure","Education", "MediaExposure",
  "Region",        "MediaExposure",
  "MediaExposure", "Turnout",      "LatentType","Turnout",
  "Identity",      "Turnout",
  "Turnout",       "VoteChoice",   "MediaExposure", "VoteChoice",
  "LatentType",    "VoteChoice",   "Identity",  "VoteChoice",
  "Tondu",         "VoteChoice"
)

nodes <- tribble(
  ~name,           ~role,
  "Age",           "Demographic", "Sex",        "Demographic",
  "Education",     "Demographic", "Region",     "Demographic",
  "Identity",      "China factor","Tondu",      "China factor",
  "LatentType",    "Latent",      "MediaExposure", "Treatment",
  "Turnout",       "Outcome",     "VoteChoice", "Outcome"
)

g <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

p_dag <- ggraph(g, layout = "sugiyama") +
  geom_edge_link(arrow = arrow(length = unit(2.2, "mm"), type = "closed"),
                 end_cap = circle(7, "mm"), start_cap = circle(7, "mm"),
                 edge_colour = "grey58", edge_width = 0.35, alpha = 0.7) +
  geom_node_label(aes(label = name, fill = role), colour = "white",
                  fontface = "bold", size = 3,
                  label.padding = unit(0.3, "lines"),
                  label.r = unit(0.28, "lines")) +
  scale_fill_manual(values = c(
    "Demographic"  = "#6B7280",
    "China factor" = "#0B7A3B",
    "Latent"       = "#8E6FBF",
    "Treatment"    = "#1F5FA9",
    "Outcome"      = "#2D3142")) +
  labs(
    title    = "Directed acyclic graph: Taiwan specification",
    subtitle = "Identity and Tondu are inserted between demographics and political behaviour",
    caption  = paste(
      "Departure from the US specification. Ethnicity is absent from the data;",
      "its role is absorbed by Identity. Following Achen and Wang (2017),\nnational",
      "identity is modeled as causally prior to cross-Strait policy preference,",
      "and together they form the China factor -- the organizing variable\nthrough",
      "which demographic position translates into media choice and vote choice.")) +
  theme_graph(base_family = "sans") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        plot.caption = element_text(colour = "grey45", size = 7.5, hjust = 0))

save_fig(p_dag, "fig01_dag_taiwan", w = 10.5, h = 7)

################################################################################
## FIG 2: VOTE CHOICE, OBSERVED VS SYNTHETIC
################################################################################

vote_dist <- purrr::map_dfr(YEARS, function(y) {
  o <- dplyr::filter(teds, YEAR == y); s <- synth_by_year[[as.character(y)]]
  bind_rows(count(o, VoteChoice) |> mutate(p = n / sum(n), source = "Observed"),
            count(s, VoteChoice) |> mutate(p = n / sum(n), source = "Synthetic")) |>
    mutate(YEAR = y)
})

p_vote <- ggplot(vote_dist, aes(VoteChoice, p, fill = source)) +
  geom_col(position = position_dodge(0.75), width = 0.68) +
  geom_text(aes(label = percent(p, accuracy = 0.1)),
            position = position_dodge(0.75), vjust = -0.45,
            size = 2.4, colour = "grey30") +
  facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_source) +
  scale_y_continuous(labels = percent_format(), expand = expansion(c(0, 0.14))) +
  labs(title = "Vote choice: observed vs synthetic",
       subtitle = "Ko Wen-je's 2024 candidacy makes 'Other' a full category, not a residual",
       x = NULL, y = "Proportion",
       caption = paste("Third-force share: Soong 12.8% (2016), Soong 4.3% (2020),",
                       "Ko 26.5% (2024). The US application could treat Other as\na",
                       "residual; the Taiwanese case cannot.")) +
  theme_teds()

save_fig(p_vote, "fig02_vote_distribution")

################################################################################
## FIG 3: THE CHINA FACTOR -- identity and tondu by cohort
################################################################################
## No US counterpart. The identity gradient is the steepest in the data and
## the reason for using five age bands rather than the US paper's four.

id_cohort <- teds |>
  count(YEAR, Age, Identity) |>
  group_by(YEAR, Age) |> mutate(p = n / sum(n)) |> ungroup()

td_cohort <- teds |>
  count(YEAR, Age, Tondu) |>
  group_by(YEAR, Age) |> mutate(p = n / sum(n)) |> ungroup()

p_id <- ggplot(id_cohort, aes(Age, p, fill = Identity)) +
  geom_col(width = 0.8) + facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_identity) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "National identity by birth cohort", x = NULL, y = NULL) +
  theme_teds() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

p_td <- ggplot(td_cohort, aes(Age, p, fill = Tondu)) +
  geom_col(width = 0.8) + facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_tondu) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "Unification-independence position by birth cohort",
       x = "Age group", y = NULL,
       caption = paste("The two components of the China factor. Identity is",
                       "modeled as causally prior to policy preference, following\nthe",
                       "standard ordering in the Taiwan politics literature. Whether",
                       "the cohort gradient is media-driven or a socialization\neffect",
                       "is separable with the identity intervention (Fig 8) -- a",
                       "question the US design cannot pose.")) +
  theme_teds() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

save_fig(p_id / p_td, "fig03_china_factor_cohort", w = 9, h = 8)

################################################################################
## FIG 4: SELECTIVE EXPOSURE
################################################################################

sel_id <- teds |>
  count(YEAR, Identity, MediaExposure) |>
  group_by(YEAR, Identity) |> mutate(p = n / sum(n)) |> ungroup()

sel_td <- teds |>
  count(YEAR, Tondu, MediaExposure) |>
  group_by(YEAR, Tondu) |> mutate(p = n / sum(n)) |> ungroup()

p_sel1 <- ggplot(sel_id, aes(Identity, p, fill = MediaExposure)) +
  geom_col(width = 0.72) + facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_media) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "Media exposure by national identity", x = NULL, y = NULL) +
  theme_teds()

p_sel2 <- ggplot(sel_td, aes(Tondu, p, fill = MediaExposure)) +
  geom_col(width = 0.72) + facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_media) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  labs(title = "Media exposure by unification-independence position",
       x = NULL, y = NULL,
       caption = paste("Selective exposure in Taiwan runs along the identity",
                       "cleavage rather than an ideological one. Outlet alignment\nfollows",
                       "Hsiao (2006) and Lo, Wang and Hou (2007); it is sharper and",
                       "more stable than the US Left/Right coding, which the\noriginal",
                       "paper conceded was coarse.")) +
  theme_teds() + theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 8))

save_fig(p_sel1 / p_sel2, "fig04_selective_exposure", w = 9, h = 8)

################################################################################
## FIG 5: JOINT DISTRIBUTION
################################################################################

joint <- purrr::map_dfr(YEARS, function(y) {
  o <- dplyr::filter(teds, YEAR == y); s <- synth_by_year[[as.character(y)]]
  bind_rows(count(o, Turnout, VoteChoice) |> mutate(p = n / sum(n), source = "Observed"),
            count(s, Turnout, VoteChoice) |> mutate(p = n / sum(n), source = "Synthetic")) |>
    mutate(YEAR = y, cell = paste(Turnout, VoteChoice, sep = " x "))
}) |> dplyr::filter(p > 0)

p_joint <- ggplot(joint, aes(reorder(cell, -p), p, fill = source)) +
  geom_col(position = position_dodge(0.75), width = 0.68) +
  facet_wrap(~ YEAR, scales = "free_x") +
  scale_fill_manual(values = pal_source) +
  scale_y_continuous(labels = percent_format(), expand = expansion(c(0, 0.08))) +
  labs(title = "Joint distribution: turnout x vote choice",
       subtitle = "Structural zeros are enforced by construction, not filtered post hoc",
       x = NULL, y = "Joint proportion") +
  theme_teds() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

save_fig(p_joint, "fig05_joint_distribution")

################################################################################
## FIG 6: ROC -- with the inverted reading annotated
################################################################################

roc_df <- purrr::map_dfr(YEARS, function(y) {
  r <- roc_by_year[[as.character(y)]]
  tibble(YEAR = y, fpr = 1 - r$specificities, tpr = r$sensitivities,
         auc = as.numeric(pROC::auc(r)))
})

auc_lab <- roc_df |> distinct(YEAR, auc) |> mutate(lab = sprintf("AUC = %.3f", auc))

p_roc <- ggplot(roc_df, aes(fpr, tpr)) +
  geom_abline(slope = 1, intercept = 0, linetype = "22",
              colour = "#C4522E", linewidth = 0.7) +
  geom_line(colour = "#1F5FA9", linewidth = 0.9) +
  geom_text(data = auc_lab, aes(x = 0.62, y = 0.13, label = lab),
            size = 3.3, fontface = "bold", colour = "#2D3142", inherit.aes = FALSE) +
  annotate("text", x = 0.50, y = 0.62, label = "target", angle = 45,
           size = 3, colour = "#C4522E", fontface = "italic") +
  facet_wrap(~ YEAR) + coord_equal() +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Adversarial classification: can a classifier tell synthetic from real?",
       subtitle = "Here the diagonal is the GOOD result -- it means the two are indistinguishable",
       x = "False positive rate", y = "True positive rate",
       caption = paste("Note the inverted reading. In prediction tasks AUC -> 1.0 is",
                       "success; in synthetic-data validation AUC -> 0.50 is success.\nA",
                       "curve bowing toward the top-left would mean the synthetic records",
                       "carry detectable artefacts.")) +
  theme_teds()

save_fig(p_roc, "fig06_roc_curves", w = 9, h = 4.6)

################################################################################
## FIG 7: MEDIA INTERVENTION
################################################################################

fx_long <- function(dat, var) {
  dat |> dplyr::filter(regime_var == var) |>
    select(YEAR, regime, d_turnout, d_dpp, d_kmt, d_other) |>
    pivot_longer(starts_with("d_"), names_to = "outcome", values_to = "delta") |>
    mutate(outcome = recode(outcome, d_turnout = "Turnout", d_dpp = "DPP share",
                            d_kmt = "KMT share", d_other = "Other share") |>
             factor(levels = c("Turnout", "DPP share", "KMT share", "Other share")))
}

p_causal_media <- fx_long(causal_effects, "MediaExposure") |>
  mutate(regime = factor(regime, levels = c("Green", "Blue", "None"))) |>
  ggplot(aes(regime, delta, fill = regime)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%+.1f", delta),
                vjust = ifelse(delta >= 0, -0.4, 1.3)),
            size = 2.3, colour = "grey25") +
  facet_grid(outcome ~ YEAR, scales = "free_y") +
  scale_fill_manual(values = pal_media) +
  labs(title = "Causal effects of media exposure interventions",
       subtitle = expression(paste("Percentage-point change from baseline under ",
                                   italic("do"), "(MediaExposure = m)")),
       x = NULL, y = "Change (pp)",
       caption = paste("Estimated by graph mutilation (Pearl 2009): all edges into",
                       "MediaExposure are removed before sampling, which is what\ndistinguishes",
                       "do(M = m) from conditioning on M = m.")) +
  theme_teds() + theme(legend.position = "none")

save_fig(p_causal_media, "fig07_causal_media", w = 9, h = 8)

################################################################################
## FIG 8: CHINA-FACTOR INTERVENTIONS -- no US counterpart
################################################################################

p_causal_id <- fx_long(causal_effects, "Identity") |>
  mutate(regime = factor(regime, levels = c("Taiwanese", "Both", "Chinese"))) |>
  ggplot(aes(regime, delta, fill = regime)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_col(width = 0.66) +
  geom_text(aes(label = sprintf("%+.1f", delta),
                vjust = ifelse(delta >= 0, -0.4, 1.3)),
            size = 2.2, colour = "grey25") +
  facet_grid(outcome ~ YEAR, scales = "free_y") +
  scale_fill_manual(values = pal_identity) +
  labs(title = expression(paste(italic("do"), "(Identity = i)")),
       x = NULL, y = "Change (pp)") +
  theme_teds() + theme(legend.position = "none")

p_causal_td <- fx_long(causal_effects, "Tondu") |>
  mutate(regime = factor(regime, levels = c("Independence", "StatusQuo",
                                            "Unification"))) |>
  ggplot(aes(regime, delta, fill = regime)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_col(width = 0.66) +
  geom_text(aes(label = sprintf("%+.1f", delta),
                vjust = ifelse(delta >= 0, -0.4, 1.3)),
            size = 2.2, colour = "grey25") +
  facet_grid(outcome ~ YEAR, scales = "free_y") +
  scale_fill_manual(values = pal_tondu) +
  labs(title = expression(paste(italic("do"), "(Tondu = t)")),
       x = NULL, y = NULL) +
  theme_teds() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1, size = 7))

p_china <- (p_causal_id | p_causal_td) +
  plot_annotation(
    title = "Causal effects of China-factor interventions",
    caption = paste("No counterpart exists in the US analysis. If these effects",
                    "exceed the media effects in Figure 7, that is a quantitative\nstatement",
                    "of Achen and Wang's (2017) argument that the China factor organizes",
                    "Taiwanese electoral behaviour more powerfully\nthan any issue dimension."),
    theme = theme(plot.title = element_text(face = "bold", size = 13),
                  plot.caption = element_text(colour = "grey45", size = 7.5,
                                              hjust = 0)))

save_fig(p_china, "fig08_causal_china_factor", w = 12, h = 8)

################################################################################
## FIG 9: THREAT-SALIENCE CONTRAST
################################################################################
## The figure that ties the cross-year story together. The three waves span
## three levels of China-threat salience, so year-specific networks rest on
## substantive rather than merely technical grounds.

p_magnitude <- effect_magnitude |>
  pivot_longer(starts_with("range_"), names_to = "outcome", values_to = "range") |>
  mutate(outcome = recode(outcome, range_turnout = "Turnout",
                          range_dpp = "DPP share")) |>
  ggplot(aes(factor(YEAR), range, fill = regime_var)) +
  geom_col(position = position_dodge(0.75), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f", range)),
            position = position_dodge(0.75), vjust = -0.4,
            size = 2.5, colour = "grey30") +
  facet_wrap(~ outcome) +
  scale_fill_manual(values = pal_regime_var) +
  scale_y_continuous(expand = expansion(c(0, 0.14))) +
  labs(title = "Which intervention moves outcomes more?",
       subtitle = "Spread of outcomes across intervention values, on a common scale",
       x = NULL, y = "Range across regimes (pp)") +
  theme_teds()

p_ratio <- salience_contrast |>
  select(YEAR, identity_over_media, tondu_over_media) |>
  pivot_longer(-YEAR, names_to = "contrast", values_to = "ratio") |>
  mutate(contrast = recode(contrast,
                           identity_over_media = "Identity / Media",
                           tondu_over_media    = "Tondu / Media")) |>
  ggplot(aes(factor(YEAR), ratio, fill = contrast)) +
  geom_hline(yintercept = 1, linetype = "22", colour = "grey40") +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", ratio)),
            position = position_dodge(0.7), vjust = -0.4,
            size = 2.6, colour = "grey30") +
  scale_fill_manual(values = c("Identity / Media" = "#0B7A3B",
                               "Tondu / Media"    = "#7A5C9E")) +
  scale_y_continuous(expand = expansion(c(0, 0.16))) +
  labs(title = "China factor relative to media, by wave",
       subtitle = "Above the dashed line, the China factor moves DPP share more than media does",
       x = NULL, y = "Ratio of effect ranges",
       caption = paste("The three waves span three levels of cross-Strait threat",
                       "salience: post-Sunflower (2016), the Hong Kong shock (2020),\nand",
                       "normalization with a third force bracketing the China question",
                       "(2024). Tsai trailed Han by up to twenty-five points in\nearly 2019",
                       "before winning a record 57.1 percent. Media effects should compress",
                       "in 2020, when an exogenous shock dominated the\nchoice environment,",
                       "while China-factor effects should peak.")) +
  theme_teds()

save_fig(p_magnitude / p_ratio, "fig09_salience_contrast", w = 9, h = 8)

################################################################################
## FIG 10: HETEROGENEOUS EFFECTS BY LATENT TYPE
################################################################################

p_hte <- hte |>
  mutate(regime = factor(regime, levels = c("Green", "Blue", "None"))) |>
  ggplot(aes(regime, 100 * p_turnout, colour = LatentType, group = LatentType)) +
  geom_line(linewidth = 0.75, alpha = 0.85) + geom_point(size = 2) +
  facet_wrap(~ YEAR) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Heterogeneous mobilization effects by latent voter type",
       subtitle = "Steeper lines indicate greater responsiveness to media regime",
       x = expression(paste(italic("do"), "(MediaExposure = m)")),
       y = "Turnout probability (%)",
       caption = paste("The US analysis found low-engagement latent types most",
                       "responsive and highly engaged partisans nearly unmoved.\nConvergence",
                       "across a two-party ideological system and a multiparty",
                       "identity-based one would suggest the Zaller (1992)\nmechanism is not",
                       "an artefact of American political structure. See",
                       "latent_profiles.csv for class composition.")) +
  theme_teds()

save_fig(p_hte, "fig10_hte_latent", w = 9, h = 5.5)

################################################################################
## FIG 11: BACKFIRE TEST
################################################################################

p_backfire <- backfire |>
  ggplot(aes(Identity, kmt_response, fill = Identity)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.45) +
  geom_col(width = 0.66) +
  geom_text(aes(label = sprintf("%+.1f", kmt_response),
                vjust = ifelse(kmt_response >= 0, -0.4, 1.3)),
            size = 2.7, colour = "grey25") +
  facet_wrap(~ YEAR) +
  scale_fill_manual(values = pal_identity) +
  labs(title = "Does pan-Blue media raise KMT support at every identity level?",
       subtitle = "Above zero = persuasion. Below zero = backfire.",
       x = "National identity",
       y = "KMT share under do(Blue) minus do(Green), pp",
       caption = paste("The US analysis found monotonic effects throughout:",
                       "left-leaning media raised Democratic support at every\nengagement",
                       "level. Kao (2026) documents experimental backfire among",
                       "PRC-skeptics. Negative bars among strong-Taiwanese-identity\nrespondents",
                       "would be a genuine cross-national divergence rather than a",
                       "replication.")) +
  theme_teds() + theme(legend.position = "none")

save_fig(p_backfire, "fig11_backfire", w = 9, h = 5)

################################################################################
## FIG 12: COEFFICIENT COMPARISON
################################################################################

p_coef <- coef_compare |>
  dplyr::filter(term != "(Intercept)") |>
  ggplot(aes(estimate, term, colour = source)) +
  geom_vline(xintercept = 0, linetype = "22", colour = "grey55") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.55), size = 0.3) +
  facet_wrap(~ YEAR) +
  scale_colour_manual(values = pal_source) +
  labs(title = "Inferential utility: DPP vote model on observed vs synthetic data",
       subtitle = "Overlapping intervals indicate preserved inference",
       x = "Estimate (log-odds)", y = NULL) +
  theme_teds() + theme(axis.text.y = element_text(size = 7))

save_fig(p_coef, "fig12_coefficients", w = 10, h = 7)

################################################################################
## FIG 13: SENSITIVITY -- DAG without direct China-factor edges
################################################################################
## Excluding the Identity/Tondu -> VoteChoice edges forces all China-factor
## influence through the media pathway. The gap is the amount by which the
## media effect would be overstated under that specification.

p_sens <- sensitivity |>
  select(YEAR, range_dpp_full, range_dpp_nodirect) |>
  pivot_longer(-YEAR, names_to = "spec", values_to = "range") |>
  mutate(spec = recode(spec,
                       range_dpp_full     = "Full DAG",
                       range_dpp_nodirect = "No direct China-factor edges")) |>
  ggplot(aes(factor(YEAR), range, fill = spec)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f", range)),
            position = position_dodge(0.7), vjust = -0.4,
            size = 2.6, colour = "grey30") +
  scale_fill_manual(values = c("Full DAG" = "#1F5FA9",
                               "No direct China-factor edges" = "#C4522E")) +
  scale_y_continuous(expand = expansion(c(0, 0.16))) +
  labs(title = "Sensitivity of the media effect to DAG specification",
       subtitle = "Dropping the direct Identity and Tondu edges to vote choice",
       x = NULL, y = "Range of DPP share across media regimes (pp)",
       caption = paste("Excluding these edges forces all China-factor influence",
                       "through the media pathway. The difference between the bars\nis the",
                       "amount by which the estimated media effect would be overstated",
                       "under that specification -- which is why the edges are\nincluded in",
                       "the main model and this comparison is reported.")) +
  theme_teds()

save_fig(p_sens, "fig13_sensitivity_dag", w = 8.5, h = 5)

################################################################################
## FIG 14: US / TAIWAN COMPARISON
################################################################################
## Fill the Taiwan rows from causal_effects once results are in.

tw_mob <- causal_effects |> dplyr::filter(regime_var == "MediaExposure")
comparison <- tibble::tribble(
  ~country,        ~metric,             ~lo,  ~hi,
  "United States", "Mobilization (pp)", -2.0,  2.0,
  "United States", "Persuasion (pp)",    2.0,  4.0,
  "Taiwan",        "Mobilization (pp)",
     round(min(tw_mob$d_turnout), 1), round(max(tw_mob$d_turnout), 1),
  "Taiwan",        "Persuasion (pp)",
     round(min(tw_mob$d_dpp), 1),     round(max(tw_mob$d_dpp), 1)
) |> mutate(mid = (lo + hi) / 2)

p_compare <- ggplot(comparison, aes(mid, metric, colour = country)) +
  geom_vline(xintercept = 0, linetype = "22", colour = "grey55") +
  geom_pointrange(aes(xmin = lo, xmax = hi),
                  position = position_dodge(0.45), size = 0.5) +
  scale_colour_manual(values = c("United States" = "#2D3142",
                                 "Taiwan" = "#0B7A3B")) +
  labs(title = "Media effects: United States and Taiwan",
       subtitle = "Identical method, different political context",
       x = "Effect size (percentage points)", y = NULL,
       caption = paste("Method held constant: same DAG logic, same LCA procedure,",
                       "same validation suite, same estimands. Differences are\nattributable",
                       "to context rather than method. Kao (2026) predicts larger Taiwanese",
                       "persuasion effects, since partisan identities\nthere remain more",
                       "fluid than in established two-party democracies.")) +
  theme_teds()

save_fig(p_compare, "fig14_us_taiwan_comparison", w = 8.5, h = 4.2)

################################################################################
## COMPOSITE PANELS FOR SLIDES
################################################################################

save_fig((p_vote | p_roc) / (p_joint | p_coef) +
  plot_annotation(title = "Validation suite: TEDS synthetic data",
    theme = theme(plot.title = element_text(face = "bold", size = 14))),
  "panel_validation", w = 15, h = 10)

save_fig((p_causal_media | p_causal_id | p_causal_td) +
  plot_annotation(title = "Causal effects: media and China-factor interventions",
    subtitle = "The identity and tondu interventions have no US counterpart",
    theme = theme(plot.title = element_text(face = "bold", size = 14))),
  "panel_causal", w = 17, h = 8)

message("Figures written to ", FIG_DIR)
