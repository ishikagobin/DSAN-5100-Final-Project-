library(tidyverse)

# ── Data ──────────────────────────────────────────────────────────────────────
# Each row = one precinct (20 per group pair × 2 phases = 120 rows)

pairs <- c(
  "Am. Indian/Alaska Native\nvs White",
  "Asian/Pacific Islander\nvs Black",
  "Black\nvs White"
)

make_dots <- function(phase, pair_label, n_over, n_under) {
  n_ns <- 20 - n_over - n_under
  tibble(
    phase = phase,
    pair  = factor(pair_label, levels = pairs),
    precinct = 1:20,
    outcome = factor(
      c(rep("Over", n_over), rep("Under", n_under), rep("NS", n_ns)),
      levels = c("Over", "Under", "NS")
    )
  )
}

df <- bind_rows(
  make_dots("Before Patternizr", pairs[1], 9, 0),
  make_dots("Before Patternizr", pairs[2], 10, 0),
  make_dots("Before Patternizr", pairs[3], 0, 5),
  make_dots("After Patternizr",  pairs[1], 5, 0),
  make_dots("After Patternizr",  pairs[2], 0, 1),
  make_dots("After Patternizr",  pairs[3], 1, 1)
) %>%
  mutate(
    phase = factor(phase, levels = c("Before Patternizr", "After Patternizr")),
    # Grid positions: 10 columns × 2 rows of dots
    col = (precinct - 1) %% 10 + 1,
    row = (precinct - 1) %/% 10 + 1
  )

# ── Colors ────────────────────────────────────────────────────────────────────
dot_colors <- c(
  "Over"  = "#185FA5",
  "Under" = "#D85A30",
  "NS"    = "#D3D1C7"
)

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(df, aes(x = col, y = row, fill = outcome)) +
  geom_point(shape = 21, size = 4.5, color = "white", stroke = 0.4) +
  facet_grid(
    pair ~ phase,
    switch = "y"
  ) +
  scale_fill_manual(
    values = dot_colors,
    labels = c("Overrepresented", "Underrepresented", "Not significant"),
    name   = NULL
  ) +
  scale_y_reverse() +
  coord_equal() +
  labs(
    title    = "Significant differences: before vs. after Patternizr",
    subtitle = "Each dot represents 1 of 20 precincts tested per group pair",
    caption  = "Colored dots = precincts where group 1 is significantly over- or underrepresented relative to group 2"
  ) +
  theme_minimal(base_size = 12, base_family = "sans") +
  theme(
    # Strips
    strip.text.y.left   = element_text(angle = 0, hjust = 1, size = 10, face = "bold"),
    strip.text.x         = element_text(size = 11, face = "bold", margin = margin(b = 8)),
    strip.placement       = "outside",

    # Axes — hide them, the dots speak for themselves
    axis.text  = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),

    # Grid
    panel.grid = element_blank(),

    # Legend
    legend.position  = "top",
    legend.justification = "center",
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 10),

    # Title
    plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40",
                                 margin = margin(b = 12)),
    plot.caption  = element_text(size = 8, color = "gray50", hjust = 0.5,
                                 margin = margin(t = 10)),

    # Spacing
    panel.spacing.x = unit(1.5, "cm"),
    panel.spacing.y = unit(0.8, "cm"),
    plot.margin     = margin(15, 15, 10, 15)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave("patternizr_dot_matrix.png", p, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("patternizr_dot_matrix.pdf", p, width = 8, height = 5, bg = "white")

print(p)
