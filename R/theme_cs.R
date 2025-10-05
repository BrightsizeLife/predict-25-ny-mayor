# ==============================================================================
# theme_cs.R
# Neon theme on black background for NYC Mayoral polling plots
# ==============================================================================

library(ggplot2)

#' Neon theme with black background and fainter grid
my_awesome_theme <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "#1ED760", linewidth = 0.15, alpha = 0.30),
      panel.grid.minor = element_line(color = "#1ED760", linewidth = 0.08, alpha = 0.20),
      text = element_text(color = "#39FF14", family = "mono"),
      axis.text = element_text(color = "#39FF14"),
      axis.title = element_text(color = "#39FF14", face = "bold"),
      plot.title = element_text(color = "#39FF14", face = "bold", size = 14),
      plot.subtitle = element_text(color = "#B0B0B0", size = 10),
      legend.background = element_rect(fill = "black", color = "#39FF14"),
      legend.text = element_text(color = "#39FF14"),
      legend.title = element_text(color = "#39FF14", face = "bold"),
      strip.background = element_rect(fill = "#1a1a1a", color = "#39FF14"),
      strip.text = element_text(color = "#39FF14", face = "bold")
    )
}

# Candidate color palette (neon accents)
# NOTE: mamdani changed from #39FF14 to #73FF6B to avoid matching grid color #1ED760
cs_palette <- c(
  mamdani = "#73FF6B",    # Neon green (lighter, distinct from grid)
  cuomo = "#D200FF",      # Neon magenta
  adams = "#E6FF00",      # Neon yellow
  sliwa = "#00E5FF",      # Neon cyan
  other = "#FF6B00",      # Neon orange
  undecided = "#B0B0B0"   # Gray
)

# Set as default theme
theme_set(my_awesome_theme())
