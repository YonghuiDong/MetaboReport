# Shared Nature Publishing Group-style visual defaults ------------------------

nature_colors <- c(
  "#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F",
  "#8491B4", "#91D1C2", "#DC0000", "#7E6148", "#B09C85"
)

nature_palette <- function(n) {
  if (n <= length(nature_colors)) return(nature_colors[seq_len(n)])
  grDevices::colorRampPalette(nature_colors)(n)
}

treatment_colors <- c(
  "Before Data Treatment" = "#8491B4",
  "After Data Treatment" = "#E64B35"
)

plot_palette_choices <- c(
  "Nature (Default)" = "Default",
  "Accent" = "Accent",
  "Dark2" = "Dark2",
  "Paired" = "Paired",
  "Pastel2" = "Pastel2",
  "Pastel1" = "Pastel1",
  "Set1" = "Set1",
  "Set2" = "Set2",
  "Set3" = "Set3"
)
