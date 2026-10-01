# Hexagon Logo Generator for morar ----
#
# Generates the package hexagon logo (man/figures/logo.png). The logo is a
# SAC amortization schedule: one bar per payment, with constant principal
# (teal) and falling interest (orange), clipped to a hexagon. It follows the
# ekioplot logo: EKIO colors, white separators, black border, Host Grotesk.
#
# Requirements:
#   - ggplot2, ggvmap, polyclip, showtext, sysfonts, ekioplot
#
# Usage: from the package root, source this script.

# ---- Packages ----
library(ggplot2)
library(ekioplot)

sysfonts::font_add_google("Host Grotesk", "Host Grotesk")
showtext::showtext_opts(dpi = 400)
showtext::showtext_auto()

# ---- Hexagon ----
hex <- as.data.frame(ggvmap::clip_hexagon())
names(hex) <- c("x", "y")

clip_to_hex <- function(xmin, xmax, ymin, ymax) {
  rect <- list(x = c(xmin, xmax, xmax, xmin), y = c(ymin, ymin, ymax, ymax))
  out <- polyclip::polyclip(rect, list(x = hex$x, y = hex$y))
  if (length(out) == 0) {
    return(NULL)
  }
  data.frame(x = out[[1]]$x, y = out[[1]]$y)
}

# ---- SAC bars ----
n_bars <- 7
edges <- seq(min(hex$x), max(hex$x), length.out = n_bars + 1)
principal <- 0.30
interest <- seq(0.30, 0.08, length.out = n_bars)

blocks <- list()
for (k in seq_len(n_bars)) {
  parts <- list(
    principal = c(0, principal),
    interest = c(principal, principal + interest[k])
  )
  for (part in names(parts)) {
    poly <- clip_to_hex(edges[k], edges[k + 1], parts[[part]][1], parts[[part]][2])
    if (!is.null(poly)) {
      poly$id <- paste(k, part)
      poly$part <- part
      blocks[[length(blocks) + 1]] <- poly
    }
  }
}
blocks <- do.call(rbind, blocks)

colors <- c(principal = ekio_pal("full")[[3]], interest = ekio_pal("full")[[2]])

# ---- Plot ----
logo <- ggplot() +
  geom_polygon(data = hex, aes(x, y), fill = ekio_pal("blue")[["700"]]) +
  geom_polygon(
    data = blocks,
    aes(x, y, group = id, fill = part),
    color = "#FFFFFF",
    linewidth = 1.6
  ) +
  geom_polygon(
    data = hex,
    aes(x, y),
    fill = NA,
    color = "#000000",
    linewidth = 1.6
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.77,
    label = "morar",
    family = "Host Grotesk",
    color = "#FFFFFF",
    size = 8.5
  ) +
  scale_fill_manual(values = colors) +
  coord_fixed(expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.position = "none",
    plot.margin = margin(4, 4, 4, 4)
  )

# ---- Save ----
# Same size as a hexSticker sticker
ggsave(
  "man/figures/logo.png",
  logo,
  width = 43.9,
  height = 50.8,
  units = "mm",
  dpi = 400,
  bg = "transparent"
)

cli::cli_alert_success("Hex logo created: man/figures/logo.png")
