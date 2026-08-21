# Original schematic of a climate tipping transition.
# Following the tipping-point definition of Lenton et al. (2008).
# Not a reproduction of any figure in the paper.

library(ggplot2)
library(svglite)

## ---- define the landscape curve ------------------------------------------
curve_fun <- function(x, a = 2.2, b = 0.45) x^4 / 4 - a * x^2 / 2 - b * x

x_vals <- seq(-2.2, 2.3, length.out = 500)
curve_df <- data.frame(x = x_vals, F = curve_fun(x_vals))

## ---- key points on the curve -----------------------------------------------
# State A: shallow local minimum (left)
# hump: local maximum, treated as rho_crit
# State B: deeper local minimum (right)
state_A <- data.frame(x = -1.37, F = curve_fun(-1.37))
hump    <- data.frame(x = -0.207, F = curve_fun(-0.207))
state_B <- data.frame(x =  1.58, F = curve_fun(1.58))

rho_crit   <- hump$x
rho_delta  <- rho_crit + 1.0        # rho_crit + delta_rho, spaced out for visibility
gap_top    <- hump$F                # extrapolated continuation height
gap_bottom <- curve_fun(rho_delta)  # actual height after crossing threshold

## ---- main curve -------------------------------------------------------------
p <- ggplot(curve_df, aes(x = x, y = F)) +
  geom_line(color = "steelblue", linewidth = 1.1) +
  
  # rho_crit: threshold line
  geom_vline(xintercept = rho_crit, linetype = "dashed", color = "firebrick") +
  annotate("text", x = rho_crit, y = 1.15, label = expression(rho[crit]),
           color = "firebrick", size = 4) +
  
  # extrapolated continuation (dotted reference line)
  annotate("segment", x = rho_crit, xend = 2.0, y = gap_top, yend = gap_top,
           linetype = "dotted", color = "grey40") +
  
  # actual gap (F-hat): qualitative change after threshold
  annotate("segment", x = rho_delta, xend = rho_delta, y = gap_top, yend = gap_bottom,
           arrow = arrow(ends = "both", length = unit(0.1, "inches")),
           color = "firebrick", linewidth = 0.7) +
  annotate("text", x = rho_delta + 0.15, y = (gap_top + gap_bottom) / 2,
           label = expression(hat(F)*": qualitative change"),
           color = "firebrick", size = 3.5, hjust = 0) +
  
  # delta_rho: perturbation beyond threshold
  annotate("segment", x = rho_crit, xend = rho_delta * 0.55, y = -2.35, yend = -2.35,
           arrow = arrow(length = unit(0.08, "inches")), color = "grey30") +
  annotate("text", x = (rho_crit + rho_delta * 0.55) / 2, y = -2.6,
           label = expression(delta*rho), size = 4) +
  
  # external forcing direction
  annotate("segment", x = -2.0, xend = -0.5, y = 2.0, yend = 2.0,
           arrow = arrow(length = unit(0.1, "inches")), color = "grey30") +
  annotate("text", x = -2.0, y = 2.3,
           label = expression("Increasing " * rho * " (external forcing)"),
           color = "grey30", size = 3.5, hjust = 0) +
  
  # State A / State B labels
  geom_point(data = state_A, aes(x = x, y = F), color = "steelblue", size = 3) +
  annotate("text", x = state_A$x, y = state_A$F - 0.55,
           label = "State A\n(current stable state)", size = 3.5, fontface = "bold",
           color = "steelblue") +
  
  geom_point(data = state_B, aes(x = x, y = F), color = "steelblue", size = 3) +
  annotate("text", x = state_B$x, y = state_B$F - 0.55,
           label = "State B\n(new stable state)", size = 3.5, fontface = "bold",
           color = "steelblue") +
  
  coord_cartesian(ylim = c(-3.0, 2.6), xlim = c(-2.2, 2.5)) +
  
  labs(
    title = "Schematic of a Climate Tipping Transition",
    subtitle = "Following the tipping-point definition of Lenton et al. (2008)",
    x = expression(rho ~ "(control parameter)"),
    y = expression("System feature " * F),
    caption = "Concept: Lenton et al. (2008). Diagram independently drawn for this report."
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_blank())

## ---- save --------------------------------------------------------------------
# SVG for HTML; PDF for LaTeX (same basename, as required for bookdown)
ggsave("work/09-tp-overview/figures/tp_tipping_point_schematic.svg", p,
       width = 8, height = 6, device = svglite)
ggsave("work/09-tp-overview/figures/tp_tipping_point_schematic.pdf", p,
       width = 8, height = 6)
