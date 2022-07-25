effects_data <- vl_data |> 
  group_by(accion) |> 
  arrange(fecha) |> 
  mutate(n = 1:n()) |> 
  filter(n <= 30) 

effect_plot <- ggplot(data = effects_data, aes(x = n, y = margen_bruto)) +
  geom_line(aes(group = accion), alpha = 0.4) +
  geom_point(data = bind_rows(
    effects_data |> filter(n == 1),
    effects_data |> filter(n == max(n))
  )) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 1)) +
  geom_smooth(method = "lm", formula = "y ~ x", color = "firebrick") +
  labs(
    x = "Trimestre",
    y = "Margen bruto",
    title = "Efecto del grupo sobre la regresión"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = here("Plots/effects_plot.png"),
  plot = effect_plot,
  dpi = 300,
  device = ragg::agg_png,
  width = 7,
  height = 7
)


ct_margen_plot <- ggplot(data = vl_data, aes(x = CT_neto, margen_bruto)) +
  geom_point(shape = 16, alpha = 0.4, size = 3) + 
  geom_smooth(method = "loess", formula = "y ~ x", color = "firebrick") +
  labs(
    title = "Capital de Trabajo vs Margen Bruto",
    x = "Capital de trabajo neto",
    y = "Margen bruto"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = here("Plots/CT_margen_bruto.png"),
  plot = ct_margen_plot,
  dpi = 300,
  device = ragg::agg_png,
  width = 7,
  height = 7
)

quadratic_plot <- ggplot(data = data.frame(x = c(-1, 1)), aes(x = x)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 1, linetype = 3) +
  geom_function(
    fun = \(x) (-0.14443 * x + 0.29669 * x ^ 2),
    aes(color = "Interceptos aleatorios")
  ) +
  geom_function(
    fun = \(x) (-0.22001 * x + 0.21527 * x ^ 2),
    aes(color = "Pendientes e interceptos aleatorios"),
    linetype = 2
  ) +
  geom_point(
    data = data.frame(x = 0.49, y = 0),
    aes(x = x, y = y),
    size = 3,
    shape = 4,
    stroke = 2,
    color = "firebrick"
  ) +
  geom_point(
    data = data.frame(x = 1.02, y = 0),
    aes(x = x, y = y),
    size = 3,
    shape = 4,
    stroke = 2,
    color = "gray40"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("firebrick", "gray40")) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.6, 0.8),
    legend.background = element_rect(fill = "transparent")
  ) +
  labs(
    title = "¿Cuánto CT conduce a un impacto positivo\nen el Margen Bruto?",
    x = "Capital de trabajo neto",
    y = "Margen bruto",
    color = NULL
  )

ggsave(
  filename = here("Plots/quadratic_plot.png"),
  plot = quadratic_plot,
  dpi = 300,
  device = ragg::agg_png,
  width = 7,
  height = 7
)
