# plots.R
# Purpose: Generate reproducible actual vs forecast plots.

save_actual_forecast_plot <- function(plot_data, title, output_path) {
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Time_Index)) +
    ggplot2::geom_line(ggplot2::aes(y = Actual, linetype = "Actual"), linewidth = 0.8, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = Forecast, linetype = "Forecast"), linewidth = 0.8, na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      breaks = plot_data$Time_Index[seq(1, nrow(plot_data), by = 4)],
      labels = plot_data$Period[seq(1, nrow(plot_data), by = 4)]
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = title, x = "Period", y = "Number of trips", linetype = "Series") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  ggplot2::ggsave(output_path, p, width = 9, height = 5, dpi = 300)
  p
}

save_component_plot <- function(component_data, title, output_path) {
  p <- ggplot2::ggplot(component_data, ggplot2::aes(x = Time_Index, y = Value)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::facet_wrap(~ Component, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(
      breaks = component_data$Time_Index[seq(1, nrow(component_data), by = 8)],
      labels = component_data$Period[seq(1, nrow(component_data), by = 8)]
    ) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = title, x = "Period", y = "Value") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  ggplot2::ggsave(output_path, p, width = 9, height = 7, dpi = 300)
  p
}
