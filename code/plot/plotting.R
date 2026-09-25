library(ggplot2)
scale_factor <- max(outputs$tonne_mile_trucking_increase / 1e9)

ggplot(outputs, aes(reorder(states, tonne_mile_trucking_increase))) +
  geom_col(aes(y = tonne_mile_trucking_increase / 1e9, fill = "Tonne-mile increase (billions)")) +
  geom_line(
    aes(y = (1 - percent_of_demand_satisfied) * scale_factor, 
        group = 1,
        color = "% demand satisfied"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = (1 - trucking_capacity) * scale_factor, 
        group = 1,
        color = "trucking capacity"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = (1 - shipping_capacity) * scale_factor, 
        group = 1,
        color = "shipping capacity"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = (1 - rail_capacity) * scale_factor, 
        group = 1,
        color = "rail capacity"),
    linewidth = 1
  ) +
  
  
  
  scale_y_continuous(
    "Increase in tonne-miles (billions)",
    sec.axis = sec_axis(~ . / scale_factor * 100, "", 
                        labels = \(x) paste0(x, "%"))
  ) +
  scale_fill_manual(values = c("Tonne-mile increase (billions)" = "grey35")) +
  scale_color_manual(values = c(
    "% demand satisfied" = "steelblue",
    "trucking capacity"    = "red",
    "shipping capacity"    = "purple",
    "rail capacity"        = "green"
  )) +
  labs(x = "State", fill = NULL, color = NULL, title = "Shock to ships without Jones Act") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right"
  )