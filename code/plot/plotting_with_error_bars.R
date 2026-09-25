library(ggplot2)
inc_fac = 1e9
scale_factor <- max(outputs$tonne_mile_trucking_increase / inc_fac)

ggplot(outputs, aes(reorder(states, tonne_mile_trucking_increase))) +
  geom_col(aes(y = tonne_mile_trucking_increase / inc_fac, fill = "Tonne-mile increase (billions)")) +
  geom_errorbar(aes(y=tonne_mile_trucking_increase / inc_fac,ymin = tonne_mile_trucking_increase/inc_fac,ymax = (tonne_mile_trucking_increase2)/(inc_fac)),color = "black")+
  geom_line(
    aes(y = (1 - percent_of_demand_satisfied) * scale_factor, 
        group = 1,
        color = "% demand satisfied"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = (1 - percent_of_demand_satisfied2) * scale_factor, 
        group = 1,
        color = "% demand satisfied"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_line(
    aes(y = (1 - trucking_capacity) * scale_factor, 
        group = 1,
        color = "trucking capacity"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = (1 - trucking_capacity2) * scale_factor, 
        group = 1,
        color = "trucking capacity"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  geom_line(
    aes(y = (1 - shipping_capacity) * scale_factor, 
        group = 1,
        color = "shipping capacity"),
    linewidth = 1
  ) +
  
  geom_line(
    aes(y = (1 - shipping_capacity2) * scale_factor, 
        group = 1,
        color = "shipping capacity"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_line(
    aes(y = (1 - rail_capacity) * scale_factor, 
        group = 1,
        color = "rail capacity"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = (1 - rail_capacity2) * scale_factor, 
        group = 1,
        color = "rail capacity"),
    linewidth = 1,
    linetype = "dashed"
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
  labs(x = "State", fill = NULL, color = NULL, title = "Shock to Rail without Jones Act") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "right"
  )