# Create hexagon sticker for `synthReturn` package

# synthetic control example data
sticker_data <- tibble(
      time = seq(1,30,1),
      Y = c(15.751021,13.682886,13.093164,20.571018,8.328278,19.193626,4.832634,12.172515,12.939626,12.136155,13.572400,9.646646,10.268429,9.267731,8.848573,11.316525,20.504881,17.454156,14.009639,15.827821,9.899535,7.013582,16.930958,11.610731,11.604536,21.860465,13.611166,8.470333,24.494189,20.861163),
      label = rep("treated", 30)
    ) %>%
  bind_rows(
   tibble(
      time = seq(1,30,1),
      Y = c(15.3588608,13.4063383,13.3685569,20.1298171,9.2178729,18.5997346,4.3038852,12.0009463,12.3287938,11.9655572,13.8442919,9.5518034,10.9204051,8.6940448,9.3182591,11.3942918,20.6466658,17.6112568,14.9252137,15.8311296,8.6635733,3.3833177,13.2187805,8.1439732,5.8644037,16.5804308,5.1746817,0.6304315,15.0390748,11.2226532),
      label = rep("control", 30)
    )
  )


# GG Plot (used as subplot in sticker)
sticker_gg <- sticker_data %>%
  filter(time >= 15 & time <= 24) %>%
  ggplot(aes(x=time, y=Y, group=label, color=label, linetype = label)) +
  geom_line(size = 1.5, alpha = 1) +
  geom_vline(xintercept = 20, colour = "black", linetype = "dashed", size = 1) +
  scale_color_manual(
    values = c("control" = "grey30", "treated" = "black")
  ) +
  scale_linetype_manual(
    values = c("control" = "solid", "treated" = "solid")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
  )


sticker(
  sticker_gg,
  package="synthReturn",
  s_x=0.9, s_y=.8, s_width=1.5, s_height=1,
  p_size=22,
  h_fill="steelblue",
  h_color="black",
  filename = "inst/figures/synthReturn.png"
  )


