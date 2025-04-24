# Create hexagon sticker for `synthReturn` package with the awesome "hexSticker" library

# synthetic control example data
sticker_data <- rbind(
  data.table::data.table(
    time = 1:30,
    Y = c(15.751021,13.682886,13.093164,20.571018,8.328278,19.193626,4.832634,12.172515,12.939626,12.136155,13.572400,9.646646,10.268429,9.267731,8.848573,
      11.316525,20.504881,17.454156,14.009639,15.827821,9.899535,7.013582,16.930958,11.610731,11.604536,21.860465,13.611166,8.470333,24.494189,20.861163),
    label = rep.int("treated", 30L)
  ),
  data.table::data.table(
    time = 1:30,
    Y = c(15.3588608,13.4063383,13.3685569,20.1298171,9.2178729,18.5997346,4.3038852,12.0009463,12.3287938,11.9655572,13.8442919,9.5518034,10.9204051,
      8.6940448,9.3182591,11.3942918,20.6466658,17.6112568,14.9252137,15.8311296,8.6635733,3.3833177,13.2187805,8.1439732,5.8644037,16.5804308,5.1746817,
      0.6304315,15.0390748,11.2226532),
    label = rep.int("control", 30L)
  )
)

# GG Plot (used as subplot in sticker)
sticker_gg <- ggplot2::ggplot(sticker_data[time %between% c(15, 24)], ggplot2::aes(x = time, y = Y, group = label, color = label, linetype = label)) +
  ggplot2::geom_line(size = 1.5, alpha = 1) +
  ggplot2::geom_vline(xintercept = 20, colour = "black", linetype = "dashed", size = 1) +
  ggplot2::scale_color_manual(
    values = c("control" = "grey30", "treated" = "black")
  ) +
  ggplot2::scale_linetype_manual(
    values = c("control" = "solid", "treated" = "solid")
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    panel.grid = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_blank()
  )

hexSticker::sticker(
  sticker_gg,
  package = "synthReturn",
  s_x = 0.9, s_y = 0.8, s_width = 1.5, s_height = 1,
  p_size = 22,
  h_fill = "steelblue",
  h_color = "black",
  filename = "inst/figures/synthReturn.png"
)
