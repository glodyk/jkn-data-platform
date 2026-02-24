library(dplyr)

data_test = read.csv(file.choose())
str(data_test)

ggplot(data = data.pareto1,aes(x = vend_name)) +
  geom_rect(aes(
    xmin = vend_name[c(1)], xmax = vend_name[c(11)],
    ymin = -Inf, ymax = Inf, #kumulatif[c(13)],
    fill = "red" , alpha = 0.01)) +
  geom_rect(aes(
    xmin = vend_name[c(11)], xmax = vend_name[c(27)],
    ymin = -Inf, ymax = Inf,
    fill = "yellow", alpha = 0.01)) +
  geom_rect(aes(
    xmin = vend_name[c(27)], xmax = vend_name[c(67)],
    ymin = -Inf, ymax = Inf,
    fill = "green", alpha = 0.01)) +
  geom_col(aes(y = total)) +
  geom_line(aes(y = pct_cuml_rescaled, group = 1)) +
  geom_point(aes(y = pct_cuml_rescaled)) +
  geom_text(aes(y = pct_cuml_rescaled, label = percent(kumulatif_persen)), nudge_y = 5) +
  theme(axis.text.x = element_text(angle = 90))