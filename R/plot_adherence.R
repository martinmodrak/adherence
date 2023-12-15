scale_colour_cohort <- ggplot2::scale_color_manual(values = c("2020" = "black", "2018" = "#DD4803"))
scale_fill_cohort <- ggplot2::scale_fill_manual(values = c("2020" = "black", "2018" = "#DD4803"))
scale_shape_cohort <- ggplot2::scale_shape_manual(values = c("2020" = 15, "2018" = 17))

plot_adherence_by_continuous <- function(data_long, by_val) {
  data_long %>%
    filter(has, !is.na(adherent_int), !is.na({{by_val}})) %>%
    group_by(drug_class, cohort) %>%
    filter(any(adherent_int == 0), any(!is.na({{by_val}}))) %>%
    ggplot(aes(y = adherent_int, x = {{by_val}}, group = 1)) + stat_smooth(method = "glm", method.args=list(family="binomial"), formula = y ~ x, color = "black") + geom_point(aes(color = cohort, shape = cohort), position = position_jitter(height = 0.1), alpha = 0.5) +
    scale_y_continuous("Proportion adherent", labels = scales::label_percent(),
                       breaks = c(0,0.5,1)) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3) )) +
    scale_colour_cohort +
    scale_shape_cohort +
    facet_wrap(~drug_class)
}

#
# plot_adherence_by_discrete <- function(data_long, by_val) {
#   data_long %>%
#     filter(has, !is.na({{by_val}})) %>%
#     group_by(drug_class) %>%
#     filter(any(adherent_int == 0)) %>%
#     mutate({{by_val}} := as_factor({{by_val}})) %>%
#     group_by(drug_class, {{by_val}}) %>%
#     summarise(yes = sum(adherent == "yes"), no = sum(adherent == "no"),
#               proportion = yes / (yes + no),
#               low95 = qbeta(0.025, yes + 1, no + 1),
#               high95 = qbeta(0.975, yes + 1, no + 1),
#               total = yes + no, .groups = "drop") %>%
#     ggplot(aes(x = {{by_val}}, y = proportion, ymin = low95, ymax = high95, label = total)) +
#     geom_linerange() + geom_point(size = 2) + geom_label(y = 1.2, size = 3) + expand_limits(y = c(0, 1.25)) +
#     scale_y_continuous("Proportion adherent", labels = scales::label_percent(),
#                        breaks = c(0,0.5,1)) +
#     facet_wrap(~drug_class)
# }

plot_adherence_by_discrete <- function(data_long, by_val) {

  my_pos <- position_dodge(width = 0.5)
  data_long %>%
    filter(has, !is.na({{by_val}})) %>%
    group_by(drug_class, cohort) %>%
    filter(any(adherent_int == 0)) %>%
    mutate({{by_val}} := as_factor({{by_val}})) %>%
    group_by(drug_class, cohort, {{by_val}}) %>%
    summarise(yes = sum(adherent == "yes"), no = sum(adherent == "no"),
              proportion = yes / (yes + no),
              low95 = qbeta(0.025, yes + 1, no + 1),
              high95 = qbeta(0.975, yes + 1, no + 1),
              text_y = if_else(unique(cohort) == "2020", 1.25, 1.65),
              total = yes + no, .groups = "drop") %>%
    ggplot(aes(x = {{by_val}}, y = proportion, ymin = low95, ymax = high95, label = total, color = cohort)) +
    geom_linerange(pos = my_pos) + geom_point(size = 2, pos = my_pos) + geom_label(aes(y = text_y), size = 3,pos = my_pos) + expand_limits(y = c(0, 1.8)) +
    scale_y_continuous("Proportion adherent", labels = scales::label_percent(),
                       breaks = c(0,0.5,1)) +
    scale_colour_cohort +
    scale_shape_cohort +
    facet_wrap(~drug_class)
}

plot_continuous_by_class_adherence <- function(data_long, drug_classes, value,
                                               value_title = NULL) {
  if(is.null(value_title)) {
    value_title <- rlang::as_name(rlang::enquo(value))
  }
  data_long %>% filter(drug_class %in% drug_classes, !is.na({{value}})) %>%
    mutate(adherent = fct_recode(adherent, "N/M" = "cannot_measure", "N/T" = "not_using"),
           adherent = fct_relevel(adherent, "no", "N/M", "N/T", "yes")) %>%
    ggplot(aes(x = adherent, y = {{value}}, color = cohort, shape = cohort)) +
    geom_boxplot(position = position_dodge(width = 1), width = 0.3, outlier.shape = NA) + geom_point(position = position_jitter(width = 0.25), alpha = 0.5) +
    scale_y_continuous(value_title) +
    scale_colour_cohort +
    scale_shape_cohort +
    guides(color = guide_legend(override.aes = list(alpha = 1) )) +
    facet_wrap(~drug_class)
  #scale_x_discrete(paste0("Adherent to ", drug_class))
}
