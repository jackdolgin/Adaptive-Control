t_test_samples_layer <- function(samples) {
  
  extra_column <- "Bias"
  extra_main_effect <- NULL
  
  if (samples == 1) {
    bracket_inner_x <- 1.13
    bracket_width <- .04
    star_dist <- -.17
    group_head <- 2
  } else {
    extra_column %<->% extra_main_effect
    bracket_inner_x <- .86
    bracket_width <- -.07
    star_dist <- .09
    group_head <- 4
  }
  
  bracket_outer_x <- bracket_inner_x + bracket_width
  
  bracket_df <- predictive_df_prevRT_filtered %>%
    grouped_tidy(
      grouping.vars = 
        c(Task, Congruency, all_of(extra_column)),
      ..f = lmer,
      formula = 
        paste0("RT ~ ", extra_main_effect, " + (1 | Sub_Code)"),
      control = lmerControl(optimizer = "bobyqa")
    ) %>%
    filter(!is.na(p.value),
           is.na(lead(p.value)),
           p.value < .05) %>%
    mutate(across(p.value, stars.pval)) %>%
    inner_join(predictive_df_prevRT_filtered) %>%
    group_by(Task, Congruency, p.value, Bias) %>%
    summarise(across(RT, median),
              x_val = c(bracket_inner_x,
                        bracket_outer_x,
                        bracket_outer_x,
                        bracket_inner_x),
              .groups = "drop_last") %>%
    mutate(
      across(RT, ~if_else(
        is.null(extra_main_effect) & Bias == "Congruent",
        0, .)),
      RT_median = median(RT)) %>%
    filter(! between(row_number(), 3, 6))
  
  stars_df <- bracket_df %>%
    filter(row_number() == 1) %>%
    mutate(across(x_val, ~. - star_dist))
  
  dash_df <- mutate(bracket_df,
                    across(x_val, ~if_else(
                      between(row_number(), 2, 3),
                      . + .1,
                      . + .25)))
  
  dash_df_1 <- filter(dash_df, row_number() < 3)
  dash_df_2 <- filter(dash_df, row_number() >= 3)
  
  list(
    geom_path(bracket_df,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE),
    geom_text(stars_df,
              mapping = aes(x_val, RT_median, label = p.value),
              inherit.aes = FALSE,
              hjust = "right"),
    geom_line(dash_df_1,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE,
              linetype = "dotted"),
    geom_line(dash_df_2,
              mapping = aes(x_val, RT),
              inherit.aes = FALSE,
              linetype = "dotted")
  ) %>%
    head(group_head)
}


split_violin_builder <- function(medium) {
  if(medium == "manuscript") {
    y_header = .98
    y_footer = .1
    extra_labels = list()
  } else {
    y_header = .89
    y_footer = .13
    extra_labels = list(
      labs(
        title = 
          "Response Time as a Function of Trial and Contextual Congruencies",
        subtitle = paste(
          "Congruency was paired with a contextual feature, either block",
          "number or stimulus location, depending on the task\n"),
        caption = str_wrap(
          "Each of the values that compose each of the vertical
          'raincloud'/violin plots is for a given image that was
          presented to a given participant. Specifically, for
          that image, it is a given participant's response time
          minus the average response time when that image was
          presented in the neutral condition.",
          160))
    )
  }
  
  ggdraw(predictive_tasks_df %>%
           ggplot(aes(x = NA,
                      y = RT,
                      fill = Bias)) +
           geom_split_violin(alpha = .5,
                             show.legend = FALSE) + 
           # geom_hline(yintercept = 0, colour = "gray100") +
           t_test_samples_layer(1) + 
           t_test_samples_layer(2) +
           geom_boxplot(aes(color = Congruency),
                        width = .2,
                        position = position_dodge(.25),
                        outlier.shape = NA,
                        alpha = .3,
                        size = .5) +
           scale_fill_manual(values = c("#D55E00",
                                        "#0072B2")) +
           scale_color_manual(values = c("#D55E00",
                                         "#0072B2")) +
           facet_wrap(Task ~ Congruency,
                      nrow = 1,
                      strip.position = "bottom") +
           theme_minimal() +
           theme(
             legend.spacing = unit(1, "cm"),
             panel.grid.major.x = element_blank(),
             # panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
             #                                 colour = "grey"), 
             axis.text.x = element_blank(),
             axis.title.x = element_blank(),
             strip.text.x = element_blank(),
             plot.title = element_text(family = "sans",
                                       vjust = 1,
                                       size = 18,
                                       margin = margin(0 , 0, 15, 0)),
             plot.caption = element_text(vjust = -30),
             plot.margin = unit(c(10, 10, 70, 10), "pt")) +
           guides(
             fill = guide_legend("Block/Location Bias"),
             color = guide_legend("Trial")) +
           ylab("Response Time (compared to control condition, in seconds)") +
           extra_labels
  ) +
    draw_label("Predictive Blocks", 0.24, y_header) +
    draw_label("Predictive Locations", .64, y_header) +
    draw_label("Congruent", .145, y_footer, size = 12) +
    draw_label("Incongruent", .35, y_footer, size = 12) +
    draw_label("Congruent", .55, y_footer, size = 12) +
    draw_label("Incongruent", .76, y_footer, size = 12) +
    draw_label("Trial Congruency", .45, y_footer - .04) +
    draw_line(c(.1, .4), y_footer + .03) +
    draw_line(c(.5, .8), y_footer + .03)
}
