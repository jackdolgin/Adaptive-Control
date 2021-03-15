diff_comparison_layer <- function(comparison) {
  
  if (comparison == "diff from 0") {
    third_group <- as.name("Bias")
    term_filter <- "Don't Filter Me"
    
    bracket_inner_x <- 1.13
    bracket_width <- .04
    star_dist <- -.17
    group_head <- 2L
    
  } else if (comparison == "diff from fellow congr") {
    third_group <- NULL
    term_filter <- "Intercept"
    
    bracket_inner_x <- .86
    bracket_width <- -.07
    star_dist <- .09
    group_head <- 4L
  }
  
  bracket_outer_x <- bracket_inner_x + bracket_width
  
  bracket_df <- finished_df %>%
    group_by(Task, Congruency, !!third_group) %>%
    group_modify(~ regress_both_tasks(.x, .y, "RT_diff")) %>%
    ungroup() %>%
    filter(!str_detect(term, term_filter),
           p.value < .05) %>%
    mutate(across(p.value, stars.pval)) %>%
    inner_join(finished_df) %>%
    group_by(Task, Congruency, p.value, Bias) %>%
    summarise(across(RT_diff, median),
              x_val = c(bracket_inner_x,
                        bracket_outer_x,
                        bracket_outer_x,
                        bracket_inner_x),
              .groups = "drop_last") %>%
    mutate(
      across(RT_diff, ~if_else(
        comparison == "diff from 0" & Bias == "Congruent", 0L, .)),
      RT_median = median(RT_diff)) %>%
    filter(! between(row_number(), 3L, 6L))
  
  stars_df <- bracket_df %>%
    filter(row_number() == 1L) %>%
    mutate(across(x_val, ~ . - star_dist))
  
  dash_df <- mutate(bracket_df,
                    across(x_val, ~if_else(
                      between(row_number(), 2L, 3L),
                      . + .1,
                      . + .25)))
  
  
  dash_df_1 <- filter(dash_df, row_number() < 3L)
  dash_df_2 <- filter(dash_df, row_number() >= 3L)
  
  list(
    geom_path(bracket_df,
              mapping = aes(x_val, RT_diff),
              inherit.aes = FALSE),
    geom_text(stars_df,
              mapping = aes(x_val, RT_median, label = p.value),
              inherit.aes = FALSE,
              hjust = "right"),
    geom_line(dash_df_1,
              mapping = aes(x_val, RT_diff),
              inherit.aes = FALSE,
              linetype = "dotted"),
    geom_line(dash_df_2,
              mapping = aes(x_val, RT_diff),
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
          160L))
    )
  }
  
  ggdraw(finished_df %>%
           ggplot(aes(x = NA,
                      y = RT_diff,
                      fill = Bias)) +
           geom_split_violin(alpha = .5,
                             show.legend = FALSE) + 
           # geom_hline(yintercept = 0, colour = "gray100") +
           diff_comparison_layer("diff from 0") + 
           diff_comparison_layer("diff from fellow congr") +
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
                      nrow = 1L,
                      strip.position = "bottom") +
           theme_minimal() +
           theme(
             legend.spacing = unit(1L, "cm"),
             panel.grid.major.x = element_blank(),
             # panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
             #                                 colour = "grey"), 
             axis.text.x = element_blank(),
             axis.title.x = element_blank(),
             strip.text.x = element_blank(),
             plot.title = element_text(family = "sans",
                                       vjust = 1,
                                       size = 18,
                                       margin = margin(0L, 0L, 15L, 0L)),
             plot.caption = element_text(vjust = -30L),
             plot.margin = unit(c(10L, 10L, 70L, 10L), "pt")) +
           guides(
             fill = guide_legend("Block/Location Bias"),
             color = guide_legend("Trial")) +
           ylab("Response Time (compared to control condition, in seconds)") +
           extra_labels
  ) +
    draw_label("Predictive Blocks", 0.24, y_header) +
    draw_label("Predictive Locations", .64, y_header) +
    draw_label("Congruent", .145, y_footer, size = 12L) +
    draw_label("Incongruent", .35, y_footer, size = 12L) +
    draw_label("Congruent", .55, y_footer, size = 12L) +
    draw_label("Incongruent", .76, y_footer, size = 12L) +
    draw_label("Trial Congruency", .45, y_footer - .04) +
    draw_line(c(.1, .4), y_footer + .03) +
    draw_line(c(.5, .8), y_footer + .03)
}
