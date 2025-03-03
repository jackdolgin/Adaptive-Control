pacman::p_load(gtools, tidyverse, cowplot, latex2exp)
devtools::source_gist("746685f5613e01ba820a31e57f87ec87")

diff_comparison_layer <- function(comparison) {
  
  if (comparison == "diff from 0") {
    third_group <- as.name("Bias")
    term_filter <- "Don't Filter Me"
    
    star_hjust <- "left"
    bracket_inner_x <- 1.13
    bracket_width <- .04
    star_dist <- -.07
    group_head <- 2L
    
  } else if (comparison == "diff from fellow congr") {
    third_group <- NULL
    term_filter <- "Intercept"
    
    star_hjust <- "right"
    bracket_inner_x <- .86
    bracket_width <- -.07
    star_dist <- .09
    group_head <- 4L
  }
  
  bracket_outer_x <- bracket_inner_x + bracket_width
  
  bracket_df <- finished_df_slight_name_change %>%
    group_by(Task, Congruency, !!third_group) %>%
    group_modify(~ regress_both_tasks(.x, .y, "RT_diff", "difference")) %>%
    ungroup() %>%
    filter(!str_detect(term, term_filter),
           p.value < .05)
  
  if (nrow(bracket_df) == 0) {
    NULL
  } else{
    bracket_df <- bracket_df %>%
      mutate(across(p.value, stars.pval)) %>%
      inner_join(finished_df_slight_name_change) %>%
      group_by(Task, Congruency, p.value, Bias) %>%
      summarise(across(RT_diff, median),
                x_val = c(bracket_inner_x,
                          bracket_outer_x,
                          bracket_outer_x,
                          bracket_inner_x)) %>%
      group_by(Task, Congruency) %>%
      mutate(
        across(RT_diff, ~if_else(
          comparison == "diff from 0" & Bias == "Mostly Congruent", 0, .)),
        RT_45thperc = quantile(RT_diff, .45, na.rm = TRUE)) %>%
      filter(! between(row_number(), 3L, 6L))
    
    stars_df <- bracket_df %>%
      slice_head %>%
      mutate(across(x_val, ~ . - star_dist))
    
    dash_df <- mutate(bracket_df,
                      across(x_val, ~if_else(
                        between(row_number(), 2L, 3L),
                        . + .1,
                        . + .25)))
    
    
    dash_df_1 <- slice_head(dash_df, n = 2L)
    dash_df_2 <- slice(dash_df, -(1:2))
    
    list(
      geom_path(bracket_df,
                mapping = aes(x_val, RT_diff),
                inherit.aes = FALSE),
      geom_text(stars_df,
                mapping = aes(x_val, RT_45thperc, label = p.value),
                inherit.aes = FALSE,
                hjust = star_hjust),
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
}


split_violin_builder <- function(medium) {
  
  finished_df_slight_name_change <<- mutate(
    finished_df,
    across(Bias, ~ paste("Mostly", Bias))
  )
  
  if(medium == "manuscript") {
    y_header = .98
    y_footer = .1
    cap_pos <- 1.6
    extra_labels = labs(caption = TeX(
      "*p<0.05; **p<0.01; ***p<0.001"
    ))
  } else {
    y_header = .89
    y_footer = .13
    cap_pos <- 1L
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
  
  ggdraw(finished_df_slight_name_change %>%
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
           scale_fill_manual(values = c("#D55E00", "#0072B2")) +
           scale_color_manual(values = c("#D55E00", "#0072B2")) +
           facet_wrap(Task ~ Congruency, nrow = 1L, strip.position = "bottom") +
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
             plot.caption = element_text(vjust = -30L, hjust = cap_pos),
             plot.margin = unit(c(10L, 10L, 70L, 10L), "pt")) +
           guides(
             fill = guide_legend("Block/Location Bias"),
             color = "none"
             ) +
           ylab("Response Time (compared to control condition, in ms)") +
           extra_labels
  ) +
    draw_label("LWPC Condition", 0.24, y_header) +
    draw_label("CSPC Condition", .64, y_header) +
    draw_label("Congruent", .145, y_footer, size = 12L) +
    draw_label("Incongruent", .35, y_footer, size = 12L) +
    draw_label("Congruent", .55, y_footer, size = 12L) +
    draw_label("Incongruent", .76, y_footer, size = 12L) +
    draw_label("Trial Congruency", .45, y_footer - .04) +
    draw_line(c(.1, .4), y_footer + .03) +
    draw_line(c(.5, .8), y_footer + .03)
}


# taken from https://stackoverflow.com/a/45614547
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, draw_group = function(self, data, ..., draw_quantiles = NULL){
  data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
  grp <- data[1,'group']
  newdata <- plyr::arrange(transform(data, x = if(grp%%2==1) xminv else xmaxv), if(grp%%2==1) y else -y)
  newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
  newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
  if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
    stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 
                                              1))
    quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
    aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
    aesthetics$alpha <- rep(1, nrow(quantiles))
    both <- cbind(quantiles, aesthetics)
    quantile_grob <- GeomPath$draw_panel(both, ...)
    ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
  }
  else {
    ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
  }
})
geom_split_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position, show.legend = show.legend, inherit.aes = inherit.aes, params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}