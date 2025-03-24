# Predicted response plots


# library(ggeffects)
# library(purrr)
# library(ggplot2)
# library(patchwork)
# 
# my_theme <- theme(
#   plot.title = element_text(size = 10, face = "bold"),
#   plot.subtitle = element_text(size = 8),
#   axis.title = element_text(size = 8),
#   axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
#   axis.text.y = element_text(size = 8),
#   legend.title = element_text(size = 8),
#   legend.text = element_text(size = 8),
#   plot.caption = element_text(size = 8),
#   strip.text = element_text(size = 7, face = "bold"),
#   panel.background = element_rect(fill = "white", color = NA),  
#   plot.background = element_rect(fill = "white", color = NA)
# )
# 
# focal_model <- top_models$`K250*ST+DM25*ST+DCV100+ST*A`
# 
# terms <- data.frame(term = attr(terms(focal_model), 'term.labels')) %>% 
#   filter(!term %in% c("site_type", "age_at_survey")) %>% 
#   mutate(term =  str_replace(term, "^(.*):site_type$", "site_type:\\1")) %>% 
#   separate(term, into = c("var1", "var2"), sep = ":", fill = "left", remove = FALSE) %>%
#   clean_terms() %>% 
#   group_by(var2) %>% 
#   filter(n() == 1 | !is.na(var1)) %>% ungroup() %>%  
#   mutate(
#     gg_terms = if_else(
#       is.na(var1),
#       map(var2, ~ .x),
#       map2(var2, var1, ~ c(.x, .y))
#     )
#   )
# Note these are just predicted response plots not partial effects plots FML
# plots <- map2(terms$gg_terms, terms$term, ~ {
#   show_legend <- (.y == last(terms$term))
#   
#   pred <- ggpredict(focal_model, terms = .x)
#   
#   ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
#     geom_ribbon(aes(ymin = conf.low, ymax = conf.high), color = NA, alpha = 0.2) +
#     geom_line() +
#     scale_color_manual(values = c("Reference" = "#7e67f8", "MPA" = "#e5188b")) +
#     scale_fill_manual(values = c("Reference" = "#7e67f8", "MPA" = "#e5188b")) +
#     labs(
#       title = NULL,
#       x = str_replace_all(str_to_sentence(.x), "_", " "),
#       y = "Biomass (kg per 100m2)",
#       color = NULL, fill = NULL) +
#     scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
#     theme_minimal() +
#     my_theme +
#     theme(
#       legend.position = if (show_legend) "right" else "none",
#       legend.key = element_blank()
#     )
# })
# 
# wrap_plots(plots)