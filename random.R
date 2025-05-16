
par(mfrow = c(2,2))
plot(fitted(m), residuals(m), main = "Fitted vs Residuals")
plot(data_sp$kelp_annual_100, residuals(m), main = "kelp_annual_100", add = T)
plot(data_sp$depth_mean_500, residuals(m), main = "depth_mean_500")
plot(data_sp$age_at_survey, residuals(m), main = "age_at_survey")
boxplot(residuals(m) ~ data_sp$site_type, main = "Residuals by Site Type")

data_sp %>%
  mutate(resid = residuals(m)) %>%
  group_by(site_type) %>%
  summarise(var_resid = var(resid, na.rm = TRUE))

par(mfrow = c(2, 2))  # 2 rows, 2 columns

plot(fitted(m), residuals(m), main = "Fitted vs Residuals")
plot(data_sp$kelp_annual_100, residuals(m), main = "kelp_annual_100")
plot(data_sp$depth_mean_500, residuals(m), main = "depth_mean_500")
plot(data_sp$age_at_survey, residuals(m), main = "age_at_survey")

hist(data_sp$kelp_annual_100)
hist(log1p(data_sp$kelp_annual_100))

ggplot(data_sp, aes(x = kelp_annual_100, y = residuals(m))) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ region4) +
  labs(y = "Model residuals", x = "Kelp cover (100m buffer)") +
  theme_minimal()


m4 <- lmer(log_c_biomass ~ hard_bottom_250 + depth_mean_500 + depth_cv_500 * site_type * age_at_survey + (1 | affiliated_mpa/site) + (1|year), data = data_sp)
plot(m4)
summary(m4)
model.sel(m, m4)
plot(allEffects(m4), multiline = T, confint = list(style = 'auto'))



# Create a function to identify too-close site pairs per year
flag_close_sites <- function(year_df, year) {
  if (nrow(year_df) < 2) return(NULL)
  
  coords <- st_coordinates(year_df)
  rownames(coords) <- year_df$site
  dists <- as.matrix(dist(coords))
  
  dist_df <- as.data.frame(as.table(dists)) %>%
    rename(site1 = Var1, site2 = Var2, distance = Freq) %>%
    filter(site1 != site2) %>%
    filter(distance < 1000) %>%
    mutate(year = year)
  
  return(dist_df)
}

site_info <- data_sp %>% 
  distinct(site, site_type, affiliated_mpa)

# Group by year and identify problematic site pairs
close_site_pairs <- data_sf %>%
  filter(!is.na(resid)) %>%
  group_by(year) %>%
  group_map(~ flag_close_sites(st_as_sf(.x), .y)) %>%
  bind_rows()

close_site_pairs2 <- close_site_pairs %>% 
  left_join(site_info, by = c("site1" = "site")) %>% 
  left_join(site_info, by = c("site2" = "site"))

# View results
close_site_pairs


data_sites <- data_rock %>% 
  dplyr::select(site:affiliated_mpa, depth_cv_100:hard_bottom_500) %>% distinct()

data_sites_kelp <- data_rock %>% 
  dplyr::select(year, site:affiliated_mpa, contains("kelp")) %>% distinct()

m <- lmer(hard_bottom_250 ~ site_type + (1 | affiliated_mpa), data = data_sites)
summary(m)

m2 <- lmer(depth_mean_500 ~ site_type + (1 | affiliated_mpa), data = data_sites)
summary(m2) # yes significant

m3 <- lmer(depth_cv_500 ~ site_type + (1 | affiliated_mpa), data = data_sites)
summary(m3) # not significant

m4 <- lmer(kelp_annual_250 ~ site_type * region4 + (1 | affiliated_mpa) , data = data_sites_kelp)
summary(m4) # not significant; differences among regions but not differences between MPAs/REFs.

