## Bootstrapped CIs for MPA RR across monitoring groups 
## Auhor: Joshua G. Smith, April 2022, following methods from Spiecker, B. 2021. 

#Requird libraries
require(googlesheets4)
require(plyr)
require(dplyr)
require(tidyr)
require(Rmisc)
require(boot)
require(broom)
require(waldo)
require(ggplot2)

#load data

data_path <- "/home/shares/ca-mpa/data/sync-data"
input_file <- "Ecol_perform_metrics_means.xlsx" 

means <- readxl::read_excel(file.path(data_path, input_file), 
                            sheet=1, na = c("NA", "."), trim_ws = T)

#============tidy data===========

means_wide <- means %>% 
  mutate(mpa_designation = if_else(mpa_designation %in% c("reference", "REF"), "REF", "MPA")) %>% 
  filter(!(group %in% c("rocky-bio", "rocky-LT"))) %>% 
  pivot_wider(id_cols = c(affiliated_mpa, group, mlpa_region, variable, indicator, year),
              names_from = mpa_designation,
              values_from = c(mean, sd, n)) 

# Drop data when MPA/reference pairs are not both surveyed in a given year
drop_data <- subset(means_wide, !is.na(mean_MPA))
drop_data.2 <- subset(drop_data, !is.na(mean_REF))
drop_data.2$logRR <- log10(drop_data.2$mean_MPA/drop_data.2$mean_REF)


# Merge the data frame with the original data to keep only the existing MPA/reference pairs and onvert back to long
means.new <- left_join(drop_data.2, means) 

means.final<-select(means.new, -c(mean_MPA,mean_REF,sd_MPA,sd_REF,n_MPA,n_REF))


# Re-order the region levels 
means.final$mlpa_region <- factor(means.final$mlpa_region, levels = c("North", "Central","South"))

#Recode and select diversity
means.final$indicator<- recode_factor(means.final$indicator, Diversity="diversity")
means.final$indicator<- recode_factor(means.final$indicator, shannon="diversity")
means.final$indicator<- recode_factor(means.final$indicator, Richness="richness")
means.final$mpa_designation<- recode_factor(means.final$mpa_designation, SMR="MPA")
means.final$mpa_designation<- recode_factor(means.final$mpa_designation, SMR_SCMA="MPA") 
means.final$mpa_designation<- recode_factor(means.final$mpa_designation, SMCA="MPA")          #COnsider SMCA as MPA for now
means.final$mpa_designation<- recode_factor(means.final$mpa_designation, reference="REF")

(categories <- unique(means.final$mpa_designation) )


#Select variables of interest

mu.H <- means.final %>%
  filter(variable=="All Fish" | 
           variable=="INVALG" |
           variable=="all species",
         indicator=='diversity',
         group=='CCFRP')            #start with CCFRP to get loop functioning

View(mu.H)


## Check the dataset if the mean column has zeros
## If yes, we will need to add a constant to all of the biomasses to avoid undefined ratios (constant = 10% of the grand mean)
# Calculate the grand mean for each variable

#m <- mean(mu.H$mean, na.rm = T)
#m.1 <- 0.1*m
#mu.H$mean <- m.1+#mu.H$mean

#start new data frame
set_data <- mu.H

# Sort the regions in an alphabetical order to ensure the loop will start from A to Z
set_data <- set_data[order(set_data$mlpa_region),]

# Sort the year in an ascending order to ensure the loop will start from the earliest year
set_data <- set_data[order(set_data$year),]


# Run the Loops ---------------------

# Create empty data frames for the loop outputs
CI_5 <- data.frame()
CI_95 <- data.frame()
year <- data.frame()
n_mpa <- data.frame()
n_ref <- data.frame()
region <- data.frame()
variable_status <- data.frame()


## Apply the nested loop function to iterate for each MPA group and for each year within each MPA group

# Identify individual region and sort them by levels
uniq_region <- unique(unlist(sort(set_data$mlpa_region)))


# Run the loop for each region
for (i in 1:length(uniq_region)){
  data_1 <- subset(set_data, mlpa_region == uniq_region[i])
  
  # Identify individual year within each region
  uniq_year <- unique(unlist(data_1$year))
  
  # Run the loop for year within each region
  for (i in 1:length(uniq_year)){
    data_2 <- subset(data_1, year == uniq_year[i])
    
    # Identify variable within each year within each region
    uniq_var <- unique(unlist(data_2$variable))
    
    # Run the loop for variable within each year within each region
    for (i in 1:length(uniq_var)){
      data_3 <- subset(data_2, variable == uniq_var[i])
      
      # Use an appropriate sample size for each site status (MPA/reference) for each region, for each year, and for each target status
      size <- function (x) summarySE(data_3, measurevar = "mean", groupvars = c("mlpa_region", "mpa_designation", "year", "variable"), na.rm = T)
      sample.size <- size(data_3)
      s_mpa <- sample.size$N[sample.size$mpa_designation == "MPA"]
      s_ref <- sample.size$N[sample.size$mpa_designation == "REF"]
      
      # Bootstrap (resampling with replacement) each set of data that was randomly sampled without replacement (for each protection status: mpa and reference)
      # Iterate the resampling 1000 times to acquire 1000 sample sets of observations

      set_mpa <- function (data_1, i) sample(data_1$mean, s_mpa, replace=F)
      mpa <- boot(data_1, set_mpa, R=1000)
      
      set_ref <- function (data_1, i) sample(data_1$mean, s_ref, replace=F)
      ref <- boot(data_1, set_ref, R=1000)
      
    
      mpa_resampled <- tidy(mpa$t)
      
      ref_resampled <- tidy(ref$t)
      
      # Calculate the mean for each sample set
      mpa_mean <- data.frame(rowMeans(mpa_resampled, na.rm=TRUE))
      colnames(mpa_mean)[1] <- "mpa_mean"
      
      ref_mean <- data.frame(rowMeans(ref_resampled, na.rm=TRUE))
      colnames(ref_mean)[1] <- "ref_mean"
      
      
      # Create MPA response ratios (mpa/reference) from sample sets
      sample_means <- cbind(mpa_mean, ref_mean) # merge mpa and reference data frames
      
      sample_means$ratio <- sample_means$mpa_mean/sample_means$ref_mean
      
      # Take log10 of the response ratios
      sample_means$log_ratio <- log10(sample_means$ratio)
      
      # Calculate the exact 5% confidence interval and store it as an output
      CI5_out <- quantile(sample_means$log_ratio, probs = c(0.05))         # Create a output
      CI_5 <- data.frame(c(CI_5, CI5_out))
      
      # Calculate the exact 95% confidence interval and store it as an output
      CI95_out <- quantile(sample_means$log_ratio, probs = c(0.95))         # Create a output
      CI_95 <- data.frame(c(CI_95, CI95_out))
      
      # Record the year the loop is calculating for and store it as an output
      year_out <- data_3$year[1]         # Create a output
      year <- data.frame(c(year, year_out))
      
      # Record the mpa sample size the loop is calculating for and store it as an output
      n_mpa_out <- s_mpa         # Create a output
      n_mpa <- data.frame(c(n_mpa, n_mpa_out))
      
      # Record the reference sample size the loop is calculating for and store it as an output
      n_ref_out <- s_ref        # Create a output
      n_ref <- data.frame(c(n_ref, n_ref_out))
      
      # Record the MPA group the loop is calculating for and store it as an output
      region_out <- as.character(data_3$mlpa_region[1])          # Create a output
      region <- data.frame(c(region, region_out))
      
      # Record the variable status the loop is calculating for and store it as an output
      variable_status_out <- as.character(data_3$variable[1])          # Create a output
      variable_status <- data.frame(c(variable_status, variable_status_out))
      
    }
  }
}


# Clean Up Output Data Frames ---------------------

# Transpose the exact 5% CI data frame into a correct format after the loop is completed 
CI_5 <- data.frame(t(CI_5))
colnames(CI_5)[1] <- "eCI_5"

# Transpose the exact 95% CI data frame into a correct format after the loop is completed 
CI_95 <- data.frame(t(CI_95))
colnames(CI_95)[1] <- "eCI_95"

# Transpose the year data frame into a correct format after the loop is completed
year <- data.frame(t(year))
colnames(year)[1] <- "Year"

# Transpose the MPA sample size data frame into a correct format after the loop is completed
n_mpa <- data.frame(t(n_mpa))
colnames(n_mpa)[1] <- "n_mpa"

# Transpose the reference sample size data frame into a correct format after the loop is completed
n_ref <- data.frame(t(n_ref))
colnames(n_ref)[1] <- "n_ref"

# Transpose the MPA group data frame into a correct format after the loop is completed
# Transpose the region data frame into a correct format after the loop is completed
region <- data.frame(t(region))
colnames(region)[1] <- "region"

# Transpose the target status data frame into a correct format after the loop is completed
variable_status <- data.frame(t(variable_status))
colnames(variable_status)[1] <- "variable_status"




# Combine Observed Means and CIs ---------------------

# Sort the true means data frame to ensure it is ordered the same way as the loop output data frames
resampled_1<- set_data[order(set_data$mlpa_region, set_data$year, set_data$mpa_designation),]
resampled_1 <- set_data%>%
  dplyr::group_by(group,mlpa_region,year,variable,indicator)%>%
  dplyr::summarize(logRR = mean(logRR,na.rm=TRUE),
            sd=mean(sd),
            #n=n()
            )



# Combine true means, null confidence intervals and year into one data frame
resampled_RR_CI <- cbind(resampled_1, CI_5, CI_95, year, n_mpa, n_ref, region) 

# Reset row names
rownames(resampled_RR_CI) <- NULL





library(ggplot2)

resampled_RR_CI$year <- as.numeric(resampled_RR_CI$year)

ggplot(resampled_RR_CI, aes(x=year, y=logRR))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  geom_ribbon(aes(ymax=eCI_95, ymin=eCI_5), group=1, alpha=0.2, fill = "#BC1605") +
  geom_point(shape=19, size=3, color = "#BC1605") +
  geom_smooth(method='lm', se = T, formula=y~x, linetype='solid', size=0.5) +
  scale_color_manual(values=c("#000000", "#A4A4A4"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Diversity[MPA]/Diversity[REF])))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(~mlpa_region, ncol=1)+
  scale_x_continuous(breaks = c(2007:2021))+
  theme_classic() 


















