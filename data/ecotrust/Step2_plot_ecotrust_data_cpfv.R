
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "ecotrust/raw")
outdir <- file.path(basedir, "ecotrust/processed")
plotdir <- "data/ecotrust/figures"

# Read data
data <- readRDS(file=file.path(outdir, "ecotrust_survey_data_cpfv.Rds"))

# Read question key
q_key <- readxl::read_excel(file.path(outdir, "question_key_cpfv.xlsx"))
r_key <- readxl::read_excel(file.path(outdir, "response_key_cpfv.xlsx"))


# Plotting function
################################################################################

q <- c("Marine resource health-present")
plot_data <- function(q){
  
  # Format data
  fdata <- data %>% 
    # Filter
    filter(question==q) %>% 
    # Summarize
    group_by(port_complex, response_id, response) %>% 
    summarize(n=n()) %>% 
    ungroup() %>% 
    group_by(port_complex) %>% 
    mutate(prop=n/sum(n)) %>% 
    ungroup()
  
  # Response order
  response_key <- r_key %>% 
    filter(question==q) %>% 
    arrange(response_id)
  
  # Port labels
  port_labels <- data %>% 
    group_by(port_complex) %>% 
    summarize(n=n_distinct(respondent_id)) %>% 
    ungroup() %>% 
    arrange(port_complex) %>% 
    mutate(port_label=paste0(port_complex, " (n=", n, ")"),
           port_label=factor(port_label, levels=port_label))
  
  # Order data
  fdata_ordered <- fdata %>% 
    mutate(response=factor(response, levels=response_key$response)) %>% 
    left_join(port_labels %>% select(port_complex, port_label), by="port_complex")

  # Theme
  my_theme <-  theme(axis.text=element_text(size=6),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.title=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  
  # Get title
  question <- q_key %>% filter(question==q) %>% pull(question_long) %>% gsub("\\\\n", "\n", .)
    
  # Plot data
  g <- ggplot(fdata_ordered, aes(x=prop, y=port_label, fill=response)) +
    geom_bar(stat="identity", color="grey30", lwd=0.3) +
    # Labels
    labs(x="Percent of respondents", y="", title=question ) +
    scale_x_continuous(labels=scales::percent) +    
    # Legend
    scale_fill_manual(name="Response", values=RColorBrewer::brewer.pal(5, "RdBu"), na.value = "grey50", drop=F) +
    # Theme
    theme_bw() + my_theme
  g
  
  # Export
  figname <- paste0("FigX_cpfv_", gsub(" |\\/", "_", tolower(q)), ".png")
  ggsave(g, filename=file.path(plotdir, figname), 
         width=6.5, height=2.5, units="in", dpi=600)
  
}

# Loop and plot
for(i in 1:nrow(q_key)){
  print(i)
  plot_data(q=q_key$question[i])
}
