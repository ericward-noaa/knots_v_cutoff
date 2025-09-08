# compare plots for indices using knots and cutoff with similar mesh$mesh$n

# url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data-raw/configuration.csv"
# config_data <- read.csv(url, stringsAsFactors = FALSE)
# config_data <- dplyr::filter(config_data,
#                              source == "NWFSC.Combo")
# saveRDS(config_data, "config_data.rds")
config_data <- readRDS("config_data.rds")
config_data$i <- seq(1, nrow(config_data))

config_data$run_name <- paste0(config_data$species, "_",config_data$i,".csv")

for(i in 1:nrow(config_data)) {
  knot_file <- paste0("output_knots/", config_data$run_name[i])
  cutoff_file <- paste0("output/", config_data$run_name[i])
  
  # load sanity info
  knot_sanity <- cutoff_sanity <- data.frame(all_ok = FALSE)
  if(file.exists(paste0("diagnostics_knots/sanity_", i, ".csv"))) {
    knot_sanity <- read.csv(paste0("diagnostics_knots/sanity_", i, ".csv"))  
  }
  if(file.exists(paste0("diagnostics/sanity_", i, ".csv"))) {
    cutoff_sanity <- read.csv(paste0("diagnostics/sanity_", i, ".csv"))
  }
  
  if(file.exists(knot_file) & file.exists(cutoff_file) & knot_sanity$all_ok==TRUE & cutoff_sanity$all_ok==TRUE) {
      d_knots <- read.csv(knot_file) |>
        dplyr::filter(index == "Coastwide")
      d_cutoff <- read.csv(cutoff_file) |>
        dplyr::filter(index == "Coastwide")
      n <- d_cutoff$cutoff[1]
      d_cutoff <- dplyr::select(d_cutoff, -cutoff)
      d_knots$mesh_type <- "knots"
      d_cutoff$mesh_type <- "cutoff"
      d <- rbind(d_knots, d_cutoff)
      d$cutoff <- n
      if(i == 1) {
        all_d <- d
      } else {
        all_d <- rbind(all_d, d)
      }
  }
}

all_d$facet_label <- paste0(all_d$common_name, " (", all_d$cutoff, ")")

ggplot(all_d, aes(year, log_est, col = mesh_type, fill = mesh_type)) + 
  geom_ribbon(aes(ymin=log_est-2*se, ymax = log_est+2*se), alpha=0.3, colour = NA) + 
  geom_line() + 
  facet_wrap(~ facet_label, scale="free") + 
  ylab("Ln index +/- 2SE") + xlab("Year") + 
  theme_bw() + 
  theme(strip.text = element_text(size = 7)) + 
  theme(axis.text.x = element_text(size = 7)) + 
  ggtitle("Comparison of indices with 2 kinds of meshes (cutoff distance in parentheses)")
ggsave("cutoff_v_knots_comparison.png", width = 9, height = 7)



