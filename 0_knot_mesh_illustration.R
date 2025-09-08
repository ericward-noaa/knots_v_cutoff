library(nwfscSurvey)
library(indexwc)
library(lubridate)
library(dplyr)
library(sdmTMB)
library(stringr)
library(tibble)
library(stringr)
library(future)
library(future.apply)
url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data-raw/configuration.csv"
config_data <- read.csv(url, stringsAsFactors = FALSE)
config_data <- dplyr::filter(config_data,
                             source == "NWFSC.Combo")
# add model index
config_data$index_id <- seq_len(nrow(config_data))

# switch signs on the depth -- negative but pos in data
config_data$max_depth <- -config_data$max_depth
config_data$min_depth <- -config_data$min_depth
# drop out pass_scaled and put in yday instead
# config_data$formula <- str_replace(config_data$formula,
#                                    "pass_scaled",
#                                    "zday + I(zday^2)")
# replace longnames in family
config_data$family <- str_replace(config_data$family, "sdmTMB::", "")
config_data$family <- str_replace(config_data$family, "\\(\\)", "")

# Use nwfscSurvey to get the data
haul <- nwfscSurvey::pull_haul(survey = "NWFSC.Combo")
dat <- nwfscSurvey::pull_catch(survey = "NWFSC.Combo",
                               common_name = config_data$species)
names(dat) <- tolower(names(dat))
dat <- dplyr::left_join(dat, haul[,c("trawl_id","area_swept_ha_der")])
# convert date string to doy
dat$yday <- lubridate::yday(dat$date)
# filter out a few bad locations
dat <- dplyr::filter(dat, !is.na(longitude_dd),
                     !is.na(latitude_dd))

# Set default CRS for estimation / prediction
crs_out <- 32610

# add X, Y
dat <- sdmTMB::add_utm_columns(dat,
                               ll_names = c("longitude_dd","latitude_dd"),
                               utm_crs = crs_out)
# temp fix for rougheye - blackspotted
dat$common_name[which(dat$common_name == "rougheye and blackspotted rockfish")] <- "rougheye rockfish"

config_data$vertices <- NA
for(spp in 1:nrow(config_data)) {
  mesh <- sdmTMB::make_mesh(dplyr::filter(dat, common_name == "yellowtail rockfish"), 
                            xy_cols = c("X","Y"),
                            n_knots = config_data$knots[spp])
  config_data$vertices[spp] <- mesh$mesh$n
}

ggplot(config_data, aes(knots, vertices)) + 
  geom_point(size=2) + 
  geom_abline(aes(intercept=0, slope=1), col = "red") +
  xlab("Input 'n_knots'") + 
  ylab("Mesh vertices") + 
  theme_bw() + xlim(0,501) + ylim(0,801)
ggsave("knot_mesh_illustration.png")

