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

# Plan for parallelization (adjust number of workers as needed)
#plan(multisession, workers = parallel::detectCores() - 1)

process_species <- function(i) {
  sub <- dplyr::filter(dat, common_name == config_data$species[i])
  sub <- dplyr::mutate(sub, zday = (yday - mean(sub$yday)) / sd(sub$yday))
  sub$pass_scaled <- sub$pass - mean(range(sub$pass)) # -0.5, 0.5
  # apply the year, latitude, and depth filters if used
  sub <- dplyr::filter(sub,
                       latitude_dd >= config_data$min_latitude[i],
                       latitude_dd < config_data$max_latitude[i],
                       year >= config_data$min_year[i],
                       year <= config_data$max_year[i],
                       depth_m >= config_data$min_depth[i],
                       depth_m <= config_data$max_depth[i]) |>
    dplyr::rename(catch_weight = total_catch_wt_kg)

  # make a mesh based on settings in config
  mesh_old <- sdmTMB::make_mesh(sub, xy_cols = c("X","Y"),
                            n_knots = config_data$knots[i])
  target_knots <- mesh_old$mesh$n
  # find approximate cutoff that yields the same number of knots
  cutoffs <- seq(from=10,to=100, by = 1)
  cutoff_knots <- NA
  for(j in 1:length(cutoffs)) {
    mesh <- sdmTMB::make_mesh(sub, xy_cols = c("X","Y"),
                                  cutoff = cutoffs[j])
    cutoff_knots[j] <- mesh$mesh$n
  }
  # use value closest to target
  target_cutoff <- cutoffs[which.min((cutoff_knots - target_knots)^2)]
  mesh <- sdmTMB::make_mesh(sub, xy_cols = c("X","Y"),
                            cutoff = target_cutoff)
  
  sub$fyear <- as.factor(sub$year) # year as factor

  sub$area_km2 <- sub$area_swept_ha_der * 0.01 # convert to km2

  # this is to help with printing, if done below
  st <- if(config_data$family[i] == "tweedie") {
    config_data$spatiotemporal1[i]
  } else {
    list(config_data$spatiotemporal1[i],
         config_data$spatiotemporal2[i])
  }

  # fit the model using arguments in configuration file
  # initialize to NULL and wrap in try() to avoid
  # 'system is computationally singular' error
  fit <- NULL
  fit <- try(sdmTMB(formula = as.formula(config_data$formula[i]),
                time = "year",
                offset = log(sub$area_km2),
                mesh = mesh,
                data = sub,
                spatial="on",
                spatiotemporal=st,
                anisotropy = config_data$anisotropy[i],
                family = get(config_data$family[i])(),
                share_range = config_data$share_range[i]), silent = TRUE)

  if(class(fit) == "sdmTMB") {
      # create output directory if it doesn't exist
      if (!dir.exists("diagnostics")) {
        dir.create("diagnostics")
      }
      # Check write access
      file.access("diagnostics", mode = 2)

      san <- sanity(fit, silent=TRUE)
      write.csv(san, file=paste0("diagnostics/sanity_",
                               config_data$index[i], ".csv"), row.names=FALSE)

      # make predictions
      wcgbts_grid <- indexwc::california_current_grid
      # first filter the grid like with the data
      wcgbts_grid$depth <- -wcgbts_grid$depth
      wcgbts_grid <- dplyr::filter(wcgbts_grid,
                                   latitude >= config_data$min_latitude[i],
                                   latitude < config_data$max_latitude[i],
                                   depth >= config_data$min_depth[i],
                                   depth < config_data$max_depth[i],
                                   area_km2_WCGBTS > 0)
      # Add calendar date -- predicting to jul 1
      wcgbts_grid$zday <- (182 - mean(sub$yday)) / sd(sub$yday)
      wcgbts_grid$pass_scaled <- 0
      # add X-Y
      wcgbts_grid <- sdmTMB::add_utm_columns(wcgbts_grid,
                                             ll_names = c("longitude","latitude"),
                                             utm_crs = crs_out)

      # replicate grid
      wcgbts_grid <- replicate_df(wcgbts_grid, time_name = "year",
                                  time_values = unique(sub$year))
      wcgbts_grid$fyear <- as.factor(wcgbts_grid$year)

      # Make coastwide index
      pred_all <- predict(fit, wcgbts_grid, return_tmb_object = TRUE)
      index_all <- get_index(pred_all,
                             area = wcgbts_grid$area_km2_WCGBTS,
                             bias_correct = TRUE)
      index_all$index <- "Coastwide"

      indices <- index_all
      indices$index_id <- config_data$index[i]
      indices$common_name <- sub$common_name[1]
      indices$cutoff <- target_cutoff
      # append date as attribute
      attr(indices, "date") <- Sys.Date()

      # create output directory if it doesn't exist
      if (!dir.exists("output")) {
        dir.create("output")
      }
      # Check write access
      file.access("output", mode = 2)

      write.csv(indices,
              paste0("output/",
                     sub$common_name[1],"_",
                     config_data$index_id[i],".csv"), row.names=FALSE)
      
  }
}

# Apply process_species in parallel
for(spp in 1:nrow(config_data)) {
  process_species(spp)
}
