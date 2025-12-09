# Combine bathymetry datasets into single raster
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024


# Combine the CSMP, CDFW, and WCDSCI layers ------------------------------------------

# Setup ----
library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

wcds.dir <- "/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
mbes.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/depth_MBES_CA"
caba.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/CA_Bathy_30m"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"


# Read ----

# Read the resampled bathy datasets
csmp <- rast(file.path(proc.dir, "csmp_30m_bathy_res.tif"))
cdfw <- rast(file.path(proc.dir, "cdfw_30m_bathy_res.tif"))
wcds <- rast(file.path(proc.dir, "wcds_30m_bathy_res.tif"))

# Check current status to confirm all is good before combining
crs(csmp)
crs(cdfw)
crs(wcds)

res(csmp)
res(cdfw)
res(wcds)

ext(csmp)
ext(cdfw)
ext(wcds)

compareGeom(csmp, cdfw, stopOnError = FALSE)
compareGeom(csmp, wcds, stopOnError = FALSE)
compareGeom(cdfw, wcds, stopOnError = FALSE)

# Explore best approach to combine the three datasets: 
# 1. The CSMP data is likely based on the raw CDFW data, but has been further
#    processed and interpolated. These two should be fairly similar, but the 
#    CSMP likely spans broader extent (though unsure if the CSMP actually cuts
#    some of the areas from CDFW...)
# 2. The WCDS is from updated mapping but at a higher resolution. It will likely
#    be the only source for deeper areas, but may also span gaps along the others


# To combine: 
# reg_sprc <- sprc(csmp, cdfw, wcds)
# reg_mosaic <- mosaic(reg_sprc)
# plot(reg_mosaic) # worked


# draw a representative sample (use template or csmp)
pts <- spatSample(csmp, size = 50000, method = "random", as.points = TRUE)

vals <- cbind(
  csmp = extract(csmp, pts)[,2],
  wcds = extract(wcds, pts)[,2],
  cdfw = extract(cdfw, pts)[,2]
) %>% as.data.frame()

# summary differences where both available
vals_long <- na.omit(vals)
summary(vals_long)
cor(vals_long$csmp, vals_long$wcds, use="complete.obs")
cor(vals_long$csmp, vals_long$cdfw, use="complete.obs")

# compute per-source RMSE vs CSMP on sample (lower = closer to CSMP)
rmse <- function(a,b) sqrt(mean((a-b)^2, na.rm=TRUE))
rmse(vals_long$wcds, vals_long$csmp)
rmse(vals_long$cdfw, vals_long$csmp)

# generally shows that CDFW and CSMP are very similar (not surprising)
# and the expectation is that WCDS will span deeper and a generally
# greater geographical range (up to WA from CA)

summary(vals_long$wcds - vals_long$csmp) # deeper
summary(vals_long$cdfw - vals_long$csmp) # essentially the same

diff_cdfw <- abs(csmp - cdfw)
plot(diff_cdfw) 
summary(diff_cdfw)

# shows that cdfw is nearly identical to CSMP, which makes sense since
# CSMP is supposedly a processed version of CDFW. 
# summary of subset shows median difference
# is 0.01, mean difference 0.09m, and 75% of differences < 0.03m
# the max difference in this subset is 3.02m

# So will use:
# 1. CSMP as primary, CDFW as next fill
# 2. WCDS for all other NAs...

# Approach in next script likely:
# final <- merge(csmp, cdfw)
# final <- merege(final, wcds)

# ^ using the _res files...

# expored whether to use cover() or merge() and was unable to really
# decode the differences, but from documentation merge() seems somewhat safer
# so want to use that...

# another option might look like this:
# Combine with priority: CSMP > CDFW > WCDS
# combined <- app(c(csmp, cdfw, wcds),
#                 fun = function(x) {
#                   if (!is.na(x[1])) return(x[1])
#                   if (!is.na(x[2])) return(x[2])
#                   x[3]
#                 },
#                 filename = "combined.tif", overwrite = TRUE
# )
# 
# # Provenance raster
# source_ids <- app(c(csmp, cdfw, wcds),
#                   fun = function(x) {
#                     if (!is.na(x[1])) return(1)
#                     if (!is.na(x[2])) return(2)
#                     if (!is.na(x[3])) return(3)
#                     NA
#                   },
#                   filename = "source_id.tif", overwrite = TRUE
# )

