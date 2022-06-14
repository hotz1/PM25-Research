# require(sp)
# require(dplyr) # bind_cols()
# require(rgdal)
# require(raster)
# require(maptools)
# require(sf) # st_read/st_intersects/etc.
# require(plyr) # alply()
# require(ncdf4)
# require(data.table)
# require(RPostgreSQL)

# setwd('P:/Satellite/MISR')
# source('code/isr_postgres_functions.R')


source('C:/Users/khang/OneDrive/USC/misr/code/misr/misr_postgres_functions.R')
# nc.list = paste0('ncdf/', list.files(path='P:/Satellite/MISR/ncdf', pattern='.nc'))

# load MISR variables & world sf ------------------------------------------
# load('misr_variables.rData')
# load('world.rData')
setwd('G:/Team Drives/UB/data')
load('misr_variables.rData')
misr.dpo = fread('misr_dpo.csv')
# cali = st_transform(st_read('ca_counties_caltrans.shp'), crs=4326)
ub = st_transform(st_read('../shapes/Khoroos.shp'), crs=4326)

# misr.vars        : variables for ncdf data layers
# misr.prods.vars  : variables for 4.4km products
# misr.aux.raw.vars: variables for Auxiliary raw data
# misr.aux.mix.vars: variables for Auxiliary 74 mixtures data
# world            : sf of 245 world countries for filtering


# DB Connection -----------------------------------------------------------
require(RPostgreSQL)
drv = dbDriver('PostgreSQL') # DB driver
pw = {"YUctmxY37aQ3BcFnRPqkAayj"} # password
misr_conn = dbConnect(drv, dbname = 'misr',
                      host = 'misr-ub.cqq6ohtik4cc.us-west-1.rds.amazonaws.com',
                      port = 58525, user = 'kchau', password = pw)
rm(pw) # remove pw

# process and upload MISR from 2010 to 2018 for Ulaanbaatar ---------------
ub.pixels.file = 'G:/Team Drives/UB/data/misr_ub_pixels.rds'
setwd('D:/_MISR UB')
nc.files = list.files(pattern = '.nc')

# find the correct order by Orbit (proxy for Date) instead of Path
orbits = data.table(
  orbit = as.numeric(substr(nc.files, 27, 32)),
  order = 1:length(nc.files)
)[order(orbit)]
orbits = merge(orbits, misr.dpo, by='orbit')
# re-order list of NC file based on above
nc.files = nc.files[orbits$order]

# # empty data set with lon, lat, and pixel_id for tracking
# # only runs once on the very first time starting a loop
# saveRDS(data.table(lon = numeric(),
#                    lat = numeric(),
#                    pixel_id = character()),
#         file=ub.pixels.file)
for(i in 801:1169){
  start = Sys.time()
  # have to store/read in RDS format so lon-lat decimals are consistent
  cat(paste0(rep('=', 60)), '\n',
      'Processing file #', sprintf('%04d', i), ' / ',
      length(nc.files), ' from ', paste0('P', sprintf('%03d',orbits[i]$path)), 
      '[', orbits[i]$date, ']:\n', sep='')
  pixels.id = readRDS(ub.pixels.file)
  # extract NC file using extract.ncdf
  results = extract.ncdf(files = nc.files, file.idx=i,
                         region = ub, pixels.list = pixels.id,
                         var.list = misr.vars,
                         filter.data = T, filter.region = T)
  # if there are new pixels from results, update PSQL 'pixels'
  if(nrow(pixels.id) < nrow(results$misr.pixels)){
    insert.pixels(current.pixels = pixels.id,
                  new.pixels = results$misr.pixels,
                  pixels.file = ub.pixels.file,
                  db.conn = misr_conn)
  }
  # upload results data to PSQL tables
  insert.misr(misr.data = results$misr.data,
              prods.vars = misr.prods.vars,
              raw.vars = misr.aux.raw.vars,
              mix.vars = misr.aux.mix.vars,
              db.conn = misr_conn)
  rm(results); gc()
  cat('TOTAL TIME..........',
      round(difftime(Sys.time(), start, units = 'secs'), 2),
      ' secs\n', sep='')
  if(i %% 50 == 0){vacuum.analyse(db.conn = misr_conn)}
}

# process and upload MISR from 2000 to 2018 for California ----------------
setwd('D:/_MISR CA')
nc.files = list.files(pattern = '.nc')

# saveRDS(data.table(lon = numeric(),
#                    lat = numeric(),
#                    pixel_id = character()),
#         'C:/Users/khang/OneDrive/USC/misr/data/misr_ca_pixels.rds')
for(i in 1:10){ # finished 1550
  start = Sys.time()
  # have to store/read in RDS format so lon-lat decimals are consistent
  pixels.id = readRDS('C:/Users/khang/OneDrive/USC/misr/data/misr_ca_pixels.rds')
  # extract NC file using extract.ncdf
  results = extract.ncdf(files = nc.files, file.idx=i,
                         region = cali, pixels.list = pixels.id,
                         var.list = misr.vars,
                         filter.data = T, filter.region = T)
  # if there are new pixels from results, update PSQL 'pixels'
  if(nrow(pixels.id) < nrow(results$misr.pixels)){
    insert.pixels(current.pixels = pixels.id,
                  new.pixels = results$misr.pixels,
                  pixels.file = 'C:/Users/khang/OneDrive/USC/misr/data/misr_ca_pixels.rds',
                  db.conn = misr_conn)
  }
  # upload results data to PSQL tables
  insert.misr(misr.data = results$misr.data,
              prods.vars = misr.prods.vars,
              raw.vars = misr.aux.raw.vars,
              mix.vars = misr.aux.mix.vars,
              db.conn = misr_conn)
  rm(results); gc()
  cat('TOTAL TIME..........',
      round(difftime(Sys.time(), start, units = 'secs'), 2),
      ' secs\n', sep='')
  if(i %% 50 == 0){vacuum.analyse(db.conn = misr_conn)}
}

# process & upload from year 2000 -----------------------------------------
# finished year 2000 for paths 40:45 & 163:168

nc.files = nc.list[substr(nc.list, 6, nchar(nc.list[1])) %in% 
                     readRDS('misr_2000_cali_kuwait.rds')$nc]
saveRDS(data.table(lon = numeric(),
                   lat = numeric(),
                   pixel_id = character()),
        'misr_pixels.rds')
for(i in 1:length(nc.files)){
  start = Sys.time()
  # have to store/read in RDS format so lon-lat decimals are consistent
  pixels.id = readRDS('misr_pixels.rds')
  # extract NC file using extract.by.path
  results = extract.ncdf(files = nc.files, file.idx = i,
                         region = world, pixels.list = pixels.id,
                         var.list = misr.vars,
                         filter.data = T, filter.region = T)
  # if there are new pixels from results, update PSQL 'pixels'
  if(nrow(lonlat.id) < nrow(results$misr.pixels)){
    insert.pixels(current.pixels = pixels.id,
                  new.pixels = results$misr.pixels,
                  pixels.file = 'misr_pixels.rds',
                  db.conn = misr_conn)
  }
  # upload results data to PSQL tables
  insert.misr(misr.data = results$misr.data,
              prods.vars = misr.prods.vars,
              raw.vars = misr.aux.raw.vars,
              mix.vars = misr.aux.mix.vars,
              db.conn = misr_conn)
  rm(results); gc()
  cat('TOTAL TIME..........',
      round(difftime(Sys.time(), start, units = 'secs'), 2),
      ' secs\n', sep='')
  if(i %% 10 == 0){vacuum.analyse(db.conn = misr_conn)}
}

# short script to reset coords when restarting whole process --------------
# saveRDS(data.table(lon = numeric(),
#                   lat = numeric(),
#                   pixel_id = character()),
#         'misr_coords.rds')
#
# get world map -----------------------------------------------------------
# world = st_read('shapes/TM_WORLD_BORDERS_SIMPL-0.3.shp')[,-c(1:4,6:11)]
# world = world[world$NAME != 'Antarctica',]
#
# # postgres template -----------------------------------------------------
# # DB driver
# drv = dbDriver('PostgreSQL')
# # password
# pw = {"YUctmxY37aQ3BcFnRPqkAayj"}
# # establish connection
# misr_conn = dbConnect(drv, dbname = 'misr',
#                       host = 'misr.cqq6ohtik4cc.us-west-1.rds.amazonaws.com',
#                       port = 58525, user = 'kchau', password = pw)
# # remove pw
# rm(pw)
#
# old ---------------------------------------------------------------------
# path.40 = nc.list[substr(nc.list, 28, 29) == '40']
# # MISR products column names
# saveRDS(names(test$misr.data)[c(252:251,3:11)], 'misr_prods_vars.rds')
# # MISR auxiliary raws column names
# saveRDS(names(test$misr.data)[c(252:251,12:28)], 'misr_aux_raw_vars.rds')
# # MISR auxiliary mixtures column names
# saveRDS(names(test$misr.data)[c(252:251,29:250)], 'misr_aux_mix_vars.rds')