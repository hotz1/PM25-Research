require(sf)
require(plyr) # alply()
require(ncdf4)
require(data.table)
require(RPostgreSQL)
# message('Loaded packages:\n- sf\n- plyr\n- ncdf4\n- data.table\n- RPostgreSQL')

# Function to extract relevant layers from NC files -----------------------
extract.ncdf = function(files, file.idx, region, var.list, pixels.list,
                        filter.data = T, filter.region = T){
  # inputs: 
  # files        : vector of files
  # file.idx     : index of file in vector
  # region       : expecting polygon sf object to filter spatially
  # var.list     : vector of NC layers variable names
  # pixels.list  : data.table of current list of unique pixels coordinates
  # filter.data  : keep pixels with at least a certain number of columns not missing
  # filter.region: filter for pixels within provided region
  
  # open NC file
  path.loc = regexpr('P', files[file.idx])
  path = substr(files[file.idx], path.loc, path.loc+3)
  mycdf = nc_open(files[file.idx])
  # cat(paste0(rep('=', 60)), '\n',
  #     'Processing file #', sprintf('%03d', file.idx), ' / ',
  #     length(files), ' from ', path, ':\n', sep='')
  cat('- AOD Mixtures......'); start = Sys.time()
  # collect 74 AOD mixtures data from 3-D array into data.table w/ 74 cols
  aod.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[38]),1), as.vector))
  # give column names
  names(aod.74) = paste0('aod_mix_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  
  cat('- X2 Mixtures.......'); start = Sys.time()
  # collect min chisq of 74 mixtures from 3-D array into data.table w/ 74 cols
  chisq.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[39]), 1), as.vector))
  # give column names
  names(chisq.74) = paste0('min_chisq_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  
  cat('- Flag Mixtures.....'); start = Sys.time()
  # collect retrieval flags of 74 mixes from 3-D array to data.table w/ 74 cols
  flags.74 = setDT(lapply(alply(ncvar_get(mycdf, var.list[40]), 1), as.vector))
  # give column names
  names(flags.74) = paste0('rtrv_flag_', sprintf('%02d', 1:74))
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  
  cat('- Products & Raws...'); start = Sys.time()
  # create data.table combining all relevant layers as columns
  tmp = na.omit(
    data.table(
      ## coords/elev/datetime
      lon = as.vector(ncvar_get(mycdf, var.list[5])),
      lat = as.vector(ncvar_get(mycdf, var.list[4])),
      elev = as.vector(ncvar_get(mycdf, var.list[6])),
      year = as.vector(ncvar_get(mycdf, var.list[7])),
      month = as.vector(ncvar_get(mycdf, var.list[9])),
      day = as.vector(ncvar_get(mycdf, var.list[10])),
      hour = as.vector(ncvar_get(mycdf, var.list[11])),
      min = as.vector(ncvar_get(mycdf, var.list[12])),
      ## layers from 4.4 km products
      aod = as.vector(ncvar_get(mycdf, var.list[14])),
      aod_unc = as.vector(ncvar_get(mycdf, var.list[15])),
      angs_exp_550_860 = as.vector(ncvar_get(mycdf, var.list[16])),
      absorp_aod = as.vector(ncvar_get(mycdf, var.list[18])),
      nonsph_aod = as.vector(ncvar_get(mycdf, var.list[19])),
      small_aod = as.vector(ncvar_get(mycdf, var.list[20])),
      medium_aod = as.vector(ncvar_get(mycdf, var.list[21])),
      large_aod = as.vector(ncvar_get(mycdf, var.list[22])),
      ## layers from auxiliary, mostly raws
      aod_raw = as.vector(ncvar_get(mycdf, var.list[24])),
      aod_unc_raw = as.vector(ncvar_get(mycdf, var.list[25])),
      angs_exp_550_860_raw = as.vector(ncvar_get(mycdf, var.list[26])),
      absorp_aod_raw = as.vector(ncvar_get(mycdf, var.list[28])),
      nonsph_aod_raw = as.vector(ncvar_get(mycdf, var.list[29])),
      small_aod_raw = as.vector(ncvar_get(mycdf, var.list[30])),
      medium_aod_raw = as.vector(ncvar_get(mycdf, var.list[31])),
      large_aod_raw = as.vector(ncvar_get(mycdf, var.list[32])),
      aeros_rtrv_conf_idx = as.vector(ncvar_get(mycdf, var.list[37])),
      cldscrn_param = as.vector(ncvar_get(mycdf, var.list[41])),
      cldscrn_neighbor3x3 = as.vector(ncvar_get(mycdf, var.list[42])),
      aeros_rtrv_scrn_flag = as.vector(ncvar_get(mycdf, var.list[43])),
      col_o3_clim = as.vector(ncvar_get(mycdf, var.list[44])),
      ocsurf_ws_clim = as.vector(ncvar_get(mycdf, var.list[45])),
      ocsurf_ws_rtrv = as.vector(ncvar_get(mycdf, var.list[46])),
      rayleigh_od = as.vector(ncvar_get(mycdf, var.list[47])),
      lowest_res_mix = as.vector(ncvar_get(mycdf, var.list[48])),
      ## previously extracted layers of 74 mixtures (aod, chisq, flags)
      aod.74, chisq.74, flags.74
    ), cols = c('lon', 'lat')) # only keep rows with no-NA coordinates
  # close NC file, remove NC connection & 74 mixtures tables for better memory
  nc_close(mycdf)
  rm(mycdf, aod.74, chisq.74, flags.74)
  gc()
  # generate single date_time column from year/month/day/hour/minute layers
  # remove those 5 columns
  tmp[, date_time := 
        paste0(year, '-', sprintf('%02d', month), '-', sprintf('%02d', day), ' ',
               hour, ':', min, ':00')]
  tmp = tmp[, -c('year','month','day','hour','min')]
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  # remove pixels missing too much data
  if(filter.data == T){
    tmp[, num_obs := Reduce(`+`, lapply(.SD,function(x) !is.na(x)))]
    tmp = tmp[num_obs > 11,-'num_obs']
  }
  # check if pixel over region
  if(filter.region == T){
    cat('- Filter for region...'); start = Sys.time()
    pixels.sf = st_as_sf(tmp[,c('lon','lat')], coords=c('lon','lat'), crs = 4326)
    over.region = suppressMessages(st_intersects(pixels.sf, region, sparse=TRUE))
    tmp = tmp[sapply(over.region, length) > 0]
    cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  }
  # count number of known unique pixels *before* current file
  path.pixels = pixels.list[substr(pixel_id, 1,4) == path]
  pix.count = nrow(path.pixels)
  if(pix.count > 0){
    # merge with pixels from current data
    # new pixels will have NAs in pixel_id column
    path.pixels = merge(path.pixels, tmp[,c('lon','lat')], 
                        all=T, by=c('lon', 'lat'))
    path.pixels = path.pixels[order(pixel_id)]
  } else{
    # to handle very first NC file with no existing pixels
    path.pixels = data.table(
      tmp[,c('lon','lat')]
    )
    # generate 'pixel_id'
    path.pixels[, pixel_id :=
                  paste0(path, '_', sprintf('%07d', 1:nrow(path.pixels)))]
  }
  # if there are new pixels
  if(nrow(path.pixels) > pix.count){
    # count new total
    new.count = nrow(path.pixels)
    cat('*** New pixels:', (new.count - pix.count), 'found\n')
    # generate pixel_id for new pixels
    path.pixels[, pixel_id := 
                  ifelse(!is.na(pixel_id), pixel_id,
                         paste0(path, '_', sprintf('%07d', (pix.count+1):new.count)))]
  }
  pixels.list = rbind(pixels.list[substr(pixel_id, 1, 4) != path],
                      path.pixels)
  # merge pixel_ids into dataset
  tmp = merge(tmp, pixels.list, all.x=T, by=c('lon','lat'))
  # return list:
  # misr.data = entire data set with pixel_id
  # misr.pixels = data.table of all unique pixels including new pixels
  return(list(misr.data = tmp, misr.pixels = pixels.list))
}

# Function to insert new pixels to PSQL db --------------------------------
insert.pixels = function(current.pixels, new.pixels, pixels.file, db.conn){
  # input:
  # current.pixels: table of uniq pixels w/o new ones
  # new.pixels    : table of uniq pixels w/ new ones
  # pixels.file   : RDS file of pixels to update
  # local         : whether to use localhost or AWS db
  
  pix.count = nrow(current.pixels)
  new.count = nrow(new.pixels)
  # save new table to RDS format for consistent coordinates decimal reading
  saveRDS(new.pixels, pixels.file)
  # collect only new pixels
  only.new = new.pixels[!(pixel_id %in% current.pixels$pixel_id)]
  
  # # PSQL connection
  # drv = dbDriver('PostgreSQL') # DB driver
  # # establish connection
  # if(local == F){ # if connecting to AWS db
  #   pw = {"ueGsjvqKi6z4Yk3bLckKs2qU"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'misr.cqq6ohtik4cc.us-west-1.rds.amazonaws.com',
  #                         port = 58510, user = 'ken', password = pw)
  # } else{ # if connecting to local db
  #   pw = {"TSb)a-Vj8Gq)x!=5_b?+GhY]"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'localhost',
  #                         port = 57463, user = 'kchau', password = pw)
  # }
  # rm(pw) # remove pw
  cat('Insert ', nrow(only.new), " pixels to PSQL 'pixels': ", sep='')
  start = Sys.time()
  # insert new pixels to 'pixels_temp' table in PSQL
  dbWriteTable(db.conn, "pixels_temp", only.new[,c(3,1,2)], 
               row.names=FALSE, append=TRUE)
  # PSQL command to insert new pixels & add geometry
  psql.pixels.insert = 
    "UPDATE pixels_temp SET geom = st_setsrid(st_makepoint(lon, lat), 4326);
     INSERT INTO pixels SELECT pixel_id, geom FROM pixels_temp;
     TRUNCATE pixels_temp;"
  # run PSQL query
  dbGetQuery(db.conn, psql.pixels.insert)
  # close PSQL connection
  # dbDisconnect(db.conn); rm(db.conn)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs.\n', sep='')
  # no outputs
}

# Function to insert MISR data to PSQL db ---------------------------------
insert.misr = function(misr.data, prods.vars, raw.vars, mix.vars, db.conn){
  # inputs:
  # misr.data : result data table from extract.ncdf()
  # prods.vars: column names for prods table
  # raw.vars  : column names for aux_raw table
  # mix.vars  : column names for aux_mix table
  # local     : whether to use localhost or AWS db
  
  # # PSQL connection
  # drv = dbDriver('PostgreSQL') # DB driver
  # # establish connection
  # if(local == F){ # if connecting to AWS db
  #   pw = {"ueGsjvqKi6z4Yk3bLckKs2qU"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'misr.cqq6ohtik4cc.us-west-1.rds.amazonaws.com',
  #                         port = 58510, user = 'ken', password = pw)
  # } else{ # if connecting to local db
  #   pw = {"TSb)a-Vj8Gq)x!=5_b?+GhY]"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'localhost',
  #                         port = 57463, user = 'kchau', password = pw)
  # }
  # rm(pw) # remove pw
  cat("Insert ", nrow(misr.data), " rows from ",
      substr(misr.data[1]$date_time,1,10),
      " into 'misr' PostgreSQL db:\n", sep='')
  cat("- prods............."); start = Sys.time()
  # insert prods table to 'misr_prods'
  dbWriteTable(db.conn, 'misr_prods', 
               misr.data[,..prods.vars],
               row.names=FALSE, append=TRUE)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  
  cat("- aux_raw..........."); start = Sys.time()
  # insert raw table to 'aux_raw'
  dbWriteTable(db.conn, 'misr_aux_raw', 
               misr.data[,..raw.vars],
               row.names=FALSE, append=TRUE)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  
  cat("- aux_mix..........."); start = Sys.time()
  # insert mix table to 'aux_mix'
  dbWriteTable(db.conn, 'misr_aux_mix', 
               misr.data[,..mix.vars],
               row.names=FALSE, append=TRUE)
  # dbDisconnect(misr_conn); rm(misr_conn)
  cat(round(difftime(Sys.time(), start, units = 'secs'), 2), ' secs\n', sep='')
  # no outputs
}

# Function to VACUUM & ANALYSE the PSQL db --------------------------------
vacuum.analyse = function(db.conn){
  # # PSQL connection
  # drv = dbDriver('PostgreSQL') # DB driver
  # # establish connection
  # if(local == F){ # if connecting to AWS db
  #   pw = {"ueGsjvqKi6z4Yk3bLckKs2qU"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'misr.cqq6ohtik4cc.us-west-1.rds.amazonaws.com',
  #                         port = 58510, user = 'ken', password = pw)
  # } else{ # if connecting to local db
  #   pw = {"TSb)a-Vj8Gq)x!=5_b?+GhY]"} # password
  #   misr_conn = dbConnect(drv, dbname = 'misr',
  #                         host = 'localhost',
  #                         port = 57463, user = 'kchau', password = pw)
  # }
  # rm(pw) # remove pw
  # table names for vac/analyse queries
  table.names = c('pixels', 'misr_prods', 'misr_aux_raw', 'misr_aux_mix')
  # the actual query
  vac.ana = paste0('VACUUM ANALYSE ', table.names, ';')
  cat(paste0(rep('#', 50)), "\n", "Vacuum & Analyse tables:\n", sep='')
  # vacuum & analyse each of the 4 db tables
  for(i in 1:4){
    cat('- ', table.names[i], rep('.', 20-nchar(table.names[i])), sep='')
    start = Sys.time()
    # run vac analyse query
    dbGetQuery(db.conn, vac.ana[i])
    cat(round(difftime(Sys.time(), start, units='secs'), 2), ' secs.\n', sep='')
  }
  # close PSQL connection
  # dbDisconnect(misr_conn); rm(misr_conn)
  # no outputs
}


# Function to generate query to add geometry column -----------------------

add.geom.query = function(table, crs, coords_x, coords_y, transform=F, new.crs){
  base.query = 
    paste(
      paste0("ALTER TABLE ", table, " ADD COLUMN geom geometry(Point, ", crs, ");\n"),
      paste0("UPDATE ", table, " SET geom = st_setsrid(st_makepoint(",
             coords_x, ",", coords_y, "), ", crs, ");\n")
    )
  index.query = paste0("CREATE INDEX ", table, "_geom_idx ON ", 
                       table, " USING gist(geom);\n")
  if(transform == T){
    transform.query = 
      paste0(
        "ALTER TABLE ", table, 
        " ALTER COLUMN geom TYPE geometry(Point, ", new.crs, 
        ") USING st_transform(geom, ", new.crs, ");\n"
      )
    query = paste(base.query, transform.query, index.query)
  } else{
    query = paste(base.query, index.query)
  }
  return(query)
}

