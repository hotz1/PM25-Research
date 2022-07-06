pkgs = c('data.table','downloader','dplyr','leaflet','magrittr','ncdf4','sf')
for(p in pkgs) suppressMessages(require(p, character.only=T))
rm(p, pkgs)

code.dir = paste0(getwd(), '/code/') # code directory
data.dir = paste0(getwd(), '/data/') # data directory
ncdf.dir = paste0(data.dir, 'ncdf/') # NCDF directory
rds.dir = paste0(data.dir, 'rds/')

load(paste0(data.dir, 'misr_variables.rData'))
misr.dpo = fread(paste0(data.dir, 'misr_dpo.csv'))

mideast = paste0(data.dir, 'shapes/') %>% 
  list.files(pattern='.shp', full.names=T) %>% 
  lapply(FUN=function(x) st_read(x, quiet=T)) %>% 
  do.call(rbind, args=.) %>%
  select(iso=ISO, name=NAME_ENGLI, geometry)

# mideast %>% 
#   leaflet %>% 
#   addProviderTiles('CartoDB.Positron') %>% 
#   addPolygons(weight=1, opacity=1, fillOpacity=.5, label=~name)

target.dpo = misr.dpo %>%
  group_by(path) %>%
  filter(path %in% 149:183 & row_number() == 1) %>%
  arrange(path)

url.supp = c('https://l0dup05.larc.nasa.gov/opendap/misrl2l3/MISR/MIL2ASAE.003/',
             '/MISR_AM1_AS_AEROSOL_P',
             '_O',
             '_F13_0023.nc')
ncdf.urls = paste0(url.supp[1], gsub('-','.',target.dpo$date),
                   url.supp[2], sprintf('%03d', target.dpo$path),
                   url.supp[3], sprintf('%06d', target.dpo$orbit),
                   url.supp[4])

# indices for downloading
idx = 1:length(ncdf.urls)
# download loop
for(i in idx){
  cat('Downloading [', sprintf('%04d',i), '/',
      sprintf('%04d', max(idx)), ']: ',
      as.character(target.dpo$date[i]),'...',sep='')
  start = Sys.time()
  # define file name and path
  misr.file.start = regexpr('MISR_AM1', ncdf.urls[i])[1]
  ncdf.file = paste0(ncdf.dir,
                     substr(ncdf.urls[i], misr.file.start, nchar(ncdf.urls[i])))
  tryCatch({ # to handle when download fail as as not to break the loop
    # download .nc file
    download.file(ncdf.urls[i], ncdf.file, mode='wb', quiet=T)
    message(paste0(round(difftime(Sys.time(), start, units = 'secs'), 2),
                   ' secs   \r'), sep='')
    flush.console()
  }, error = function(cond){
    # gives warning if failed, including file name
    message(paste0('\nWARNING: ',
                   substr(ncdf.urls[i],77,nchar(ncdf.urls[i])),
                   ' failed to download\n'))
  })
}

ncdf.files = list.files(ncdf.dir, pattern='.nc', full.names=T)
pixels.list = list()
idx = 1:length(ncdf.files)
for(i in idx){
  mycdf = nc_open(ncdf.files[i])
  path.loc = ncdf.files[i] %>% regexpr(pattern='_P') %>% extract(1)
  path = substr(ncdf.files[i], path.loc+2, path.loc+4)
  cat(path)
  pixels = data.table(
    lon = ncvar_get(mycdf, misr.vars[5]) %>% as.vector(),
    lat = ncvar_get(mycdf, misr.vars[4]) %>% as.vector()) %>% 
    na.omit
  print(pixels)
  in.mideast = pixels %>% 
    st_as_sf(coords=c('lon','lat'), crs=4326, remove=F) %>% 
    st_contains(x=mideast, y=.) %>% 
    unlist
  cat(in.mideast %>% length, '\n\r')
  pixels.list[[i]] = pixels %>% 
    filter(row_number() %in% in.mideast) %>% 
    mutate(misr_id=paste0(path, sprintf('%06d', 1:n()))) %>% 
    select(misr_id, lon, lat) %>% data.table
  nc_close(mycdf); rm(mycdf)
  flush.console()
}

pixels.list %>% 
  rbindlist %>% 
  saveRDS(file = paste0(data.dir, 'pixels/misr_mideast_pixels.rds'))

# # misr.pixels = readRDS(paste0(data.dir, 'pixels/misr_mideast_pixels.rds'))
# 
# rds.files = list.files(rds.dir, pattern='MIL2ASAE', full.names=T)
# 
# data.pixels = rbindlist(lapply(rds.files,
#                                function(x) readRDS(x)[,'misr_id']))
# 
# pixel.counts = data.pixels[, .N, by='misr_id']
# pixel.pal = colorFactor(palette=c('navy','bisque','brown'),
#                          domain=pixel.counts$N)
# me.pixels = merge(misr.pixels, pixel.counts, by='misr_id')
# me.pixels[sample(nrow(me.pixels), floor(nrow(me.pixels) * .01))][order(N)] %>% 
#   leaflet() %>% 
#   addProviderTiles('CartoDB.Positron') %>% 
#   addCircleMarkers(~lon, ~lat, color=~pixel.pal(N),
#                    radius=1, opacity=1) %>% 
#   addLegend('bottomleft', pal=pixel.pal, values=me.pixels$N,
#             title='N', opacity=1)

# pixels = list()
# for(i in 1:length(ncdf.files)){
#   path.idx = regexpr('AEROSOL_P', ncdf.files[i]) + 9
#   path = as.numeric(substr(ncdf.files[i], path.idx, path.idx + 2))
#   mycdf = nc_open(ncdf.files[i])
#   pixels[[i]] = na.omit(
#     data.table(
#       path=path,
#       lon = as.vector(ncvar_get(mycdf, misr.vars[5])),
#       lat = as.vector(ncvar_get(mycdf, misr.vars[4]))
#     )
#   )
# }
# pixels = rbindlist(pixels)
# 
# system.time({
#   tmp = st_contains(mideast,
#                     st_as_sf(pixels, coords=c('lon','lat'), crs=4326))
# })
#
# pixels = pixels[unlist(tmp)]
# pixels = rbindlist(
#   lapply(
#     split(pixels, by='path'),
#     function(x)
#       x[, misr_id := paste0(path, sprintf('%06d', 1:nrow(x)))][, -'path']
#   )
# )
# 
# saveRDS(pixels[,c('misr_id', 'lon', 'lat')],
#         paste0(data.dir, '/rds/misr_149-183_pixels.rds'))