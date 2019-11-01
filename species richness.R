
#### caterpillar species richness from CFIS surveys ####
# goals - map species richness
# calculate richness across ecoregions
# determine total number of sites based on spatial aggregation of species occurrences
# interpolate richness across full range
# experiment with different map styles and resolutions

#### prelims ####
library(viridis)
library(sf)
library(rmapshaper)
library(ggthemes)
library(tidyverse)
library(units)
library(jcolors)


#### read in shapefiles #####

# Canada Lambert conformal conic
lcc<-'+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# canada country boundary
can.simp<-st_read('/Users/collnell/Dropbox/CFIS maps/canada map/can_simplen.shp')%>%st_transform(lcc) # simplified outline of canada
outline<-can.simp%>%group_by()%>%summarize()%>%ms_simplify(keep=0.01) # outline of country simplified

# vegetation delineations

# Ecoprovince - A subdivision of an ecozone characterized by major assemblages of 
# structural or surface forms, faunal realms, and vegetation, hydrology, soil, and macro climate. 
# For example, the Newfoundland ecoprovince (no. 6.4) is one of six ecoprovinces within the Boreal Shield Ecozone.
# delineation of ecoprovinces (Marshall, I. B., E. Wiken, and H. Hirvonen, 1998)
# The number of ecoprovinces depicted in Canada reflects the demand in Canada for a sub-division of ecozones for 
# broad conservation and resource purposes and the need to correlate the delineation of ecoregions across the Canada - United States boundar

# Ecoregion - subdivision of an ecoprovince characterized by distinctive regional ecological factors
# like climate, physiography, vegetation, soil, water, and fauna
# some data attributes only avaialbe at ecoregion or ecodistrict level

# http://sis.agr.gc.ca/cansis/nsdb/ecostrat/1999report/data_tables.html

# read in and trim to terrestrial only, simplify 
ecoz<-st_read('/Users/collnell/Dropbox/Projects/canada/Ecozones/Ecozones.shp')%>%
  dplyr::select(-ZONE_NOM)%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecozone=ZONE_NAME)# convert to Canada Lambert Conformal Conic CRS
ecor<-st_read('~/Dropbox/CFIS maps//Ecoregions/ecoregions.shp')%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecoregion=REGION_NAM) ## the ecoregion delinations
ecop<-st_read('~/Dropbox/CFIS maps/Ecoprovinces/ecoprovinces.shp')%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecoprovince=ECOPROVINC)

## combine hierarchy
eco.all<-ecor%>%st_buffer(0)%>%st_intersection(ecoz%>%st_buffer(0))%>%st_buffer(0)
glimpse(eco.all)

# simple plot of ecozones
ggplot()+
  geom_sf(data=eco.all, aes(fill=ecozone), size=.5)+
  geom_sf(data=outline, fill=NA, color='black')+
  theme_map()+
  theme(legend.position='none')

length(unique(ecor$ecoregion))


## ntesting new color palettes for gradient scales
display_all_jcolors()
display_all_jcolors_contin()
display_jcolors(3)
display_all_jcolors_contin
pal2 <- c("#1a1334", "#26294a", "#01545a", "#017351", "#03c383", 
          "#aad962")
pal10 <- c("#3e71a8", "#577f9f", "#698e96", "#779d8d", "#84ad83", 
           "#8fbd77", "#99cd6b", "#a2dd5c", "#aaee49", "#b2ff2e")
pal11 <- c("#202547", "#323649", "#41474b", "#4e5a4c", "#5c6c4c", 
           "#68804c", "#75944b", "#81a949", "#8ebe45", "#9ad340", 
           "#a6e939", "#b2ff2e")
pal12 <- c("#202547", "#43444a", "#5f654a", "#7b8948", "#97b043", 
           "#b2d736", "#ceff1a", "#d8e01b", "#dfc11b", "#e2a11b", 
           "#e37f1b", "#e1581a", "#de1a1a")

###################################################
#### chloropleth map of richness by ecoregion ####

# data on lep taxonomy
lep.info<-read_excel('~/Dropbox/Projects/canada/raw data/212 Lep species.xlsx')%>%
  distinct(HERB_ID_SP, HERB_family_2019, feeding, SC)

# read in occurrences for all species
pts.files<-list.files('~/Dropbox/CFIS maps/pts')%>%word(1, sep='_')%>%unique()
pts.in<-do.call(rbind, lapply(pts.files, function(x)st_read(paste0('~/Dropbox/CFIS maps/pts/',x, '_pts.shp'))%>%
                                mutate(sp = x)%>%st_transform(lcc)))%>%
  st_intersection(outline)

pts.in$id<-seq(1:length(pts.in$id))
glimpse(pts.in)

ggplot()+
  geom_sf(data=pts.in, alpha=.1, size=.1)+
  theme_map()

## intersect collection sites with ecoregion data to calculate species richness
eco.pts<-pts.in%>%st_intersection(eco.all)
ecor.r.rich<-eco.pts%>%
  group_by(ecoregion, ecozone)%>%
  summarize(sp = length(unique(sp)))

# how many ecoregions in?
length(unique(ecor.r.rich$ecoregion)) #115 from 192 possible

## plot chloropleth
# join species richness data to province shapefiles
ecor.rich<-eco.all%>%
  group_by(ecoregion)%>%summarize()%>%
  left_join(ecor.r.rich%>%st_drop_geometry)%>%
  mutate(AREA = set_units(st_area(.),km2), dens=as.numeric(sp/AREA)*1000) # get area of each region to calc sp density
glimpse(ecor.rich)

## crop with bounding box around region where recorded
bbox.pts<-st_bbox(ecor.rich%>%filter(!is.na(sp)))
ecor.rich.bbox<-ecor.rich%>%st_crop(bbox.pts)
outline.bbox<-outline%>%st_crop(bbox.pts)

### PLOT BY ECOREGION
guide_opts<-guide_colorbar(direction='horizontal', label.position='bottom', barwidth=14, title.position='top', fill=NA)

ggplot()+
  geom_sf(data=ecor.rich, aes(fill=dens), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis(name='Caterpillar species density\n(per 1000 km2)', na.value='lightgrey', trans='log1p',begin=0.1, end=0.95, guide=guide_opts)+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggsave('~/Dropbox/CFIS maps/maps/ecoprovince_density.pdf', width=7, height=5)

# raw richness
ggplot()+
  geom_sf(data=ecor.rich, aes(fill=sp), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis(name='Caterpillar species richness', na.value='lightgrey', begin=0.1, end=0.95, guide=guide_opts)+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggsave('~/Dropbox/CFIS maps/maps/ecoprovince_rich.pdf', width=7, height=5)

###################################################
#### rasterize ! #####
library(raster)

## make a series of maps with varying raster resolution and color schemes

# what is appropriate scale for analysis?
# how many total sites are there when points are spatially aggregated?
# can we project richness estimates across the whole country?
# what is the range size of each species?

# input resolution in km
make_rast_rich<-function(res){
  res<-res*1000
  rast.r<-raster(outline.bbox) # basic raster grid
  res(rast.r)<-res # set resoltion
  rich.r<-rasterize(pts.in, rast.r, 'sp', function(x, ...) length(unique(na.omit(x)))) # populate with species richness
  return(rich.r)
}

#make_rast_rich(res=30)
res.list<-c(10, 12, 15, 20, 25, 30, 50, 75) #units reflect km dimensions of raster
rast.rich.list<-lapply(res.list, make_rast_rich)

# how many cells in each res? effectively sites in this case
lapply(rast.rich.list, function(x)na.omit(values(x))%>%length)

### make nice plot of rasterized species richness
rast.df<-lapply(rast.rich.list, function(x)as.data.frame(x, xy=TRUE))%>%
  setNames(res.list)%>%
  bind_rows(.id='res')%>%mutate(res = paste0(res, ' x ', res, ' km')) # convert for plotting

## plot rasters
pal_num<-colorRampPalette(pal12)

ggplot()+
  geom_raster(data=rast.df, aes(x=x,y=y, fill=layer), alpha=.9)+
  geom_sf(data=outline.bbox, fill=NA, color='black')+
  scale_fill_gradientn(colors=c('turquoise','darkslateblue'), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60,100),guide=guide_opts)+
  theme_map(base_size=16)+
  theme(legend.position='top', strip.background = element_rect(color=NA, fill=NA))+
  facet_wrap(~res, ncol=2)

ggsave('~/Dropbox/CFIS maps/caterpillar richness/raster_series_blue.pdf', width=16, height=28)


## experimenting with contouring
## with contouring
ggplot()+
  geom_raster(data=df.50, aes(fill=layer, x=x, y=y))+
  geom_sf(data=outline.bbox, fill=NA, color='black')+
  scale_color_gradientn(colors=rev(pal_num(10)), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60, 100),guide=guide_opts)+
  scale_fill_gradientn(colors=rev(pal_num(10)), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60, 100),guide=guide_opts)+
  theme_map()
ggsave('~/Dropbox/CFIS maps/maps/contour_25_rich.pdf', width=7, height=5)


####################################################
#### determine number of sites ####
# this is going to vary with resolution

res<-50
res<-res*1000
rast.r<-raster(outline.bbox) # basic raster grid
res(rast.r)<-res # set resoltion

make_grid<-function(res){
  res<-res*1000
  rast.r<-raster(outline.bbox) # basic raster grid
  res(rast.r)<-res # set resoltion
  return(rast.r)
}

## do for each species individually to retain info on composition, combine
cat.sps<-unique(pts.in$sp)

get_sites<-function(cat, rast.r){
  sp.r<-rasterize(pts.in%>%filter(sp == cat), rast.r, 'sp', function(x, ...) length(x))%>%
    as.data.frame(xy=TRUE)%>%
    mutate(sp = cat)%>%rename(sites=layer)
  return(sp.r)
}


res<-20
res<-res*1000
rast.r<-raster(outline.bbox) # basic raster grid
res(rast.r)<-res
cat.sites<-lapply(cat.sps, get_sites)%>%bind_rows%>%
  dcast(x+y~sp, value.var='sites') # cast to wide format
cat.mat<-cat.sites%>%dplyr::select(-x,-y)
cat.sites<-cat.sites%>%mutate(sp_rich=rowSums(cat.mat, na.rm=TRUE))%>%filter(sp_rich != 0)

write.csv(cat.sites, '~/Dropbox/Projects/canada/R/caterpillar_sites_20km.csv', row.names=FALSE)

## do this for multiple resolutions

####################################################
# rarefied diversity in each site using the number of sites and the number of species

pal_num<-colorRampPalette(pal2)
pal_num(3)

## species distriution probability

library(unmarked)

