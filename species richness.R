

## plot combined sf object

# color points by species
library(viridis)
library(sf)
library(rmapshaper)
library(ggthemes)
library(tidyverse)
library(units)
library(jcolors)

### making chlorpleth maps

# read in ecoregion shapefiles
list.files('~/Dropbox/CFIS maps/shapefiles')

# Canada Lambert conformal conic
lcc<-'+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# canada country boundary
can.simp<-st_read('/Users/collnell/Dropbox/CFIS maps/canada map/can_simplen.shp')%>%st_transform(lcc) # simplified outline of canada
outline<-can.simp%>%group_by()%>%summarize()%>%ms_simplify(keep=0.01)

ggplot()+
  geom_sf(data=outline, fill=NA)+
  theme_map()


ggsave('~/Dropbox/CFIS maps/maps/ecozone.pdf', width=7, height=5)

# vegetation delineations

ecoz<-st_read('/Users/collnell/Dropbox/Projects/canada/Ecozones/Ecozones.shp')%>%
  dplyr::select(-ZONE_NOM)%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecozone=ZONE_NAME)# convert to Canada Lambert Conformal Conic CRS
ecor<-st_read('~/Dropbox/CFIS maps//Ecoregions/ecoregions.shp')%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecoregion=REGION_NAM) ## the ecoregion delinations
ecop<-st_read('~/Dropbox/CFIS maps/Ecoprovinces/ecoprovinces.shp')%>%st_transform(lcc)%>%ms_simplify(keep=0.15)%>%st_intersection(outline)%>%select(ecoprovince=ECOPROVINC)
glimpse(ecoz)

## combine all 3 in the hierarchjy
eco.all<-ecor%>%st_join(ecop)%>%st_join(ecoz)%>%
  dplyr::select(ecoprovince=ECOPROVINC, ecoregion=REGION_NAM, ecozone=ZONE_NAME)%>%
  group_by(ecozone, ecoregion)%>%summarize()
glimpse(eco.all)

ggplot()+
  geom_sf(data=eco.all, aes(fill=ecozone), color='white',size=.5)+
  geom_sf(data=outline, fill=NA, color='black')+
  theme_map()+
  theme(legend.position='top')+
  scale_fill_manual(values=pal_new)

display_all_jcolors()
display_all_jcolors_contin()
display_jcolors('pal8')
jcolors_contin
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

pal_new<-c('moderate_violet','very_dark_cyan','slightly_desaturated_yellow','moderate_blue','dark_moderate_lime_green','very_dark_pink',
           'slightly_desaturated_cyan','dark_moderate_blue','fluorescent_orange','mardi_gras','tiffany_blue','blue_yonder','raspberry','vivid_orange','vivid_yellow')

# associate caterpillar richness with map ecoregions

# map ecoregions based on number of species

## maps species richness
provs<-st_read('~/Dropbox/CFIS maps/caterpillar_sites_ecoprovince.shp')%>%st_transform(lcc)%>%select(sp, ecoprovince) # sites categorized by points
length(unique(provs$ecoprovince)) ## 35 ecoprovinces ocurring in
glimpse(provs)
## almost 50,000 points

prov.sr<-provs%>%
  st_drop_geometry()%>%
  group_by(ecoprovince)%>%
  summarize(sp_rich = length(unique(sp)), sites=length(sp))

ggplot(prov.sr)+geom_histogram(aes(x=sp_rich), bins=20) #
range(prov.sr$sp_rich)# ranges from 1 to 188
#prov.sr%>%View

## need more info abotu provinces - what is cliamte like there?
# general ecotype - ecozone

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


## join species richness data to province shapefiles
cp.agg<-ecop%>%group_by(ecoprovince)%>%summarize()%>%st_transform(lcc)%>%left_join(prov.sr)%>%
  mutate(AREA = set_units(st_area(.),km2), sp_area=as.numeric(sp_rich/AREA)*1000)
glimpse(cp.agg)

## crop with bounding box aroundd provinces of interest
bbox.prov<-st_bbox(cp.agg%>%filter(!is.na(sp_rich)))

cp.agg<-cp.agg%>%st_crop(bbox.prov)


### PLOT BY ECOPROVINCE
ggplot()+
  geom_sf(data=cp.agg, aes(fill=sp_area), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis( name='Caterpillar species density\n(per 1000 km2)', na.value='lightgrey', trans='log1p',begin=0.1, end=0.95, 
                      guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top', fill=NA))+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggplot()+
  geom_sf(data=cp.agg, aes(fill=sp_area), color=NA, size=.3)+
  theme_map()+
  scale_fill_jcolors_contin('pal12')+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggsave('~/Dropbox/CFIS maps/maps/ecoprovince_density.pdf', width=7, height=5)

# raw richness
ggplot()+
  geom_sf(data=cp.agg, aes(fill=sp_rich), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis( name='Caterpillar species richness', na.value='lightgrey', begin=0.1, end=0.95, 
                      guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top'))+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggplot()+
  geom_sf(data=cp.agg, aes(fill=sp_rich), color=NA, size=.3)+
  theme_map()+
  scale_fill_jcolors_contin('pal12')+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggsave('~/Dropbox/CFIS maps/maps/ecoprovince_rich.pdf', width=7, height=5)


## ecoregions
glimpse(ecor)
ecor.cat<-provs%>%st_intersection(ecor)
glimpse(ecor.cat)

ecor.rich<-ecor%>%
  group_by(ecoregion)%>%summarize()%>%st_transform(lcc)%>%
  left_join(ecor.cat%>%st_drop_geometry()%>%group_by(ecoregion)%>%summarize(sp_rich = length(unique(sp)), sites=length(sp)))%>%
  mutate(AREA = set_units(st_area(.),km2), sp_area=as.numeric(sp_rich/AREA)*1000)%>%
  st_crop(bbox.prov)
glimpse(cp.agg)

ggplot()+
  geom_sf(data=ecor.rich, aes(fill=sp_area), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis( name='Caterpillar species density\n(per 1000 km2)', trans='log1p',na.value='lightgrey', begin=0.1, end=0.95, 
                      guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top'))+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggplot()+
  geom_sf(data=outline%>%st_crop(bbox.prov), fill=NA, color='black', size=.4)+
  geom_sf(data=ecor.rich%>%filter(!is.na(sp_area)), aes(fill=log(1+sp_area)), color='white', size=.2)+
  geom_sf(data=outline%>%st_crop(bbox.prov), fill=NA, color='black', size=.25)+
  theme_map()+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())+
  scale_fill_jcolors_contin('pal3', bias=2.35, reverse=TRUE)

ggsave('~/Dropbox/CFIS maps/maps/ecoregion_density.pdf', width=7, height=5)

## raw richenss 
ggplot()+
  geom_sf(data=ecor.rich, aes(fill=sp_rich), color=NA, size=.3)+
  theme_map()+
  scale_fill_viridis( name='Caterpillar species richness',na.value='lightgrey', begin=0.1, end=0.95, 
                      guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top'))+
  theme(legend.position=c(.05,0.01), legend.background = element_blank())

ggsave('~/Dropbox/CFIS maps/maps/ecoregion_rich.pdf', width=7, height=5)


## rasterize sites

# data need to be projected to an equal-area coordinate ststem
# in lat/lon cells of same degree get smaller towards equator

glimpse(provs) # species points

# distribution of species richness via raster
# empty grid of correct extent and resolution
library(raster)

## make a series of maps with varying resolution

# input resolution in km
make_rast_rich<-function(res){
  res<-res*1000
  rast.r<-raster(outline%>%st_crop(bbox.prov)) # basic raster grid
  res(rast.r)<-res # set resoltion
  rich.r<-rasterize(provs, rast.r, 'sp', function(x, ...) length(unique(na.omit(x)))) # populate with species richness
  return(rich.r)
}

make_rast_rich(res=30)
res.list<-c(10, 12, 15, 20, 25, 30, 50, 75)
rast.rich.list<-lapply(res.list, make_rast_rich)
str(rast.rich.list)


# how many sites?
lapply(rast.rich.list, function(x)na.omit(values(x))%>%length)

### nice plot of rasterized species richness
rast.df<-lapply(rast.rich.list, function(x)as.data.frame(x, xy=TRUE))
names(rast.df)<-res.list
rast.df<-bind_rows(rast.df, .id='res')
out.crop<-outline%>%st_crop(bbox.prov)


ggplot()+
  geom_raster(data=rast.df%>%mutate(res = paste0(res, ' x ', res, ' km')), aes(x=x,y=y, fill=layer), alpha=.9)+
  geom_sf(data=out.crop, fill=NA, color='black')+
  scale_fill_gradientn(colors=rev(pal_purp(10)), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60,100),
                       guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=15, title.position='top'))+
  theme_map(base_size=16)+
  theme(legend.position='top', strip.background = element_rect(color=NA, fill=NA))+
  facet_wrap(~res, ncol=2)

ggsave('~/Dropbox/CFIS maps/maps/raster_series_10_75_pal8.pdf', width=16, height=30)

## with contouring
ggplot()+
  geom_raster(data=df.50, aes(fill=layer, x=x, y=y))+
  geom_sf(data=outline%>%st_crop(bbox.prov), fill=NA, color='black')+
  scale_color_gradientn(colors=rev(pal_purp(10)), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60, 100),
                        guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top'))+
  scale_fill_gradientn(colors=rev(pal_purp(10)), na.value=NA, trans='log', name='species\nrichness', breaks=c(1,5,10,20,40,60, 100),
                        guide=guide_colorbar(direction='horizontal', label.position='bottom', barwidth=7, title.position='top'))+
  theme_map()
ggsave('~/Dropbox/CFIS maps/maps/contour_25_rich.pdf', width=7, height=5)

# rarefied diversity in each site using the number of sites and the number of species

colorRampPalette(pal2)


## species distriution probability

library(unmarked)

