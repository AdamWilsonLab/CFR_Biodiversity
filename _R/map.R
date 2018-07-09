## Process ecosystem data for web map

library(rgdal)
library(leaflet)
library(rgeos)
library(htmltools)
library(htmlwidgets)
library(raster)
library(dplyr)
library(ggplot2)
library(sf)
library(ggmap)
library(ggsn)
library(units)

GM="+init=epsg:3857" # google mercator projection
wproj="+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # working projection 
wgs84="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

eco=readOGR("_data/vegm2006_biomes_withforests/vegm2006_biomes_withforests.shp")
#eco=subset(eco,BIOMENAME%in%c("Fynbos","Succulent Karoo","Nama-Karoo"))
eco$BIOMENAME=as.character(eco$BIOMENAME)
eco=eco[!eco$POLYGONID%in%c(1,2,4,17,19,53,69,180,236,285,286),] #remove some forests that are not in the gcfr.  This list was created by looking at the shapefile in qgis.

#eco$type="Terrestrial"
#eco=eco%>%st_as_sf()

eco$color=viridis::inferno(nrow(eco), begin=0.8)
ecos=gUnionCascaded(eco,id=eco$BIOMENAME)#%>%
#  gSimplify(0.001,topologyPreserve = T)
ecosd=SpatialPolygonsDataFrame(ecos,data.frame(name=names(ecos),row.names = names(ecos)))%>%st_as_sf()
ecosd$type="Terrestrial"
ecosd$color=viridis::inferno(nrow(ecosd), begin=0.5)

eproj=st_crs(ecosd)

mar=readOGR("_data/NBA_marine_benthiccoastal_habitattypes/NBA_benthic_and_coastal_habitat.shp")
#mar=subset(mar,!Biogeogr%in%c("Southeast Atlantic","South Atlantic","Southwest Indian","Natal","Natal-Delagoa","Delagoa","Harbour"))
mar$Biogeogr=as.character(mar$Biogeogr)
mars=gUnionCascaded(mar,id=mar$Biogeogr)#%>%
#  gSimplify(0.001,topologyPreserve = T)
marsd=SpatialPolygonsDataFrame(mars,data.frame(name=names(mars),row.names = names(mars)))%>%
  st_as_sf()%>%
  st_set_crs(eproj)
marsd$type="Marine"
marsd$color=viridis::viridis(nrow(marsd),begin = 0.7, end=0.1)

## old cfr boundary
cfr=readOGR("/Users/adamw/Documents/repos/ProteaAtlas/inst/extdata/CFR_env/CFR.shp")%>%
  st_as_sf()%>%
#  st_buffer(dist=1,nQuadSegs=100)%>%
  st_set_crs(eproj)

## Build new GCFR boundary
gcfr=filter(ecosd,
            name%in%c("Forests","Fynbos","Succulent Karoo"))%>%
  st_buffer(.001)%>%
  st_combine()%>%
  st_union()%>%
  st_sf()%>%
  st_set_crs(eproj)

write_sf(gcfr,"_data/gcfr.shp")

# get bbox for region

gcfr_bbox=gcfr%>%
  st_buffer(dist=0.4)%>%
#  st_as_sfc()%>%
  st_set_crs(eproj)

gcfr_bbox2=st_bbox(gcfr_bbox); names(gcfr_bbox2)=c("left", "bottom", "right", "top")

# Create Buffered CFR polygon for core region
bcfr=cfr%>%
  st_segmentize(20000)%>%
  st_transform(wproj)%>%
  st_buffer(dist=20000,nQuadSegs=1000)%>%
  st_transform(wgs84)

## ROIs
roi1=st_as_sfc("POLYGON((18.2 -33.7,19.5 -33.7,19.5 -34.6,18.2 -34.6,18.2 -33.7))",crs=4326)%>%
  st_sf()%>% st_set_crs(eproj)
roi2=st_as_sfc("POLYGON((17 -32,21 -32,21 -35,17 -35,17 -35,17 -32))",crs=4326)%>%
  st_sf()%>%
  st_set_crs(eproj)

### ROI Areas
#roi1%>%st_transform()%>%st_area()

cfrp=st_coordinates(cfr)#@polygons[[1]]@Polygons[[1]]@coords
  


## Combine and crop to GCFR
biomes=rbind(ecosd,marsd)%>%
  st_intersection(gcfr_bbox)#%>%
#  st_simplify(dTolerance=0.001)
#biomes_all=gUnaryUnion(biomes)

## Protected areas
sacad=readOGR("_data/SACAD_OR_2017_Q1/SACAD_OR_2017_Q1.shp")
sacad2=sacad[sacad$SITE_TYPE%in%c("Biosphere Reserve","Ramsar Site"),]%>%
  gUnionCascaded(id="SITE_TYPE")%>%
  gSimplify(0.001,topologyPreserve = T)%>%
    st_as_sfc()%>%
  st_sf()

sapad_raw=st_read("_data/SAPAD_OR_2017_Q1/SAPAD_OR_2017_Q1.shp")%>%
  st_set_crs(st_crs(gcfr_bbox))%>%
  st_buffer(0)

gcfr_buffer=st_buffer(gcfr,0.5) 
sapad=filter(sapad_raw,st_intersects(sapad_raw, gcfr, sparse = FALSE))

#  gUnionCascaded(id="CUR_NME")%>%
#  gSimplify(0.001,topologyPreserve = T)#%>%
#  st_as_sfc()%>%
#  st_sf()

# export table mountain polygon
#tmnp=read_sf("_data/SAPAD_OR_2017_Q1/SAPAD_OR_2017_Q1.shp")%>%
#  filter(CUR_NME=="Table Mountain National Park")%>%
#  st_simplify(dTolerance=0.001)
#st_write(tmnp,"~/Downloads/",layer="tmnp",driver="ESRI Shapefile")

#"https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}.png"
#"http://tile.stamen.com/terrain-background/{z}/{x}/{y}.png"
lc=st_coordinates(st_centroid(biomes$geom))

lb=leaflet(biomes)%>% 
  addTiles(urlTemplate = "https://stamen-tiles-{s}.a.ssl.fastly.net/terrain-background/{z}/{x}/{y}.png")%>%
  addPolygons(
    stroke = T, fillOpacity = .5, smoothFactor = 0.5,
    #fillColor = ~colorFactor("Paired",name)(name),
    color="black", weight=0.2,
    fillColor = biomes$color,
    popup=~htmlEscape(name))%>%
  addPolygons(
    lat=st_coordinates(roi1)[,1],
    lng=st_coordinates(roi1)[,2],
    stroke = T, fillOpacity = 0,
    color="red", weight=3)
lb

saveWidget(lb, file="map.html")



##
# ggplot stamen
base <- get_stamenmap(gcfr_bbox2, zoom = 7, maptype = "toner-lite")
base2 <- get_stamenmap(gcfr_bbox2, zoom = 9, maptype = "toner-lite")

base3=dismo::gmap(extent(gcfr), type='terrain', scale=2, zoom=7,  lonlat=T)

theme_set(theme_bw(28))

gcfrmap = ggmap(base)
  

gcfrmap2=gcfrmap+
  geom_sf(data=biomes,inherit.aes = F,aes(fill=name,color=type),color=NA,alpha=.4)+
#  geom_sf(data=cfr,inherit.aes = F,fill=NA,color="black",linetype="dashed")+
  geom_sf(data=gcfr,inherit.aes = F,fill=NA,color="black")+
  scale_fill_manual(values=biomes$color,name="Biome")+
  guides(fill=guide_legend(ncol=2))+
  coord_sf(
    ylim=c(gcfr_bb[c("bottom","top")]),
    xlim=c(gcfr_bb[c("left","right")]))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme(legend.position=c(.75,.8))
#theme(legend.position="bottom",legend.position=c(.9,.75))

png("img/biomes.png",width=1200,height=1000)
gcfrmap2
dev.off()


### Protected areas
gcfrmap_protected=gcfrmap+
  geom_sf(data=sapad,aes(fill=SITE_TYPE),inherit.aes = F)+
  #  geom_sf(data=cfr,inherit.aes = F,fill=NA,color="black",linetype="dashed")+
  geom_sf(data=gcfr,inherit.aes = F,fill=NA,color="black")+
  scale_fill_viridis_d(name="Protected\nArea Type")+
  coord_sf(
    ylim=c(gcfr_bb[c("bottom","top")]),
    xlim=c(gcfr_bb[c("left","right")]))+
  ylab("Latitude")+
  xlab("Longitude")+
  theme(legend.position=c(.75,.8))
#theme(legend.position="bottom",legend.position=c(.9,.75))

png("img/protected.png",width=1200,height=1000)
gcfrmap_protected
dev.off()


### ROI map
### 
f1=makeFlight(x=3750/8,y=14*8,angle=0)%>%
  st_transform(eproj)

#f2=makeFlight(origin=c(17.75,-34.9),x=2*3750/10,y=14*2.5*10,angle=0)%>%
#  st_transform(eproj)


plot(f1)

gcfrmap2 = ggmap(base2)

gcfrmap_roi=gcfrmap+
  geom_sf(data=f1,inherit.aes = F,fill=NA,col="red",linetype="dotted",size=1.5)+
  geom_sf(data=bcfr,inherit.aes = F,fill=NA,col="blue",size=2)+
  geom_sf(data=sapad,inherit.aes = F,col=NA,fill="darkgreen",alpha=.5)+
  geom_sf(data=gcfr,inherit.aes = F,fill=NA,color="black")+
#  coord_sf(
#    ylim=c(-34.9,-33.5),
#    xlim=c(18,20))+
  ylab("Latitude")+
  xlab("Longitude")
gcfrmap_roi

png("img/roi.png",width=1200,height=1000)
gcfrmap_roi
dev.off()

# Summary stats

roi_area=st_area(bcfr)
units(roi_area)<-with(ud_units,km^2);roi_area

sapad_roi=sapad[unlist(st_intersects(bcfr,sapad)),]
summary(sapad_roi)

table(sapad_roi$SITE_TYPE)

filter(sapad_roi,SITE_TYPE=="Nature Reserve")$CUR_NME
filter(sapad_roi,SITE_TYPE=="World Heritage Site")$CUR_NME
filter(sapad_roi,SITE_TYPE=="Marine Protected Area")$CUR_NME
