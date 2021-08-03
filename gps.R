require(sf)
require(ggplot2)
require(ggmap)
devtools::install_github("r-spatial/lwgeom")
require(lwgeom)
require(viridis)

#trail <-readOGR("gpx/Wilsons Promontory - South Eastern Circuit.gpx", layer = "track_points")
trail <-sf::st_read("gpx/Wilsons Promontory - South Eastern Circuit.gpx", layer = "track_points")


trail_co<-as.data.frame(st_coordinates(trail))
trail_co$Elevation<-trail$ele


cl<-viridis(trail_co$Elevation)
b<-make_bbox(lon = trail_co$X, lat=trail_co$Y, f=0.1)
map<-get_map(location = b, maptype = "satellite", source = "osm")

start<-trail_co[1,]
end<-trail_co[nrow(trail_co),]

png("gps_path.png", width = 960, height = 540)
ggmap(map) + 
  geom_point(data = trail_co, mapping = aes(x = X, y = Y, colour=Elevation)) +
  geom_point(data = start, mapping = aes(x = X, y = Y), colour="green", alpha=0.3, size = 4) +
  geom_point(data = end, mapping = aes(x = X, y = Y), colour="red", alpha=0.3, size = 4) +
  labs(x = "Longitude", y = "Latitude", colour = "Ëlevation (m)")
dev.off()

#gsf <- readOGR("layer/sg_geological_unit_250k")
gsf <- sf::st_read("layer/sg_geological_unit_250k")
#bsf <- readOGR("layer/nv2005_evbcs")
bsf <- sf::st_read("layer/nv2005_evbcs")

trail<-st_transform(trail, st_crs(gsf))

#trail<-st_cast(trail, "LINESTRING")
d<-sf::st_distance(trail)

acc<-numeric(nrow(d))
for (i in 1:(nrow(d)-1)){
  acc[i+1]<-(as.numeric(d[i+1,i])+acc[i])  
}

trail$Cummlative_distance<-acc

ggplot(data = trail, aes(x=Cummlative_distance, y=ele, colour=ele)) + 
  geom_path(size=2) + 
  theme(legend.position='none')

sf::sf_use_s2(FALSE)
trail<-sf::st_join(trail, gsf, suffix=c("","GEO"))
trail<-sf::st_join(trail, bsf, suffix=c("","GEO"))


geo_blocks <- data.frame()
dis <- 0
start <- trail[1,]$ID
for(i in 1:nrow(trail)-1){
  if( !(trail[i+1,]$ID %in% start) ) {
    geo_blocks <- rbind(geo_blocks, data.frame(start, dis, trail[i,]$Cummlative_distance, trail[i,]$DESC ))
    dis <- trail[i+1,]$Cummlative_distance
    start <- trail[i+1,]$ID
  }
}
geo_blocks <- rbind(geo_blocks, data.frame(start, dis, trail[i,]$Cummlative_distance, trail[i,]$DESC ))
names(geo_blocks) <- c("ID","START", "END", "DESC")

geo_blocks$DESC<-as.factor(geo_blocks$DESC)

c_g <- heat.colors(nlevels(geo_blocks$DESC))

bio_blocks <- data.frame()
dis <- 0
start <- trail[1,]$EVC
for(i in 1:nrow(trail)-1){
  if( !(trail[i+1,]$EVC %in% start) ) {
    bio_blocks <- rbind(bio_blocks, data.frame(start, dis, trail[i,]$Cummlative_distance, trail[i,]$X_EVCNAME ))
    dis <- trail[i+1,]$Cummlative_distance
    start <- trail[i+1,]$EVC
  }
}
bio_blocks <- rbind(bio_blocks, data.frame(start, dis, trail[i,]$Cummlative_distance, trail[i,]$X_EVCNAME ))
names(bio_blocks) <- c("ID","START", "END", "DESC")

bio_blocks$DESC<-as.factor(bio_blocks$DESC)

c_b <- terrain.colors(nlevels(bio_blocks$DESC))


png("zones.png", width=960, height=540)
ggplot(data = trail, aes(x=Cummlative_distance, y=ele, colour=ele)) + 
  geom_path(size=2) + 
  annotate("rect", xmin = geo_blocks$START, xmax = geo_blocks$END, ymin = -30, ymax = -5, fill = c_g[geo_blocks$DESC]) +
  annotate("rect", xmin = bio_blocks$START, xmax = bio_blocks$END, ymin = -75, ymax = -45, fill = c_b[bio_blocks$DESC]) +
  annotate("text", label = "Geological Zones", x = 4000, y = 2.5)  +
  annotate("text", label = "Bioregion Zones", x = 4000, y = -38.5)  +
  theme(legend.position = "none") +
  labs(y = "Elevation (m)", x = "distance covered (m)")
dev.off()

   





