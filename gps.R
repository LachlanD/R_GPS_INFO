require(sf)
require(ggplot2)
require(ggmap)
devtools::install_github("r-spatial/lwgeom")
require(lwgeom)

#trail <-readOGR("gpx/Wilsons Promontory - South Eastern Circuit.gpx", layer = "track_points")
trail <-sf::st_read("gpx/Wilsons Promontory - South Eastern Circuit.gpx", layer = "track_points")


trail_co<-as.data.frame(st_coordinates(trail))
trail_co$A<-trail$ele


cl<-viridis(trail_co$A)
b<-make_bbox(lon = trail_co$X, lat=trail_co$Y, f=0.1)
map<-get_map(location = b, maptype = "satellite", source = "osm")

ggmap(map) + geom_point(data = trail_co, mapping = aes(x = X, y = Y, colour=A))


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

trail_co$C<-acc

ggplot(data = trail_co, aes(x=C, y=A, colour=A)) + geom_path(size=2)

sf::sf_use_s2(FALSE)
trail<-sf::st_join(trail, gsf, suffix=c("","GEO"))

geo<-factor(t$DESC)



png("gps_geology.png", height=1080, width=1920)
plot(x=acc, y=t$ele, col=cl[geo], type="p", pch=16)
legend("topright",legend=levels(geo), col=cl, fill=cl)
dev.off()




