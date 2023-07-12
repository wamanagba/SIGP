library(sf)
a_single_point <- st_point(x = c(1,3)) # a single point with XY coordinates of 1,3 in unspecified coordinate system
a_single_point # view the content of the object

#create five sfg point objects
point1 <- st_point(x = c(1,3)) 
point2 <- st_point(x = c(2, 4))
point3 <- st_point(x = c(3, 3))
point4 <- st_point(x = c(4, 3))
point5 <- st_point(x = c(5, 2))

points <- st_sfc(point1,point2,point3,point4,point5) # create a single sfc points object

points


points_wgs <- st_sfc(point1,point2,point3,point4,point5,crs=4326) # create a single sfc points object, and define the CRS

points_wgs

plot(points_wgs,col="red",pch=16) # map the sfc object using the plot function


points_attribute_data <- data.frame(transport_mode = c("Bicycle","Pedestrian","Motor Vehicle","Motor Vehicle","Motor Vehicle"))

points_attribute_data


points_sf <- st_sf(points_attribute_data,geometry=points)

points_sf
plot(points_sf,pch=16)







a_single_line_matrix <- rbind(c(1,1), 
                              c(2, 4), 
                              c(3, 3), 
                              c(4, 3), 
                              c(5,2))# create a matrix with 

a_single_line_matrix
a_single_line <-  st_linestring(a_single_line_matrix)
a_single_line
plot(a_single_line,col="darkblue")



line1 <- st_linestring(rbind(c(1,1), 
                             c(2,4), 
                             c(3,3), 
                             c(4,3), 
                             c(5,2)))

line2 <- st_linestring(rbind(c(5,2), 
                             c(5,3), 
                             c(4,5)))

line3 <- st_linestring(rbind(c(4,5), 
                             c(-1,4), 
                             c(1,1)))

lines <- st_sfc(line1,line2,line3,crs=4326)

lines
plot(lines)




line_attribute_data <- data.frame(road_name = c("Vinmore Avenue","Williams Road","Empress Avenue"), 
                                  speed_limit = c(30,50,40))

line_attribute_data
lines_sf <- st_sf(line_attribute_data,geometry=lines)

lines_sf


plot(lines_sf)
plot(lines_sf[2])






# Polyone


a_polygon_vertices_matrix <- rbind(c(1,1), c(2, 4), c(3, 3), c(4, 3), c(5,2),c(1,1))
a_polygon_list = list(a_polygon_vertices_matrix)
a_polygon_list

a_polygon <-  st_polygon(a_polygon_list)

plot(a_polygon,col="forestgreen")



lake_vertices_matrix <- rbind(c(1.5, 1.5),c(1.5,1.75),c(1.75,1.75),c(1.75,1.5),c(1.5,1.5))
lake_vertices_matrix
a_single_polygon_with_a_hole_list = list(a_polygon_vertices_matrix,lake_vertices_matrix)
a_single_polygon_with_a_hole_list


a_single_polygon_with_a_hole <-  st_polygon(a_single_polygon_with_a_hole_list)

plot(a_single_polygon_with_a_hole,col="forestgreen")



park1 <- a_single_polygon_with_a_hole
park2 <- st_polygon(list(
  rbind(
    c(6,6),c(8,7),c(11,9), c(10,6),c(8,5),c(6,6)
  )
))

park_attributes <- data.frame(park_name = c("Gilmore","Dixon"))

parks_sf <- st_sf(park_attributes, geometry = st_sfc(park1,park2,crs=4326))

plot(parks_sf)
