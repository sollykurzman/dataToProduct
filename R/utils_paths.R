create_curved_path <- function(lon1, lat1, lon2, lat2, num_points = 50) {
  
  gcircle <- function(lon1, lat1, lon2, lat2, n) {
    lon1_rad <- lon1 * pi / 180
    lat1_rad <- lat1 * pi / 180
    lon2_rad <- lon2 * pi / 180
    lat2_rad <- lat2 * pi / 180
    
    d <- 2 * asin(sqrt((sin((lat1_rad - lat2_rad) / 2))^2 + 
                         cos(lat1_rad) * cos(lat2_rad) * 
                         (sin((lon1_rad - lon2_rad) / 2))^2))
    
    if (d == 0) 
      return(data.frame(lon = lon1, lat = lat1))
    
    f <- seq(0, 1, length.out = n)
    
    A <- sin((1 - f) * d) / sin(d)
    B <- sin(f * d) / sin(d)
    
    x <- A * cos(lat1_rad) * cos(lon1_rad) + B * cos(lat2_rad) * cos(lon2_rad)
    y <- A * cos(lat1_rad) * sin(lon1_rad) + B * cos(lat2_rad) * sin(lon2_rad)
    z <- A * sin(lat1_rad) + B * sin(lat2_rad)
    
    lat <- atan2(z, sqrt(x^2 + y^2))
    lon <- atan2(y, x)
    
    data.frame(
      lon = lon * 180 / pi,
      lat = lat * 180 / pi
    )
  }
  
  gcircle(lon1, lat1, lon2, lat2, num_points)
}
