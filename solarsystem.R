library(magicaxis)
library(celestial)


# convert a Gregorian date into Julian day number
ut_to_julian = Vectorize(function(year=2000,month=1,day=1,hour=0,min=0) {
  date = sprintf('%d/%d/%d',floor(day),month,year)
  jd = julian(as.Date(date, "%d/%m/%Y"),-2440588)[[1]]+day%%1-0.5+hour/24+min/24/60
  return(jd)
})
  
# get planet positions in ecliptic cartesian coordinates at a particular Julian date
planet.coordinates = function(jd, planets=1:9, elements=1) {
  
  # Set Keplerian orbital elements from NASA JPL
  # https://ssd.jpl.nasa.gov/planets/approx_pos.html
  
  if (elements==1) {
    
    # Valid for 1800 AD to 2050 AD (Table 1)
    elements = rbind(c(0.38709927,0.20563593,7.00497902,252.25032350,77.45779628,48.33076593,0.00000037,0.00001906,-0.00594749,149472.67411175,0.16047689,-0.12534081,0,0,0,0),
                     c(0.72333566,0.00677672,3.39467605,181.97909950,131.60246718,76.67984255,0.00000390,-0.00004107,-0.00078890,58517.81538729,0.00268329,-0.27769418,0,0,0,0),
                     c(1.00000261,0.01671123,-0.00001531,100.46457166,102.93768193,0.0,0.00000562,-0.00004392,-0.01294668,35999.37244981,0.32327364,0.0,0,0,0,0),
                     c(1.52371034,0.09339410,1.84969142,-4.55343205,-23.94362959,49.55953891,0.00001847,0.00007882,-0.00813131,19140.30268499,0.44441088,-0.29257343,0,0,0,0),
                     #Need to determine asteroids features
                     c(3.52371034,0.09339410, 1.84969142, -4.55343205,-23.94362959,49.55953891,0.00001847,0.00007882,-0.00813131,19140.30268499,0.44441088,-0.29257343,0,0,0,0),
                     
                     c(5.20288700,0.04838624,1.30439695,34.39644051,14.72847983,100.47390909,-0.00011607,-0.00013253,-0.00183714,3034.74612775,0.21252668,0.20469106,0,0,0,0),
                     c(9.53667594,0.05386179,2.48599187,49.95424423,92.59887831,113.66242448,-0.00125060,-0.00050991,0.00193609,1222.49362201,-0.41897216,-0.28867794,0,0,0,0),
                     c(19.18916464,0.04725744,0.77263783,313.23810451,170.95427630,74.01692503,-0.00196176,-0.00004397,-0.00242939,428.48202785,0.40805281,0.04240589,0,0,0,0),
                     c(30.06992276,0.00859048,1.77004347,-55.12002969,44.96476227,131.78422574,0.00026291,0.00005105,0.00035372,218.45945325,-0.32241464,-0.00508664,0,0,0,0))
    #elements[3,1:6] = c(1.0000010178,0.0167086,,,102.9,)
    
  } else if (elements==2) {
  
    # Valid for 3000 BC to 3000 AD (Table 2)
    elements = rbind(c(0.38709843,0.20563661,7.00559432,252.25166724,77.45771895,48.33961819,0.00000000,0.00002123,-0.00590158,149472.67486623,0.15940013,-0.12214182,0,0,0,0),
                     c(0.72332102,0.00676399,3.39777545,181.97970850,131.76755713,76.67261496,-0.00000026,-0.00005107,0.00043494,58517.81560260,0.05679648,-0.27274174,0,0,0,0),
                     c(1.00000018,0.01673163,-0.00054346,100.46691572,102.93005885,-5.11260389,-0.00000003,-0.00003661,-0.01337178,35999.37306329,0.31795260,-0.24123856,0,0,0,0),
                     c(1.52371243,0.09336511,1.85181869,-4.56813164,-23.91744784,49.71320984,0.00000097,0.00009149,-0.00724757,19140.29934243,0.45223625,-0.26852431,0,0,0,0),
                     #Need to determine asteroids features
                     c(3.52371243,0.09336511,1.85181869,-4.56813164,-23.91744784,49.71320984,0.00000097,0.00009149,-0.00724757,19140.29934243,0.45223625,-0.26852431,0,0,0,0),
                     
                     c(5.20248019,0.04853590,1.29861416,34.33479152,14.27495244,100.29282654,-0.00002864,0.00018026,-0.00322699,3034.90371757,0.18199196,0.13024619,-0.00012452,0.06064060,-0.35635438,38.35125000),
                     c(9.54149883,0.05550825,2.49424102,50.07571329,92.86136063,113.63998702,-0.00003065,-0.00032044,0.00451969,1222.11494724,0.54179478,-0.25015002,0.00025899,-0.13434469,0.87320147,38.35125000),
                     c(19.18797948,0.04685740,0.77298127,314.20276625,172.43404441,73.96250215,-0.00020455,-0.00001550,-0.00180155,428.49512595,0.09266985,0.05739699,0.00058331,-0.97731848,0.17689245,7.67025000),
                     c(30.06952752,0.00895439,1.77005520,304.22289287,46.68158724,131.78635853,0.00006447,0.00000818,0.00022400,218.46515314,0.01009938,-0.00606302,-0.00041348,0.68346318,-0.10162547,7.67025000))
    
  } else {
    
    stop('elements must be 1 or 2')
    
  }
  
  # add planet names
  rownames(elements) = c('Mercury','Venus','Earth','Mars','Asteroid','Jupiter','Saturn','Uranus','Neptune')
  
  # add parameter names
  colnames(elements) = c('a0','e0','I0','L0','q0','O0','adot','edot','Idot','Ldot','qdot','Odot','b','c','s','f')
  # a0/adot: semi-major axis [au, au/cty]
  # e0/edot: eccentricity [-, 1/cty]
  # I0/Idot: inclination [deg, deg/cty]
  # L0/Ldot: mean longitude [deg, deg/cty]
  # q0/qdot: longitude of perihelion [deg, deg/cty]
  # O0/Odot: longitude of ascending node [deg, deg/cty]
  
  # Step 1
  # Compute the value of six Keplerian elements on given date
  t = (jd-2451545.0)/36525
  p = elements[planets,1:6]+t*elements[planets,7:12]
  colnames(p) = c('a','e','I','L','q','O')
  
  # Step 2
  # Compute the argument of perihelion w, and the mean anomaly M
  w = p[,'q']-p[,'O']
  k = pi/180
  M = p[,'L']-w+elements[planets,'b']*t^2+elements[planets,'c']*cos(elements[planets,'f']*t*k)+elements[planets,'s']*sin(elements[planets,'f']*t*k)
  M = (M+180)%%360-180 # so that -180<=M<=180
  
  # Step 3
  # Obtain the eccentric anomaly E from the solution of Kepler's equation
  estar = p[,'e']/k
  E = M-estar*sin(M*k)
  E = (E+180+360)%%360-180 # so that -180<=M<=180
  dE = 1
  while (max(dE)>1e-7) {
    dM = M-(E-estar*sin(E*k))
    dE = dM/(1-p[,'e']*cos(E*k))
    E = E+dE
  }
  
  # Step 4
  # Compute the planet's heliocentric coordinates in its orbital plane with the x'-axis aligned from the focus to the perihelion
  xp = p[,'a']*(cos(E*k)-p[,'e'])
  yp = p[,'a']*sqrt(1-p[,'e']^2)*sin(E*k)

  # Step 5
  # Compute the coordinates in the J2000 ecliptic plane, with the x-axis aligned toward the equinox
  x = (cos(w*k)*cos(p[,'O']*k)-sin(w*k)*sin(p[,'O']*k)*cos(p[,'I']*k))*xp+(-sin(w*k)*cos(p[,'O']*k)-cos(w*k)*sin(p[,'O']*k)*cos(p[,'I']*k))*yp
  y = (cos(w*k)*sin(p[,'O']*k)+sin(w*k)*cos(p[,'O']*k)*cos(p[,'I']*k))*xp+(-sin(w*k)*sin(p[,'O']*k)+cos(w*k)*cos(p[,'O']*k)*cos(p[,'I']*k))*yp
  z = sin(w*k)*sin(p[,'I']*k)*xp+cos(w*k)*sin(p[,'I']*k)*yp
  
  # NB: There is an optional step 6 to convert ecliptic coordinates to equatorial coordinates.
  # This is not needed here
  
  # Return data
  out = cbind(x=x,y=y,z=z)
  rownames(out) = rownames(elements)[planets]
  return(out)
  
}


# Function to convert RA and Dec to ecliptic coordinates
ra_dec_to_ecliptic <- function(ra, dec) {
  # Constants
  rad_per_deg <- pi / 180.0
  
  # Convert to radians
  ra_rad <- ra * rad_per_deg
  dec_rad <- dec * rad_per_deg
  
  # Ecliptic obliquity (mean value for J2000 epoch)
  obliquity_epsilon <- (23.439291 - 0.000130042 * (2000 - 2000)) * rad_per_deg
  
  # Calculate ecliptic coordinates
  sin_ecliptic_lat <- sin(dec_rad) * cos(obliquity_epsilon) +
    cos(dec_rad) * sin(ra_rad) * sin(obliquity_epsilon)
  ecliptic_lat <- asin(sin_ecliptic_lat) / rad_per_deg
  
  cos_ecliptic_lon <- cos(dec_rad) * cos(ra_rad)
  sin_ecliptic_lon <- cos(dec_rad) * sin(ra_rad) * cos(obliquity_epsilon) -
    sin(dec_rad) * sin(obliquity_epsilon)
  ecliptic_lon <- atan2(sin_ecliptic_lon, cos_ecliptic_lon) / rad_per_deg
  
  # Adjust for negative ecliptic longitude values
  ecliptic_lon <- ifelse(ecliptic_lon < 0, ecliptic_lon + 360, ecliptic_lon)
  
  return(c(lon = ecliptic_lon, lat = ecliptic_lat))
}

ecliptic_to_cartesian <- function(lat, lon) {
  lon_rad <- lon * pi / 180
  lat_rad <- lat * pi / 180
  
  x <- cos(lon_rad) * cos(lat_rad)
  y <- sin(lon_rad) * cos(lat_rad)
  
  return(c(x, y))
}

SS_Plot <- function(RA,Dec,year,month,day,hour,minute){
# plot positions for a selected period
# date.start = ut_to_julian(2016,4,1,3,0) # Spring equinox 2010
# date.stop = ut_to_julian(2016,4,1,3,0) # Autumn equinox 2010
date.start = ut_to_julian(year,month,day,hour,minute)
date.stop = ut_to_julian(year,month,day,hour,minute)
npoints = 1e3
coordinates = array(NA,c(npoints,9,3))
  for (i in seq(npoints)) {
    jd = date.start+(date.stop-date.start)*(i-1)/(npoints-1)
    coordinates[i,,] = planet.coordinates(jd)
  }

ecliptic_bearing = ra_dec_to_ecliptic(RA, Dec)
cartesian_bearing = ecliptic_to_cartesian(ecliptic_bearing[[1]],ecliptic_bearing[[2]])
print(ecliptic_bearing)

slope <- (cartesian_bearing[[2]] - tail(coordinates[,3,c(1,2)],n=1)[[2]]) / (cartesian_bearing[[1]] - tail(coordinates[,3,c(1,2)],n=1)[[1]])

magplot(coordinates[,3,c(1,2)],type='l',lwd=7,col='blue',
        xlim=c(-5,5),ylim=c(-5,5),xlab='x-coordinate [AU]',ylab='y-coordinate [AU]')
for(i in c(1,2,4,6,7,8,9)){
  points(coordinates[,i,c(1,2)],type='l',lwd=5,col='red',
         xlim=c(-5,5),ylim=c(-5,5),xlab='x-coordinate [AU]',ylab='y-coordinate [AU]')
}
points(coordinates[,5,c(1,2)],type='l',lwd=7,col='purple',
       xlim=c(-5,5),ylim=c(-5,5),xlab='x-coordinate [AU]',ylab='y-coordinate [AU]')


segments(tail(coordinates[,3,c(1,2)],n=1)[[1]], tail(coordinates[,3,c(1,2)],n=1)[[2]], tail(coordinates[,3,c(1,2)],n=1)[[1]] + slope * 100, tail(coordinates[,3,c(1,2)],n=1)[[2]] + slope * 100, col = "forestgreen", lty =5)
#lines(x = c(tail(coordinates[,3,c(1,2)],n=1)[[1]],cartesian_bearing[[1]]), y = c(tail(coordinates[,3,c(1,2)],n=1)[[2]],cartesian_bearing[[2]]) ,col="forestgreen", lwd = 2)

points(0,0,pch=15,cex=1,col='#ffcc00')
#print(tail(coordinates[,3,c(1,2)],n=1))
#abline(v = tail(coordinates[,3,c(1,2)],n=1)[[1]], col="red", lwd=3, lty=2)

}



asteroid.coordinates = function(year,month,day,hour,minute, a , e, I, L, q, o){
  date.start = ut_to_julian(year,month,day,hour,minute)
  elements = 1
  jd = date.start

    # Valid for 1800 AD to 2050 AD (Table 1)
    elements =   rbind(c(a, e, I, L, q, o, 0.00001847,0,-0.00813131,19140.30268499,0.44441088,-0.29257343,0,0,0,0)
                       ,c(3.52371034,0.09339410, 1.84969142, -4.55343205,-23.94362959,49.55953891,0.00001847,0.00007882,-0.00813131,19140.30268499,0.44441088,-0.29257343,0,0,0,0))
  
  rownames(elements) = c("Asteroid", "Asteroid")
    
  # add parameter names
  colnames(elements) = c('a0','e0','I0','L0','q0','O0','adot','edot','Idot','Ldot','qdot','Odot','b','c','s','f')
  # a0/adot: semi-major axis [au, au/cty]
  # e0/edot: eccentricity [-, 1/cty]
  # I0/Idot: inclination [deg, deg/cty]
  # L0/Ldot: mean longitude [deg, deg/cty]
  # q0/qdot: longitude of perihelion [deg, deg/cty]
  # O0/Odot: longitude of ascending node [deg, deg/cty]
  
  # Step 1
  # Compute the value of six Keplerian elements on given date
  t = (jd-2451545.0)/36525
  p = t(as.data.frame(elements[1,1:6]+t*elements[1,7:12]))
  colnames(p) = c('a','e','I','L','q','O')
  rownames(p) = c("Asteroid")
  print(p)
  
  # Step 2
  # Compute the argument of perihelion w, and the mean anomaly M
  w = p['q']-p['O']
  k = pi/180
  M = p['L']-w+elements[1,'b']*t^2+elements[1,'c']*cos(elements[1,'f']*t*k)+elements[1,'s']*sin(elements[1,'f']*t*k)
  M = (M+180)%%360-180 # so that -180<=M<=180
  
  # Step 3
  # Obtain the eccentric anomaly E from the solution of Kepler's equation
  estar = p[,'e']/k
  E = M-estar*sin(M*k)
  E = (E+180+360)%%360-180 # so that -180<=M<=180
  dE = 1
  while (max(dE)>1e-7) {
    dM = M-(E-estar*sin(E*k))
    dE = dM/(1-p['e']*cos(E*k))
    E = E+dE
  }
  
  # Step 4
  # Compute the planet's heliocentric coordinates in its orbital plane with the x'-axis aligned from the focus to the perihelion
  xp = p['a']*(cos(E*k)-p['e'])
  yp = p['a']*sqrt(1-p['e']^2)*sin(E*k)
  
  # Step 5
  # Compute the coordinates in the J2000 ecliptic plane, with the x-axis aligned toward the equinox
  x = (cos(w*k)*cos(p['O']*k)-sin(w*k)*sin(p['O']*k)*cos(p['I']*k))*xp+(-sin(w*k)*cos(p['O']*k)-cos(w*k)*sin(p['O']*k)*cos(p['I']*k))*yp
  y = (cos(w*k)*sin(p['O']*k)+sin(w*k)*cos(p['O']*k)*cos(p['I']*k))*xp+(-sin(w*k)*sin(p['O']*k)+cos(w*k)*cos(p['O']*k)*cos(p['I']*k))*yp
  z = sin(w*k)*sin(p['I']*k)*xp+cos(w*k)*sin(p['I']*k)*yp
  
  # NB: There is an optional step 6 to convert ecliptic coordinates to equatorial coordinates.
  # This is not needed here
  
  # Function to convert ecliptic coordinates to RA and Dec
    # Convert radians to degrees
  deg <- function(rad) rad * 180 / pi
  
  # Ecliptic to Equatorial coordinates conversion
  epsilon <- 23.439292 * (pi / 180)  # Obliquity of the ecliptic
  ra_rad <- atan2(y, x)
  dec_rad <- asin(z * sin(epsilon) + cos(epsilon) * sin(ra_rad))
  # Ensure RA is in the range [0, 2*pi)
  ra_rad <- (ra_rad + 2 * pi) %% (2 * pi)
  
  # Convert radians to degrees
  ra_deg <- deg(ra_rad)
  dec_deg <- deg(dec_rad)
  out = cbind("RA"=ra_deg, "Dec"=dec_deg)
  rownames(out) = c("Asteroid")
  
  return(out)

}


#asteroid.coordinates(2016,4,1,3,0)
asteroid.coordinates(2016,4,1,3,0, 3.52371034, 0.09339410, 1.84969142, -4.55343205,-23.94362959,49.55953891)
ut_to_julian(2016,4,1,3,0)




