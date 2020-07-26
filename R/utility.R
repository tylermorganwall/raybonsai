# convert an angle from degrees to radians
radians <- function(degree) {
  pi * degree / 180
}

# plane 1
extend_x <- function(distance, angle, azimuth) {
  distance * cos(radians(angle)) * sin(radians(azimuth))
}

# plane 2
extend_z <- function(distance, angle, azimuth) {
  distance * sin(radians(angle)) * sin(radians(azimuth))
}

# vertical distance
extend_y <- function(distance, angle, azimuth) {
  distance * cos(radians(azimuth))
}

unit_vector = function(x) {
  x/sqrt(sum(x * x))
}

cross = function(u, v) {
  return(c(u[2] * v[3] - u[3] * v[2],
           u[3] * v[1] - u[1] * v[3],
           u[1] * v[2] - u[2] * v[1]))
}

build_from_w = function(dir) {
  basis = matrix(0,3,3)
  basis[3,] = unit_vector(dir)
  if(abs(basis[1,1]) > 0.9) {
    a = c(0,1,0)
  } else {
    a = c(1,0,0)
  }
  basis[2,] = unit_vector(cross(basis[3,],a))
  basis[1,] = cross(basis[3,], basis[2,])
  basis
}

