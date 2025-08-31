## ----angles-------------------------------------------------------------------
library("affiner")
as_angle(90, "degrees") + turns(1)
is_congruent(degrees(180), radians(pi))
as.numeric(turns(1/3), "radians")

## ----trig---------------------------------------------------------------------
library("affiner")
sin(2 * pi)
sine(degrees(360))
arctangent(x = 0, y = 1)

## ----2d-----------------------------------------------------------------------
# Cartesian coordinates
library("affiner")
p <- as_coord2d(x = 1:10, y = 1:10)
print(p)

p2 <- p$
    clone()$
    scale(x = 0.5)$
    rotate(degrees(90))$
    reflect(as_line2d("y-axis"))$
    translate(as_coord2d(x = 0.5, y = 0.5))$
    print()

# Polar coordinates
theta <- degrees(seq(0, 300, by = 60))
radius <- 1
p <- as_coord2d(theta, radius = radius)
is_congruent(as_angle(p), theta) |> all()
is_congruent(abs(p), radius) |> all()

## ----3d-----------------------------------------------------------------------
# Cartesian coordinates
library("affiner")
p <- as_coord3d(x = 1:10, y = 1:10, z = 1:10)
print(p)

p2 <- p$
    clone()$
    scale(z = 0.5)$
    rotate(axis = as_coord3d("z-axis"), theta = degrees(90))$
    reflect(as_plane3d("yz-plane"))$
    shear(xy_shear = 0.5)$
    translate(as_coord3d(x = 0.5, y = 0.5, z = 0.5))$
    print()

# Spherical coordinates
inclination <- as_angle(p, type = "inclination")
azimuth <- as_angle(p, type = "azimuth")
radius <- abs(p)
ps <- as_coord3d(azimuth, radius = radius, inclination = inclination)
all.equal(p, ps)

# Cylindrical coordinates
radius <- as_coord2d(p, plane = "xy-plane") |> abs()
pc <- as_coord3d(azimuth, radius = radius, z = p$z)
all.equal(p, pc)

