setwd("C:/Users/HP/Documents/Foresttype")
libs <- c(
  "giscoR", "terra", "elevatr",
  "png", "rayshader", "magick"
)

installed_libs <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libs == F)){
  install.packages(
    libs[!installed_libs]
  )
}

invisible(lapply(
  libs, library,
  character.only = T
))

# 1. COUNTRY BOUNDARIES
#----------------------

country_sf <- giscoR::gisco_get_countries(
  country = "SZ",
  resolution = "1"
)
plot(country_sf$geometry)

# 2. FOREST TYPE RASTER
#----------------------

url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/E000N60/E000N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"
download.file(
  url = url,
  destfile = basename(url),
  mode = "wb"
)
file<- "E020S20_PROBAV_LC100_global_v3.0.1_2019-nrt_Forest-Type-layer_EPSG-4326.tif"

forest_type <- terra::rast(
  basename(file)
)

vals <- terra::values(
  forest_type,
  dataframe = T
)
View(vals)

names(vals)
names(vals)[1] <- "value"
unique(vals$value)

# 3. CROP FOREST TYPE RASTER
#---------------------------

crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"

country_forest_type <- terra::crop(
  forest_type,
  terra::vect(country_sf),
  snap = "in",
  mask = T
) #|>
  #terra::project(crs_lambert)

#terra::plot(country_forest_type)
plot(country_forest_type)
# 4. FOREST TYPE RASTER TO IMAGE
#-------------------------------

cols <- c(
  "#073b4c", 
  "#7cd8ff",
  "#ffcf6a", 
  "#e73f69"
)

from <- c(0:2, 4:5)
to <- t(col2rgb(
  cols
))

forest_terra <- na.omit(
  country_forest_type
)

forest_type_image <- terra::subst(
  forest_terra,
  from,
  to,
  names = cols
)

terra::plotRGB(forest_type_image)
# Get the current working directory
working_dir <- getwd()
# Specify the image file name
img_file <- file.path(working_dir, "Eswatini-forest-image.png")
# Save the image to the working directory
terra::writeRaster(
  forest_type_image,
  img_file,
  overwrite = TRUE,
  NAflag = 255
)
# there was a problem here so I used the above one
img_file <- "Swaziland/Eswatini-forest-image.png"
terra::writeRaster(
  forest_type_image,
  img_file,
  overwrite = T,
  NAflag = 255
)

img <- png::readPNG(img_file)

# 5. COUNTRY ELEVATION RASTER
#----------------------------

elev <- elevatr::get_elev_raster(
  locations = country_sf, #warn about locations
  z = 10, clip = "locations"
)

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev
)

# 6. RENDER SCENE
#----------------

h <- nrow(elev)
w <- ncol(elev)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      "white"
    )(512)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = .9,
    alphacolor = "white"
  ) |>
  rayshader::add_shadow(
    rayshader::lamb_shade(
      elmat,
      zscale = 60,
      sunaltitude = 90,
      sunangle = 315,
    ), max_darken = .30
  ) |>
  rayshader::add_shadow(
    rayshader::texture_shade(
      elmat,
      detail = .95,
      brightness = 90, #warn
      contrast = 80,
    ), max_darken = .1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 5,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 5, h / 5
    ),
    zoom = .5,
    phi = 85,
    theta = 0 
  )

# 7. RENDER OBJECT
#-----------------

rayshader::render_highquality(
  filename = "forest-type-Swaziland-3d.png",
  preview = T,
  light = F,
  environment_light = "air_museum_playground_4k.hdr",
  intensity_env = 2,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = w, height = h
)

# 8. MAKE LEGEND
#---------------

png("my_legend.png")
par(family = "mono")
plot(
  NULL, xaxt = "n",
  yaxt = "n", bty = "n",
  ylab = "", xlab = "",
  xlim = 0:1, ylim = 0:1,
  xaxs = "i", yaxs = "i"
)
legend(
  "center",
  legend = c(
    "Unknown",
    "Evergreen broad leaf",
    "Deciduous broad leaf",
    "Mixed"
  ),
  pch = 16,
  pt.cex = 3,
  cex = 1.5,
  bty = "n",
  col = cols
)
dev.off()

# 9. FINAL MAP
#-------------

forest_img <- magick::image_read(
  "forest-type-Swaziland-3d.png"
)

my_legend <- magick::image_read(
  "my_legend.png"
)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"), 2000
) |>
  magick::image_transparent("white")

p <- magick::image_composite(
  magick::image_scale(
    forest_img,
    "x4000"
  ),
  my_legend_scaled,
  offset = "+100+0"
)

magick::image_write(
  p,
  "final-map.png"
)