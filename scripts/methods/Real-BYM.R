


# Here I follow what Paula did here https://www.paulamoraga.com/book-geospatial/sec-arealdataexamplespatial.html
# Except that I changed the following tow lines of the code 
# marg <- res$marginals.fitted.values[[1]]
# 1 - inla.pmarginal(q = 2, marginal = marg)
# To this 

# exc <- sapply(res$marginals.linear.predictor, FUN = function(marg){1 - inla.pmarginal(q=log(2) , marginal = marg)})

# The reason of changing is the two lines are not working withme, mabye becuase there is updat in INLA









library(SpatialEpi)
library(sp)
library(sf)




# The map of Scotland counties
map <- scotland$spatial.polygon

#We assign this proj4 string to map and set +units=km 
# because this is the unit of the map projection.


proj4string(map) <- "+proj=tmerc +lat_0=49 +lon_0=-2
+k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36
+units=km +no_defs"



# we transform map to latitude and longitude using WGS84

map <- spTransform(map,
                   CRS("+proj=longlat +datum=WGS84 +no_defs"))





d <- scotland$data[,c("county.names", "cases", "expected", "AFF")]
names(d) <- c("county", "Y", "E", "AFF")


d$SIR <- d$Y / d$E


sapply(slot(map, "polygons"), function(x){slot(x, "ID")})



library(sp)
rownames(d) <- d$county
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)




library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)


nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


map$idareau <- 1:nrow(map@data)
map$idareav <- 1:nrow(map@data)


formula <- Y ~ AFF +
  f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")

res <- inla(formula,
            family = "poisson", data = map@data,
            E = E, control.compute = list(return.marginals.predictor = TRUE),
            num.threads = "1:1"
)




map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]


exc <- sapply(res$marginals.linear.predictor, FUN = function(marg){1 - inla.pmarginal(q=log(2) , marginal = marg)})
map$exc <- exc

library(leaflet)
pal <- colorNumeric(palette = "YlOrRd", domain = map$exc)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s) <br/> P(RR>2): %s",
                  map$county, map$Y, round(map$E, 2),
                  map$AFF, round(map$SIR, 2), round(map$RR, 2),
                  round(map$LL, 2), round(map$UL, 2), round(map$exc, 2)
) %>% lapply(htmltools::HTML)

lexc <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(exc),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~exc, opacity = 0.5, title = "P(RR>2)",
    position = "bottomright"
  )

lexc




