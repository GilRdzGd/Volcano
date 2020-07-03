library('sp')
library('leaflet')
library('htmltools')
library('highcharter')
library('htmlwidgets')

# library('leaflet.providers')
# str(providers_default())

dataframeToPolygons <- function(df, latCol, lngCol, idCol) {
  SpatialPolygons(lapply(unique(df[[idCol]]), function(id) {
    Polygons(list(Polygon(as.matrix(
      df[df[[idCol]] == id, c(lngCol, latCol)]
    ))), id)
  }))
}

dataInterpolation <- read.csv(url("https://raw.githubusercontent.com/GilRdzGd/Volcano/master/coordinates.csv"))

dataVolcanoes <- jsonlite::fromJSON( 'https://query.data.world/s/hdcbjpsxhwizwnwiamfcruipfuslkx')  %>%
  as.data.frame
dataVolcanoes <- as.data.frame(dataVolcanoes$features.properties)

# https://sites.google.com/site/thesmokingmountain94/eruptive-history
dataPopocatepetl <- read.csv(url('https://raw.githubusercontent.com/GilRdzGd/Volcano/master/Popocatepetl.csv'))

dataPopocatepetl$Height <- as.numeric(gsub('[a-zA-Z ]', '', dataPopocatepetl$Height))
dataPopocatepetl <- dataPopocatepetl[!is.na(dataPopocatepetl$Height),]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

dataPopocatepetl$Year <- substrRight(as.character(dataPopocatepetl$Start.), 4)

# dataPopocatepetl$Start. <- gsub(',', '', dataPopocatepetl$Start.)
# dataPopocatepetl$Start <- as.Date(dataPopocatepetl$Start.,format = '%B %d %Y')

colGradient <- c('#ceba63', '#d6cc68', '#d66d46', '#d66d46', '#d3c76a',
                 '#c93e42', '#d6cc68', '#d3c76a', '#d7ab5f', '#d48c4f',
                 '#ccea75', '#d7ad5d', '#d3c76a', '#d86f48', '#d48c4f',
                 '#ccea75', '#b9d880', '#a4c194', '#b9d880', '#d7ca6d',
                 '#d7ab5f', '#d28a4d', '#d7d67c', '#d0c666', '#bcd489',
                 '#d2c669', '#d5a95d', '#d38b4e', '#d0363c', '#d46b44',
                 '#ceea79', '#d6ba70', '#d6ba70', '#ceea79', '#d6a266',
                 '#d36d54', '#93afa3', '#93afa3', '#93afa3', '#bcd489',
                 '#a6c496', '#d6ba70', '#d38b4e', '#a6c496', '#bcd489',
                 '#ceea79', '#d8a468', '#d26f52', '#d26f52', '#d37053',
                 '#c54345', '#c54345', '#c54345', '#d8a468', '#d8a468',
                 '#d37053', '#d26f52', '#d48a5b', '#d48a5b', '#d37053',
                 '#ca965a', '#d48a5b', '#9bb7ab', '#adb995', '#d6ba79',
                 '#c2cc8f', '#cfd374', '#c5ca88', '#98afa7', '#98afa7',
                 '#98afa7', '#bcd489', '#5799bb', '#78a0a8', '#5a99b8',
                 '#7aa2aa', '#7aa2aa', '#a4c194', '#7ba3ab', '#90b5a3',
                 '#90b5a3', '#ccea75', '#d2c669', '#78a1ab', '#bcd489',
                 '#a6c496', '#bcd489', '#a6c496', '#78a1ab', '#93afa3',
                 '#d26f52', '#acbc98', '#d5d97c', '#acbc98', '#acbc98',
                 '#93afa3', '#d5d97b', '#93afa3', '#93afa3', '#a6c496',
                 '#cc4a4c', '#cc4a4c', '#589abc')

legendCol <- c('#2892c6', '#60a3b4', '#8cb7a4', '#b0cc91', '#d7e37d',
               '#fafa64', '#fccf52', '#fca440', '#f87a2d', '#f24d1f',
               '#e81015')

legendVal <- c('&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',
               '&nbsp;&nbsp;40&nbsp;&nbsp;',
               '&nbsp;&nbsp;50&nbsp;&nbsp;',
               '&nbsp;&nbsp;60&nbsp;&nbsp;',
               '&nbsp;&nbsp;70&nbsp;&nbsp;',
               '&nbsp;&nbsp;80&nbsp;&nbsp;',
               '&nbsp;&nbsp;90&nbsp;&nbsp;',
               '&nbsp;100&nbsp',
               '&nbsp;120&nbsp',
               '&nbsp;150&nbsp',
               '&nbsp;200&nbsp')

iconVolcano <- list(iconUrl = 'https://img.icons8.com/emoji/48/000000/red-triangle-pointed-up-emoji.png',
                    iconSize = c(20, 20))

legendIcn <- paste("&nbsp;&nbsp;<img src='https://img.icons8.com/emoji/48/000000/red-triangle-pointed-up-emoji.png' width='12' height='12'>",
                   '&nbsp;&nbsp;&nbsp;Volcán&nbsp;&nbsp;',"<br/>")

as.character.htmlwidget <- function(x, ...) {
  htmltools::HTML(
    htmltools:::as.character.shiny.tag.list(
      htmlwidgets:::as.tags.htmlwidget(
        x
      ),
      ...
    )
  )
}


mm <- dataInterpolation %>%
  dataframeToPolygons('lat', 'log', 'id') %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(stroke = FALSE,
              color  = colGradient,
              fillOpacity = 0.8) %>%
  addMarkers(icon =  iconVolcano,
             lng  = dataVolcanoes$LONG_[dataVolcanoes$NOMB != 'Popocatepetl' & dataVolcanoes$TIPO != 'Campo Volcanico'],
             lat  = dataVolcanoes$LAT[dataVolcanoes$NOMB != 'Popocatepetl' & dataVolcanoes$TIPO != 'Campo Volcanico'],
             label = dataVolcanoes$NOMB[dataVolcanoes$NOMB != 'Popocatepetl' & dataVolcanoes$TIPO != 'Campo Volcanico'],
             clusterOptions = markerClusterOptions()) %>%
  # addCircleMarkers() %>%
  addMarkers(icon =  iconVolcano,
             lng  = dataVolcanoes$LONG_[dataVolcanoes$NOMB == 'Popocatepetl'],
             lat  = dataVolcanoes$LAT[dataVolcanoes$NOMB == 'Popocatepetl'],
             popup = list(paste(as.character(
                     hchart(dataPopocatepetl,
                            type = 'line',
                            hcaes(x = rev(dataPopocatepetl$Start.),
                                  y = rev(dataPopocatepetl$Height)),
                            color = '#9e141e',
                            tooltip = list(pointFormat = '{point.y} km')) %>%
                       hc_title(text = 'Popocatepetl') %>%
                       hc_xAxis(title = list(text =  ''),
                                categories = rev(dataPopocatepetl$Year)) %>%
                       hc_yAxis(title  = list(text =  'Altura'),
                                max    = 10,
                                labels = list(formatter = JS("function () {return this.value + 'Km';}"))) %>%
                       hc_size(width = 300, height = 200)
                     ))),
             popupOptions = popupOptions(minWidth = 300, maxHeight = 200)) %>%
  onRender("function(el,x) { 
                            this.on('popupopen', function() {HTMLWidgets.staticRender();}) 
                            }") %>%
  prependContent(tagList(list(getDependency('highchart', 'highcharter')))) %>%
  addLegend(title  = '<center>Flujo de </br></center>
                      Calor mW/m<sup>2</sup>',
            labels = legendVal,
            colors = legendCol) %>%
  prependContent(tags$style(type = 'text/css', '.leaflet .legend {text-align: center;}')) %>%
  addControl(html = legendIcn, position = 'topright') %>%
  # addProviderTiles('Esri.WorldImagery')
  addProviderTiles('Stamen.Terrain')

mm

# saveWidget(mm, 'mm.html')