library(ggplot2)

dist_map <- function(filename, wd) {
  
  # set working directory to where the data is located
  setwd(wd)
  
  # read in bird occurrence data with coordinates
  mapdata <- read.csv(filename, stringsAsFactors = FALSE)
  
  # extract data points for desired species by grepping
  # any number of species can be used, as well as other taxon
  mapdata_A <- mapdata[grep('Anas platyrhynchos',
                            mapdata$taxon_species_name), ]
  mapdata_B <- mapdata[grep('Zenaida macroura',
                            mapdata$taxon_species_name), ]
  mapdata_C <- mapdata[grep('Sayornis nigricans',
                            mapdata$taxon_species_name), ]
  
  # set the theme parameters for the map
  theming <- theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(),
                   axis.ticks.x = element_blank(),axis.text.x = element_blank(),
                   aspect.ratio = 1,
                   plot.title = element_text(lineheight=.8, 
                                             face="bold",
                                             vjust=1,
                                             hjust = .5,
                                             size = 20),
                   legend.position = 'right',
                   legend.direction = 'vertical',
                   legend.title = element_text(face = 'bold',
                                               size = 16),
                   legend.text = element_text(size = 14),
                   legend.background = element_rect(fill = 'gray95',
                                                    color = 'black'),
                   legend.key = element_rect(fill = 'gray95',
                                             color = 'gray95'),
                   panel.background = element_rect(fill = 'lightsteelblue2',
                                                   colour = 'black',
                                                   size = 1,
                                                   linetype = "solid"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())
  
  # for creating the county boundaries, uses the maps package from ggplot2
  if (require("maps")) {
    
    CA <- map_data("county", "california")
    
    # assigning parameters to the map variable
    # data for the base polygon layer is california counties for this data
    # can be adjusted to any state county boundaries
    map <- ggplot(CA, aes(long, lat)) +
      geom_polygon(aes(group = group), fill = 'gray80', color = "black") +
      # setting a coordinate boundary to focus on the data
      coord_cartesian(xlim = c(-118.9, -117.9), ylim = c(33.6, 34.6)) +
      # define the axis labels and title
      labs(x = '', y = '',
           title = 'Common Bird Distributions within\nthe LA River Watershed Region') +
      # add the theming that was set earlier and set to a variable
      theming +
      # define the title of the legend
      guides(fill = guide_legend(title = 'Species'),
             # manually adjusting key symbol sizes
             # must be done by overriding the aesthetics for guides
             color = guide_legend(override.aes = list(size=3))) +
      # plot the first species points
      geom_point(data = mapdata_A,
                 aes(x = longitude, y = latitude, color = 'Mallard', size = 'Mallard')) +
      # plot the second species points
      geom_point(data = mapdata_B,
                 aes(x = longitude, y = latitude, color = 'Mourning Dove', size = 'Mourning Dove')) +
      # plot the third species points
      geom_point(data = mapdata_C,
                 aes(x = longitude, y = latitude, color = 'Black Phoebe', size = 'Black Phoebe')) +
      # set the colors for each species
      scale_color_manual(name = 'Species',
                         values = c('Mallard'='red', 
                                    'Mourning Dove'='blue', 
                                    'Black Phoebe'='palegreen4')) +
      # set the size of each point for each species
      scale_size_manual(name = 'Species',
                        values = c('Mallard'=.75, 
                                   'Mourning Dove'=.75, 
                                   'Black Phoebe'=.75))
    
  }
  
  map
  
}

dist_map('birds_larw.csv', '~/Developer/repos/eeb-c177-project/data/')
