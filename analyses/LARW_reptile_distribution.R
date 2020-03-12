library(ggplot2)

dist_map <- function(filename, wd) {
  
  # set working directory to where the data is located
  setwd(wd)
  
  # read in bird occurrence data with coordinates
  mapdata <- read.csv(filename, stringsAsFactors = FALSE)
  
  # extract data points for desired species
  mapdata_A <- mapdata[grep('Sceloporus occidentalis',
                            mapdata$taxon_species_name), ]
  mapdata_B <- mapdata[grep('Elgaria multicarinata',
                            mapdata$taxon_species_name), ]
  mapdata_C <- mapdata[grep('Uta stansburiana',
                            mapdata$taxon_species_name), ]
  
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
  
  if (require("maps")) {
    
    CA <- map_data("county", "california")
    
    map <- ggplot(CA, aes(long, lat)) +
      geom_polygon(aes(group = group), fill = 'gray80', color = "black") +
      coord_cartesian(xlim = c(-118.9, -117.9), ylim = c(33.6, 34.6)) +
      # define the axis labels and title
      labs(x = '', y = '',
           title = 'Common Reptile Distributions within\nthe LA River Watershed Region') +
      # add the theming that was set earlier and set to a variable
      theming +
      # define the title of the legend
      guides(fill = guide_legend(title = 'Species'),
             # manually adjusting key symbol sizes
             color = guide_legend(override.aes = list(size=3))) +
      # plot the first species points
      geom_point(data = mapdata_A,
                 aes(x = longitude, y = latitude, color = 'Western Fence Lizard', size = 'Western Fence Lizard')) +
      # plot the second species points
      geom_point(data = mapdata_B,
                 aes(x = longitude, y = latitude, color = 'Southern Alligator Lizard', size = 'Southern Alligator Lizard')) +
      # plot the third species points
      geom_point(data = mapdata_C,
                 aes(x = longitude, y = latitude, color = 'Common Side-blotched Lizard', size = 'Common Side-blotched Lizard')) +
      # set the colors for each species
      scale_color_manual(name = 'Species',
                         values = c('Western Fence Lizard'='red', 
                                    'Southern Alligator Lizard'='blue', 
                                    'Common Side-blotched Lizard'='palegreen4')) +
      # set the size of each point for each species
      scale_size_manual(name = 'Species',
                        values = c('Western Fence Lizard'=.75, 
                                   'Southern Alligator Lizard'=.75, 
                                   'Common Side-blotched Lizard'=.75))
    
  }
  
  map
  
}

dist_map('reptiles_larw.csv', '~/Developer/repos/eeb-c177-project/data/')
