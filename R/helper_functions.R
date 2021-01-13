
# Helper functions --------------------------------------------------------



# Add annotations to plots ------------------------------------------------

add_annotations <- function(plot_object, model_object) {
  
  # DEBUG ---
  # model_object = model
  # plot_object = plot_output
  
  
  # DF_arrows = parameters::parameters(model_object) %>% 
  #   filter(p<.05) %>%
  #   filter(Parameter != "(Intercept)") %>% 
  #   select(Parameter, p)
  
  DF_arrows = parameters::p_value(model_object, method = "ml1") %>% 
    filter(p < .05) %>%
    filter(Parameter != "(Intercept)") %>% 
    select(Parameter, p)
  
  
  for (i in 1:nrow(DF_arrows)) {
    plot_object = plot_object +
      
      geom_curve(
        data = data.frame(x = 2.198, y = 2.3, xend = 1.198, yend = 2.3, name = DF_arrows$Parameter[i]),
        mapping = aes(x = x, y = y, xend = xend, yend = yend), angle = 90L, 
        colour = "black", curvature = 0L, inherit.aes = FALSE, linetype = "solid", size = .3,
        arrow = arrow(angle = 90, length = unit(0.05, "inches"), ends = "both", type = "open"),
        show.legend = FALSE) + 
      
      geom_text(
        data = data.frame(x = (2.198 + 1.198)/2, y = 2.4, 
                          label = paste0("p = ", format.pval(DF_arrows$p[i], digits = 3, eps = 0.001)), 
                          name = DF_arrows$Parameter[i]), 
        mapping = aes(x = x, y = y, label = label), angle = 0L, lineheight = 1L, hjust = 0.5, 
        vjust = 0.5, colour = "black", family = "sans", fontface = "plain", 
        inherit.aes = FALSE, show.legend = FALSE)
  }
  
  suppressMessages(plot_object)
  
}



# Name contrasts in models ------------------------------------------------

named.contr.sum <- function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    paste0(" (", names(x[x>0]), " - ", names(x[x<0]), ")")
  )
  x
}




# Raincloud plots ---------------------------------------------------------

### This script creates an R function to generate raincloud plots, then simulates
### data for plots. If using for your own data, you only need lines 1-80.
### It relies largely on code previously written by David Robinson
### (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
### and the package ggplot2 by Hadley Wickham

# Check if required packages are installed ----
# packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc", "plyr")
packages <- c("ggplot2", "dplyr", "plyr")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

# Load packages ----
library(ggplot2)

# Defining the geom_flat_violin function ----
# Note: the below code modifies the
# existing github page by removing a parenthesis in line 50

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)

      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        group_by(group) %>%
        mutate(
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },

    draw_group = function(data, panel_scales, coord) {
      # Find the points for the line to go all the way around
      data <- transform(data,
        xminv = x,
        xmaxv = x + violinwidth * (xmax - x)
      )

      # Make sure it's sorted properly to draw the outline
      newdata <- rbind(
        plyr::arrange(transform(data, x = xminv), y),
        plyr::arrange(transform(data, x = xmaxv), -y)
      )

      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1, ])

      ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },

    draw_key = draw_key_polygon,

    default_aes = aes(
      weight = 1, colour = "grey20", fill = "white", size = 0.5,
      alpha = NA, linetype = "solid"
    ),

    required_aes = c("x", "y")
  )
