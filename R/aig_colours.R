#' aig colour pallete
aig_colours <- c(
  'aig_red' = "#a6192e",
  'aig_blue' = "#1f49a2",
  'aig_pink' = "#f896a2",
  'aig_aqua' = "#3fb5a0",
  'aig_purple' = "#7030a0",
  'aig_midblue' = "#8abbd0",
  'aig_beige'= "#efe8d1",
  'aig_grey' = "#333F48",
  'aig_lightgrey' = "#a6192e"
)

aig_red <- "#a6192e"
aig_blue <- "#1f49a2"
aig_pink <- "#f896a2"
aig_aqua <- "#3fb5a0"
aig_purple <- "#7030a0"
aig_midblue <- "#8abbd0"
aig_beige <- "#efe8d1"
aig_grey <- "#333F48"
aig_lightgrey <- "#a6192e"

#########################
#' Create a Aig-appropriate palette for your chart.
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 10. Using more than 6 is not recommended. If you don't
#'   specify `n`, a five-colour palette will be used, which may not look right.
#'   Specify `n`.
#'
#'   By default, n = 2 will give you light orange and dark orange. Use n = "2a"
#'   if you want light orange and red.
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order. Standard colour order runs from light to dark. If
#'   you set reverse to TRUE, colours will run from dark to light.
#' @param faded Logical. FALSE by default. Setting to TRUE returns the faded
#'   variations of the standard colours.
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     theme_grattan() +
#'     scale_colour_manual(values = grattan_pal(n = 3))
#'
#' p
#'
#' # Alternatively, use aig_colour_manual(), which is a wrapper
#' # around scale_colour_manual():
#'
#' p <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'     geom_point() +
#'     aig_style() +
#'     aig_colour_manual(n = 3)
#'
#' p
#'
#' @export

aig_pal <- function(n = 0, reverse = FALSE, faded = FALSE) {
  
  if (n == 0) {
    n <- 5
    "Your chart will probably look better if you specify n in aig_pal()."
  }
  
  if (n > 6 & n <= 9) {
    warning("Using more than six colours is not recommended.")
  }
  
  if (n > 9 & n != "2a") {
    stop(paste0("You've requested ", n,
                " colours; aig_pal() only supports up to 9."))
  }
  
  if(isFALSE(faded)) {
    palette <- regular_palette(n)
  }
  
  if(isTRUE(faded)) {
    palette <- regular_palette(n)
  }
  
  if (isTRUE(reverse)) {
    palette <- rev(palette)
  }
  
  palette
}

regular_palette <- function(n) {
  
  if (n == 1) {
    palette <- aig_red
  } else if (n == "2a") {
    palette <- c(aig_red,
                 aig_pink)
  } else if (n == 2) {
    palette <- c(aig_red,
                 aig_blue)
  } else if (n == 3) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink)
  } else if (n == 4) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua)
  } else if (n == 5) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua,
                 aig_purple)
  } else if (n == 6) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua,
                 aig_purple,
                 aig_midblue)
  } else if (n == 7) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua,
                 aig_purple,
                 aig_midblue,
                 aig_beige)
  } else if (n == 8) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua,
                 aig_purple,
                 aig_midblue,
                 aig_beige,
                 aig_grey)
  } else if (n == 9) {
    palette <- c(aig_red,
                 aig_blue,
                 aig_pink,
                 aig_aqua,
                 aig_purple,
                 aig_midblue,
                 aig_beige,
                 aig_grey,
                 aig_lightgrey)
  } 
  palette
}




#' Convenient functions to set aig-appropriate palettes
#'
#' @param n Numeric. The number of levels in your colour scale. Minimum value is
#'   1, maximum is 7.
#'
#' @param reverse Logical. FALSE by default. Setting to TRUE reverses the
#'   standard colour order.
#'
#' @param faded Logical. FALSE by default. Setting to TRUE returns faded
#'   variations of the standard colours.
#'
#' @param discrete Logical. TRUE by default. Setting to FALSE generates a
#'   continuous colour scale.
#'
#' @param palette Sets the colours that will form the continuous palette when
#'   discrete = FALSE. One of:
#'
#' \itemize{
##' \item{"full"}{The default. Red, dark orange, light orange, yellow, light
##' yellow}
##' \item{"full_f"}{ faded version of "full"}
##' \item{"light"}{ light
##' orange, yellow, light yellow}
##' \item{"dark"}{ red, dark orange, light orange}
##' \item{"diverging"}{ red, faded red, white, faded light orange, light orange}
##' \item{"grey"}{ grey 1, grey 2, grey 3, grey 4, grey 5}
##'}
#'
#' @param ... arguments passed to ggplot2 scales
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(data = mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
#'    geom_point() +
#'    aig_colour_manual(n = 3) +
#'    aig_style()
#'
#' @name aig_colours
#' @aliases NULL
NULL

#' @rdname aig_colours
#' @import ggplot2
#' @export


aig_colour_manual <- function(n = 0,
                                  reverse = FALSE,
                                  discrete = TRUE,
                                  faded = FALSE,
                                  palette = "full", ...) {
  if (discrete) {
    return(
      ggplot2::scale_colour_manual(...,
                                   values = aig_pal(n = n,
                                            reverse = reverse,
                                            faded = faded))
    )
  }
  
  if (!discrete) {
    pal <- aig_palette(palette = palette, reverse = reverse)
    return(ggplot2::scale_color_gradientn(colours = pal(256), ...))
  }
}

#' @rdname aig_colours
#' @import ggplot2
#' @export

aig_fill_manual <- function(n = 0, reverse = FALSE,
                                discrete = TRUE,
                                faded = FALSE,
                                palette = "full", ...) {
  if (discrete) {
    return(
      ggplot2::scale_fill_manual(...,
                                 values = aig_pal(n = n,
                                                      reverse = reverse,
                                                      faded = faded))
    )
  }
  
  if (!discrete) {
    pal <- aig_palette(palette = palette, reverse = reverse)
    return(ggplot2::scale_fill_gradientn(colours = pal(256), ...))
  }
  
}
