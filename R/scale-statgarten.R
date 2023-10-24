sg_colours_d <-  list(
  statgarten = c("#C70A80", "#37E2D5", "#FBCB0A", "#3EC70B", "#590696"),
  seoul = c("#EA047E", "#FF6D28", "#FCE700", "#00F5FF", "#38E54D"),
  suncheon = c("#EE5007", "#F8CB2E", "#006E7F", "#92BA92", "#B22727")
)

sg_colours_c <- list(
  statgarten = c("#F2F8FF", "#004DA6"),
  jeju1 = c("#F0FAFA", "#004B4D"),
  jeju2 = c("#FFDCB9", "#C25700"),
  seoul = c("#E7D6C5", "#785330"),
  suncheon = c("#FFCCD8", "#D13D60")
)

statgarten_palettes <-  function(name, n, type = c("discrete", "continuous")) {
  palette <- if(type == "discrete") {
    sg_colours_d[[name]]
  } else {
    sg_colours_c[[name]]
  }

  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}


#' Statgarten colour palettes
#'
#' The statgarten colour palettes are designed to be used with the
#' ggplot2 package. They are based on the colours used in the statgarten.
#'
#' @rdname scale_statgarten
#' @export
#' @examples
#' # Use statgarten_d with discrete data
#' library(tidyverse)
#' iris %>%
#'   ggplot(aes(Sepal.Length, Sepal.Width, colour = Species)) +
#'     geom_point() +
#'     scale_color_statgarten_d("seoul") +
#'     sgthemes::theme_statgarten()
#'
#' # Use statgarten_c with continuous data
#' iris %>%
#'   ggplot(aes(Sepal.Length, Sepal.Width, colour = Sepal.Length)) +
#'     geom_point() +
#'     scale_color_statgarten_c() +
#'     sgthemes::theme_statgarten()

scale_color_statgarten_d = function(palette = "statgarten") {
  ggplot2::scale_colour_manual(values = statgarten_palettes(name = palette, type = "discrete"))
}

#' @export
#' @rdname scale_statgarten

scale_fill_statgarten_d = function(palette = "statgarten") {
  ggplot2::scale_fill_manual(values = statgarten_palettes(name = palette, type = "discrete"))
}

#' @export
#' @rdname scale_statgarten

scale_color_statgarten_c = function(palette = "statgarten") {
  ggplot2::scale_colour_gradientn(colours = statgarten_palettes(name = palette, type = "continuous"))
}

#' @export
#' @rdname scale_statgarten

scale_fill_statgarten_c = function(palette = "statgarten") {
  ggplot2::scale_fill_gradientn(colours = statgarten_palettes(name = palette, type = "continuous"))
}
