% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/plot_continuous.r
\name{plot_continuous}
\alias{plot_continuous}
\title{Plots map and places part of the labels outside of the map (Only for map of Leipzig)}
\usage{
plot_continuous(map_data, label_text, pal = "Greens", fill_data = NULL,
  title = NULL, label_data, right = NULL, left = NULL, bottom = NULL,
  label_line_colour = "blue", label_size = 4, family = "Helvetica",
  alpha = 1, inLabel_off = "no", margin_left = 0.055,
  margin_right = 0.02, margin_bottom = 0, right_top = 51.43357,
  right_bottom = 51.3519, left_alignment = 12.52072, left_top = 51.36014,
  left_bottom = 51.27666, right_alignment = 12.23666,
  bottom_right = 12.44595, bottom_left = 12.3366, top_alignment = 51.2598,
  with_legend = "yes")
}
\arguments{
\item{map_data}{polygon data of the map}

\item{label_text}{text or graphics of the labels}

\item{pal}{colouring palette, as in ggplot2}

\item{fill_data}{OPTIONAL: a column in map_data for filling polygons with colours (alwasys merge with map_data)}

\item{title}{title of the plot}

\item{label_data}{dataframe contains label best_x and best_y}

\item{right}{an array of polygon ids whose labels are placed in label region right}

\item{left}{an array of polygon ids whose labels are placed in label region left}

\item{bottom}{an array of polygon ids whose labels are placed in label region bottom}

\item{label_line_colour}{line colour for labels outside}

\item{label_size}{text size of the labels}

\item{family}{font of the labels}
}
\value{
ggplot2 map of Leipzig with labels in label regions
}
\description{
Plots map of Leipzig and places labels at the label regions outside of the map. Three label regions: right, left and bottom.
}
\details{
User can define which labels are placed in which of the three label regions.
}
\examples{
see "use-cases/usecase-plot_continous.r" and "use-cases/usecase-plot_continous_sbz.r"
}

