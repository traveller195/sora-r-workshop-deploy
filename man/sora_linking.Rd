% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sora_objects.R
\name{sora_linking}
\alias{sora_linking}
\title{Linking method}
\usage{
sora_linking(method, ...)
}
\arguments{
\item{method}{Linking method to be used. You can look
up possible methods in the data picker (see \code{\link{sora_datapicker}}).}

\item{...}{Further arguments passed to the linking method. See
\code{\link{sora_datapicker}} for a list of available arguments.}
}
\value{
An object of class \code{sora_linking}.
}
\description{
Specify parameters for a selected linking method to be used in
\code{\link{sora_request}}.
}
\examples{
# You need to always provide at least a linking method
sora_linking("lookup")

# In most cases, a linking method requires additional parameters
sora_linking("count_object", selection_area = "square", length = 1000)
}
\section{Further arguments}{
The following is a basic list of possible
    arguments that can be passed to the respective linking methods. Do
    note that this list is only updated with every release. In other words,
    some of the arguments might not be up-to-date and others might be
    missing. Use \code{\link{sora_arguments}} or visit the data picker
    (see \code{\link{sora_datapicker}}) for more accurate descriptions of
    possible arguments.
\describe{
\item{\code{aggregate_attribute}}{\tabular{lllll}{
 \strong{Name} \tab \strong{Data type} \tab \strong{Optional?} \tab \strong{Range} \tab \strong{Details} \cr
  \code{buffer_logic} \tab character \tab no \tab  \tab square                          \cr
 \code{length}       \tab numeric   \tab no \tab 1, 20000 \tab In Meters; Range from 1 to 20000\cr
 \code{output}       \tab character \tab no \tab  \tab min;max;mean;median;sd;sum;mode 
}
}\cr
\item{\code{area_polygons}}{\tabular{lllll}{
 \strong{Name} \tab \strong{Data type} \tab \strong{Optional?} \tab \strong{Range} \tab \strong{Details} \cr
  \code{buffer_logic} \tab character \tab no \tab  \tab square                          \cr
 \code{length}       \tab numeric   \tab no \tab 1, 20000 \tab In Meters; Range from 1 to 20000
}
}\cr
\item{\code{count_objects}}{\tabular{lllll}{
 \strong{Name} \tab \strong{Data type} \tab \strong{Optional?} \tab \strong{Range} \tab \strong{Details} \cr
  \code{buffer_logic} \tab character \tab no \tab  \tab square                          \cr
 \code{length}       \tab numeric   \tab no \tab 1, 20000 \tab In Meters; Range from 1 to 20000
}
}\cr
\item{\code{length_lines}}{\tabular{lllll}{
 \strong{Name} \tab \strong{Data type} \tab \strong{Optional?} \tab \strong{Range} \tab \strong{Details} \cr
  \code{buffer_logic} \tab character \tab no \tab  \tab square                          \cr
 \code{length}       \tab numeric   \tab no \tab 1, 20000 \tab In Meters; Range from 1 to 20000
}
}\cr
\item{\code{lookup}}{No parameters defined.}\cr
\item{\code{nearest_poi}}{\tabular{llll}{
 \strong{Name} \tab \strong{Data type} \tab \strong{Optional?} \tab \strong{Details} \cr
  \code{routing_logic} \tab character \tab no \tab straight_line
}
}\cr
}
}
