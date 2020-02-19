\name{ggtoppt}
\alias{ggtoppt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Export ggplot and base graphics to PowerPoint (.pptx)
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ggtoppt(gg, export = FALSE, pptname = "exportedplots.pptx")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gg}{A ggplot object (first class must be "gg").}
  \item{export}{FALSE to keep adding plots. TRUE to create the pptx file.}
  \item{pptname}{Name for the pptx file.}
}
\details{
First use the function with \strong{export = FALSE} to create an object (a list like object called "filename" you will see in the enviroment), with all the graphs you want to export in the same ppt (either ggplot or base graphics).

When you are done, run a line with \strong{export = TRUE} to create a power point in the working enviroment. In this line first argument will be not used so it does not matter its value, but it has to be there (sorry, will fix later).

Base is more complicated because it does not store plots in an object. You must create a function to make the plot. It can be combined with ggplot in the same exportation. Example in "Examples" section.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Valeria Rolle
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Save a plot in "plot1" object.

      plot1 <- ggplot(mpg, aes(displ, hwy)) +
                  geom_point() +
                  geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3),
                  se = FALSE)

# Add the plot to "filename" object to export it later.

      ggtoppt(plot1)

# Add "plot2" to "filename"

      plot2 <- ggplot(mpg, aes(displ, hwy)) +
                  geom_point() +
                  geom_smooth(span = 0.8) +
                  facet_wrap(~drv)

# Repeat with "plot2"

      ggtoppt(plot2)

# Export it all to working directory (will be printed in Console).

      ggtoppt(asdf, export = TRUE)

# If you want to start all over again, make sure to delete "filename"
from the enviroment, if you want to add new plots to the same series, keep it.

##############################################################################

### How to export base:

# Create a function with the plot.

      plot3 <- function(){
            barplot(1:10)
      }

# Save plot in "filename".

      ggtoppt(plot3)

# Export both ggplot and one base to a pptx. ("filename" was not deleted).

      ggtoppt(asdf, export = TRUE)
      }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
