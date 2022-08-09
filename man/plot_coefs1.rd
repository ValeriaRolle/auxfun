\name{plot_coefs1}
\alias{plot_coefs1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ jtools::plot_coeffs personalization
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_coefs1(
  ...,
  ci_level = 0.95,
  inner_ci_level = NULL,
  model.names = NULL,
  coefs = NULL,
  omit.coefs = c("(Intercept)", "Intercept"),
  colors = "CUD Bright",
  plot.distributions = FALSE,
  rescale.distributions = FALSE,
  exp = FALSE,
  point.shape = TRUE,
  point.size = 3,
  legend.title = "Model",
  groups = NULL,
  facet.rows = NULL,
  facet.cols = NULL,
  facet.label.pos = "top",
  color.class = colors,
  resp = NULL,
  dpar = NULL
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
See ?jtools::plot_coefs
}
\details{
This is a slight edition of the plot_coefs function in the package jtools. It only changes styling parameters.
}
\value{
A ggplot object.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{ Valeria Rolle
%%  ~~who you are~~
}
\note{
If you encounter an error along the lines of not having the same number of attributes than elements and you are plotting categorical variables, remember to specify the variable + category name in the coefs argument. For example: you must specify c("Speciessetosa", "Speciesversicolor", "Speciesvirginica") (well, minus the reference one) and NOT only "Species".

This *feature* is present in the original function in the jtools package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
mtcars$cyl <- as.character(mtcars$cyl)
mtcars$cat2 <- c(rep("primera", 16), rep("segunda", 16))

m1 <- glm(vs ~ carb + gear, data = mtcars, family = "binomial")
m2 <- glm(vs ~ cyl + drat + hp , data = mtcars, family = "binomial")
m3 <- glm(vs ~ cyl, data= mtcars, family = "binomial")

plot_coefs1(m3,m2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
