\name{sjags}
\alias{sjags}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Simplified jags function
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sjags(form, data, family = "gaussian", n.iter = 10000, n.chains = 4, n.burnin = 500, n.thin = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{An object of class formula (y ~ x). Currently up to 4 explanatory (x) variables in binomial or poisson; 5 in gaussian.

}
  \item{data}{An object of class data.frame to get the data from.
}
  \item{family}{"binomial", "gaussian" or "poisson". }
  \item{...}{Arguments to pass to JAGS.

  }
}
\details{
Factors are supported. (2018June25)
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
\author{ Valeria Rolle
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
sjags(mpg ~ wt + cyl + vs,
                family = "gaussian", data = mtcars)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
