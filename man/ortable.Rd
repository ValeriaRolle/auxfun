\name{ortable}
\alias{ortable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ OR table for logistic or survival models
%%  ~~function to do ... ~~
}
\description{
Prints OR and confident intervals at 95, p-values and AIC from given model.
}
\usage{
vcol(m1, nvar)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m1}{ Logistic glm model or survival (coxph) model. Trusts on class(m1) to distinguish.
%%     ~~Describe \code{x} here~~
}\item{nvar}{ Number of independent variables in the model. }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
m1 <- glm(am ~ disp + hp, data = mtcars, family = "binomial")
ortable(m1, nvar = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
