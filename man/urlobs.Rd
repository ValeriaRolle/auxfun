\name{urlobs}
\alias{urlobs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ URL translator for obsidian
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
urlobs()
}
%- maybe also 'usage' for other objects documented here.
\arguments{

}
\details{
It reads and URL from the clipboard (MAC NOT TESTED) and returns that URL with backslash replaced with '/' and spaces replaced with 'percentagesymbol20'. It alters the content of the clipboard with the result URL.
The URL is ready to use in Obsidian with the classical syntaxis: [word](URL)
}
\value{
Character string with the translated URL. It also alters the content of the clipboard with the altered URL.
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
