\name{setup}
\alias{setup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to customize your .Rprofile file.
}
\description{
This function adds code to your existing ~/.Rprofile that will cause Agroft to launch when R is started, rather than requiring the user to load the package. If no ~/.Rprofile exists, one will be created. You can also use it to remove that custom code if it has been run before. 
}
\usage{
setup()
}
%- maybe also 'usage' for other objects documented here.
\details{
If an existing Rprofile exists, it will not be overwritten, only edited, but if you have customized your Rprofile, you wouldn't want to use this feature anyway. Recommended for users who will only be using Agroft. 
}
\value{
None. 
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
\code{\link{launch}} for starting a new Agroft session. 
}

