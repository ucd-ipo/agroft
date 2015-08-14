# Agricultural Field Trial Statistics Package

An analysis interface for the Agriculture Innovation Project

## Demo

A demo of the app can be accessed here:

https://moorepants.shinyapps.io/aftsp

## Installation

This Shiny app has been built into a package and the development version is
hosted on Github. Currently, it can be installed using the `devtools` R
package.

Install devtools and the Shiny R packages:

```R
> install.packages('devtools')
> install.packages('shiny')
```

Now install and load the development version of the app with:

```R
> devtools::install_github('ucd-ipo/aip-analysis')
> library('AIP')
```

Get all of the remaining dependencies by initializing the app:

```R
> initialize_AIP()
```

### Notes on installation

You may need to update the lattice package after the `initialize_AIP()` call:

```R
> unloadNamespace("lattice")
> update.packages('lattice')
```

The required packages are listed in "Enhances" rather than "Depends" or
"Imports" because `install_github` won't install dependencies from github. In
the package, there is a function for installing all the packages called
`initialize_AIP`. This will check if required packages are on the search path,
and if not, it will install them. All the required packages are on CRAN, but
shinyACE and shinyBS need version 0.2-1 and 0.5, respectively, so until those
are on CRAN, this annoying workaround is required. Once they are on CRAN, this
workaround can be removed and the packages can be moved to "Imports" rather
than "Enhances" in the "DESRIPTION" file.

## Usage

The function `AIP` will run the app (i.e., just run `AIP()` in the console). If
all packages aren't installed, a warning will be generated telling you to stop
the app and run `initialize_AIP`.

Run the app with:

```R
> AIP()
```

The app should open in your default webbrowser. If not, navigate to the
provided URL.
