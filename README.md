# Agricultural Field Trial Statistics Package

An analysis interface for the Agriculture Innovation Project

## Demo

A demo of the app can be accessed here:

https://moorepants.shinyapps.io/aftsp

## Installation

This Shiny app has been built into a package and the development version is
hosted on Github. Currently, it can be installed from source using the
`devtools` R package.

Install devtools:

```R
> install.packages('devtools')
```

Now install and load the development version of the app with:

```R
> devtools::install_github('ucd-ipo/aip-analysis')
```

## Usage

The function `AIP` will run the app (i.e., just run `AIP()` in the console). If
all packages aren't installed, a warning will be generated telling you to stop
the app and run `initialize_AIP`. First load the library:

```R
> library('AIP')
```

Run the app with:

```R
> AIP()
```

The app should open in your default web browser. If not, navigate to the
provided URL.
