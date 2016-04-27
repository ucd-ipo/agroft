# Agricultural Field Trial Statistics Package

An analysis interface for the Agriculture Innovation Project

## Demo

A demo of the app can be accessed here:

http://ikkyle.com/playground/aip-analysis/

## Installation

This Shiny app has been built into a package and the development version is
hosted on Github. Currently, it can be installed from source using the
`ghit` R package.

Install `ghit`:

```R
> install.packages('ghit')
```

Now install and load the development version of the app with:

```R
> ghit::install_github('ucd-ipo/aip-analysis')
> library(AIP)
```

## Usage

The function `AIP` will run the app (i.e., just run `AIP()` in the console):

```R
> AIP()
```

The app should open in your default web browser. If not, navigate to the
provided URL.
