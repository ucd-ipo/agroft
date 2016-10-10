# Agroft: Agricultural Field Trial Statistics Package

An analysis interface for the Agriculture Innovation Project

## Installation

This Shiny app has been built into a package and the development version is
hosted on Github. Currently, it can be installed from source using the
`devtools` R package.

Install `devtools`:

```R
> install.packages('devtools')
```

Now install and load the development version of the app with:

```R
> devtools::install_github('ucd-ipo/agroft')
> library(AIP)
```

## Usage

The function `launch` will run the app (i.e., just run `launch()` in the console):

```R
> launch()
```

The app should open in your default web browser. If not, navigate to the
provided URL.

You can also use the `setup` function to confugre R so that it will launch an agroft session when R starts. 

```R
> setup()
```