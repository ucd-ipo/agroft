# Agroft: Agricultural Field Trial Statistics Package

An analysis interface for the Agriculture Innovation Project

## Installation

The Agroft application is a R package and the development version is
hosted on Github. The easiest way to install it is via `install.packages`, 
specifying this repository in addition to the normal CRAN repository. 

You will also need Rtools installed if you are using Microsoft Windows. Download the Rtools\<XY\>.exe file that corresponds to your verision of R from [this website](https://cran.r-project.org/bin/windows/Rtools/).

After installing Rtools, re-open R and run:

```R
> install.packages('agroft', 
                   repos = c('@CRAN@', 'https://ucd-ipo.github.io/agroft'), 
                   type='source')
```

Older versions of R may not be able to download data from the Agroft repository. If you get the error:

```
Warning in install.packages :
  package 'agroft' is not available (for R version x.y.z)
```

You will need to install Agroft using the `devtools` package. 

Install `devtools`:

```R
> install.packages('devtools')
```

Now install the development version of the app with:

```R
> devtools::install_github('ucd-ipo/agroft')
```

This step only has to be done once. 

## Usage

Open R and load the package with the `library` function:


```R
> library(agroft)
```

This will load the package and open a new Agroft session in your default web browser. Closing the browser window or tab in which Agroft is running will end your Agroft session (data and progress will not be saved between Agroft sessions). To start another new Agroft session, use the `launch` function (i.e. type `launch()` at the R command prompt (`>`) and press enter). 

```R
> launch()
```

A new session will start in your default web browser.

You can also use the `setup` function to confugre R so that it will launch an Agroft session when R starts. This option is designed for users who only plan on using R to run Agroft, and not for other purposes. 

```R
> setup()
```
