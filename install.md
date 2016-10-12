---
title: "Installation"
output: html_document
---

### Installing R

Since Agroft is based on the powerful R language, you must first download and install R on your computer. R is available for all popular operating systems. Go to [https://cran.r-project.org/](https://cran.r-project.org/) and click the link that says "Download R for \<your operating system\>" that corresponds to your operating system. 

**Important:** Windows users will also need to install a program called "Rtools" or Agroft won't work. Go to [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/) and download the Rtools exe file that is compatible with the version of R you just installed. After you have installed R, install Rtools. 

If you already have an older version of R installed on your computer, you may not have to install a new version. Your version of R should be at least version 3.0.0. If your existing R version is less than that, you will need to upgrade to the latest version. 

### Installing Agroft

Once R and, if you use Windows, Rtools, have been installed, you are ready to install Agroft. 

Start by opening the R application. A window will open and some text will appear that looks like the following:


```
R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
Copyright (C) 2016 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.
```

Under that text will be the "R prompt", which is a single "greater-than-symbol" (looks like ">"). To interact with R, the user types special commands at that prompt that tell R to do different things. R is very powerful, but can also be difficult to learn. Agroft allows you to use R's abilities without have to learn any programming, but you will have to enter a few commands to get Agroft working first. 

The commands are slightly different for Windows and OSX, so make sure to enter the commands for your operating system. 

**Important:** If your version of R is less than 3.2.0 because you already had R installed and chose to not upgrade, look at the instructions on the tab called "For R <3.2.0", regardless of your operating system. 


#### Code {.tabset}

##### For Windows

At the R prompt, Enter the first command shown below, exactly as it appears, and press enter. The command should execute instantly, and a new prompt should appear directly below the old one.  

```R
options(download.file.method = "wininet", url.method = "wininet")
```

Now, run the command below to install Agroft. This command can take several minutes, depending on your computer and internet connection. You will know that it is complete when a new R prompt appears. A small window may open after running this command that asks you to select a "CRAN Mirror". Select the first one in the list that will be presented to you, called "0-Cloud [https]" and click "OK". 

```R
install.packages('agroft', repos = c('@CRAN@', 'https://ucd-ipo.github.io/agroft'), type='source')
```

A lot of text will start popping up when this command is running. After it is done, look near the end of the output for text that says:

```
** testing if installed package can be loaded
* DONE (agroft)
```

If you see this, you are done installing Agroft. If instead you see a message that says:

```
Warning in install.packages :
  installation of package 'agroft' had non-zero exit status
```

Or something like:

```
Warning message:
package "agroft" is not available (for R version x.y.z) 
```

then Agroft was not installed. Check to make sure you installed Rtools correctly and your R version is greater than or equal to 3.2.0. If not, install Rtools and then try the instructions listed under the tab "For  R <3.2.0".


##### For OSX

At the R prompt, Enter the first command shown below, exactly as it appears, and press enter. The command should execute instantly, and a new prompt should appear directly below the old one.  

```R
options(download.file.method = "libcurl", url.method = "libcurl")
```

Now, run the command below to install Agroft. This command can take several minutes, depending on your computer and internet connection. You will know that it is complete when a new R prompt appears. A small window may open after running this command that asks you to select a "CRAN Mirror". Select the first one in the list that will be presented to you, called "0-Cloud [https]" and click "OK". 

```R
install.packages('agroft', repos = c('@CRAN@', 'https://ucd-ipo.github.io/agroft'), type='source')
```

A lot of text will start popping up when this command is running. After it is done, look near the end of the output for text that says:

```
** testing if installed package can be loaded
* DONE (agroft)
```

If you see this, you are done installing Agroft. If instead you see a message that says:

```
Warning in install.packages :
  installation of package 'agroft' had non-zero exit status
```

Or something like:

```
Warning message:
package "agroft" is not available (for R version x.y.z) 
```

then Agroft was not installed. Check to make sure your R version greater than or equal to 3.2.0. If not, try the instructions listed under the tab "For  R <3.2.0".

##### For  R <3.2.0

At the R prompt, Enter the first command shown below, exactly as it appears, and press enter. The command can take a couple minutes, and a new prompt should appear directly below the old one once it is done. 

```R
install.packages('devtools', repos = 'http://cran.r-project.org')
```

A lot of text will start popping up when this command is running. After it is done, look near the end of the output for text that says:

```
** testing if installed package can be loaded
* DONE (devtools)
```


If instead you see a message that says:

```
Warning in install.packages :
  installation of package 'devtools' had non-zero exit status
```

Then check to make sure Rtools was installed correctly (if you use Windows) and try again.

Once that works, you are ready to install Agroft. Run the following command and wait for it to complete. This may take a couple minutes. You will know it is done when a new R prompt appears. 

```R
devtools::install_github('ucd-ipo/agroft')
```
--------------------
#### Starting Agroft

With Agroft successfully installed, you are ready to launch your first Agroft session. 

Every time you start a new R session, you will also need to load Agroft in order to use it. Do this by running the command below. 

```R
library(agroft)
```

This will load everything needed to start Agroft and will also launch your first Agroft session. The Agroft session will open in your default web browser.

Closing the browser window or the tab that Agroft is running in will end your Agroft session, but it will not quit R. If you want to start a new Agroft session and haven't restarted R, run the command below:

```R
launch()
```

If you want to quit R, close the R program. This will also close any active Agroft session you are running at that time. 




