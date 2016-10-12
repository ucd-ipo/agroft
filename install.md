---
title: "Installation"
output: html_document
---

### Installing R

Since Agroft is based on the powerful R language, you must first download and install R on your computer. R is available for all popular operating systems. Go to [https://cran.r-project.org/](https://cran.r-project.org/) and click the link that says "Download R for \<your system\>" that corresponds to your operating system. Follow the instructions provided to download the R installer.  Once downloaded, run the file to begin the installation process.

If you already have an older version of R installed on your computer, you may not have to install a new version, but using an old version may complicate the installation of Agroft. Your version of R should be at least version 3.0.0. If your existing R version is less than that, you will need to upgrade to the latest version.

**Important:** Windows and OSX users will need to first install additional software before installing Agroft. See the tab below for your operating system for more information.

#### Additional Software {.tabset}

##### Windows

The additional software required by Windows to run Agroft is called "Rtools" and is made by the same developers who created R. It gives your computer the extra tools it needs to run specific code.

To install Rtools, go to [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/) and download the Rtools exe file that is compatible with the version of R you just installed. Double click on the Rtools.exe file after it finishes downloading to install it.

***

##### OSX

The additional software required by OSX to run Agroft, "Xcode Command Line Tools" is made and distributed by Apple. If you are running OSX 10.9 or higher, open a terminal and execute the following command:

```
xcode-select --install
```

Follow the instructions on your screen as they appear to install Xcode command line tools.

See [http://stackoverflow.com/a/9329325](http://stackoverflow.com/a/9329325) for more information on this.

***

### Installing Agroft

Once R and your operating system-specific software have been installed, you are ready to install Agroft.

Start by opening the R application. A window will open and some text will appear that will look similar to the following:

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

Under that text will be the "R prompt", which is a single "greater-than-symbol" (looks like `>`). To interact with R, the user types special commands at that prompt that tell R to do different things. R is very powerful, but can also be difficult to learn. Agroft allows you to use R's abilities without having to learn any programming. Despite this, you will have to enter a few simple commands to get Agroft working.

The commands are slightly different for Windows and OSX, so make sure to enter the commands for your operating system.

**Important:** If your version of R is less than 3.2.0 because you already have R installed and choose to not upgrade, look at the instructions on the tab called "For R <3.2.0", regardless of your operating system.


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
#### Github Issues Page
```
Warning message:
package "agroft" is not available (for R version x.y.z)
```

then Agroft was not installed. Check to make sure you installed Rtools correctly and your R version is greater than or equal to 3.2.0. If not, install Rtools correctly and then try the instructions listed under the tab "For  R <3.2.0".

***

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

then Agroft was not installed. Check to make sure your R version is greater than or equal to 3.2.0 and that you have correctly installed Xcode Command line Developer Tools. If not, try the instructions listed under the tab "For  R <3.2.0" after Xcode Command line tools have been installed.

***

##### For  R <3.2.0 {#install-old}

At the R prompt, Enter the first command shown below, exactly as it appears, and press enter. The command will take a couple minutes, and a new prompt should appear directly below the old one once it is done.

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

Then check to make sure Rtools or Xcode developer tools was installed correctly and try again.

Once that works, you are ready to install Agroft. Run the following command and wait for it to complete. This may take a couple minutes. You will know it is done when a new R prompt appears.

```R
devtools::install_github('ucd-ipo/agroft')
```

***

#### Issues

If you still can't get Agroft installed, see the section on installing Agroft in [Troubleshooting](#trouble-install).

#### Launching Agroft

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

#### Automatically Starting Agroft

If you will only be using R to run Agroft, you can tell R to automatically load Agroft and start an Agroft session when you open the R application. You can do this using the `setup` function in Agroft.

Run the following command to begin this process:

```R
agroft::setup()
```
Some information about automatically starting Agroft will be printed on the screen and you will be asked "Do you want Agroft to open automatically when you start R?"

Type "yes", "no", or "cancel" and press Enter. If you chose "yes", the next time you open R, an Agroft session will start automatically.

If at anytime you change your mind, you can re-run `agroft::setup()`, and change your answer. The next time you start R, your new decision will go into effect and will remain in effect until you change it again.
