# ProFAst (R package)

## TODO:
- [ ] ProFAst.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] Pre_Proc.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] Multi_Detect.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] Axrat_Filter.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] Flux_Filter.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] N100_Filter.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] Group_Cutter.R
  - [ ] R-package header
  - [ ] Add proper return values for other functions
- [ ] Group_Remover.R
  - [ ] R-package header
- [ ] Linear_Fit.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
  - [ ] Add proper return values for other functions
- [ ] MPC_PSV.R
  - [ ] R-package header
  - [ ] Add temp file option, default to FALSE
- [ ] Manual_Detection_Extractor.R
  - [ ] Check if needed
- [ ] MPC_Formatter.R
  - [ ] Check if needed
- [ ] Param_Processor.R
  - [ ] Check if needed
- [ ] PSV-Fixer.R
  - [ ] Check if needed
- [ ] tempfixer.R
  - [ ] Check if needed
<!-- badges: start -->
![R-CMD-check](https://github.com/LDCKirkby/ProFAst/workflows/R-CMD-check/badge.svg)
<!-- badges: end -->

## Synopsis

ProFAst is an astronomy tool based on the **ProFound** package suite developed by Aaron Robotham (https://github.com/asgr). Designed to detect asteroids across multi-band astronomical images through key parameters that help distinguish an asteroid from other stationary points.
## Installation

### Getting R

First things first, you will probably want to install a recent version of **R** that lets you build packages from source. The advantage of choosing this route is you can then update bleeding edge versions directly from GitHub. If you rely on the pre-built binaries on CRAN you might be waiting much longer.

#### Mac

For Mac just get the latest binaries from the **R** project pages:

<https://cloud.r-project.org/bin/macosx/>

#### Windows

For Windows just get the latest binaries from the **R** project pages:

<https://cloud.r-project.org/bin/windows/>

#### Linux

Debian:	`sudo apt-get install r-base r-base-dev`

Fedora:	`sudo yum install R`

Suse:	More of a pain, see here <https://cloud.r-project.org/bin/linux/suse/README.html>

Ubuntu:	`sudo apt-get install r-base-dev`

All the info on binaries is here: <https://cloud.r-project.org/bin/linux/>

If you have a poorly supported version of Linux (e.g. CentOS) you will need to install **R** from source with the development flags (this bit is important). You can read more here: <https://cloud.r-project.org/sources.html>

Now you have the development version of **R** installed (hopefully) I would also suggest you get yourself **R-Studio**. It is a very popular and well maintained **R** IDE that gives you a lot of helpful shortcuts to scripting and analysing with **R**. The latest version can be grabbed from <https://www.rstudio.com/products/rstudio/> where you almost certainly want the free Desktop version.

If you wish to use the command line version of **R** on Mac (why?!) then you might need to separately install **XQuartz** and set the DISPLAY system variable via something like export DISPLAY=:0 (this is not an issue for most people however).

### Build Tools

Some of **ProFAst** requires compiling, so here is what you might need depending on your platform.

#### Linux Users

You know what you are doing. You do you!

#### Mac Users

You should not need to install separate compilers with any **R** after v4.0.0, but in case you are stuck on a museum version you can follow the extra instructions here:

[https://mac.r-project.org/tools/](https://mac.r-project.org/tools/)

#### Windows Users

Windows users might need to go through a couple of additional steps depending on how their system is set up, but most likely you will need to at least install *Rtools* for later parts of this course, which are available at [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/) and follow the instructions about how to link these into your system path. You will know it is working because the following will not be empty:

```R
Sys.which("make")
```

### Getting ProFAst

Source installation from GitHub should be easy:

```R
install.packages('remotes')
remotes::install_github("LDCKirkby/ProFAst")
library(ProFAst)
```
#### Package Dependencies

The above should also install the required packages. If you have trouble with this you can try installing the required packages manually first and then retry the installation for **ProFAst**:

```R
install.packages('remotes')
remotes::install_github("asgr/Rfits") #ProFAst is built on packages that aren't available to install from CRAN
remotes::install_github("asgr/Rwcs")
remotes::install_github("asgr/ProFound")
remotes::install_github("asgr/ProPane") 
remotes::install_github("LDCKirkby/ProFAst")
```

## Motivation

Seeing the strengths that ProFound has for elliptical and unconventional shaped sources inspired the attempt to use it on detecting asteroids.
Extra routines were then created to refine the output and filter out false positives. 
ProFAst is also designed to output its detections into the PSV format that the Minor Planet Center Requires for their asteroid detections. 

## License

LGPL-3+

