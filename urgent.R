source("./ProFAst/Core/Pre_Proc.R")
source("./ProFAst/Core/Multi_Detect.R")
source("./ProFAst/Core/Flux_Filter.R")
source("./ProFAst/Core/Axrat_Filter.R")
source("./ProFAst/Core/N100_Filter.R")
source("./ProFAst/Core/Group_Cutter.R")

library(peakRAM,quietly = TRUE)
library(xtable, quietly = TRUE)
library(dst,quietly = TRUE)
library(celestial,quietly = TRUE)
library(devtools,quietly = TRUE)
library(Cairo,quietly = TRUE)
library(Rfits,quietly = TRUE)
library(Rwcs,quietly = TRUE)
library(ProFound,quietly = TRUE)
library(magicaxis,quietly = TRUE)
library(data.table,quietly = TRUE)
library(plotrix,quietly = TRUE)
library(foreign,quietly = TRUE)
library(MASS,quietly = TRUE)
library(ProPane, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)
library(gsubfn, quietly = TRUE)
library(fs, quietly = TRUE)
library(showtext, quietly = TRUE)
showtext_auto()

args = commandArgs(trailingOnly = TRUE)
RA_DEC = gsub("[\r\n]", "", as.character(args[[1]]))

computer = "simon"

Flux_Filter(RA_DEC)

Axrat_Filter(RA_DEC)

N100_Filter(RA_DEC)

Group_Cutter(RA_DEC, computer)