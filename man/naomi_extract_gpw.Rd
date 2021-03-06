% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_population.R
\name{naomi_extract_gpw}
\alias{naomi_extract_gpw}
\title{Extract Gridded Population of the World (GPW) raster data}
\usage{
naomi_extract_gpw(areas, gpw_path = "~/Data/population/GPW 4.11/")
}
\arguments{
\item{areas}{Naomi area hierarchy dataset with boundaries.}

\item{gpw_path}{Local path to GPW v4.11 raster files.}
}
\value{
A data frame formatted as Naomi population dataset.
}
\description{
Extract Gridded Population of the World (GPW) raster data
}
\details{
This function relies on accessing GPW population files via a local path
to the GPW v4.11 rasters because the files are very large.

Datasets are downloaded from:
\itemize{
\item Age/sex stratified populations for 2010: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-basic-demographic-characteristics-rev11/data-download (each file ~2GB).
\item Total population in 2000, 2005, 2010, 2015, 2020 (unraked): https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download (each file ~400MB).
}

Downloaded datasets should be saved in the following directory structure under
\code{gpw_path}:

~/Data/population/GPW 4.11/
├── Demographic characteristics
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a000_004_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a005_009_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a010_014_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a015_019_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a020_024_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a025_029_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a030_034_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a035_039_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a040_044_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a045_049_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a050_054_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a055_059_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a060_064_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a065_069_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a070_074_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a075_079_2010_30_sec_tif
│   ├── gpw-v4-basic-demographic-characteristics-rev11_a080_084_2010_30_sec_tif
│   └── gpw-v4-basic-demographic-characteristics-rev11_a085plus_2010_30_sec_tif
└── Unraked
├── gpw-v4-population-count-rev11_2000_30_sec_tif
├── gpw-v4-population-count-rev11_2005_30_sec_tif
├── gpw-v4-population-count-rev11_2010_30_sec_tif
├── gpw-v4-population-count-rev11_2015_30_sec_tif
└── gpw-v4-population-count-rev11_2020_30_sec_tif
}
