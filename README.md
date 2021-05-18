README
================

## Body-Brain Imaging scripts

This repository contains the data-preparation, and data-processing
scripts used in the first CoMorMent Body-Brain imaging project:

*Gurholt et al., Population-based body-brain mapping links brain
morphology with anthropometrics and body composition, Translational
Psychiatry, 2021 **xxx:xxx**, **doi=xxx***

Please credit this work if you choose to use or adapt these scripts.

These scripts have been applied to *UK Biobank* data - including brain
imaging data processed in FreeSurfer 5.3.0. After acquiring access to
the UK Biobank data, you need to extract the necessary data and ensure
they are on the same format as expected by the scripts.

To run the scripts, you will need to specify **paths**, and
**input-files** in the base- and init-files, based on your local
settings. The data processing is performed in two steps:

1.  Run the data-preparation scripts (*data\_preparation\_BodyBrain.R*).
2.  Run the data-analysis scripts (*do\_analysis.R*).

If you encounter problems when running the
*data\_preparation\_BodyBrain.R*, please check your settings, input
files, and that your variable naming etc. corresponds to the
expectations of the scripts.

These scripts were developed in a linux environment.

## Funding

This work was funded by the:

  - European Union’s Horizon2020 Research and Innovation Programme
    (CoMorMent project; Grant \#847776)
  - European Research Council (ERC) StG (Grant \#802998)
  - Research Council of Norway (\#223273; \#276082)
  - South-Eastern Norway Regional Health Authority (\#2017112;
    \#2020060).

## Feedback

If you encounter any issues, please let us know by creating an
[issue](https://github.com/comorment/BodyBrainImaging/issues).
