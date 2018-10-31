# GCAC

GCAC can be used as virtual screening platform for drug discovery process. User may create , share and use predictive model that can be used to predict the activity of given compound. The GCAC uses PaDEL for descriptor calculation and R-caret for predictive modelling. “MayaChemTools” used for extracting the potential compounds from large compound library (SDF/Mol). Entire GCAC pipeline is available as galaxy wrapper which ensures reproducibility and sharing of code and data across the globe.

# Availability 

   1. As Virtual Machine ([Link](https://drive.google.com/file/d/1ZQM5x-9J49rvPy9SoWOrM3Xm1MHZdr1I/view?usp=sharing))
   2. As Toolshed Repository ([Link](https://toolshed.g2.bx.psu.edu/repository?repository_id=351af44ceb587e54))
   3. As Demo Server ([Link](http://ccbb.jnu.ac.in/gcac))

If Toolshed repository is used for installation in galaxy then following prerequisites are need to be address. 

Prerequisites :
1) The system package dependencies for GCAC.
The following yum packages need to be installed on the Galaxy instance host
machine.
- ed
- mawk
- readline
- readline-devel
- zlib
- zlib-static
- zlib-devel
- bzip2
- bzip2-devel
- openmpi-devel
- libpng
- libpng-static
- libpng-devel
- NLopt-devel
- texlive-*
- texinfo
- java-1.8.0-openjdk-devel

2) The latex package dependencies for GCAC.
The following latex packages need to be installed on the Galaxy instance host
machine.
- algorithm2e
- relsize

# Note:
Manual available at demo server. You may download manual from here also. Other important documents are available at demo server “http://ccbb.jnu.ac.in/gcac”. 

