**********
* README *
**********

This is the README for "Ideological Moderation and Success in U.S. Elections, 2020-2022" by Michael A. Bailey and Benjamin F. Reese. These files contain the data and code to replicate all of the results in the main text and the Online Appendix. More information about the ideal point estimates can be found in the Online Appendix. All of the code for the analyses was written in R.

*****************************
* Replication File Contents *
*****************************

- README.txt
- JOP_Replication.R
- JOP_Replication_ONLINE_APPENDIX.R
- Data
- h_functions
- JOP_2025_Replication.Rproj

"README.txt" is this general information file explaining the contents of the replication files.

"JOP_Replication.R" is the R script that contains the code for all of the analyses in the main text.

"JOP_Replication_ONLINE_APPENDIX.R" is the R script that contains the code to run all of the analyses in the Online Appendix.

"Data" is a subfolder that contains the data for the analyses.

"h_functions" is a subfolder that contains the functions used to execute the analyses. 

"JOP_2025_Replication.Rproj" is the R project file that can be used to make file pathing easier.

****************************
* Replication Instructions *
****************************

All of the code to replicate the analyses in the main text and the Online Appendix is in "JOP_Replication.R" and "JOP_Replication_ONLINE_APPENDIX.R", respectively. To execute the main paper analysis: (1) download all of the replication files; (2) ensure all of the R packages mentioned in the R script (also mentioned at the end of this README) are installed; (3) open "JOP_2025_Replication.Rproj" or set the correct working directory with the correct file pathing to read in the data and functions. Brief additional instructions are included at the top of the R scripts.

****************
* Data Sources *
****************

All of the data sources used in the analyses are included in the "Data" folder, cited in the References, and described in the main text and Online Appendix.

*************************
* Software Dependencies *
*************************

The analyses were replicated in R version 4.4.2 - "Pile of Leaves" on a Windows 11 operating system. 

The following R packages are required to run "JOP_Replication.R" and "JOP_Replication_ONLINE_APPENDIX.R".

- tidyverse 2.0.0
- estimatr 1.0.4
- tinytex 0.54
- gridExtra 2.3.0
- readxl 1.4.3
- scales 1.4.0
- colorspace 2.1-1
- kableExtra 1.4.0
- haven 2.5.4
- patchwork 1.3.0
- fixest 0.12.1

*********************************
* Estimated Replication Runtime *
*********************************

All results replicated on:

Processor	Intel(R) Core(TM) i7-10750H CPU @ 2.60GHz   2.59 GHz
Installed RAM	16.0 GB
System type	64-bit operating system, x64-based processor

The following are the estimated runtimes to replicate the results for the main text and online appendix based on the specifications mentioned above:

JOP_Replication.R
- 13 seconds

JOP_Replication_ONLINE_APPENDIX.R
- 14 seconds

