# OracleBaseball
A repository for material related to my JMM presentation

## The files
The scripts include

*finaloracle.R: The script for performing the Oracle ratings. It requires that the parsed retrosheet event files appear in the download.folder/unzipped directory.

*parse.retrosheet.pbp.R: The script from Albert & Marchi's book, "Analyzing Baseball Data with R" for getting and parsing Retrosheet play-by-play files.

Other files include

*batters201x.csv and pitchers201x.csv files which contain the complete Oracle ratings.

*nmaster.csv The Retrosheet master names list with retrosheet player IDs, complete through 2016.

The OracleApp directory contains the server.R and ui.R files used for the OracleApp Shiny app.
