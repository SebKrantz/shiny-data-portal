# Shiny data Portal
A Shiny Application to host, filter, aggregate and and download data in various formats.

***

This folder contains the code to create a simple web-application 
to host and download data in a unified format. The application is 
developed using the 'shiny' R package. It can be deployed to a free
web-server at www.shinyapps.io, or hosted locally using shiny server
(https://rstudio.com/products/shiny/shiny-server/). 

***

## Contents

* app.R : The main file pulling everything together and running the app. Running this file should run the app once
          the correct working directory is set.

* ui_code.R : Contains the code to build the main user interface (don't rename to ui.R which is a protected name and will yield an error)

* server_code.R : Contains the code to serve and process the data and statistical computations on the data as requested by the user.
                  (also don't rename to server.R)

* style.css : A CSS stylesheet controlling the visual appearance of the application.

* get_data.R : An R file to download various macroeconomic data from the World Bank, IMF, UN etc. 
               This file can be used to get macroeconomic data for the app, but any data in any format is supported by the app, 
               as long as it is converted to a tidy data.frame in R. Datasets saved in these files should have two attributes:
               a "updated" attribute giving the date when the data was last updated (assigned through Sys.data()), and an optional
               "url" attribute providing a link to the raw data. Furthermore, each column / variable in each dataset should preferably
               have a "label" attribute describing the variable, and (optionally) another "source" attribute providing the source of 
               this variable (if distinct from the source of the dataset to which the variable belongs). 

* /data : A directory containing the data hosted in the app. Data is saved in .RData files generated from get_data.R. These files are loaded in app.R.
          In addition the directory contains two excel sheets: datasources.xlsx and datasets.xlsx. These are used to provide information about the data
          in the application. The format of these sheets should be kept (also the column names). It is important that the column 'DSID' in datasets.xlsx
          matches the names of the datasets daved in the .RData files. The subfolder /raw was created to save any temporary files from get_data.R, 
          or to place any locally supplied raw data which can be read into R and processed with get_data.R.

* /www : A directory containing the logo for the app as a png file, and any other web-content for the app. 
   
* indicator_nomenclature.xlsx : Provides some simple suggestions for developing indicator codes for locally supplied data. 


## Setting up the App

First things: Install R and Rstudio and open the project file SIMPLE DATA PORTAL.Rproj in Rstudio. Call getwd() and make sure the working directory is set in the project folder,
otherwise set it there using setwd(). Then run the app by opening the app.R file (it whould be open already) and clicking on the green 'Run App' arrow at the top-right of the editor.

Currently the app is supplied with some macroeconomic data for Uganda. To adapt the app for your purposes,
start with get_data.R, download the data for your country or process the data you want to supply, and save it in .Rdata files in the /data folder. 
Then make sure  datasources.xlsx and datasets.xlsx match your data. Then edit server_code.R (the first two blocks) switching between datasets i.e. finding
the requested data, the rest of the code is generic. For the cases where multiple versions of a dataset are supplied (i.e. a sub-switch is used in the second block),
the respective entry in ui_code.R has to be edited or removed. Finally, make sure app.R loads your files correctly and see if the generated Dataset description frame
(`DS_DESC_UPDATED`) and indicators overview table (`IND_OV`) are sensible. 

Play around with the application to see if everything works find the way you set up up. Once you have created a shinyapps.io account, you can manage and deploy the app from inside Rstudio. 
