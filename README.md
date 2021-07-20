# Shiny Data Portal
A free Shiny Application to host, filter, aggregate and and download data in various formats. It can easily be adapted for any purpose and used with any data that can be converted to tidy data frames (spreadsheet format). 

**Running Application:** [https://sebkrantz.shinyapps.io/shiny-data-portal/](https://sebkrantz.shinyapps.io/shiny-data-portal/)

***

This folder contains the code to create the application as it is shown [here](https://sebkrantz.shinyapps.io/shiny-data-portal/). 
The application is developed using the 'shiny' R package. It can be deployed to a free
web-server at www.shinyapps.io, or hosted locally using shiny server
(https://rstudio.com/products/shiny/shiny-server/). 

***

## Contents

* **app.R** : The main file pulling everything together and running the app. Running this file should run the app.

* **ui_code.R** : Contains the code to build the main user interface (don't rename to ui.R which is a protected name and will yield an error)

* **server_code.R** : Contains the code to serve and process the data and statistical computations on the data as requested by the user (also don't rename to server.R).

* **style.css** : A CSS style sheet controlling the visual appearance of the application.

* **get_data.R** : (Optional) R file to download and process various macroeconomic data from the World Bank, IMF, UN etc. and 
                   local sources (Bank of Uganda in this case) and save them in .RData files in the **/data** folder. 
                   This file can be adapted to get macroeconomic data for the app, but any data in any format is supported by the app, as long as it is converted to a tidy data.frame in R. Datasets saved in these .RData files should have two attributes:
               a `"updated"` attribute giving the date when the data was last updated (assigned through `Sys.Date()`), and an optional
               `"url"` attribute providing a link to the raw data. Furthermore, each column / variable in each dataset should preferably
               have a `"label"` attribute describing the variable, and (optionally) a `"source"` attribute providing the source of 
               this variable (if distinct from the source of the dataset to which the variable belongs). 

* **/rawdata** : (Optional) folder to save any temporary files from get_data.R, or to place any locally supplied raw data which can be read into R and processed with get_data.R.

* **/data** : A folder containing the processed data hosted in the app. Data is saved in .RData files generated from get_data.R. 
          In addition the directory contains two excel sheets: **datasources.xlsx** and **datasets.xlsx**. These are used to provide information about the data
          in the application. The format of these sheets should be kept (also the column names). It is important that the column `DSID` in **datasets.xlsx**
          matches the names of the datasets saved in the .RData files. 
          
* **load_all.R** : Based on the .RData files and **datasources.xlsx** and **datasets.xlsx** saved under **/data**,  
                   this file generates dataset and indicator overview tables and saves them, together with all the datasets, 
                   in a central **load_all.RData** file which is loaded in app.R. 

* **/www** : A directory containing the logo for the app as a .png file, and any other web-content for the app. 
   
* **indicator_nomenclature.xlsx** : Provides some simple suggestions for developing indicator codes for locally supplied data. 

*Note:* To run the app, only the files app.R, ui_code.R, server_code.R, styles.css, and /www and load_all.RData are needed. Only these files should be uploaded to the server for deployment of the app.  

## Setting up the App

First things: Install R and Rstudio, download this repository and unpack it to a project folder. Then open the project file shiny-data-portal.Rproj in Rstudio. Call `getwd()` and make sure the working directory is set in the project folder, otherwise set it there using `setwd()`. Then run the app by opening the app.R file, installing any required packages (listed at the top of app.R) and clicking on the green 'Run App' arrow at the top-right of the Rstudio editor. The app should run. 

## Customizing the App

Currently the app is supplied with some macroeconomic data for Uganda. To adapt the app for your purposes,
start with get_data.R, download the data for your country or process the data you want to supply, and save it in .Rdata files in the /data folder. Then make sure datasources.xlsx and datasets.xlsx match your data. Then edit server_code.R (the first two blocks) switching between datasets i.e. finding
the requested data, the rest of the code is generic. For the cases where multiple versions of a dataset are supplied (i.e. a sub-switch is used in the second block), the respective entry in ui_code.R commented as 'Special cases' has to be edited or removed. Then move to load_all.R, make sure the dataset description table (`DS_DESC_UPDATED`) and indicators overview table (`IND_OV`) are generated / updated properly and saved, together with all data in load_all.RData. Finally, make sure app.R loads load_all.RData correctly and run the app. 

Play around with the application to see if everything works fine the way you set up up. Once you have created a shinyapps.io account, you can manage and deploy the app from inside Rstudio. For this click the blue circle at the top-right of the Rstudio editor when app.R is opened, click 'Publish Application', and paste the token from shinyapps.io to connect to the free account you have created on shinyapps.io. Then select the files app.R, ui_code.R, server_code.R, styles.css, www/logo.png and load_all.RData to upload to shiny (unselect the other files/folders) and click 'Publish'. Wait for the app to build and then you will be directed to the web. Congratulations! You have created your own online data platform!
