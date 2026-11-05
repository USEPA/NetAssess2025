# NetAssess2025
Tools for the 2025 5-year Ambient Air Monitoring Network Assessments

URL: https://rconnect-public.epa.gov/NetAssess2025/

Update of the NetAssess2020 app (https://sti-r-shiny.shinyapps.io/epa_network_assessment)

Based on the NetAssess app from LADCO (https://ladco.shinyapps.io/NetAssessApp/)

Author: Ben Wells, US EPA

Latest Update: November 5, 2026

## Description

The NetAssess2025 app is meant to assist State, Local and Tribal air monitoring agencies with the 5-year Ambient Air Monitoring Network Assessments as required by 40 CFR §58.10(d). The tool provides several data analysis options, including adding and removing sites from the air monitoring network, estimating the population served by each monitor, calculating correlation between monitors, estimating monitor removal bias, and probability of NAAQS exceedances based on data fusion products.

## Repository contents

This repository contains the source code and other files for the NetAssess2025 R shiny app. The app data is contained in an RSQLite database, which is not provided in this repository as it is too large, but can be obtained by contacting the app author at Wells.Benjamin@epa.gov. The data refresh script used to update the app is included in this repository for developers, but it requires direct SQL access to the AQS database. Individual file contents are listed below.
1. `global.R` - Loads dependencies and sets global environment variables used in the NetAssess2025 app.
2. `server.R` - R shiny server-side functions that control how data processing is handled in the app.
3. `deploy.R` - Deployment script for Posit Connect deployments to the staging server only.
4. `updatedb.R` - Script used to refresh the app data in `netassess.sqlite`. Provided for developers only, not needed to run the app.
5. `www/index.html` - Primary script controlling the user-interface.
6. `www/css/` - Folder containing CSS scripts used in the app.
7. `www/data/` - Folder containing data files used in the app.
8. `www/fonts/` - Folder containing custom fonts used in the app.
9. `www/images/` - Folder containing images used in the app.
10. `www/js` - Folder containing Javascript scripts used in the app.

## Requirements and credentials needed
The app requires the R packages data.table, deldir, plyr, reshape2, RSQLite, sf, stringr, and shiny; deploy.R additionally requires rsconnect and Git.

For local use, copy .Renviron.example to .Renviron in the repository root and fill in the settings required for your workflow. Set environment vairables in your local .Renviron file for AQS API access. Restart R after creating or editing .Renviron so the values are loaded. Public deployments also need the app_owner environment variable and access to the configured pins board.

The data refresh script, `updatedb.R` uses environment variables `AQS_userid` and `AQS_passwd` for direct AQS database access. These are separate from the app's credentials and are not needed to run or publish the Shiny app.

## How to refresh the supporting data
The data file `NetAssess.sqlite` is not available in this repository but can be obtained by contacting the app author at Wells.Benjamin@epa.gov. The script `updatedb.R` can also be used to create this file. The script primarily retrieves AQS data and stores it in database tables, but it also pulls from some U.S. Census Bureau datasets, which must be downloaded manually. Download instructions are contained in the `updatedb.R` script.

## How to get this application running locally
1. Clone this repository to your local machine.
2. Contact the app author at Wells.Benjamin@epa.gov to request the `NetAssess.sqlite` data file needed to run the app. This file should be copied to your local clone of this repository.
3. In RStudio, open the repository folder, then run the app from server.R, or run shiny::runApp() from the R console.

## Using the application
The app opens with a tour explaining how to use each feature within the app. The tour can be accessed at any time using the `Help` button on the right hand side of the screen.

## Testing and deployment to staging
Local deployments will need to request the `NetAssess.sqlite` data file or refresh it using the `updatedb.R` script.

### Option 1: Deploy to staging from RStudio
1) Check out the feature or review branch you want to test.
2) Run the app locally and confirm it uses the bundled data.
3) Use RStudio's **Publish** button and select the staging Connect server and account.

### Option 2: Deploy to staging with `deploy.R`
This script is for staging only. Configure the staging server URL and staging credentials in your local `.Renviron` file (or in the deployment environment):

```text
CONNECT_STAGING_SERVER=https://your-staging-connect-server
CONNECT_STAGING_ACCOUNT=your_account
CONNECT_STAGING_API_KEY=your_api_key
```

`CONNECT_STAGING_SERVER` is required; there is no fallback to a generic or public-server setting. `CONNECT_STAGING_ACCOUNT` defaults to `dmap`, and the API key can be omitted if `rsconnect` is already configured for that staging server and account. Set the URL to the staging Connect server. The script creates a branch-specific app name prefixed with `ee_dvcalc_staging_`. It can be run from any branch from the repository root, directly in the RStudio console.

## Public deployment
`deploy.R` does not deploy to the public server. Public deployment must use a separate, explicitly reviewed workflow; confirm the target, app name, data source, and hosted credentials with the developer before publishing public content. Public deployments need the `app_owner` environment variable.

## Contact
Ben Wells, US EPA (Wells.Benjamin@epa.gov)
