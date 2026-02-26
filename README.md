# IDD_App_Dev
Developing and testing IDD dashboard elements.

## Producation

Links to producation dashboards

https://dashboards.i2d2.iastate.edu/Demographic/  
https://dashboards.i2d2.iastate.edu/Economic/  
https://dashboards.i2d2.iastate.edu/Health_and_Social/  
https://dashboards.i2d2.iastate.edu/Services/  


## Development 

Links to development dashboards

https://devboards-rit-i2d2-iadatadrive.apps.nimbus.las.iastate.edu/Demographic/  
https://devboards-rit-i2d2-iadatadrive.apps.nimbus.las.iastate.edu/Economic/  
https://devboards-rit-i2d2-iadatadrive.apps.nimbus.las.iastate.edu/Health_and_Social/  
https://devboards-rit-i2d2-iadatadrive.apps.nimbus.las.iastate.edu/Services/  



## Styling 
- [App Styling](App/Docs/styling.md)




Indicators are computed (by Jamy and Fattan) and finalized formatted tables pushed to CyBox folder `Iowa IDS > Projects > _IA Data Drive > Indicators > I2D2 Data > ACS INDICATORS > Indicators_GioUpload`

Files with computed indicators are copied to /Data folder in this project directory for development purposes. After dashboard is built, all files should be move to Azure Blob space and read from there.


## QUESTIONS TO THE TEAM REAGARDING ECONOMIC INDICATORS
2025-08-29

### 1. Are the computed economic indicators finalized in `Indicators_GioUpload` folder ([CyBox link](https://iastate.app.box.com/folder/336699639719?s=96puodplevp76jhy7t9uan3scxem96cy))

- [ ] All People in Poverty > Federal Poverty Level
- [ ] Children in Poverty > Federal Poverty Level 100%
- [ ] Children in Poverty > Free or Reduced Priced Lunch
- [x] Children in Poverty > Medicaid or WIC Receipt at Birth
- [ ] Homelessness > Point in Time
- [ ] Homelessness > School homelessness
- [ ] Unemployment > Unemployment

- Any additional year to be computed?

### 2. Point in Time indicator data is missing in the `Indicators_GioUpload` folder. 

- Is that computed already and need to be dropped there?
- Who is computing?

### 3. Regarding `Eco, Demog, Heal, Definitions & breakdowns.xlsx` ([CyBox link](https://iastate.box.com/s/zbch5sntshfzii5y3h3l5cxqxxpaz3l8))

- Are the economic indicators' definitions completed?
- Should I use figure names from this file (tab name **Economic-figure-definitions**)?

### 4. Economic idnicators for ECI areas

- Should I work on having an option on dashboard for selecting ECI areas already?
- Are we ready to compute the indicators for ECI areas?


## iFrame Code 

```
<iframe 
    src="https://i2d2.shinyapps.io/IDD_Dashboard_Demographic/" 
    width="100%" 
    height="1200px" 
    style="border:none;" 
    title="IDD Demographic">
</iframe>
```
