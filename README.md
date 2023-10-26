# gitwic3proj
Baseline SSP2 assumptions + projection. Make your own scenario and project.

Prepared code for SSP2 projection (five R files)
1. set scenario details
2. set statespace
3. fill statespace
4. projection
5. results (graphs/tables)


Setup Folders  and copy files
1. Create a working folder (any name; let us call it 'wic3proj'). We will later created multiple sub-folders
2. Open RStudio and create an R project  in the folder "wic3proj" named "gitwic3proj" (File->New Project->New Directory-> DIRECTORY NAME as "gitwic3proj", a new folder 'gitwic3proj' will be created.
3. We will now download the files from the github account https://github.com/kcsamir/gitwic3proj and save them in the folder "gitwic3proj". I have shared a zip file for the first day (unzip it and save it in the directory).
4. Open "Main WIC.r" and start running the code

R-Studio session .....   
The following folders are created within the code   
1. Create "input_wic3proj" and "output_wic3proj" folder in the "output" folder
2. "input_wic3proj" contains all the data files (RData and csv) needed to run the first scenario - SSP2
3. "output_wic3proj" will contain an additional folder for each scenario (created within the code)
4. Next, save all the input files provided (via WeChat or email in the input_wic3proj folder)

Now, we will start the scenario (SSP2)
1. Open the first file "scen_w09_ssp2_emort_efert_emig.r". Let us go through this file.
2. ... we will setup the scenario file using file "scen_w09*.r"
3. we will create state-space for our projection "statespace.r" and "fillstatespace.r"
4. we will project using file "projection.r"
5. Finally, we will visualize the results using "Report cntry files rmd.r" (sorry for the long name!) and funstack.r
 




