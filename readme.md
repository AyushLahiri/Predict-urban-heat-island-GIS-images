## Repo for Final_project Ayush Lahiri

The `data` folder contains two subfolders with the datasets. This file needs to be unzipped first due to its size:
- `Source Raw Data` contains the raw dataset which is processed in the R script to create the final images used in training the model. Each city whose data is utilized in the final dataset has its source raw data in separate folders. Data sources include shape files for each city, census block group demarcation shapefiles, a buildings vector file, a roads vector file, green cover raster file, and UHI raster files.
- `Final Dataset_train_test_split` contains two subfolders:
  - `aerial_splits` which contains the final images in train-test split subfolders, which act as input to the aerial image classification models.
  - `segmented_splits` contains the final images in train-test split subfolders, which act as input to the segmented image classification models.

The `R scripts` folder contains two subfolders, one for creating the final aerial image dataset and another for creating the segmented images dataset. Each of these folders contains a unique script for Chicago, LA, and NY to process their data.of the format `Lahiri_Stage5__Subset_data_(cityname)_aerial` for processing aerial images 
an `Lahiri_Stage5__Subset_data_(cityname)` for processing segmented images.

`Python Scripts (htmls and notebooks)` contains three python scripts. These notebooks are shared in both html and ipynb formats:
- `Lahiri_Stage5_Descriptives_train_test_split` generates the descriptive statistics presented in the final report and shows the pipeline for stratified sampling and creation of train-test splits of images.
- `Lahiri_Stage5_Final_project_segmented_classification` notebook contains the full experimentation pipeline for training various CNNs, saves trained models, and creates sample feature maps for investigating segmented images.
- `Lahiri_Stage5_Final_project_aerial_classification` does the same for aerial images.


The final report and presentation are available as separate files in the overall folder.