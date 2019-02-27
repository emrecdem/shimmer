The R code in this repository was written to facilitate the extraction of features from multiple Shimmer wearable sensor data in short lasting (couple of hour) experiments, specific to a studies being carried out at UTwente (Netherlands). In addition to feature extraction the code includes and attempt to detect peaks in the acceleration signals and from that derive hand claps when peaks co-occur in the sensor data from both wrists.

### Assumptions the code makes about the input files

- Filename has following structure: Ses9_070518_Session1_C9BB_partB_Calibrated_SD.csv. Where,
  - Ses9 is the **session name**
  - partB is the **part** of the session
  - 070518 is the **date of measurement** and together with session name and part acts as person identifier.
  - C9BB is the **device serial number** of the Shimmer device which helps to identify on what location is worn.
- Inside the file there are column names that include the device serial number (C9BB).
  Except from the C9BB_ prefix the rest of the column names are assumed to be identical across measurements. For convenience a [csv-file](example_firstrows_shimmerfile.csv) is included in the repository with two rows of example data.
  The key columns from the file we will use are:
  - CD5D_partA_Timestamp_Unix_CAL
  - CD5D_partA_Euler_9DOF_Yaw_WR_CAL
  - CD5D_partA_Euler_9DOF_Yaw_LN_CAL
  - CD5D_partA_Euler_9DOF_Roll_WR_CAL
  - CD5D_partA_Euler_9DOF_Roll_LN_CAL
  - CD5D_partA_Euler_9DOF_Pitch_WR_CAL
  - CD5D_partA_Euler_9DOF_Pitch_LN_CAL
  - CD5D_partA_Accel_WR_Z_CAL
  - CD5D_partA_Accel_WR_Y_CAL
  - CD5D_partA_Accel_WR_X_CAL
  - CD5D_partA_Accel_LN_Z_CAL
  - CD5D_partA_Accel_LN_Y_CAL
  - CD5D_partA_Accel_LN_X_CAL
- The code currently assumes that shimmer devices are worn on the non dominant wrist, dominant wrist and/or chest.

## Step 1

- Open [extractfeatures.R](extractfeatures.R) and modify the top part such that
datafolder points to the folder where the data (csv files) sit. Also make sure that outputfolder point to the folder where you want the output to be stored. The code will create a folder in this folder named "myresults".
- Also check that the device serial numbers for each device location are correct.

## Step 2

- Source [extractfeatures.R](extractfeatures.R) in R(Studio).
- We expect output (csv) files with columns:
  - time
  -	secondsinrecording
  -	numerictime: seconds since 1-1-1970
  **variables used for peak detection**
  -	acc_percentile50: 50th percentile of magnitude of acceleration (m/s2) based on the three band-pass (5-50 Hertz) filtered acceleration signals.
  -	acc_percentile75: 75th percentile of magnitude of acceleration (m/s2) based on the three band-pass (5-50 Hertz) filtered acceleration signals.
  -	acc_percentile99: 99th percentile of magnitude of acceleration (m/s2) based on the three band-pass (5-50 Hertz) filtered acceleration signals.
  -	acc_max: maximum of magnitude of acceleration (m/s2) based on the three band-pass (5-50 Hertz) filtered acceleration signals.
  **variables for describing kinematics**
  -	acc_mean: mean of high-pass (0.2 Hertz) filtered magnitude of acceleration (m/s2) after which negative values are rounded to zero.
  -	acc_std: standard deviation of high-pass (0.2 Hertz) filtered magnitude of acceleration (m/s2) after which negative values are rounded to zero.
  -	pitch_mean: mean Euler angle pitch.
  -	roll_mean: mean Euler angle roll.
  -	yaw_mean: mean Euler angle yaw.
  -	acc_peak: 0 or 1, where 1 indicates a detected peak.

## Step 3

- Open [detecthandclap.R](detecthandclap.R) and modify the top part such that path specifies the same diretory as outputfolder in step 1. The code now assumes that there is a folder "myresults" in this folder, as created by the code in step 1. The handclap times will be stored in csv-files in the outputfolder.

## Step 4

- Source [detecthandclap.R](detecthandclap.R)  in R(Studio).
- We expect an output (csv) files with one column: timestamp.
