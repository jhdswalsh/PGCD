The Codebook
========
Codebook generated on 07/Jul/2016 

Variable list and descriptions
------------------------------

Variable name    | Description
-----------------|------------
subject          | ID the subject who performed the act for each window sample. Its range is from 1 to 30.
act		         | act name
ftDom		     | Feature: Time domain signal or frequency domain signal (Time or Freq)
ftIns			 | Feature: Measuring instrument (Accelerometer or Gyroscope)
ftAcc			 | Feature: Acceleration signal (Body or Gravity)
ftVar		     | Feature: Variable (Mean or SD)
ftJerk      	 | Feature: Jerk signal
ftMag    		 | Feature: Magnitude of the signals calculated using the Euclidean norm
ftAxis         	 | Feature: 3-axial signals in the X, Y and Z directions (X, Y, or Z)
ftCount        	 | Feature: Count of data points used to compute `mean_val`
ftmean_val      	 | Feature: mean_val of each variable for each act and each subject

Dataset structure
-----------------


```r
str(Tidy_Data)
```

```
## Classes 'data.table' and 'data.frame':	11880 obs. of  11 variables:
##  $ subject         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ act        : Factor w/ 6 levels "LAYING","SITTING",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ ftDom     	  : Factor w/ 2 levels "Time","Freq": 1 1 1 1 1 1 1 1 1 1 ...
##  $ ftAcc			  : Factor w/ 3 levels NA,"Body","Gravity": 1 1 1 1 1 1 1 1 1 1 ...
##  $ ftIns  		  : Factor w/ 2 levels "Accelerometer",..: 2 2 2 2 2 2 2 2 2 2 ...
##  $ ftJerk          : Factor w/ 2 levels NA,"Jerk": 1 1 1 1 1 1 1 1 2 2 ...
##  $ ftMag   		  : Factor w/ 2 levels NA,"Magnitude": 1 1 1 1 1 1 2 2 1 1 ...
##  $ ftVar    		  : Factor w/ 2 levels "Mean","SD": 1 1 1 2 2 2 1 2 1 1 ...
##  $ ftAxis          : Factor w/ 4 levels NA,"X","Y","Z": 2 3 4 2 3 4 1 1 2 3 ...
##  $ count           : int  50 50 50 50 50 50 50 50 50 50 ...
##  $ mean_val         : num  -0.0166 -0.0645 0.1487 -0.8735 -0.9511 ...
##  - attr(*, "sorted")= chr  "subject" "act" "ftDom" "ftAcc" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```


List the key variables in the data table
----------------------------------------


```r
key(Tidy_Data)
```

```
## [1] "subject"          "act"         "ftDom"      
## [4] "ftAcc" "ftIns"   "ftJerk"        
## [7] "ftMag"    "ftVar"     "ftAxis"
```


first 5 rows of the dataset
------------------------------


```r
Tidy_data
```

```
##        subject         act ftDom ftAcc ftIns
##     1:       1           LAYING       Time               NA      Gyroscope
##     2:       1           LAYING       Time               NA      Gyroscope
##     3:       1           LAYING       Time               NA      Gyroscope
##     4:       1           LAYING       Time               NA      Gyroscope
##     5:       1           LAYING       Time               NA      Gyroscope
##    ---                                                                    
## 11876:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer
## 11877:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer
## 11878:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer
## 11879:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer
## 11880:      30 WALKING_UPSTAIRS       Freq             Body  Accelerometer
##        ftJerk ftMag ftVar ftAxis count  mean_val
##     1:       NA            NA         Mean        X    50 -0.01655
##     2:       NA            NA         Mean        Y    50 -0.06449
##     3:       NA            NA         Mean        Z    50  0.14869
##     4:       NA            NA           SD        X    50 -0.87354
##     5:       NA            NA           SD        Y    50 -0.95109
##    ---                                                            
## 11876:     Jerk            NA           SD        X    65 -0.56157
## 11877:     Jerk            NA           SD        Y    65 -0.61083
## 11878:     Jerk            NA           SD        Z    65 -0.78475
## 11879:     Jerk     Magnitude         Mean       NA    65 -0.54978
## 11880:     Jerk     Magnitude           SD       NA    65 -0.58088
```


Summary of variables
--------------------


```r
summary(Tidy_Data)
```

```
##     subject                   act    ftDom  ftAcc
##  Min.   : 1.0   LAYING            :1980   Time:7200   NA     :4680    
##  1st Qu.: 8.0   SITTING           :1980   Freq:4680   Body   :5760    
##  Median :15.5   STANDING          :1980               Gravity:1440    
##  Mean   :15.5   WALKING           :1980                               
##  3rd Qu.:23.0   WALKING_DOWNSTAIRS:1980                               
##  Max.   :30.0   WALKING_UPSTAIRS  :1980                               
##        ftIns ftJerk      ftMag  ftVar ftAxis 
##  Accelerometer:7200   NA  :7200   NA       :8640   Mean:5940    NA:3240  
##  Gyroscope    :4680   Jerk:4680   Magnitude:3240   SD  :5940    X :2880  
##                                                                 Y :2880  
##                                                                 Z :2880  
##                                                                          
##                                                                          
##      count         mean_val       
##  Min.   :36.0   Min.   :-0.9977  
##  1st Qu.:49.0   1st Qu.:-0.9621  
##  Median :54.5   Median :-0.4699  
##  Mean   :57.2   Mean   :-0.4844  
##  3rd Qu.:63.2   3rd Qu.:-0.0784  
##  Max.   :95.0   Max.   : 0.9745
```


Save to file
------------

Save data table objects to a tab-delimited text file called `Tidy_Data.txt`.


```r
f <- file.path("Tidy_Data.txt")
write.table(Tidy_Data, f, quote = FALSE, sep = "\t", row.names = FALSE)
```
