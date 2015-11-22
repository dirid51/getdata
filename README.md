# getdata
R files for Coursera Getting and Cleaning Data course
## How the run_analysis.R file works
1. Make sure the R script can access the data files
2. Read each of the data files in using read.table. The script takes this opportunity to preemptively add the measurement types (a.k.a. "variable names") from the features.txt data file.
3. Make one great big happy data set! This script first uses cbind to add the data from subject_test.txt and activity_labels.txt to the data from the X_test.txt data file. It similarly adds subject_train.txt and activity_labels.txt to the data set from the X_train.txt file. Then it uses bind_rows from the dplyr package to glue test and train together.
4. Throw away anything that is not a mean or an average of a measurement. The script has a hard coded list of columns to keep - yes, entering the list was tedious, but it allows for precision. The script runs the select function to pull into a new data frame only those columns that are preapproved. The code book section provides a list of the columns that were selected to be kept.
5. Provide descriptive variable (column, measurement) names. Again, a hard coded list is used to create the new variable names. Each variable name was chosen in an effort to help better describe where the data comes from and what actions were performed on it to get it into it's current state. A list of the variable names is included in the code book.
6. Create a new data set from the results of what the script has done to this point that includes the mean, or average, of each measurement type within the groupings of the person, or subject, and the activity type. This was accomplished by running a group_by function on the current data set followed by the summarize_each function, which applied the mean to each of the groups generated by the group_by function. The variable names are once again modified, this time adding "Mean" to the end of the variable name to indicate that the variable contains the average of that particular measurement. The data set is then written to a text file using the write.table function.

## Code Book
Put code book here