
##ReadMe
*Please download script - run_analysis.R from github into your local working directory. Working directory can be got by getwd()
*In the console run the command - source("run_analysis.R")
*The program first checks if the dataset(UCI HAR Dataset) exists in your working directory (getwd() ), if it does not exists it
downloads the files, if not it continues to use the file.
*Current task performed is indicated on the console a message is displayed after completion of each major task.
*At various points data is checked for integrity, if there is any issue, execution will stop and you will have to retrigger
the process.
*Abrupt termination of program, will delete the dataset(UCI HAR Dataset), so that subsequent run will download fresh dataset.
*Refer to CodeBook.md for more details.

### The two files generated by the program will be availabe will available in folder 'analysis_out' in your working directory.
*First file created 'analysis_out/TrainTest_MnStd_wAct.txt' having mean,std measures.
*Second file created 'analysis_out/TrainTest_Avg_ByActSub.txt' where average is computed by activity, subject

