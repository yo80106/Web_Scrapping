library(taskscheduleR)
library(shiny)
library(miniUI)
myscript <- system.file("extdata", "currency.R", package = "taskscheduleR")
## run script once within 62 seconds
taskscheduler_create(taskname = "currency_minute", rscript = myscript, schedule = "MINUTE", starttime = "10:39", modifier = 3, startdate = format(Sys.Date(), "%Y/%m/%d"))#, rscript_args = c("39.5", "38.9", "sell"))

## get a data.frame of all tasks
tasks <- taskscheduler_ls()
str(tasks)

## log file is at the place where the helloworld.R script was located
mylog <- system.file("extdata", "currency.log", package = "taskscheduleR")
cat(readLines(mylog), sep = "\n")

## delete the tasks
taskscheduler_delete(taskname = "currency_minute")
