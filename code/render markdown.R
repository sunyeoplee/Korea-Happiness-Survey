
library(rmarkdown)

# render r markdown files
{
version <- 'v1'
date <- '20210925'
filename <- paste(date, version, sep='_')

rmarkdown::render(input = "C:\\Users\\USER\\Desktop\\Backup\\Gap year\\Korea Happiness Survey\\Korea-Happiness-Survey\\code\\data_analysis.Rmd", 
                  output_file = filename,
                  output_dir = 'C:\\Users\\USER\\Desktop\\Backup\\Gap year\\Korea Happiness Survey\\outputs',
                  clean = TRUE)
}
