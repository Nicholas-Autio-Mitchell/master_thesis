## ================================================================== ##
##  Clean just the text returned from the Python "text_extractor.py"  ##
## ================================================================== ##

## Clear workspace and open R session
rm(list=ls())

## The "magrittr" package is required for helpful semantics
if(require("magrittr")) {
  message('Package "magrittr" loaded correctly')
} else {
    install.packages("magrittr")
    library("magrittr")
}

## Ensure other necesary packages are loaded
library("tm")
library("dplyr")

## List files in a distant folder --> with $: doesn't include backup files e.g. *.txt~
#dir(path = "/Volumes/Mac OS Drive/Data Backup/Data/scrape_raw/Dow Jones/", pattern = "*.txt$")

## ============================ ##
##  Clean each tweet-text file  ##
## ============================ ##

## Input
if (Sys.info()[['sysname']] == "Darwin") {
    sep = "/"
    input_path = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/text_only/"
    output_path = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/clean/"

} else if (Sys.info()[['sysname']] == "win32") {
    sep = "/"
    input_path = "c:/Users/dev231/Documents/Thesis/Data/sentiment_analysis/input/text_only/"
    output_path = "c:/Users/dev231/Documents/Thesis/Data/sentiment_analysis/input/clean/"
    
}

## Output

## These are the folders of the search terms "Dow Jones", "bear market", etc.
input_folders = dir(input_path)

## If html files must be used, then:
##filenames = append(dir(path = input_folder, pattern = c("*.txt$")), dir(path = input_folder, pattern = c("*.html$")))

## Loop through each of the folders, cleaning each of the files in each folder and writing it out ready for SA
for( i in 1:length(input_folders)) {

    curr_folder = input_folders[i]
    input_files = dir(path = paste0(input_path, curr_folder), pattern = "*.txt$")

    output_folder = paste0(output_path, curr_folder, sep)
    
    ## If folder doesn't exist, create it
    ifelse(dir.exists(output_folder), print(paste0(curr_folder, "--> folder already exists")), dir.create(output_folder, recursive = TRUE))

    for( i in 1:length(input_files)) {

        input_folder = paste0(input_path, curr_folder, sep)
        curr_file = paste0(input_folder, input_files[i])

        dirty_file <- read.csv(file = curr_file, header = TRUE, sep = "\n", quote = "", row.names = NULL, stringsAsFactors = FALSE)

        ## Hex codes: http://www.asciitable.com/
        clean_file <-  sapply(dirty_file, function(x) {
            gsub("&amp;", "&", x) %>%
                gsub("[^\\x{20}-\\x{7F}]", "", ., perl = TRUE) %>%   ## Leaves only ascii characters and removes non-printing characters
                gsub("\\x{22}*", "", ., perl = TRUE) %>%  # "
                gsub("\\x{23}*", "", ., perl = TRUE) %>%  # #
                gsub("\\x{24}*", "", ., perl = TRUE) %>%  # $
                gsub("\\x{25}*", "", ., perl = TRUE) %>%  # %
                gsub("^\\x{27}", "", ., perl = TRUE) %>%  # '                
                gsub("\\.{2,}", " ", ., perl = TRUE) %>%  # two or more .
                gsub("\\x{2A}*", "", ., perl = TRUE) %>%  # *
                gsub("\\x{5B}*", "", ., perl = TRUE) %>%  # [
                gsub("\\x{5D}*", "", ., perl = TRUE) %>%  # ]
                gsub("\\x{5F}*", "", ., perl = TRUE) %>%  # _
                gsub("\\x{60}*", "", ., perl = TRUE) %>%  # `
                gsub("[\\x{7B}-\\x{7F}]", "", ., perl = TRUE) %>%  # 5 characters: { | } ~ DEL
                gsub("http\\S+\\s*", "", .) %>%  # URLs
                gsub("^\\.?", "", .) %>%  # Remove any . at beginning of string
                stripWhitespace() %>%
                gsub("^ ", "", .) %>%
                gsub(" $", "", .)
                ##gsub("\"", "", ., perl=TRUE) %>%
                ##gsub("[^[:alnum:][:space:]\\'-]", "", .) %>%            
        })

        ## Define output file's name
        clean_name = gsub(pattern = ".txt_dirty_text.txt", replacement = "_clean_text.txt", x = input_files[i])
        clean_out_file = paste0(output_folder, clean_name)
        
        ## clean_name = paste0(substr(input_files[i], 1, gregexpr(pattern = "/20",
        ##                                                   text = curr_file)[[1]][1] - 1),
        ##                     "_SA_ready.txt")

        ## Write file
        write.table(x = clean_file, file = clean_out_file, sep = "\t", fileEncoding = "ASCII", quote = FALSE, row.names = FALSE, col.names = FALSE)

    }
}

#########################################################################################################

## ================================== ##
##  OLD METHODS FOR TESTING PURPOSES  ##
## ================================== ##

## ================== ##
##  Clean the tweets  ##
## ================== ##
##  y <- read.csv(file = curr_file, header = TRUE, sep = "\n", quote = "", row.names = NULL, stringsAsFactors = FALSE)

## y$transformed1 <- sapply(y, FUN = function(x) gsub("&amp;", "&", x))
## y$transformed2 <- sapply(y$transformed1, FUN = function(x) gsub(pattern = "http\\S+\\s*", replacement = "", x)) # carefully remove links and only links
## y$transformed3 <- sapply(y$transformed2, FUN = function(x) gsub("[^[:alpha:][:space:]&\']", "", x)) # Remove anything other than alphanumberic characters, (remaining spaces and )
## y$transformed4 <- sapply(y$transformed3, FUN = function(x) stripWhitespace(x)) # Remove cases of >1 space together, tabs, carriage returns etc.
## y$transformed5 <- sapply(y$transformed4, FUN = function(x) gsub("^ ", "", x)) # Remove spaces at beginning of line
## y$transformed6 <- sapply(y$transformed5, FUN = function(x) gsub(" $", "", x)) # Remove spaces at end of line
## Seems to make no difference following previous steps
## y$transformed7 <- sapply(y$transformed6, FUN = function(x) gsub("[ |\t]{2,}", "", x)) # Remove any spaces created by tabs - "\t"

## ================================ ##
##  Check that each step is useful  ##
## ================================ ##
## Should all return TRUE
## identical(y$text, y$transformed0)
## identical(y$transformed1, y$transformed0)
## identical(y$transformed1, y$transformed2)
## identical(y$transformed3, y$transformed2)
## identical(y$transformed3, y$transformed4)
## identical(y$transformed5, y$transformed4)
## identical(y$transformed5, y$transformed6)
## ## Seems to make no difference following previous steps
## identical(y$transformed7, y$transformed6)

## ========================================= ##
##  Write cleaned text to file ready for SA  ##
## ========================================= ##

## write.table(y$transformed6, file = paste0(output_folder, out_file), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)

## ============================================ ##
##  Test if all tweets contain term e.g. "dow"  ##  --> Currently not working
## ============================================ ##

## term = "dow"
## ## Niko - example function to check if dow was mentioned in tweet x
## dowMentioned <- function(x, string = term) string %in% unlist(strsplit(x, " " )) 
## data_dow$mentioned <- as.numeric(dowMentioned(data_dow$Tweet, term)) # 'dow' always mentioned (as expected)
## ## My attempt
## dowMentioned <- function(x, string="dow") for (i in 1:dim(x)[1]){string %in% x$text[i]}
## dowMentioned <- function(x, string="dow") ifelse(grepl(paste("(?i)", string), x, ignore.case = TRUE, useBytes = TRUE), TRUE, FALSE)
## x$mentioned <- as.numeric(dowMentioned(x, "dow")) # 'dow' always mentioned (as expected)

## ====================== ##
##  Test the pipe syntax  ##
## ====================== ##

## file_name = "2013-11-01_til_2013-09-05.txt_SA_input.txt"
## in_file <- read.csv(file = file_name, header = TRUE, sep = "\n", quote = "", row.names = NULL, stringsAsFactors = FALSE)

## out_file <-  sapply(in_file, function(x) {
##     gsub("&amp;", "&", x) %>%
##     gsub("http\\S+\\s*", "", .) %>%
##     gsub("[^[:alpha:][:space:]&\']", "", .) %>%
##     stripWhitespace() %>%
##     gsub("^ ", "", .) %>%
##     gsub(" $", "", .)
## }) 
