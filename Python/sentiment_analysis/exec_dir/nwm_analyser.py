
## ================================================ ##
##  Complete sentiment analysis on all input files  ##
## ================================================ ##

## As this file is to be run in the folder with all the material, paths are short or can be omitted
## this means this file must create a new Python process (inferior buffer) in its own right
## Source of help: http://stackoverflow.com/questions/33715277/recreate-input-folder-tree-for-output-of-some-analyses/33715658#33715658
from subprocess import check_output, call
#from sys import platform as operating_system
import shutil
import csv
import fnmatch
import itertools
import os
#import time


## ======================================= ##
##  Define paths depending on system used  ##
## ======================================= ##

## Create the command to send to terminal
#if operating_system == "darwin":
#src_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/clean/"
#dst_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/output/"

# elif operating_system == "win32":
#      ## The actual command line inputs used below must be adapted for a windows console ##
#      src_dir = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\clean\\"
#      dst_dir = "C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\output\\"

## ================================== ##
##  Define a function for each model  ##
## ================================== ##

## Emolex
def run_emolex(input_file, output_dir, output_file):

    ## create and run the shell command
    cmd = "./emolex.pl -f " + "\'" + input_file + "\'" + " > tmp0_emolex.txt"
    check_output(cmd, shell=True)

    with open("tmp0_emolex.txt", "rb") as to_read:
        #ofile = os.path.join(output_dir, output_file)
        with open("tmp_emolex.txt", "wb") as to_write:
            reader = csv.reader(to_read, delimiter = "\t")
            writer = csv.writer(to_write, delimiter = "\t")
        
            ## add headers for new columm
            writer.writerow(["Emolex_raw", "Emolex_rounded"])
            ## write the results, ?? skipping an empty header line specific to emolex ??
            for line in reader:
                writer.writerow(line)

## Sentiment140
def run_sentiment140(input_file, output_dir, output_file):

    ## create and run the shell command
    cmd = "java -jar sentiment140.jar -f " + "\'" + input_file + "\'" + " > tmp0_sentiment140.txt"
    check_output(cmd, shell=True)

    with open("tmp0_sentiment140.txt", "rb") as to_read:
        #ofile = os.path.join(output_dir, output_file)
        with open("tmp_sentiment140.txt", "wb") as to_write:
            reader = csv.reader(to_read, delimiter = "\t")
            writer = csv.writer(to_write, delimiter = "\t")

            ## add header for new columm
            writer.writerow(["Sentiment140"])
            ## write the results        
            for line in reader:
                writer.writerow(line)

## Vader-Afinn
def run_vader_afinn(input_file, output_dir, output_file):

    ## create and run the shell command
    cmd = "python v_afinn.py -f " + "\'" + input_file + "\'" + " > tmp0_vader_afinn.txt"
    check_output(cmd, shell=True)

    with open("tmp0_vader_afinn.txt", "rb") as to_read:
        with open ("tmp_vader_afinn.txt", "wb") as to_write:
            reader = csv.reader(to_read, delimiter = "\t")
            writer = csv.writer(to_write, delimiter = "\t")

            ## add header for new column
            writer.writerow(["Vader_Afinn"])
            ## write the results        
            for line in reader:
                writer.writerow(line)

## Vader
def run_vader(input_file, output_dir, output_file):

    ## create and run the shell command
    cmd = "python vader.py -f " + "\'" + input_file + "\'" + " > tmp0_vader.txt"
    check_output(cmd, shell=True)

    with open("tmp0_vader.txt", "rb") as to_read:
        with open("tmp_vader.txt", "wb") as to_write:
            reader = csv.reader(to_read, delimiter = "\t")
            writer = csv.writer(to_write, delimiter = "\t")

            ## add headers for new columm
            writer.writerow(["Vader_decimal", "Vader_rounded"])
            ## write the results        
            for line in reader:
                writer.writerow(line)

## SentiStrength
def run_sentistrength(input_file, output_dir, output_file):

    ## create and run the shell command
    cmd = "java -jar SentiStrength.jar sentidata data/ input " + "\'" + input_file + "\'" + " > trial.txt"
    check_output(cmd, shell=True)

     ## ======================================================================================= ##
     ##  The .jar file in SentiStrength creates its output in whichever folder it finds itself
     ##  that file must therefore be copied to output directory to be joined to other models
     ## ======================================================================================= ##
      
    #shutil.copy(input_file[0:len(input_file)-4] + "0_out.txt", os.path.join(input ,input_file[0:len(input_file)-4] + "0_out.txt"))
    #time.sleep(2)
    
    
    ## copy model output 
    #shutil.copy(input_file[0:len(input_file)-4] + "0_out.txt", "tmp0_sentistrength.txt")
    os.rename(input_file[0:len(input_file)-4] + "0_out.txt", "tmp_sentistrength.txt")
    #shutil.copy2("tmp_sentistrength.txt", "tmp_results.txt")
    ## rearrange columns for using later to hold the results of all models
    with open("tmp_sentistrength.txt", "rb") as tmp_sentistrength:
        with open("outfile.txt", "wb") as outfile: # use outfile (not tmp_sentistrength) as file is base for output 
            reader = csv.reader(tmp_sentistrength, delimiter = "\t")
            writer = csv.writer(outfile, delimiter = "\t")

            ## skip the old headers and create new ones
            reader.next()
            writer.writerow(["Tweet-Text", "Sentistrength_pos", "Sentistrength_neg"])
            ## write the results  
            for row in reader:  # reorder the columns, ready for main output
                writer.writerow([row[2], row[0], row[1]])

    

## Define a function that analyses a file for each of the models
def analyser(input_file, output_dir, output_file):

    run_emolex(input_file, output_dir, output_file)
    run_sentiment140(input_file, output_dir, output_file)
    run_vader_afinn(input_file, output_dir, output_file)
    run_vader(input_file, output_dir, output_file)
    run_sentistrength(input_file, output_dir, output_file)

    #output_file = os.path.splitext(input_file)[0] + '_output.txt'
    
    ## use the shell command dos2unix to remove all text markers like ^M for new lines etc.
    ## this solution was found here: http://stackoverflow.com/questions/33725958/how-to-use-the-unix-shell-paste-command-for-several-files/33726730#33726730
    # cmd1 = "dos2unix outfile.txt tmp_emolex.txt tmp_sentiment140.txt tmp_vader_afinn.txt tmp_vader.txt"
    cmd1 = "dos2unix tmp_emolex.txt tmp_sentiment140.txt tmp_vader_afinn.txt tmp_vader.txt outfile.txt"
    check_output(cmd1, shell=True)

    ## join all results together into one outfile, via unix shell command 'paste' with default delimiter "\t"
    # cmd2 = "paste outfile.txt tmp_emolex.txt tmp_sentiment140.txt tmp_vader_afinn.txt tmp_vader.txt > " + "\'" + output_file + "\'"
    cmd2 = "paste outfile.txt tmp_emolex.txt tmp_sentiment140.txt tmp_vader_afinn.txt tmp_vader.txt > " + "\'" + output_file + "\'"
    call(cmd2, shell=True)

## Define a function that will work through a tree of folders, applying the analyser function to all .txt files withih the tree
## A new tree is created in the destination folder that replicates the structure of the input tree, containing one output file for each input file
def tree_analyser(src_dir, dst_dir):
    ## Work through all .txt files in folder tree, creating same tree for the SA output
    ignore_dir = lambda d: d in ('.git', '.svn', '.hg', '.DS_Store')
    src_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/clean/"
    dst_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/output/"
    for root, dirs, files in os.walk(src_dir):
        for input_file in fnmatch.filter(files, "*.txt"): # for each input file
            output_file = os.path.splitext(input_file)[0] + '_output.txt'  
            output_dir = os.path.join(dst_dir, root[len(src_dir):])

            if not os.path.isdir(output_dir):
                os.makedirs(output_dir) # create destination directories 
            analyser(os.path.join(root, input_file),  # perform analysis
                     output_dir,
                     os.path.join(output_dir, output_file))

        # don't visit ignored subtrees
        dirs[:] = itertools.ifilterfalse(ignore_dir, dirs)

if __name__ == "__main__":

    src_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/clean/"
    dst_dir = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/output/"
    
    tree_analyser(src_dir, dst_dir)
