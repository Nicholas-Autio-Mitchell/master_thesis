
## ===================================================================== ##
##  A function that generates files that match a given filename pattern  ##
## ===================================================================== ##
## source: http://www.dabeaz.com/generators-uk/genfind.py
## It works recursively from top to bottom

## Original use case to move the output files created by the SentiStrength model

import os
import shutil
import fnmatch
from sys import platform as operating_system

def gen_find(file_pattern, top):
    for path, dirlist, filelist in os.walk(top):
        for name in fnmatch.filter(filelist, file_pattern):
            yield os.path.join(path, name)


if operating_system == 'darwin':

    source = '/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input/clean/ttt/' # input
    destination = '/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/output/ttt/' # desired location

elif operating_system == 'win32':

    source = 'C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\input\\clean\\' # input
    destination = 'C:\\Users\\dev231\\Documents\\Thesis\\Data\\sentiment_analysis\\output\\' # desired location

filesToMove = gen_find('*_out.txt', source)

for name in filesToMove:
    shutil.move(name, destination)
            
# Example use

if __name__ == '__main__':

    filesToMove = gen_find('*_out.txt', source)

    for name in filesToMove:
        shutil.move(name, destination)
