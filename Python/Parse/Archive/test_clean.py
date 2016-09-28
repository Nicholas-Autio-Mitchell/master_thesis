import csv

def removeNonAscii(s): return "".join(letter for letter in row if ord(i)<128)

with open("tester.txt", "rU+") as fi:
    with open("cleaned_tester.txt", "wb") as fo:
        reader = csv.reader(fi)
        writer = csv.writer(fo)

#        for row in reader:
        line = fi.readline()
        lineout = removeNonAscii(line)
        print lineout

            
           # output = list(row.replace(u"â", "") for row in reader)
##            output = list(row.encode("ascii", "ignore"))
        #writer.writerow(output)

# def removeNonAscii(s): return "".join(i for i in s if ord(i)<128)

# with open("tester.txt", "r") as fi:
#     with open("cleaned_tester.txt", "wb") as fo:
#         reader = csv.reader(fi)
#         writer = csv.writer(fo)
        
#         for row in fi:
            
# with open("tester.txt") as ff:
#     for row in ff:
#         for i in row:
#             if ord(i)>127:
#                 i = ""
#             else: output1 = "".join(i)
                
#         output2 = "".join(output1)
#     output3 = "".join(output2)
#     print output3

