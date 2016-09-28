import csv


input_file = '2013-08-11_til_2013-07-27_clean_text.txt'
tmp = input_file[0:len(input_file)-4] + "0_out.txt"

# with open("tmp_output.txt", "wb") as output:
reader = csv.reader(open(tmp, "rb"), delimiter = "\t")
writer = csv.writer(open("outfile.txt", "wb"), delimiter = "\t")
  
for row in reader:
    #print row
    writer.writerow([row[2], row[0], row[1]])
