## Script to retrieve, clean and create some form of output for general terms

## A list displaying the possible queries as strings into the searchTwitter function (for multiple search terms)
"#Obamacare" 	                will find tweets containing the hashtag "Obamacare"
"@BarackObama" 	              will find tweets referencing Barack Obama's account
"ACA until:2014-08-22" 	      will find tweets containing "ACA" and sent before 2010-08-25
"from:BarackObama" 	          will find tweets sent from Barack Obama
"Obamacare -ACA" 	            will find tweets containing "Obamacare" but not "ACA
"Obamacare ACA" 	            will find tweets containing both "Obamacare" and "ACA"; not case sensitive
"Obamacare ACA" 	            will find tweets containing the exact phrase "Obamacare ACA"; not case sensitive
"Obamacare OR ACA" 	          will find tweets containing either "Obamacare" or "ACA" or both; not case sensitive; the OR operator IS case sensitive.
"Obamacare since:2014-08-25" 	will find tweets containing "Obamacare" and sent since 2010-08-25 (year-month-day)
"to:BarackObama"             	will find tweets sent to Barack Obama