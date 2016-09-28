<<<<<<< HEAD
rm(curr_file)

curr_file  <- read.csv("tester.txt", header = TRUE, sep = "\n", quote = "", row.names = NULL, stringsAsFactors = FALSE)


## Hex codes: http://www.asciitable.com/

clean_file <-  sapply(curr_file, function(x) {
    gsub("&amp;", "&", x) %>%
        gsub("[^\\x{20}-\\x{7F}]", "", ., perl = TRUE) %>%   ## Removes non-printing characters
        gsub("\\x{22}*", "", ., perl = TRUE) %>%  # "
        gsub("\\x{23}*", "", ., perl = TRUE) %>%  # #
        gsub("\\x{24}*", "", ., perl = TRUE) %>%  # $
        gsub("\\x{25}*", "", ., perl = TRUE) %>%  # %
        gsub("\\.{2,}", " ", ., perl = TRUE) %>%  # two or more .
        gsub("\\x{2A}*", "", ., perl = TRUE) %>%  # *
        gsub("\\x{5B}*", "", ., perl = TRUE) %>%  # [
        gsub("\\x{5D}*", "", ., perl = TRUE) %>%  # ]
        gsub("\\x{5F}*", "", ., perl = TRUE) %>%  # _
        gsub("\\x{60}*", "", ., perl = TRUE) %>%  # `
        gsub("[\\x{7B}-\\x{7F}]", "", ., perl = TRUE) %>%  # 5 characters: { | } ~ DEL
        #gsub("\"", "", ., perl=TRUE) %>%
        gsub("^\\x{27}", "", ., perl = TRUE) %>%
        gsub("http\\S+\\s*", "", .) %>%
        #gsub("[^[:alnum:][:space:]\\'-]", "", .) %>%
        stripWhitespace() %>%
        gsub("^ ", "", .) %>%
        gsub(" $", "", .)      
})
#-\\x{7b}
write.table(x = clean_file, file = "t.txt", sep = "\t", fileEncoding = "UTF-8", quote = FALSE, row.names = FALSE, col.names = FALSE)
=======
text
ExchangeNews Direct: S&P Dow Jones Indices Announces Changes To The S&P/TSX Canadian Indices - A Deletion From... http://bit.ly/1dVd0Qk 
Svelte Medical Systems Raises $22M for Heart-Surgery Products http://bit.ly/16qaCMK 
Dow Jones industrial average tumbles below 15000 on budget standoff $DJI http://bit.ly/16lSnbz 
Dow approaches record high as Fed meeting begins -  NEW YORK Investors pushed stocks higher... http://j.mp/1acouHw   #DowJones #europe
[http://Omaha.com  Money] How the Dow Jones industrial average did Thursday http://dlvr.it/4F6BL2 
Just another day at #dowjones brewing up exciting #DJX answers for our customers! @djsupport @Q5446 pic.twitter.com/ldjrogrdCO
ExchangeNews Direct: 2014 Weights For The Dow Jones-UBS Commodity Index Announced By S&P Dow Jones Indices And... http://bit.ly/1aMDEYA 
"DOW off -73pts, SPI FUTURES up +3pts, Macquarie result (roughly inline), More on NAB, Market PE, Around the grounds…. http://mymarketview.com.au/friday-1st-november-2013-dow-jones-off-73pts-spi-futures-up-3pts-macquarie-result-roughly-inline-more-on-nab-market-pe-around-the-grounds/ …"
"Dow, S&amp;P 500 Finish at Record Highs - The Dow Jones Industrial Average finished at a new high... http://j.mp/16jWTqG   #VolumeDelayed"
NovaQuest Capital Management Raises $459M for Structured Financings in Biotech http://bit.ly/1hxLwQz 
"TABLE-DJ-UBS commodity index cuts natgas, WTI; hikes Brent for 2014: [Reuters] - The Dow Jones-UBS Commodity I... http://yhoo.it/16PMXYp "
Stock market sets record after Fed keeps stimulus -  NEW YORK The stock market hit a record... http://j.mp/1gAj3X1   #DowJones #WellsFargo
"Wall Street fecha em queda: Dow Jones -0,47%, Nasdaq, -0,28%: Wall Street fechou em queda nesta quinta-feira, ... http://bit.ly/1crwGat "
"@tweetacoffee to @ang_ferrante Shout-out to 1 my many favorite #DowJones #customerservice superstars, have a work appropriate drink on me!"
Good news: Venture capital returns improve; Bad news: Still don’t beat public markets - http://bit.ly/16pZUpq 
Time Warner Cable Revenue Rises http://ow.ly/qmuAL 
"En el mes: Dow Jones +2.75%, Nasdaq +3.92%, S&P +4.46%"
"Wall Street fecha em queda: Dow Jones -0,47%, Nasdaq, -0,28%: NOVA YORK, 31 Out 2013 (AFP) - Wall Street fecho... http://bit.ly/1aMuyuI "
"Wall Street fecha em queda: Dow Jones -0,47%, Nasdaq, -0,28%: NOVA YORK, 31 Out 2013 (AFP) - Wall Street fecho... http://bit.ly/1crqIX7 "
S&P Dow Jones Indices: Greece no longer classifies as developed market  Greek market fluctuates http://english.capital.gr/News.asp?id=1899400 … via @capitalgr
"Stock Market Futures 11/1/13 Dow Jones, Nasdaq, S & P Futures http://goo.gl/fb/4GcLc "
Delectable #halloween treats at the @DowJones Princeton cafeteria! pic.twitter.com/GBKbxuFgLO
S&P Dow Jones Indices Announces Changes to the S&P/TSX Canadian Indices: A Deletion from the S&P/TSX Preferred... http://bit.ly/1aJGmfB 
"The Dow Jones Industrial Average DJI - 15,545.75 spent time on both  sides of breakeven today, ended up finishing 73 points, or 0.5%, lower"
Best @dowjones Halloween costume ever right here. #djx pic.twitter.com/mOQaViVpUF
U.S. stocks open lower as Fed statement weighs; Dow Jones down 0.12% http://ift.tt/16PBR5G  #sfx #fx #nasdaq
Dow Jones Industrial Average: Why A 10\% Pullback Isn’t A Sure Thing http://dlvr.it/4F50SS 
How the Dow Jones industrial average did Thursday: [AP] - The Dow Jones industrial average and the Standard & ... http://yhoo.it/1dv2AnQ 
"Dow Jones ESL – Make It Jump (Left Cheek, Right Cheek) http://fb.me/2tv3YPHEr "
How the Dow Jones Industrial Average Did Thursday http://abcn.ws/1aJyKJU  *NOT #Anonymous News
How the Dow Jones Industrial Average Did Thursday: How the Dow Jones industrial average and other major indexe... http://abcn.ws/1aJyKcX 
How the Dow Jones Industrial Average Did Thursday http://abcn.ws/1aJyKcX 
"Dow Jones ESL – Make It Jump (Left Cheek, Right Cheek) http://fb.me/W32q8KGr "
"The #US #stock #markets were down today. S&P 500▼, #Dow Jones Industrials▼, and #Nasdaq▼."
Consumer Financial-Services Startup 91Jinrong Raises $10M Second Round http://bit.ly/HuPPPi 
Centri Raises $13M Series C to Help Mobile Networks Run More Efficiently http://bit.ly/1bH0S0s 
"Product Technologist-Software Engineer - Dow Jones -  New York, NY: Help revolutionize digital media. Are you ... http://bit.ly/1aMjtKj "
Dow Jones Industrials component charts and news  ►►► http://sharptraders.com/dow30.html 
U.S. #VCFirms Top the Charts as Europe Disappoints Again: http://hub.am/17oUsWO  #venturecapital
^DJI: Dow jones pic.twitter.com/6biMVihQf4
"Weak finish on Wall Street with Dow Jones ending -0,5% weighed by Visa. This was the 1st two day losing streak in over three weeks."
"Rentabilidad mes de octubre: Dow Jones: +2,8%; S&P 500: +4,5%; Nasdaq: +3,9%"
Hangin with the festive @DowJones HR crew! @a_mccreary @KristinFinnegan @mruffdogg @charlenekeelan #halloween pic.twitter.com/Pj0bcXOgpR
"Dow Jones Industrial Average: Why A 10% Pullback Isn’t A Sure Thing: INDEXDJX:.DJI, INDEX... http://bit.ly/1cr3LTX  #mkt #etf #gold #etn"
Greece downgraded to emerging market status by S&P Dow Jones http://zite.to/17z2WbX 
Dow Jones Industrial Average Fast Facts http://dlvr.it/4F45ND 
"U.S. stocks end lower Thursday; in October, S&P 500 rises 4.5% and Dow Jones Industrial Average gains 2.8%."
"Dow, S&amp;P 500 Finish at Record Highs - The Dow Jones Industrial Average finished at a new high... http://j.mp/16jWTqG   #VolumeDelayed"
It has been a wild ride in $FB today. I had the headline article on Dow Jones Marketwatch today. http://stks.co/rZi5 
NZX-50 Closes up 0.9% Led by Fletcher Building: 0424 GMT [Dow Jones] The NZX-50 closes up 0.9% at 4909.726 led... http://binged.it/16Piiul 
"""Dow Jones - A Solfège Economy"" exhibition by Gareth Walsh in Art Sci Gallery at @CNSI_UCLA 5pm tonight! http://ow.ly/qf0z6 "
"Happy Halloween from Dow Jones in Princeton, NJ!     https://vine.co/v/hD2l7qrU6gZ "
"Welcome to Twitter, @gerardtbaker, the @WSJ's managing editor (and editor-in-chief of @DowJones)!"
RT @dow jones : @amaurialberto11 INDU Quote - Dow Jones Industrial Average Index - Bloomberg: http://bloom.bg/13xcUGr 
#unemployment \ What Unemployment Figures Can Tell You About the Dow's Future: The Dow Jones Industrial Averag... http://uci.im/1cqP2sk 
Dow Jones industrial average closes at record high -  THE ASSOCIATED... http://j.mp/1hre4v8   #DowJones #FederalReserve #UpdatedOctober
Dow Jones - Trading Update! http://www.john-gossen.de/dow-jones-trading-update/ … via @John Gossen
#Stock Market Investors::How to Make Money by Selling Call Options on the Dow Jones 30.  See:: http://www.corkin.com/listings/viewlisting.cfm?t=How-to-Make-Money-by-Selling-Call-Options&listingid=192405#.UnADnZn3f2X.twitter … …
"#S&P Dow Jones to demote #Greece, promote 2 to emerging markets status http://goo.gl/NV6ATU "
"#stocknews Dow Jones Industrial Average (INDEXDJX:.DJI), S&P 500 ... http://dlvr.it/4F34g1  #investments"
Buy - Cisco Systems - CSCO - U.S Stock - Dow Jones 30 - SUCCESSFUL on 16 May 2013 (COMPLETED) http://is.gd/ng9GHb 
"Dow, S&amp;P 500 Finish at Record Highs - The Dow Jones Industrial Average finished at a new high... http://j.mp/16jWTqG   #VolumeDelayed"
"Consumer lending grows at fastest rate since 2008, as Bank of England admits error - live http://bit.ly/16P0ijG  RT @guardiannews UK"
S&P Dow Jones Indices to promote both #Qatar and #UAE to emerging markets from frontier markets http://goo.gl/NV6ATU  #EM #asset_mgmt
S&P Dow Jones Indices will demote #Greece to the status of emerging market from developed market http://goo.gl/NV6ATU  #EM #asset_mgmt
"An Interpretation Of Pop Music - DJ Dow Jones, Sam Hoody, 4 Tr3 DJs http://fb.me/2XTpPQlOj "
"Dow Jones, -0,14%; S&P 500, -0,05%; Nasdaq, -0,17%"
Da A$tronautz - Grand Theft Audio 2 Hosted By Dj Dow Jones - download and stream | AudioMack http://fb.me/2H8WIhG0d 
Just Rich Gates x Trap America x 808 Maifa x Dj Dow Jones preview pt1: http://youtu.be/A6ZVhhPY6R0  via @youtube
"S&P Dow Jones to demote Greece, promote 2 to emerging markets status http://ow.ly/2Bdgut "
Today's Dow Jones Industrial Average DJIA Nasdaq S&P 500 stock ...: Stocks were positioned for the weaker star... http://bit.ly/1dTL3bu 
U.S. futures edge lower after FOMC remarks; Dow Jones down 0.06% http://sttsp.pl/21bmr  #IraqiDinaratTampaDinar
"PLEASE checkOUT my NEW VIDEO for my NEW HIT SINGLE ""MAKE IT JUMP"" [LEFT CHEEK, RIGHT CHEEK]!!...by DOW JONES ESL... http://fb.me/2SWQ4mVAJ "
"Dow Jones ESL – Make It Jump (Left Cheek, Right Cheek) @IAMDOWJONESESL  http://wihiphop.com/id/4095  via @WIhiphop"
Exclusive: Investors of TH Lee Putnam's $1.1B fund weigh offer to sell interests to Oak Hill spinout http://bit.ly/17ZGxRC  #privateequity
#Halloween at @DowJones Princeton with @lindsay_ferrone and mommy Barbara! pic.twitter.com/VoU93ebhSD
3 Commodity ETFs Surging Higher (iPath Dow Jones-UBS Cocoa Subindex Total Return ETN) ( NIB )  http://lnkd.in/bqVjNKd 
"PLEASE checkOUT my NEW SONG called ""IDEA""!!...by DOW JONES ESL EVERYONE...PLEASE like, SHARE, or COMMENT!!!... http://fb.me/2MswgKAQy "
WSJ Managing Editor @gerardtbaker talks digital transformation & Miley Cyrus in our monthly newsletter http://tinyurl.com/nt6ag6a 
Possible triple top in Dow Jones: Dukas - http://bit.ly/HrIWhq 
"Futures rise again on strong corporate earns -  THE ASSOCIATED PRESS October 25, 2013... http://j.mp/1chhSzq   #DowJones #ProcterGamble"
$GROW U.S. stocks open lower as Fed statement weighs; Dow Jones down 0.12% http://top10stocks2buy.com/i.php?http://www.nasdaq.com/article/us-stocks-open-lower-as-fed-statement-weighs-dow-jones-down-012-cm294447 …
Daily FX Initiative: EURUSD Searches for Support Amid Growing Threat for Deflation: The Dow Jones-FXCM U.S. Do...  http://bit.ly/4fxcpe 
How the Dow Jones industrial average and other major stock indexes fared on ... $DJI http://bit.ly/18neqz3 
New post: Midday Market Stats: Dow Jones Industrial Average Fighting Back from Earlier Losses http://bit.ly/18GgeiE 
$TRV TRAVELERS COMPANIES INC : Travelers Named to 2013 Dow Jones ... http://www.stocknomics.co/u/MTkyNjQz 
RThx > @futurescoach re: Leading DJ Index >>> Dow Jones US Consumer Electronics Index ($DJUSCE)
How the Dow Jones Industrial Average Did Wednesday $DJI http://bit.ly/1ag47cL 
"Dow Jones Industrial Average, S&P 500: Still More Room To Expand $DJI http://bit.ly/1ag47cJ "
"[Financial Markets] S&P Dow Jones to upgrade Qatar, UAE to emerging market status: International equity index ... http://bit.ly/1cq3ot5 "
>>>>>>> ffe66895523ed7f67d8760544390319fd4deda55
