# Cricket Data Analysis

## System requirements
* Python (3.4+)
  * BeautifulSoup 4
  * Pandas
* R (3.x+)
  * tidyverse
  * stringr
  * zoo
 
 This is a collection of scripts for scraping and analysing cricket data. The data is scraped from statsguru enging on [espncricinfo](www.espncricinfo.com).  
 Python scripts in the 'scraper' folder scrape data for different batsmen and bowlers and write them to the 'data' folder. Data formatting and statistical analysis is done by the R scripts.  
 
 ## Example script - mahela_analysis.R
 
 Mahela Jayawardena is perhaps the second or third greatest batsman to come out of Sri Lanka. Known especially for his batting in Test Cricket, he's one of the few players to amass 10,000 runs in both formats of the game. This script analysis his career in terms of 'Runs per Innings' (RPI) and 'Batting average' (BA). While I believe RPI to be a better judge of a batsman's average run output, there are some who lean heavily towards BA in that regard. Both statistics tell the same story: Mahela's career peaked as a batsman around 2010, hitting a BA of 55. However as with any cricketer, age caught up to him and declined gracefully to a BA of 49.84.