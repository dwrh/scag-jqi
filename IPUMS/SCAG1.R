#### IPUMS FOR JQI ####
# KKANE, 10/24/23
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

# if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(doBy)
library(ipumsr)

setwd("~/Documents/GitHub/BEAR/scag-jqi/IPUMS")
#setwd("P:/kkane/econ/iers_jobquality/acs_2021_test_KK")

library( data.table )

###ddi <- read_ipums_ddi("usa_00002.xml")
###data <- read_ipums_micro(ddi)
###head(data)


#read in as single lines
#DT <- fread( "usa_00002.dat", sep = "" )
#head(DT)
#read each row as xml, store in list
#L <- apply( DT, 1, xml2::read_xml )
# process further

ddi = read_ipums_ddi("usa_00005.xml")
d = read_ipums_micro(ddi)

# Restrict to those in the labor force
d = subset(d, LABFORCE==2)

# Restrict to those working in the SCAG region
d = subset(d, PWSTATE2==6)
d = subset(d, PWCOUNTY==25 | PWCOUNTY==37 | PWCOUNTY==59 | PWCOUNTY==65 | PWCOUNTY==71 | PWCOUNTY==111)

# Create 2-digit NAICS
d$naics2 = as.numeric(as.character(substr(d$INDNAICS,1,2)))
table(d$naics2)


#### COLLAPSE TO COUNTY AND INDUSTRY-LEVEL VARS ####

# Any Health insurance 
u = summaryBy(PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
u$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
hains = summaryBy(PERWT ~ naics2 + PWCOUNTY, data=subset(d, HCOVANY==2), FUN=sum)
hains$cntyind = paste(hains$PWCOUNTY, hains$naics2, sep="")
u$ainsemp = hains$PERWT.sum[match(u$cntyind, hains$cntyind)]
u$p_ainsemp = u$ainsemp/u$PERWT.sum

# Employer Health insurance
# u = summaryBy(PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
# u$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
heins = summaryBy(PERWT ~ naics2 + PWCOUNTY, data=subset(d, HINSEMP==2), FUN=sum)
heins$cntyind = paste(heins$PWCOUNTY, heins$naics2, sep="")
u$einsemp = heins$PERWT.sum[match(u$cntyind, heins$cntyind)]
u$p_einsemp = u$einsemp/u$PERWT.sum

# Commute time
d$transum = d$TRANTIME*d$PERWT
commute = summaryBy(transum + PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
commute$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
commute$trantime = commute$transum.sum/commute$PERWT.sum
u$trantime = commute$trantime[match(u$cntyind, commute$cntyind)]

# Job Status - Nam-Powers-Boyd
d$jstatNPB = d$NPBOSS90*d$PERWT
jstat = summaryBy(jstatNPB + PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
jstat$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
jstat$NPBOSS90 = jstat$jstatNPB.sum/jstat$PERWT.sum
u$NPBOSS90 = jstat$NPBOSS90[match(u$cntyind, commute$cntyind)]

# Job Status - Nakano-Treas
d$jstatNT = d$PRENT*d$PERWT
jstat = summaryBy(jstatNT + PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
jstat$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
jstat$PRENT = jstat$jstatNT.sum/jstat$PERWT.sum
u$PRENT = jstat$PRENT[match(u$cntyind, commute$cntyind)]

# Job Status - Seigal
d$jstatS = d$PRESGL*d$PERWT
jstat = summaryBy(jstatS + PERWT ~ naics2 + PWCOUNTY, data=d, FUN=sum)
jstat$cntyind = paste(u$PWCOUNTY, u$naics2, sep="")
jstat$PRESGL = jstat$jstatS.sum/jstat$PERWT.sum
u$PRESGL = jstat$PRESGL[match(u$cntyind, commute$cntyind)]

# Share of household income spent on gross rent or other owner costs
# use household weights.  this is a shortcoming, but the interpretation is "a household with a person working in this industry spends XXX"
own = subset(d, OWNERSHP==1)
own$pct_burd = own$OWNCOST*12/own$HHINCOME
own$pct_burd[own$pct_burd>1] <- NA
rent = subset(d, OWNERSHP==2)
rent$pct_burd = rent$RENTGRS*12/rent$HHINCOME
rent$pct_burd[rent$pct_burd>1] <- NA
d2 = rbind(own, rent)
d2 = d2[!is.na(d2$pct_burd),]

d2$burdsum = d2$pct_burd*d2$HHWT
burd = summaryBy(pct_burd + HHWT ~ naics2 + PWCOUNTY, data=d2, FUN=sum, na.rm=T)
burd$cntyind = paste(burd$PWCOUNTY, burd$naics2, sep="")
burd$p_burd = burd$pct_burd.sum/burd$HHWT.sum*100
u$p_burd = burd$p_burd[match(u$cntyind, burd$cntyind)]

write.csv(u, "acsipums21_for_jqi_102423.csv")

