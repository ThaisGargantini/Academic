# Bases:https://drive.google.com/drive/u/0/folders/1nIjJe5p7qfXhICBBAFDM_pMRDhhJd7Jx

#########################################
###  NFT MA Thesis ###
#########################################
rm(list = ls())
install.packages("dplyr");library(dplyr)
install.packages("stringr");library(stringr)
install.packages("xlsx"); library("xlsx")
install.packages("readr");library("readr")
install.packages("GGally"); library("GGally")
install.packages("tidyverse"); library("tidyverse")
install.packages("readxl"); library("readxl")
if(!require("devtools"))install.packages("devtools");library("devtools")
install_github("clbustos/dominanceanalysis")
if(!require("dominanceanalysis")) install.packages("dominanceanalysis");library("dominanceanalysis")
if(!require("car"))install.packages("car");library("car")
if(!require("plyr"))install.packages("plyr");library("plyr")
if(!require("tidyr"))install.packages("tidyr");library("tidyr")
if(!require("naniar"))install.packages("naniar");library("naniar")
if(!require("magrittr"))install.packages("magrittr");library("magrittr")
if(!require("pastecs"))install.packages("pastecs");library("pastecs")
if(!require("lubridate"))install.packages("lubridate");library("lubridate")
if(!require("factoextra")) install.packages("factoextra");library("factoextra")
if(!require("mice")) install.packages("mice");library("mice")
if(!require("psych")) install.packages("psych");library("psych")
if(!require("GPArotation")) install.packages("GPArotation");library("GPArotation")
if(!require("naniar")) install.packages("naniar");library("naniar")
if(!require("pivottabler")) install.packages("pivottabler");library("pivottabler")
if(!require("rstatix")) install.packages("rstatix"); library("rstatix")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require(RVAideMemoire)) install.packages("RVAideMemoire"); library(RVAideMemoire)
install.packages("epiDisplay"); library(epiDisplay)
install.packages("benford"); library("benford")
install.packages("finalfit"); library("finalfit")
install.packages("matrixStats"); library("matrixStats")
install.packages("rje"); library("rje")

# Importing Data
## without the second line (the description os each variable).
nft <- read_xlsx("/Users/thaisgargantini/Documents/Academic/Academic/NFT_December+23,+2022_11.56.xlsx")
#glimpse(nft)
nftp <- nft[nft$StartDate>"2022-12-20", ]
#glimpse(nftp)
nftpi <- nft[nft$Status== "IP Address",]
#View(nftpi)
nftpi <- nftpi[complete.cases(nftpi[,"LikeNFT_1"]),]

# Creating a column with the name of the conditions
attach(nftpi)
nftpi$Condition[TheKiss_E1_ControlV>=0] <- "e1_c"
nftpi$Condition[TheKiss_E1_50V>=0] <- "e1_50"
nftpi$Condition[TheKiss_E1_90V>=0] <- "e1_90"
nftpi$Condition[TheKiss_E2_ControlV>=0] <- "e2_c"
nftpi$Condition[TheKiss_E2_50V>=0] <- "e2_50"
nftpi$Condition[TheKiss_E2_90V>=0] <- "e2_90"
nftpi$Condition[TheKiss_E3_ControlV>=0] <- "e3_c"
nftpi$Condition[TheKiss_E3_50V>=0] <- "e3_50"
nftpi$Condition[TheKiss_E3_90V>=0] <- "e3_90"
nftpi$Condition[TheKiss_E4_ControlV>=0] <- "e4_c"
nftpi$Condition[TheKiss_E4_50V>=0] <- "e4_50"
nftpi$Condition[TheKiss_E4_90V>=0] <- "e4_90"
table(nftpi$Condition)

# Creating a column with the buy X sell
attach(nftpi)
nftpi$BuySell[TheKiss_E1_ControlV>=0] <- "buy"
nftpi$BuySell[TheKiss_E1_50V>=0] <- "buy"
nftpi$BuySell[TheKiss_E1_90V>=0] <- "buy"
nftpi$BuySell[TheKiss_E2_ControlV>=0] <- "sell"
nftpi$BuySell[TheKiss_E2_50V>=0] <- "sell"
nftpi$BuySell[TheKiss_E2_90V>=0] <- "sell"
nftpi$BuySell[TheKiss_E3_ControlV>=0] <- "buy"
nftpi$BuySell[TheKiss_E3_50V>=0] <- "buy"
nftpi$BuySell[TheKiss_E3_90V>=0] <- "buy"
nftpi$BuySell[TheKiss_E4_ControlV>=0] <- "buy"
nftpi$BuySell[TheKiss_E4_50V>=0] <- "buy"
nftpi$BuySell[TheKiss_E4_90V>=0] <- "buy"
table(nftpi$BuySell)

# Creating a column with the artist X anonymous user
attach(nftpi)
nftpi$ArtistAnonymousUser[TheKiss_E1_ControlV>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E1_50V>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E1_90V>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E2_ControlV>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E2_50V>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E2_90V>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E3_ControlV>=0] <- "anonymous user"
nftpi$ArtistAnonymousUser[TheKiss_E3_50V>=0] <- "anonymous user"
nftpi$ArtistAnonymousUser[TheKiss_E3_90V>=0] <- "anonymous user"
nftpi$ArtistAnonymousUser[TheKiss_E4_ControlV>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E4_50V>=0] <- "artist"
nftpi$ArtistAnonymousUser[TheKiss_E4_90V>=0] <- "artist"
table(nftpi$ArtistAnonymousUser)

# Creating a column with the buy artist X anonymous user
attach(nftpi)
nftpi$BuyArtistAnonymousUser[TheKiss_E1_ControlV>=0] <- "artist"
nftpi$BuyArtistAnonymousUser[TheKiss_E1_50V>=0] <- "artist"
nftpi$BuyArtistAnonymousUser[TheKiss_E1_90V>=0] <- "artist"
nftpi$BuyArtistAnonymousUser[TheKiss_E3_ControlV>=0] <- "anonymous user"
nftpi$BuyArtistAnonymousUser[TheKiss_E3_50V>=0] <- "anonymous user"
nftpi$BuyArtistAnonymousUser[TheKiss_E3_90V>=0] <- "anonymous user"
nftpi$BuyArtistAnonymousUser[TheKiss_E4_ControlV>=0] <- "artist"
nftpi$BuyArtistAnonymousUser[TheKiss_E4_50V>=0] <- "artist"
nftpi$BuyArtistAnonymousUser[TheKiss_E4_90V>=0] <- "artist"
table(nftpi$BuyArtistAnonymousUser)

# Creating a column with experiments
attach(nftpi)
nftpi$Experiment[TheKiss_E1_ControlV>=0] <- "e1"
nftpi$Experiment[TheKiss_E1_50V>=0] <- "e1"
nftpi$Experiment[TheKiss_E1_90V>=0] <- "e1"
nftpi$Experiment[TheKiss_E2_ControlV>=0] <- "e2"
nftpi$Experiment[TheKiss_E2_50V>=0] <- "e2"
nftpi$Experiment[TheKiss_E2_90V>=0] <- "e2"
nftpi$Experiment[TheKiss_E3_ControlV>=0] <- "e3"
nftpi$Experiment[TheKiss_E3_50V>=0] <- "e3"
nftpi$Experiment[TheKiss_E3_90V>=0] <- "e3"
nftpi$Experiment[TheKiss_E4_ControlV>=0] <- "e4"
nftpi$Experiment[TheKiss_E4_50V>=0] <- "e4"
nftpi$Experiment[TheKiss_E4_90V>=0] <- "e4"
table(nftpi$Experiment)

#Conditions
# Creating a column with experiments
attach(nftpi)
nftpi$Percentage[TheKiss_E1_ControlV>=0] <- "c"
nftpi$Percentage[TheKiss_E1_50V>=0] <- "50"
nftpi$Percentage[TheKiss_E1_90V>=0] <- "90"
nftpi$Percentage[TheKiss_E2_ControlV>=0] <- "c"
nftpi$Percentage[TheKiss_E2_50V>=0] <- "50"
nftpi$Percentage[TheKiss_E2_90V>=0] <- "90"
nftpi$Percentage[TheKiss_E3_ControlV>=0] <- "c"
nftpi$Percentage[TheKiss_E3_50V>=0] <- "50"
nftpi$Percentage[TheKiss_E3_90V>=0] <- "90"
nftpi$Percentage[TheKiss_E4_ControlV>=0] <- "c"
nftpi$Percentage[TheKiss_E4_50V>=0] <- "50"
nftpi$Percentage[TheKiss_E4_90V>=0] <- "90"
table(nftpi$Percentage)

### Ilicitation prices
# Organizing Data price with incitation
attach(nftpi)
nftpi$FPrice[FPrice_E1_Control6=="No"] <- 0
nftpi$FPrice[FPrice_E1_Control6=="Yes"] <- 600
nftpi$FPrice[FPrice_E1_Control16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E1_Control54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E1Control78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E1_Control102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E1_Control126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E1_Control150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E2Control6=="No"] <- 0
nftpi$FPrice[FPrice_E2Control6=="Yes"] <- 600
nftpi$FPrice[FPrice_E2Control16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E2_Control54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E2_Control78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E2_Control102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E2_Control126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E2_Control150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E3_Control6=="No"] <- 0
nftpi$FPrice[FPrice_E3_Control6=="Yes"] <- 600
nftpi$FPrice[FPrice_E3_Control16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E3_Control54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E3Control78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E3_Control102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E3_Control126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E3_Control150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E4_Control6=="No"] <- 0
nftpi$FPrice[FPrice_E4_Control6=="Yes"] <- 600
nftpi$FPrice[FPrice_E4_Control16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E4_Control54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E4Control78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E4_Control102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E4_Control126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E4_Control150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E1_90_6=="No"] <- 0
nftpi$FPrice[FPrice_E1_90_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E1_90_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E1_90_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E1_90_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E1_90_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E1_90_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E1_90_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E2_90_6=="No"] <- 0
nftpi$FPrice[FPrice_E2_90_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E2_90_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E2_90_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E2_90_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E2_90_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E2_90_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E2_90_150=="Yes"] <-15000
nftpi$FPrice[FPrice_E3_90_6=="No"] <- 0
nftpi$FPrice[FPrice_E3_90_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E3_90_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E3_90_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E3_90_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E3_90_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E3_90_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E3_90_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E4_90_6=="No"] <- 0
nftpi$FPrice[FPrice_E4_90_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E4_90_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E4_90_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E4_90_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E4_90_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E4_90_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E4_90_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E1_50_6=="No"] <- 0
nftpi$FPrice[FPrice_E1_50_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E1_50_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E1_50_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E1_50_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E1_50_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E1_50_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E1_50_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E2_50_6=="No"] <- 0
nftpi$FPrice[FPrice_E2_50_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E2_50_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E2_50_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E2_50_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E2_50_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E2_50_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E2_50_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E3_50_6=="No"] <- 0
nftpi$FPrice[FPrice_E3_50_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E3_50_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E3_50_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E3_50_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E3_50_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E3_50_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E3_50_150=="Yes"] <- 15000
nftpi$FPrice[FPrice_E4_50_6=="No"] <- 0
nftpi$FPrice[FPrice_E4_50_6=="Yes"] <- 600
nftpi$FPrice[FPrice_E4_50_16=="Yes"] <- 1600
nftpi$FPrice[FPrice_E4_50_54=="Yes"] <- 5400
nftpi$FPrice[FPrice_E4_50_78=="Yes"] <- 7800
nftpi$FPrice[FPrice_E4_50_102=="Yes"] <- 10200
nftpi$FPrice[FPrice_E4_50_126=="Yes"] <- 12600
nftpi$FPrice[FPrice_E4_50_150=="Yes"] <- 15000
table(nftpi$FPrice)
#View(nftpi)

#Cheking if my code worked
check_ilicitation <- nftpi[c("FPrice_E1Control78","FPrice_E1_Control54","FPrice_E1_Control16","FPrice_E1_Control6","FPrice_E1_Control102",
                             "FPrice_E1_Control126", "FPrice_E1_Control150", "FPrice_E1_90_78","FPrice_E1_90_54","FPrice_E1_90_16", "FPrice_E1_90_6",
                             "FPrice_E1_90_102","FPrice_E1_90_126", "FPrice_E1_90_150","FPrice_E1_50_78","FPrice_E1_50_54", "FPrice_E1_50_16",
                             "FPrice_E1_50_6","FPrice_E1_50_102","FPrice_E1_50_126","FPrice_E1_50_150","FPrice_E2_Control78","FPrice_E2_Control102",
                             "FPrice_E2_Control126","FPrice_E2_Control150","FPrice_E2_Control54","FPrice_E2Control16","FPrice_E2Control6",
                             "FPrice_E2_90_78","FPrice_E2_90_102","FPrice_E2_90_126","FPrice_E2_90_150","FPrice_E2_90_54","FPrice_E2_90_16",
                             "FPrice_E2_90_6","FPrice_E2_50_78","FPrice_E2_50_102","FPrice_E2_50_126","FPrice_E2_50_150","FPrice_E2_50_54",
                             "FPrice_E2_50_16","FPrice_E2_50_6","FPrice_E3Control78","FPrice_E3_Control54","FPrice_E3_Control16","FPrice_E3_Control6",
                             "FPrice_E3_Control102","FPrice_E3_Control126","FPrice_E3_Control150","FPrice_E3_90_78","FPrice_E3_90_54","FPrice_E3_90_16",
                             "FPrice_E3_90_6","FPrice_E3_90_102","FPrice_E3_90_126","FPrice_E3_90_150","FPrice_E3_50_78","FPrice_E3_50_54","FPrice_E3_50_16",
                             "FPrice_E3_50_6","FPrice_E3_50_102","FPrice_E3_50_126","FPrice_E3_50_150","FPrice_E4Control78","FPrice_E4_Control54",
                             "FPrice_E4_Control16","FPrice_E4_Control6","FPrice_E4_Control102","FPrice_E4_Control126","FPrice_E4_Control150",
                             "FPrice_E4_90_78","FPrice_E4_90_54","FPrice_E4_90_16","FPrice_E4_90_6","FPrice_E4_90_102","FPrice_E4_90_126",
                             "FPrice_E4_90_150","FPrice_E4_50_78","FPrice_E4_50_54","FPrice_E4_50_16","FPrice_E4_50_6","FPrice_E4_50_102","FPrice_E4_50_126",
                             "FPrice_E4_50_150","FPrice","Condition")]
#View(check_ilicitation)

#Proud
attach(nftpi)
nftpi$Proud[Proud_1=="No"] <- 1
nftpi$Proud[Proud_1=="Yes"] <- 2

attach(nftpi)
nftpi$Owner_[Owner=="No"] <- 1
nftpi$Owner_[Owner=="Yes"] <- 2

#Averaging the prices of NFTs per condition - Taking MLK out due to the problem with validation - answers came back empty
nftpi$AveragePrice_E1_C <- rowMeans(nftpi[,c("TheKiss_E1_ControlV", "Dali_E1_ControlV", "911_E1_ControlV",
                                                       "Mandela_E1_ControlV", "Tol_E1_ControlV", "Einstein_E1_ControlV",
                                                       "Malala_E1_ControlV", "Monaliza_E1_ControlV", "UN_E1_ControlV")],na.rm=TRUE)
nftpi$AveragePrice_E1_90 <- rowMeans(nftpi[,c("TheKiss_E1_90V", "Dali_E1_90V","911_E1_90V", "Mandela_E1_90V", "Tol_E1_90V", "Einstein_E1_90V",
                                              "Malala_E1_90V", "Monaliza_E1_90V", "UN_E1_90V")],na.rm=TRUE)
nftpi$AveragePrice_E1_50 <- rowMeans(nftpi[,c("TheKiss_E1_50V", "Dali_E1_50V","911_E1_50V", "Mandela_E1_50V", "Tol_E1_50V",
                                              "Einstein_E1_50V","Malala_E1_50V", "Monaliza_E1_50V", "UN_E1_50V")],na.rm=TRUE)
nftpi$AveragePrice_E2_C <- rowMeans(nftpi[,c("TheKiss_E2_ControlV","Dali_E2_ControlV","911_E2_ControlV","Mandela_E2_ControlV","Tol_E2_ControlV",
                                             "Einstein_E2_ControlV","Malala_E2_ControlV","Monalisa_E2_ControlV","UN_E2_ControlV")],na.rm=TRUE)
nftpi$AveragePrice_E2_90 <- rowMeans(nftpi[,c("TheKiss_E2_90V", "Dali_E2_90V","911_E2_90V", "Mandela_E2_90V","Tol_E2_90V","Einstein_E2_90V",
                                              "Malala_E2_90V", "Monalisa_E2_90V","UN_E2_90V")],na.rm=TRUE)
nftpi$AveragePrice_E2_50 <- rowMeans(nftpi[,c("TheKiss_E2_50V", "Dali_E2_50V","911_E2_50V","Mandela_E2_50V","Tol_E2_50V", "Einstein_E2_50V", 
                                              "Malala_E2_50V","Monalisa_E2_50V","UN_E2_50V")],na.rm=TRUE)
nftpi$AveragePrice_E3_C <- rowMeans(nftpi[,c("TheKiss_E3_ControlV","Dali_E3_ControlV", "911_E3_ControlV","Mandela_E3_ControlV","Tol_E3_ControlV",
                                             "Einstein_E3_ControlV","Malala_E3_ControlV","Monaliza_E3_ControlV","UN_E3_ControlV")],na.rm=TRUE)
nftpi$AveragePrice_E3_90 <- rowMeans(nftpi[,c("TheKiss_E3_90V","Dali_E3_90V", "911_E3_90V", "Mandela_E3_90V","Tol_E3_90V","Einstein_E3_90V",
                                              "Malala_E3_90V", "Monaliza_E3_90V","UN_E3_90V")],na.rm=TRUE)
nftpi$AveragePrice_E3_50 <- rowMeans(nftpi[,c("TheKiss_E3_50V","Dali_E3_50V","911_E3_50V","Mandela_E3_50V","Tol_E3_50V","Einstein_E3_50V",
                                              "Malala_E3_50V","Monaliza_E3_50V","UN_E3_50V")],na.rm=TRUE)
nftpi$AveragePrice_E4_C <- rowMeans(nftpi[,c("TheKiss_E4_ControlV","Dali_E4_ControlV","911_E4_ControlV","Mandela_E4_ControlV","Tol_E4_ControlV",
                                             "Einstein_E4_ControlV","Malala_E4_ControlV","Monaliza_E4_ControlV","UN_E4_ControlV")],na.rm=TRUE)
nftpi$AveragePrice_E4_90 <- rowMeans(nftpi[,c("TheKiss_E4_90V","Dali_E4_90V", "911_E4_90V","Mandela_E4_90V","Tol_E4_90V","Einstein_E4_90V",
                                              "Malala_E4_90V", "Monaliza_E4_90V","UN_E4_90V")],na.rm=TRUE)
nftpi$AveragePrice_E4_50 <- rowMeans(nftpi[,c("TheKiss_E4_50V", "Dali_E4_50V","911_E4_50V","Mandela_E4_50V","Tol_E4_50V","Einstein_E4_50V",
                                              "Malala_E4_50V", "Monaliza_E4_50V","UN_E4_50V")],na.rm=TRUE)
nftpi$AllAverages <- rowSums(nftpi[,c("AveragePrice_E1_C","AveragePrice_E1_90","AveragePrice_E1_50", "AveragePrice_E2_C","AveragePrice_E2_90",
                                       "AveragePrice_E2_50","AveragePrice_E3_C","AveragePrice_E3_90","AveragePrice_E3_50","AveragePrice_E4_C",
                                      "AveragePrice_E4_90","AveragePrice_E4_50")],na.rm = TRUE)
nftpi$AveragePrice_C <- rowMeans(nftpi[,c("AveragePrice_E1_C","AveragePrice_E2_C","AveragePrice_E3_C","AveragePrice_E4_C")],na.rm=TRUE)
nftpi$AveragePrice_C_Buy <- rowMeans(nftpi[,c("AveragePrice_E1_C","AveragePrice_E3_C","AveragePrice_E4_C")],na.rm=TRUE)
nftpi$AveragePrice_90 <- rowMeans(nftpi[,c("AveragePrice_E1_90","AveragePrice_E2_90","AveragePrice_E3_90","AveragePrice_E4_90")],na.rm=TRUE)
nftpi$AveragePrice_90_Buy <- rowMeans(nftpi[,c("AveragePrice_E1_90","AveragePrice_E3_90","AveragePrice_E4_90")],na.rm=TRUE)
nftpi$AveragePrice_50 <- rowMeans(nftpi[,c("AveragePrice_E1_50","AveragePrice_E2_50","AveragePrice_E3_50","AveragePrice_E4_50")],na.rm=TRUE)
nftpi$AveragePrice_50_Buy <- rowMeans(nftpi[,c("AveragePrice_E1_50","AveragePrice_E3_50","AveragePrice_E4_50")],na.rm=TRUE)

#Checking if the code I did was right
#summary(nftpi$AllAverages)
nftpi$AllAveragesCheck <- nftpi$AllAverages - nftpi$AveragePrice_E1_C
nftpi_check <- nftpi[!is.na(nftpi$TheKiss_E1_ControlV),]
table(nftpi_check$AllAveragesCheck)
nftpi$AllAveragesCheck <- nftpi$AllAverages - nftpi$AveragePrice_E2_C
nftpi_check <- nftpi[!is.na(nftpi$TheKiss_E2_ControlV),]
table(nftpi_check$AllAveragesCheck)
nftpi$AllAveragesCheck <- nftpi$AllAverages - nftpi$AveragePrice_E3_C
nftpi_check <- nftpi[!is.na(nftpi$TheKiss_E3_ControlV),]
table(nftpi_check$AllAveragesCheck)
nftpi$AllAveragesCheck <- nftpi$AllAverages - nftpi$AveragePrice_E4_C
nftpi_check <- nftpi[!is.na(nftpi$TheKiss_E4_ControlV),]
table(nftpi_check$AllAveragesCheck)

#as.numeric
nftpi$LikeNFT_1<-as.numeric(nftpi$LikeNFT_1)
nftpi$Proud_1<-as.numeric(nftpi$Proud_1)
nftpi$Like_1<-as.numeric(nftpi$Like_1)
nftpi$Like_2<-as.numeric(nftpi$Like_2)
nftpi$Like_3<-as.numeric(nftpi$Like_3)
nftpi$Like_4<-as.numeric(nftpi$Like_4)
nftpi$SellFor<-as.numeric(nftpi$SellFor)

# Log of Average
nftpi$LogAllAverage<-log(nftpi$AllAverages)

nftpi$Experiment<-as.factor(nftpi$Experiment)
nftpi$BuySell<-as.factor(nftpi$BuySell)
nftpi$BuyArtistAnonymousUser<-as.factor(nftpi$BuyArtistAnonymousUser)
nftpi$ArtistAnonymousUser<-as.factor(nftpi$ArtistAnonymousUser)
nftpi$Percentage<-as.factor(nftpi$Percentage)
nftpi$Partner<-as.factor(nftpi$Partner)

# Analysis without 0s
nftpi_w0 <- nftpi[nftpi$AllAverages>0,]
nftpi_w0$LogAllAverage_w0<-log(nftpi_w0$AllAverages)
summary(nftpi_w0$LogAllAverage_w0)
# Creating a demographic table
explanatory =  c("Age", "Income", "Ethnicity", "Gender", "Education")
dependent = "Condition"
nftpi %>% 
  summary_factorlist(dependent, explanatory, na_include=TRUE)
table1_w0 <- nftpi %>%  
  summary_factorlist(dependent, explanatory, na_include=TRUE, 
                     add_dependent_label=TRUE,
                     dependent_label_prefix = "Demographic of participants per condition: "
  )
table1

write.table(table1, file = "demographics_w0.txt", sep = ",", quote = FALSE, row.names = F)

#Checking if the data is normal
byf.shapiro(AllAverages~Condition,nftpi)
byf.shapiro(FPrice~Condition,nftpi)
byf.shapiro(LogAllAverage~Condition,nftpi)
nftpi$LogAllAverage

#Checking homogeneidade de variâncias
leveneTest(AllAverages ~ Condition, nftpi, center=mean)
leveneTest(FPrice ~ Condition, nftpi, center=mean)

# Subseting the data
nftpi_AllAverages_e1c <- nftpi[nftpi$Condition=="e1_c",]
nftpi_AllAverages_e190 <- nftpi[nftpi$Condition=="e1_90",]
nftpi_AllAverages_e150 <- nftpi[nftpi$Condition=="e1_50",]
nftpi_AllAverages_e2c <- nftpi[nftpi$Condition=="e2_c",]
nftpi_AllAverages_e290 <- nftpi[nftpi$Condition=="e2_90",]
nftpi_AllAverages_e250 <- nftpi[nftpi$Condition=="e2_50",]
nftpi_AllAverages_e3c <- nftpi[nftpi$Condition=="e3_c",]
nftpi_AllAverages_e390 <- nftpi[nftpi$Condition=="e3_90",]
nftpi_AllAverages_e350 <- nftpi[nftpi$Condition=="e3_50",]
nftpi_AllAverages_e4c <- nftpi[nftpi$Condition=="e4_c",]
nftpi_AllAverages_e490 <- nftpi[nftpi$Condition=="e4_90",]
nftpi_AllAverages_e450 <- nftpi[nftpi$Condition=="e4_50",]
nftpi_AllAverages_e1 <- nftpi[nftpi$Condition=="e1_c" | nftpi$Condition=="e1_90" | nftpi$Condition=="e1_50",]
nftpi_AllAverages_e2 <- nftpi[nftpi$Condition=="e2_c" | nftpi$Condition=="e2_90" | nftpi$Condition=="e2_50",]
nftpi_AllAverages_e3 <- nftpi[nftpi$Condition=="e3_c" | nftpi$Condition=="e3_90" | nftpi$Condition=="e3_50",]
nftpi_AllAverages_e4 <- nftpi[nftpi$Condition=="e4_c" | nftpi$Condition=="e4_90" | nftpi$Condition=="e4_50",]
nftpi_AllAverages_c <- nftpi[nftpi$Condition=="e1_c" | nftpi$Condition=="e2_c" | nftpi$Condition=="e3_c"
                                   | nftpi$Condition=="e4_c",]
nftpi_AllAverages_90 <- nftpi[nftpi$Condition=="e1_90" | nftpi$Condition=="e2_90" | nftpi$Condition=="e3_90"
                                    | nftpi$Condition=="e4_90",]
nftpi_AllAverages_50 <- nftpi[nftpi$Condition=="e1_50" | nftpi$Condition=="e2_50" | nftpi$Condition=="e3_50"
                                    | nftpi$Condition=="e4_50",]
nftpi_AllAverages_buy <- nftpi[nftpi$Condition=="e1_c" | nftpi$Condition=="e1_90" | nftpi$Condition=="e1_50"
                                     | nftpi$Condition=="e3_c" | nftpi$Condition=="e3_90" | nftpi$Condition=="e3_50"
                                     | nftpi$Condition=="e4_c" | nftpi$Condition=="e4_90" | nftpi$Condition=="e4_50",]
nftpi_AllAverages_3_4 <- nftpi[ nftpi$Condition=="e3_c" | nftpi$Condition=="e3_90" | nftpi$Condition=="e3_50"
                                      | nftpi$Condition=="e4_c" | nftpi$Condition=="e4_90" | nftpi$Condition=="e4_50",]

######################################################
# EXECUTANDO AULA DO CARLOS 13, 14 E 15
######################################################

#Passo 0: EDA  Análise de dados com 0
glimpse(nftpi)
summary(nftpi)

png(file = "AllAverages_AllGraphs.png")
par(mfrow = c(4,2))
plot(nftpi$LikeNFT_1,nftpi$AllAverages)
plot(nftpi$Proud_1,nftpi$AllAverages)
plot(nftpi$FPrice,nftpi$AllAverages)
plot(nftpi$Like_1,nftpi$AllAverages)
plot(nftpi$Like_2,nftpi$AllAverages)
plot(nftpi$Like_3,nftpi$AllAverages)
plot(nftpi$Like_4,nftpi$AllAverages)
plot(nftpi$Owner_,nftpi$AllAverages)
dev.off()

png(file="Histogram_AllAverages")
par(mfrow=c(1,2))
hist(nftpi$AllAverages)
hist(nftpi_w0$AllAverages)
dev.off()

#Passo 1 é ajustar o modelo
modelo1<-lm(AllAverages~Proud_1+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi)
summary(modelo1)
modelo1_w0<-lm(AllAverages~Proud_1+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_w0)
summary(modelo1_w0)

#Passo 2: Verificar outliers e forma funcional
# Para isso fazemos um residuais vs yhat e/ou residuais vs variaveis explicativas
t=rstudent(modelo1)
yhat=fitted(modelo1)
png(file="Passo2_Modelo1")
par(mfrow=c(4,2))
plot(yhat,t)
plot(nftpi$Experiment,t)
plot(nftpi$Proud_1,t)
plot(nftpi$Like_1,t)
plot(nftpi$Like_2,t)
plot(nftpi$Like_3,t)
plot(nftpi$Like_4,t)
plot(nftpi$LikeNFT_1,t)
dev.off()

t_w0=rstudent(modelo1_w0)
yhat_w0=fitted(modelo1_w0)
png(file="Passo2_Modelo1_w0")
par(mfrow=c(4,2))
plot(yhat_w0,t_w0)
plot(nftpi_w0$Experiment,t_w0)
plot(nftpi_w0$Proud_1,t_w0)
plot(nftpi_w0$Like_1,t_w0)
plot(nftpi_w0$Like_2,t_w0)
plot(nftpi_w0$Like_3,t_w0)
plot(nftpi_w0$Like_4,t_w0)
plot(nftpi_w0$LikeNFT_1,t_w0)
dev.off()

modelo2<-lm(AllAverages~Proud_1+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+I(yhat^2), data=nftpi)
summary(modelo2)
modelo2_w0<-lm(AllAverages~Proud_1+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+I(yhat_w0^2), data=nftpi_w0)
summary(modelo2_w0)

modelo3<-lm(AllAverages~Proud_1+I(Proud_1^2)+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi)
summary(modelo3)
modelo3_w0<-lm(AllAverages~Proud_1+I(Proud_1^2)+Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_w0)
summary(modelo3_w0)

modelo4<-lm(AllAverages~Proud_1+I(Proud_1^2)+Experiment+Like_1+Like_2+Like_3+Like_4+I(Like_4^2)+LikeNFT_1, data=nftpi)
summary(modelo4)
modelo4_w0<-lm(AllAverages~Proud_1+I(Proud_1^2)+Experiment+Like_1+Like_2+Like_3+Like_4+I(Like_4^2)+LikeNFT_1, data=nftpi_w0)
summary(modelo4_w0)

modelo_e1<-lm(AllAverages~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e1)
summary(modelo_e1)
yhat_e1=fitted(modelo_e1)
modelo_e1_2<-lm(AllAverages~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+I(yhat_e1^2), data=nftpi_AllAverages_e1)
summary(modelo_e1_2)
modelo_e1_3<-lm(AllAverages~Proud_1+I(Proud_1^2)+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e1)
summary(modelo_e1_3)
modelo_e1_4<-lm(AllAverages~Proud_1+I(Proud_1^2)+Condition+Like_4+I(Like_4^2)+LikeNFT_1+I(yhat_e1^2), data=nftpi_AllAverages_e1)
summary(modelo_e1_4)
modelo_e1_4<-lm(AllAverages~Proud_1+I(Proud_1^2)+Condition+Like_4+I(Like_4^2), data=nftpi_AllAverages_e1)
summary(modelo_e1_4)
modelo_e1_5<-lm(AllAverages~Proud_1+I(Proud_1^2)+Condition+Like_4+I(Like_4^2)+Condition*Like_4, data=nftpi_AllAverages_e1)
summary(modelo_e1_5)
modelo_e1_6<-lm(AllAverages~Proud_1+Condition+Like_4+I(Like_4^2)+Condition*Proud_1, data=nftpi_AllAverages_e1)
summary(modelo_e1_6)
modelo_e1_7<-lm(AllAverages~Proud_1+I(Proud_1^2)+Condition, data=nftpi_AllAverages_e1)
summary(modelo_e1_7)
modelo_e1_8<-lm(AllAverages~Proud_1+Condition, data=nftpi_AllAverages_e1)
summary(modelo_e1_8)
kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_e1)
#NOT statistical significant averages between conditions
anova_e1<-aov(AllAverages~Condition, data=nftpi_AllAverages_e1)
summary(anova_e1)
#NOT statistical significant averages between conditions

modelo_e2<-lm(AllAverages~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e2)
summary(modelo_e2)
yhat_e2=fitted(modelo_e2)
kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_e2)
#Statistical significant averages between conditions
anova_e2<-aov(AllAverages~Condition, data=nftpi_AllAverages_e2)
summary(anova_e2)
#NOT statistical significant averages between conditions

modelo_e3<-lm(AllAverages~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e3)
summary(modelo_e3)
kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_e3)
#NOT statistical significant averages between conditions
anova_e3<-aov(AllAverages~Condition, data=nftpi_AllAverages_e3)
summary(anova_e3)
#NOT statistical significant averages between conditions

modelo_e4<-lm(AllAverages~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e4)
summary(modelo_e4)
kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_e4)
#NOT statistical significant averages between conditions
anova_e4<-aov(AllAverages~Condition, data=nftpi_AllAverages_e4)
summary(anova_e4)
#NOT statistical significant averages between conditions

modelo_e1_log<-lm(LogAllAverage~Proud_1+Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1, data=nftpi_AllAverages_e1, na.action=na.exclude)
summary(nftpi_AllAverages_e1$LogAllAverage)
kruskal.test(LogAllAverage~Condition, data = nftpi_AllAverages_e1)
#NOT statistical significant averages between conditions
anova_e4<-aov(AllAverages~Condition, data=nftpi_AllAverages_e4)
summary(anova_e4)
#NOT statistical significant averages between conditions
nftpi_AllAverages_e2_below50 <- 

png(file="hist_e2")
hist(nftpi_AllAverages_e2$AllAverages)
dev.off()

# Kruskal Test
kruskal.test(AllAverages~Condition, data = nftpi)
post_hoc_AllAverages <- dunn_test(AllAverages~Condition, data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages, file = "post_hoc_AllAverages.txt", sep = ",", quote = FALSE, row.names = F)

kruskal.test(AllAverages~Condition, data = nftpi)
post_hoc_AllAverages <- dunn_test(AllAverages~Condition, data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages, file = "post_hoc_AllAverages_w0.txt", sep = ",", quote = FALSE, row.names = F)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_e1)
kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_e1)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_e2)
post_hoc_AllAverages_e2 <- dunn_test(AllAverages~Condition, data = nftpi_AllAverages_w0_e2, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages_e2, file = "post_hoc_AllAverages_e2.txt", sep = ",", quote = FALSE, row.names = F)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_e3)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_e4)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_c)
post_hoc_AllAverages_c <- dunn_test(AllAverages~Condition, data = nftpi_AllAverages_w0_c, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages_c, file = "post_hoc_AllAverages_c.txt", sep = ",", quote = FALSE, row.names = F)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_90)
post_hoc_AllAverages_90 <- dunn_test(AllAverages~Condition, data = nftpi_AllAverages_w0_90, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages_90, file = "post_hoc_AllAverages_90.txt", sep = ",", quote = FALSE, row.names = F)

kruskal.test(AllAverages~Condition, data = nftpi_AllAverages_w0_50)
post_hoc_AllAverages_50 <- dunn_test(AllAverages~Condition, data = nftpi_AllAverages_w0_50, p.adjust.method = "bonferroni")
write.table(post_hoc_AllAverages_50, file = "post_hoc_AllAverages_50.txt", sep = ",", quote = FALSE, row.names = F)

# Boxplot log ()
png(file = "allexperiments_log.png")
boxplot(LogAllAverage~Condition, data = nftpi)
dev.off()

png(file = "e1_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_e1)
dev.off()

png(file = "e2_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_e2)
dev.off()

png(file = "e3_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_e3)
dev.off()

png(file = "e4_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_e4)
dev.off()

png(file = "c_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_c)
dev.off()

png(file = "90_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_90)
dev.off()

png(file = "50_log.png")
boxplot(LogAllAverage~Condition, data = nftpi_AllAverages_w0_50)
dev.off()

# Regression
r_AllAverages_w0 <- lm(AllAverages~Condition+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi)
summary(r_AllAverages_w0)

r_AllAverages_w0_experiment <- lm(AllAverages~Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor+Experiment*Proud_1,data=nftpi)
summary(r_AllAverages_w0_experiment)

r_AllAverages_w0_percentages <- lm(AllAverages~Percentage+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi)
summary(r_AllAverages_w0_percentages)

r_AllAverages_w0_expXperc <- lm(AllAverages~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor+Experiment*Percentage,data=nftpi)
summary(r_AllAverages_w0_expXperc)

r_AllAverages_w0_buysell <- lm(AllAverages~BuySell+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor+BuySell*Percentage,data=nftpi)
summary(r_AllAverages_w0_buysell)

r_AllAverages_w0_all <- lm(AllAverages~Percentage+Experiment+Experiment*Percentage+Like_4+Like_4*Experiment+Like_4*Percentage,data=nftpi)
summary(r_AllAverages_w0_all)

r_AllAverages_w0 <- lm(LogAllAverage~BuySell+Percentage+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor+BuySell*Proud_1,data=nftpi)
summary(r_AllAverages_w0)

r_AllAverages_w0 <- lm(LogAllAverage~Experiment+Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor+Experiment*Proud_1+Experiment*Owner,data=nftpi_AllAverages_w0_3_4)
summary(r_AllAverages_w0)

png(file = "Carlos.png")
plot(r_AllAverages_w0)
dev.off()
residuals_Allaverages <- residuals(r_AllAverages_w0)

png(file = "Carlos2.png")
hist(residuals_Allaverages)
dev.off()

png(file = "Carlos3.png")
acf(residuals_Allaverages)
dev.off()

png(file = "Carlos4.png")
hist(nftpi$LogAllAverage)
dev.off()

summary(nftpi$LogAllAverage)
quantile(nftpi$LogAllAverage,probs = c(0.01,0.05,0.1,0.15,0.2,0.25))

#Plots Dan
e90 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_90, mean)
rownames(e90) <- e90[,1]
e90 <- as.matrix(e90[,-1])
lim_e90 <- 1.5*max(e90)
error.bar_e90 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e90 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_90, sd)
rownames(stdev_e90) <- stdev_e90[,1]
stdev_e90 <- as.matrix(stdev_e90[,-1]) * 1.96 / 10
png(file = "90_AllExperiments_mean_sd.png")
barplot_e90 <- barplot(e90,main = "All Experiments:90%", beside=T , legend.text=T,col="grey",ylim=c(0,lim_e90),
                       ylab="Mean and sd for the average price",names.arg =c("e1","e2","e3","e4"))
error.bar_e90(barplot_e90,e90,stdev_e90)
dev.off()

e50 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_50, mean)
rownames(e50) <- e50[,1]
e50 <- as.matrix(e50[,-1])
lim_e50 <- 1.5*max(e50)
error.bar_e50 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e50 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_50, sd)
rownames(stdev_e50) <- stdev_e50[,1]
stdev_e50 <- as.matrix(stdev_e50[,-1]) * 1.96 / 10
png(file = "50_AllExperiments_mean_sd.png")
barplot_e50 <- barplot(e50, main = "All Experiments: 50%", beside=T , legend.text=T,col="grey",ylim=c(0,lim_e50),
                       ylab="Mean and sd for the average price",names.arg=c("e1","e2","e3","e4"))
error.bar_e50(barplot_e50,e50,stdev_e50)
dev.off()

ec <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_c, mean)
rownames(ec) <- ec[,1]
ec <- as.matrix(ec[,-1])
lim_ec <- 2.2*max(ec)
error.bar_ec <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_ec <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_c, sd)
rownames(stdev_ec) <- stdev_ec[,1]
stdev_ec <- as.matrix(stdev_ec[,-1]) * 1.96 / 10
png(file = "100_AllExperiments_mean_sd.png")
barplot_ec <- barplot(ec, main = "All Experiments: 100%",beside=T , legend.text=T,col="grey",ylim=c(0,lim_ec),
                       ylab="Mean and sd for the average price",names.arg=c("e1","e2","e3","e4"))
error.bar_ec(barplot_ec,ec,stdev_ec)
dev.off()

e1 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e1, mean)
rownames(e1) <- e1[,1]
e1 <- as.matrix(e1[,-1])
lim_e1 <- 2*max(e1)
error.bar_e1 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e1 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e1, sd)
rownames(stdev_e1) <- stdev_e1[,1]
stdev_e1 <- as.matrix(stdev_e1[,-1]) * 1.96 / 10
png(file = "Experiment1_mean_sd.png")
barplot_e1 <- barplot(e1, main = "Experiment 1: Buying artist/remainder Artist",beside=T, legend.text=T,col="grey",ylim=c(0,lim_e1),
                       ylab="Mean and sd for the average price", names.arg=c("e1_50","e1_90","e1_c"))
error.bar_e1(barplot_e1,e1,stdev_e1)
dev.off()

e2 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e2, mean)
rownames(e2) <- e2[,1]
e2 <- as.matrix(e2[,-1])
lim_e2 <- 2.2*max(e2)
error.bar_e2 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e2 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e2, sd)
rownames(stdev_e2) <- stdev_e2[,1]
stdev_e2 <- as.matrix(stdev_e2[,-1]) * 1.96 / 10
png(file = "Experiment2_mean_sd.png")
barplot_e2 <- barplot(e2, main = "Experiment 2: Selling/remainder Artist",beside=T, legend.text=T,col="grey",ylim=c(0,lim_e2),
                      ylab="Mean and sd for the average selling price", names.arg=c("e2_50","e2_90","e2_c"))
error.bar_e2(barplot_e2,e2,stdev_e2)
dev.off()

e3 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e3, mean)
rownames(e3) <- e3[,1]
e3 <- as.matrix(e3[,-1])
lim_e3 <- 2*max(e3)
error.bar_e3 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e3 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e3, sd)
rownames(stdev_e3) <- stdev_e3[,1]
stdev_e3 <- as.matrix(stdev_e3[,-1]) * 1.96 / 10
png(file = "Experiment3_mean_sd.png")
barplot_e3 <- barplot(e3, main = "Experiment 3: Buying anonymous user/remainder anonymous user",beside=T, legend.text=T,col="grey",ylim=c(0,lim_e3),
                      ylab="Mean and sd for the average price", names.arg=c("e1_50","e1_90","e1_c"))
error.bar_e3(barplot_e3,e3,stdev_e3)
dev.off()

e4 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e4, mean)
rownames(e4) <- e4[,1]
e4 <- as.matrix(e4[,-1])
lim_e4 <- 1.73*max(e4)
error.bar_e4 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e4 <- aggregate(AllAverages~Condition, data=nftpi_AllAverages_w0_e4, sd)
rownames(stdev_e4) <- stdev_e4[,1]
stdev_e4 <- as.matrix(stdev_e4[,-1]) * 1.96 / 10
png(file = "Experiment4_mean_sd.png")
barplot_e4 <- barplot(e4, main = "Experiment 4: Buying anonymous user/remainder Artist",beside=T, legend.text=T,col="grey",ylim=c(0,lim_e4),
                      ylab="Mean and sd for the average price", names.arg=c("e1_50","e1_90","e1_c"))
error.bar_e4(barplot_e4,e4,stdev_e4)
dev.off()

############################################# Highest Value #####################################################################################
#################################################################################################################################################

#Highest value
nftpi$HPrice_E1_C <- apply(nftpi[,c("TheKiss_E1_ControlV", "Dali_E1_ControlV", "911_E1_ControlV",
                                             "Mandela_E1_ControlV", "Tol_E1_ControlV", "Einstein_E1_ControlV",
                                             "Malala_E1_ControlV", "Monaliza_E1_ControlV", "UN_E1_ControlV")],1,max)
nftpi$HPrice_E1_90 <- apply(nftpi[,c("TheKiss_E1_90V", "Dali_E1_90V","911_E1_90V", "Mandela_E1_90V", "Tol_E1_90V", "Einstein_E1_90V",
                                              "Malala_E1_90V", "Monaliza_E1_90V", "UN_E1_90V")],1,max)
nftpi$HPrice_E1_50 <- apply(nftpi[,c("TheKiss_E1_50V", "Dali_E1_50V","911_E1_50V", "Mandela_E1_50V", "Tol_E1_50V",
                                              "Einstein_E1_50V","Malala_E1_50V", "Monaliza_E1_50V", "UN_E1_50V")],1,max)
nftpi$HPrice_E2_C <- apply(nftpi[,c("TheKiss_E2_ControlV","Dali_E2_ControlV","911_E2_ControlV","Mandela_E2_ControlV","Tol_E2_ControlV",
                                             "Einstein_E2_ControlV","Malala_E2_ControlV","Monalisa_E2_ControlV","UN_E2_ControlV")],1,max)
nftpi$HPrice_E2_90 <- apply(nftpi[,c("TheKiss_E2_90V", "Dali_E2_90V","911_E2_90V", "Mandela_E2_90V","Tol_E2_90V","Einstein_E2_90V",
                                              "Malala_E2_90V", "Monalisa_E2_90V","UN_E2_90V")],1,max)
nftpi$HPrice_E2_50 <- apply(nftpi[,c("TheKiss_E2_50V", "Dali_E2_50V","911_E2_50V","Mandela_E2_50V","Tol_E2_50V", "Einstein_E2_50V", 
                                              "Malala_E2_50V","Monalisa_E2_50V","UN_E2_50V")],1,max)
nftpi$HPrice_E3_C <- apply(nftpi[,c("TheKiss_E3_ControlV","Dali_E3_ControlV", "911_E3_ControlV","Mandela_E3_ControlV","Tol_E3_ControlV",
                                             "Einstein_E3_ControlV","Malala_E3_ControlV","Monaliza_E3_ControlV","UN_E3_ControlV")],1,max)
nftpi$HPrice_E3_90 <- apply(nftpi[,c("TheKiss_E3_90V","Dali_E3_90V", "911_E3_90V", "Mandela_E3_90V","Tol_E3_90V","Einstein_E3_90V",
                                              "Malala_E3_90V", "Monaliza_E3_90V","UN_E3_90V")],1,max)
nftpi$HPrice_E3_50 <- apply(nftpi[,c("TheKiss_E3_50V","Dali_E3_50V","911_E3_50V","Mandela_E3_50V","Tol_E3_50V","Einstein_E3_50V",
                                              "Malala_E3_50V","Monaliza_E3_50V","UN_E3_50V")],1,max)
nftpi$HPrice_E4_C <- apply(nftpi[,c("TheKiss_E4_ControlV","Dali_E4_ControlV","911_E4_ControlV","Mandela_E4_ControlV","Tol_E4_ControlV",
                                             "Einstein_E4_ControlV","Malala_E4_ControlV","Monaliza_E4_ControlV","UN_E4_ControlV")],1,max)
nftpi$HPrice_E4_90 <- apply(nftpi[,c("TheKiss_E4_90V","Dali_E4_90V", "911_E4_90V","Mandela_E4_90V","Tol_E4_90V","Einstein_E4_90V",
                                              "Malala_E4_90V", "Monaliza_E4_90V","UN_E4_90V")],1,max)
nftpi$HPrice_E4_50 <- apply(nftpi[,c("TheKiss_E4_50V", "Dali_E4_50V","911_E4_50V","Mandela_E4_50V","Tol_E4_50V","Einstein_E4_50V",
                                              "Malala_E4_50V", "Monaliza_E4_50V","UN_E4_50V")],1,max)
nftpi$AllHPrice <- rowSums(nftpi[,c("HPrice_E1_C","HPrice_E1_90","HPrice_E1_50","HPrice_E2_C","HPrice_E2_90","HPrice_E2_50",
                                    "HPrice_E3_C","HPrice_E3_90","HPrice_E3_50","HPrice_E4_C","HPrice_E4_90","HPrice_E4_50")],na.rm = TRUE)
png(file = "histogram_allhprice.png")
hist(nftpi$AllHPrice)
dev.off()
View(nftpi)

#HPrice without 0
nftpi_HPrice_w0 <- nftpi[nftpi$AllHPrice>0,]
nftpi_HPrice_w0$AllHPrice<-as.numeric(nftpi_HPrice_w0$AllHPrice)
table(nftpi_HPrice_w0$AllHPrice)
png(file = "histogram_hprice_w0.png")
hist(nftpi_HPrice_w0$AllHPrice)
dev.off()

#log
nftpi_HPrice_w0$LogAllHPrice<-log(nftpi_HPrice_w0$AllHPrice)

#Subsetting data
nftpi_HPrice_w0_e1c <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_c",]
nftpi_HPrice_w0_e190 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_90",]
nftpi_HPrice_w0_e150 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_50",]
nftpi_HPrice_w0_e2c <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e2_c",]
nftpi_HPrice_w0_e290 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e2_90",]
nftpi_HPrice_w0_e250 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e2_50",]
nftpi_HPrice_w0_e3c <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e3_c",]
nftpi_HPrice_w0_e390 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e3_90",]
nftpi_HPrice_w0_e350 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e3_50",]
nftpi_HPrice_w0_e4c <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e4_c",]
nftpi_HPrice_w0_e490 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e4_90",]
nftpi_HPrice_w0_e450 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e4_50",]
nftpi_HPrice_w0_e1 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_c" | nftpi_HPrice_w0$Condition=="e1_90" | nftpi_HPrice_w0$Condition=="e1_50",]
nftpi_HPrice_w0_e2 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e2_c" | nftpi_HPrice_w0$Condition=="e2_90" | nftpi_HPrice_w0$Condition=="e2_50",]
nftpi_HPrice_w0_e3 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e3_c" | nftpi_HPrice_w0$Condition=="e3_90" | nftpi_HPrice_w0$Condition=="e3_50",]
nftpi_HPrice_w0_e4 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e4_c" | nftpi_HPrice_w0$Condition=="e4_90" | nftpi_HPrice_w0$Condition=="e4_50",]
nftpi_HPrice_w0_c <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_c" | nftpi_HPrice_w0$Condition=="e2_c" | nftpi_HPrice_w0$Condition=="e3_c"
                                     | nftpi_HPrice_w0$Condition=="e4_c",]
nftpi_HPrice_w0_90 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_90" | nftpi_HPrice_w0$Condition=="e2_90" | nftpi_HPrice_w0$Condition=="e3_90"
                                     | nftpi_HPrice_w0$Condition=="e4_90",]
nftpi_HPrice_w0_50 <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_50" | nftpi_HPrice_w0$Condition=="e2_50" | nftpi_HPrice_w0$Condition=="e3_50"
                                     | nftpi_HPrice_w0$Condition=="e4_50",]
nftpi_HPrice_w0_Buy <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e1_c" | nftpi_HPrice_w0$Condition=="e1_90" | nftpi_HPrice_w0$Condition=="e1_50"
                                      | nftpi_HPrice_w0$Condition=="e3_c"|nftpi_HPrice_w0$Condition=="e3_90" | nftpi_HPrice_w0$Condition=="e3_50" 
                                      | nftpi_HPrice_w0$Condition=="e4_c"|nftpi_HPrice_w0$Condition=="e4_90" | nftpi_HPrice_w0$Condition=="e4_50",]
nftpi_HPrice_w0_Sell <- nftpi_HPrice_w0[nftpi_HPrice_w0$Condition=="e2_c" | nftpi_HPrice_w0$Condition=="e2_90" | nftpi_HPrice_w0$Condition=="e2_50",]

#Kruskal Test
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0)
post_hoc_HPrice_w0 <- dunn_test(AllHPrice~Condition, data = nftpi_HPrice_w0, p.adjust.method = "bonferroni")
write.table(post_hoc_HPrice_w0, file = "post_hoc_HPrice_w0.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0_e1)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0_e2)
dunn_test(AllHPrice~Condition, data = nftpi_HPrice_w0_e2, p.adjust.method = "bonferroni")
write.table(post_hoc_HPrice_w0_e2, file = "post_hoc_HPrice_w0_e2.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0_e3)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0_e4)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0_c)
dunn_test(AllHPrice~Condition, data = nftpi_HPrice_w0_c, p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~Condition, data= nftpi_HPrice_w0_90)
dunn_test(AllHPrice~Condition, data= nftpi_HPrice_w0_90,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~Condition, data=nftpi_HPrice_w0_50)
dunn_test(AllHPrice~Condition, data= nftpi_HPrice_w0_50,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_w0)
post_hoc_HPrice_w0 <- dunn_test(AllHPrice~Condition, data = nftpi_HPrice_w0, p.adjust.method = "bonferroni")
write.table(post_hoc_HPrice_w0, file = "post_hoc_HPrice_w0.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(Condition~Age, data = nftpi_HPrice_w0)
kruskal.test(Condition~Ethnicity, data = nftpi_HPrice_w0)
kruskal.test(Condition~Income, data = nftpi_HPrice_w0)
kruskal.test(Condition~Gender, data = nftpi_HPrice_w0)
kruskal.test(Condition~Education, data = nftpi_HPrice_w0)
kruskal.test(AllHPrice~BuySell, data = nftpi_HPrice_w0)
dunn_test(AllHPrice~BuySell, data = nftpi_HPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~ArtistAnonymousUser, data = nftpi_HPrice_w0)
dunn_test(AllHPrice~ArtistAnonymousUser, data = nftpi_HPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~BuyArtistAnonymousUser, data = nftpi_HPrice_w0)
dunn_test(AllHPrice~BuyArtistAnonymousUser, data = nftpi_HPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~Percentage, data = nftpi_HPrice_w0)
dunn_test(AllHPrice~Percentage, data = nftpi_HPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(AllHPrice~Experiment, data = nftpi_HPrice_w0)
dunn_test(AllHPrice~Experiment, data = nftpi_HPrice_w0,p.adjust.method = "bonferroni")

#Boxplot
png(file = "e1_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_e1)
dev.off()

png(file = "e2_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_e2)
dev.off()

png(file = "e3_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_e3)
dev.off()

png(file = "e4_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_e4)
dev.off()

png(file = "c_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_c)
dev.off()

png(file = "90_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_90)
dev.off()

png(file = "50_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_50)
dev.off()

png(file = "buy_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_Buy)
dev.off()

png(file = "sell_loghprice_w0.png")
boxplot(LogAllHPrice~Condition, data = nftpi_HPrice_w0_Sell)
dev.off()

#Histogram
png(file = "histogram_hprice_w0_e1.png")
hist(nftpi_HPrice_w0_e1$AllHPrice)
dev.off()
png(file = "histogram_hprice_w0_e2.png")
hist(nftpi_HPrice_w0_e2$AllHPrice)
dev.off()
png(file = "histogram_hprice_w0_e3.png")
hist(nftpi_HPrice_w0_e3$AllHPrice)
dev.off()
png(file = "histogram_hprice_w0_e4.png")
hist(nftpi_HPrice_w0_e4$AllHPrice)
dev.off()
png(file = "e1_hprice_w0.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e1)
dev.off()
png(file = "e2_hprice_w0.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e2)
dev.off()

#Regression
r_HPrice_w0_e1c <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e1c)
summary(r_HPrice_w0_e1c)
r_HPrice_w0_e2c <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e2c)
summary(r_HPrice_w0_e2c)
r_HPrice_w0_e3c <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e3c)
summary(r_HPrice_w0_e3c)
r_HPrice_w0_e4c <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e4c)
summary(r_HPrice_w0_e4c)
r_HPrice_w0_e190 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e190)
summary(r_HPrice_w0_e190)
r_HPrice_w0_e290 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e290)
summary(r_HPrice_w0_e290)
r_HPrice_w0_e390 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e390)
summary(r_HPrice_w0_e390)
r_HPrice_w0_e490 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e490)
summary(r_HPrice_w0_e490)
r_HPrice_w0_e150 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e150)
summary(r_HPrice_w0_e150)
r_HPrice_w0_e250 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e250)
summary(r_HPrice_w0_e250)
r_HPrice_w0_e350 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e350)
summary(r_HPrice_w0_e350)
r_HPrice_w0_e450 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e450)
summary(r_HPrice_w0_e450)
r_HPrice_w0_e1 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e1)
summary(r_HPrice_w0_e1)
r_HPrice_w0_e2 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e2)
summary(r_HPrice_w0_e2)
r_HPrice_w0_e3 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e3)
summary(r_HPrice_w0_e3)
r_HPrice_w0_e4 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_e4)
summary(r_HPrice_w0_e4)
r_HPrice_w0_c <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_c)
summary(r_HPrice_w0_c)
r_HPrice_w0_90 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_90)
summary(r_HPrice_w0_90)
r_HPrice_w0_50 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0_50)
summary(r_HPrice_w0_50)
r_HPrice_w0 <- lm(AllHPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_HPrice_w0)
summary(r_HPrice_w0)
r_HPrice_w0_d <- lm(AllHPrice~Age+Ethnicity+Gender+Education+Income,data=nftpi_HPrice_w0)
summary(r_HPrice_w0_d)

# BarGraph
e1 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e1, mean())
rownames(e1) <- e1[,1]
e1 <- as.matrix(e1[,-1])
lim_e1 <- 3*max(e1)
error.bar_e1 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
quartiles_e1 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e1, FUN = quantile, probs=c(0.25,0.75))
rownames(quartiles_e1) <- quartiles_e1[,1]
stdev_e1 <- as.matrix(stdev_e1[,-1]) * 1.96 / 10
summary(e1)

png(file = "e1_barplot.png")
barplot_e1 <- barplot(e1[order(e1[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey", 
                      ylim=c(0,lim_e1),ylab="Mean and sd for the highest price given in Experiment 1",
                      names.arg=c("50%","Control - 100%","90%"))
error.bar_e1(barplot_e1,e1,stdev_e1)
dev.off()
summary(nftpi_HPrice_w0_e1c$AllHPrice)
summary(nftpi_HPrice_w0_e190$AllHPrice)
summary(nftpi_HPrice_w0_e150$AllHPrice)

e2 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e2, median)
rownames(e2) <- e2[,1]
e2 <- as.matrix(e2[,-1])
lim_e2 <- 9*max(e2)
error.bar_e2 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e2 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e2, sd)
rownames(stdev_e2) <- stdev_e2[,1]
stdev_e2 <- as.matrix(stdev_e2[,-1]) * 1.96 / 10
png(file = "e2_barplot.png")
barplot_e2 <- barplot(e2[order(e2[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_e2),
                      ylab="Mean and sd for the highest price given in Experiment 2",names.arg=c("50%","Control-100%","90%"))
error.bar_e2(barplot_e2,e2,stdev_e2)
dev.off()
summary(nftpi_HPrice_w0_e2c$AllHPrice)
summary(nftpi_HPrice_w0_e290$AllHPrice)
summary(nftpi_HPrice_w0_e250$AllHPrice)

e3 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e3, median)
rownames(e3) <- e3[,1]
e3 <- as.matrix(e3[,-1])
lim_e3 <- 7*max(e3)
error.bar_e3 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e3 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e3, sd)
rownames(stdev_e3) <- stdev_e3[,1]
stdev_e3 <- as.matrix(stdev_e3[,-1]) * 1.96 / 10
png(file = "e3_barplot.png")
barplot_e3 <- barplot(e3[order(e3[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_e3),
                      ylab="Mean and sd for the highest price given in Experiment 3",names.arg=c("50%","90%","Control-100%"))
error.bar_e3(barplot_e3,e3,stdev_e3)
dev.off()
summary(nftpi_HPrice_w0_e3c$AllHPrice)
summary(nftpi_HPrice_w0_e390$AllHPrice)
summary(nftpi_HPrice_w0_e350$AllHPrice)

e4 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e4, median)
rownames(e4) <- e4[,1]
e4 <- as.matrix(e4[,-1])
lim_e4 <- 7*max(e4)
error.bar_e4 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e4 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_e4, sd)
rownames(stdev_e4) <- stdev_e4[,1]
stdev_e4 <- as.matrix(stdev_e4[,-1]) * 1.96 / 10
png(file = "e4_barplot.png")
barplot_e4 <- barplot(e4[order(e4[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_e4),
                      ylab="Mean and sd for the highest price given in Experiment 4",names.arg=c("50%","Control-100%","90%"))
error.bar_e4(barplot_e4,e4,stdev_e4)
dev.off()
summary(nftpi_HPrice_w0_e4c$AllHPrice)
summary(nftpi_HPrice_w0_e490$AllHPrice)
summary(nftpi_HPrice_w0_e450$AllHPrice)

ec <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_c, median)
rownames(ec) <- ec[,1]
ec <- as.matrix(ec[,-1])
lim_ec <- 10*max(ec)
error.bar_ec <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_ec <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_c, sd)
rownames(stdev_ec) <- stdev_ec[,1]
stdev_ec <- as.matrix(stdev_ec[,-1]) * 1.96 / 10
png(file = "ec_barplot.png")
barplot_ec <- barplot(ec[order(ec[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_ec),
                      ylab="Mean and sd for the highest price given in Control Conditions - All Experiments",names.arg=c("e1","e4","e3","e2"))
error.bar_ec(barplot_ec,ec,stdev_ec)
dev.off()
summary(nftpi_HPrice_w0_c$HPrice_E1_C)
summary(nftpi_HPrice_w0_c$HPrice_E2_C)
summary(nftpi_HPrice_w0_c$HPrice_E3_C)
summary(nftpi_HPrice_w0_c$HPrice_E4_C)

e90 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_90, median)
rownames(e90) <- e90[,1]
e90 <- as.matrix(e90[,-1])
lim_e90 <- 3*max(e90)
error.bar_e90 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e90 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_90, sd)
rownames(stdev_e90) <- stdev_e90[,1]
stdev_e90 <- as.matrix(stdev_e90[,-1]) * 1.96 / 10
png(file = "e90_barplot.png")
barplot_e90 <- barplot(e90[order(e90[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_e90),
                      ylab="Mean and sd for the highest price given in Condition 90% - All Experiments",names.arg=c("e3","e4","e1","e2"))
error.bar_e90(barplot_e90,e90,stdev_e90)
dev.off()
summary(nftpi_HPrice_w0_90$HPrice_E1_90)
summary(nftpi_HPrice_w0_90$HPrice_E2_90)
summary(nftpi_HPrice_w0_90$HPrice_E3_90)
summary(nftpi_HPrice_w0_90$HPrice_E4_90)

e50 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_50, median)
rownames(e50) <- e50[,1]
e50 <- as.matrix(e50[,-1])
lim_e50 <- 3*max(e50)
error.bar_e50 <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_e50 <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0_50, sd)
rownames(stdev_e50) <- stdev_e50[,1]
stdev_e50 <- as.matrix(stdev_e50[,-1]) * 1.96 / 10
png(file = "e50_barplot.png")
barplot_e50 <- barplot(e50[order(e50[,1],decreasing = FALSE),], beside=T , legend.text=T,col="grey",ylim=c(0,lim_e50),
                       ylab="Mean and sd for the highest price given in Condition 50% - All Experiments",names.arg=c("e1","e3","e4","e2"))
error.bar_e50(barplot_e50,e50,stdev_e50)
dev.off()
summary(nftpi_HPrice_w0_50$HPrice_E1_50)
summary(nftpi_HPrice_w0_50$HPrice_E2_50)
summary(nftpi_HPrice_w0_50$HPrice_E3_50)
summary(nftpi_HPrice_w0_50$HPrice_E4_50)

#Not working and I don't know why
BuySell <- aggregate(AllHPrice~BuySell, data=nftpi_HPrice_w0, median)
rownames(BuySell) <- BuySell[,1]
BuySell <- as.matrix(BuySell[,-1])
lim_BuySell <- 7*max(BuySell)
error.bar_BuySell <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}
stdev_BuySell <- aggregate(AllHPrice~Condition, data=nftpi_HPrice_w0, sd)
rownames(stdev_BuySell) <- stdev_BuySell[,1]
stdev_BuySell <- as.matrix(stdev_BuySell[,-1]) * 1.96 / 10
png(file = "BuySell_barplot.png")
barplot_BuySell <- barplot(BuySell, beside=T,legend.text=T,col="grey",
                           ylab="Mean and sd for the highest price given in Buying and Selling Conditions")
error.bar_BuySell(barplot_BuySell,BuySell,stdev_BuySell)
dev.off()
summary(nftpi_HPrice_w0_e4c$AllHPrice)
summary(nftpi_HPrice_w0_e490$AllHPrice)
summary(nftpi_HPrice_w0_e450$AllHPrice)

#Boxplot
png(file = "e1_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e1, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "e2_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e2, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "e3_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e3, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "e4_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_e4, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "ec_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_c, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "e90_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_90, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()

png(file = "e50_HighestPrice_boxplot.png")
boxplot(AllHPrice~Condition, data = nftpi_HPrice_w0_50, ylab = "Highest Price", xlab = "Condition",main="Highest NFT Price per Condition")
dev.off()
####################################################################################################################################################
########################################################### Favorite Price Ilicitation ##############################################################
####################################################################################################################################################
#FPrice without 0
nftpi_FPrice_w0 <- nftpi[nftpi$FPrice>0,]
table(nftpi_FPrice_w0$FPrice)

#Subsetting data
nftpi_FPrice_w0_e1c <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_c",]
nftpi_FPrice_w0_e190 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_90",]
nftpi_FPrice_w0_e150 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_50",]
nftpi_FPrice_w0_e2c <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e2_c",]
nftpi_FPrice_w0_e290 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e2_90",]
nftpi_FPrice_w0_e250 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e2_50",]
nftpi_FPrice_w0_e3c <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e3_c",]
nftpi_FPrice_w0_e390 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e3_90",]
nftpi_FPrice_w0_e350 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e3_50",]
nftpi_FPrice_w0_e4c <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e4_c",]
nftpi_FPrice_w0_e490 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e4_90",]
nftpi_FPrice_w0_e450 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e4_50",]
nftpi_FPrice_w0_e1 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_c" | nftpi_FPrice_w0$Condition=="e1_90" | nftpi_FPrice_w0$Condition=="e1_50",]
nftpi_FPrice_w0_e2 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e2_c" | nftpi_FPrice_w0$Condition=="e2_90" | nftpi_FPrice_w0$Condition=="e2_50",]
nftpi_FPrice_w0_e3 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e3_c" | nftpi_FPrice_w0$Condition=="e3_90" | nftpi_FPrice_w0$Condition=="e3_50",]
nftpi_FPrice_w0_e4 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e4_c" | nftpi_FPrice_w0$Condition=="e4_90" | nftpi_FPrice_w0$Condition=="e4_50",]
nftpi_FPrice_w0_c <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_c" | nftpi_FPrice_w0$Condition=="e2_c" | nftpi_FPrice_w0$Condition=="e3_c"
                                     | nftpi_FPrice_w0$Condition=="e4_c",]
nftpi_FPrice_w0_90 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_90" | nftpi_FPrice_w0$Condition=="e2_90" | nftpi_FPrice_w0$Condition=="e3_90"
                                      | nftpi_FPrice_w0$Condition=="e4_90",]
nftpi_FPrice_w0_50 <- nftpi_FPrice_w0[nftpi_FPrice_w0$Condition=="e1_50" | nftpi_FPrice_w0$Condition=="e2_50" | nftpi_FPrice_w0$Condition=="e3_50"
                                      | nftpi_FPrice_w0$Condition=="e4_50",]

#Kruskal Test
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0)
post_hoc_FPrice_w0 <- dunn_test(FPrice~Condition, data = nftpi_FPrice_w0, p.adjust.method = "bonferroni")
write.table(post_hoc_FPrice_w0, file = "post_hoc_FPrice_w0.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0_e1)
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0_e2)
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0_e3)
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0_e4)
kruskal.test(FPrice~Condition, data = nftpi_FPrice_w0_c)
dunn_test(FPrice~Condition, data = nftpi_FPrice_w0_c, p.adjust.method = "bonferroni")
kruskal.test(FPrice~Condition, data= nftpi_FPrice_w0_90)
dunn_test(FPrice~Condition, data= nftpi_FPrice_w0_90,p.adjust.method = "bonferroni")
kruskal.test(FPrice~Condition, data=nftpi_FPrice_w0_50)
dunn_test(FPrice~Condition, data= nftpi_FPrice_w0_50,p.adjust.method = "bonferroni")
kruskal.test(FPrice~BuySell, data = nftpi_FPrice_w0)
dunn_test(FPrice~BuySell, data = nftpi_FPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(FPrice~ArtistAnonymousUser, data = nftpi_FPrice_w0)
dunn_test(FPrice~ArtistAnonymousUser, data = nftpi_FPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(FPrice~BuyArtistAnonymousUser, data = nftpi_FPrice_w0)
dunn_test(FPrice~BuyArtistAnonymousUser, data = nftpi_FPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(FPrice~Percentage, data = nftpi_FPrice_w0)
dunn_test(FPrice~Percentage, data = nftpi_FPrice_w0,p.adjust.method = "bonferroni")
kruskal.test(FPrice~Experiment, data = nftpi_FPrice_w0)
dunn_test(FPrice~Experiment, data = nftpi_FPrice_w0,p.adjust.method = "bonferroni")

#Regression
r_FPrice_w0_e1c <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e1c)
summary(r_FPrice_w0_e1c)
r_FPrice_w0_e2c <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e2c)
summary(r_FPrice_w0_e2c)
r_FPrice_w0_e3c <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e3c)
summary(r_FPrice_w0_e3c)
r_FPrice_w0_e4c <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e4c)
summary(r_FPrice_w0_e4c)
r_FPrice_w0_e190 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e190)
summary(r_FPrice_w0_e190)
r_FPrice_w0_e290 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e290)
summary(r_FPrice_w0_e290)
r_FPrice_w0_e390 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e390)
summary(r_FPrice_w0_e390)
r_FPrice_w0_e490 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e490)
summary(r_FPrice_w0_e490)
r_FPrice_w0_e150 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e150)
summary(r_FPrice_w0_e150)
r_FPrice_w0_e250 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e250)
summary(r_FPrice_w0_e250)
r_FPrice_w0_e350 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e350)
summary(r_FPrice_w0_e350)
r_FPrice_w0_e450 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e450)
summary(r_FPrice_w0_e450)
r_FPrice_w0_e1 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e1)
summary(r_FPrice_w0_e1)
r_FPrice_w0_e2 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e2)
summary(r_FPrice_w0_e2)
r_FPrice_w0_e3 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e3)
summary(r_FPrice_w0_e3)
r_FPrice_w0_e4 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_e4)
summary(r_FPrice_w0_e4)
r_FPrice_w0_c <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_c)
summary(r_FPrice_w0_c)
r_FPrice_w0_90 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_90)
summary(r_FPrice_w0_90)
r_FPrice_w0_50 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0_50)
summary(r_FPrice_w0_50)
r_FPrice_w0 <- lm(FPrice~Like_1+Like_2+Like_3+Like_4+LikeNFT_1+Owner+Proud_1+Partner+SellFor,data=nftpi_FPrice_w0)
summary(r_FPrice_w0)
r_FPrice_w0_d <- lm(FPrice~Age+Ethnicity+Gender+Education+Income,data=nftpi_FPrice_w0)
summary(r_FPrice_w0_d)

############################################################################################################################################
############################################################################################################################################
#################################### Analysis I am not using ###############################################################################
############################################################################################################################################
############################################################################################################################################
#Outliers
png(file = "all_data.png")
boxplot(AllAverages~Condition, data = nftpi, ylab = "Average Price", xlab = "Condition",main="Average NFT Price per Condition")
dev.off()

png(file = "all_data_fprice.png")
boxplot(FPrice~Condition, data = nftpi, ylab = "Ilicitation Price", xlab = "Condition",main="Ilicitation NFT Price per Condition")
dev.off()

nftpi %>%
  group_by(Condition) %>%
  identify_outliers(AllAverages)

nftpi %>%
  group_by(Condition) %>%
  identify_outliers(FPrice)

#Dominance Analyisis Highest Price > 100 
da<-dominanceAnalysis(r_HPrice_w0_e1c)
print(da)
dominanceBriefing(da)
averageContribution(da)
plot(da,which.graph='complete')

#Higher than 100
nftpi_HPrice_100 <- nftpi[nftpi$AllHPrice>=100,]
table(nftpi_HPrice_100$AllHPrice)
png(file = "histogram_hprice_100.png")
hist(nftpi_HPrice_100$AllHPrice)
dev.off()
nftpi_HPrice_100_e1c <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e1_c",]
nftpi_HPrice_100_e190 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e1_90",]
nftpi_HPrice_100_e150 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e1_50",]
nftpi_HPrice_100_e2c <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e2_c",]
nftpi_HPrice_100_e290 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e2_90",]
nftpi_HPrice_100_e250 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e2_50",]
nftpi_HPrice_100_e3c <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e3_c",]
nftpi_HPrice_100_e390 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e3_90",]
nftpi_HPrice_100_e350 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e3_50",]
nftpi_HPrice_100_e4c <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e4_c",]
nftpi_HPrice_100_e490 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e4_90",]
nftpi_HPrice_100_e450 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e4_50",]
nftpi_HPrice_100_e1 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e1_c" | nftpi_HPrice_100$Condition=="e1_90" | nftpi_HPrice_100$Condition=="e1_50",]
nftpi_HPrice_100_e2 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e2_c" | nftpi_HPrice_100$Condition=="e2_90" | nftpi_HPrice_100$Condition=="e2_50",]
nftpi_HPrice_100_e3 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e3_c" | nftpi_HPrice_100$Condition=="e3_90" | nftpi_HPrice_100$Condition=="e3_50",]
nftpi_HPrice_100_e4 <- nftpi_HPrice_100[nftpi_HPrice_100$Condition=="e4_c" | nftpi_HPrice_100$Condition=="e4_90" | nftpi_HPrice_100$Condition=="e4_50",]
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_100_e1)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_100_e2)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_100_e3)
kruskal.test(AllHPrice~Condition, data = nftpi_HPrice_100_e4)


#Analysis Tali Experiment 1
explanatory =  c("Age", "Income", "Ethnicity", "Gender", "Education")
dependent = "Condition"
nftpi_e1 %>% 
  summary_factorlist(dependent, explanatory, na_include=TRUE)
table_Experiment1 <- nftpi_e1 %>%  
  summary_factorlist(dependent, explanatory, na_include=TRUE, 
                     add_dependent_label=TRUE,
                     dependent_label_prefix = "Demographic of participants Experiment 1 per condition: "
  )
table_Experiment1

write.table(table_Experiment1, file = "demographics_Experiment1.txt", sep = ",", quote = FALSE, row.names = F)

explanatory =  c("SellFor", "AllAverages", "FPrice")
dependent = "Condition"
nftpi_e1 %>% 
  summary_factorlist(dependent, explanatory, na_include=TRUE)
table_Experiment1_outcomes <- nftpi_e1 %>%  
  summary_factorlist(dependent, explanatory, na_include=TRUE, 
                     add_dependent_label=TRUE,
                     dependent_label_prefix = "Outcome Variables Experiment 1 per condition: "
  )
table_Experiment1_outcomes

write.table(table_Experiment1_outcomes, file = "Outcome_Experiment1.txt", sep = ",", quote = FALSE, row.names = F)

png(file = "e1_SellFor_FPrice_AllAverages.png")
par(mfrow=c(1,3))
boxplot(SellFor ~ Condition, data = nftpi_e1)
boxplot(AllAverages ~ Condition, data = nftpi_e1)
boxplot(FPrice ~ Condition, data = nftpi_e1)
dev.off()

kruskal.test(AllAverages~Condition, data = nftpi_e1)
post_hoc_e1 <- dunn_test(AllAverages~Condition, data = nftpi_e1, p.adjust.method = "bonferroni")
write.table(post_hoc_e1, file = "post_hoc_e1.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(FPrice~Condition,data = nftpi_e1)
post_hoc_e1_fprice <- dunn_test(FPrice~Condition, data = nftpi_e1, p.adjust.method = "bonferroni")
write.table(post_hoc_e1_fprice, file = "post_hoc_e1_fprice.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(SellFor~Condition, data = nftpi_e1)
post_hoc_e1_sellfor <- dunn_test(SellFor~Condition, data = nftpi_e1, p.adjust.method = "bonferroni")
write.table(post_hoc_e1_sellfor, file = "post_hoc_e1_sellfor.txt", sep = ",", quote = FALSE, row.names = F)

r <- lm(AllAverages~ Condition, 
             data=nftpi)
summary(r)

r_e1_a <- lm(AllAverages~ Condition, 
           data=nftpi_e1)
summary(r_e1_a)

r_e1_b <- lm(AllAverages~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e1)
summary(r_e1_b)

r_e1_c <- lm(AllAverages~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e1)
summary(r_e1_c)

r_e1_d <- lm(FPrice~ Condition, 
             data=nftpi_e1)
summary(r_e1_d)

r_e1_e <- lm(FPrice~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e1)
summary(r_e1_e)

r_e1_f <- lm(FPrice~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e1)
summary(r_e1_f)

r_e1_g <- lm(SellFor~ Condition, 
             data=nftpi_e1)
summary(r_e1_g)

r_e1_h <- lm(SellFor~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e1)
summary(r_e1_h)

r_e1_i <- lm(SellFor~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e1)
summary(r_e1_i)

#Analysis Tali Experiment 2
explanatory =  c("Age", "Income", "Ethnicity", "Gender", "Education")
dependent = "Condition"
nftpi_e2 %>% 
  summary_factorlist(dependent, explanatory, na_include=TRUE)
table_Experiment2 <- nftpi_e2 %>%  
  summary_factorlist(dependent, explanatory, na_include=TRUE, 
                     add_dependent_label=TRUE,
                     dependent_label_prefix = "Demographic of participants Experiment 2 per condition: "
  )
table_Experiment2

write.table(table_Experiment2, file = "demographics_Experiment2.txt", sep = ",", quote = FALSE, row.names = F)

explanatory =  c("SellFor", "AllAverages", "FPrice")
dependent = "Condition"
nftpi_e2 %>% 
  summary_factorlist(dependent, explanatory, na_include=TRUE)
table_Experiment2_outcomes <- nftpi_e2 %>%  
  summary_factorlist(dependent, explanatory, na_include=TRUE, 
                     add_dependent_label=TRUE,
                     dependent_label_prefix = "Outcome Variables Experiment 2 per condition: "
  )
table_Experiment2_outcomes

write.table(table_Experiment2_outcomes, file = "Outcome_Experiment2.txt", sep = ",", quote = FALSE, row.names = F)

png(file = "e2_SellFor_FPrice_AllAverages.png")
par(mfrow=c(1,3))
boxplot(SellFor ~ Condition, data = nftpi_e2)
boxplot(AllAverages ~ Condition, data = nftpi_e2)
boxplot(FPrice ~ Condition, data = nftpi_e2)
dev.off()

kruskal.test(AllAverages~Condition, data = nftpi_e2)
post_hoc_e2 <- dunn_test(AllAverages~Condition, data = nftpi_e2, p.adjust.method = "bonferroni")
write.table(post_hoc_e2, file = "post_hoc_e2.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(FPrice~Condition,data = nftpi_e2)
post_hoc_e2_fprice <- dunn_test(FPrice~Condition, data = nftpi_e2, p.adjust.method = "bonferroni")
write.table(post_hoc_e2_fprice, file = "post_hoc_e2_fprice.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(SellFor~Condition, data = nftpi_e2)
post_hoc_e2_sellfor <- dunn_test(SellFor~Condition, data = nftpi_e2, p.adjust.method = "bonferroni")
write.table(post_hoc_e2_sellfor, file = "post_hoc_e2_sellfor.txt", sep = ",", quote = FALSE, row.names = F)

mean(nftpi_e2$AllAverages)
summary(nftpi_e2$AllAverages)
summary(nftpi_e2$AveragePrice_50)
summary(nftpi_e2$AveragePrice_90)
summary(nftpi_e2$AveragePrice_C)
r_e2_a <- lm(AllAverages~ Condition, 
             data=nftpi_e2)
summary(r_e2_a)

r_e2_b <- lm(AllAverages~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e2)
summary(r_e2_b)

r_e2_c <- lm(AllAverages~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e2)
summary(r_e2_c)

r_e2_d <- lm(FPrice~ Condition, 
             data=nftpi_e2)
summary(r_e2_d)

r_e2_e <- lm(FPrice~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e2)
summary(r_e2_e)

r_e2_f <- lm(FPrice~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e2)
summary(r_e2_f)

r_e2_g <- lm(SellFor~ Condition, 
             data=nftpi_e2)
summary(r_e2_g)

r_e2_h <- lm(SellFor~ Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e2)
summary(r_e2_h)

r_e2_i <- lm(SellFor~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner, 
             data=nftpi_e2)
summary(r_e2_i)

#With outliers
#Non parametric tests all data
kruskal.test(AllAverages~Condition, data = nftpi)
post_hoc_all <- dunn_test(AllAverages~Condition, data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_all, file = "post_hoc_all.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(FPrice~Condition,data = nftpi)
post_hoc_all_fprice <- dunn_test(FPrice~Condition, data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_all_fprice, file = "post_hoc_all_fprice.txt", sep = ",", quote = FALSE, row.names = F)
kruskal.test(SellFor~Condition, data = nftpi)
post_hoc_all_sellfor <- dunn_test(SellFor~Condition, data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_all_sellfor, file = "post_hoc_all_sellfor.txt", sep = ",", quote = FALSE, row.names = F)

#Non parametric tests only experiments buying
kruskal.test(AllAverages~Condition, data = nftpi_buy)
kruskal.test(FPrice~Condition,data = nftpi_buy)
kruskal.test(SellFor~Condition,data = nftpi_buy)

kruskal.test(AllAverages~Condition, data = nftpi_3_4)
kruskal.test(FPrice~Condition,data = nftpi_3_4)
kruskal.test(SellFor~Condition,data = nftpi_3_4)

kruskal.test(AllAverages~Condition, data = nftpi_50)
kruskal.test(FPrice~Condition,data = nftpi_50)
kruskal.test(SellFor~Condition,data = nftpi_50)
dunn_test(AllAverages~Condition, data = nftpi_50, p.adjust.method = "bonferroni")
dunn_test(FPrice~Condition, data = nftpi_50, p.adjust.method = "bonferroni")
dunn_test(SellFor~Condition, data = nftpi_50, p.adjust.method = "bonferroni")

kruskal.test(AllAverages~Condition, data = nftpi_90)
kruskal.test(FPrice~Condition,data = nftpi_90)
kruskal.test(SellFor~Condition,data = nftpi_90)
dunn_test(AllAverages~Condition, data = nftpi_90, p.adjust.method = "bonferroni")
dunn_test(FPrice~Condition, data = nftpi_90, p.adjust.method = "bonferroni")

kruskal.test(AllAverages~Condition, data = nftpi_c)
kruskal.test(FPrice~Condition,data = nftpi_c)
kruskal.test(SellFor~Condition,data = nftpi_c)
dunn_test(AllAverages~Condition, data = nftpi_c, p.adjust.method = "bonferroni")
dunn_test(FPrice~Condition, data = nftpi_c, p.adjust.method = "bonferroni")
dunn_test(SellFor~Condition, data = nftpi_c, p.adjust.method = "bonferroni")

kruskal.test(AllAverages~Condition, data = nftpi_e1)
kruskal.test(FPrice~Condition,data = nftpi_e1)
kruskal.test(SellFor~Condition,data = nftpi_e1)

kruskal.test(AllAverages~Condition, data = nftpi_e2)
dunn_test(AllAverages~Condition, data = nftpi_e2)
kruskal.test(FPrice~Condition,data = nftpi_e2)
kruskal.test(SellFor~Condition,data = nftpi_e2)

kruskal.test(AllAverages~Condition, data = nftpi_e3)
kruskal.test(FPrice~Condition, data = nftpi_e3)
kruskal.test(SellFor~Condition, data = nftpi_e3)

kruskal.test(AllAverages~Condition, data = nftpi_e4)
kruskal.test(FPrice~Condition, data = nftpi_e4)
kruskal.test(SellFor~Condition, data = nftpi_e4)

#Investigating Relationship with the artist
kruskal.test(Proud_1~Condition, data = nftpi)

kruskal.test(Like_1~Condition, data = nftpi)

kruskal.test(Like_2~Condition, data = nftpi)

kruskal.test(Like_3~Condition, data = nftpi)

kruskal.test(Like_4~Condition, data = nftpi)

kruskal.test(Owner~Condition, data = nftpi)
post_hoc_owner <- dunn_test(Owner~Condition,data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_owner, file = "post_hoc_owner.txt", sep = ",", quote = FALSE, row.names = F)
#The differences was only between Selling and Buying
#Can I use this test for binary data?

kruskal.test(Partner~Condition, data = nftpi)
post_hoc_partner <- dunn_test(Partner~Condition,data = nftpi, p.adjust.method = "bonferroni")
write.table(post_hoc_partner, file = "post_hoc_partner.txt", sep = ",", quote = FALSE, row.names = F)
mean(nftpi_e1$Partner)
#Can I use this test for binary data?

# Descriptive Analysis
descriptive_all_data <- nftpi %>% group_by(Condition) %>% 
  get_summary_stats(AllAverages, FPrice, SellFor, type = "median_iqr")
write.table(descriptive_all_data, file = "descriptive_all_data.txt", sep = ",", quote = FALSE, row.names = F)

#Correlations
cor(nftpi$LikeNFT_1,nftpi$Proud_1, method = "pearson", use = "complete.obs")
cor(nftpi$Like_1,nftpi$Proud_1, method = "pearson", use = "complete.obs")
cor(nftpi$Like_2,nftpi$Proud_1, method = "pearson", use = "complete.obs")
cor(nftpi$Like_3,nftpi$Proud_1, method = "pearson", use = "complete.obs")
cor(nftpi$Like_4,nftpi$Proud_1, method = "pearson", use = "complete.obs")

table(nftpi_e1$Owner)
table(nftpi_e2$Owner)
table(nftpi_e3$Owner)
table(nftpi_e4$Owner)

nftpi$Proud <- nftpi$Proud_1[Proud_1=="Yes"]

descriptive_mediators <- nftpi %>% group_by(Condition) %>% 
  get_summary_stats(LikeNFT_1, Like_1, Like_2, Like_3, Like_4, Proud, Owner_, type = "median_iqr")
write.table(descriptive_all_data, file = "descriptive_mediators.txt", sep = ",", quote = FALSE, row.names = F)

table(nftpi_e1$Partner)
table(nftpi_e2$Partner)
table(nftpi_e3$Partner)
table(nftpi_e4$Partner)

#Regression
r_sellfor <- lm(SellFor~LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+Condition,data = nftpi)
summary(r_sellfor)

r_sellfor_e1 <- lm(SellFor~LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+Condition,data = nftpi_e1)
summary(r_sellfor_e1)

r_sellfor_e2 <- lm(SellFor~LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+Condition,data = nftpi_e2)
summary(r_sellfor_e2)

r_sellfor_e3 <- lm(SellFor~LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+Condition,data = nftpi_e3)
summary(r_sellfor_e3)

r_sellfor_e4 <- lm(SellFor~LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+Condition,data = nftpi_e4)
summary(r_sellfor_e4)

#Taking out outliers using this technique https://universeofdatascience.com/how-to-remove-outliers-from-data-in-r/#:~:text=Firstly%2C%20we%20find%20first%20(Q1,()%20function%20to%20remove%20outliers.
#Creating the boxplot
q_e1 <- quantile(nftpi_e1$AllAverages)
iqr_e1 <- IQR(nftpi_e1$AllAverages)
l_iqr_e1 <- q_e1[2] - 1.5*iqr_e1
u_iqr_e1 <- q_e1[4] + 1.5*iqr_e1
nftpi_e1_nooutlier <- nftpi_e1[nftpi_e1$AllAverages>l_iqr_e1 & nftpi_e1$AllAverages<u_iqr_e1,]
nftpi_e1_nooutlier %>%
  group_by(Condition) %>%
  identify_outliers(AllAverages)
#View(nftpi_e1_nooutlier)

png(file = "e1.png")
boxplot(AllAverages ~ Condition, data=nftpi_e1_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_e2 <- quantile(nftpi_e2$AllAverages)
iqr_e2 <- IQR(nftpi_e2$AllAverages)
l_iqr_e2 <- q_e2[2] - 1.5*iqr_e2
u_iqr_e2 <- q_e2[4] + 1.5*iqr_e2
nftpi_e2_nooutlier <- nftpi_e2[nftpi_e2$AllAverages>l_iqr_e2 & nftpi_e2$AllAverages<u_iqr_e2,]
summary(nftpi_e2_nooutlier$AllAverages)
nrow(nftpi_e2_nooutlier)

png(file = "e2.png")
boxplot(AllAverages ~ Condition, data=nftpi_e2_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_e3 <- quantile(nftpi_e3$AllAverages)
iqr_e3 <- IQR(nftpi_e3$AllAverages)
l_iqr_e3 <- q_e3[2] - 1.5*iqr_e3
u_iqr_e3 <- q_e3[4] + 1.5*iqr_e3
nftpi_e3_nooutlier <- nftpi_e3[nftpi_e3$AllAverages>l_iqr_e3 & nftpi_e3$AllAverages<u_iqr_e3,]
summary(nftpi_e3_nooutlier$AllAverages)
nrow(nftpi_e3_nooutlier)

png(file = "e3.png")
boxplot(AllAverages ~ Condition, data=nftpi_e3_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_e4 <- quantile(nftpi_e4$AllAverages)
iqr_e4 <- IQR(nftpi_e4$AllAverages)
l_iqr_e4 <- q_e4[2] - 1.5*iqr_e4
u_iqr_e4 <- q_e4[4] + 1.5*iqr_e4
nftpi_e4_nooutlier <- nftpi_e4[nftpi_e4$AllAverages>l_iqr_e4 & nftpi_e4$AllAverages<u_iqr_e4,]
summary(nftpi_e4_nooutlier$AllAverages)
nrow(nftpi_e4_nooutlier)

png(file = "e4.png")
boxplot(AllAverages ~ Condition, data=nftpi_e4_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_c <- quantile(nftpi_c$AllAverages)
iqr_c <- IQR(nftpi_c$AllAverages)
l_iqr_c <- q_c[2] - 1.5*iqr_c
u_iqr_c <- q_c[4] + 1.5*iqr_c
nftpi_c_nooutlier <- nftpi_c[nftpi_c$AllAverages>l_iqr_c & nftpi_c$AllAverages<u_iqr_c,]
summary(nftpi_c_nooutlier$AllAverages)
nrow(nftpi_c_nooutlier)

png(file = "c.png")
boxplot(AllAverages ~ Condition, data=nftpi_c_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_90 <- quantile(nftpi_90$AllAverages)
iqr_90 <- IQR(nftpi_90$AllAverages)
l_iqr_90 <- q_90[2] - 1.5*iqr_90
u_iqr_90 <- q_90[4] + 1.5*iqr_90
nftpi_90_nooutlier <- nftpi_90[nftpi_90$AllAverages>l_iqr_90 & nftpi_90$AllAverages<u_iqr_90,]
summary(nftpi_90_nooutlier$AllAverages)
nrow(nftpi_90_nooutlier)

png(file = "90.png")
boxplot(AllAverages ~ Condition, data=nftpi_90_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

q_50 <- quantile(nftpi_50$AllAverages)
iqr_50 <- IQR(nftpi_50$AllAverages)
l_iqr_50 <- q_50[2] - 1.5*iqr_50
u_iqr_50 <- q_50[4] + 1.5*iqr_50
nftpi_50_nooutlier <- nftpi_50[nftpi_50$AllAverages>l_iqr_50 & nftpi_50$AllAverages<u_iqr_50,]
summary(nftpi_50_nooutlier$AllAverages)
nrow(nftpi_50_nooutlier)

png(file = "50.png")
boxplot(AllAverages ~ Condition, data=nftpi_50_nooutlier,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price per Condition - No Outlier")
dev.off()

#Creating percentages of the Control - 90 and 50
nftpi_c_nooutlier$AllAverages100_90 <- nftpi_c_nooutlier$AllAverages*0.9
#View(nftpi_c_nooutlier)
nftpi_90_nooutlier$AllAverages100_90 <- rowSums(nftpi_90_nooutlier[,c("AveragePrice_E1_90","AveragePrice_E2_90","AveragePrice_E3_90",
                                                                      "AveragePrice_E4_90")],na.rm = TRUE)
nftpi_c_nooutlier_merge100_90 <- rbind(nftpi_c_nooutlier, nftpi_90_nooutlier)
#view(nftpi_c_nooutlier_merge100_90)

png(file = "90C_90.png")
boxplot(AllAverages100_90~Condition,data=nftpi_c_nooutlier_merge100_90,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price Control 90% - No Outlier")
dev.off()

nftpi_c_nooutlier$AllAverages100_50 <- nftpi_c_nooutlier$AllAverages*0.5
nftpi_50_nooutlier$AllAverages100_50 <- rowSums(nftpi_50_nooutlier[,c("AveragePrice_E1_50", "AveragePrice_E2_50", "AveragePrice_E3_50", 
                                                                      "AveragePrice_E4_50")],na.rm=TRUE)
nftpi_50_nooutlier$AllAverages100_90 <- NA
nftpi_c_nooutlier_merge100_50 <- rbind(nftpi_c_nooutlier, nftpi_50_nooutlier)

png(file = "50C_50.png")
boxplot(AllAverages100_50~Condition,data=nftpi_c_nooutlier_merge100_50,xlab = "Condition",ylab = "NFT Price Average", main="Average NFT Price Control 50% - No Outlier")
dev.off()

#subsetting sell X buy
nfti_buy_nooutlier <- rbind(nftpi_e1_nooutlier,nftpi_e3_nooutlier, nftpi_e4_nooutlier)
#View(nfti_buy)
png(file = "Buy_All.png")
boxplot(AllAverages~Condition,data=nfti_buy_nooutlier,xlab = "Condition",ylab = "NFT Price Average for Buying Condition", main="Average NFT Price for Buying Conditions - No Outlier")
dev.off()

nfti_3_4_nooutlier <- rbind(nftpi_e3_nooutlier, nftpi_e4_nooutlier)
png(file = "Buy_From Anonymous User.png")
boxplot(AllAverages~Condition,data=nfti_3_4_nooutlier,xlab = "Condition",ylab = "NFT Price Average for Buying Condition", main="Average NFT Price for Buying Conditions From Anonymous User - No Outlier")
dev.off()

png(file = "Buy_From Anonymous User_50.png")
boxplot(AveragePrice_50_Buy~Condition,data=nfti_3_4_nooutlier,xlab = "Condition",ylab = "NFT Price Average for Buying Condition", main="Average NFT Price for Buying Conditions From Anonymous User - No Outlier")
dev.off()

png(file = "Buy_From Anonymous User_90.png")
boxplot(AveragePrice_90_Buy~Condition,data=nfti_3_4_nooutlier,xlab = "Condition",ylab = "NFT Price Average for Buying Condition", main="Average NFT Price for Buying Conditions From Anonymous User - No Outlier")
dev.off()

png(file = "Buy_From Anonymous User_C.png")
boxplot(AveragePrice_C_Buy~Condition,data=nfti_3_4_nooutlier,xlab = "Condition",ylab = "NFT Price Average for Buying Condition", main="Average NFT Price for Buying Conditions From Anonymous User - No Outlier")
dev.off()

#Regression Analysis all
r_e1_c <- lm(AveragePrice_E1_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
           data=nftpi_e1_nooutlier)
summary(r_e1_c)

r_e2_c <- lm(AveragePrice_E2_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
           data=nftpi_e2_nooutlier)
summary(r_e2_c)

r_e3_c <- lm(AveragePrice_E3_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e3_nooutlier)
summary(r_e3_c)

r_e4_c <- lm(AveragePrice_E4_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e4_nooutlier)
summary(r_e4_c)

r_e1_90 <- lm(AveragePrice_E1_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e1_nooutlier)
summary(r_e1_90)

r_e2_90 <- lm(AveragePrice_E2_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e2_nooutlier)
summary(r_e2_90)

r_e3_90 <- lm(AveragePrice_E3_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e3_nooutlier)
summary(r_e3_90)

r_e4_90 <- lm(AveragePrice_E4_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
             data=nftpi_e4_nooutlier)
summary(r_e4_90)

r_e1_50 <- lm(AveragePrice_E1_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
              data=nftpi_e1_nooutlier)
summary(r_e1_50)

r_e2_50 <- lm(AveragePrice_E2_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
              data=nftpi_e2_nooutlier)
summary(r_e2_50)

r_e3_50 <- lm(AveragePrice_E3_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
              data=nftpi_e3_nooutlier)
summary(r_e3_50)

r_e4_50 <- lm(AveragePrice_E4_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor+Age+Ethnicity+Gender+Education+Income, 
              data=nftpi_e4_nooutlier)
summary(r_e4_50)

# Regression Artist Relashionship
r_e1_c_a <- lm(AveragePrice_E1_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor, 
             data=nftpi_e1_nooutlier)
summary(r_e1_c_a)

r_e2_c_a <- lm(AveragePrice_E2_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
             data=nftpi_e2_nooutlier)
summary(r_e2_c_a)

r_e3_c_a <- lm(AveragePrice_E3_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
             data=nftpi_e3_nooutlier)
summary(r_e3_c_a)

r_e4_c_a <- lm(AveragePrice_E4_C~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
             data=nftpi_e4_nooutlier)
summary(r_e4_c_a)

r_e1_90_a <- lm(AveragePrice_E1_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor, 
              data=nftpi_e1_nooutlier)
summary(r_e1_90_a)

r_e2_90_a <- lm(AveragePrice_E2_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e2_nooutlier)
summary(r_e2_90_a)

r_e3_90_a <- lm(AveragePrice_E3_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e3_nooutlier)
summary(r_e3_90_a)

r_e4_90_a <- lm(AveragePrice_E4_90~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e4_nooutlier)
summary(r_e4_90_a)

r_e1_50_a <- lm(AveragePrice_E1_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e1_nooutlier)
summary(r_e1_50_a)

r_e2_50_a <- lm(AveragePrice_E2_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e2_nooutlier)
summary(r_e2_50_a)

r_e3_50_a <- lm(AveragePrice_E3_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e3_nooutlier)
summary(r_e3_50_a)

r_e4_50_a <- lm(AveragePrice_E4_50~ LikeNFT_1+Proud_1+Like_1+Like_2+Like_3+Like_4+Owner+Partner+SellFor,
              data=nftpi_e4_nooutlier)
summary(r_e4_50_a)

#Boxplot ilicitation
nftpi$FPrice <- as.numeric(nftpi$FPrice)
png(file = "All_i.png")
boxplot(FPrice ~ Condition, data=nftpi,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition")
dev.off()

png(file = "e1_i.png")
boxplot(FPrice ~ Condition, data=nftpi_e1,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Experiment 1")
dev.off()

png(file = "e2_i.png")
boxplot(FPrice ~ Condition, data=nftpi_e2,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Experiment 2")
dev.off()

png(file = "e3_i.png")
boxplot(FPrice ~ Condition, data=nftpi_e3,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Experiment 3")
dev.off()

png(file = "e4_i.png")
boxplot(FPrice ~ Condition, data=nftpi_e4,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Experiment 4")
dev.off()

png(file = "C_i.png")
boxplot(FPrice ~ Condition, data=nftpi_c,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Control")
dev.off()

png(file = "90_i.png")
boxplot(FPrice ~ Condition, data=nftpi_90,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - 90%")
dev.off()

png(file = "50_i.png")
boxplot(FPrice ~ Condition, data=nftpi_50,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - 50%")
dev.off()

png(file = "Buy_i.png")
boxplot(FPrice ~ Condition, data=nftpi_buy ,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Buying Conditions")
dev.off()

png(file = "3_4_i.png")
boxplot(FPrice ~ Condition, data=nftpi_3_4 ,xlab = "Condition",ylab = "NFT Price Ilicitation", main="NFT Price Ilicitation per condition - Buying Experiment 3 and 4")
dev.off()

#Without outliers
kruskal.test(AllAverages~Condition, data = nftpi_c_nooutlier)
kruskal.test(FPrice~Condition,data = nftpi_c_nooutlier)
dunn_test(AllAverages~Condition,data = nftpi_c_nooutlier)
dunn_test(FPrice~Condition,data = nftpi_c_nooutlier)

kruskal.test(AllAverages~Condition, data = nftpi_90_nooutlier)
kruskal.test(FPrice~Condition,data = nftpi_90_nooutlier)
dunn_test(AllAverages~Condition,data = nftpi_90_nooutlier)
dunn_test(FPrice~Condition,data = nftpi_90_nooutlier)

kruskal.test(AllAverages~Condition, data = nftpi_50_nooutlier)
kruskal.test(FPrice~Condition,data = nftpi_50_nooutlier)
dunn_test(AllAverages~Condition,data = nftpi_50_nooutlier)
dunn_test(FPrice~Condition,data = nftpi_50_nooutlier)

nftpi_AllAverages_3_4 <- nftpi_w0[ nftpi_w0$Condition=="e3_c" | nftpi_w0$Condition=="e3_90" | nftpi_w0$Condition=="e3_50"
nftpi_AllAverages_3_4 <- nftpi_w0[ nftpi_w0$Condition=="e3_c" | nftpi_w0$Condition=="e3_90" | nftpi_w0$Condition=="e3_50"
nftpi_AllAverages_3_4 <- nftpi_w0[ nftpi_w0$Condition=="e3_c" | nftpi_w0$Condition=="e3_90" | nftpi_w0$Condition=="e3_50"
                                      | nftpi_w0$Condition=="e4_c" | nftpi_w0$Condition=="e4_90" | nftpi_w0$Condition=="e4_50",]
