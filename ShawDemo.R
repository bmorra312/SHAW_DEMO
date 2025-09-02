###
#This is a method for running the SHAW model in batch for 21 soil pedons downloaded using the SoilDB package
#It runs a each pedon under 10 weather conditions using gridMET data downloaded from the ClimateR package
  #the 5 driest years on record
  #the 5 wettest years on record
#it also looks at the effect of antecedent soil moisture conditions by starting
#the initial moisture conditions at either saturation or residual soil moisture.

#The shaw model needs 4 pieces to run:
#a site file (.sit), 
#initial moisture conditions for each soil layer (.moi),
#initial soil temp conditions for each layer (.tem), and
#Daily weather (.wea).
#An input file (.inp) connects these 4 pieces and gets run by the SHAW model executable.

#to run many input files, the script ends by writing a .bat file to open and run SHAW.exe on each .inp 
###
#config


required_packages <- c("tidyverse", "soilDB", "RCurl", "here","stringr")

installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

library(tidyverse)
library(soilDB)
library(RCurl)
library(here)
library(stringr)
#######

weab <- getURL("https://raw.githubusercontent.com/bmorra312/SHAW_DEMO/refs/heads/main/Weather_shaw_demo.csv")
weab <- read.csv(text = weab)

#weather data was downloaded using the ClimateR package and features the 5 wettest and driest years on record
#for the centroid of each seed zone


#Soils data from KSSL

soils <- getURL("https://raw.githubusercontent.com/bmorra312/SHAW_DEMO/refs/heads/main/soils_shaw_demo.csv")
soils <- read.csv(text = soils)


#calculate hydraulic properties

c<- (soils[,c(5:8)])


############
#calculate Hydraulic parameters using the Van Genuchten equation from Rossetta
prop<-data.frame( ROSETTA(
  c,
  vars = c("sand",  "silt" , "clay" , "db_od" ),
  v="1"
))


#documentation for the soilDB library says Rossetta returns the logged values for alpha, the n parameter and saturated K
prop$alpha<-(1/(10^prop$alpha))/100 #this comes as log10(1/cm)
prop$npar<-10^prop$npar
prop$ksat<-10^prop$ksat
prop$ksat<-prop$ksat/24 #SHAW needs units of cm/hr 

complete<-cbind(soils[,-c(5:8)],prop)

#make the site files

complete$hzn_bot<-complete$hzn_bot/100 #units meters
complete$db_od<-complete$db_od*1000 #units kg/m3
complete$alpha<-complete$alpha #
complete$Rock<-rep(0)
complete$kLat<-rep(0)
complete$AirEntry<-rep(0)
complete$pore_con<-rep(0.5)
complete<-complete[,-c(6)]

#The order of columns matters for the Site file see pg 19 of shaw users guide 
site<-complete[,c(1,6,3,7:9,18,5,10,15,19,20,12,14,11,21,13)]

site$Pedon_Key<-as.numeric(site$Pedon_Key)



#write a site file (.sit) and moisture (.moi) file for each soil 
#set_here("~/GitHub/SHAW_DEMO") 


#make a list of years associated with each pedon_key
wea_yr<-weab %>% group_by(FID_L0West,category) %>% 
  reframe(year=unique(year)
            )
output_dir <- here("GitHub","SHAW_DEMO")

for (j in 1:length(unique(site$Pedon_Key))) { 
  sol<-subset(site,Pedon_Key==unique(Pedon_Key)[j])
  sol<-left_join(sol,wea_yr, relationship ="many-to-many" ) 
  
  
  for (k in 1:length(unique(sol$year))) {

   prof<-subset(sol,year==unique(sol$year)[k])
   prof <- prof %>% distinct() #get rid of duplicate rows
   input<-list(
        paste0("Pedon",prof[1,1],"SeedZone", prof[1,2],sep="_"),
        paste0("1 24 ",unique(prof$year)," 365 ",unique(prof$year)," -Line B" ,sep=" "),#paste the year into the site
       "43 35 0.000 0.000 12 946.404 -Line C",
        paste0(" 0 0 0 ", nrow(prof)+1, " 0 0.010 24 0 0 0 0 0 0 -Line D"),
        "10.0000 2.000 2.000  -Line E",
        "1  0.000 0.1500 0  -Line G",
        "1 0 0.15 0.0 3 0 -Line J",
        cbind(0.01, prof[1,c(4:17)] ),#Adds a node at 1 cm to simulate shallow conditions at 1 cm depth
        prof[,c(3:17)] # soil characteristics
        
      )
   #setwd("C:/Users/Brian.Morra/Documents/Shaw303/") 
      
      #file_conn <- file(paste0(prof[1,1],"_",prof[1,19],"_",prof[1,2], ".sit"), "w")#naming convention is pedon, year, Seed zone
      file_conn <- file(file.path(here(output_dir,"Site"), paste0(prof[1,1], "_", prof[1,19], "_", prof[1,2], ".sit")), "w")
      # Loop through each element of the list and write it to the file
      for (item in input) {
        if (is.data.frame(item)) {
          item <- item %>% distinct() #get rid of duplicate rows
          # If the item is a data frame, convert it to a space-separated string and write it
          write.table(item, file_conn, row.names = FALSE, col.names = FALSE, sep = " ", quote = FALSE)
        } else {
          # If the item is a string, simply write it
          writeLines(item, file_conn)
        }
      }
      
      # Close the file connection
      close(file_conn)

      
            
moi<-data.frame()
     
  #wet antecedent conditions
      sat<-prof$theta_s
         moi<-  rbind(moi,c("1 24 ", unique(prof$year)," ",sat[1],sat), #adding an extra layer 1 to match adding an extra node above in the site file
                      c("365 24 ",unique(prof$year)," ",sat[1],sat) )  
         #setwd("C:/Users/Brian.Morra/Documents/Shaw303/") 
         moi_path <- file.path(here(output_dir,"Moi"), paste0(prof[1,1], "_", prof[1,19], "SAT.moi"))
         write.table(moi, moi_path, quote = FALSE, row.names = FALSE, col.names = FALSE)
         #write.table(moi, paste0(prof[1,1],"_",prof[1,19],"SAT.moi"), quote = FALSE, row.names = FALSE,col.names=FALSE )#naming convention is pedon, year and SAT for wet years
         moi<-data.frame()
  #dry antecedent conditions  
      sat<-prof$theta_r+0.01 
         moi<-  rbind(moi,c("1 24 ", unique(prof$year)," ",sat[1],sat), #adding an extra layer 1 to match adding an extra node above in the site file
                      c("365 24 ",unique(prof$year)," ",sat[1],sat) )  
         #setwd("C:/Users/Brian.Morra/Documents/Shaw303/") 
         moi_path <- file.path(here(output_dir,"Moi"), paste0(prof[1,1], "_", prof[1,19], "DRY.moi"))
         write.table(moi, moi_path, quote = FALSE, row.names = FALSE, col.names = FALSE)
         #write.table(moi, paste0(prof[1,1],"_",prof[1,19],"DRY.moi"), quote = FALSE, row.names = FALSE,col.names=FALSE )#naming convention is pedon, year and DRY for dry years
         moi<-data.frame()
         
    }
    
  }

#create weather (.wea) files for each seed zone
weab <- getURL("https://raw.githubusercontent.com/bmorra312/SHAW_DEMO/refs/heads/main/Weather_shaw_demo.csv")
weab <- read.csv(text = weab)


for (i in 1:length(unique(weab$FID_L0West))) {
  output<-subset(weab,FID_L0West==unique(weab$FID_L0West)[i])
  output1<-output[,c(3:10)]
  wea_path <- file.path(here("Wea"), paste0(unique(output$FID_L0West),".wea"))
  write.table(output1, wea_path, quote = FALSE, row.names = FALSE,col.names=FALSE )
  
}



####
#Write the input files
###
#setwd("C:/Users/Brian.Morra/Documents/Shaw303/Site/")

File_names <- data.frame(id=list.files(here("Site"),pattern="\\.sit$", full.names=TRUE))

File_names$id<-str_extract(File_names$id , "[^/]+$")
File_names1<-File_names
File_names1$id<-stringr::str_replace_all(File_names$id, ".sit", "")



File_names1<-data.frame(stringr::str_split_fixed(File_names1$id, "_", 3 ))
names(File_names1)[1]<-"Pedon_Key"
names(File_names1)[2]<-"Year"
names(File_names1)[3]<-"SeedZone"

File_namesd <- File_names1 %>%
  mutate(
      Ant_moi=rep("DRY")
    
  )
File_namess <- File_names1 %>%
  mutate(
   
    Ant_moi=rep("SAT")
    
  )
File_names1<-rbind(File_namesd,File_namess)

#setwd("C:/Users/Brian/Box/Shaw Model/INP_files/")
#setwd("C:/Users/Brian.Morra/Documents/Shaw303/Inp")
b<-data.frame()

for (i in 1:nrow(File_names1)) {  #nrow(File_names)
  
  b<-rbind(
    "Shaw 3.0",
    "1 1 0 0",
    paste0(here("Site"),"/",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$SeedZone[i],na.omit(File_names1$Burn[i]),".sit"),
   
    paste0(here("Wea"),"/",File_names1$SeedZone[i],".wea" ),
 
   paste0(here("Moi"),"/",File_names1$Pedon_Key[i],"_",File_names1$Year[i],na.omit(File_names1$Burn[i]),File_names1$Ant_moi[i],".moi"),# 
   paste0(here("Temp"),"/",File_names1$Year[i],".tem"),#temperature file
    "0 24 24  0  24  0  0  0 0 24  24  0  0  24  0  0  0  0  0  0", #Adjust this to change the output files created pg 9 
    paste0(here("ModelOutputs"),"/OUT.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/TEMP.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i]),
    paste0(here("ModelOutputs"),"/MOIST.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/LIQUID.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/MATRIC.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/CANTMP.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/CANHUM.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/SNOWTMP.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/ENERGY.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i]),
    paste0(here("ModelOutputs"),"/WATER.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/WFLOW.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/ROOTXT.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/LATERAL.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    paste0(here("ModelOutputs"),"/FROST.",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$Ant_moi[i] ),
    "\r", "\n", "\r" ,"\n" ,"\r" ,"\n"
    
  )

  output_path <- file.path(here("Inp"), paste0(File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$SeedZone[i], na.omit(File_names1$Burn[i]),"_",File_names1$Ant_moi[i],".inp" ))
  writeLines(b,output_path) #
  b<-data.frame()
  }

#Write the soil temperature files

#setwd("C:/Users/Brian.Morra/Documents/Shaw303/TempFiles/")
tem<-data.frame()
for (i in 1:length(unique(File_names1$Year))) {
  
  
  tem<-  rbind(tem,paste0("1 24 ", unique(File_names1$Year)[i]," 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100"), 
               paste0("365 24 ",unique(File_names1$Year)[i]," 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100 1.100") )  
  tem_path<-file.path(here("Temp"), paste0(unique(File_names1$Year)[i],".tem") )
  write.table(tem,tem_path, quote = FALSE, row.names = FALSE,col.names=FALSE )
  tem<-data.frame()
}

##Write a .bat file to execute the shaw model many many times 

batch<-data.frame()
for (i in 1:nrow(File_names1)) {  #nrow(File_names)
  loo<-paste0("echo ", here("Inp"), "/",File_names1$Pedon_Key[i],"_",File_names1$Year[i],"_",File_names1$SeedZone[i],na.omit(File_names1$Burn[i]),"_",File_names1$Ant_moi[i],".inp |", ' "C:\\Users\\Brian.Morra\\Documents\\Shaw303\\Shaw303.exe" ')
  
  batch<-rbind(batch,loo)
}
bat_path<-file.path(paste0("BATCH_FULL",".bat"))
write.table(batch,bat_path, quote = FALSE, row.names = FALSE,col.names=FALSE)


