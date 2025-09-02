# SHAW_DEMO
Documenting the work flow for creating batch files to run the SHAW model

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
