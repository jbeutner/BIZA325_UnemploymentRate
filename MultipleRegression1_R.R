
#This is a multiple linear regression model attempting to predict US unemployment rate based on a variety of economic and entrepreneurship data from NBER, BLS, and the Ewing Marion Kauffman Foundation's Kauffman Index.

#Variable List:
  #Unemployment Rate = UnemRate
  #Years Past Recession = YrsPstRec
  #Number of firms less than 1 year old = Firms1YrOld
  #Rate of New Entrepreneurs = RtNewEship
  #Opportunity Share (%) = OppShr
  #Startup Density = SUDen
  #Rate of Business Owners(%) = RtBusOwners
  #Business Survival Rate = BusSurvRate
  #Small Business Density = SmBusDen
  #Rate of Startup Growth = RtSUGrwth
  #Share of Scaleups = ShrScaleUps
  #More info about variables at www.kauffmanindex.org

###Begin model
#Import data
#Split data into a training set (traindata) and a test set (testdata)
#Print summary statistics of training set (eshipdata[1:17,1:11])

eshipdata = read.csv("Stats325_DataSet_R.csv", header = TRUE)
traindata = eshipdata[1:17,1:11]
testdata = eshipdata[18:23,1:11]
  summary(traindata)

#Create visual and tabular correlation matricies of training set (eshipdata[1:17,1:11])

cor(traindata)
pairs(traindata)

#Multiple Regression Model 1
#Creates coefficient values, p-values, and metrics for evaluating the model (r^2, etc)

model1 <- lm(UnemRate ~ YrsPstRec + Firms1YrOld + RtNewEship + OppShr + SUDen + RtBusOwners + BusSurvRt + SmBusDen + RtSUGrwth + ShrScaleUps, data = traindata)
  summary(model1)
  
#Generate predictions using Model 1  

p1=predict(model1, data.frame("YrsPstRec"=eshipdata[18,1], "Firms1YrOld"=eshipdata[18,2], "RtNewEship"=eshipdata[18,3], "OppShr"=eshipdata[18,4], "SUDen"=eshipdata[18,5], "RtBusOwners"=eshipdata[18,6], "BusSurvRt"=eshipdata[18,7], "SmBusDen"=eshipdata[18,8], "RtSUGrwth"=eshipdata[18,9], "ShrScaleUps"=eshipdata[18,10]))
p2=predict(model1, data.frame("YrsPstRec"=eshipdata[19,1], "Firms1YrOld"=eshipdata[19,2], "RtNewEship"=eshipdata[19,3], "OppShr"=eshipdata[19,4], "SUDen"=eshipdata[19,5], "RtBusOwners"=eshipdata[19,6], "BusSurvRt"=eshipdata[19,7], "SmBusDen"=eshipdata[19,8], "RtSUGrwth"=eshipdata[19,9], "ShrScaleUps"=eshipdata[19,10]))
p3=predict(model1, data.frame("YrsPstRec"=eshipdata[20,1], "Firms1YrOld"=eshipdata[20,2], "RtNewEship"=eshipdata[20,3], "OppShr"=eshipdata[20,4], "SUDen"=eshipdata[20,5], "RtBusOwners"=eshipdata[20,6], "BusSurvRt"=eshipdata[20,7], "SmBusDen"=eshipdata[20,8], "RtSUGrwth"=eshipdata[20,9], "ShrScaleUps"=eshipdata[20,10]))
p4=predict(model1, data.frame("YrsPstRec"=eshipdata[21,1], "Firms1YrOld"=eshipdata[21,2], "RtNewEship"=eshipdata[21,3], "OppShr"=eshipdata[21,4], "SUDen"=eshipdata[21,5], "RtBusOwners"=eshipdata[21,6], "BusSurvRt"=eshipdata[21,7], "SmBusDen"=eshipdata[21,8], "RtSUGrwth"=eshipdata[21,9], "ShrScaleUps"=eshipdata[21,10]))
p5=predict(model1, data.frame("YrsPstRec"=eshipdata[22,1], "Firms1YrOld"=eshipdata[22,2], "RtNewEship"=eshipdata[22,3], "OppShr"=eshipdata[22,4], "SUDen"=eshipdata[22,5], "RtBusOwners"=eshipdata[22,6], "BusSurvRt"=eshipdata[22,7], "SmBusDen"=eshipdata[22,8], "RtSUGrwth"=eshipdata[22,9], "ShrScaleUps"=eshipdata[22,10]))

#Creates dataframe of unemployment rate predictions from Model 1 and dataframe of actual unemployment rates 

predictions = data.frame(p1,p2,p3,p4,p5)
actual = data.frame(eshipdata[18,11],eshipdata[19,11],eshipdata[20,11],eshipdata[21,11],eshipdata[22,11])

#Finds absolute errors by subtracting the actual unemployment rate from the predicted unemployment rates

UnemMAPE = data.frame(abs(predictions-actual))
  print(UnemMAPE)

#Finds the average absolute error
  
AvgUnemMAPE = sum(UnemMAPE[1,1:5])/length(UnemMAPE)
  print(AvgUnemMAPE)

#Multiple Regression Model 2
#Variable selected if p-value < 0.15
#Creates coefficient values, p-values, and metrics for evaluating the model (r^2, etc)
  
model2 <- lm(UnemRate ~ Firms1YrOld + RtNewEship + OppShr + SUDen, data = traindata)
  summary(model2)
  
#Generate predictions using Model 2  
  
p1.m2=predict(model2, data.frame("YrsPstRec"=eshipdata[18,1], "Firms1YrOld"=eshipdata[18,2], "RtNewEship"=eshipdata[18,3], "OppShr"=eshipdata[18,4], "SUDen"=eshipdata[18,5]))
p2.m2=predict(model2, data.frame("YrsPstRec"=eshipdata[19,1], "Firms1YrOld"=eshipdata[19,2], "RtNewEship"=eshipdata[19,3], "OppShr"=eshipdata[19,4], "SUDen"=eshipdata[19,5]))
p3.m2=predict(model2, data.frame("YrsPstRec"=eshipdata[20,1], "Firms1YrOld"=eshipdata[20,2], "RtNewEship"=eshipdata[20,3], "OppShr"=eshipdata[20,4], "SUDen"=eshipdata[20,5]))
p4.m2=predict(model2, data.frame("YrsPstRec"=eshipdata[21,1], "Firms1YrOld"=eshipdata[21,2], "RtNewEship"=eshipdata[21,3], "OppShr"=eshipdata[21,4], "SUDen"=eshipdata[21,5]))
p5.m2=predict(model2, data.frame("YrsPstRec"=eshipdata[22,1], "Firms1YrOld"=eshipdata[22,2], "RtNewEship"=eshipdata[22,3], "OppShr"=eshipdata[22,4], "SUDen"=eshipdata[22,5]))
  
#Creates dataframe of unemployment rate predictions from Model 2 and dataframe of actual unemployment rates 
  
predictions.model2 = data.frame(p1.m2,p2.m2,p3.m2,p4.m2,p5.m2)
actual.model2 = data.frame(eshipdata[18,11],eshipdata[19,11],eshipdata[20,11],eshipdata[21,11],eshipdata[22,11])
  
#Finds absolute errors by subtracting the actual unemployment rate from the predicted unemployment rates
  
UnemMAPE.model2 = data.frame(abs(predictions.model2-actual))
print(UnemMAPE.model2)
  
#Finds the average absolute error
  
AvgUnemMAPE.model2 = sum(UnemMAPE.model2[1,1:5])/length(UnemMAPE.model2)
  print(AvgUnemMAPE.model2)
