


inputControl <- function(args){
  counter <- 0
  for(arg in args){
    if(arg == "ali") {counter <- counter + 1}
  }
  return(counter)
}

names1 <- c("ali", "mehmet", "can", "zühre", "ali")
names2 <- c("mehmet")
names3 <- NULL

inputControl(names1)

irisDataSet <- iris
dim(irisDataSet)
apply(irisDataSet[0:4], 2, mean)

myList <- list(head(irisDataSet[1], 10), head(irisDataSet[2], 10), head(irisDataSet[3], 10), head(irisDataSet[4], 10))

retList <- NULL
for(list in myList){
  retList <- c(retList, list(apply(list, 2, sum)))
}

retList

retVec <- NULL
for(list in myList){
  retVec <- c(retVec, apply(list, 2, mean))
}
retVec

inp <- c("ali", "veli", "ayþe", "kazým")
match("ali", inp)

inp <- c("ali", "veli", "ayþe", "kazým", NA, NA, "kerim")

inpWithoutNA <- NULL
for(str in inp){
  if(!is.na(str)){
    inpWithoutNA <- c(inpWithoutNA, str)
  }
}

proteinData <- data.frame(read.delim("protein.txt"))
dim(proteinData)
colnames(proteinData)

patientData <- data.frame(read.csv2("Patient-Subtype.csv"))
dim(patientData)
colnames(patientData)

basalPatientID <- c("")

for(i in 1:nrow(patientData)){
  if(patientData[i, 2] == "basal"){
    temp <- patientData[i, 1]
    basalPatientID <- c(basalPatientID, droplevels(temp))
    print(droplevels(temp))
  }
}

basalPatientID <- as.vector(patientData[which(patientData$Sub.type == "basal"),1])

basalPatientID

testData <- proteinData[,basalPatientID]

head(testData)

# each row represents a protein - out of 143

# each column represents a patient - out of 4

for(list in testData){
  print(sum(as.numeric(list))) #sum patients
  print(mean(as.numeric(list)))#mean
  print(sd(as.numeric(list))) #sd
}


