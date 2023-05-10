#CHOO YI BEI
#TP066534

View(houserent_data)

#2.0 DATA IMPORT
houserent_data = read.csv("C:\\Users\\yibei\\Desktop\\HAHAHAHA\\SEM 1\\Programming for Data Analysis\\Assignment\\House_Rent_Dataset.csv", 
                          header = TRUE)

head(houserent_data,10) #read first 10 rows from the dataset

names(houserent_data) = c("POSTED_DATE","BHK","RENT","SIZE","FLOOR","AREA_TYPE","AREA_LOCALITY","CITY","FURNISHING_STATUS"
                          ,"TENANT_PREFERRED","BATHROOM","POINT_OF_CONTACT")  #assign headers

tail(houserent_data) #read the last 6 rows 

#2.1 DATA PRE-PROCESSING
#2.1.1 Analysis outliers between relationship RENT and CITY

library(ggplot2)
ggplot(houserent_data, aes(y=RENT, x=CITY)) + geom_boxplot() +
  scale_y_continuous(labels = scales::comma)

summary(houserent_data$RENT) #summary for the RENT

#           following source code obtained from (Schork, n.d.)
tapply(houserent_data$RENT, houserent_data$CITY, summary) #descriptive summary statistic by group

#plot which ignore the outliers
library(ggplot2)
#           following source code obtained from (GeeksForGeeks, 2021)
outliers_ignored1 <- ggplot(houserent_data, aes(y=RENT, x=CITY))
outliers_ignored1 + geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = c(0,200000))

#data removed the outliers
#           following source code obtained from (Dag, 2022)   
quartiles1 <- quantile(houserent_data$RENT, probs=c(.25,.75), na.rm = FALSE)
IQR1 <- IQR(houserent_data$RENT)
Lower1 <- quartiles1[1] - 1.5*IQR1
Upper1 <- quartiles1[2] + 1.5*IQR1
data_no_outlier1 <- subset(houserent_data, houserent_data$RENT > Lower1 & 
                             houserent_data$RENT < Upper1)

summary(data_no_outlier1$RENT) #summary for RENT which outliers were removed

#           following source code obtained from (Schork, n.d.)
tapply(data_no_outlier1$RENT, data_no_outlier1$CITY, summary) 

#2.1.2 Analysis outliers between relationship RENT and SIZE
#           following source code obtained from (STHDA, n.d.)

library(ggplot2)
ggplot(houserent_data, aes(x=SIZE, y=RENT, color=RENT)) + geom_point()+
  scale_color_gradientn(colors = rainbow(5)) +
  scale_y_continuous(labels = scales::comma)  #remove the sciencticfic notation 

#plot after extreme outliers of RENT is removed
ggplot(data_no_outlier1, aes(x=SIZE, y=RENT, color=RENT)) + geom_point()+
  scale_color_gradientn(colors = rainbow(5))   #the data is the data without the outliers

#shows rows and columns of dataset
dim(houserent_data)
dim(data_no_outlier1)

#2.2 DATA TRANSFORMATION
#2.2.1 Simplify the data of tenant 
levels(as.factor(houserent_data$TENANT_PREFERRED))   #show the levels of the tenant

houserent_data$TENANT_PREFERRED=ifelse(houserent_data$TENANT_PREFERRED=="Bachelors/Family","Both",
                                       houserent_data$TENANT_PREFERRED)

View(houserent_data)

#2.2.2 Add new column which is total number of floors
levels(as.factor(houserent_data$FLOOR))

houserent_data$FLOOR <- ifelse(houserent_data$FLOOR=="1","1 out of 1",
                               houserent_data$FLOOR)                               

houserent_data$FLOOR <- ifelse(houserent_data$FLOOR=="3","3 out of 3",
                               houserent_data$FLOOR)                    #change those values into other values


library(dplyr)
n_last <- 1 #specify number of characters to extract
#           following source code obtained from (Statistic Globe, n.d.)

no_of_floor <- houserent_data %>% mutate(No_Of_Floor = substr(houserent_data$FLOOR,nchar(houserent_data$FLOOR) - n_last, 
                                                              nchar(houserent_data$FLOOR)))


houserent_data <- houserent_data %>% mutate(no_of_floor) #add the column into the table

dim(houserent_data) #acknowledge the total number of columns

levels(as.factor(houserent_data$No_Of_Floor))

houserent_data %>% filter(houserent_data$No_Of_Floor == "nd") #find out which row having nd

houserent_data$No_Of_Floor=ifelse(houserent_data$No_Of_Floor=="nd",1,
                                       houserent_data$No_Of_Floor)     #replace nd with 1


#2.2.3 Add new column about which floor will be rent

#           following source code obtained from (Stack Overflow, n.d.-c)
floor_rent <- houserent_data %>% mutate(
  Floor_Rent = vapply(strsplit(houserent_data$FLOOR," "),`[`,1,FUN.VALUE = character(1)))

houserent_data <- houserent_data %>% mutate(floor_rent) #add the Floor_Rent coloum into the dataset

levels(as.factor(houserent_data$Floor_Rent)) #View the levels of Floor_Rent


#2.2.4 Replace abnormal data at Area Locality

levels(as.factor(houserent_data$AREA_LOCALITY))

#              first abnormal data
houserent_data[houserent_data$AREA_LOCALITY=="2 BHK",]     #filter out the data to find belongs to which city
library(dplyr)
filtered_area <- houserent_data %>% filter(houserent_data$CITY =="Kolkata")   #belongs to Kolkata
filtered_area <- filtered_area %>% group_by(filtered_area$AREA_LOCALITY) %>% count()
filtered_area <- filtered_area %>% arrange(-filtered_area$n)  #arrange in desc order
View(filtered_area)

houserent_data$AREA_LOCALITY<- ifelse(houserent_data$AREA_LOCALITY=="2 BHK",
                         "Salt Lake City Sector 2",houserent_data$AREA_LOCALITY)  #replace the abnormal data with the most counted area locality

#              second abnormal data
houserent_data[houserent_data$AREA_LOCALITY=="5000",] 
filtered_area <- houserent_data %>% filter(houserent_data$CITY =="Chennai")  #belongs to Chennai
filtered_area <- filtered_area %>% group_by(filtered_area$AREA_LOCALITY) %>% count()
filtered_area <- filtered_area %>% arrange(-filtered_area$n)
View(filtered_area)

houserent_data$AREA_LOCALITY<- ifelse(houserent_data$AREA_LOCALITY=="5000",
                                      "Velachery",houserent_data$AREA_LOCALITY)

#              third abnormal data
houserent_data[houserent_data$AREA_LOCALITY=="700051",] 
filtered_area <- houserent_data %>% filter(houserent_data$CITY =="Kolkata") #belongs to Kolkata 
                                                                 

filtered_area <- filtered_area %>% group_by(filtered_area$AREA_LOCALITY) %>% count()
filtered_area <- filtered_area %>% arrange(-filtered_area$n)
View(filtered_area)

houserent_data$AREA_LOCALITY<- ifelse(houserent_data$AREA_LOCALITY=="700051",
                                      "Salt Lake City Sector 2",houserent_data$AREA_LOCALITY)


#2.2.5 Simplify point of contact 

#           following source code obtained from (Stack Overflow, n.d.-a)
contact_point <- vapply(strsplit(houserent_data$POINT_OF_CONTACT," "),`[`,-1,FUN.VALUE = character(1))
houserent_data$POINT_OF_CONTACT <- contact_point
View(houserent_data)                                   #split the string with empty space and get the last word


#2.2.6 Simplify area type

area_type <- vapply(strsplit(houserent_data$AREA_TYPE," "),`[`,1,FUN.VALUE = character(1))
houserent_data$AREA_TYPE <- area_type
View(houserent_data)                            #split the string with empty space and get the first word

#2.2.7 Extract Month form the date

date_posted = houserent_data[houserent_data$POSTED_DATE=="5/18/2022",]
class(date_posted)            #test the datatype of POSTED_DATE


#           following source code obtained from (Stack Overflow, n.d.-f)
library(lubridate)
month_rent = month(as.POSIXct(houserent_data$POSTED_DATE,format="%m/%d/%Y"), 
                   abbr=TRUE,label=TRUE)
month_rent                                        #convert to date 

houserent_data <- houserent_data %>% mutate(month_rent)  #create a new column


#3.0 Question 1: Factor affecting a bachelor to rent a house 
#3.1 Analysis: Total counts of house rental for bachelor

bac = nrow(houserent_data[houserent_data$TENANT_PREFERRED=="Bachelors" | 
                            houserent_data$TENANT_PREFERRED=="Both",])   
bac

fam = nrow(houserent_data[houserent_data$TENANT_PREFERRED=="Family",])
fam                                                                       #gather the number of rows that meets the conditions

a =c(bac,fam) #combine and create a new vector
b = c("Bachelors","Family")    #another variable with 2 strings
pie(a,a,radius=1,col=c("pink","violet"),clockwise = TRUE,
    main="Pie Chart of number of houses for bachelors and family",)
legend("topright",legend=b,fill=c("pink","violet"),cex=0.6)        #add legend to pie chart

#           following source code obtained from (dplyr, n.d.)
count_of_tenant <- houserent_data %>% count(houserent_data$TENANT_PREFERRED)   #count the number of rows for each tenant
colnames(count_of_tenant) <- c("TenantPreferred", "Count") #rename the columns
count_of_tenant #show table 

library(ggplot2)
ggplot(count_of_tenant, aes(TenantPreferred,Count,fill=TenantPreferred)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#FFFCAC","#ACFFFE","#F6ACFF")) + 
  geom_text(aes(label=Count),vjust=-0.1,col="Purple")                 #add colors manually


#3.2 Analysis: What is the rent of houses for bachelors
filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")

ggplot(filtered_data, aes(x=RENT,y=SIZE)) + 
  geom_point(aes(color=factor(TENANT_PREFERRED))) +
  scale_x_continuous(labels = scales::comma)           #remove scientific notation

ggplot(filtered_data, aes(x=RENT,y=SIZE)) + 
  geom_point(aes(color=factor(TENANT_PREFERRED))) + facet_wrap(~TENANT_PREFERRED) +
  scale_x_continuous(labels = scales::comma)          #split the graphs based on the identity of tenant


#3.3 Analysis: Bachelors usually able to rent house at which floor  

library(dplyr)
filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")
levels(as.factor(filtered_data$Floor_Rent))

pie(table(filtered_data$Floor_Rent),radius=1,main="FLoor for rent") #show pie with bachelors and both

nrow(filtered_data[filtered_data$Floor_Rent=="Ground",])
nrow(filtered_data[filtered_data$Floor_Rent=="1",])
nrow(filtered_data[filtered_data$Floor_Rent=="2",])
nrow(filtered_data[filtered_data$Floor_Rent=="3",]) #calculate the rows of each

library(dplyr)
bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")

library(ggplot2)
ggplot(bachelors_only, aes(x=Floor_Rent)) + geom_bar(color="black",fill="pink") + 
  geom_text(stat="count",aes(label=..count..)) #bar chart with only bachelors


#3.4 Analysis: How many bedrooms can a bachelor rent

levels(as.factor(houserent_data$BHK))

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")
ggplot(filtered_data, aes(x=BHK,y=SIZE)) + 
  geom_point(aes(shape=factor(TENANT_PREFERRED),color=factor(TENANT_PREFERRED))) #graph of size for each BHK for bachelors and both

bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")

ggplot(bachelors_only, aes(x=BHK)) + geom_histogram(binwidth=1,color="navy blue",aes(fill=..count..)) +
  scale_fill_gradient("Count", low="purple", high="blue")  #histogram of frequency of BHK rent for bachelors


#3.5 Analysis: Which area type are more likely to rent to bachelors

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")
counts_areatype <- filtered_data %>% group_by(filtered_data$AREA_TYPE) %>% summarise(counts=n())   #create a data frame that include area type and counts
colnames(counts_areatype) = c("area_type","counts")   #rename the columns
counts_areatype
library(plotrix)
pie3D(counts_areatype$counts,labels=c("Built Area","Carpet Area","Super Area"),explode=.6) #for bachelors and both


bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")
counts_for_bachelors <- bachelors_only %>% group_by(bachelors_only$AREA_TYPE) %>% summarise(counts=n())
colnames(counts_for_bachelors) <- c("area_type","counts")
counts_for_bachelors
library(ggplot2)
ggplot(counts_for_bachelors, aes(x=area_type, y=counts)) + 
  geom_bar(stat="identity",width=0.6,color="red",fill="gold") +
  geom_text(aes(label=counts),color="blue", vjust=0.1)           #for bachelors only

  
#3.6 Analysis: Which city welcomes bachelors more to rent a house

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")
library(ggplot2)
ggplot(filtered_data, aes(CITY, ..count..)) + 
  geom_bar(aes(fill=TENANT_PREFERRED), position="dodge") #dodge is used so can seperate into 2 individual bars


bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")

#           following source code obtained from (R CHARTS, 2021)
with_percentage <- bachelors_only %>% group_by(CITY) %>%
  count() %>% ungroup() %>%
  mutate(perc=`n`/sum(`n`)) %>%     #calculate the percentage
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))   #assgin labels with the data that have been converted into percentage form

ggplot(with_percentage,aes(x="",y=perc, fill=CITY)) + geom_col() +       #create a pie chart using ggplot
  geom_text(aes(label = labels), position=position_stack(vjust = 0.5)) +
  coord_polar(theta="y")              #each bar will stack together to form a circle   
  

#3.7 Analysis: What is the furnishing status when bachelors renting houses

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")
#           following source code obtained from (R CHARTS, 2021a)
counts_furnishingstatus <- filtered_data %>% group_by(FURNISHING_STATUS) %>% count()
library(plotrix)
pie3D(counts_furnishingstatus$n, main="Furnishing Status", 
      col=hcl.colors(length(counts_furnishingstatus$FURNISHING_STATUS),"Spectral"),  #assign colors automatically
      labels=counts_furnishingstatus$FURNISHING_STATUS) #for bachelors and both


bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")
furnishing_bachelors <- bachelors_only %>% group_by(FURNISHING_STATUS) %>% count()
#           following source code obtained from (Stack Overflow, n.d.-b)
pie3D(furnishing_bachelors$n,labels=furnishing_bachelors$n,
      main="Furnishing Status",radius=1,labelcex=1)
par(xpd=TRUE)     #so that legend can fit in
legend(1,0.7,legend=furnishing_bachelors$FURNISHING_STATUS,cex=0.55,yjust=0.5,xjust=-0.1,
       fill = c("red","green","blue"))

#3.8 Analysis: How many bathrooms is needed for bachelors 

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both") 

bathrooms_both <- filtered_data %>% group_by(TENANT_PREFERRED) %>% group_by(BATHROOM) %>% count()
colnames(bathrooms_both) = c("No_Of_Bathroom","Count")
ggplot(data=bathrooms_both,aes(y=Count,x=No_Of_Bathroom,)) + 
  geom_bar(stat="identity",width=0.5,fill="white",color="blue") +
  geom_text(aes(label=Count),col="purple",vjust=-0.3)               #for both


bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")
bath_bac <- bachelors_only %>% group_by(BATHROOM) %>% count()
#           following source code obtained from (Tutorials Point, n.d.)
barplot(bath_bac$n, main="Number of Bathrooms",
        names.arg=bath_bac$BATHROOM,xlab="No of Bathroom",
        ylab="Count",col=c("black","pink"),ylim = c(0,500)) 

#3.9 Analysis: Which month is the best month for bachelors to rent house

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both") 
month_both <- filtered_data %>% group_by(filtered_data$month_rent) %>% count()            #for both 
colnames(month_both) = c("MONTH","COUNTS")
library(ggplot2)
ggplot(month_both, aes(x=MONTH,y=COUNTS,group=1)) + geom_line(col="orange") +
  geom_point(col="red") + geom_text(aes(label=COUNTS),vjust=-1)          #some source code obtained from (Stack Overflow, n.d.-e)


bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")
bac_month <- bachelors_only %>% group_by(bachelors_only$month_rent) %>% count()
colnames(bac_month) = c("month","n")
ggplot(bac_month,aes(month,n)) + geom_bar(stat="identity",fill="purple",col="green") +
  geom_text(aes(label=n),col="blue",vjust=-1) + labs(x="Month",y="Counts")                #for bachelors only


#3.10 Analysis: Who will bachelors reach out for enquiries

levels(as.factor(houserent_data$POINT_OF_CONTACT))

library(dplyr)
filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors" |
                                             houserent_data$TENANT_PREFERRED == "Both")   #for both
counts_both <- filtered_data %>% group_by(filtered_data$POINT_OF_CONTACT) %>% count()
colnames(counts_both) <- c("PointOfContact","Counts")
library(ggplot2)
ggplot(counts_both, aes(PointOfContact, Counts)) + 
  geom_bar(stat="identity",col="blue",fill="#0ABAB5") +
  geom_text(aes(label=Counts))



bachelors_only <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")   #for bachelors only
point_bec <- bachelors_only %>% 
  group_by(bachelors_only$POINT_OF_CONTACT) %>% count()
colnames(point_bec) <- c("PointOfContact","Counts")

pie(point_bec$Counts, labels=point_bec$Counts,main="Pie Chart of Point of Contact",
    col=c("pink","#E6E6FA"),radius=1)
legend("topright",point_bec$PointOfContact,cex=0.8,fill=c("pink","#E6E6FA"))

#4.0 Question 2: Factor affecting a family to rent a house
#4.1 Analysis: When will family rent houses


filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

month_fam <- filtered_data %>% group_by(filtered_data$month_rent) %>% count()   
colnames(month_fam) <- c("Month","Counts")

library(ggplot2)
ggplot(month_fam,aes(Month,Counts,group=1)) + geom_line(col="red") + geom_point() +
  geom_text(aes(label=Counts), col="Blue", vjust=-0.5)


#4.2 Analysis: How family can choose a house based on the BHK configurations

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

bhk_fam <- filtered_data %>% group_by(BHK) %>% count()
colnames(bhk_fam) <- c("BHK","Counts")
library(ggplot2)
ggplot(bhk_fam,aes(BHK,Counts)) + geom_bar(stat="identity",fill="#87CEEB",col="blue") + 
  ylim(0,250) + geom_text(aes(label=Counts),col="#e75480", vjust=-0.5)     #limits the y scale range


#4.3 Analysis: What is the rent mostly accepted by family

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

library(ggplot2)
ggplot(filtered_data,aes(x=factor(BHK), y=RENT)) + 
  geom_boxplot(col="#7863FE",fill="#45FC54")

ggplot(filtered_data,aes(x=factor(BHK), y=RENT)) + 
  geom_boxplot(col="#7863FE",fill="#45FC54") + ylim(0,35000) + labs(x="BHK")  #limits y range so that can ignore some outliers

median(filtered_data$RENT)

#4.4 Analysis: Which floor normally will be rent by family

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

library(dplyr)
floor_fam <- filtered_data %>% group_by(filtered_data$Floor_Rent) %>% count()
colnames(floor_fam) <- c("Floor","Counts")

library(ggplot2)
ggplot(floor_fam, aes(x=Floor, y=Counts)) + 
  geom_bar(stat="identity",col="#E687FF",fill="#F1BEFF") +
  geom_text(aes(label=Counts),col="#83578F",vjust=-0.5)    #vjust is to move the text slightly above the bar


#4.5 Analysis:  What size are the rental house for family

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

plot(filtered_data$SIZE,type="l",xlab="COUNTS",ylab="SIZE",
     main="HOUSE SIZE RENT FOR FAMILY",col="blue")


#4.6 Analysis:  What area type family will want most

library(dplyr)
filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

area_fam <- filtered_data %>% group_by(filtered_data$AREA_TYPE) %>%
  count()
colnames(area_fam) = c("AREA_TYPE","COUNTS")


pie(area_fam$COUNTS,area_fam$COUNTS,radius=1,col=c("#ECC9F6","#F92A60"),
    main="Pie Chart of Area Type")
legend("topright",area_fam$AREA_TYPE,cex=0.5,fill=c("#ECC9F6","#F92A60"))

#4.7 Analysis:  Which city is more accepting of families to rent a house

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family" |
                                             houserent_data$TENANT_PREFERRED == "Bachelors")

library(ggplot2)
ggplot(filtered_data,aes(CITY,..count..)) + geom_bar(stat="identity",fill="#D790F7") +
  facet_wrap(~TENANT_PREFERRED)


#4.8 Analysis:  Does the family prefer to rent a furnished house

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

fur_fam <- filtered_data %>% group_by(FURNISHING_STATUS) %>% count()

pie3D(fur_fam$n,labels=fur_fam$FURNISHING_STATUS,
      col=c("#F9FEAC","#8CFBFF","#A4B7FE"),main="3D Pie Chart for Furnishing Status")


# 4.9 Analysis:  How many bathrooms is enough for a family

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")
filtered_data2 <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Bachelors")

bath_fam <- filtered_data %>% group_by(BATHROOM) %>% count()
bath_bac <- filtered_data2 %>% group_by(BATHROOM) %>% count()

plot(bath_fam$n,type="l",xlab="NO. of BATHROOMS",ylab="COUNTS",
     main="Graph of counts of No of BATHROOM",col="pink",ylim=c(0,450)) 
lines(bath_bac$n,type="l",xlab="NO, of BATHROOMS",ylab="COUNTS",
     main="Graph of counts of No of BATHROOM",col="blue",ylim=c(0,450))       #plot the second line with lines() function
legend("topright",c("Family","Bachelors"),cex=0.5,fill=c("pink","blue"))


#4.10 Analysis:  Who should a family contact to ask queries about the rental houses

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

cont_fam <- filtered_data %>% group_by(POINT_OF_CONTACT) %>% count()
colnames(cont_fam) = c("PointOfContact","Counts")

ggplot(cont_fam,aes(PointOfContact,Counts)) + geom_bar(stat="identity",
                                                       fill="#CBFFF6",col="#CDCA69") + 
  geom_text(aes(label=Counts),col="#AC9100",vjust=-0.5,size=6)


#4.11 Analysis:  Family prefer a house with how many floors

filtered_data <- houserent_data %>% filter(houserent_data$TENANT_PREFERRED == "Family")

floors_fam <- filtered_data %>% group_by(No_Of_Floor) %>% count()
colnames(floors_fam) <- c("NoOfFloors","Counts")

floors_fam$NoOfFloors = as.numeric(floors_fam$NoOfFloors) #convert to numeric form so that will follow asc order

ggplot(floors_fam,aes(factor(NoOfFloors),Counts)) +    #use factor to change it to categorical data
  geom_point(aes(col=factor(NoOfFloors)),size=3) +  #increase the size of points
  labs(title="The relationship between No of Floors and Counts",
       x="No of floors",col="No of floors")


#5.0 Question 3: What kind of houses are usually popular these months
#5.1 Analysis: Which month usually people rent their house 

counts_month <- houserent_data %>% group_by(month_rent) %>% count()
colnames(counts_month) = c("Month","Counts")

ggplot(counts_month,aes(Month,Counts)) + geom_bar(stat="identity",
                                                  col="#7C4381",fill="#EA9AF1") +
  geom_text(aes(label=Counts),vjust=-0.5)


#5.2 Analysis: Which BHK configuration would be popular during these months

for_bhk <- houserent_data %>% group_by(month_rent) %>% count(BHK)
colnames(for_bhk) <- c("Month","BHK","Count")


ggplot(for_bhk,aes(BHK,Count)) + geom_bar(stat="identity",col="green",fill="yellow") +
  facet_wrap(~Month) + geom_text(aes(label=Count), vjust=-0.5)


#5.3 Analysis: What is the rent during each month

#with outliers

rent_month <- select(houserent_data,month_rent,RENT)
colnames(rent_month) = c("MONTH","RENT")

april <- rent_month %>% filter(MONTH =="Apr") %>% arrange(MONTH)
may <- rent_month %>% filter(MONTH =="May") %>% arrange(MONTH)
june <- rent_month %>% filter(MONTH =="Jun") %>% arrange(MONTH)
july <- rent_month %>% filter(MONTH =="Jul") %>% arrange(MONTH)   #filter out the every month separately

options(scipen=999)  #source are from (Tutorials Point, n.d.-a)
plot(april$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="red",ylim=c(0,450000))
lines(may$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="green",ylim=c(0,450000))
lines(june$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="blue",ylim=c(0,450000))
lines(july$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="orange",ylim=c(0,450000))  #adjust the y scale
legend("topleft",c("April","May","June","July"),fill=c("red","green","blue","orange"),cex=0.4)  #add legend

max(houserent_data$RENT)  #to output the max rent


#without outliers
quartiles1 <- quantile(houserent_data$RENT, probs=c(.25,.75), na.rm = FALSE)   #to remove outliers
IQR1 <- IQR(houserent_data$RENT)
Lower1 <- quartiles1[1] - 1.5*IQR1
Upper1 <- quartiles1[2] + 1.5*IQR1        #calculate the lower quartiles and upper quartiles
data_no_outlier1 <- subset(houserent_data, houserent_data$RENT > Lower1 & 
                             houserent_data$RENT < Upper1)  

rent_month <- select(data_no_outlier1,month_rent,RENT)  #select only specific columns
colnames(rent_month) = c("MONTH","RENT")

april <- rent_month %>% filter(MONTH =="Apr") %>% arrange(MONTH)
may <- rent_month %>% filter(MONTH =="May") %>% arrange(MONTH)
june <- rent_month %>% filter(MONTH =="Jun") %>% arrange(MONTH)
july <- rent_month %>% filter(MONTH =="Jul") %>% arrange(MONTH)

plot(april$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="red",ylim=c(0,80000))
lines(may$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="green",ylim=c(0,80000))
lines(june$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="blue",ylim=c(0,80000))
lines(july$RENT,type="l",xlab="COUNTS",ylab="RENT",main="RENT each month",col="orange",ylim=c(0,80000))
legend("topleft",c("April","May","June","July"),fill=c("red","green","blue","orange"),cex=0.3)    #same way to plot the graph but without outliers


#5.4 Analysis: Which month will the Kolkata city have more rental options

kolkata_month <- houserent_data %>% filter(CITY =="Kolkata")  #filter out the data belongs to Kolkata

kolkata_month <- kolkata_month %>% group_by(month_rent) %>% count()  #calculate the rows grouped by month
colnames(kolkata_month) <- c("MONTH","COUNTS")   #rename


ggplot(kolkata_month,aes(MONTH,COUNTS)) + 
  geom_bar(stat="identity",col="#FF4821",fill="#F98C4D") +
  geom_text(aes(label=COUNTS), vjust=-0.5)


#5.5 Analysis: Which month will the Bangalore city have more rental options

bangalore_month <- houserent_data %>% filter(CITY =="Bangalore")

bangalore_month <- bangalore_month %>% group_by(month_rent) %>% count()
colnames(bangalore_month) <- c("MONTH","COUNTS")

piepercent <- round(100*bangalore_month$COUNTS/sum(bangalore_month$COUNTS),2)

pie(bangalore_month$COUNTS, labels=piepercent,
    main="Number of rental houses during each month",
    col=rainbow(length(bangalore_month$COUNTS)),radius=1) #assign colors automatically

legend("topright",c("April","May","June","July"),
       cex=0.5,fill=rainbow(length(bangalore_month$COUNTS)))



#5.6 Analysis: Which month will the Chennai city have more rental options

chennai_month <- houserent_data %>% filter(CITY =="Chennai")

chennai_month <- chennai_month %>% group_by(month_rent) %>% count()
colnames(chennai_month) <- c("MONTH","COUNTS")


pie3D(chennai_month$COUNTS,labels=chennai_month$MONTH,
      main="Chart for Chennai",radius=1,
      col = hcl.colors(length(chennai_month$MONTH), "Spectral"),
      border="white")  

pie3D(chennai_month$COUNTS,labels=chennai_month$COUNTS,
      main="Chart for Chennai",radius=1,
      col = hcl.colors(length(chennai_month$MONTH), "Spectral"),
      border="white")


#5.7 Analysis: Which month is the best month to rent houses at Delhi

delhi_month <- houserent_data %>% filter(CITY =="Delhi")

delhi_month <- delhi_month %>% group_by(month_rent) %>% count()
colnames(delhi_month) <- c("MONTH","COUNTS")

#           following source code obtained from (Holtz, n.d.)
barplot(height = delhi_month$COUNTS, names=delhi_month$MONTH,
        density=c(5,10,20,30),angle=c(0,45,90,11),col="purple",ylim=c(0,300),
        main = "Barchart of Delhi")     #density and angle is to make pattern inside the bar


#5.8 Analysis: Which month will the Hyderabad city have more rental options

hyderabad_month <- houserent_data %>% filter(CITY == "Hyderabad")

ggplot(hyderabad_month,aes(month_rent,..count..)) + 
  geom_bar(aes(fill=FURNISHING_STATUS), position = position_dodge(width=0.5)) +  #adjust the width of two individual bars
  scale_y_continuous(limits=c(0,150))   


#5.9 Analysis: Which month will the Mumbai city have more rental options

mumbai_month <- houserent_data %>% filter(CITY == "Mumbai")

ggplot(mumbai_month,aes(month_rent,..count..)) + 
  geom_bar(aes(fill=POINT_OF_CONTACT)) +
  scale_y_continuous(limits=c(0,450)) +   #limits the y axis scale
  labs(title="Counts of rental houses in Mumbai", x="MONTH",y="COUNTS")


#5.10 Analysis: The relationship between month and furnishing status

month_furn <- houserent_data %>% group_by(month_rent) %>% 
  count(FURNISHING_STATUS)
colnames(month_furn) = c("MONTH","FURNISHING_STATUS","COUNTS")

ggplot(month_furn, aes(MONTH,COUNTS)) + 
  geom_point(aes(shape=factor(FURNISHING_STATUS),col=factor(FURNISHING_STATUS)))+
  labs(title="The relationship between month and furnishing status",
       x="MONTH",y="COUNTS",shape="Furnihsing Status",col="Furnihsing Status") 




#6.0 Question 4: How BHK of a house being affected
#6.1 Analysis: The relationship between BHK and Rent

ggplot(houserent_data, aes(BHK,RENT)) + geom_point(aes(col=BHK)) +
  labs(title="The relationship between BHK and Rent",
       x="BHK",y="RENT",col="BHK") +
  scale_y_continuous(labels = scales::comma)

#6.2 Analysis: How size of the house affecting BHK

ggplot(houserent_data,aes(SIZE,RENT)) + geom_point(aes(col=factor(BHK))) +
  facet_wrap(~BHK) + scale_y_continuous(labels = scales::comma) +
  labs(title="The relationship between Rent and Size for each BHK",
       col="BHK")


#6.3 Analysis: The relationship between BHK and area type

ggplot(houserent_data, aes(BHK, ..count..)) + 
  geom_bar(aes(fill=AREA_TYPE), position="dodge") + 
  scale_y_continuous(limits=c(-2,1400)) +         #-2 is the lowest yaxis value so that the bar with little counts can be show
  labs(title="The relationship between BHK and area type",
       fill="Area Type", y="Counts")


#6.4 Analysis: Which BHK configuration is more popular in respective city


ggplot(houserent_data,aes(BHK,..count..)) + 
  geom_bar(fill="yellow",col="purple")+ 
  facet_wrap(~CITY) +labs(title="The number of each BHK in each city",
                           y="Counts")


#6.5 Analysis: How BHK will be based on the tenant

filtered_data <- houserent_data %>% filter(TENANT_PREFERRED=="Bachelors" | 
                                             TENANT_PREFERRED == "Family")

ggplot(filtered_data,aes(..count..,BHK)) +
  geom_bar(aes(fill=factor(TENANT_PREFERRED))) +theme(legend.position = "top")+  #move the legend to the top of the graph
  labs(title="The number of BHK for each tenant",
       fill="Tenant Preferred", x="Counts") + 
  scale_fill_manual(values=c("#D44E28","#FAE5D3"))


#6.6 Analysis: 1BHK normally will have how many bathrooms

bath_1BHK <- houserent_data %>% filter(BHK=="1")

bath_1BHK <- bath_1BHK %>% group_by(BATHROOM) %>% count()
colnames(bath_1BHK) = c("BATHROOM","COUNT")

pie(bath_1BHK$COUNT,labels=bath_1BHK$COUNT,radius=1,
    col=c("yellow","pink","green","blue","purple"),
    main="Pie Chart of number of bathrooms for 1BHK")
legend("topright",legend=bath_1BHK$BATHROOM,
       cex=0.6,fill=c("yellow","pink","green","blue","purple"))


#6.7 Analysis: 2BHK normally will have how many bathrooms


bath_2BHK <- houserent_data %>% filter(BHK=="2")

bath_2BHK <- bath_2BHK %>% group_by(BATHROOM) %>% count()
colnames(bath_2BHK) = c("BATHROOM","COUNT")

ggplot(bath_2BHK,aes(BATHROOM,COUNT,fill=c("1","2","3","4"))) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#9EFFFC","#F9F782","#AFFF8E","#C496FF"))+
  labs(title="Bar Chart of number of bathrooms for 2BHK",
       fill="No of BATHROOM")


#6.8 Analysis: 3BHK normally will have how many bathrooms

bath_3BHK <- houserent_data %>% filter(BHK=="3")

bath_3BHK <- bath_3BHK %>% group_by(BATHROOM) %>% count()
colnames(bath_3BHK) = c("BATHROOM","COUNT")

ggplot(bath_3BHK,aes(y=COUNT,x=factor(BATHROOM),group=1)) + geom_line(col="purple") +
  geom_point(col="yellow",size=3) +
  labs(title="Graph of Count against no of Bathroom",
       x="No of BATHROOM") 


#6.9 Analysis: 4BHK normally will have how many bathrooms

bath_4BHK <- houserent_data %>% filter(BHK=="4")

bath_4BHK <- bath_4BHK %>% group_by(BATHROOM) %>% count()
colnames(bath_4BHK) = c("BATHROOM","COUNT")

ggplot(bath_4BHK,aes(BATHROOM,COUNT,fill=COUNT)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=COUNT)) + ylim(0,100) +
  scale_fill_gradient(low="#C5FC95",high="#46850F")


#6.10 Analysis: Which BHK needed more bathroom, 5BHK or 6BHK

both_bhk <- houserent_data %>% filter(BHK=="5" | BHK=="6")

both_bhk <- both_bhk %>% group_by(BHK) %>% count(BATHROOM)
colnames(both_bhk) = c("BHK","BATHROOM","COUNT")


#           following source code obtained from (R CHARTS, 2021b)
ggplot(both_bhk,aes(x="",y=COUNT,fill=factor(BATHROOM))) + 
  geom_col(col="white") + coord_polar(theta="y") +  #create pie chart with ggplot function
  facet_wrap(~BHK) + 
  geom_text(aes(label=COUNT),
            position = position_stack(vjust=0.5),col="white") +  #add the label of counts at the boader of the pie
  scale_fill_manual(values=c("#FFB6A6","#ECC562","#A0F748",
                             "#80FFEC","#975AFD","#F895FF"))
  

#6.11 Analysis: Will the number of floors affected BHK configuration

houserent_data$No_Of_Floor <- as.numeric(houserent_data$No_Of_Floor)

ggplot(houserent_data,aes(BHK,factor(No_Of_Floor),col=factor(BHK))) + geom_point() +
  labs(title="BHK for each floor", y="No of Floors", col="BHK") 


#6.12 Analysis: The relationship between BHK and popular rental floor 

floor_filtered <- houserent_data %>% filter(Floor_Rent=="Ground"| Floor_Rent=="1"|
                                            Floor_Rent=="2" | Floor_Rent=="3")  

floor_filtered <- floor_filtered %>% group_by(Floor_Rent) %>% count(BHK)
colnames(floor_filtered) <- c("Floor","BHK","Counts")


ggplot(floor_filtered,aes(Floor,Counts,fill=factor(BHK))) + geom_bar(stat="identity") + 
  facet_wrap(~BHK) + 
  scale_fill_manual(values=c("#808B96","#F8C471","#7DCEA0",
                             "#D98880","#C39BD3","#AEB6BF")) +
  geom_text(aes(label=Counts),col="#212F3C") + labs(fill="BHK")

#6.13 Analysis: Which BHK is more popular during certain month

month_bhk <- houserent_data %>% group_by(month_rent) %>% count(BHK)
colnames(month_bhk) <- c("MONTH","BHK","COUNTS")

ggplot(month_bhk,aes(BHK,COUNTS)) + 
  geom_point(aes(col=factor(month_bhk$MONTH)),size=4)+
  facet_wrap(~MONTH) +labs(title="Numbers of each BHK in each Month",col="MONTH")


#7.0 Question 4: What factors affect the rent
#7.1 Analysis: Will BHK affect the rent

filtered_1BHK <- houserent_data %>% filter(BHK=="1")  #filter data which meets the condition
max_1BHK <- max(filtered_1BHK$RENT)                   #calculate the maximum value of the rent

filtered_2BHK <- houserent_data %>% filter(BHK=="2")
max_2BHK <- max(filtered_2BHK$RENT)

filtered_3BHK <- houserent_data %>% filter(BHK=="3")
max_3BHK <- max(filtered_3BHK$RENT)

filtered_4BHK <- houserent_data %>% filter(BHK=="4")
max_4BHK <- max(filtered_4BHK$RENT)

filtered_5BHK <- houserent_data %>% filter(BHK=="5")
max_5BHK <- max(filtered_5BHK$RENT)

filtered_6BHK <- houserent_data %>% filter(BHK=="6")
max_6BHK <- max(filtered_6BHK$RENT)

max_1BHK
max_2BHK
max_3BHK
max_4BHK
max_5BHK
max_6BHK

rent_bhk <- c(max_1BHK,max_2BHK,max_3BHK,max_4BHK,max_5BHK,max_6BHK)   #combined the max rent into a vector
bhk_name <- c("1":"6")             #create a vector contains number from 1 to 6, which 1 represent 1BHK
rent_bhk <- cbind(bhk_name,rent_bhk)      #combined 2 columns together
colnames(rent_bhk) <- c("BHK","MAXRENT")
View(rent_bhk)

rent_bhk <- as.data.frame(rent_bhk)   #convert into data frame
ggplot(rent_bhk,aes(BHK,MAXRENT)) + geom_line(col="green") + 
  geom_point(col="gold", size=3) + 
  geom_text(aes(label=MAXRENT),vjust=-0.5) +
  scale_y_continuous(labels = scales::comma)


#7.2 What is the rent for Bachelors 

filtered_data <- houserent_data %>% filter(TENANT_PREFERRED=="Bachelors")

plot(filtered_data$RENT,type="l",main="The count of each rent for bachelors",
     col="#E198FA",xlab="Counts",ylab="Rent")

summary(filtered_data$RENT)   #get every details of rent


#7.3 Analysis: What is the rent for Family

filtered_data <- houserent_data %>% filter(TENANT_PREFERRED=="Family")

options(scipen=999)   #remove the scientific notation of plot() function
plot(filtered_data$RENT,type="l",main="The count of each rent for family",
     col="#189696",xlab="Counts",ylab="Rent")

summary(filtered_data$RENT)


#7.4 Analysis: The relationship between rent and size

ggplot(houserent_data,aes(RENT,SIZE)) + 
  geom_point(aes(col=SIZE)) + labs(title="Graph SIZE against RENT")

#7.5 Analysis: Which month have the highest rent 

april_data <- houserent_data %>% filter(month_rent =="Apr")
may_data <- houserent_data %>% filter(month_rent =="May")
june_data <- houserent_data %>% filter(month_rent =="Jun")
july_data <- houserent_data %>% filter(month_rent =="Jul")   #filter out rows of each month

max_april <- max(april_data$RENT)
max_may <- max(may_data$RENT)
max_june <- max(june_data$RENT)
max_july <- max(july_data$RENT)     #calculate the max rent 

rentofmonth <- cbind(c(max_april,max_may,max_june,max_july),
                    c("April","May","June","July"))
colnames(rentofmonth) = c("MAXRENT","MONTH")

rentofmonth <- as.data.frame(rentofmonth)

rentofmonth$MONTH <- factor(rentofmonth$MONTH,levels=month.name) #this code is from (Stack Overflow, n.d.-a)

ggplot(rentofmonth,aes(MONTH,MAXRENT,group=1)) + geom_line(col="red") +
  geom_point(col="blue",size=3) + geom_text(aes(label=MAXRENT), vjust=-0.5) +
  labs(title="Maximum rent in each month", y="RENT")

#7.6 Analysis: Which month have the lowest rent 

april_data <- houserent_data %>% filter(month_rent =="Apr")
may_data <- houserent_data %>% filter(month_rent =="May")
june_data <- houserent_data %>% filter(month_rent =="Jun")
july_data <- houserent_data %>% filter(month_rent =="Jul")

min_april <- min(april_data$RENT)
min_may <- min(may_data$RENT)
min_june <- min(june_data$RENT)
min_july <- min(july_data$RENT)  #cal min rent of each month

rentofmonth <- cbind(c(min_april,min_may,min_june,min_july),
                     c("April","May","June","July"))
colnames(rentofmonth) = c("MINRENT","MONTH")

rentofmonth <- as.data.frame(rentofmonth)    #change to data frame form

rentofmonth$MONTH <- factor(rentofmonth$MONTH,levels=month.name)  #change the month to date form

ggplot(rentofmonth,aes(MONTH,MINRENT,group=1)) + geom_line(col="#50305F") +
  geom_point(col="#9066A5",size=3) + geom_text(aes(label=MINRENT), vjust=-0.5) +
  labs(title="Minimum rent in each month", y="RENT")

#7.7 Analysis: What is the mean rent for each month

april_data <- houserent_data %>% filter(month_rent =="Apr")
may_data <- houserent_data %>% filter(month_rent =="May")
june_data <- houserent_data %>% filter(month_rent =="Jun")
july_data <- houserent_data %>% filter(month_rent =="Jul")

mean_april <- round(mean(april_data$RENT),2)
mean_may <- round(mean(may_data$RENT),2)
mean_june <- round(mean(june_data$RENT),2)
mean_july <- round(mean(july_data$RENT),2)   #cal average rent and round up to 2 decimal places

rentofmonth <- cbind(c(mean_april,mean_may,mean_june,mean_july),
                     c("April","May","June","July"))
colnames(rentofmonth) = c("MEANRENT","MONTH")

rentofmonth <- as.data.frame(rentofmonth)

rentofmonth$MONTH <- factor(rentofmonth$MONTH,levels=month.name)

ggplot(rentofmonth,aes(MONTH,MEANRENT,group=1)) + geom_line(col="#50305F") +
  geom_point(col="#9066A5",size=3) + geom_text(aes(label=MEANRENT), vjust=-0.5) +
  labs(title="Mean rent in each month", y="RENT")


#7.8 Analysis: How area type affects the rent

ggplot(houserent_data,aes(AREA_TYPE,RENT)) + 
  geom_point(aes(shape=factor(AREA_TYPE),col=factor(AREA_TYPE))) + 
  labs(title="The rent of each area type",x="Area Type",
       col="Area Type",shape="Area Type")

houserent_data[houserent_data$AREA_TYPE=="Built Area",]  #show the rent of built area
  

#7.9 Analysis: Which city have the most expansive house rent


kol_rent <- houserent_data %>% filter(CITY=="Kolkata")
bang_rent <- houserent_data %>% filter(CITY=="Bangalore")
chen_rent <- houserent_data %>% filter(CITY=="Chennai")
del_rent <- houserent_data %>% filter(CITY=="Delhi")
hyder_rent <- houserent_data %>% filter(CITY=="Hyderabad")
mum_rent <- houserent_data %>% filter(CITY=="Mumbai")     #filter each city

kol_max <- max(kol_rent$RENT)
bang_max <- max(bang_rent$RENT)
chen_max <- max(chen_rent$RENT)
del_max <- max(del_rent$RENT)
hyder_max <- max(hyder_rent$RENT)
mum_max <- max(mum_rent$RENT)       #cal maax rent

city_rent <- cbind(c(kol_max,bang_max,chen_max,
                     del_max,hyder_max,mum_max),
                   c("Kolkata","Bangalore","Chennai",
                     "Delhi","Hyderabad","Mumbai"))    #combine 2 vectors 

city_rent <- as.data.frame(city_rent)   #change to data frame so can plot

colnames(city_rent) <- c("MAXRENT","CITY")

ggplot(city_rent,aes(CITY,MAXRENT,group=1)) + geom_line(col="black") +
  geom_point(col="pink",size =3) + labs(title="Max Rent in each City",
                                        x="CITY",y="RENT") +
  geom_text(aes(label=MAXRENT),vjust=-0.5)


#7.10 Analysis: Which city have the cheapest house rent

kol_rent <- houserent_data %>% filter(CITY=="Kolkata")
bang_rent <- houserent_data %>% filter(CITY=="Bangalore")
chen_rent <- houserent_data %>% filter(CITY=="Chennai")
del_rent <- houserent_data %>% filter(CITY=="Delhi")
hyder_rent <- houserent_data %>% filter(CITY=="Hyderabad")
mum_rent <- houserent_data %>% filter(CITY=="Mumbai")

kol_min <- min(kol_rent$RENT)
bang_min <- min(bang_rent$RENT)
chen_min <- min(chen_rent$RENT)
del_min <- min(del_rent$RENT)
hyder_min <- min(hyder_rent$RENT)
mum_min <- min(mum_rent$RENT)        #cal min rent

city_rent <- cbind(c(kol_min,bang_min,chen_min,
                     del_min,hyder_min,mum_min),
                   c("Kolkata","Bangalore","Chennai",
                     "Delhi","Hyderabad","Mumbai"))

city_rent <- as.data.frame(city_rent)

colnames(city_rent) <- c("MINRENT","CITY")


ggplot(city_rent,aes(CITY,MINRENT)) + geom_bar(stat="identity",col="brown",
                                                 fill="purple") +
  geom_text(aes(label=MINRENT),vjust=-1, col="red") + 
  labs(title="Bar chart of min rent for each city", y ="RENT")


#7.11 Analysis: Which furnishing status are the most expansive

ggplot(houserent_data,aes(FURNISHING_STATUS,RENT,col=FURNISHING_STATUS)) + 
  geom_boxplot() +
  ylim(0,1300000) + labs(title="Boxplot of rent for each furnishing status",
                         x="Status",col="Furnishing Status")


#7.12 Analysis: Will the number of bathrooms affect the price

ggplot(houserent_data,aes(factor(BATHROOM),RENT,fill=factor(BATHROOM))) + 
  geom_bar(stat="identity") +  
  labs(title="Rent of different number of bathrooms",
       fill="No of Bathroom",x="Bathroom") 


#7.13 Analysis: Which contact point cost more in rent

ggplot(houserent_data,aes(POINT_OF_CONTACT,RENT,col=POINT_OF_CONTACT)) + 
  geom_point() + labs(title="Plot of the rent for each contact point",
                      x="Point of Contact",col="Contact Point") +
  scale_color_manual(values=c("#FF9EE6","#AAE2E1","#EFF382"))


houserent_data[houserent_data$POINT_OF_CONTACT=="Builder",]


#7.14 Analysis: Will the floor of house rent affected the rent


ggplot(houserent_data,aes(Floor_Rent,RENT)) + 
  geom_bar(stat="identity",fill="#C8B37D") + 
  labs(title="Bar Chart of rent of each floor",x="Floor")


#8.0 Question 6: How size being affected
#8.1 Analysis: Will size of a house restricted by area type

ggplot(houserent_data,aes(AREA_TYPE,SIZE,fill=AREA_TYPE)) + geom_boxplot() +
  labs(title="Boxplot of size for each area type",x="Area Type",fill="Area Type")


#8.2 Analysis: What is the average size of houses in each city

kol_size <- houserent_data %>% filter(CITY=="Kolkata")
bang_size <- houserent_data %>% filter(CITY=="Bangalore")
chen_size <- houserent_data %>% filter(CITY=="Chennai")
del_size <- houserent_data %>% filter(CITY=="Delhi")
hyder_size <- houserent_data %>% filter(CITY=="Hyderabad")
mum_size <- houserent_data %>% filter(CITY=="Mumbai")

kol_mean <- round(mean(kol_size$SIZE),2)
bang_mean <- round(mean(bang_size$SIZE),2)
chen_mean <- round(mean(chen_size$SIZE),2)
del_mean <- round(mean(del_size$SIZE),2)
hyder_mean <- round(mean(hyder_size$SIZE),2)
mum_mean <- round(mean(mum_size$SIZE),2)     #cal avaerage size and with 2 decimal places

city_size <- cbind(c(kol_mean,bang_mean,chen_mean,
                     del_mean,hyder_mean,mum_mean),
                   c("Kolkata","Bangalore","Chennai",
                     "Delhi","Hyderabad","Mumbai"))

city_size <- as.data.frame(city_size)

colnames(city_size) = c("MEAN_SIZE","CITY")

city_size$MEAN_SIZE <- as.numeric(city_size$MEAN_SIZE) 

ggplot(city_size,aes(CITY,MEAN_SIZE,group=1)) + geom_line(col="blue") + 
  geom_text(aes(label=MEAN_SIZE),vjust=-1) + 
                  labs(title="Line graph of the mean rent for each city",
                       y="Size") 

#8.3 Analysis: Will the total number of the floors affect the size of the house

size_floors <- houserent_data

size_floors$No_Of_Floor <- as.numeric(houserent_data$No_Of_Floor)

ggplot(size_floors,aes(factor(No_Of_Floor),SIZE,col=factor(No_Of_Floor))) + 
  geom_point() + labs(x="No of floors", col="No of floors")


#8.4 Analysis: Will the number of bathrooms affect the size of house

levels(as.factor(houserent_data$BATHROOM))

filtered_data <- houserent_data %>% filter(BATHROOM=="1")
filtered_data2 <- houserent_data %>% filter(BATHROOM=="2")
filtered_data3 <- houserent_data %>% filter(BATHROOM=="3")
filtered_data4 <- houserent_data %>% filter(BATHROOM=="4")
filtered_data5 <- houserent_data %>% filter(BATHROOM=="5")
filtered_data6 <- houserent_data %>% filter(BATHROOM=="6")
filtered_data7 <- houserent_data %>% filter(BATHROOM=="7")
filtered_data8 <- houserent_data %>% filter(BATHROOM=="10")

one_bath <- max(filtered_data$SIZE)
two_bath <- max(filtered_data2$SIZE)
three_bath <- max(filtered_data3$SIZE)
four_bath <- max(filtered_data4$SIZE)
five_bath <- max(filtered_data5$SIZE)
six_bath <- max(filtered_data6$SIZE)
sev_bath <- max(filtered_data7$SIZE)
ten_bath <- max(filtered_data8$SIZE)     #cal max size of each house with diffrent number of bathrooms

bath_size <- cbind(c(one_bath,two_bath,three_bath,four_bath,five_bath,
                     six_bath,sev_bath,ten_bath),
                   c("1","2","3","4","5","6","7","10"))

bath_size <- as.data.frame(bath_size)

colnames(bath_size) = c("SIZE","BATHROOM")

bath_size$BATHROOM <- as.numeric(bath_size$BATHROOM)

ggplot(bath_size,aes(factor(BATHROOM),SIZE,group=1)) + 
  geom_point(col="#60C179",size=3) +
  geom_line(col="#7062E5") + geom_text(aes(label=SIZE),vjust=-0.5) +
  labs(x="BATHROOM")


#9.0 What kind of houses people will rent in certain city
#9.1 What is the rent for Bangalore 

filtered_data <- houserent_data %>% filter(CITY=="Bangalore")

plot(filtered_data$RENT,type="l",col="red",
     main="Graph of rent of Bangalore against counts",xlab="Counts",ylab="Rent")


#9.2 Analysis: What is the rent for Chennai

filtered_data <- houserent_data %>% filter(CITY=="Chennai")

options(scipen=999)
plot(filtered_data$RENT,type="l",col="orange",
     main="Graph of rent of Chennai against counts",xlab="Counts",ylab="Rent")

#9.3 Analysis: What is the rent for Delhi

filtered_data <- houserent_data %>% filter(CITY=="Delhi")

plot(filtered_data$RENT,type="l",col="#E7E000",
     main="Graph of rent of Delhi against counts",xlab="Counts",ylab="Rent")

#9.4 Analysis: What is the rent for Hyderabad

filtered_data <- houserent_data %>% filter(CITY=="Hyderabad")

plot(filtered_data$RENT,type="l",col="Green",
     main="Graph of rent of Hyderabad against counts",xlab="Counts",ylab="Rent")

#9.5 Analysis: What is the rent for Kolkata

filtered_data <- houserent_data %>% filter(CITY=="Kolkata")

plot(filtered_data$RENT,type="l",col="blue",
     main="Graph of rent of Kolkata against counts",xlab="Counts",ylab="Rent")

#9.6 Analysis: What is the rent for Mumbai

filtered_data <- houserent_data %>% filter(CITY=="Mumbai")

plot(filtered_data$RENT,type="l",col="purple",
     main="Graph of rent of Mumbai against counts",xlab="Counts",ylab="Rent")

#9.7 Analysis: Which city is the best choice for cheaper rent

filtered_data <- houserent_data %>% filter(CITY=="Bangalore")
filtered_data2 <- houserent_data %>% filter(CITY=="Chennai")
filtered_data3 <- houserent_data %>% filter(CITY=="Delhi")
filtered_data4 <- houserent_data %>% filter(CITY=="Hyderabad")
filtered_data5 <- houserent_data %>% filter(CITY=="Kolkata")
filtered_data6 <- houserent_data %>% filter(CITY=="Mumbai")

plot(filtered_data$RENT,type="l",col="red",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

lines(filtered_data2$RENT,type="l",col="orange",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

lines(filtered_data3$RENT,type="l",col="#E7E000",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

lines(filtered_data4$RENT,type="l",col="Green",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

lines(filtered_data5$RENT,type="l",col="blue",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

lines(filtered_data6$RENT,type="l",col="purple",
     main="Graph of rent against counts",xlab="Counts",ylab="Rent")

par(cex=0.55)   #bigger the font size of legend
legend("topright",legend=c("Bangalore","Chennai","Delhi"
                           ,"Hyderabad","Kolkata","Mumbai"),
        fill=c("red","orange","#E7E000","Green","blue","purple"),
       title="City",bty="n")

#    ignoring the outliers

filtered_data <- houserent_data %>% group_by(CITY) %>% count(RENT)

#            following code are from (Holtz, n.d.-b)
install.packages("hrbrthemes")
ggplot(filtered_data,aes(n,RENT,fill=factor(CITY))) + geom_area() + #use area chart because chart above was hard to interpret with multiple lines
  ggtitle("The rent in each city") +
  scale_y_continuous(labels=scales::comma) + ylim(0,2000000) +
  labs(x="COUNT",fill="CITY")                                       

#9.8 Analysis: What is the most popular BHK configuration in Bangalore

filtered_data <- houserent_data %>% filter(CITY=="Bangalore")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="blue",fill="white") +    #plot histogram with 1 binwidth
  labs(title="Histogram of BHK in Bangalore") 

#9.9 Analysis: What is the most popular BHK configuration in Chennai

filtered_data <- houserent_data %>% filter(CITY=="Chennai")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="pink",fill="purple") + 
  labs(title="Histogram of BHK in Chennai")

#9.10 Analysis: What is the most popular BHK configuration in Delhi

filtered_data <- houserent_data %>% filter(CITY=="Delhi")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="purple",fill="green") + 
  labs(title="Histogram of BHK in Delhi")

#9.11 Analysis: What is the most popular BHK configuration in Hyderabad

filtered_data <- houserent_data %>% filter(CITY=="Hyderabad")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="#CD9F95",fill="#FFECE8") + 
  labs(title="Histogram of BHK in Hyderabad")


#9.12 Analysis: What is the most popular BHK configuration in Kolkata

filtered_data <- houserent_data %>% filter(CITY=="Kolkata")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="#F555F7",fill="#E8E8FF") + 
  labs(title="Histogram of BHK in Kolkata")

#9.13 Analysis: What is the most popular BHK configuration in Mumbai

filtered_data <- houserent_data %>% filter(CITY=="Mumbai")

ggplot(filtered_data,aes(BHK)) + 
  geom_histogram(binwidth = 1,col="#FF6FE9",fill="black") + 
  labs(title="Histogram of BHK in Mumbai")

#9.14 Analysis: Which city have more rental house during April

filtered_data <- houserent_data %>% filter(month_rent=="Apr")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()
colnames(filtered_data) <- c("CITY","COUNT")

ggplot(filtered_data,aes(CITY,COUNT,fill=CITY)) + geom_bar(stat="identity") +
  scale_fill_manual(values=c("#948277","#789477","#778494",
                             "#887794","#947787","#D06774"))

#9.15 Analysis: Which city have more rental house during May

filtered_data <- houserent_data %>% filter(month_rent=="May")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()
colnames(filtered_data) <- c("CITY","COUNT")

pie(filtered_data$COUNT,filtered_data$COUNT,
    main="Pie Chart of count of houses in every city during May",
    col=c("#FFCCCC","#F9FFCC","#CCFFD1","#CCEBFF",
          "#E2CCFF","#FFCCE1"),radius=1.3)
legend("topright",filtered_data$CITY,
       fill=c("#FFCCCC","#F9FFCC","#CCFFD1","#CCEBFF",
              "#E2CCFF","#FFCCE1"),cex=0.44)

#9.16 Analysis: Which city have more rental house during June

filtered_data <- houserent_data %>% filter(month_rent=="Jun")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()
colnames(filtered_data) <- c("CITY","COUNT")

ggplot(filtered_data,aes(CITY,COUNT,group=1)) + geom_line(col="#2A70CF") +
  geom_point(col="#F94F4F")

#9.17 Analysis: Which city have more rental house during July

filtered_data <- houserent_data %>% filter(month_rent=="Jul")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()
colnames(filtered_data) <- c("CITY","COUNT")

pie3D(filtered_data$COUNT,labels=filtered_data$COUNT,
      col=c("#CD5959","#EBF47F","#88F47F",
            "#7FEFF4","#9A7FF4","#F47FE6",radius=1,labelcex=1),
      main="Pie Chart of Count of houses in cities during July")
par(xpd=TRUE)
legend(1.1,0.3,legend=filtered_data$CITY,cex=0.55,yjust=0.5,
       xjust=-0.1,fill=c("#CD5959","#EBF47F","#88F47F",
                         "#7FEFF4","#9A7FF4","#F47FE6"))


#10.0 Question 8: What to expect for different furnishing status houses
#10.1 Analysis: Furnished houses are more in which city

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Furnished")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()

ggplot(filtered_data,aes(CITY,n,fill=CITY)) + geom_bar(stat="identity") +
  geom_text(aes(label=n),vjust=-0.5) + labs(title="Bar Chart of number of furnished house of each city",
                                 y="Counts")

#10.2 Analysis: Semi-furnished houses are more in which city

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Semi-Furnished")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()

pie(filtered_data$n,filtered_data$n,main="Pie Chart of semi-furnished houses in each city",
    col=c("#8C533E","#868C3E","#438C3E","#3E8C85","#653E8C","#8C3E69"),radius=2)

legend("topright",legend=filtered_data$CITY,fill=c("#8C533E","#868C3E","#438C3E",
                                        "#3E8C85","#653E8C","#8C3E69"),
       cex=0.55)

#10.3 Analysis: Unfurnished houses are more in which city

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Unfurnished")

filtered_data <- filtered_data %>% group_by(CITY) %>% count()

ggplot(filtered_data,aes(CITY,n,group=1)) + geom_line(col="#582842") +
  geom_text(aes(label=n),vjust=-0.5) + labs(title="Line Graph of number of 
                                            unfurnished houses in each city",
                                 y="Counts")

#10.3 Analysis: Unfurnished houses usually have how many bathrooms

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Unfurnished")

filtered_data <- filtered_data %>% group_by(BATHROOM) %>% count()

ggplot(filtered_data,aes(factor(BATHROOM),n,fill=factor(BATHROOM))) + 
  geom_bar(stat="identity") + ylim(-2,900) + labs(y="Counts",x="No of Bathroom",
                                                  fill="No of Bathroom")

#10.5 Analysis: Semi-furnished houses usually have how many bathrooms

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Semi-Furnished")

filtered_data <- filtered_data %>% group_by(BATHROOM) %>% count()

ggplot(filtered_data,aes(factor(BATHROOM),n,col=factor(BATHROOM))) + 
  geom_point(size=5) + labs(y="Counts",x="No of Bathroom",col="No of Bathroom")

#10.6 Analysis: Furnished houses usually have how many bathrooms

filtered_data <- houserent_data %>% filter(FURNISHING_STATUS=="Furnished")

filtered_data <- filtered_data %>% group_by(BATHROOM) %>% count()

ggplot(filtered_data,aes(factor(BATHROOM),n,fill=factor(BATHROOM))) + 
  geom_bar(stat="identity") + ylim(-2,400) + labs(y="Counts",x="No of Bathroom",
                                                  fill="No of Bathroom") +
  geom_text(aes(label=n),vjust=-0.5,col="blue") 


#11.0 Question 9: Why people will want to live in remote area of each city
#11.1 Analysis: Compare the remote area and lively area of Bangalore

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Bangalore") %>%
  group_by(AREA_LOCALITY)%>% count() 

filtered_data <- arrange(filtered_data,n)    #arrange data in asc order
View(filtered_data)

head(filtered_data)   #get the first 6 rows from the data

remote_area <- houserent_data %>% filter(houserent_data$AREA_LOCALITY=="Aarna Enclave"|
                                           houserent_data$AREA_LOCALITY=="Abbiareddy Layout, Kaggadasapura"|
                                           houserent_data$AREA_LOCALITY=="Aditya Nagar-Vidyaranyapura, Vidyaranyapura"|
                                           houserent_data$AREA_LOCALITY=="Aduru"
                                  |houserent_data$AREA_LOCALITY=="Agrahara Layout"
                                  |houserent_data$AREA_LOCALITY=="Ags Layout, Hebbal")  #filter out the data which meets the conditions
View(remote_area)

#11.2 Analysis: Compare the similarities of houses in remote area of Chennai

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Chennai") %>%
  group_by(AREA_LOCALITY)%>% count() 

filtered_data <- filter(filtered_data,n=="1")   #filter out rows which counts equals to 1

View(filtered_data)

head(filtered_data)  #get the first 6 rows data

remote_area <- houserent_data %>% filter(houserent_data$AREA_LOCALITY=="2nd Main Road"|
                                           houserent_data$AREA_LOCALITY=="355 konnur highroad Ayanavaram"|
                                           houserent_data$AREA_LOCALITY=="58 block"|
                                           houserent_data$AREA_LOCALITY=="Adyar, Thiruvanmiyur, Chennai"
                                         |houserent_data$AREA_LOCALITY=="Alapakkam, Porur"
                                         |houserent_data$AREA_LOCALITY=="Alwarthirunagar")    #filter out data meets the requirements

View(remote_area)

#11.3 Analysis: Compare the similarities of houses in remote area of Delhi

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Delhi") %>%
  group_by(AREA_LOCALITY)%>% count() 

filtered_data <- filter(filtered_data,n=="1")


tail(filtered_data)   #get the last 6 rows 

remote_area <- houserent_data %>% filter(houserent_data$AREA_LOCALITY=="Vinod Nagar West"|
                                           houserent_data$AREA_LOCALITY=="Viswas Park, Matiala"|
                                           houserent_data$AREA_LOCALITY=="Vivek Vihar"|
                                           houserent_data$AREA_LOCALITY=="Vivek Vihar Phase 1"
                                         |houserent_data$AREA_LOCALITY=="Vivek Vihar Phase 2"
                                         |houserent_data$AREA_LOCALITY=="West End")

View(remote_area)

#11.4 Analysis: Compare the similarities of houses in remote area of Hyderabad

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Hyderabad") %>%
  group_by(AREA_LOCALITY) %>% filter(n()==1)  #some source code refers to (Stack Overflow, n.d.-h)
                                                #easier way compared to before analysis

remote_area <- filtered_data[sample(1:nrow(filtered_data),6),]   #get 6 rows randomly 

View(remote_area)

#11.5 Analysis: Compare the similarities of houses in remote area of Kolkata

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Kolkata") %>%
  group_by(AREA_LOCALITY) %>% filter(n()==1)  

remote_area <- filtered_data[sample(1:nrow(filtered_data),6),]

View(remote_area)

#11.6 Analysis: Compare the similarities of houses in remote area of Mumbai

filtered_data <- houserent_data %>% filter(houserent_data$CITY =="Mumbai") %>%
  group_by(AREA_LOCALITY) %>% filter(n()==1)  

remote_area <- filtered_data[sample(1:nrow(filtered_data),6),]

View(remote_area)


#12.0 Question 10: Reasons people would like to rent basement

filtered_data <- filter(houserent_data,Floor_Rent=="Upper" 
                        | Floor_Rent=="Lower")


#12.1 Whether relating to BHK 

bhk_basement <- filtered_data %>% group_by(BHK) %>% count()

#              following source code are from (plotly, n.d.)
library(plotly)

fun_chart <- plot_ly()     #to plot a funnel chart

fun_chart <- fun_chart %>% add_trace(
  type="funnel" , y=factor(bhk_basement$BHK), x=bhk_basement$n    #add trace
)

fun_chart <- fun_chart %>% layout(yaxis=list(categoryarray=bhk_basement$BHK))  #specify complex plot

fun_chart  #show the plot


#12.2 Whether relating to rent

options(scipen=999)
plot(filtered_data$RENT, type="l", main="Line graph of rent for basement", xlab="Counts",
     ylab="Rent",col="#7700C0", lwd=4)    #increase the thickness of line 
                                          #lwd is refers to (Tutorial Kart, n.d.)

summary(houserent_data$RENT)

summary(filtered_data$RENT)


#12.3 Whether relating to size

#               following source code are from (Zach, 2022)
ggplot(filtered_data,aes(SIZE,RENT,size=after_stat(count))) +
  geom_point(alpha=0.5,col="#356966") +   #alpha is to adjust the opacity
  scale_size(range=c(2,10),name="COUNTS")   #limit the size of the points


#12.4 Whether relating to area type

area_base <- filtered_data %>% group_by(AREA_TYPE) %>% count()

ggplot(area_base, aes(AREA_TYPE,n,fill=AREA_TYPE)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#8033FF","#3272D5")) +
  geom_text(aes(label=n),vjust=-0.5,col="#B3B763") + 
  labs(title="bar chart of counts of area type",
                                 x="Area Type",y="Counts",fill="Area Type")


#12.5 Whether relating to city

city_base <- filtered_data %>% group_by(CITY) %>% count()

pie(city_base$n,city_base$n,radius=1,clockwise = FALSE,main="pie chart of counts of basement
    in each city",col=c("blue","red","pink","yellow","grey","purple"))
legend("topright",city_base$CITY,cex=0.5,
       fill=c("blue","red","pink","yellow","grey","purple"),bty="n")

#12.6 Whether relating to furnishing status

furs_base <- filtered_data %>% group_by(FURNISHING_STATUS) %>% count()

pie3D(furs_base$n, labels=furs_base$n, 
      main="Pie Chart for count of furnishing status",radius=1,
      labelcex = 1,explode = 0.2)
par(xpd=TRUE)
legend(1,0.7,legend=furs_base$FURNISHING_STATUS,cex=0.55,yjust=0.5,xjust=-0.1,
       fill=c("red","green","blue"),bty="n")  #n bty is to remove the boader


#12.7 Whether relating to number of bathrooms

bath_base <- filtered_data %>% group_by(BATHROOM) %>% count()

ggplot(bath_base,aes(factor(BATHROOM),n,col=factor(BATHROOM))) + 
  geom_point(size=3) + labs(title="Counts of BATHROOM numbers of basement",
                            y="counts",x="Number of bathrooms",col="bathroom")


#12.8 Whether relating to number of number of floors

floors_base <- filtered_data %>% group_by(No_Of_Floor) %>% count()

#            following source code are from (GeeksforGeeks, 2021)
ggplot(floors_base,aes(No_Of_Floor,n)) + 
  geom_segment(aes(x=No_Of_Floor,xend=No_Of_Floor,y=0,yend=n,col="#FAE5D3")) +   #control the height of the line
  geom_point(size=4,col="#A569BD") + 
  geom_label(aes(No_Of_Floor,n,label=signif(n)),colour="#6A2A1D",nudge_x=0.35,size=5) +   #signif is to add a rectangular border around the counts label
  labs(y="counts", x="number of floors")



library(plotrix)
library(ggplot2)
library(dplyr)


