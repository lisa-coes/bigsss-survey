if (!require("pacman")) install.packages("pacman") # instalar pacman
pacman::p_load(tidyverse,haven,sjlabelled,sjmisc,sjPlot,stringr)

#STEP 0:load data from excel file
df1 <- 
  xlsx::read.xlsx(file = here::here("input/data/original/BIGSSS Doctoral Fellow Survey 2023(1-32).xlsx"),
                  sheetIndex = 1)



# STEP 1: set new variable names
varnames<- names(df1) #get the variable names as variable labels
names(df1) <- paste0("v",1:length(varnames))

# STEP 2: retreive variable labels from variable names
#save variable labels
varlab<- stringr::str_replace_all(string = varnames,pattern = "\\.",replacement = " ")


# STEP 3: set likert variables as factors 
#all the factor variables
tofactor<- names(select(df1,v6:v33,v35:v45,v47:v55,v57:v61,v63:v68))
#transform all the variables to factors
df1[tofactor]<- as_factor(df1[tofactor])
sjPlot::view_df(df1,show.frq = T,show.string.values = T)
#reorder factor levels
#variables with agree to disagree

for (i in names(select(df1,v35:v45))) {
  df1[[i]] <- car::recode(df1[[i]],"'Disgree'='Disagree'")  #fix typo 
}

for (i in names(select(df1,v10:v32,v35:v45,v47:v55,v57:v61,v63:v68))) {
  df1[[i]] <- factor(df1[[i]],
                     levels = rev(c("Strongly agree","Agree","Neutral",
                                    "Disagree","Strongly disagree",
                                    "NA/Don't know"))) 
}
sjPlot::view_df(df1,show.frq = T,show.string.values = T)

labels <-  c("-999", "Strongly disagree","Disagree","Neutral","Agree","Strongly agree")
levels <- rev(c("Strongly agree","Agree","Neutral","Disagree","Strongly disagree","NA/Don't know"))

for (i in names(select(df1,v10:v32,v35:v45,v47:v55,v57:v61,v63:v68))) {
  df1[[i]] <- factor(df1[[i]],levels =levels,labels = labels) 
}

sjPlot::view_df(df1,show.frq = T,show.string.values = T)

frq(df1$v33)
df1$v33 <- factor(df1$v33,
                  levels = rev(c("Very satisfied","Somewhat satisfied",
                             "Neither satisfied nor dissatisfied",
                             "Somewhat dissatisfied","Very dissatisfied","NA/Don't know")),
                  labels = rev(c("Very satisfied","Somewhat satisfied",
                                 "Neither satisfied nor dissatisfied",
                                 "Somewhat dissatisfied","Very dissatisfied","-999")))



select(df1,v6:v33,v35:v45,v47:v55,v57:v61,v63:v68) %>% frq()


# STEP 4: set variable labels recovered from STEP 1
df1 <- sjlabelled::set_label(x = df1,label = varlab)
get_label(df1) #OK!


# STEP 5: multiple options variables
v70 <- as.data.frame(str_split_fixed(string = df1$v70,pattern = ";",n = 3))
names(v70) <- c("v70_1","v70_2","v70_3")
v70$v70_3 <- stringr::str_replace_all(string = v70$v70_3 ,pattern = "\\;",replacement = "") 
v70[v70==""] <- NA

df2 <- bind_cols(df1,v70)
df2$v70_1 <- as.factor(df2$v70_1)
frq(df2$v70_1)
df2$v70_2 <- as.factor(df2$v70_2)
frq(df2$v70_2)
df2$v70_3 <- as.factor(df2$v70_3)
frq(df2$v70_3)


levels_v70 <- 
c("Contacts with BIGSSS fellows or faculty",
"Funding (e.g., scholarship)",
"Academic excellence in the social sciences",
"Structured doctoral curriculum",
"Freedom to pursue your own research project",
"Close supervision through a doctoral committee",
"The opportunity to work interdisciplinarily",
"The international character of BIGSSS",
"Being part of a cohort of other PhD students",
"Wanted to live in Germany and/or Bremen, specifically",
"It was required for your job",
"Good working conditions (e.g., a work contract)")

labelsv70 <- paste0(c("A","B","C","D",
                      "E","F","G","H",
                      "I","J","K","L"),". ",levels_v70)

frq(df2$v70_1)
df2$v70_1 <- factor(df2$v70_1,levels = levels_v70,labels = labelsv70)
set_label(df2$v70_1) <- "Most important behind decision PhD at BIGSSS: First mention"
frq(df2$v70_1)

frq(df2$v70_2)
df2$v70_2 <- factor(df2$v70_2,levels = levels_v70,labels = labelsv70)
set_label(df2$v70_2) <- "Most important behind decision PhD at BIGSSS: Second mention"
frq(df2$v70_2)

frq(df2$v70_1)
df2$v70_3 <- factor(df2$v70_3,levels = levels_v70,labels = labelsv70)
set_label(df2$v70_3) <- "Most important behind decision PhD at BIGSSS: Third mention"
frq(df2$v70_3)


#check the dataset
df2$v71 <- NULL
sjPlot::view_df(df2,show.frq = T,
                show.string.values = T,
                show.prc = T,
                file = here::here("output/codebook.html"))

save(df2,file = here::here("input/data/proc/bigsss_2023.RData"))

haven::write_dta(df2,path = here::here("input/data/proc/bigsss_2023.dta"))
haven::write_sav(df2,path = here::here("input/data/proc/bigsss_2023.sav"))
