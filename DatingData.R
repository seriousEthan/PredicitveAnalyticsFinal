#Check out my packages
require(ISLR)
require(boot)
require(caret)
require(ggplot2)
require(plyr)
require(ROCR)
require(ranger)
#Aquire all the data. This dataset contains 8378 observations of  195 variables.
dating <-read.csv("Speed Dating Data.csv")
# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}


#Examine the dataset
summary(dating)
str(dating)
head(dating)
tail(dating)

#-Drop some variables to make the data more manageable----------------
#and I do not have a super computer so this will need to be simplified to work with the resourses available.

dating <- dating[,-(1:2),drop=FALSE]
dating <- subset( dating, select = -c( idg : pid ))
dating$samerace <- NULL
dating <- subset( dating, select = -c( race_o : met_o ))
dating <- subset( dating, select = -c( tuition : from ))
dating <- subset( dating, select = -c( career : yoga ))
dating <- subset( dating, select = -c( attr1_1 : amb5_3 ))
dating$income <- NULL
dating$field <- NULL
#Rename the columns for clarity
colnames(dating) <- c('gender', 'matchYN', 'intRate', 'agePart', 'age', 'field', 'almaMater', 'brains', 'home',  
                       'goal', 'dateFreq', 'socialScore', 'attitude', 'swagLevel')
Tidy1Dating<- dating

#Examine the dataset
summary(dating)
str(dating)

#swagLevel has too many NA's so I am going to remove that
dating$swagLevel <- NULL

#look closer at remaining columns
summary(dating$almaMater)

#the number of factors needs to be reduced time to create new catagories.

#--Schools in north and south america outside the US---------------------------

dating$almaMater <- gsub('^University of Toronto', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Universidad Iberoamericana', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Universidad de Chile', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Universidad de Costa Rica', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Rutgers University - New Brunswick', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^school of social sciences in uruguay', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^U. del Rosario, Medicine, Colombia SA', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Universidad Catolica de Chile', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^Universidad de los Andes', 'Foreign American School', dating$almaMater)
dating$almaMater <- gsub('^McGill University', 'Foreign American School', dating$almaMater)



#---Shools in Europe, and Russia---------------------------------------------
#(this is also where I learned that you need to put Oxford University before Oxford, for example, or you end up with almaMater University)

dating$almaMater <- gsub('^LUISS, Rome', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Oxford University', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Oxford', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Cologne, Germany', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Reading, England', 'European School', dating$almaMater)
dating$almaMater <- gsub('^charles university, prague, czech republic', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Paris', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Naples, Italy', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Genova', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Warsaw', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Ecole Normale SupZrieure, Paris', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Novosibirsk State University', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Univeristy of Barcelona', 'European School', dating$almaMater)
##dating$almaMater <- gsub('^European School University', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Cambridge University', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Bocconi University Milan', 'European School', dating$almaMater)
dating$almaMater <- gsub('^ecole polytechnique', 'European School', dating$almaMater)
dating$almaMater <- gsub('^HEC FRance', 'European School', dating$almaMater)
dating$almaMater <- gsub('^MSU, Russia', 'European School', dating$almaMater)
##dating$almaMater <- gsub('^Supaero (France)', 'European School', dating$almaMater)  #NOT WORKING!!!!!!!!!!!!!!!!!!!!!!
dating$almaMater <- gsub('^University of Heidelberg', 'European School', dating$almaMater)
dating$almaMater <- gsub('^University of Karlsruhe/Germany', 'European School', dating$almaMater)
dating$almaMater <- gsub('^warsaw university', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Bucharest University', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Cagliari - Italy', 'European School', dating$almaMater)
dating$almaMater <- gsub('^Ecole Polytechnique (France)', 'European School', dating$almaMater)
##dating$almaMater <- gsub('^Ecole Superieure d'Electricite', 'European School', dating$almaMater) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#----Schools in Asia and the South Pacific--------------------------------

dating$almaMater <- gsub('^S.V Regional Engineering College,India', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Delhi University', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Beijing University', 'Asian School', dating$almaMater)
###dating$almaMater <- gsub('^ChungShenMedicalUniversity(Taiwan)', 'Asian School', dating$almaMater)  ##NOT WORKING!!!!!!!!!!!!!!!!!!!!!
dating$almaMater <- gsub('^MSU, Russia(Taiwan)', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Nirma Institute of Technology-India', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Tianjin University in China', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^REC, Rourkela', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Monash University - Australia', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^SOGANG UNIVERSITY(KOREA)', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Fudan University, Shanghai, China', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Fudan', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Harbin Medical University, China', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Kunitachi College of Music (in Japan)', 'Asian School', dating$almaMater) ##NOT WORKING!!!!!!!!!!!!!!!!!!!
dating$almaMater <- gsub('^National University of Singapore', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Rizvi College of Architecture, Bombay University', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Shia-Chian University', 'Asian School', dating$almaMater)
###dating$almaMater <- gsub('^SOGANG UNIVERSITY(KOREA)', 'Asian School', dating$almaMater) ##NOT WORKING!!!!!!!!!!!!!!!!!!!
dating$almaMater <- gsub('^Univ. of Bombay', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^University of Delhi', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^University of the Philippines', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Ateneo de Manila University - Philippines', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Bombay, India', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Fu Jen Catholic University, Taiwan', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Jesus and Mary College,Delhi', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^National Taiwan University', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^National University Of Singapore', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Taiwan University', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^university of the philippines', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^university of the philippines', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^China', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Harcourt Butler Technological Institute', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^Hebrew University', 'Asian School', dating$almaMater)
dating$almaMater <- gsub('^NUS', 'Asian School', dating$almaMater)

#dating$almaMater <- gsub('^Tokyo Woman's Christian University, Japan', 'Asian School', dating$almaMater) NOTWORKING!!!!!!!!!!!!!!!!!!!!!

#-----Group the Ivy Leauge together--------------------------------------------------------

dating$almaMater <- gsub('^Columbia Business School', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Columbia College, CU', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Columbia College', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^ColumbiaU', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Columbia', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Brown University', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Brown', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Cornell University', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Cornell', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Dartmouth College', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Harvard University', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Harvard College', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Harvard', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^harvard', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Princeton University', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Princeton U.', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Princeton', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^University of Pennsylvania', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^university of pennsylvania', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Univ of Pennsylvania', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^UPenn', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Yale University', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Yale', 'Ivy', dating$almaMater)
dating$almaMater <- gsub('^Ivy University', 'Ivy', dating$almaMater)


#------Group the little Ivies together--------------------------------
dating$almaMater <- gsub('Amherst College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Bowdoin College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Bucknell University', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Colgate University', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Conneticut College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Connecticut College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('colby college, waterville, me', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Hamilton College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Lafayette College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Middlebury College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Swarthmore College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Trinity College', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Tufts University', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Tufts', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('tufts', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('Wesleyan University', 'LittleIvy', dating$almaMater)
dating$almaMater <- gsub('LittleIvy University', 'LittleIvy', dating$almaMater)

#-------Womens Colleges--------------------------
dating$almaMater <- gsub('^wellesley college', 'Ladies', dating$almaMater)
dating$almaMater <- gsub('^Barnard College', 'Ladies', dating$almaMater)
dating$almaMater <- gsub('^Smith College', 'Ladies', dating$almaMater)
dating$almaMater <- gsub('^wellesley college', 'Ladies', dating$almaMater)


#--------Public Universities not in California------------------------------
dating$almaMater <- gsub('Arizona State', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Colorado State', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Florida International University', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('GA Tech', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Miami University', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Rutgers College', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Rutgers University', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('SUNY Binghamton', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('SUNY Stony Brook', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('SUNY Albany', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('SUNY Geneseo', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Texas A&M', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Binghamton University', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('U of  Michigan', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('U of Vermont', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Univ of New Mexico', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Univ. of Connecticut', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Akron, OH', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Arizona', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Florida', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Delaware', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Illinois/Champaign', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Kansas', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Maryland, and Oxford', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Massachusetts-Amherst', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Michigan', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Michigan-Ann Arbor', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of North Carolina at Charlotte', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Oregon', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Tennessee', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Texas', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Vermont', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Washington', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('University of Wisconsin-Madison', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('university of wisconsin/la crosse', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('UW Madison', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('UNLV', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Penn State University', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('PublicSchool-Ann Arbor', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Illinois', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('UM', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('umass', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Purdue', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('Univeristy of Michigan', 'PublicSchool', dating$almaMater)
dating$almaMater <- gsub('George Mason University', 'PublicSchool', dating$almaMater)



#---------California schools----------------------------------
dating$almaMater <- gsub('Berklee College Of Music', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Cal Berkeley', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Cal State Univ., Long Beach', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('California State University Los Angeles', 'CaliSchool', dating$almaMater)
##dating$almaMater <- gsub('Saint Mary's College of California', 'CaliSchool', dating$almaMater) !!!!!!!!!!!!!
dating$almaMater <- gsub('Santa Clara University', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Stanford University', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Stanford', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('u of southern california, economics', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('U.C. Berkeley', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UC Berkeley', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UC Davis', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UC Irvine', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UC Santa Cruz', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UC, IRVINE!!!!!!!!!', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('ucla', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UCLA', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('UCSB', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Univeristy of California, Davis', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('University of California at Santa Barbara', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('University of California at Santa Cruz', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('University of Southern California', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('CSUN', 'CaliSchool', dating$almaMater)
dating$almaMater <- gsub('Harvey Mudd College (Physics)', 'CaliSchool', dating$almaMater)



#----------Large Private Universities (undergrad > 7,500)--------------------------------
dating$almaMater <- gsub('Boston College', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('MIT', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('New York University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Notre Dame', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Northwestern University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('NYU', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('nyu', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Syracuse University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Texas State University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Vanderbilt University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('George Washington University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Fordham University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Georgetown University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Georgetown', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('GW', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Emory University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('American University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('HOWARD UNIVERSITY', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('John Hopkins', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('PACE University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Queens College', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('University of Chicago', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('University of Rochester', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Wake Forest', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Washington U. in St. Louis', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('washington university in st louis', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('Washington University in St. Louis', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('William and Mary', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('BigPrivate University', 'BigPrivate', dating$almaMater)
dating$almaMater <- gsub('The BigPrivate', 'BigPrivate', dating$almaMater)


#-----------Smaller Private schools-------------------------------------
dating$almaMater <- gsub('^Loyola College in Maryland', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Loyola College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Bennington College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Brandeis University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Case Western Reserve University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^COOPER UNION', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Cooper Union, Bard college, and SUNY Purchase', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Duquesne University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Augustana College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Hampshire College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Holy Cross', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^lipscomb university', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Mary Baldwin College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^marymount manhattan college', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Oberlin College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^oberlin', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Occidental College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Rice University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Rice', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^RPI', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Sarah Lawrence College', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Southwestern University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Kettering University / GMI', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^Southwestern University', 'SmallPrivate', dating$almaMater)
dating$almaMater <- gsub('^SmallPrivate College', 'SmallPrivate', dating$almaMater)


#------------Group some random stuff under Other and re-factor it------------------------

dating$almaMater <- gsub('^Engineering', 'Other', dating$almaMater)
dating$almaMater <- gsub('^medicine', 'Other', dating$almaMater)
##dating$almaMater <- gsub('^school overseas (need a name ?)', 'Other', dating$almaMater) ###NOT WORKING!!!!!!!!!!!!!!!!!!
dating$almaMater <- gsub('^tech school', 'Other', dating$almaMater)
dating$almaMater <- gsub('^University of International Business  & Economics', 'Other', dating$almaMater)
dating$almaMater <- gsub('^Biological Sciences', 'Other', dating$almaMater)
dating$almaMater <- gsub('^School of the Arts', 'Other', dating$almaMater)
dating$almaMater <- gsub('^SEAS', 'Other', dating$almaMater)

dating$almaMater <- as.factor(dating$almaMater)
levels(dating$almaMater)

#-------------Some of the levels are not being corrected with gsub so I am using a different method to fix that------
levels(dating$almaMater)[1] <- "Other"
levels(dating$almaMater)[5] <- "Asian School"
levels(dating$almaMater)[6] <- "European School"
levels(dating$almaMater)[7] <- "CaliSchool"
levels(dating$almaMater)[8]<- "Asian School"
levels(dating$almaMater)[14] <- "Asian School"
levels(dating$almaMater)[15] <- "Asian School"
levels(dating$almaMater)[11] <- "CaliSchool"
levels(dating$almaMater)[11] <- "Other"
levels(dating$almaMater)[12] <- "European School"
levels(dating$almaMater)[12] <- "SmallPrivate"

#--------------Finally!!-----------------
summary(dating$almaMater)
TidywalmaMater <- dating
#Other is a little too large, but i like how well everything else is distributed
#Now to slim down the number of factors in the brains column
#Looks like about 3000 people responded so I am going to split them into an approximately
#even set of three levels, "low", "med", and "high" and While I'm at it I will rename " " "na."
summary(dating$brains)
levels(dating$brains)

levels(dating$brains)[1] <- "na"
levels(dating$brains)[2:43] <- "low"
levels(dating$brains)[3:18] <- "mid"
levels(dating$brains)[4:13] <- "high"

#Time to look at home
summary(dating$home)
levels (dating$home)
#I would like to use this, but there is no way to simplify this really.  An entire Zip code
#is a large enough area as it is.  I am going to combine " " "0" and "(Other)" together at least.
levels(dating$home)[1:2] <- "Other"

#---------------correctly identify things------------------------
dating$gender <- factor(dating$gender)
summary(dating$gender)
levels(dating$gender)[1]<- "female"
levels(dating$gender)[2]<- "male"

dating$matchYN <- factor(dating$matchYN)
summary(dating$matchYN)
levels(dating$matchYN)[1] <- "no"
levels(dating$matchYN)[2] <- "yes"

dating$field <- factor(dating$field)

levels(dating$field)[1] <- "Law"
levels(dating$field)[2] <- "Math"
levels(dating$field)[3] <- "Social Science"
levels(dating$field)[4] <- "Medical Science"
levels(dating$field)[5] <- "Engineering"
levels(dating$field)[6] <- "English"
levels(dating$field)[7] <- "History/Philosophy"
levels(dating$field)[8] <- "Business"
levels(dating$field)[9] <- "Education"
levels(dating$field)[10] <- "Science"
levels(dating$field)[11] <- "Social Work"
levels(dating$field)[12] <- "Undecided"
levels(dating$field)[13] <- "PolySci"
levels(dating$field)[14] <- "Film"
levels(dating$field)[15] <- "Fine Arts"
levels(dating$field)[16] <- "Languages"
levels(dating$field)[17] <- "Architecture"
levels(dating$field)[18] <- "Other"

summary(dating$field)
levels(dating$field)
#I could combine Film and Fine Arts maybe....gonna leave it alone for now

dating$goal <- factor(dating$goal)

levels(dating$goal)[1] <- "Seemed like a fun night out"
levels(dating$goal)[2] <- "To meet new people"
levels(dating$goal)[3] <- "To get a date"
levels(dating$goal)[4] <- "Looking for a serious relationship"
levels(dating$goal)[5] <- "To say I did it"
levels(dating$goal)[6] <- "Other"

summary(dating$goal)

dating$dateFreq <- factor(dating$dateFreq)

levels(dating$dateFreq)[1] <- "Several times a week"
levels(dating$dateFreq)[2] <- "Twice a week"
levels(dating$dateFreq)[3] <- "Once a week"
levels(dating$dateFreq)[4] <- "Twice a month"
levels(dating$dateFreq)[5] <- "Once a month"
levels(dating$dateFreq)[6] <- "Several times a year"
levels(dating$dateFreq)[7] <- "Almost Never"

dating$socialScore <- factor(dating$socialScore)

levels(dating$socialScore)[1] <- "Several times a week"
levels(dating$socialScore)[2] <- "Twice a week"
levels(dating$socialScore)[3] <- "Once a week"
levels(dating$socialScore)[4] <- "Twice a month"
levels(dating$socialScore)[5] <- "Once a month"
levels(dating$socialScore)[6] <- "Several times a year"
levels(dating$socialScore)[7] <- "Almost never"

summary(dating$socialScore)

TidydataPrePurge <- dating
#----------------I think the data is TIDY!! YAY!! Lets Graph some stuff!---------------
#-----------------Match Level by Alma Mater-------------------------------------------------------

# create a dataframe
df1 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$almaMater))
names(df1) <- c('match', 'almaMater', 'count')
df1
# calculate the percentages
df1 <- ddply(df1, .(almaMater), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df1 <- ddply(df1, .(almaMater), transform, pos = (cumsum(count) - 0.5 * count))
df1$label <- paste0(sprintf("%.0f", df1$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df1, aes(x = almaMater, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Alma Mater')

#People from the Ladies group and  the small private schools are slightly more likely to match
#------------------Match Level with Different Fields of Study--------------------------------------------------------------------

# create a dataframe
df2 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$field))
names(df2) <- c('match', 'fieldOfStudy', 'count')
df2
# calculate the percentages
df2 <- ddply(df2, .(fieldOfStudy), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df2 <- ddply(df2, .(fieldOfStudy), transform, pos = (cumsum(count) - 0.5 * count))
df2$label <- paste0(sprintf("%.0f", df2$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df2, aes(x = fieldOfStudy, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level with Different Fields of Study')

#The different fields of study yield more diversity, poor math majors....also medical majors, lawyers, and people 
#who can speak foreign languages are very popular.
#-------------------Match Level by Intelligence---------------------------------------------------------------------

# create a dataframe
df3 <- data.frame(table(TidywalmaMater$matchYN, TidywalmaMater$brains))
names(df3) <- c('match', 'intelligence', 'count')
df3
# calculate the percentages
df3 <- ddply(df3, .(intelligence), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df3 <- ddply(df3, .(intelligence), transform, pos = (cumsum(count) - 0.5 * count))
df3$label <- paste0(sprintf("%.0f", df3$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df3, aes(x = intelligence, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Intelligence')

#not too much going on here,  at first glance, high portion of na. probably should get rid of this one.
#I just came back to look at it with a different grouping.  this time I used 1,000s, 1100s, etc. going to look at new graph...
#summary(TidywalmaMater$brains)
#levels(TidywalmaMater$brains)
#levels(TidywalmaMater$brains)[1] <- "na"
#levels(TidywalmaMater$brains)[2:13] <- "1,000s"
#levels(TidywalmaMater$brains)[3:18] <- "1,100s"
#levels(TidywalmaMater$brains)[4:21] <- "1,200s"
#levels(TidywalmaMater$brains)[5:16] <- "1,300s"
#levels(TidywalmaMater$brains)[6:13] <- "1,400s"
#levels(TidywalmaMater$brains)[7:8] <- "1,000s"
#It's a little better....still not very good.  Still ok wih discarding this. I feel this should be more important, but I guess
#it isn't...Too bad there is no standard rating for looks available.......
#--------------------Match Level by Goal----------------------------------------------------------------

# create a dataframe
df4 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$goal))
names(df4) <- c('match', 'Goal', 'count')
df4
# calculate the percentages
df4 <- ddply(df4, .(Goal), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df4 <- ddply(df4, .(Goal), transform, pos = (cumsum(count) - 0.5 * count))
df4$label <- paste0(sprintf("%.0f", df4$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df4, aes(x = Goal, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Goal')

#not too much going on here, probably noise/one to get rid of
#---------------------Match Level by Date Frequency---------------------------------------------------------------

# create a dataframe
df5 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$dateFreq))
names(df5) <- c('match', 'dateFrequency', 'count')
df5
# calculate the percentages
df5 <- ddply(df5, .(dateFrequency), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df5 <- ddply(df5, .(dateFrequency), transform, pos = (cumsum(count) - 0.5 * count))
df5$label <- paste0(sprintf("%.0f", df5$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df5, aes(x = dateFrequency, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Date Frequency')

#this one might be good, nice variance here compared to some of the others
#----------------------Match Level by Social Score-----------------------------------------------------------------

# create a dataframe
df6 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$socialScore))
names(df6) <- c('match', 'Sociability', 'count')
df6
# calculate the percentages
df6 <- ddply(df6, .(Sociability), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df6 <- ddply(df6, .(Sociability), transform, pos = (cumsum(count) - 0.5 * count))
df6$label <- paste0(sprintf("%.0f", df6$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df6, aes(x = Sociability, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Social Score')

#This one has some variance but I worry that social score and date frequency might be correlated, so this one
#which is not as good as the other one, may need to go.
#-----------------------Match Level by Age of Respondent-----------------------------------------------------------------

# create a dataframe
df7 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$age))
names(df7) <- c('match', 'Age', 'count')
df7
# calculate the percentages
df7 <- ddply(df7, .(Age), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df7 <- ddply(df7, .(Age), transform, pos = (cumsum(count) - 0.5 * count))
df7$label <- paste0(sprintf("%.0f", df7$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df7, aes(x = Age, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Age of Respondent')

#This is probably the best one so far. Not too surprising really
#------------------------Match Level by Age of Partner----------------------------------
# create a dataframe
df71 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$agePart))
names(df71) <- c('match', 'AgePart', 'count')
df71
# calculate the percentages
df71 <- ddply(df71, .(AgePart), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df71 <- ddply(df71, .(AgePart), transform, pos = (cumsum(count) - 0.5 * count))
df71$label <- paste0(sprintf("%.0f", df71$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df71, aes(x = AgePart, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Age of Partner')
#if you aren't under 21 hope you are 25


#-------------------------Match Level by Expected Happiness--------------------------------------------------------------------

# create a dataframe
df8 <- data.frame(table(TidydataPrePurge$matchYN, TidydataPrePurge$attitude))
names(df8) <- c('match', 'Attitude', 'count')
df8
# calculate the percentages
df8 <- ddply(df8, .(Attitude), transform, percent = count/sum(count) * 100)

# format the labels and calculate their positions
df8 <- ddply(df8, .(Attitude), transform, pos = (cumsum(count) - 0.5 * count))
df8$label <- paste0(sprintf("%.0f", df8$percent), "%")

# bar plot of counts by occupation with in group proportions 
ggplot(df8, aes(x = Attitude, y = count, fill = match)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = pos, label = label), size = 2) + 
  ggtitle('Match Level by Expected Happiness')
# Not alot going on here  good candidate for noise reduction....


#--------------------------Time to do so trimming of the fat------------------------
#Home has too many factors I think, I'm going to get rid of it
dating$home <- NULL
dating$goal <- NULL
dating$socialScore <- NULL
dating$attitude<- NULL
dating$brains<-NULL
dating$swagLevel <- NULL
#After doing evaluation and backwards selection I am removing gender
dating$gender <- NULL

#--------------------------Partitioning the Data-------------------------
summary(dating)
#Spliting the data to test some models! Setting seed to ensure replicable results.
set.seed(22)

#partition
dset <-createDataPartition(dating$matchYN, p=4/5, list=F)

#double-check the partition size
nrow(dset) / nrow(dating)

#create training/test data
trainDate <-dating[dset,]
testDate <-dating[-dset,]

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

#---------------------------Time to fit some models! logregmod--------------
logregmod <- glm(matchYN ~ ., data = trainDate, family = binomial('logit'))


# evaluation
m_full <- logregmod  # full model is the model just fitted
m_null <- glm(matchYN ~ 1, data = trainDate, family = binomial('logit'))

# backward selection
step(m_full, trace = F, scope = list(lower=formula(m_null), upper=formula(m_full)),
     direction = 'backward')
step(logregmod, direction="backward")

rmse(logregmod$residuals)
mae(logregmod$residuals)
1- with(logregmod, deviance/null.deviance)
# RMSE=2.816 MAE=2 R^2=.023

logregmod.pred.test <- predict(logregmod, testDate, type = "response", positive=2)

# If logregmod.pred.test exceeds threshold of 0.5, yes else no
y_or_n <- ifelse(logregmod.pred.test>0.5, "yes", "no")

# Convert to factor: p_class
p_class <- factor( y_or_n, levels = levels(testDate$matchYN))

# Create confusion matrix
confusionMatrix(p_class, testDate$matchYN)

#This model is accurate but it does not do well guessing the yes responses. It only guessed 2 out of 268.
summary(dating$matchYN)
#Fun fact being Pre Med is the best way to get people to match with you
#the worst thing you could do is study Architecture, although that was a small number of people.
#the worst thing a significant number of people do is be undecided on thier major.
#These are the features we are going to work with
#----------------------------Over, Under, & Both time to resample----------------------------------------------------------------------



#This data set is dominated by "no" matches, I'm going to try to fix that by over/under sampling.
#The problem is the model is seeking to be accurate, and the best way to be accurate is to simply guess no for the
#match response.  So what I'm going to do is provide more samples of yes to train the model to pick all the yes answers.
#Since I am trying to find the yes answers as my goal this method should help. I am making sure to only oversample the training
#set so information from the test set doesn't make its way into the sample. I would like to look into SMOTE but we haven't covered that.
library(ROSE)

over <- ovun.sample(matchYN ~., data=trainDate, method="over")$data
under <- ovun.sample(matchYN~. , data=trainDate, method = "under")$data
both <- ovun.sample(matchYN~. , data=trainDate, method = "both")$data

#-----------------------------logregmodover------54.704% of the yeses testDate 59.176% trainDate---------------------------------------------------------------------------------

logregmodover <- glm(matchYN ~ ., data = over, family = binomial('logit'))

rmse(logregmodover$residuals)
mae(logregmodover$residuals)
1- with(logregmodover, deviance/null.deviance)
# RMSE= 2.04  MAE= 2 R^2=.03

logregmodover.pred.test.train <- predict(logregmodover, trainDate, type = "response", positive=2)

# If logregmod.pred.test exceeds threshold of 0.5, yes else no
y_or_nover.train <- ifelse(logregmodover.pred.test.train>0.5, "yes", "no")

# Convert to factor: p_class
p_classover.train <- factor( y_or_nover.train, levels = levels(trainDate$matchYN))

# Create confusion matrix
confusionMatrix(p_classover.train, trainDate$matchYN)

632/(436+632)

logregmodover.pred.test <- predict(logregmodover, testDate, type = "response", positive=2)

# If logregmod.pred.test exceeds threshold of 0.5, yes else no
y_or_nover <- ifelse(logregmodover.pred.test>0.5, "yes", "no")

# Convert to factor: p_class
p_classover <- factor( y_or_nover, levels = levels(testDate$matchYN))

# Create confusion matrix
confusionMatrix(p_classover, testDate$matchYN)
145/(145+125)
#got 54% of the yeses

#The accuracy has gone down but the model had correctly predicted over half of the yeses
#instead of virtually none of them.

#------------------------------logregmodunder is not working-----------------------------------------------------------

#logregmodunder <- glm(matchYN ~ ., data = under, family = binomial('logit'))

#rmse(logregmodunder$residuals)
#mae(logregmodunder$residuals)
#1- with(logregmodunder, deviance/null.deviance)
# RMSE= 2.05  MAE= 2 R^2=.03

#logregmodunder.pred.test <- predict(logregmodunder, testDate, type = "response", positive=2)

#y_or_nunder <- ifelse(logregmodunder.pred.test>0.5, "yes", "no")

# Convert to factor: p_class
#p_classunder <- factor( y_or_nunder, levels = levels(testDate$matchYN))

# Create confusion matrix
#confusionMatrix(p_classunder, testDate$matchYN)







#-------------------------------logregmodboth------53% of the yeses---------------------------------------

logregmodboth <- glm(matchYN ~ ., data = both, family = binomial('logit'))

rmse(logregmodboth$residuals)
mae(logregmodboth$residuals)
1- with(logregmodboth, deviance/null.deviance)
# RMSE= 2.05  MAE= 2 R^2=.03


logregmodboth.pred.test <- predict(logregmodboth, testDate, type = "response", positive=2)

y_or_nboth <- ifelse(logregmodboth.pred.test>0.5, "yes", "no")

# Convert to factor: p_class
p_classboth <- factor( y_or_nboth, levels = levels(testDate$matchYN))

# Create confusion matrix
confusionMatrix(p_classboth, testDate$matchYN)
142/(142+128)
#got 53% of the yeses

#--------------------------------CART model--------49% & 41%------------------------
library(rpart)
tree2 <- rpart(matchYN ~ ., data = trainDate, method = 'class', cp = 1e-3)
tree2.pred.prob <- predict(tree2, newdata = testDate, type = 'prob')
tree2.pred <- predict(tree2, newdata = testDate, type = 'class')
# confusion matrix 
tb2 <- table(tree2.pred, testDate$matchYN)
tb2
#the regular cart model did better than the regular GLM time to try it with the different samples
tree2over <- rpart(matchYN ~ ., data = over, method = 'class', cp = 1e-3)
tree2over.pred.prob <- predict(tree2over, newdata = testDate, type = 'prob')
tree2over.pred <- predict(tree2over, newdata = testDate, type = 'class')
# confusion matrix 
tb2 <- table(tree2over.pred, testDate$matchYN)
tb2
134/(134+142)
#this got 49% of the yeses

tree2both <- rpart(matchYN ~ ., data = both, method = 'class', cp = 1e-3)
tree2both.pred.prob <- predict(tree2both, newdata = testDate, type = 'prob')
tree2both.pred <- predict(tree2both, newdata = testDate, type = 'class')
# confusion matrix 
tb2 <- table(tree2both.pred, testDate$matchYN)
tb2
113/(113+163)
#this got 41% of the yeses

#---------------------------------randomForest---oversampling results-----53.704% of yeses testDate---59.010% trainDate-------------------------------------
library(randomForest)


ControlParameters <-trainControl(method="cv", 
                                 number=5,
                                 savePredictions = TRUE,
                                 classProbs = TRUE)
parameterGrid <-expand.grid(mtry=c(2,3,4))
modelRandomover <-train(matchYN~., 
                        data=over,
                        method="rf",
                        trControl=ControlParameters,
                        tuneGrid=parameterGrid)

rfover.pred.train <- predict(modelRandomover, trainDate, type = "prob", positive=2)

y_or_nrfover.train <- ifelse(rfover.pred.train>0.5, "yes", "no")

# Convert to factor: p_class
p_classrfover.train <- factor( y_or_nrfover.train, levels = levels(trainDate$matchYN))

# Create confusion matrix
confusionMatrix(p_classover.train, trainDate$matchYN, positive = "yes")

632/(439+632)



rfover.pred.test <- predict(modelRandomover, testDate, type = "prob", positive=2)

y_or_nrfover <- ifelse(logregmodover.pred.test>0.5, "yes", "no")

# Convert to factor: p_class
p_classrfover <- factor( y_or_nrfover, levels = levels(testDate$matchYN))

# Create confusion matrix
confusionMatrix(p_classover, testDate$matchYN, positive = "yes")

145/(145+125)

#-----------------------------------------------------------------------------------

rf3 <- randomForest(matchYN ~ ., data = trainDate, ntree = 1000, na.action = na.omit)
rf3.pred.prob <- predict(rf3, newdata = testDate, type = 'prob')
rf3.pred <- predict(rf3, newdata = testDate, type = 'class')
# confusion matrix 
tb3 <- table(rf3.pred, testDate$matchYN)
tb3


#It got 18 out of 270 yeses

rfover <- randomForest(matchYN ~ ., data = over, ntree = 1000, na.action = na.omit)
rfover.pred.prob <- predict(rfover, newdata = testDate, type = 'prob')
rfover.pred <- predict(rfover, newdata = testDate, type = 'class')
# confusion matrix 
tb3over <- table(rfover.pred, testDate$matchYN)
tb3over

#it got 66/270

rfboth <- randomForest(matchYN ~ ., data = both, ntree = 1000, na.action = na.omit)
rfboth.pred.prob <- predict(rfboth, newdata = testDate, type = 'prob')
rfboth.pred <- predict(rfboth, newdata = testDate, type = 'class')
# confusion matrix 
tb3both <- table(rfboth.pred, testDate$matchYN)
tb3both





#lrmover <-train(matchYN~., 
#                data=over,
#               method="glm",
#                trControl=ControlParameters)


#lrmover.pred.test <- predict(lrmover, testDate, type = "prob", positive=2)

### If logregmod.pred.test exceeds threshold of 0.5, yes else no
#y_or_nlrmover <- ifelse(lrmover.pred.test$yes>0.5, "yes", "no")

### Convert to factor: p_class
#p_classlrmover <- factor( y_or_nlrmover, levels = levels(testDate$matchYN))

### Create confusion matrix
#confusionMatrix(p_classlrmover, testDate$matchYN)
#----------------------------------THE END---------------------------------------------







