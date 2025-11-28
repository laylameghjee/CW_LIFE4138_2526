# # # # # # # # # # # # # # # # # # # #
#        Pumpkins Challenge           #
#           Layla Meghjee             #
#           November 2025             #
# # # # # # # # # # # # # # # # # # # #

# importing libraries 
import pandas as pd
import matplotlib.pyplot as plt

##      TASK ONE
# reading in csv file
pumpkins = pd.read_csv('pumpkins_01.csv')


##      TASK TWO
# identifying the heaviest pumpkin.
heaviest = pumpkins.sort_values("weight_lbs", ascending=False)
print("Below is the heaviest pumpkin")
# prints only selected columns from the data set
print(heaviest.iloc[[0]][['id','weight_lbs', 'city', 'country', 'variety' ]])


##      TASK THREE
# converting pounds to kilos
def convert(weight_lbs):
	return weight_lbs * 0.453592

# creating a new column to add in kilo values
pumpkins["weight_kg"] = convert(pumpkins["weight_lbs"])


##      TASK FOUR
# adding a weight, class column with three categories
def classw(w):
	if w < 500:
		return "light"
	elif w < 1000:
		return "medium"
	else:
		return "heavy"

# creating a new column to add in values for weight class
# using apply so the function runs on each individual value to avoid errors
pumpkins["weight_class"] = pumpkins["weight_lbs"].apply(classw)	


##      TASK FIVE
#plot estimated v actual weight and coloured by class
category = pumpkins["weight_class"]
colour_options = {'light': 'green', 'medium': 'orange', 'heavy': 'red'}
colors = category.map(colour_options)
plt.scatter(pumpkins["est_weight"], pumpkins["weight_lbs"], c=colors, edgecolor="black")
plt.xlabel("estimated weight(lbs)")
plt.ylabel("actual weight(lbs)")
plt.title("Scatter to show estimated weight v actual weight")
plt.show() 


##      TASK FIVE
#filter for three countries and save (pumpkins_filtered.csv)


#summarise mean weights by country and variety 


#create boxplot for the three countries


#create a faceted plot by variety 
