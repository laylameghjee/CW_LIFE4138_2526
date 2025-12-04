# # # # # # # # # # # # # # # # # # # #
#        Pumpkins Challenge           #
#           Layla Meghjee             #
#           December 2025             #
#            LIFE4138 CW              #
# # # # # # # # # # # # # # # # # # # #

# importing libraries 
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

##      TASK ONE
# reading in csv file
pumpkins = pd.read_csv('pumpkins_01.csv')


##      TASK TWO
# identifying the heaviest pumpkin.
heaviest = pumpkins.sort_values('weight_lbs', ascending=False)
print('Below is the heaviest pumpkin')
# prints only selected columns from the data set
print(heaviest.iloc[[0]][['id','weight_lbs', 'city', 'country', 'variety']])


##      TASK THREE
# converting pounds to kilos
def convert(weight_lbs):
	return weight_lbs * 0.453592

# creating a new column to add in kilo values
pumpkins['weight_kg'] = convert(pumpkins['weight_lbs'])


##      TASK FOUR
# adding a weight, class column with three categories
def classw(w):
	if w < 500:
		return 'light'
	elif w < 1000:
		return 'medium'
	else:
		return 'heavy'

# creating a new column to add in values for weight class
# using apply so the function runs on each individual value to avoid errors
pumpkins['weight_class'] = pumpkins['weight_lbs'].apply(classw)	


##      TASK FIVE
#plot estimated v actual weight and coloured by class
category = pumpkins['weight_class']
colour_options = {'light': 'green', 'medium': 'orange', 'heavy': 'red'}
colors = category.map(colour_options)
plt.scatter(pumpkins['est_weight'], pumpkins['weight_lbs'], c=colors, edgecolor='black')
plt.xlabel('estimated weight(lbs)')
plt.ylabel('actual weight(lbs)')
plt.title('Scatter to show estimated weight v actual weight')
plt.show() 

##      TASK SIX
#filter for three countries and save (pumpkins_filtered.csv)
countries = ['Italy', 'United States', 'Austria']
pumpkins_filtered = pumpkins[pumpkins['country'].isin(countries)]
pumpkins_filtered.to_csv('pumpkins_filtered.csv')


##      TASK SEVEN
#summarise mean weights by country and variety 
# reading in filtered csv file
pumpkins_filt = pd.read_csv('pumpkins_filtered.csv')
#summarise mean weights by country
country_mean = pumpkins_filt.groupby('country')['weight_lbs'].mean().to_string() 
print(country_mean)
#summarise mean weights by variety
variety_mean = pumpkins_filt.groupby('variety')['weight_lbs'].mean().to_string() 
print(variety_mean)


##      TASK EIGHT
#create boxplot for the three countries
sns.set_theme(style="darkgrid")
sns.boxplot(x = 'country', y='weight_lbs', data=pumpkins_filt, color = 'orange')
plt.ylabel('weight(lbs)')
plt.title('Pumpkin Weights by Country')
plt.show()


##      TASK NINE
#create a faceted plot by variety 
sns.set_theme(style='darkgrid')
fplot = sns.catplot(x='variety', y='weight_lbs', col='country', data=pumpkins_filt, kind=
'box', hue = 'variety', palette = 'pastel', col_wrap=3)
fplot.set_axis_labels('Variety', 'Weight(lbs)')
fplot.set_titles('{col_name}') #pulls the names from the dataset
for ax in fplot.axes.flatten(): #plays with the aes of the axis
    ax.tick_params(axis="x", rotation=90) #rotates x axis labels 
plt.tight_layout()
plt.show()





