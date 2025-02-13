import pandas as pd
import statsmodels.api as sm
import seaborn as sns
import matplotlib.pyplot as plt

#Module 5 assignment
#ALY 6010
#Dia Khosla
#Dia Khosla

# Loading the Lung Cap Dataset as lc
lc = pd.read_csv('LungCapDataCSV.csv') # lc as Lung Cap
print(lc.head())

vars = ['LungCap', 'Age', 'Height']
matrix = lc[vars].corr()

    # Displaying correlation matrix
print("matrix", matrix)
       #correlation coefficients suggest strong positive correlations between LungCap
         ##and both Age (0.819) and Height (0.912).
plt.figure(figsize=(8, 6))
sns.heatmap(matrix, annot=True, cmap='Greens', fmt='.4f', linewidths=.5)
plt.title('correlation_matrix')
plt.show()
#If r = -1, it means that there is a perfect negative correlation.
##If r = 0, it means that there is no correlation between the two variables.
##If r = 1, it means that there is a perfect positive correlation.

import statsmodels.api as sm

# Defining dependent variable
y = lc['LungCap']

# Defing independent variables
X = lc[['Age', 'Height']]

# Adding constant to the predictor variables
X = sm.add_constant(X)

# OLS regression model
model = sm.OLS(y, X).fit()

print("Regression Model on Age and Height", model.summary())

# Sub-setting data for males and females separately
male_data = lc[lc['Gender'] == 'male']
female_data = lc[lc['Gender'] == 'female']
male_smokers = male_data[male_data['Smoke'] == 'yes']
y_male = male_smokers['LungCap']
X_male = sm.add_constant(male_smokers[['Age', 'Height']])

# Fitting OLS regression model for males who smoke
model_male = sm.OLS(y_male, X_male).fit()

# Males who smoke
print("Regression Summary for Males who Smoke:")
print(model_male.summary())

female_cesarean = female_data[female_data['Caesarean'] == 'yes']
y_female = female_cesarean['LungCap']
X_female = sm.add_constant(female_cesarean[['Age', 'Height']])

# Fitting OLS regression model
model_female = sm.OLS(y_female, X_female).fit()

# Regression summary for females with Cesarean
print("Regression Summary for Females with Cesarean:")
print(model_female.summary())


# Threshold for lung diseases
threshold = 10.5
# Binary outcome
lc['LungDisease'] = (lc['LungCap'] > threshold).astype(int)
# Distribution of LungDisease
print(lc['LungDisease'].value_counts())
# Smoke
print(lc['Smoke'].value_counts())
print(lc['Gender'].value_counts())

# Subset data for males and females separately
male_data = lc[lc['Gender'] == 'male']
female_data = lc[lc['Gender'] == 'female']

# Plot for males: Smoking vs LungDisease
plt.figure(figsize=(12, 6))
plt.subplot(1, 2, 1)
sns.countplot(x='Smoke', hue='LungDisease', data=male_data, hue_order=[0, 1])
plt.title('Males: Distribution of Lung Disease by Smoking Status')
plt.xlabel('Smoke')
plt.ylabel('Count')
plt.legend(title='Lung Disease', labels=['No', 'Yes'])

# Plot for females: Smoking vs LungDisease
plt.subplot(1, 2, 2)
sns.countplot(x='Smoke', hue='LungDisease', data=female_data, hue_order=[0, 1])
plt.title('Females: Distribution of Lung Disease by Smoking Status')
plt.xlabel('Smoke')
plt.ylabel('Count')
plt.legend(title='Lung Disease', labels=['No', 'Yes'])

plt.tight_layout()
plt.show()
# Perform logistic regression for males: Smoke vs LungDisease
model_male_smoke = sm.Logit(male_data['LungDisease'], sm.add_constant((male_data['Smoke'] == 'yes').astype(int))).fit()
print("Logistic Regression Results for Males - Smoke vs LungDisease:")
print(model_male_smoke.summary())
print()

model_male_nosmoke = sm.Logit(male_data['LungDisease'], sm.add_constant((male_data['Smoke'] == 'no').astype(int))).fit()
print("Logistic Regression Results for Males - No Smoke vs LungDisease:")
print(model_male_nosmoke.summary())
print()

# Perform logistic regression for females: Smoke vs LungDisease
model_female_smoke = sm.Logit(female_data['LungDisease'], sm.add_constant((female_data['Smoke'] == 'yes').astype(int))).fit()
print("Logistic Regression Results for Females - Smoke vs LungDisease:")
print(model_female_smoke.summary())
print()

model_female_nosmoke = sm.Logit(female_data['LungDisease'], sm.add_constant((female_data['Smoke'] == 'no').astype(int))).fit()
print("Logistic Regression Results for Females - No Smoke vs LungDisease:")
print(model_female_nosmoke.summary())
print()
