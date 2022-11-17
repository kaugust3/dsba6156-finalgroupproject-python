# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

# Load packages
import numpy as np, pandas as pd

# Load the data and replace with your CSV file path
df = pd.read_csv("C:/Users/BarriP01/Desktop/media prediction and its cost.csv")
#print(df.head())
#print(df.tail())


df['food_category']=df['food_category'].astype('category')
df['food_department']=df['food_department'].astype('category')
df['food_family']=df['food_family'].astype('category')
df['promotion_name']=df['promotion_name'].astype('category')
df['sales_country']=df['sales_country'].astype('category')
df['marital_status']=df['marital_status'].astype('category')
df['gender']=df['gender'].astype('category')
df['education']=df['education'].astype('category')
df['member_card']=df['member_card'].astype('category')
df['occupation']=df['occupation'].astype('category')
df['houseowner']=df['houseowner'].astype('category')
df['brand_name']=df['brand_name'].astype('category')
df['store_type']=df['store_type'].astype('category')
df['store_city']=df['store_city'].astype('category')
df['store_state']=df['store_state'].astype('category')
df['media_type']=df['media_type'].astype('category')


df['coffee_bar']=df['coffee_bar'].astype('boolean')
df['video_store']=df['video_store'].astype('boolean')
df['salad_bar']=df['salad_bar'].astype('boolean')
df['prepared_food']=df['prepared_food'].astype('boolean')




print(df.info())


# Get the summary statistics



#locations = df.drop_duplicates(subset=['store_city', 'store_state', 'sales_country'])
#print(locations[['store_city','store_state','sales_country']])


#categorical variables
print('-----------------------------------------------')
print('Categorical:')

print(df['food_category'].value_counts())
print(df['food_category'].describe())


print(df['food_department'].value_counts())
print(df['food_department'].describe())

print(df['food_family'].value_counts())
print(df['food_family'].describe())

print(df['promotion_name'].value_counts())
print(df['promotion_name'].describe())

print(df['sales_country'].value_counts())
print(df['sales_country'].describe())

print(df['marital_status'].value_counts())
print(df['marital_status'].describe())

print(df['gender'].value_counts())
print(df['gender'].describe())

print(df['education'].value_counts())
print(df['education'].describe())

print(df['member_card'].value_counts())
print(df['member_card'].describe())

print(df['occupation'].value_counts())
print(df['occupation'].describe())

print(df['houseowner'].value_counts())
print(df['houseowner'].describe())

print(df['brand_name'].value_counts())
print(df['brand_name'].describe())

print(df['store_type'].value_counts())
print(df['store_type'].describe())

print(df['store_city'].value_counts())
print(df['store_city'].describe())

print(df['store_state'].value_counts())
print(df['store_state'].describe())

print(df['media_type'].value_counts())
print(df['media_type'].describe())



print('-----------------------------------------------')
print('Binary:')

print(df['coffee_bar'].value_counts())
print(df['video_store'].value_counts())
print(df['salad_bar'].value_counts())
print(df['prepared_food'].value_counts())



print('-----------------------------------------------')
print('Quantitative:')


print(df['store_sales(in millions)'].describe())
print(df['store_cost(in millions)'].describe())
print(df['unit_sales(in millions)'].describe())
print(df['total_children'].describe())
print(df['avg_cars_at home(approx)'].describe())
print(df['avg. yearly_income'].describe())
print(df['num_children_at_home'].describe())
print(df['avg_cars_at home(approx).1'].describe())
print(df['SRP'].describe())
print(df['gross_weight'].describe())
print(df['net_weight'].describe())
print(df['recyclable_package'].describe())
print(df['low_fat'].describe())
print(df['units_per_case'].describe())
print(df['store_sqft'].describe())
print(df['grocery_sqft'].describe())
print(df['frozen_sqft'].describe())
print(df['meat_sqft'].describe())
print(df['florist'].describe())
print(df['cost'].describe())


