from bs4 import BeautifulSoup
import os
import requests
import re
import urllib
import csv

#creating .csv file
with open("carsales.csv", mode="w", newline='')as csv_file:
    fieldnames = ['brand', "year", "sales"]
    writer=csv.writer(csv_file)
    writer.writerow(['brand', 'year', 'sales'])

#getting links for data for each car
res = requests.get("https://carsalesbase.com/car-sales-us-home-main/car-sales-by-brand-us/")
soup = BeautifulSoup(res.text, 'html.parser')
links = []
for link in soup.find_all('a'):
    links.append(link.get('href'))
links=links[88:153]

def get_car_data(url):
    try:
        res = requests.get(url)
        soup = BeautifulSoup(res.text, 'html.parser')

        # brand name
        name = soup.title.string
        name = name[0:name.index(" ")]

        # getting data
        sstring = soup.get_text()
        sstring = sstring.replace(".", "")
        sstring = sstring.replace(",", "")
        listy = re.findall("\d+|Annual Vehicle Sales", sstring)
        indicies = [i for i, x in enumerate(listy) if x == "Annual Vehicle Sales"]
        cleaned_data = []
        if int(listy[indicies[0] + 3]) == (int(listy[indicies[0] + 1]) + 1):
            listy1 = listy[(indicies[0] + 1):indicies[1]]
            cleaned_data.append(listy1[0])
            cleaned_data.append(listy1[1])
            listy1 = listy[(indicies[0] + 3):indicies[1]]
            for i, x in enumerate(listy1):
                if (i + 3) % 3 == 0:
                    cleaned_data.append(x)
                elif (i + 3) % 3 == 1:
                    cleaned_data.append(x)
                else:
                    continue
        else:
            listy1 = listy[(indicies[0] + 1):indicies[1]]
            cleaned_data.append(listy1[0])
            cleaned_data.append(listy1[1])
            listy1 = listy[(indicies[0] + 4):indicies[1]]
            for i, x in enumerate(listy1):
                if (i + 4) % 4 == 0:
                    cleaned_data.append(x)
                elif (i + 4) % 4 == 1:
                    cleaned_data.append(x)
                else:
                    continue

        # putting data in lists for csv
        year = []
        sales = []
        for index, x in enumerate(cleaned_data):
            if (index + 2) % 2 == 0:
                year.append(x)
            else:
                sales.append(x)

        # putting lists into csv
        with open('/Users/alex/PycharmProjects/topmovies/venv/carsales.csv', 'a') as csv_file:
            fieldnames = ['brand', "year", "sales"]
            writer = csv.writer(csv_file)
            for x in range(len(year)):
                writer.writerow([f'{name}', f'{year[x]}', f'{sales[x]}'])
        print(f"done with {name} ")
    except:
        print(f"{name} had some problems")

for x in links:
    get_car_data(x)