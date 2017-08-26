import pandas as pd
from bs4 import BeautifulSoup
import urllib
import time

######################################## ORI DATA #######################################################
import requests

#Opener call:
opener = urllib.request.build_opener()
opener.addheaders = [('User-Agent', "Mozilla/5.0 (Windows NT 10.0; WOW64; rv:50.0) Gecko/20100101 Firefox/50.0")]

ori_links = []
ori_state = []
url = 'https://www.icpsr.umich.edu/NACJD/ORIs/STATESoris.html'
source_code = requests.get(url)
plain_text = source_code.text
soup = BeautifulSoup(plain_text, 'html.parser')

for link in soup.find_all('a'):
    ori_links.append(link['href'])

for state in soup.find_all('a'):
    state_text = str(state.string).split("(")[0]
    ori_state.append(state_text)



new_dept_comp = []
new_ori_comp = []
new_fips_comp = []
new_ucr_comp = []
new_county_comp = []
new_state_comp = []


for l in ori_links:
    url = 'https://www.icpsr.umich.edu/NACJD/ORIs/'+str(l)
    print(url)
    source_code = requests.get(url)
    plain_text = source_code.text
    soup = BeautifulSoup(plain_text, 'html.parser')

    new_fips = []
    new_ucr = []
    new_county = []
    new_state = []

    for header in soup.find_all("h3"):
        header_text = str(header.string)

        state_txt = header_text.split(", ")[1].split(":")[0]
        new_state.append(state_txt)

        ucr_text = header_text.split("=")[2].split(")")[0]
        new_ucr.append(ucr_text)

        fips_text = header_text.split("=")[1].split(" ")[0]
        new_fips.append(fips_text)

        county_text = header_text.split("(")[0].strip()
        new_county.append(county_text)



    dept_data_list = []

    for data in soup.find_all("pre"):
        dept = str(data.string)
        dept_data_list.append(dept)


    for i in range(0,len(new_county)):
        dept_data = dept_data_list[i].split("\n")
        for each_item in dept_data:
            if each_item != '' and 'CITY/AGENCY' not in each_item:
                ori_text = each_item.split("   ")[-2].strip()
                new_ori_comp.append(ori_text)

                dept_text = each_item.split("   ")[0].strip()
                new_dept_comp.append(dept_text)

                new_fips_comp.append(new_fips[i])
                new_ucr_comp.append(new_ucr[i])
                new_county_comp.append(new_county[i])
                new_state_comp.append(new_state[i])

    time.sleep(3)
    print(l, "is Done.")


department_ori_df = pd.DataFrame({"ORI": new_ori_comp, "Department": new_dept_comp, "FIPS":new_fips_comp,
                                 "UCR": new_ucr_comp, "County":new_county_comp, "State": new_state_comp})
department_ori_df.to_csv("C:\\Users\\Max\\Documents\\Northwestern\\PRED 498\\Assignment1\\ORI by County.csv")




