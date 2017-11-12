import csv
import os
# import urllib
import requests
from bs4 import BeautifulSoup
from urllib.parse import urlencode

import time

# =================================================================================================

def to_float(s):

    try:
        s_value = float(s)
    except ValueError:
        s_value = 0.0

    return s_value


def get_tr_row(block, is_title=False):

    tag = 'th' if is_title else 'td'
    sub_block = block.find_all(tag)

    row = list()
    
    for td in sub_block:
        if is_title:
            row.append(td.text)
        else:
            row.append(to_float(td.text))

    return row

def get_tr_block(block, is_title=False):

    block_list = list()

    for tr in block:
        row = get_tr_row(tr, is_title=is_title)
        if row: block_list.append(row)

    return block_list
    
# =================================================================================================

def page_parser(st_no, st_name, date):

    param = {
        'command'    : 'viewMain',
        'station'    : st_no,
        'stname'     : 'none',
        'datepicker' : date
    }

    ext_title = ['date', 'st_no', 'st_name']
    ext_data  = [date, st_no, st_name]
    has_title = os.path.isfile(F_RESULT)

    list_data = list()
   
    url = LIST_URL + urlencode(param)
    print(url)
    list_req = requests.get(url)

    if list_req.status_code == requests.codes.ok:
        soup = BeautifulSoup(list_req.content, 'html.parser')
        tab  = soup.find('table', id='MyTable')

        if not has_title:
            tag_trs = tab.find_all(class_='second_tr')
            title   = get_tr_block(tag_trs, is_title=True)[0]
            title.extend(ext_title)

        tag_trs = tab.find_all('tr')
        block   = get_tr_block(tag_trs)
        list_data.extend(block)

    for i in range(len(list_data)):
        list_data[i].extend(ext_data)
    
    # =================================================================================================

    with open(F_RESULT, 'a') as f_csv:
        f_writer = csv.writer(f_csv, dialect='excel')

        if not has_title:
            f_writer.writerow(title)

        for row in list_data:
            f_writer.writerow(row)

# =================================================================================================

F_RESULT = './result.csv'
LIST_URL = 'http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?'

# =================================================================================================

if os.path.isfile(F_RESULT):
    has_title = False
    os.remove(F_RESULT)

# =================================================================================================

with open('./station_loc_selected.csv', 'r') as f_csv:
    station = list(csv.reader(f_csv))

target_date = ['2017-07-28', '2017-07-29', '2017-07-30', '2017-07-31']

# '''
for date in target_date:
    for i in range(1, len(station)):
        page_parser(st_no=station[i][0], st_name=station[i][1], date=date)
        time.sleep(0.5)

'''        
page_parser(st_no=station[1][0], st_name=station[1][1], date=target_date[0])
page_parser(st_no=station[1][0], st_name=station[1][1], date=target_date[1])
page_parser(st_no=station[1][0], st_name=station[1][1], date=target_date[2])
# '''
# =================================================================================================
'''
F_RESULT    = './result.csv'

LIST_URL = 'http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?'

has_title = False

# =================================================================================================

if os.path.isfile(F_RESULT):
    has_title = False
    os.remove(F_RESULT)

# =================================================================================================

with open('./station_loc_selected.csv', 'r') as f_csv:
    station = list(csv.reader(f_csv))

target_date = ['2017-07-28', '2017-07-29', '2017-07-30', '2017-07-31']

# LIST_URL = 'http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=466910&stname=%25E9%259E%258D%25E9%2583%25A8&datepicker=2017-07-28'

param = {
    'command'    : 'viewMain',
    'station'    : station[1][0],
    'stname'     : 'none',
    'datepicker' : target_date[0]
}

param_encoded = urlencode(param)

LIST_URL = 'http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?' + param_encoded
print(LIST_URL)
print('http://e-service.cwb.gov.tw/HistoryDataQuery/DayDataController.do?command=viewMain&station=466880&stname=%25E6%259D%25BF%25E6%25A9%258B&datepicker=2017-07-28')

list_req = requests.get(LIST_URL)

list_data = list()

if list_req.status_code == requests.codes.ok:
    soup = BeautifulSoup(list_req.content, 'html.parser')
    tab  = soup.find('table', id='MyTable')

    if not has_title:
        has_title = True
        tag_trs = tab.find_all(class_='second_tr')
        block   = get_tr_block(tag_trs, is_title=True)
        list_data.extend(block)

    tag_trs = tab.find_all('tr')
    block   = get_tr_block(tag_trs)
    list_data.extend(block)

# =================================================================================================
# Exend Columns

ext_title = ['date']
ext_title.extend(station[0])
ext_data  = [target_date[0]]
ext_data.extend(station[1])

for i in range(len(list_data)):
    ext = ext_title if i == 0 else ext_data
    list_data[i].extend(ext)
    

# =================================================================================================

with open(F_RESULT, 'a') as f_csv:
    f_writer = csv.writer(f_csv, dialect='excel')

    for row in list_data:
        f_writer.writerow(row)
'''

