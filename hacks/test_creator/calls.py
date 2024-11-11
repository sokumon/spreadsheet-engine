import requests
from bs4 import BeautifulSoup
import json 
# fethces function calls and its result 
def get_calls_and_results():
    url = "https://formulajs.info/functions/"
    calls_and_results = []
    response = requests.get(url)
    print(response.status_code)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        calls = soup.find_all(class_="function-call")
        result = soup.select(".function > td:nth-child(3)")
        print(calls)
        for i in range(len(calls)):
            temp = {
                "calls": calls[i].text.strip(),
                "result": result[i].text.strip()
            }
            print(temp)
            calls_and_results.append(temp)
        return calls_and_results
    else:
        print(f"Failed to retrieve the page. Status code: {response.status_code}")
        return []


data = get_calls_and_results()
with open("calls.json","w") as f:
    f.write(json.dumps(data,indent=4))