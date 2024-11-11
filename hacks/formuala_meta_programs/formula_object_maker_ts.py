import requests
from bs4 import BeautifulSoup
import re

def computeArg(formula_call):
    matches = re.search(r'(\w+)\(([^)]+)\)', formula_call)
    if matches:
        all_args = matches.group(2)
        return computeArgsSplit(all_args)
    return ""

def computeArgsSplit(input_string):
    pattern = r"(?:'[^']*'|\[[^\]]*\]|[^,()]+)"
    matches = re.findall(pattern, input_string)

    result = []
    for match in matches:
        stripped = match.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            result.append(stripped)  # Keep the entire bracketed content
        else:
            result.append(stripped.strip("'"))
    
    return result

def escape_string(s):
    return s.replace('"', '\\"').replace("'", "\\'")

def create_js_object(final_formulas):
    header_string = """import * as formulajs from '@formulajs/formulajs';
export const formulas = {
"""
    end_string = "};\n"
    mid_string = ""

    with open("formula.ts", "w") as f:
        f.write(header_string)
        for formula in final_formulas:
            result = f"[{', '.join(['Value'] * formula['argsLength'])}]"
            desc_escaped = escape_string(formula['desc'])
            temp_string = f"""
    {formula['name'].lower()}: {{
        desc: "{desc_escaped}",
        compute: (values: Array<Value>) : Value => {{
            if (values.length !== {formula['argsLength']}) {{
                throw new Error("{formula['name']} requires exactly {formula['argsLength']} arguments.");
            }}
            return formulajs.{formula['name']}.apply(null, values as {result});
        }},
        completion: "{formula['name']}("
    }},
"""
            mid_string += temp_string

        f.write(mid_string)
        f.write(end_string)

def get_elements_by_class(url, class_name):
    response = requests.get(url)
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        elements = soup.find_all(class_=class_name)
        return elements
    else:
        print(f"Failed to retrieve the page. Status code: {response.status_code}")
        return []

def get_google_sheet(url):
    response = requests.get(url)
    google_list = []
    if response.status_code == 200:
        soup = BeautifulSoup(response.content, 'html.parser')
        formula_names = soup.select("table > tbody > tr > td:nth-child(2)")
        formula_desc = soup.select("table > tbody > tr > td:nth-child(4)")
        for i in range(len(formula_names)):
            temp = {
                "name": formula_names[i].text.strip(),
                "desc": formula_desc[i].text.strip().replace("Learn more", ""),
            }
            google_list.append(temp)
    return google_list

if __name__ == "__main__":
    url = 'https://formulajs.info/functions/'
    google_url = "https://support.google.com/docs/table/25273?hl=en"
    class_name = 'function-name'
    
    all_google_formulas = get_google_sheet(google_url)
    formulajs_formulas = get_elements_by_class(url, class_name)
    formulajs_functions = get_elements_by_class(url, "function-call")
    final_formulas = []
    for i in range(len(formulajs_formulas)):
        for google in all_google_formulas:
            if formulajs_formulas[i].text.strip() == google["name"]:
                google["argsLength"] = len(computeArg(str(formulajs_functions[i])))
                final_formulas.append(google)
    create_js_object(final_formulas)
