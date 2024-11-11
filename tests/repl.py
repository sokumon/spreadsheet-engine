import os
import pexpect
import json 
from colorama import Fore, Back, Style

project_root = os.path.realpath('..')
js_file = os.path.join(project_root,"build/repl.js")
correct = 0
wrong = 0

def execute(fn,expected_value):
    try:

        child = pexpect.spawn(f'node {js_file}', encoding="utf-8")
        child.expect('> ')
        child.sendline(fn)
        child.expect("> ")
        output = child.before
        final_result = output.split("\n")[-2].strip()
        # print(f"Raw Output {output}")
        # Check for decimal stuff 1.1 type things
        
        if final_result.find(".") != -1 and expected_value.find(".") != -1:
            try: 
                final_result = f"{float(final_result):.4f}"
                expected_value = f"{float(expected_value):.4f}"
            except ValueError as e:
                    print(e)
        
        print(f"{Fore.WHITE}Expected Value : {str(expected_value)}")
        print(f"{Fore.WHITE}Final Value    : {str(final_result)}")
        if final_result == expected_value:
            print(f"{Fore.GREEN}{fn} works perfecly")
            return True
        else:


            print(f"{Fore.RED}{fn} doesnt work perfectly")
            return False
        
        child.close()
    except pexpect.EOF:
        print(f"{Fore.BLUE}Process ended.")
    except pexpect.TIMEOUT:
        print(f"{Fore.YELLOW}Timed out waiting for output.")



with open("calls.json","r") as f:
    data = json.loads(f.read())


try:

    for d in data:
        parts = d["calls"].split("(")
        parts[0] = parts[0].lower()
        parts[1] = str(parts[1]).replace("'", '"')
        s = "("
        formula = s.join(parts)
        print(f"{Fore.MAGENTA}Checking for {formula}")
        if execute(formula,d["result"]):
            correct = correct + 1
        else:
            wrong = wrong + 1
    

    print(f"{Fore.YELLOW}Interpreter Stats")
    print(f"{Fore.BLUE} Out of {len(data)} formulas")
    print(f"{Fore.GREEN}Correctly Interpreted : {correct}")
    print(f"{Fore.RED}Wrongly Interpreted : {wrong}")
    print(f"{Fore.YELLOW} {(correct/len(data)) * 100}% Success")
        
except Exception as e:
    print("Exception is",e)
    pass