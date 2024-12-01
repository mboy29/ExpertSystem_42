
# +------------------------------------------------+ 
# |                   TEST UNIT                    |
# +------------------------------------------------+

# Description:
# Functions used by the testing script at the root of
# projects

# +-------------------- IMPORTS -------------------+

from typing import IO
import re, json, subprocess, logging
from Sources.Tools import *

# +--------------------  CLASS --------------------+

def ft_test_exec(file_path: str, script_path: str):
    
    """
    Execute the script with the given file and return the output.

    Parameters:
        file_path (str): The path to the test file to be executed.
        script_path (stt): The path to the expert-system main.
    Returns:
        tuple: A tuple containing the stdout and stderr of the execution.
    Raises:
        FileNotFoundError: If the script or the file does not exist.
        subprocess.SubprocessError: If an error occurs during execution.
    """
    
    command = ["python3", "-B", script_path, file_path]
    result = subprocess.run(command, capture_output=True, text=True)
    return result.stdout, result.stderr

def ft_test_strip(text):
    
    """
    Remove ANSI escape sequences from text.

    Parameters:
        text (str): The string to remove escape sequences from.
    Returns:
        str: The string with ANSI escape sequences removed.
    Raises: None
    """
    
    ansi_escape = re.compile(r'\x1b(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
    return ansi_escape.sub('', text)


def ft_test_results_load(file_path):
    
    """
    Load the expected results from a JSON file.

    Parameters:
        file_path (str): The path to the JSON file containing expected results.
    Returns:
        dict: A dictionary containing the expected results.
    Raises:
        FileNotFoundError: If the file does not exist.
        json.JSONDecodeError: If the file is not a valid JSON.
    """
   
    with open(file_path, 'r') as file:
        return json.load(file)

def ft_test_results_parse(output):
    """
    Extract results from the script's output.

    Parameters:
        output (str): The output of the script to parse.
    Returns:
        dict: A dictionary with parsed results, where keys are labels and values are boolean.
    Raises:
        ValueError: If the output format is invalid.
    """

    results = {}
    for line in output.splitlines()[::-1]:
        clean_line = ft_test_strip(line)
        if len(clean_line.strip()) == 0:
            break
        splitted = clean_line[7:].split(": ")
        results[splitted[0]] = splitted[1].strip().lower() == "true"
    return results



def ft_test_log(log_file, file, script):
    """
    Run the test and log the results into the log file.
    
    Parameters:
        file (str): Path to the test file.
        script (str): Path to the script to execute.
    
    Returns:
        tuple: stdout, stderr
    """
    # Set up logging to capture output into a log file
    logging.basicConfig(
        filename=log_file,
        level=logging.DEBUG,
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    stdout, stderr = ft_test_exec(file, script)
    logging.info(f"Running test: {file}")
    logging.info(f"Output:\n{stdout}")
    if stderr:
        logging.error(f"Error:\n{stderr}")
    
    return stdout, stderr
