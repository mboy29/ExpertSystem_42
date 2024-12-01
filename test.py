# +------------------------------------------------+ 
# |                   ARGUMENTS                    |
# +------------------------------------------------+

# Description:
# This script runs and validates test cases for the 
# expert system program.

# +-------------------- IMPORTS -------------------+

import os
from Sources.Tools import *

# +-------------------- GLOBALS -------------------+

SCRIPT_PATH = "main.py"
INVALID_DIR_PATH = "Tests/Invalid"
VALID_DIR_PATH = "Tests/Valid"
RESULTS_PATH = "Tests/results.json"
LOG_FILE_PATH = "Tests/test_results.log"

# +------------------- FUNCTIONS ------------------+

def ft_test_invalid(directory):
    
    """
    Validate that all invalid test cases end with [ERROR].

    Parameters:
        directory (str): Path to the directory with invalid test cases.

    Returns:
        bool: True if all invalid tests pass, False otherwise.
    """

    Logger.info("Validating invalid test cases...\n")
    all_valid = True
    for file in os.listdir(directory):
        file = os.path.join(directory, file)
        if os.path.isfile(file):
            stdout, stderr = ft_test_log(LOG_FILE_PATH, file, SCRIPT_PATH)
            if "[ERROR]" not in stdout:
                all_valid = False
                Logger.test("INVALID_FAIL", file)
            else:
                Logger.test("INVALID_PASS", file)
    return all_valid

def ft_test_valid(directory: str, results):
   
    """
    Validate that all valid test cases match expected results.

    Parameters:
        directory (str): Path to the directory with valid test cases.
        results (dict): Expected results for each test file.

    Returns:
        bool: True if all valid tests pass, False otherwise.
    """
    
    Logger.info("Validating valid test cases...\n")
    all_valid = True
    for category, tests in results.items():
        Logger.info(f"Testing {category} files...")
        for file, output in tests.items():
            stdout, stderr = ft_test_log(LOG_FILE_PATH, file, SCRIPT_PATH)
            results = ft_test_results_parse(stdout)
            if results != output:
                all_valid = False
                Logger.test("VALID_FAIL", file, output, results)
            else:
                Logger.test("VALID_PASS", file)
        print("")
    return all_valid

def ft_test(valid: bool, invalid: bool) -> bool:
    
    """
    Run both valid and invalid tests based on arguments.

    Parameters:
        valid (bool): Flag to run valid tests.
        invalid (bool): Flag to run invalid tests.

    Returns:
        bool: True if all selected tests pass, False otherwise.
    """
    
    results = ft_test_results_load(RESULTS_PATH)  
    if invalid or not (valid or invalid):
        invalid_results = ft_test_invalid(INVALID_DIR_PATH)
        print("")
    else:
        invalid_results = True    
    if valid or not (valid or invalid):
        valid_results = ft_test_valid(VALID_DIR_PATH, results)
    else:
        valid_results = True
    
    return invalid_results and valid_results

def ft_main() -> None:
    
    """
    Main function to execute tests and display results.

    Parameters: None
    Returns: None    
    """
    
    Logger.header()
    parser = CustomArgumentParserTest()
    args = parser.parse_args()
    if ft_test(args.valid, args.invalid):
        Logger.success("All tests passed!")
    else:
        Logger.error("Some tests failed. Check the logs at '{LOG_FILE_PATH}' for more details.")

if __name__ == "__main__":
    try:
        ft_main()
    except Exception as e:
        ft_exit(str(e), 1, True)
