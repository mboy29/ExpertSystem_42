import os
import subprocess

def execute_script_for_files(directory, script_name):
    # Get all files in the given directory
    files = os.listdir(directory)
    
    # Iterate over each file in the directory
    for file in files:
        file_path = os.path.join(directory, file)
        # Ensure that it's a file (not a directory or other item)
        if os.path.isfile(file_path):
            print(f"Executing {script_name} with {file}")
            
            # Construct the command to execute main.py with the file as a parameter
            command = ["python3", "-B", script_name, file_path]
            
            # Execute the command
            result = subprocess.run(command, capture_output=True, text=True)
            
            # Print the output from the execution
            print(f"Output for {file}:\n{result.stdout}")
            if result.stderr:
                print(f"Error for {file}:\n{result.stderr}")

if __name__ == "__main__":
    invalid_dir = "Tests/Invalid"  # Directory containing the input files
    valid_dir = "Tests/Valid"      # Directory containing the input files
    script_name = "main.py"      # Script to execute
    
    # Call the function to execute the script for all files in the directory
    print("--------------- Executing script for invalid files: ---------------")
    execute_script_for_files(invalid_dir, script_name)
    print("--------------- Executing script for valid files: ---------------")
    execute_script_for_files(valid_dir, script_name)

