import os
import re

# Directory containing the files
directory = "/home/mahdi/PL-Project/tests"

# Regex to match for loops
for_loop_pattern = re.compile(r'for\s*\(([^;]*);([^;]*);([^\)]*)\)\s*\{')

def convert_for_to_while(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

    # Find all for loops and convert them
    matches = for_loop_pattern.finditer(content)
    for match in matches:
        init = match.group(1).strip()
        condition = match.group(2).strip()
        increment = match.group(3).strip()

        # Create equivalent while loop
        while_loop = f"{init};\nwhile ({condition}) {{\n"
        if increment:
            while_loop += f"    {increment};\n"

        # Replace the for loop with the while loop
        content = content.replace(match.group(0), while_loop)

    # Write the modified content back to the file
    with open(file_path, 'w') as file:
        file.write(content)

# Process all .c files in the directory
for filename in os.listdir(directory):
    if filename.endswith(".c"):
        convert_for_to_while(os.path.join(directory, filename))
