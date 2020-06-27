import os
import subprocess
# set working directory to be project source
os.chdir(os.getcwd()+"/../src")

for filename in os.listdir(os.getcwd()):
    if filename.endswith(".hs"):
        print("WORKING ON FILE:", filename)
        result = subprocess.run(["hlint", filename])
