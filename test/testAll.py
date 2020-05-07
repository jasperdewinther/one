import os
import subprocess
#set working directory to be project root
os.chdir(os.getcwd()+"/..")

#all programs in the "programs folder" with their expected output
programNameWithResult = {
    "+": 25,
    "a": 914,
    "c": 23,
    "e": 30,
    "f": 6765,
    "l": 1000000,
    "x": 1,
}

#run every program and check the result
for key, value in programNameWithResult.items():
    result = subprocess.run(
        ["cabal", "run", "one", "programs/" + key + ".one"], stdout=subprocess.DEVNULL)
    print("program: " + key + " expected: " + str(value) + " result: " +
          str(result.returncode), end=" status: ")
    if not value == result.returncode:
        print("ERROR")
    else:
        print("succes")
