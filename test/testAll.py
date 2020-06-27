import os
import subprocess
import multiprocessing.dummy as mp
from time import sleep
from random import randint
# set working directory to be project root
os.chdir(os.getcwd()+"/..")

# all programs in the "programs folder" with their expected output
programNameWithResult = {
    "+": 251,
    "a": 914,
    "c": 23,
    "e": 30,
    "f": 6765,
    "l": 1000000,
    "x": 2,
    "p": 4294967295,
}


def runProgram(programNameWithResult):
    # decrease the likelyhood of multiple programs using the same tmp files
    sleep(randint(0, 1000)/1000)
    result = subprocess.run(
        ["cabal", "run", "one", "programs/" + programNameWithResult[0] + ".one"], stdout=subprocess.DEVNULL)
    print("program: " + programNameWithResult[0] + " expected: " + str(programNameWithResult[1]) + " result: " +
          str(result.returncode), end=" status: ")
    if not programNameWithResult[1] == result.returncode:
        print("ERROR")
    else:
        print("succes")


# make sure program is compiled before running all programs
runProgram(list(programNameWithResult.items())[0])
# run all programs at the same time
threadpool = mp.Pool(len(programNameWithResult))
threadpool.map(runProgram, programNameWithResult.items())
