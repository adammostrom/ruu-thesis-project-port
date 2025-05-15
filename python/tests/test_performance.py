import csv
from enum import Enum, auto
from timeit import default_timer as timer

from src.implementations.MatterMostSDP import MatterMost as memosdp
from src.implementations.MatterMostSDP import State as memoState
from src.implementations.MatterMostSDP import MatterMost as mmsdp
from src.implementations.MatterMostSDP import State as basicState

#from src.implementations.MatterMostSDP import State

sdp_basic = mmsdp()
sdp_memo = memosdp()

def run_old_mm():
    results = []
    for i in range(1,13):
        start = timer()
        sdp_basic.run_best(0, i, basicState.DHU)
        end = timer()
        results.append((i, end-start))
    return(results)


def run_memo_mm():
    results = []
    for i in range(1, 13):
        start = timer()
        sdp_memo.run_best(0, i, memoState.DHU)
        end = timer()
        results.append((i, end-start))
    return(results)
    
def write_to_csv(standard, memo):
    with open('results.csv', 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
    
        # Header
        writer.writerow(['method', 'timestep', 'time'])

        # Write standard results
        for timestep, time in standard:
            writer.writerow(['python-standard', timestep, time])
        
        # Write memoized results
        for timestep, time in memo:
            writer.writerow(['python-memoization', timestep, time])
        
    
    
    
def main():
    standard = run_old_mm()
    memo = run_memo_mm()
    write_to_csv(standard, memo)