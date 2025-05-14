import subprocess
from time import time

"""
This file will not be of reproductive value unless the user also inherits the original Responsibility Under Uncertainty Idris code.
The test is invoking a shell session to run the Idris computation.
Place this file inside the Idris repository and run it.
Original repo found here: https://gitlab.pik-potsdam.de/botta/papers/-/tree/master/2021.Responsibility%20under%20uncertainty:%20which%20climate%20decisions%20matter%20most%3F?ref_type=heads
"""

def run_idris_script(t, n, x):
     
    start_overhead = time()
    shell = f'echo ":exec best 1 1 1" | idris Responsibility.idr'
    result = subprocess.run(shell, shell=True, text=True, capture_output=True, timeout=30)
    end_overhead = time()
    shell_overhead = end_overhead - start_overhead
    
    start = time()

    cmd = f'echo ":exec best 0 {n} DHU" | idris Responsibility.idr'
    print(f"Running command: {cmd}") 

    try:
        result = subprocess.run(cmd, shell=True, text=True, capture_output=True, timeout=300)

        if result.returncode != 0:
            print(f"Error occurred with return code {result.returncode}")
            print(result.stderr)
        else:
            print(result.stdout)

    except subprocess.TimeoutExpired:
        print("The process timed out.")

    end = time()

    elapsed = (end - start) - shell_overhead
    


    print(f"Time taken: {elapsed:.6f} seconds")
    return elapsed

if __name__ == "__main__":
    results = []
    
    for i in range (1, 10):
        elaps = run_idris_script(0, i, 'DHU')
        
        results.append((i, elaps))
        print(results)

