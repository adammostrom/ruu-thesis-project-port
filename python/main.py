# main.py

from src.implementations.MatterMostSDP import MatterMost as module
from src.implementations.MatterMostSDP import State

sdp_instance = module()

def run_best(t, n, x):
    print(sdp_instance.best(t, n, x))

def run_mMeas(t, n, x):
    print(sdp_instance.mMeas(t, n, x))

if __name__ == "__main__":
    import code
    banner = "MatterMost implementation (see src/implementations).\nTo instantiate a new sdp, follow the template in src/implementations/specificationTemplateSDP \nand load it as the module in this file. \nFunctions available: \n * run_best(t, n, x)\n * run_mMeas(t, n, x)"
    vars = globals().copy()
    vars.update(locals())
    code.interact(banner=banner, local=vars)