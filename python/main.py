# main.py

import argparse

from src.implementations import (AdvancedStatesSDP, MatterMostMemo,
                                 MatterMostPareto)

IMPLEMENTATIONS = {
    "mattermost": (MatterMostMemo.MatterMost, MatterMostMemo.State),
    "mattermost pareto": (MatterMostPareto.MatterMost, MatterMostPareto.State),
    "advanced states": (AdvancedStatesSDP.Specification, AdvancedStatesSDP.State),
}

def get_sdp_instance(impl_name):
    if impl_name not in IMPLEMENTATIONS:
        raise ValueError(f"Unknown implementation: {impl_name}")
    cls, _ = IMPLEMENTATIONS[impl_name]
    return cls()

def run_best(sdp, t, n, x):
    return(sdp.best(t, n, x))

def run_mMeas(sdp, t, n, x):
    return(sdp.mMeas(t, n, x))

def run_bi(sdp, t, n):
    return(sdp.bi(t, n))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--impl", choices=IMPLEMENTATIONS.keys(), required=True)
    args = parser.parse_args()

    sdp_instance = get_sdp_instance(args.impl)

    banner = f"Running interactive shell with '{args.impl}' implementation.\n"
    banner += "Available functions:\n * run_best(sdp, t, n, x)\n * run_mMeas(sdp, t, n, x)\n"
    banner += "Access the instance as `sdp_instance`"

    import code
    vars = globals().copy()
    vars.update(locals())
    code.interact(banner=banner, local=vars)
