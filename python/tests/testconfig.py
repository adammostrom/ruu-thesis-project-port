import importlib
import os
import subprocess

import numpy as np

sdp_instance = None

def _list_impl_files():
    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))  # /tests -> /python
    impl_dir = os.path.join(root_dir, "src", "implementations")
    return sorted([
        f for f in os.listdir(impl_dir)
        if f.endswith('.py') and 'SDP' in f and '__' not in f and f != 'specificationTemplateSDP.py'
    ])

def _choose_impl():
    files = _list_impl_files()
    if not files:
        raise RuntimeError("No valid SDP implementations found.")
    print(f"\nFound [{len(files)}] implementations. Select one to run:\n")
    for i, file in enumerate(files):
        print(f"{i}: {file}")
    print()
    try:
        choice = int(input("Enter number: "))
        filename = files[choice]
        class_name = filename.replace('.py', '')
        module_path = f"python.src.implementations.{class_name}"
        return module_path, class_name
    except (IndexError, ValueError):
        raise ValueError("Invalid input. Please enter a valid number.")

# Choose implementation
if "SDP_IMPL" not in os.environ:
    module_path, class_name = _choose_impl()
else:
    class_name = os.environ["SDP_IMPL"]
    module_path = f"python.src.implementations.{class_name}"

# Import module dynamically
module = importlib.import_module(module_path)

# Match class to use
possible_classnames = ["MatterMostMemo", "MatterMostPareto", "Labyrinth", "Specification", class_name.removesuffix("SDP")]
SpecClass = None
used_classname = None
for name in possible_classnames:
    SpecClass = getattr(module, name, None)
    if SpecClass:
        used_classname = name
        print(f"Using class: {name}")
        break


if SpecClass is None:
    raise AttributeError(f"No valid class found in module: {module_path}")


if class_name == "MatterMost":
    
    test_file = {
        "MatterMost": "tests/test_properties.py"
    }.get(type(sdp_instance).__name__)

    subprocess.run(["pytest", "-s", "-v", test_file])


# Special-case instantiation
if class_name == "AdvancedStatesSDP":
    decisionValues = np.arange(0, 3, 1)
    climValues = np.arange(1, 2, 1)
    econValues = np.arange(1, 2, 1)
    sdp_instance = SpecClass(decisionValues, climValues, econValues, 0.5)
    
    
else:
    sdp_instance = SpecClass()
