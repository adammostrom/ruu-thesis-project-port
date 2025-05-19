import importlib
import os
import subprocess

sdp_instance = None

def _list_impl_files():
    root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))  # up from /tests
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

# Determine module and class to load
if "SDP_IMPL" not in os.environ:
    module_path, class_name = _choose_impl()
else:
    class_name = os.environ["SDP_IMPL"]
    module_path = f"python.src.implementations.{class_name}"

# Dynamic import
module = importlib.import_module(module_path)

# Try to get the main class from possible options
POSSIBLE_CLASSES = ["MatterMostMemo", "MatterMostPareto", "Labyrinth", class_name.removesuffix("SDP")]
SpecClass = None
for name in POSSIBLE_CLASSES:
    print(f"Importing from module: {module_path}")
    print(f"Available attributes: {dir(module)}")
    SpecClass = getattr(module, name, None)
    if SpecClass:
        print(f"Using class: {name}")
        break
    
""" if SpecClass == "MatterMost":
        subprocess.run(["pytest", "-s", "-v", "test_properties.py"])
 """
if SpecClass is None:
    raise AttributeError(f"No valid class found in module: {module_path}")

sdp_instance = SpecClass()
