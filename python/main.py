import code
import importlib
import os
import sys

# Add project root to sys.path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

if __name__ == "__main__":
    path = 'python/src/implementations'
    files = [f for f in os.listdir(path) if "__" not in f and f != 'specificationTemplate.py']
    files.sort()

    # Filter files containing "SDP" substring (optional)
    files = [f for f in files if "SDP" in f]

    files_index = list(enumerate(files))

    print(f"\nFound [{len(files)}] SDP implementations. Select number from list to run.\n")
    for i, file in files_index:
        print(f"{i}: {file}")
    print()

    try:
        choice = int(input("Enter number: "))
        selected_file = files_index[choice][1].replace('.py', '')
        module_path = f"{path.replace('/', '.')}.{selected_file}"
        module = importlib.import_module(module_path)

        # Handle advanced state special input
        if selected_file == 'AdvancedStatesSDP':
            print("AdvancedStatesSDP requires extra initialization parameters.")
            # Example: ask for parameter 'param1' and 'param2'
            param1 = input("Enter param1 (int): ")
            param2 = input("Enter param2 (str): ")
            try:
                param1 = int(param1)
            except ValueError:
                print("Invalid param1, should be int. Exiting.")
                sys.exit(1)
        else:
            param1 = param2 = None

        # Look for main class to instantiate
        POSSIBLE_CLASSES = ["MatterMost", "MatterMostMemo", "AdvancedStates", "Labyrinth", "NumberLine", "MatterMostPareto", selected_file]
        SpecClass = None
        for name in POSSIBLE_CLASSES:
            SpecClass = getattr(module, name, None)
            if SpecClass:
                print(f"Using class: {name}")
                break

        if SpecClass is None:
            raise AttributeError("No valid class found in module.")

        # Instantiate with parameters if advanced
        if selected_file == 'AdvancedStatesSDP':
            sdp = SpecClass(param1, param2)  # Customize constructor args as needed
        else:
            sdp = SpecClass()

        State = getattr(module, "State", None)

        print("\nInstance created as 'sdp'. For a list of functions, run 'sdp.help()'")
        print("Enter `help(sdp)` for detailed info or run commands interactively.\n")

        # If LabyrinthSDP, special interactive shell with State
        if selected_file == 'LabyrinthSDP':
            SpecClassLabyrinth = getattr(module, "SmallLabyrinthDet", None)
            if SpecClassLabyrinth:
                sdp = SpecClassLabyrinth()
                State = getattr(module, "State", None)
                print("Using SmallLabyrinthDet instance as 'sdp'. Starting interactive shell...\n")
                code.interact(local={"sdp": sdp, "State": State})
                sys.exit(0)

        code.interact(local={"sdp": sdp, "State": State})

    except (IndexError, ValueError):
        print("Invalid input. Please enter a valid number.")
    except Exception as e:
        print(f"Failed: {e}")
