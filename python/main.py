import code
import importlib
import os

# TODO: CREATE SPECIAL CASE FOR ADVANCED STATE AND ASK FOR MORE INPUTS.

if __name__ == "__main__":
    path = 'src/implementations'
    files = os.listdir(path)
    files = [file for file in files if "__" not in file and file != 'specificationTemplateSDP.py']
    files.sort()

    files_index = [(i, file) for i, file in enumerate(files)]

    print("\nFound [", len(files), "] implementations. Select numerator from the list to run.\n")
    for i, file in files_index:
        print(f"{i}: {file}")
    print()

    try:
        choice = int(input("Enter number: "))
        selected_file = files_index[choice][1].replace('.py', '')
        module_path = f"{path.replace('/', '.')}.{selected_file}"

        module = importlib.import_module(module_path)
        
        # TODO: TAKE INPUT PARAMETERS FOR THE ADVANCED STATE
        if selected_file == 'AdvancedStatesSDP':
            print("Advanced State requires further arguments: ")
            
            

        # TODO: MAKE MORE INTERACTIVE FOR USERS IN LABYRINTH, MAYBE EASIER FUNCTIONS THAT TAKES INPUTS AND MAKES THE LOOP ETC.
        if selected_file == 'LabyrinthSDP':
            SpecClass = getattr(module, "SmallLabyrinthDet", None)
            State = getattr(module, "State", None)
            sdp = SpecClass()
            print("\nInstance created as 'sdp'. Entering interactive shell...\n")

            code.interact(local=dict(globals(), **{
                "sdp": sdp,
                "State": State
            }))
                
        

        # Replace your class-loading block with this:
        State = getattr(module, "State", None)

        # Try to get the main class
        POSSIBLE_CLASSES = ["MatterMost", "AdvancedStates", "Labyrinth"]
        SpecClass = None
        for name in POSSIBLE_CLASSES:
            SpecClass = getattr(module, name, None)
            if SpecClass:
                print(f"Using class: {name}")
                break

        if SpecClass is None:
            raise AttributeError("No valid class found in module.")

        # Launch shell
        sdp = SpecClass()
        
        # TODO: LIST FUNCTONS AVAILABLE
        print("\nInstance created as 'sdp'. Entering interactive shell...\n")

        code.interact(local=dict(globals(), **{
            "sdp": sdp,
            "State": State
        }))

    except (IndexError, ValueError):
        print("Invalid input. Please enter a valid number.")
    except Exception as e:
        print(f"Failed: {e}")
