import code
import importlib
import os

# TODO: CREATE SPECIAL CASE FOR ADVANCED STATE AND ASK FOR MORE INPUTS.

if __name__ == "__main__":
    path = 'src/implementations'
    files = os.listdir(path)
    files = [file for file in files if "__" not in file and file != 'specificationTemplate.py']
    files.sort()

    # TODO, LOOK FOR THE SUBSTRING "SDP"
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
        POSSIBLE_CLASSES = ["MatterMost", "AdvancedStates", "Labyrinth", selected_file]
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
        
        # TODO: LIST FUNCTONS AVAILABLE, MAKE LESS GENERIC, MAYBE LET EACH IMPELEMTATION HOLD THESE OR
        print("\nInstance created as 'sdp'. Entering interactive shell.\nTo exit, type 'quit()'. ")
        EXPOSED_METHODS = {
            "states    ": "states(t: int) -> list[State]",
            "actions   ": "actions(t: int, x: State) -> list[Action] | list[None]",
            "nextFunc  ": "nextFunc(t: int, x: State, y: Action) -> dict[State, float]",
            "reward    ": "reward(t: int, x: State, y: Action, x_prim: State) -> float",
            "val       ": "val(t: int, ps: PolicySequence | list[None], x: State) -> float",
            "bestExt   ": "bestExt(t: int, ps_tail: PolicySequence) -> Policy",
            "worstExt  ": "worstExt(t: int, ps_tail: PolicySequence | list[None]) -> Policy",
            "randomExt ": "randomExt(t: int, ps_tail: PolicySequence) -> Policy",
            "bi        ": "bi(t: int, n: int) -> PolicySequence",
            "randomPS  ": "randomPS(t: int, n: int) -> PolicySequence",
            "best      ": "best(t: int, n: int, x: State) -> str",
            "worst     ": "worst(t: int, n: int, x: State) -> str",
            "mMeas     ": "mMeas(t: int, n: int, x: State) -> float",
            "best_time ": "best(t: int, n: int, x: State) -> float"
        }
        print("Available functions: ")
        for sig in EXPOSED_METHODS.values():
            print(f"  • {sig}")

        print("\nRun the functions using the sdp instance: `sdp.best(0, 1, State.DHU)`\nFor more detailed information about the currently loaded sdp, run 'help(sdp)' or review documentation. \n")
        code.interact(local=dict(globals(), **{
            "sdp": sdp,
            "State": State
        }))

    except (IndexError, ValueError):
        print("Invalid input. Please enter a valid number.")
    except Exception as e:
        print(f"Failed: {e}")
