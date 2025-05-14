# test_config.py
import os


def _choose_impl():
    options = {
        "1": ("MatterMost", "src.implementations.MatterMostSDP", "MatterMost"),
        "2": ("NumberLine", "src.implementations.numberLineSDP", "NumberLine"),
        "3": ("Labyrinth", "src.implementations.labyrinthSDP", "MediumLabyrinth"),
    }

    print("Choose an SDP implementation to test:")
    for key, (name, _, _) in options.items():
        print(f"  {key}: {name}")
    
    choice = input("Enter choice [1-3]: ").strip()
    if choice not in options:
        raise ValueError("Invalid selection")

    return options[choice]
    

# Only prompt when running as a script or SDP_IMPL not set
if "SDP_IMPL" not in os.environ:
    name, module_path, class_name = _choose_impl()
else:
    name = os.environ["SDP_IMPL"].lower()
    mapping = {
        "mattermost": ("src.implementations.MatterMostSDP", "MatterMost"),
        "numberline": ("src.implementations.numberLineSDP", "NumberLine"),
        "labyrinth": ("src.implementations.labyrinthSDP", "MediumLabyrinth"),
    }
    if name not in mapping:
        raise ValueError(f"Unknown SDP_IMPL: {name}")
    module_path, class_name = mapping[name]

# Dynamic import
import importlib

module = importlib.import_module(module_path)
SelectedSDP = getattr(module, class_name)

sdp_instance = SelectedSDP()
