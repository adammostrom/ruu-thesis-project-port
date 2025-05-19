import subprocess

from python.tests.testconfig import sdp_instance

test_file = {
    "MatterMost": "tests/test_properties.py"
}.get(type(sdp_instance).__name__, "tests/test_propertiesMemo.py")

print(f"\nRunning pytest for: {test_file}")
subprocess.run(["pytest", "-s", "-v", test_file])
