import subprocess
import json


def run_bash_command(command: str, *, json_output: bool = False) -> tuple[dict | None, int]:
    try:
        # Run the command and capture the output
        result = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            shell=True,
        )

        # Check if the command was successful
        if result.returncode == 0:
            # Parse the JSON output if requested
            if json_output:
                parsed_output = json.loads(result.stdout)
                return parsed_output, result.returncode
            else:
                return None, result.returncode
        else:
            # Handle the case where the command failed
            err_msg = f"Error running command: {result.stderr}"
            return err_msg, result.returncode
    except Exception as e:
        # Handle other exceptions
        err_msg = f"An error occurred: {e}"
        return err_msg, -1
