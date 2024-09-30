#!/usr/bin/env python3

import sys
from script_utils import run_bash_command

def get_space_data_from_uuid(
    yabai_spaces: dict, uuid: str
) -> dict | None:
    for space in yabai_spaces:
        if space["uuid"] == uuid:
            return space

    # The space was not found
    return None


def main():
    # Get the space uuid from the command line arguments
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <space_uuid>")
        return

    space_uuid = sys.argv[1]

    yabai_spaces, ret_code = run_bash_command("yabai -m query --spaces", json_output=True)

    if ret_code != 0:
        return

    space = get_space_data_from_uuid(yabai_spaces, space_uuid)

    # If the space was found, focus to it
    if space is not None:
        run_bash_command(f"yabai -m space --focus {space['index']}")


if __name__ == "__main__":
    main()
