#!/usr/bin/env python3
# NOTE: On reboot, you may get an error that:
# `Error running command: cannot focus space due to an error with the scripting-addition.`
#
# To fix this, you need to run the following commands:
# `sudo yabai --uninstall-sa`
# `sudo yabai --load-sa`
# `sudo yabai --load-sa` ## Run this command twice for some reason
#
# Follow https://github.com/koekeishiya/yabai/issues/1158 for more info.
import sys
import argparse
import re
from script_utils import run_bash_command

def get_window_data(yabai_windows: dict, application_name: str, title_regex: str = None) -> dict | None:
    for window in yabai_windows:
        if window["app"] == application_name:
            if title_regex is None or re.search(title_regex, window["title"]):
                return window
    # The application or matching window was not found
    return None

def main():
    parser = argparse.ArgumentParser(description="Focus on specified application windows.")
    parser.add_argument("app_names", nargs="+", help="Names of the applications to focus on")
    parser.add_argument("--window-title", help="Regex pattern to match window titles")
    args = parser.parse_args()

    yabai_windows, ret_code = run_bash_command("yabai -m query --windows", json_output=True)
    if ret_code != 0 or yabai_windows is None:
        return

    for app_name in args.app_names:
        window = get_window_data(yabai_windows, app_name, args.window_title)
        # If the window was found, focus to it and exit
        if window is not None:
            run_bash_command(f"yabai -m space --focus {window['space']}")
            run_bash_command(f"yabai -m window --focus {window['id']}")
            print(f"Focused on {app_name}" + (f" (title matched: {window['title']})" if args.window_title else ""))
            return

    print("None of the specified applications" + (" with matching window titles" if args.window_title else "") + " were found.")

if __name__ == "__main__":
    main()
