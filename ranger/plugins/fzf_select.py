import subprocess
import os
from ranger.api.commands import Command

class fzf_select(Command):
    """Find a file or directory using fzf and navigate to it."""
    def execute(self):
        fzf = self.fm.execute_command(
            "fd --hidden --follow --exclude .git | fzf +m",
            stdout=subprocess.PIPE, shell=True
        )
        stdout, _ = fzf.communicate()
        if fzf.returncode == 0:
            selected = os.path.abspath(stdout.decode("utf-8").strip())
            if os.path.isdir(selected):
                self.fm.cd(selected)
            else:
                self.fm.select_file(selected)
