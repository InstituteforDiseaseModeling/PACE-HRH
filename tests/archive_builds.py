import os
import pathlib
import re
from subprocess import Popen, PIPE

files = os.listdir("..")
for f in files:
    if str(f).endswith("zip"):
        fullname= pathlib.Path(f).absolute()
        version = str(re.search('ehep_(.+).zip', f).groups(1)[0])
        # construct gh command
        print("------------------")
        cmd = f'gh release create {version} {f} --generate-notes'
        git_log_cmd = f'git log --grep={version}\.\? --all-match --format=%h'
        p= Popen(git_log_cmd, stdout=PIPE, stderr=PIPE)
        out, err = p.communicate()
        hash = out.decode("utf-8").strip("\n")
        if len(hash)!=7:
            print(f"#unable to fetch commit, please manually search version {version}")
            continue
        git_tag_cmd = f'git tag {version} {hash}'
        git_tag_push_cmd = f'git push origin {version}'
        git_tag_fetch_cmd = f'git fetch --tags origin'
        print(git_tag_cmd)
        print(git_tag_push_cmd)
        print(git_tag_fetch_cmd)
        print(cmd)
        if os.path.exists(f'ehep_{version}.tar.gz'):
            upload_cmd = f'gh release upload {version} ehep_{version}.tar.gz'
            print(upload_cmd)