|命令|作用|
|:---|:---|
| chdir dir cmd [args]...   | run command in a given directory |
| rename oldname newname    | rename a file or directory (on one volume) |
| copy file destination     | copy file to destination (either file or directory) |
| copy_if_different in-file out-file  | copy file if input has changed |
| copy_directory source destination   | copy directory 'source' content to directory 'destination'  compare_files file1 file2 | check if file1 is same as file2 |
| echo [string]...          | displays arguments as text |
| echo_append [string]...   | displays arguments as text but no new line |
| environment               | display the current enviroment |
| make_directory dir        | create a directory |
| md5sum file1 [...]        | compute md5sum of files |
| remove_directory dir      | remove a directory and its contents |
| remove [-f] file1 file2 ... | remove the file(s), use -f to force it |
| tar [cxt][vfz] file.tar file/dir1 file/dir2 ... | create a tar archive |
| time command [args] ...   | run command and return elapsed time |
| touch file                | touch a file. |
| touch_nocreate file       | touch a file but do not create it. |
| build build_dir           | build the project in build_dir. |
| write_regv key value      | write registry value |
| delete_regv key           | delete registry value |
| comspec                   | on windows 9x use this for RunCommand |
