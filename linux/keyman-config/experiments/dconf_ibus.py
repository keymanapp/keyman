#!/usr/bin/python3

import argparse
import ast
import subprocess
import sys

def main():
    parser = argparse.ArgumentParser(description='Install or uninstall a Keyman keyboard in IBus')
    parser.add_argument('-u', "--uninstall", help='uninstall', action="store_true")
    parser.add_argument('kmnfile', help='kmn file path')
    args = parser.parse_args()

    # if len(sys.argv) != 2:
    #     print("dconf_ibus.py <kmn file path>")
    #     sys.exit(2)

    result = subprocess.run(["dconf", "read", "/desktop/ibus/general/preload-engines"],
        stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")
    # print("Output:")
    # print(result.stdout)
    # print("StdErr")
    # print(result.stderr)

    # test = "['xkb:us::eng', 'xkb:gb:extd:eng', 'Unikey', 'libthai', '/usr/local/share/keyman/sil_areare/sil_areare.kmn', '/usr/local/share/keyman/arabic_izza/arabic_izza.kmn']"
    # test_ast = ast.literal_eval(test)
    # print(test_ast)

    # test2 = str(result.stdout)
    # print(test2)

    if (result.returncode == 0):
        preload_engines = ast.literal_eval(result.stdout)
        print(preload_engines)
        if args.uninstall:
            if args.kmnfile not in preload_engines:
                print(args.kmnfile, "is not installed")
                return
            preload_engines.remove(args.kmnfile)
            print(preload_engines)
        else:
            print("new kmn:", args.kmnfile)
            preload_engines.append(args.kmnfile)
            print(preload_engines)
        result2 = subprocess.run(["dconf", "write", "/desktop/ibus/general/preload-engines", str(preload_engines)],
            stdout=subprocess.PIPE, stderr= subprocess.STDOUT, encoding="UTF8")

if __name__ == "__main__":
	main()